open Prelude
module Syntax = Oak_core_syntax
module Cow_slice = Utility.Cow_slice

(*
  It is fine to use this value as the context_size as long as we don't call any other functions that use this context_size.
  In other words we cannot be reentrant.
*)
let temporary_context_size = Int.max_value / 2

let eval_closure1 eval (closure : _ Syntax.closure) arg =
  eval (Syntax.Env.push arg closure.closure_env) closure.x
;;

let eval_closure0 eval (closure : _ Syntax.closure) = eval closure.closure_env closure.x

let rec eval_value (env : Syntax.env) (e : Syntax.term) : Syntax.value =
  match e with
  | Term_bound index -> Syntax.Env.get_index_exn env index
  | Term_free level -> Syntax.Value.free level
  | Term_app { func; arg } ->
    let func = eval_value env func in
    let arg = eval_arg env arg in
    app_value func arg
  | Term_fun { name; icit; body } ->
    Value_fun { name; icit; body = { closure_env = env; x = body } }
  | Term_proj { strukt; field } ->
    let strukt = eval_value env strukt in
    proj_value strukt field
  | Term_struct { field_impls } ->
    let field_impls = Cow_slice.map field_impls ~f:(eval_field_impl env) in
    Value_struct { field_impls }
  | Term_sing_in e ->
    let e = eval_value env e in
    Value_sing_in e
  | Term_sing_out e ->
    let e = eval_value env e in
    out_value e
  | Term_let { name = _; rhs; body } ->
    let rhs = eval_value env rhs in
    eval_value (Syntax.Env.push rhs env) body
  | Term_ignore -> Value_ignore
  | Term_encode_ty { ty; props } ->
    let ty = eval_ty env ty in
    Value_encode_ty { ty; props }
  | Term_data data -> Syntax.Value.of_head (Data (eval_term_data env data))
  | Term_data_rec data_rec ->
    Syntax.Value.of_head (Data_rec (eval_term_data_rec env data_rec))

and eval_term_data (env : Syntax.env) ({ num_params; body; ty } : Syntax.term_data)
  : Syntax.value_data
  =
  { num_params; body = { closure_env = env; x = body }; ty = eval_ty env ty }

and eval_term_data_rec (env : Syntax.env) ({ decls; ty } : Syntax.term_data_rec)
  : Syntax.value_data_rec
  =
  { decls = { closure_env = env; x = decls }; ty = eval_ty env ty }

and eval_ty (env : Syntax.env) (ty : Syntax.term_ty) : Syntax.ty =
  match ty with
  | Term_ty_decode e ->
    let e = eval_value env e in
    decode_value e
  | Term_ty_fun { param; body_ty } ->
    let param_ty = eval_ty env param.ty in
    let param : Syntax.value_param =
      { name = param.name; modifiers = param.modifiers; ty = param_ty }
    in
    Ty_fun { param; body_ty = { closure_env = env; x = body_ty } }
  | Term_ty_struct ty -> Ty_struct (eval_term_ty_struct env ty)
  | Term_ty_sing { identity; ty } ->
    let identity = eval_value env identity in
    let ty = eval_ty env ty in
    Ty_sing { identity; ty }
  | Term_ty_pack ty ->
    let ty = eval_ty env ty in
    Ty_pack ty
  | Term_ty_core ty -> Ty_core ty
  | Term_ty_universe props -> Ty_universe props

and eval_term_ty_struct env ({ field_specs } : Syntax.term_ty_struct) : Syntax.ty_struct =
  let field_specs = Cow_slice.map field_specs ~f:(eval_field_spec env) in
  { field_specs }

and eval_field_spec env ({ name; ty; relevancy } : Syntax.term_field_spec)
  : Syntax.value_field_spec
  =
  { name; ty = { closure_env = env; x = ty }; relevancy }

and eval_field_impl env ({ name; e } : Syntax.term_field_impl) : Syntax.value_field_impl =
  let e = eval_value env e in
  { name; e }

and eval_arg env ({ e; icit } : Syntax.term_arg) : Syntax.value_arg =
  let e = eval_value env e in
  { e; icit }

and decode_value (ty : Syntax.value) : Syntax.ty =
  match ty with
  | Value_encode_ty { ty; props = _ } -> ty
  | Value_neutral e -> Ty_decode e
  | _ -> failwith "Expected a type code"

and app_value (func : Syntax.value) (arg : Syntax.value_arg) : Syntax.value =
  match func with
  | Value_ignore ->
    (* Function types can have kind Type *)
    Value_ignore
  | Value_fun func -> app_fun func arg.e
  | Value_neutral { head; spine } -> Value_neutral { head; spine = spine <: App arg }
  | _ -> failwith "Expected function value"

and proj_value (strukt : Syntax.value) (field : Syntax.field_loc) : Syntax.value =
  (* No ignore case here because structures always have kind Sig *)
  match strukt with
  | Value_struct strukt -> proj_struct strukt field
  | Value_neutral { head; spine } -> Value_neutral { head; spine = spine <: Proj field }
  | _ -> failwith "Expected a struct value"

and out_value (sing : Syntax.value) : Syntax.value =
  match sing with
  | Value_sing_in e -> e
  | Value_neutral { head; spine } -> Value_neutral { head; spine = spine <: Out }
  | _ -> failwith "Expected a singleton value"

(* precondition: strukt is whnf, postcondition: result is whnf *)
and app_whnf (ty_env : Syntax.ty_env) (func : Syntax.value) (arg : Syntax.value_arg)
  : Syntax.value
  =
  match func with
  | Value_ignore ->
    (* Function types can have kind Type *)
    Value_ignore
  | Value_fun func -> whnf_value ty_env (app_fun func arg.e)
  | Value_neutral { head; spine } -> Value_neutral { head; spine = spine <: App arg }
  | _ -> failwith "Expected function value"

(* precondition: strukt is whnf, postcondition: result is whnf *)
and proj_whnf (ty_env : Syntax.ty_env) (strukt : Syntax.value) (field : Syntax.field_loc)
  : Syntax.value
  =
  match strukt with
  | Value_struct strukt -> whnf_value ty_env (proj_struct strukt field)
  | Value_neutral { head; spine } -> Value_neutral { head; spine = spine <: Proj field }
  | _ -> failwith "Expected a struct value"

and proj_struct (strukt : Syntax.value_struct) (field : Syntax.field_loc) =
  let field_impl = Cow_slice.get strukt.field_impls field.index in
  field_impl.e

and app_fun (abs : Syntax.value_fun) (arg : Syntax.value) =
  eval_value_closure1 abs.body arg

and eval_ty_closure0 closure = eval_closure0 eval_ty closure
and eval_value_closure0 closure = eval_closure0 eval_value closure

and eval_value_closure1 (closure : Syntax.term Syntax.closure) arg =
  eval_value (Syntax.Env.push arg closure.closure_env) closure.x

and eval_ty_closure1 (closure : Syntax.term_ty Syntax.closure) arg =
  eval_ty (Syntax.Env.push arg closure.closure_env) closure.x

and whnf_value ty_env (e : Syntax.value) : Syntax.value =
  match e with
  | Value_neutral neutral -> whnf_neutral ty_env neutral
  | Value_ignore | Value_struct _ | Value_fun _ | Value_sing_in _ | Value_encode_ty _ -> e

and whnf_ty (ty_env : Syntax.ty_env) (ty : Syntax.ty) : Syntax.ty =
  match ty with
  | Ty_decode e -> begin
    match whnf_neutral ty_env e with
    | Value_encode_ty { ty; props = _ } -> whnf_ty ty_env ty
    | Value_neutral e -> Ty_decode e
    | _ -> failwith "Expected a type code"
  end
  | Ty_universe _ | Ty_sing _ | Ty_struct _ | Ty_fun _ | Ty_core _ | Ty_pack _ -> ty

and app_fun_ty (func_ty : Syntax.ty_fun) (arg : Syntax.value_arg) : Syntax.ty =
  eval_ty_closure1 func_ty.body_ty arg.e

and app_ty (ty_env : Syntax.ty_env) (ty : Syntax.ty) (arg : Syntax.value_arg) : Syntax.ty =
  app_fun_ty (Syntax.Ty.ty_fun_val_exn (whnf_ty ty_env ty)) arg

and out_ty (ty_env : Syntax.ty_env) (ty : Syntax.ty) : Syntax.ty =
  (Syntax.Ty.ty_sing_val_exn (whnf_ty ty_env ty)).ty

and eval_term_data_field (env : Syntax.env) ({ name; ty } : Syntax.term_data_field)
  : Syntax.value_data_field
  =
  let ty = eval_ty env ty in
  { name; ty }

and eval_term_data_constructor
      (env : Syntax.env)
      ({ name; ty } : Syntax.term_data_constructor)
  : Syntax.value_data_constructor
  =
  let ty = Option.map ~f:(eval_ty env) ty in
  { name; ty }

and eval_term_data_decl
      (env : Syntax.env)
      ({ name; num_params; body } : Syntax.term_data_decl)
  : Syntax.value_data_decl
  =
  { name; num_params; body = { closure_env = env; x = body } }

and eval_term_data_body (env : Syntax.env) (data_body : Syntax.term_data_body)
  : Syntax.value_data_body
  =
  match data_body with
  | Term_data_record { fields } ->
    let fields = List.map fields ~f:(eval_term_data_field env) in
    Value_data_record { fields }
  | Term_data_variant { constructors } ->
    let constructors = List.map constructors ~f:(eval_term_data_constructor env) in
    Value_data_variant { constructors }

and proj_struct_ty_field_spec (struct_ty : Syntax.ty_struct) (field : Syntax.field_loc) =
  Cow_slice.get struct_ty.field_specs field.index

and proj_struct_ty_relevancy (struct_ty : Syntax.ty_struct) (field : Syntax.field_loc)
  : Syntax.Relevancy.t
  =
  let field_spec = Cow_slice.get struct_ty.field_specs field.index in
  field_spec.relevancy

and proj_struct_ty
      (strukt : Syntax.value)
      (struct_ty : Syntax.ty_struct)
      (field : Syntax.field_loc)
  : Syntax.ty
  =
  let field_spec = Cow_slice.get struct_ty.field_specs field.index in
  eval_ty_closure1 field_spec.ty strukt

and proj_struct_ty_non_dependent (struct_ty : Syntax.ty_struct) (field : Syntax.field_loc)
  : Syntax.ty
  =
  let field_spec = Cow_slice.get struct_ty.field_specs field.index in
  eval_ty_closure0 field_spec.ty

and proj_ty
      (ty_env : Syntax.ty_env)
      (strukt : Syntax.value)
      (ty : Syntax.ty)
      (field : Syntax.field_loc)
  : Syntax.ty
  =
  proj_struct_ty strukt (Syntax.Ty.ty_struct_val_exn (whnf_ty ty_env ty)) field

and proj_ty_non_dependent
      (ty_env : Syntax.ty_env)
      (ty : Syntax.ty)
      (field : Syntax.field_loc)
  : Syntax.ty
  =
  proj_struct_ty_non_dependent (Syntax.Ty.ty_struct_val_exn (whnf_ty ty_env ty)) field

and whnf_neutral (ty_env : Syntax.ty_env) (e : Syntax.neutral) : Syntax.value =
  let ~value, .. =
    Bwd.fold_left
      e.spine
      ~init:
        ( ~value:(Value_neutral { head = e.head; spine = Empty })
        , ~ty:(infer_head ty_env e.head) )
      ~f:(fun (~value, ~ty) (frame : Syntax.frame) ->
        (* invariant: e is whnf, ty may not be whnf *)
        match frame with
        | App arg -> ~value:(app_whnf ty_env value arg), ~ty:(app_ty ty_env ty arg)
        | Proj field ->
          ~value:(proj_whnf ty_env value field), ~ty:(proj_ty ty_env value ty field)
        | Out ->
          let ty =
            match whnf_ty ty_env ty with
            | Ty_sing sing -> sing
            | _ -> failwith "Expected singleton type"
          in
          ~value:(whnf_value ty_env ty.identity), ~ty:ty.ty)
  in
  value

and infer_props (ty_env : Syntax.ty_env) (ty : Syntax.ty) =
  match ty with
  | Ty_universe props -> props
  | Ty_sing _ -> { size = Syntax.Size.sig_ }
  | Ty_struct ty ->
    let ~size, .. =
      Cow_slice.foldi
        ty.field_specs
        ~init:
          ( ~size:Syntax.Size.sig_
          , ~ty_env
          , ~running_field_impls:(Cow_slice.create (Cow_slice.length ty.field_specs)) )
        ~f:(fun index (~size, ~ty_env, ~running_field_impls) field_spec ->
          let field_spec_ty =
            proj_struct_ty
              (Syntax.Value.create_struct running_field_impls)
              ty
              { name = field_spec.name.name; index }
          in
          let props = infer_props ty_env field_spec_ty in
          ( ~size:(Syntax.Size.max size props.size)
          , ~ty_env:(Syntax.Env.push field_spec_ty ty_env)
          , ~running_field_impls:(Cow_slice.push_full_slice_exn
                                    running_field_impls
                                    (Syntax.Value_field_impl.create
                                       field_spec.name.name
                                       (Syntax.Value.free_of_size
                                          (Syntax.Env.length ty_env))
                                     : Syntax.value_field_impl)) ))
    in
    { size }
  | Ty_fun ty ->
    let arg : Syntax.value_arg =
      { e = Syntax.Value.free_of_size (Syntax.Env.length ty_env)
      ; icit = ty.param.modifiers.icit
      }
    in
    let param_ty_props = infer_props ty_env ty.param.ty in
    let body_ty_props =
      infer_props (Syntax.Env.push ty.param.ty ty_env) (app_fun_ty ty arg)
    in
    { size = Syntax.Size.max param_ty_props.size body_ty_props.size }
  | Ty_pack _ | Ty_core _ -> { size = Syntax.Size.type_ }
  | Ty_decode ty -> infer_neutral_universe ty_env ty

and infer_neutral (ty_env : Syntax.ty_env) (e : Syntax.neutral) : Syntax.ty =
  let ~ty, .. =
    Bwd.fold_left
      e.spine
      ~init:(~spine:Bwd.Empty, ~ty:(infer_head ty_env e.head))
      ~f:(fun (~spine, ~ty) (frame : Syntax.frame) ->
        let ty =
          match frame with
          | App arg -> app_ty ty_env ty arg
          | Proj field -> proj_ty ty_env (Value_neutral { head = e.head; spine }) ty field
          | Out -> out_ty ty_env ty
        in
        ~spine:(spine <: frame), ~ty)
  in
  ty

and infer_head (ty_env : Syntax.ty_env) (head : Syntax.head) : Syntax.ty =
  match head with
  | Free free -> Syntax.Env.get_level_exn ty_env free
  | Data { ty; _ } -> ty
  | Data_rec { ty; _ } -> ty

and infer_neutral_universe (ty_env : Syntax.ty_env) (e : Syntax.neutral)
  : Syntax.Ty_props.t
  =
  let ty = infer_neutral ty_env e in
  whnf_ty ty_env ty |> Syntax.Ty.ty_universe_val_exn
;;

(* Substitutes free variables into bound variables *)
module Close = struct
  type t =
    { map : int Int.Map.t
    ; lift : int
    }
  [@@deriving sexp_of]

  let empty = { map = Int.Map.empty; lift = 0 }
  let lift n (close : t) = { close with lift = close.lift + n }

  let singleton (level : Syntax.Level.t) (index : Syntax.Index.t) : t =
    { map = Int.Map.singleton level.level index.index; lift = 0 }
  ;;

  let add_exn (level : Syntax.Level.t) (index : Syntax.Index.t) (close : t) =
    { close with
      map = Map.add_exn close.map ~key:level.level ~data:(index.index - close.lift)
    }
  ;;

  let compose ~(second : t) ~(first : t) =
    let map =
      Map.merge first.map second.map ~f:(fun ~key:_ e ->
        Some
          (match e with
           | `Right v -> v - first.lift + second.lift
           | `Left v -> v
           | `Both (v1, _v2) -> v1))
    in
    { first with map }
  ;;

  let find (close : t) (level : Syntax.Level.t) =
    Option.map
      ~f:(fun i -> Syntax.Index.of_int (i + close.lift))
      (Map.find close.map level.level)
  ;;

  let push_exn l t = add_exn l Syntax.Index.zero (lift 1 t)
end

let rec quote_value context_size (e : Syntax.value) : Syntax.term =
  match e with
  | Value_ignore -> Term_ignore
  | Value_struct { field_impls } ->
    let field_impls = Cow_slice.map field_impls ~f:(quote_field_impl context_size) in
    Term_struct { field_impls }
  | Value_fun { name; body; icit } ->
    let body =
      eval_value_closure1 body (Syntax.Value.free_of_size context_size)
      |> quote_value (context_size + 1)
      |> close_single (Syntax.Level.of_int context_size)
    in
    Term_fun { name; body; icit }
  | Value_sing_in e -> Term_sing_in (quote_value context_size e)
  | Value_neutral e -> quote_neutral context_size e
  | Value_encode_ty { ty; props } ->
    let ty = quote_ty context_size ty in
    Term_encode_ty { ty; props }

and quote_ty context_size (ty : Syntax.ty) : Syntax.term_ty =
  match ty with
  | Ty_universe props -> Term_ty_universe props
  | Ty_sing { identity; ty } ->
    let identity = quote_value context_size identity in
    let ty = quote_ty context_size ty in
    Term_ty_sing { identity; ty }
  | Ty_struct { field_specs } ->
    let field_specs =
      Cow_slice.map field_specs ~f:(fun { name; ty; relevancy } ->
        let level = Syntax.Level.of_int context_size in
        let context_size = context_size + 1 in
        let ty =
          eval_ty_closure1 ty (Syntax.Value.free level)
          |> quote_ty context_size
          |> close_ty_single level
        in
        ({ name; ty; relevancy } : Syntax.term_field_spec))
    in
    Term_ty_struct { field_specs }
  | Ty_fun { param; body_ty } ->
    let param_ty = quote_ty context_size param.ty in
    let body_ty =
      eval_ty_closure1 body_ty (Syntax.Value.free_of_size context_size)
      |> quote_ty (context_size + 1)
      |> close_ty_single (Syntax.Level.of_int context_size)
    in
    let param : Syntax.term_param =
      { name = param.name; modifiers = param.modifiers; ty = param_ty }
    in
    Term_ty_fun { param; body_ty }
  | Ty_core ty -> Term_ty_core ty
  | Ty_pack ty -> Term_ty_pack (quote_ty context_size ty)
  | Ty_decode e ->
    let e = quote_neutral context_size e in
    Term_ty_decode e

and quote_neutral context_size (e : Syntax.neutral) : Syntax.term =
  Bwd.fold_left
    e.spine
    ~init:(quote_head context_size e.head)
    ~f:(fun e (elim : Syntax.frame) : Syntax.term ->
      match elim with
      | Proj field -> Term_proj { strukt = e; field }
      | App arg ->
        let arg = quote_arg context_size arg in
        Term_app { func = e; arg }
      | Out -> Term_sing_out e)

and quote_head context_size (head : Syntax.head) : Syntax.term =
  match head with
  | Free free ->
    assert (free.level < context_size);
    Term_free free
  | Data _ | Data_rec _ -> failwith ""

and quote_arg context_size (arg : Syntax.value_arg) : Syntax.term_arg =
  let e = quote_value context_size arg.e in
  { e; icit = arg.icit }

and quote_field_impl (context_size : int) ({ name; e } : Syntax.value_field_impl)
  : Syntax.term_field_impl
  =
  let e = quote_value context_size e in
  { name; e }

and close_term (c : Close.t) (e : Syntax.term) : Syntax.term =
  match e with
  | Term_bound v -> Term_bound v
  | Term_free i ->
    Close.find c i |> Option.value_map ~default:e ~f:(fun v -> Term_bound v)
  | Term_app { func; arg } -> Term_app { func = close_term c func; arg = close_arg c arg }
  | Term_fun { name; icit; body } ->
    Term_fun { name; icit; body = close_term (Close.lift 1 c) body }
  | Term_proj { strukt; field } -> Term_proj { strukt = close_term c strukt; field }
  | Term_struct { field_impls } ->
    Term_struct { field_impls = Cow_slice.map field_impls ~f:(close_field_impl c) }
  | Term_encode_ty { ty; props } ->
    let ty = close_ty c ty in
    Term_encode_ty { ty; props }
  | Term_sing_in e -> Term_sing_in (close_term c e)
  | Term_sing_out e -> Term_sing_out (close_term c e)
  | Term_let { name; rhs; body } ->
    Term_let { name; rhs = close_term c rhs; body = close_term (Close.lift 1 c) body }
  | Term_ignore -> Term_ignore
  | Term_data { num_params; body; ty } ->
    let c = Close.lift num_params c in
    Term_data { num_params; body = close_data_body c body; ty = close_ty c ty }
  | Term_data_rec { decls; ty } ->
    let c = Close.lift 1 c in
    let decls =
      List.map decls ~f:(fun ({ name; num_params; body } : Syntax.term_data_decl) ->
        let c = Close.lift num_params c in
        ({ name; num_params; body = close_data_body c body } : Syntax.term_data_decl))
    in
    let ty = close_ty c ty in
    Term_data_rec { decls; ty }

and close_single (level : Syntax.Level.t) e =
  close_term (Close.singleton level (Syntax.Index.of_int 0)) e

and close_ty_single (level : Syntax.Level.t) ty =
  close_ty (Close.singleton level (Syntax.Index.of_int 0)) ty

and close_ty (c : Close.t) (ty : Syntax.term_ty) : Syntax.term_ty =
  match ty with
  | Term_ty_decode e ->
    let e = close_term c e in
    Term_ty_decode e
  | Term_ty_fun { param; body_ty } ->
    let param_ty = close_ty c param.ty in
    let body_ty = close_ty (Close.lift 1 c) body_ty in
    let param : Syntax.term_param =
      { name = param.name; modifiers = param.modifiers; ty = param_ty }
    in
    Term_ty_fun { param; body_ty }
  | Term_ty_struct { field_specs } ->
    let c = Close.lift 1 c in
    let field_specs =
      Cow_slice.map field_specs ~f:(fun { name; ty; relevancy } ->
        ({ name; ty = close_ty c ty; relevancy } : Syntax.term_field_spec))
    in
    Term_ty_struct { field_specs }
  | Term_ty_sing { identity; ty } ->
    let identity = close_term c identity in
    let ty = close_ty c ty in
    Term_ty_sing { identity; ty }
  | Term_ty_pack ty ->
    let ty = close_ty c ty in
    Term_ty_pack ty
  | Term_ty_core ty -> Term_ty_core ty
  | Term_ty_universe props -> Term_ty_universe props

and close_data_body (c : Close.t) (body : Syntax.term_data_body) : Syntax.term_data_body =
  match body with
  | Term_data_record { fields } ->
    Term_data_record { fields = List.map fields ~f:(close_data_field c) }
  | Term_data_variant { constructors } ->
    Term_data_variant
      { constructors = List.map constructors ~f:(close_data_constructor c) }

and close_data_field (c : Close.t) ({ name; ty } : Syntax.term_data_field)
  : Syntax.term_data_field
  =
  { name; ty = close_ty c ty }

and close_data_constructor (c : Close.t) ({ name; ty } : Syntax.term_data_constructor)
  : Syntax.term_data_constructor
  =
  { name; ty = Option.map ty ~f:(close_ty c) }

and close_field_impl (c : Close.t) ({ name; e } : Syntax.term_field_impl) =
  { name; e = close_term c e }

and close_arg (c : Close.t) ({ e; icit } : Syntax.term_arg) = { e = close_term c e; icit }

module Term = struct
  let close = close_term
  let close_single = close_single
  let eval = eval_value
end

module Term_ty = struct
  let close = close_ty
  let close_single = close_ty_single
  let eval = eval_ty
end

module Term_ty_struct = struct
  let eval = eval_term_ty_struct
end

module Term_data_decl = struct
  let eval = eval_term_data_decl
end

module Term_data_rec = struct
  let eval = eval_term_data_rec
end

module Term_data_body = struct
  let eval = eval_term_data_body
  let close = close_data_body
end

module Struct = struct
  let proj = proj_struct
end

module Ty_struct = struct
  let proj_field_spec = proj_struct_ty_field_spec
  let proj = proj_struct_ty
  let proj_non_dependent = proj_struct_ty_non_dependent
end

module Fun = struct
  let app = app_fun
end

module Fun_ty = struct
  let app = app_fun_ty
end

module Ty_fun = Fun_ty

module Value = struct
  let whnf = whnf_value
  let eval = eval_value
  let quote_with = quote_value
  let quote = quote_value temporary_context_size
  let proj = proj_value
  let app = app_value
  let out = out_value
  let decode = decode_value
end

module Closure = struct
  let eval1 = eval_closure1
  let eval0 = eval_closure0
end

module Term_closure = struct
  let eval1 = eval_value_closure1
end

module Term_ty_closure = struct
  let eval1 = eval_ty_closure1
  let eval0 = eval_ty_closure0
end

module Ty = struct
  let infer_props = infer_props
  let whnf = whnf_ty
  let eval = eval_ty
  let quote_with = quote_ty
  let quote = quote_ty temporary_context_size
  let proj = proj_ty
  let proj_non_dependent = proj_ty_non_dependent
  let app = app_ty
  let out = out_ty
end

module Neutral = struct
  let infer_ty = infer_neutral
  let infer_universe = infer_neutral_universe
  let whnf = whnf_neutral
end

module Head = struct
  let infer_ty = infer_head
end
