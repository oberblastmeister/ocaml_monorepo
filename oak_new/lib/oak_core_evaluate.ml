open Prelude
module Core = Oak_core_syntax

let rec eval_value (env : Core.env) (e : Core.term) : Core.value =
  match e with
  | Term_bound index -> Core.Seq.get_index_exn env index
  | Term_free level -> Core.Value.free level
  | Term_app { func; arg } ->
    let func = eval_value env func in
    let arg = eval_arg env arg in
    app_value func arg
  | Term_fun { name; icit; body } -> Value_fun { name; icit; body = { env; body } }
  | Term_proj { strukt; field } ->
    let strukt = eval_value env strukt in
    proj_value strukt field
  | Term_struct { field_impls } ->
    let field_impls = List.map field_impls ~f:(eval_field_impl env) in
    Value_struct { field_impls }
  | Term_sing_in e ->
    let e = eval_value env e in
    Value_sing_in e
  | Term_sing_out e ->
    let e = eval_value env e in
    out_value e
  | Term_let { name = _; rhs; body } ->
    let rhs = eval_value env rhs in
    eval_value (Core.Seq.push rhs env) body
  | Term_ignore -> Value_ignore
  | Term_encode_ty { ty; props } ->
    let ty = eval_ty env ty in
    Value_encode_ty { ty; props }
  | Term_data { num_params; body; ty } ->
    Core.Value.of_head (Data { env; num_params; body; ty = eval_ty env ty })
  | Term_data_rec { decls; ty } ->
    Core.Value.of_head (Data_rec { env; decls; ty = eval_ty env ty })

and eval_ty (env : Core.env) (ty : Core.term_ty) : Core.ty =
  match ty with
  | Term_ty_decode e ->
    let e = eval_value env e in
    decode_value e
  | Term_ty_fun { name; param_ty; param_modifiers; body_ty } ->
    let param_ty = eval_ty env param_ty in
    Ty_fun { name; param_modifiers; param_ty; body_ty = { env; body = body_ty } }
  | Term_ty_struct { field_specs } -> Ty_struct { env; field_specs }
  | Term_ty_sing { identity; ty } ->
    let identity = eval_value env identity in
    let ty = eval_ty env ty in
    Ty_sing { identity; ty }
  | Term_ty_pack ty ->
    let ty = eval_ty env ty in
    Ty_pack ty
  | Term_ty_core ty -> Ty_core ty
  | Term_ty_universe props -> Ty_universe props

and eval_field_impl env ({ name; e } : Core.term_field_impl) : Core.value_field_impl =
  let e = eval_value env e in
  { name; e }

and eval_arg env ({ e; icit } : Core.term_arg) : Core.value_arg =
  let e = eval_value env e in
  { e; icit }

and decode_value (ty : Core.value) : Core.ty =
  match ty with
  | Value_encode_ty { ty; props = _ } -> ty
  | Value_neutral e -> Ty_decode e
  | _ -> failwith "Expected a type code"

and app_value (func : Core.value) (arg : Core.value_arg) : Core.value =
  match func with
  | Value_ignore ->
    (* Function types can have kind Type *)
    Value_ignore
  | Value_fun func -> app_fun func arg.e
  | Value_neutral { head; spine } -> Value_neutral { head; spine = spine <: App arg }
  | _ -> failwith "Expected function value"

and proj_value (strukt : Core.value) (field : Core.field_loc) : Core.value =
  (* No ignore case here because structures always have kind Sig *)
  match strukt with
  | Value_struct strukt -> proj_struct strukt field
  | Value_neutral { head; spine } -> Value_neutral { head; spine = spine <: Proj field }
  | _ -> failwith "Expected a struct value"

and out_value (sing : Core.value) : Core.value =
  match sing with
  | Value_sing_in e -> e
  | Value_neutral { head; spine } -> Value_neutral { head; spine = spine <: Out }
  | _ -> failwith "Expected a singleton value"

(* precondition: strukt is whnf, postcondition: result is whnf *)
and app_whnf (ty_env : Core.ty_env) (func : Core.value) (arg : Core.value_arg)
  : Core.value
  =
  match func with
  | Value_ignore ->
    (* Function types can have kind Type *)
    Value_ignore
  | Value_fun func -> whnf_value ty_env (app_fun func arg.e)
  | Value_neutral { head; spine } -> Value_neutral { head; spine = spine <: App arg }
  | _ -> failwith "Expected function value"

(* precondition: strukt is whnf, postcondition: result is whnf *)
and proj_whnf (ty_env : Core.ty_env) (strukt : Core.value) (field : Core.field_loc)
  : Core.value
  =
  match strukt with
  | Value_struct strukt -> whnf_value ty_env (proj_struct strukt field)
  | Value_neutral { head; spine } -> Value_neutral { head; spine = spine <: Proj field }
  | _ -> failwith "Expected a struct value"

and proj_struct (strukt : Core.value_struct) (field : Core.field_loc) =
  let field_impl = List.drop strukt.field_impls field.index |> List.hd_exn in
  field_impl.e

and app_fun (abs : Core.value_fun) (arg : Core.value) = eval_closure1 abs.body arg
and eval_closure1 closure arg = eval_value (Core.Seq.push arg closure.env) closure.body

and eval_ty_closure1 (closure : Core.ty_closure) arg =
  eval_ty (Core.Seq.push arg closure.env) closure.body

and whnf_value ty_env (e : Core.value) : Core.value =
  match e with
  | Value_neutral neutral -> whnf_neutral ty_env neutral
  | Value_ignore | Value_struct _ | Value_fun _ | Value_sing_in _ | Value_encode_ty _ -> e

and whnf_ty (ty_env : Core.ty_env) (ty : Core.ty) : Core.ty =
  match ty with
  | Ty_decode e -> begin
    match whnf_neutral ty_env e with
    | Value_encode_ty { ty; props = _ } -> whnf_ty ty_env ty
    | Value_neutral e -> Ty_decode e
    | _ -> failwith "Expected a type code"
  end
  | Ty_universe _ | Ty_sing _ | Ty_struct _ | Ty_fun _ | Ty_core _ | Ty_pack _ -> ty

and app_fun_ty (func_ty : Core.ty_fun) (arg : Core.value_arg) : Core.ty =
  eval_ty_closure1 func_ty.body_ty arg.e

and app_ty (ty_env : Core.ty_env) (ty : Core.ty) (arg : Core.value_arg) : Core.ty =
  app_fun_ty (Core.Ty.ty_fun_val_exn (whnf_ty ty_env ty)) arg

and out_ty (ty_env : Core.ty_env) (ty : Core.ty) : Core.ty =
  (Core.Ty.ty_sing_val_exn (whnf_ty ty_env ty)).ty

and proj_struct_ty
      (strukt : Core.value)
      (struct_ty : Core.ty_struct)
      (field : Core.field_loc)
  : Core.value_field_spec
  =
  let field_spec = List.drop struct_ty.field_specs field.index |> List.hd_exn in
  let field_spec_ty = eval_ty (Core.Seq.push strukt struct_ty.env) field_spec.ty in
  { name = field_spec.name; ty = field_spec_ty; relevancy = field_spec.relevancy }

and proj_ty
      (ty_env : Core.ty_env)
      (strukt : Core.value)
      (ty : Core.ty)
      (field : Core.field_loc)
  : Core.value_field_spec
  =
  proj_struct_ty strukt (Core.Ty.ty_struct_val_exn (whnf_ty ty_env ty)) field

and whnf_neutral (ty_env : Core.ty_env) (e : Core.neutral) : Core.value =
  let e, _ty =
    Bwd.fold_left
      e.spine
      ~init:(Value_neutral { head = e.head; spine = Empty }, infer_head ty_env e.head)
      ~f:(fun (e, ty) (frame : Core.frame) ->
        (* invariant: e is whnf, ty may not be whnf *)
        match frame with
        | App arg -> app_whnf ty_env e arg, app_ty ty_env ty arg
        | Proj field -> proj_whnf ty_env e field, (proj_ty ty_env e ty field).ty
        | Out ->
          let ty =
            match whnf_ty ty_env ty with
            | Ty_sing sing -> sing
            | _ -> failwith "Expected singleton type"
          in
          whnf_value ty_env ty.identity, ty.ty)
  in
  e

and infer_props (ty_env : Core.ty_env) (ty : Core.ty) =
  match ty with
  | Ty_universe props -> props
  | Ty_sing _ -> { size = Core.Size.sig_ }
  | Ty_struct ty ->
    let size, _, _ =
      List.foldi
        ty.field_specs
        ~init:(Core.Size.sig_, ty_env, Bwd.Empty)
        ~f:(fun index (size, ty_env, running_field_impls) field_spec ->
          let field_spec_ty =
            (proj_struct_ty
               (Value_struct (Core.Struct.create (Bwd.to_list running_field_impls)))
               ty
               { name = field_spec.name.name; index })
              .ty
          in
          let props = infer_props ty_env field_spec_ty in
          ( Core.Size.max size props.size
          , Core.Seq.push field_spec_ty ty_env
          , Bwd.snoc
              running_field_impls
              (Core.Value_field_impl.create
                 field_spec.name.name
                 (Core.Value.free_of_size (Core.Seq.length ty_env))
               : Core.value_field_impl) ))
    in
    { size }
  | Ty_fun ty ->
    let arg : Core.value_arg =
      { e = Core.Value.free_of_size (Core.Seq.length ty_env)
      ; icit = ty.param_modifiers.icit
      }
    in
    let param_ty_props = infer_props ty_env ty.param_ty in
    let body_ty_props =
      infer_props (Core.Seq.push ty.param_ty ty_env) (app_fun_ty ty arg)
    in
    { size = Core.Size.max param_ty_props.size body_ty_props.size }
  | Ty_pack _ | Ty_core _ -> { size = Core.Size.type_ }
  | Ty_decode ty -> infer_neutral_universe ty_env ty

and infer_neutral (ty_env : Core.ty_env) (e : Core.neutral) : Core.ty =
  Bwd.fold_left
    e.spine
    ~init:(Bwd.Empty, infer_head ty_env e.head)
    ~f:(fun (spine, ty) (frame : Core.frame) ->
      let ty =
        match frame with
        | App arg -> app_ty ty_env ty arg
        | Proj field ->
          (proj_ty ty_env (Value_neutral { head = e.head; spine }) ty field).ty
        | Out -> out_ty ty_env ty
      in
      spine <: frame, ty)
  |> snd

and infer_head (ty_env : Core.ty_env) (head : Core.head) : Core.ty =
  match head with
  | Free free -> Core.Seq.get_level_exn ty_env free
  | Data { ty; _ } -> ty
  | Data_rec { ty; _ } -> ty

and infer_neutral_universe (ty_env : Core.ty_env) (e : Core.neutral) : Core.Ty_props.t =
  let ty = infer_neutral ty_env e in
  whnf_ty ty_env ty |> Core.Ty.ty_universe_val_exn
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

  let singleton (level : Core.Level.t) (index : Core.Index.t) : t =
    { map = Int.Map.singleton level.level index.index; lift = 0 }
  ;;

  let add_exn (level : Core.Level.t) (index : Core.Index.t) (close : t) =
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

  let find (close : t) (level : Core.Level.t) =
    Option.map
      ~f:(fun i -> Core.Index.of_int (i + close.lift))
      (Map.find close.map level.level)
  ;;

  let push_exn l t = add_exn l Core.Index.zero (lift 1 t)
end

let rec quote_value context_size (e : Core.value) : Core.term =
  match e with
  | Value_ignore -> Term_ignore
  | Value_struct { field_impls } ->
    let field_impls = List.map field_impls ~f:(quote_field_impl context_size) in
    Term_struct { field_impls }
  | Value_fun { name; body; icit } ->
    let body =
      eval_closure1 body (Core.Value.free_of_size context_size)
      |> quote_value (context_size + 1)
      |> close_single (Core.Level.of_int context_size)
    in
    Term_fun { name; body; icit }
  | Value_sing_in e -> Term_sing_in (quote_value context_size e)
  | Value_neutral e -> quote_neutral context_size e
  | Value_encode_ty { ty; props } ->
    let ty = quote_ty context_size ty in
    Term_encode_ty { ty; props }

and quote_ty context_size (ty : Core.ty) : Core.term_ty =
  match ty with
  | Ty_universe props -> Term_ty_universe props
  | Ty_sing { identity; ty } ->
    let identity = quote_value context_size identity in
    let ty = quote_ty context_size ty in
    Term_ty_sing { identity; ty }
  | Ty_struct { env = closure_env; field_specs } ->
    let level = Core.Level.of_int context_size in
    let closure_env = Core.Seq.push (Core.Value.free level) closure_env in
    let context_size = context_size + 1 in
    let field_specs =
      List.map field_specs ~f:(fun { name; ty; relevancy } ->
        let ty =
          eval_ty closure_env ty |> quote_ty context_size |> close_ty_single level
        in
        ({ name; ty; relevancy } : Core.term_field_spec))
    in
    Term_ty_struct { field_specs }
  | Ty_fun { name; param_modifiers; param_ty; body_ty } ->
    let param_ty = quote_ty context_size param_ty in
    let body_ty =
      eval_ty_closure1 body_ty (Core.Value.free_of_size context_size)
      |> quote_ty (context_size + 1)
      |> close_ty_single (Core.Level.of_int context_size)
    in
    Term_ty_fun { name; param_modifiers; param_ty; body_ty }
  | Ty_core ty -> Term_ty_core ty
  | Ty_pack ty -> Term_ty_pack (quote_ty context_size ty)
  | Ty_decode e ->
    let e = quote_neutral context_size e in
    Term_ty_decode e

and quote_neutral context_size (e : Core.neutral) : Core.term =
  Bwd.fold_left
    e.spine
    ~init:(quote_head context_size e.head)
    ~f:(fun e (elim : Core.frame) : Core.term ->
      match elim with
      | Proj field -> Term_proj { strukt = e; field }
      | App arg ->
        let arg = quote_arg context_size arg in
        Term_app { func = e; arg }
      | Out -> Term_sing_out e)

and quote_head context_size (head : Core.head) : Core.term =
  match head with
  | Free free ->
    assert (free.level < context_size);
    Term_free free
  | Data _ | Data_rec _ -> failwith ""

and quote_arg context_size (arg : Core.value_arg) : Core.term_arg =
  let e = quote_value context_size arg.e in
  { e; icit = arg.icit }

and quote_field_impl (context_size : int) ({ name; e } : Core.value_field_impl)
  : Core.term_field_impl
  =
  let e = quote_value context_size e in
  { name; e }

and close (c : Close.t) (e : Core.term) : Core.term =
  match e with
  | Term_bound v -> Term_bound v
  | Term_free i ->
    Close.find c i |> Option.value_map ~default:e ~f:(fun v -> Term_bound v)
  | Term_app { func; arg } -> Term_app { func = close c func; arg = close_arg c arg }
  | Term_fun { name; icit; body } ->
    Term_fun { name; icit; body = close (Close.lift 1 c) body }
  | Term_proj { strukt; field } -> Term_proj { strukt = close c strukt; field }
  | Term_struct { field_impls } ->
    Term_struct { field_impls = List.map field_impls ~f:(close_field_impl c) }
  | Term_encode_ty { ty; props } ->
    let ty = close_ty c ty in
    Term_encode_ty { ty; props }
  | Term_sing_in e -> Term_sing_in (close c e)
  | Term_sing_out e -> Term_sing_out (close c e)
  | Term_let { name; rhs; body } ->
    Term_let { name; rhs = close c rhs; body = close (Close.lift 1 c) body }
  | Term_ignore -> Term_ignore
  | Term_data { num_params; body; ty } ->
    let c = Close.lift num_params c in
    Term_data { num_params; body = close_data_body c body; ty = close_ty c ty }
  | Term_data_rec { decls; ty } ->
    let c = Close.lift 1 c in
    let decls =
      List.map decls ~f:(fun ({ name; num_params; body } : Core.term_data_decl) ->
        let c = Close.lift num_params c in
        ({ name; num_params; body = close_data_body c body } : Core.term_data_decl))
    in
    let ty = close_ty c ty in
    Term_data_rec { decls; ty }

and close_single (level : Core.Level.t) e =
  close (Close.singleton level (Core.Index.of_int 0)) e

and close_ty_single (level : Core.Level.t) ty =
  close_ty (Close.singleton level (Core.Index.of_int 0)) ty

and close_ty (c : Close.t) (ty : Core.term_ty) : Core.term_ty =
  match ty with
  | Term_ty_decode e ->
    let e = close c e in
    Term_ty_decode e
  | Term_ty_fun { name; param_ty; param_modifiers; body_ty } ->
    let param_ty = close_ty c param_ty in
    let body_ty = close_ty (Close.lift 1 c) body_ty in
    Term_ty_fun { name; param_modifiers; param_ty; body_ty }
  | Term_ty_struct { field_specs } ->
    let c = Close.lift 1 c in
    let field_specs =
      List.map field_specs ~f:(fun { name; ty; relevancy } ->
        ({ name; ty = close_ty c ty; relevancy } : Core.term_field_spec))
    in
    Term_ty_struct { field_specs }
  | Term_ty_sing { identity; ty } ->
    let identity = close c identity in
    let ty = close_ty c ty in
    Term_ty_sing { identity; ty }
  | Term_ty_pack ty ->
    let ty = close_ty c ty in
    Term_ty_pack ty
  | Term_ty_core ty -> Term_ty_core ty
  | Term_ty_universe props -> Term_ty_universe props

and close_data_body (c : Close.t) (body : Core.term_data_body) : Core.term_data_body =
  match body with
  | Term_data_record { fields } ->
    Term_data_record { fields = List.map fields ~f:(close_data_field c) }
  | Term_data_variant { constructor } ->
    Term_data_variant { constructor = List.map constructor ~f:(close_data_constructor c) }

and close_data_field (c : Close.t) ({ name; ty } : Core.term_data_field)
  : Core.term_data_field
  =
  { name; ty = close_ty c ty }

and close_data_constructor (c : Close.t) ({ name; ty } : Core.term_data_constructor)
  : Core.term_data_constructor
  =
  { name; ty = Option.map ty ~f:(close_ty c) }

and close_field_impl (c : Close.t) ({ name; e } : Core.term_field_impl) =
  { name; e = close c e }

and close_arg (c : Close.t) ({ e; icit } : Core.term_arg) = { e = close c e; icit }

(* Since we only use the context size to generate fresh free variables, we can just use a really large context size *)
let quote_value = quote_value (Int.max_value / 2)
let quote_ty = quote_ty (Int.max_value / 2)

let struct_ty_of_iterated_binders (field_specs : Core.term_field_spec list) =
  let res =
    List.fold_map
      ~init:Core.Seq.empty
      ~f:(fun running_env ({ name; ty; relevancy } : Core.term_field_spec) ->
        let field_spec : Core.value_field_spec =
          { name; ty = eval_ty running_env ty; relevancy }
        in
        running_env, field_spec)
  in
  failwith ""
;;

module Term = struct
  let close = close
  let close_single = close_single
  let eval = eval_value
end

module Term_ty = struct
  let close = close_ty
  let close_single = close_ty_single
  let eval = eval_ty
end

module Struct = struct
  let proj = proj_struct
end

module Struct_ty = struct
  let proj = proj_struct_ty
end

module Ty_struct = Struct_ty

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
  let quote = quote_value
  let proj = proj_value
  let app = app_value
  let out = out_value
  let decode = decode_value
end

module Ty = struct
  let infer_props = infer_props
  let whnf = whnf_ty
  let eval = eval_ty
  let quote = quote_ty
  let proj = proj_ty
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
