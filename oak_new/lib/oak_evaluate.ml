open Prelude
open Oak_syntax

let rec eval_value (env : env) (e : term) : value =
  match e with
  | Term_bound index -> Seq.get_index_exn env index
  | Term_free level -> Value.free level
  | Term_app { func; arg } ->
    let func = eval_value env func in
    let arg = eval_arg env arg in
    app_value func arg
  | Term_fun { name; param_props; body } ->
    Value_fun { name; param_props; body = { env; body } }
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
    eval_value (Seq.push rhs env) body
  | Term_ignore -> Value_ignore
  | Term_encode_ty { ty; props } ->
    let ty = eval_ty env ty in
    Value_encode_ty { ty; props }

and eval_ty (env : env) (ty : term_ty) : ty =
  match ty with
  | Term_ty_decode e ->
    let e = eval_value env e in
    decode_value e
  | Term_ty_fun { name; param_ty; param_props; body_ty } ->
    let param_ty = eval_ty env param_ty in
    Ty_fun { name; param_props; param_ty; body_ty = { env; body = body_ty } }
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

and eval_field_impl env ({ name; e } : term_field_impl) : value_field_impl =
  let e = eval_value env e in
  { name; e }

and eval_arg env ({ e; param_props } : term_arg) : value_arg =
  let e = eval_value env e in
  { e; param_props }

and decode_value (ty : value) : ty =
  match ty with
  | Value_encode_ty { ty; props = _ } -> ty
  | Value_neutral e -> Ty_decode e
  | _ -> failwith "Expected a type code"

and app_value (func : value) (arg : value_arg) =
  match func with
  | Value_ignore ->
    (* Function types can have kind Type *)
    Value_ignore
  | Value_fun func -> app_fun func arg
  | Value_neutral { head; spine } -> Value_neutral { head; spine = spine <: App arg }
  | _ -> failwith "Expected function value"

and proj_value (strukt : value) (field : field_loc) =
  (* No ignore case here because structures always have kind Sig *)
  match strukt with
  | Value_struct strukt -> proj_struct strukt field
  | Value_neutral { head; spine } -> Value_neutral { head; spine = spine <: Proj field }
  | _ -> failwith "Expected a struct value"

and out_value (sing : value) =
  match sing with
  | Value_sing_in e -> e
  | Value_neutral { head; spine } -> Value_neutral { head; spine = spine <: Out }
  | _ -> failwith "Expected a singleton value"

and proj_struct (strukt : value_struct) (field : field_loc) =
  let field_impl = List.drop strukt.field_impls field.index |> List.hd_exn in
  field_impl.e

and app_fun (abs : value_fun) (arg : value_arg) = eval_closure1 abs.body arg.e
and eval_closure1 closure arg = eval_value (Seq.push arg closure.env) closure.body

and eval_ty_closure1 (closure : ty_closure) arg =
  eval_ty (Seq.push arg closure.env) closure.body

and whnf_value ty_env (e : value) : value =
  match e with
  | Value_neutral neutral -> whnf_neutral ty_env neutral
  | Value_ignore | Value_struct _ | Value_fun _ | Value_sing_in _ | Value_encode_ty _ -> e

and whnf_ty (ty_env : ty_env) (ty : ty) : ty =
  match ty with
  | Ty_decode e -> whnf_ty ty_env (decode_value (whnf_neutral ty_env e))
  | Ty_universe _ | Ty_sing _ | Ty_struct _ | Ty_fun _ | Ty_core _ | Ty_pack _ -> ty

and app_fun_ty (func_ty : ty_fun) (arg : value_arg) : ty =
  eval_ty_closure1 func_ty.body_ty arg.e

and app_ty (ty_env : ty_env) (ty : ty) (arg : value_arg) : ty =
  app_fun_ty (Ty.ty_fun_val_exn (whnf_ty ty_env ty)) arg

and out_ty (ty_env : ty_env) (ty : ty) : ty = (Ty.ty_sing_val_exn (whnf_ty ty_env ty)).ty

and proj_struct_ty (strukt : value) (struct_ty : ty_struct) (field : field_loc) : ty =
  let field_spec = List.drop struct_ty.field_specs field.index |> List.hd_exn in
  let env =
    List.take struct_ty.field_specs field.index
    |> List.foldi ~init:struct_ty.env ~f:(fun index env field_spec ->
      Seq.push (proj_value strukt { name = field_spec.name.name; index }) env)
  in
  eval_ty env field_spec.ty

and proj_ty (ty_env : ty_env) (strukt : value) (ty : ty) (field : field_loc) : ty =
  proj_struct_ty strukt (Ty.ty_struct_val_exn (whnf_ty ty_env ty)) field

and whnf_neutral (ty_env : ty_env) (e : neutral) : value =
  let e, _ty =
    Bwd.fold_left
      e.spine
      ~init:
        (Value_neutral { head = e.head; spine = Empty }, Seq.get_level_exn ty_env e.head)
      ~f:(fun (e, ty) frame ->
        match frame with
        | App arg -> whnf_value ty_env (app_value e arg), app_ty ty_env ty arg
        | Proj field -> whnf_value ty_env (proj_value e field), proj_ty ty_env e ty field
        | Out ->
          let ty =
            match whnf_ty ty_env ty with
            | Ty_sing sing -> sing
            | _ -> failwith "Expected singleton type"
          in
          whnf_value ty_env ty.identity, ty.ty)
  in
  e
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

  let singleton (level : Level.t) (index : Index.t) : t =
    { map = Int.Map.singleton level.level index.index; lift = 0 }
  ;;

  let add_exn (level : Level.t) (index : Index.t) (close : t) =
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

  let find (close : t) (level : Level.t) =
    Option.map
      ~f:(fun i -> Index.of_int (i + close.lift))
      (Map.find close.map level.level)
  ;;
end

let rec quote context_size (e : value) : term =
  match e with
  | Value_ignore -> Term_ignore
  | Value_struct { field_impls } ->
    let field_impls = List.map field_impls ~f:(quote_field_impl context_size) in
    Term_struct { field_impls }
  | Value_fun { name; body; param_props } ->
    let body =
      eval_closure1 body (Value.free_of_size context_size)
      |> quote (context_size + 1)
      |> close_single (Level.of_int context_size)
    in
    Term_fun { name; body; param_props }
  | Value_sing_in e -> Term_sing_in (quote context_size e)
  | Value_neutral e -> quote_neutral context_size e
  | Value_encode_ty _ -> failwith ""

and quote_ty context_size (ty : ty) : term_ty =
  match ty with
  | Ty_universe props -> Term_ty_universe props
  | Ty_sing { identity; ty } ->
    let identity = quote context_size identity in
    let ty = quote_ty context_size ty in
    Term_ty_sing { identity; ty }
  | Ty_struct { env; field_specs } ->
    let _, field_specs =
      List.fold_map
        field_specs
        ~init:(context_size, env, Close.empty)
        ~f:(fun (context_size, closure_env, c) { name; ty; relevancy } ->
          let ty = eval_ty closure_env ty |> quote_ty context_size |> close_ty c in
          ( ( context_size + 1
            , Seq.push (Value.free_of_size context_size) closure_env
            , Close.add_exn (Level.of_int context_size) Index.zero (Close.lift 1 c) )
          , ({ name; ty; relevancy } : term_field_spec) ))
    in
    Term_ty_struct { field_specs }
  | Ty_fun { name; param_props; param_ty; body_ty } ->
    let param_ty = quote_ty context_size param_ty in
    let body_ty =
      eval_ty_closure1 body_ty (Value.free_of_size context_size)
      |> quote_ty (context_size + 1)
      |> close_ty_single (Level.of_int context_size)
    in
    Term_ty_fun { name; param_props; param_ty; body_ty }
  | Ty_core ty -> Term_ty_core ty
  | Ty_pack ty -> Term_ty_pack (quote_ty context_size ty)
  | Ty_decode e ->
    let e = quote_neutral context_size e in
    Term_ty_decode e

and quote_neutral context_size (e : neutral) : term =
  Bwd.fold_left e.spine ~init:(Term_free e.head) ~f:(fun e elim ->
    match elim with
    | Proj field -> Term_proj { strukt = e; field }
    | App arg ->
      let arg = quote_arg context_size arg in
      Term_app { func = e; arg }
    | Out -> Term_sing_out e)

and quote_arg context_size (arg : value_arg) : term_arg =
  let e = quote context_size arg.e in
  { e; param_props = arg.param_props }

and quote_field_impl (context_size : int) (field_impl : value_field_impl)
  : term_field_impl
  =
  let e = quote context_size field_impl.e in
  { name = field_impl.name; e }

and close (c : Close.t) (e : term) : term =
  match e with
  | Term_bound v -> Term_bound v
  | Term_free i ->
    Close.find c i |> Option.value_map ~default:e ~f:(fun v -> Term_bound v)
  | Term_app { func; arg } -> Term_app { func = close c func; arg = close_arg c arg }
  | Term_fun { name; param_props; body } ->
    Term_fun { name; param_props; body = close (Close.lift 1 c) body }
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

and close_single (level : Level.t) e = close (Close.singleton level (Index.of_int 0)) e

and close_ty_single (level : Level.t) ty =
  close_ty (Close.singleton level (Index.of_int 0)) ty

and close_ty (c : Close.t) (ty : term_ty) : term_ty =
  match ty with
  | Term_ty_decode e ->
    let e = close c e in
    Term_ty_decode e
  | Term_ty_fun { name; param_ty; param_props; body_ty } ->
    let param_ty = close_ty c param_ty in
    let body_ty = close_ty (Close.lift 1 c) body_ty in
    Term_ty_fun { name; param_props; param_ty; body_ty }
  | Term_ty_struct { field_specs } ->
    let _, field_specs =
      List.fold_map field_specs ~init:0 ~f:(fun under { name; ty; relevancy } ->
        ( under + 1
        , ({ name; ty = close_ty (Close.lift under c) ty; relevancy } : term_field_spec) ))
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

and close_field_impl (c : Close.t) ({ name; e } : term_field_impl) =
  { name; e = close c e }

and close_arg (c : Close.t) ({ e; param_props } : term_arg) =
  { e = close c e; param_props }
;;

module Struct = struct
  let proj = proj_struct
end

module Struct_ty = struct
  let proj = proj_struct_ty
end

module Fun = struct
  let app = app_fun
end

module Fun_ty = struct
  let app = app_fun_ty
end

module Value = struct
  let proj = proj_value
  let app = app_value
  let out = out_value
  let decode = decode_value
end

module Ty = struct
  let proj = proj_ty
  let app = app_ty
  let out = out_ty
end
