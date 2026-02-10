open Prelude
open Oak_syntax

(* This evaluates only terms that have identity, so all terms that have type in universe at least Kind.
   This means it ignores all runtime terms.
*)
let rec eval (env : Env.t) (term : term) : value =
  match term with
  | Term_var var -> Env.find_exn env var
  | Term_app { func; arg } ->
    let func = eval env func in
    let arg = eval env arg in
    app_value func arg
  | Term_abs { var; body } -> Value_abs { var; body = { env; body } }
  | Term_ty_fun { var; param_ty; body_ty } ->
    let param_ty = eval env param_ty in
    Value_ty_fun { var; param_ty; body_ty = { env; body = body_ty } }
  | Term_proj { mod_e; field; field_index } ->
    let mod_e = eval env mod_e in
    proj_value mod_e field field_index
  | Term_mod { fields } ->
    let fields =
      List.map fields ~f:(fun { name; e } -> ({ name; e = eval env e } : value_field))
    in
    Value_mod { fields }
  | Term_ty_mod { ty_decls } -> Value_ty_mod { env; ty_decls }
  | Term_let { var = _; rhs; body } ->
    let rhs = eval env rhs in
    eval (Env.push rhs env) body
  | Term_ty_sing { identity; ty } ->
    let identity = eval env identity in
    let ty = eval env ty in
    Value_ty_sing { identity; ty }
  | Term_ty_pack ty ->
    let ty = eval env ty in
    Value_ty_pack ty
  | Term_universe universe -> Value_universe universe
  | Term_core_ty ty -> Value_core_ty ty
  | Term_sing_in e ->
    let e = eval env e in
    Value_sing_in e
  | Term_sing_out { identity; e } ->
    let identity = eval env identity in
    let e = eval env e in
    unwrap_value identity e
  | Term_weaken term -> eval (Env.pop_exn env) term
  | Term_unit | Term_pack _ | Term_bind _ | Term_ignore | Term_if _ | Term_bool _ ->
    Value_ignore

and unwrap_value identity e =
  begin match e with
  | Value_ignore -> identity
  | Value_sing_in e -> e
  | Value_neutral { head; spine } ->
    Value_neutral { head; spine = spine <: Elim_out { identity } }
  | _ -> assert false
  end

and app_value func arg =
  begin match func with
  | Value_ignore -> Value_ignore
  | Value_abs func -> app_abs func arg
  | Value_neutral { head; spine } -> Value_neutral { head; spine = spine <: Elim_app arg }
  | _ -> assert false
  end

and proj_value mod_e field field_index =
  begin match mod_e with
  | Value_ignore -> Value_ignore
  | Value_mod mod_e -> proj_mod mod_e field_index
  | Value_neutral { head; spine } ->
    Value_neutral { head; spine = spine <: Elim_proj { field; field_index } }
  | _ -> assert false
  end

and app_abs (abs : value_abs) arg = eval_closure1 abs.body arg

and proj_mod (mod_e : value_mod) index =
  let field = List.drop mod_e.fields index |> List.hd_exn in
  field.e

and eval_closure1 closure arg = eval (Env.push arg closure.env) closure.body

and eval_ty_mod_closure e (ty : value_ty_mod_closure) field_index =
  let ty_decl = List.drop ty.ty_decls field_index |> List.hd_exn in
  let env =
    List.take ty.ty_decls field_index
    |> List.foldi ~init:ty.env ~f:(fun field_index env ty_decl ->
      Env.push (proj_value e ty_decl.var.name field_index) env)
  in
  eval env ty_decl.ty
;;

(* Apply singleton extension rule to remove Neutral_sing_out.*)
let rec unfold (e : value) : uvalue =
  match e with
  | Value_ignore -> Uvalue_ignore
  | Value_mod { fields } -> Uvalue_mod { fields }
  | Value_abs abs -> Uvalue_abs abs
  | Value_sing_in e -> Uvalue_sing_in e
  | Value_core_ty ty -> Uvalue_core_ty ty
  | Value_neutral neutral -> unfold_neutral neutral
  | Value_universe u -> Uvalue_universe u
  | Value_ty_sing sing -> Uvalue_ty_sing sing
  | Value_ty_mod ty_mod -> Uvalue_ty_mod ty_mod
  | Value_ty_fun ty_fun -> Uvalue_ty_fun ty_fun
  | Value_ty_pack ty -> Uvalue_ty_pack ty

and unfold_neutral (e : neutral) : uvalue =
  Bwd.fold_left
    e.spine
    ~init:(Uvalue_neutral { head = e.head; spine = Empty })
    ~f:(fun e elim ->
      match elim with
      | Elim_app arg -> app_uvalue e arg
      | Elim_proj { field; field_index } -> proj_uvalue e field field_index
      | Elim_out { identity } -> unfold identity)

and app_uvalue func arg =
  begin match func with
  | Uvalue_abs func -> unfold (app_abs func arg)
  | Uvalue_neutral { head; spine } ->
    Uvalue_neutral { head; spine = spine <: Uelim_app arg }
  | _ -> assert false
  end

and proj_uvalue mod_e field field_index =
  begin match mod_e with
  | Uvalue_mod mod_e -> unfold (proj_mod mod_e field_index)
  | Uvalue_neutral { head; spine } ->
    Uvalue_neutral { head; spine = spine <: Uelim_proj { field; field_index } }
  | _ -> assert false
  end
;;

let next_var_of_size size = Value.var (Level.of_int size)
let next_var_of_env env = next_var_of_size (Env.size env)

let rec quote context_size (e : value) : term =
  match e with
  | Value_ignore -> Term_ignore
  | Value_mod { fields } ->
    let fields =
      List.map fields ~f:(fun { name; e } ->
        let e = quote context_size e in
        ({ name; e } : term_field))
    in
    Term_mod { fields }
  | Value_abs { var; body } ->
    let body =
      quote (context_size + 1) (eval_closure1 body (next_var_of_size context_size))
    in
    Term_abs { var; body }
  | Value_sing_in e ->
    let e = quote context_size e in
    Term_sing_in e
  | Value_core_ty ty -> Term_core_ty ty
  | Value_neutral neutral -> quote_neutral context_size neutral
  | Value_universe universe -> Term_universe universe
  | Value_ty_sing { identity; ty } ->
    let identity = quote context_size identity in
    let ty = quote context_size ty in
    Term_ty_sing { identity; ty }
  | Value_ty_fun { var; param_ty; body_ty } ->
    let param_ty = quote context_size param_ty in
    let body_ty =
      quote (context_size + 1) (eval_closure1 body_ty (next_var_of_size context_size))
    in
    Term_ty_fun { var; param_ty; body_ty }
  | Value_ty_mod ty ->
    let _, ty_decls =
      List.fold_map
        ty.ty_decls
        ~init:(context_size, ty.env)
        ~f:(fun (context_size, closure_env) { var; ty } ->
          let ty = quote context_size (eval closure_env ty) in
          ( (context_size + 1, Env.push (next_var_of_size context_size) closure_env)
          , ({ var; ty } : term_ty_decl) ))
    in
    Term_ty_mod { ty_decls }
  | Value_ty_pack ty ->
    let ty = quote context_size ty in
    Term_ty_pack ty

and quote_neutral context_size (e : neutral) : term =
  Bwd.fold_left
    e.spine
    ~init:(Term_var (Index.of_level context_size e.head))
    ~f:(fun e elim ->
      match elim with
      | Elim_proj { field; field_index } -> Term_proj { mod_e = e; field; field_index }
      | Elim_app arg ->
        let arg = quote context_size arg in
        Term_app { func = e; arg }
      | Elim_out { identity } ->
        let identity = quote context_size identity in
        Term_sing_out { identity; e })
;;
