open Prelude
open Oak_syntax

(* This evaluates only terms that have identity, so all terms that have type in universe at least Kind.
   This means it ignores all runtime terms.
*)
let rec eval (env : Env.t) (term : term) : value =
  match term with
  | Term_bound var -> Env.find_index_exn env var
  | Term_free var -> Value.free var
  | Term_app { func; arg; icit } ->
    let func = eval env func in
    let arg = eval env arg in
    app_value func arg icit
  | Term_abs { var; body; icit } -> Value_abs { var; body = { env; body }; icit }
  | Term_ty_fun { var; param_ty; icit; body_ty } ->
    let param_ty = eval env param_ty in
    Value_ty_fun { var; param_ty; icit; body_ty = { env; body = body_ty } }
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
  | Term_sing_out e ->
    let e = eval env e in
    out_value e
  | Term_literal _ | Term_pack _ | Term_ignore | Term_if _ | Term_bind _ -> Value_ignore
  | Term_ty_meta meta -> Value_ty_meta meta
  | Term_rec decls ->
    let fields =
      List.map decls ~f:(fun decl -> { name = decl.var.name; e = Value_ignore })
    in
    Value_mod { fields }

and eval_closure1 closure arg = eval (Env.push arg closure.env) closure.body

and proj_mod_ty (mod_e : value) (ty : value_ty_mod_closure) field_index =
  let ty_decl = List.drop ty.ty_decls field_index |> List.hd_exn in
  let env =
    List.take ty.ty_decls field_index
    |> List.foldi ~init:ty.env ~f:(fun field_index env ty_decl ->
      Env.push (proj_value mod_e ty_decl.var.name field_index) env)
  in
  eval env ty_decl.ty

and proj_ty (ty_env : Env.t) (mod_e : value) (ty : ty) field_index =
  proj_mod_ty mod_e (Whnf.ty_mod_val_exn (unfold ty_env ty)) field_index

and app_fun_ty (ty : value_ty_fun) arg = eval_closure1 ty.body_ty arg

and app_ty (ty_env : Env.t) (ty : ty) arg =
  app_fun_ty (Whnf.ty_fun_val_exn (unfold ty_env ty)) arg

and out_ty ty_env ty =
  let ty = unfold ty_env ty |> Whnf.ty_sing_val_exn in
  ty.ty

and app_value func arg icit =
  begin match func with
  | Value_ignore -> Value_ignore
  | Value_abs func -> app_abs func arg
  | Value_neutral { head; spine } ->
    Value_neutral { head; spine = spine <: Elim_app { arg; icit } }
  | _ -> assert false
  end

and proj_value (mod_e : value) (field : string) (field_index : int) =
  begin match mod_e with
  | Value_ignore -> Value_ignore
  | Value_mod mod_e -> proj_mod mod_e field_index
  | Value_neutral { head; spine } ->
    Value_neutral { head; spine = spine <: Elim_proj { field; field_index } }
  | _ -> assert false
  end

and out_value e =
  begin match e with
  | Value_ignore -> failwith "singleton cannot be ignored for implementation simplicity"
  | Value_sing_in e -> e
  | Value_neutral { head; spine } -> Value_neutral { head; spine = spine <: Elim_out }
  | _ -> assert false
  end

and unfold ty_env (e : value) : whnf =
  match e with
  | Value_ignore -> Value_ignore
  | Value_mod { fields } -> Value_mod { fields }
  | Value_abs abs -> Value_abs abs
  | Value_sing_in e -> Value_sing_in e
  | Value_core_ty ty -> Value_core_ty ty
  | Value_neutral neutral -> unfold_neutral ty_env neutral
  | Value_universe u -> Value_universe u
  | Value_ty_meta meta -> begin
    (* TODO: perform some path compression *)
    match meta.state with
    | Meta_unsolved -> Value_ty_meta { meta }
    | Meta_link ty | Meta_solved ty -> unfold ty_env ty
  end
  | Value_ty_sing sing -> Value_ty_sing sing
  | Value_ty_mod ty_mod -> Value_ty_mod ty_mod
  | Value_ty_fun ty_fun -> Value_ty_fun ty_fun
  | Value_ty_pack ty -> Value_ty_pack ty

and unfold_neutral ty_env (e : neutral) : whnf =
  let e, _ty =
    Bwd.fold_left
      e.spine
      ~init:
        (Value_neutral { head = e.head; spine = Empty }, Env.find_level_exn ty_env e.head)
      ~f:(fun (e, (ty : ty)) elim ->
        match elim with
        | Elim_app { arg; icit } -> app_whnf ty_env e arg icit, app_ty ty_env ty arg
        | Elim_proj { field; field_index } ->
          ( proj_whnf ty_env e field field_index
          , proj_ty ty_env (Whnf.to_value e) ty field_index )
        | Elim_out ->
          let ty = Whnf.ty_sing_val_exn (unfold ty_env ty) in
          unfold ty_env ty.identity, ty.ty)
  in
  e

and app_abs (abs : value_abs) arg = eval_closure1 abs.body arg

and proj_mod (mod_e : value_mod) (index : int) : value =
  let field = List.drop mod_e.fields index |> List.hd_exn in
  field.e

and app_whnf ty_env func arg icit : whnf =
  begin match func with
  | Value_ignore -> Value_ignore
  | Value_abs func -> unfold ty_env (app_abs func arg)
  | Value_neutral { head; spine } ->
    Value_neutral { head; spine = spine <: Whnf_elim_app { arg; icit } }
  | _ -> assert false
  end

and proj_whnf ty_env mod_e field field_index : whnf =
  begin match mod_e with
  | Value_ignore -> Value_ignore
  | Value_mod mod_e -> unfold ty_env (proj_mod mod_e field_index)
  | Value_neutral { head; spine } ->
    Value_neutral { head; spine = spine <: Whnf_elim_proj { field; field_index } }
  | _ -> assert false
  end
;;

let next_free_of_size size = Value.free (Level.of_int size)
let next_free_of_env env = next_free_of_size (Env.size env)

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

(* This should only be used in irrelevant contexts *)
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
  | Value_abs { var; body; icit } ->
    let body =
      eval_closure1 body (next_free_of_size context_size)
      |> quote (context_size + 1)
      |> close_single (Level.of_int context_size)
    in
    Term_abs { var; body; icit }
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
  | Value_ty_fun { var; param_ty; icit; body_ty } ->
    let param_ty = quote context_size param_ty in
    let body_ty =
      eval_closure1 body_ty (next_free_of_size context_size)
      |> quote (context_size + 1)
      |> close_single (Level.of_int context_size)
    in
    Term_ty_fun { var; param_ty; icit; body_ty }
  | Value_ty_mod ty ->
    let _, ty_decls =
      List.fold_map
        ty.ty_decls
        ~init:(context_size, ty.env, Close.empty)
        ~f:(fun (context_size, closure_env, (c : Close.t)) { var; ty } ->
          let ty = eval closure_env ty |> quote context_size |> close c in
          ( ( context_size + 1
            , Env.push (next_free_of_size context_size) closure_env
            , Close.add_exn (Level.of_int context_size) Index.zero (Close.lift 1 c) )
          , ({ var; ty } : term_ty_decl) ))
    in
    Term_ty_mod { ty_decls }
  | Value_ty_pack ty ->
    let ty = quote context_size ty in
    Term_ty_pack ty
  | Value_ty_meta meta -> Term_ty_meta meta

and quote_neutral context_size (e : neutral) : term =
  Bwd.fold_left e.spine ~init:(Term_free e.head) ~f:(fun e elim ->
    match elim with
    | Elim_proj { field; field_index } -> Term_proj { mod_e = e; field; field_index }
    | Elim_app { arg; icit } ->
      let arg = quote context_size arg in
      Term_app { func = e; arg; icit }
    | Elim_out -> Term_sing_out e)

and close (c : Close.t) e =
  match e with
  | Term_bound v -> Term_bound v
  | Term_free i ->
    Close.find c i |> Option.value_map ~default:e ~f:(fun v -> Term_bound v)
  | Term_app { func; arg; icit } ->
    Term_app { func = close c func; arg = close c arg; icit }
  | Term_abs { var; body; icit } ->
    Term_abs { var; body = close (Close.lift 1 c) body; icit }
  | Term_ty_fun { var; param_ty; icit; body_ty } ->
    Term_ty_fun
      { var; param_ty = close c param_ty; icit; body_ty = close (Close.lift 1 c) body_ty }
  | Term_proj { mod_e; field; field_index } ->
    Term_proj { mod_e = close c mod_e; field; field_index }
  | Term_mod { fields } ->
    Term_mod
      { fields =
          List.map fields ~f:(fun { name; e } -> ({ name; e = close c e } : term_field))
      }
  | Term_ty_mod { ty_decls } ->
    let _, ty_decls =
      List.fold_map ty_decls ~init:0 ~f:(fun under { var; ty } ->
        under + 1, ({ var; ty = close (Close.lift under c) ty } : term_ty_decl))
    in
    Term_ty_mod { ty_decls }
  | Term_let { var; rhs; body } ->
    Term_let { var; rhs = close c rhs; body = close (Close.lift 1 c) body }
  | Term_ty_sing { identity; ty } ->
    Term_ty_sing { identity = close c identity; ty = close c ty }
  | Term_sing_in e -> Term_sing_in (close c e)
  | Term_sing_out e -> Term_sing_out (close c e)
  | Term_ty_pack ty -> Term_ty_pack (close c ty)
  | Term_pack e -> Term_pack (close c e)
  | Term_bind { var; rhs; body } ->
    Term_bind { var; rhs = close c rhs; body = close (Close.lift 1 c) body }
  | Term_universe u -> Term_universe u
  | Term_core_ty ty -> Term_core_ty ty
  | Term_literal lit -> Term_literal lit
  | Term_ignore -> Term_ignore
  | Term_if { cond; body1; body2 } ->
    Term_if { cond = close c cond; body1 = close c body1; body2 = close c body2 }
  | Term_ty_meta meta -> begin
    match meta.state with
    | Meta_unsolved | Meta_link _ ->
      begin match Map.min_elt c.map with
      | Some (smallest_level, _) when smallest_level >= meta.context_size -> ()
      (* Otherwise, we are closing over a variable that can be a part of the meta's solution, which is invalid *)
      | Some _ -> failwith "cannot close over meta variable that we are currently solving"
      | None -> ()
      end;
      Term_ty_meta meta
    | Meta_solved ty -> close c (quote meta.context_size ty)
  end
  | Term_rec decls ->
    let c = Close.lift (List.length decls) c in
    let decls =
      List.map decls ~f:(fun { var; e } -> ({ var; e = close c e } : term_rec_decl))
    in
    Term_rec decls

and close_single (level : Level.t) e = close (Close.singleton level (Index.of_int 0)) e

module Mod = struct
  let proj = proj_mod
end

module Mod_ty = struct
  let proj = proj_mod_ty
end

module Abs = struct
  let app = app_abs
end

module Fun_ty = struct
  let app = app_fun_ty
end

module Value = struct
  let proj = proj_value
  let app = app_value
  let out = out_value
end

module Ty = struct
  let proj = proj_ty
  let app = app_ty
  let out = out_ty
end
