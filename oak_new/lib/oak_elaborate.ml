(*
  Todo, only have Var_info for bound variables and not free variables.
*)
open Prelude
open Oak_syntax

exception Error of Error.t

let fail e = raise_notrace (Error e)
let fail_s s = fail (Error.t_of_sexp s)

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
  | Term_sing_wrap { identity; e } ->
    let identity = eval env identity in
    let e = eval env e in
    Value_sing_wrap { identity; e }
  | Term_sing_unwrap { identity; e } ->
    let identity = eval env identity in
    let e = eval env e in
    unwrap_value identity e
  | Term_weaken term -> eval (Env.pop_exn env) term
  | Term_pack _ | Term_bind _ | Term_ignore | Term_if _ | Term_bool _ -> Value_ignore

and unwrap_value identity e =
  begin match e with
  | Value_ignore -> identity
  | Value_sing_wrap { identity = _; e = e' } -> e'
  | Value_neutral e -> Value_neutral (Neutral_sing_unwrap { identity; e })
  | _ -> assert false
  end

and app_value func arg =
  begin match func with
  | Value_ignore -> Value_ignore
  | Value_abs func -> app_abs func arg
  | Value_neutral func -> Value_neutral (Neutral_app { func; arg })
  | _ -> assert false
  end

and proj_value mod_e field field_index =
  begin match mod_e with
  | Value_ignore -> Value_ignore
  | Value_mod mod_e -> proj_mod mod_e field_index
  | Value_neutral mod_e -> Value_neutral (Neutral_proj { mod_e; field; field_index })
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

(* Apply singleton extension rule to remove Neutral_sing_unwrap.*)
let rec unfold (e : value) : uvalue =
  match e with
  | Value_ignore -> Uvalue_ignore
  | Value_mod { fields } -> Uvalue_mod { fields }
  | Value_abs abs -> Uvalue_abs abs
  | Value_sing_wrap { identity; e } -> Uvalue_sing_wrap { identity; e }
  | Value_core_ty ty -> Uvalue_core_ty ty
  | Value_neutral neutral -> unfold_neutral neutral
  | Value_universe u -> Uvalue_universe u
  | Value_ty_sing sing -> Uvalue_ty_sing sing
  | Value_ty_mod ty_mod -> Uvalue_ty_mod ty_mod
  | Value_ty_fun ty_fun -> Uvalue_ty_fun ty_fun
  | Value_ty_pack ty -> Uvalue_ty_pack ty

and unfold_neutral (e : neutral) : uvalue =
  match e with
  | Neutral_var var -> Uvalue_neutral (Uneutral_var var)
  | Neutral_app { func; arg } ->
    let func = unfold_neutral func in
    app_uvalue func arg
  | Neutral_proj { mod_e; field; field_index } ->
    let mod_e = unfold_neutral mod_e in
    proj_uvalue mod_e field field_index
  | Neutral_sing_unwrap { identity; e = _ } -> unfold identity

and app_uvalue func arg =
  begin match func with
  | Uvalue_abs func -> unfold (app_abs func arg)
  | Uvalue_neutral func -> Uvalue_neutral (Uneutral_app { func; arg })
  | _ -> assert false
  end

and proj_uvalue mod_e field field_index =
  begin match mod_e with
  | Uvalue_mod mod_e -> unfold (proj_mod mod_e field_index)
  | Uvalue_neutral mod_e -> Uvalue_neutral (Uneutral_proj { mod_e; field; field_index })
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
  | Value_sing_wrap { identity; e } ->
    let identity = quote context_size identity in
    let e = quote context_size e in
    Term_sing_wrap { identity; e }
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
  match e with
  | Neutral_var var -> Term_var (Index.of_level context_size var)
  | Neutral_app { func; arg } ->
    let func = quote_neutral context_size func in
    let arg = quote context_size arg in
    Term_app { func; arg }
  | Neutral_proj { mod_e; field; field_index } ->
    let mod_e = quote_neutral context_size mod_e in
    Term_proj { mod_e; field; field_index }
  | Neutral_sing_unwrap { identity; e } ->
    let identity = quote context_size identity in
    let e = quote_neutral context_size e in
    Term_sing_unwrap { identity; e }
;;

module Context = struct
  type t =
    { value_env : Env.t (* These are all just bound variables *)
    ; ty_env : Env.t
    ; var_info : Var_info.t list
    }

  let empty = { value_env = Env.empty; ty_env = Env.empty; var_info = [] }

  let bind var ty cx =
    { cx with
      value_env =
        Env.push
          (Value_neutral (Neutral_var (Level.of_int (Env.size cx.value_env))))
          cx.value_env
    ; ty_env = Env.push ty cx.ty_env
    ; var_info = var :: cx.var_info
    }
  ;;

  let size (cx : t) = Env.size cx.ty_env
  let next_var cx = Value.var (Level.of_int (size cx))
  let eval cx e = eval cx.value_env e
  let quote (cx : t) e = quote (Env.size cx.ty_env) e
  let var_ty cx (var : Index.t) = Env.find_exn cx.ty_env var

  let level_var_ty cx (var : Level.t) =
    Env.find_exn cx.ty_env (Index.of_level (size cx) var)
  ;;
end

(* precondition: the neutral must be well typed, but it *doesn't* have to be an element of some universe. *)
let rec infer_neutral (ty_env : Env.t) (e : neutral) : value =
  match e with
  | Neutral_var var ->
    let index = Index.of_level (Env.size ty_env) var in
    let ty = Env.find_exn ty_env index in
    ty
  | Neutral_app { func; arg } ->
    let func_ty = infer_neutral ty_env func |> unfold |> Uvalue.ty_fun_val_exn in
    eval_closure1 func_ty.body_ty arg
  | Neutral_proj { mod_e; field = _; field_index } ->
    let mod_ty = infer_neutral ty_env mod_e |> unfold |> Uvalue.ty_mod_val_exn in
    eval_ty_mod_closure (Value_neutral mod_e) mod_ty field_index
  | Neutral_sing_unwrap { identity = _; e } ->
    (*
      We ignore identity here instead of inferring from it because we can only reliably infer universes from values.
      It can also be faster to avoid infering the identity and infer the neutral instead.
      Imagine if the identity is a huge signature, while e is just a neutral, so it could be a variable.
    *)
    let e_ty = infer_neutral ty_env e |> unfold |> Uvalue.ty_sing_val_exn in
    e_ty.ty
;;

(* precondition: e1 and e2 must have type ty. ty must be an element of some universe. *)
let rec conv ty_env (e1 : value) (e2 : value) (ty : value) : unit =
  match unfold ty with
  | Uvalue_ignore | Uvalue_mod _ | Uvalue_abs _ | Uvalue_sing_wrap _ ->
    raise_s [%message "Not a type" (ty : value)]
  (* These have kind Type, so can be ignored *)
  | Uvalue_ty_pack _ | Uvalue_core_ty _ -> ()
  (* Singletons can be immediately eliminated using unwrap, and since both values have the same singleton type they must be equal. *)
  | Uvalue_ty_sing _ -> ()
  | Uvalue_universe _ ->
    (* both types are elements of some universe *)
    conv_ty ty_env e1 e2
  | Uvalue_neutral uneutral ->
    (* If e1 and e2 have a type that is neutral, we can think of their type as being abstract. *)
    let universe =
      infer_neutral ty_env (Uneutral.to_neutral uneutral)
      |> unfold
      |> Uvalue.universe_val_exn
    in
    begin match universe with
    (* abstract types can be ignored *)
    | Type -> ()
    | Kind | Sig ->
      let ty1 = unfold e1 |> Uvalue.neutral_val_exn in
      let ty2 = unfold e2 |> Uvalue.neutral_val_exn in
      let _ = conv_neutral ty_env ty1 ty2 in
      ()
    end
  | Uvalue_ty_fun ty ->
    let var_value = Env.next_var ty_env in
    conv
      (Env.push ty.param_ty ty_env)
      (app_value e1 var_value)
      (app_value e2 var_value)
      (eval_closure1 ty.body_ty var_value)
  | Uvalue_ty_mod ty ->
    let closure_env = ty.env in
    let _ =
      List.foldi ty.ty_decls ~init:closure_env ~f:(fun field_index closure_env ty_decl ->
        let e1 = proj_value e1 ty_decl.var.name field_index in
        let e2 = proj_value e2 ty_decl.var.name field_index in
        conv ty_env e1 e2 (eval closure_env ty_decl.ty);
        Env.push e1 closure_env)
    in
    ()

(*
  precondition: both ty1 and ty2 must be an element of some universe, but they may be different universes.
*)
and conv_ty (ty_env : Env.t) (ty1 : ty) (ty2 : ty) : unit =
  match unfold ty1, unfold ty2 with
  | Uvalue_ty_sing ty1, Uvalue_ty_sing ty2 ->
    conv_ty ty_env ty1.ty ty2.ty;
    (* we now know that ty1 = ty2 *)
    conv ty_env ty1.identity ty2.identity ty1.ty
  | Uvalue_universe universe1, Uvalue_universe universe2 ->
    if not (Universe.equal universe1 universe2)
    then fail_s [%message "Universes were not equal"]
  | Uvalue_ty_fun ty1, Uvalue_ty_fun ty2 ->
    conv_ty ty_env ty1.param_ty ty2.param_ty;
    conv_ty
      (Env.push ty1.param_ty ty_env)
      (eval_closure1 ty1.body_ty (Env.next_var ty_env))
      (eval_closure1 ty2.body_ty (Env.next_var ty_env))
  | Uvalue_ty_pack ty1, Uvalue_ty_pack ty2 -> conv_ty ty_env ty1 ty2
  | Uvalue_ty_mod ty1, Uvalue_ty_mod ty2 ->
    let zipped_ty_decls =
      match List.zip ty1.ty_decls ty2.ty_decls with
      | Ok t -> t
      | Unequal_lengths ->
        fail_s
          [%message
            "Record number of declarations was different"
              (ty1.ty_decls : term_ty_decl list)
              (ty2.ty_decls : term_ty_decl list)]
    in
    let _ =
      List.fold
        zipped_ty_decls
        ~init:(ty1.env, ty2.env, ty_env)
        ~f:(fun (closure_env1, closure_env2, cx) (ty_decl1, ty_decl2) ->
          let name1 = ty_decl1.var.name in
          let name2 = ty_decl2.var.name in
          if not (String.equal name1 name2)
          then
            fail_s
              [%message "Declaration name not equal" (name1 : string) (name2 : string)];
          let ty1 = eval closure_env1 ty_decl1.ty in
          let ty2 = eval closure_env2 ty_decl2.ty in
          conv_ty cx ty1 ty2;
          let var_value = Env.next_var ty_env in
          ( Env.push var_value closure_env1
          , Env.push var_value closure_env2
          , Env.push ty1 cx ))
    in
    ()
  | Uvalue_neutral ty1, Uvalue_neutral ty2 ->
    (* ty1 and ty2 are elements of some universe, and the kind of a universe is at least Kind *)
    let _ = conv_neutral ty_env ty1 ty2 in
    ()
  | _ -> fail_s [%message "Types were not equal" (ty1 : ty) (ty2 : ty)]

(*
  Precondition: if e1 : t1 and e2 : t2 then t1 : U_i and t2 : U_j where i > 0 and j > 0, so both types must have universe at least Kind.
  In other words, both inputs must be at least types.
  Checks if e1 = e2. Returns the kind that they are equivalent at.
*)
and conv_neutral (ty_env : Env.t) (ty1 : uneutral) (ty2 : uneutral) : value =
  match ty1, ty2 with
  | Uneutral_var ty1, Uneutral_var ty2 ->
    if not (Level.equal ty1 ty2)
    then fail_s [%message "Variables were not equal" (ty1 : Level.t) (ty2 : Level.t)];
    Env.find_exn ty_env (Index.of_level (Env.size ty_env) ty1)
  | Uneutral_app ty1, Uneutral_app ty2 ->
    (*
      This case is why we return the kind that the two neutrals are equivalent at,
      because we need the type of the two arguments to check them for equivalence, since they are values.
    *)
    let func_kind =
      conv_neutral ty_env ty1.func ty2.func |> unfold |> Uvalue.ty_fun_val_exn
    in
    conv ty_env ty1.arg ty2.arg func_kind.param_ty;
    eval_closure1 func_kind.body_ty ty1.arg
  | Uneutral_proj ty1, Uneutral_proj ty2 ->
    let kind =
      conv_neutral ty_env ty1.mod_e ty2.mod_e |> unfold |> Uvalue.ty_mod_val_exn
    in
    (*
      Must do this check after we check that the modules are equivalent.
      Otherwise it would not make sense to compare the indices.
    *)
    if not (ty1.field_index = ty2.field_index)
    then
      fail_s
        [%message
          "Fields were not equal in a projection"
            (ty1.field : string)
            (ty2.field : string)];
    eval_ty_mod_closure
      (Value_neutral (Uneutral.to_neutral ty1.mod_e))
      kind
      ty1.field_index
  | _ ->
    fail_s [%message "Neutrals were not equivalent" (ty1 : uneutral) (ty2 : uneutral)]
;;

let rec coerce_singleton context_size (e : term) (ty : ty) : term * uvalue =
  match unfold ty with
  | Uvalue_ty_sing { identity; ty = kind } ->
    let identity = quote context_size identity in
    coerce_singleton context_size (Term_sing_unwrap { identity; e }) kind
  | ty -> e, ty
;;

(*
  Check whether A <= B, and coerces the term if it is.
  Returns none when the types were equal, to prevent useless eta expansion
*)
let rec sub cx (e : term) (ty1 : ty) (ty2 : ty) : term option =
  match unfold ty1, unfold ty2 with
  | Uvalue_universe ty1, Uvalue_universe ty2 ->
    if not (Universe.equal ty1 ty2)
    then
      fail_s [%message "Universes were not equal" (ty1 : Universe.t) (ty2 : Universe.t)];
    None
  | Uvalue_core_ty ty1, Uvalue_core_ty ty2 ->
    if not (equal_core_ty ty1 ty2)
    then fail_s [%message "Core types were not equal" (ty1 : core_ty) (ty2 : core_ty)];
    None
  | Uvalue_ty_sing ty1, Uvalue_ty_sing ty2 ->
    let e' =
      sub
        cx
        (Term_sing_unwrap { identity = Context.quote cx ty1.identity; e })
        ty1.ty
        ty2.ty
    in
    begin match e' with
    | None ->
      conv cx.ty_env ty1.identity ty2.identity ty1.ty;
      None
    | Some e' ->
      conv cx.ty_env (Context.eval cx e') ty2.identity ty2.ty;
      Some (Term_sing_wrap { identity = e'; e = e' })
    end
  | Uvalue_ty_sing _, _ ->
    let e, ty1 = coerce_singleton (Context.size cx) e ty1 in
    sub cx e (Uvalue.to_value ty1) ty2
  | _, Uvalue_ty_sing ty2 ->
    let e = coerce cx e ty1 ty2.ty in
    conv cx.ty_env (Context.eval cx e) ty2.identity ty2.ty;
    Some (Term_sing_wrap { identity = e; e })
  | Uvalue_ty_fun ty1, Uvalue_ty_fun ty2 ->
    let var = ty2.var in
    let arg_var_value = Context.next_var cx in
    let cx = Context.bind var ty2.param_ty cx in
    let arg_var_term = Term_var (Index.of_int 0) in
    let arg = sub cx arg_var_term ty2.param_ty ty1.param_ty in
    begin match arg with
    | None ->
      let body =
        sub
          cx
          (Term_app { func = Term_weaken e; arg = arg_var_term })
          (eval_closure1 ty1.body_ty arg_var_value)
          (eval_closure1 ty2.body_ty arg_var_value)
      in
      Option.map body ~f:(fun body -> Term_abs { var; body })
    | Some arg ->
      let arg_value = Context.eval cx arg in
      let body =
        coerce
          cx
          (Term_app { func = Term_weaken e; arg })
          (eval_closure1 ty1.body_ty arg_value)
          (eval_closure1 ty2.body_ty arg_value)
      in
      Some (Term_abs { var; body })
    end
  | Uvalue_ty_mod ty1, Uvalue_ty_mod ty2 ->
    let value = Context.eval cx e in
    let ty1_map =
      List.fold_mapi ty1.ty_decls ~init:ty1.env ~f:(fun field_index closure_env ty_decl ->
        let proj_ty = eval closure_env ty_decl.ty in
        let field_name = ty_decl.var.name in
        ( Env.push (proj_value value field_name field_index) closure_env
        , (field_name, (field_index, proj_ty)) ))
      |> snd
      |> String.Map.of_alist_exn
    in
    let (did_coerce, _), fields =
      List.fold_map
        ty2.ty_decls
        ~init:(false, ty2.env)
        ~f:(fun (did_coerce, closure_env) ty2_decl ->
          let field_name = ty2_decl.var.name in
          let ty1_field_index, ty1_proj_ty =
            match Map.find ty1_map field_name with
            | None ->
              fail_s
                [%message
                  "Modules are not subtypes"
                    (ty1 : value_ty_mod_closure)
                    (ty2 : value_ty_mod_closure)
                    "Could not find field"
                    ~field:(ty2_decl.var.name : string)]
            | Some t -> t
          in
          let proj_term =
            Term_proj { mod_e = e; field = field_name; field_index = ty1_field_index }
          in
          let coerced_proj_term =
            sub cx proj_term ty1_proj_ty (eval closure_env ty2_decl.ty)
          in
          let did_coerce = did_coerce || Option.is_some coerced_proj_term in
          let coerced_proj_term = Option.value coerced_proj_term ~default:proj_term in
          let field : term_field = { name = field_name; e = coerced_proj_term } in
          (did_coerce, Env.push (Context.eval cx coerced_proj_term) closure_env), field)
    in
    if not did_coerce then None else Some (Term_mod { fields })
  | _ ->
    (* Otherwise fallback to checking for equivalence *)
    conv_ty cx.ty_env ty1 ty2;
    None

and coerce cx e ty1 ty2 = sub cx e ty1 ty2 |> Option.value ~default:e

(* precondition: value must be well typed and must be in some universe *)
let rec infer_value_universe (ty_env : Env.t) (e : value) : Universe.t =
  let panic () = raise_s [%message "value was not in a universe" (e : value)] in
  match e with
  | Value_mod _ | Value_abs _ | Value_ignore | Value_sing_wrap _ -> panic ()
  | Value_core_ty _ | Value_ty_pack _ -> Universe.Type
  | Value_neutral neutral ->
    infer_neutral ty_env neutral |> unfold |> Uvalue.universe_val_exn
  | Value_universe u -> Universe.incr_exn u
  | Value_ty_sing { identity = _; ty } ->
    (* We have the invariant that the universe of ty must be at least Kind *)
    Universe.decr_exn (infer_value_universe ty_env ty)
  | Value_ty_fun { var = _; param_ty; body_ty } ->
    let universe1 = infer_value_universe ty_env param_ty in
    let universe2 =
      infer_value_universe
        (Env.push param_ty ty_env)
        (eval_closure1 body_ty (next_var_of_env ty_env))
    in
    Universe.max universe1 universe2
  | Value_ty_mod { env = closure_env; ty_decls } ->
    (* Modules have at least universe Kind because they include subtyping *)
    let _, _, universe =
      List.fold
        ty_decls
        ~init:(closure_env, ty_env, Universe.Kind)
        ~f:(fun (closure_env, ty_env, universe) ty_decl ->
          let ty = eval closure_env ty_decl.ty in
          let universe' = infer_value_universe ty_env ty in
          ( Env.push
              (* Important: make sure to use the size of ty_env instead of env *)
              (next_var_of_env ty_env)
              closure_env
          , Env.push ty ty_env
          , Universe.max universe universe' ))
    in
    universe
;;

(*
  The following function implements the ignorable judgement which is used in the following judgement.
  
    t : U_i        t ignorable
  ----------------------------------
            ignore : t
            
  This allows us to for example, instead of constructing
  (fun (a : Type) -> (x : a) -> ignore) : Fun (a : Type) -> a -> a
  we can do instead
  ignore : Fun (a : Type) -> a -> a
  So we can ignore an entire type at once.
  This corresponds to the sealed at judgement in https://www.cs.cmu.edu/~rwh/papers/multiphase/mlw.pdf
*)
let rec check_ty_ignorable (ty_env : Env.t) (ty : ty) : unit =
  match unfold ty with
  | Uvalue_ignore | Uvalue_mod _ | Uvalue_abs _ | Uvalue_sing_wrap _ ->
    raise_s [%message "Not a type" (ty : value)]
  | Uvalue_ty_pack _ | Uvalue_core_ty _ | Uvalue_ty_sing _ -> ()
  | Uvalue_neutral neutral ->
    let kind =
      infer_neutral ty_env (Uneutral.to_neutral neutral)
      |> unfold
      |> Uvalue.universe_val_exn
    in
    if not (Universe.equal kind Type)
    then fail_s [%message "Type was not ignorable" (ty : ty)]
  | Uvalue_ty_fun ty ->
    check_ty_ignorable
      (Env.push ty.param_ty ty_env)
      (eval_closure1 ty.body_ty (next_var_of_env ty_env))
  | Uvalue_ty_mod ty ->
    let _ =
      List.fold
        ty.ty_decls
        ~init:(ty_env, ty.env)
        ~f:(fun (ty_env, closure_env) ty_decl ->
          let ty = eval closure_env ty_decl.ty in
          check_ty_ignorable ty_env ty;
          Env.push ty ty_env, Env.push (next_var_of_env ty_env) closure_env)
    in
    ()
  | _ -> fail_s [%message "Type was not ignorable" (ty : ty)]
;;

(* TODO: these should not unwrap types with exn *)
let rec infer (cx : Context.t) (e : expr) : term * ty =
  match e with
  | Expr_var { var; span = _ } ->
    let ty = Context.var_ty cx var in
    (* TODO: cache the universe in the context *)
    let universe = infer_value_universe cx.ty_env ty in
    begin match universe with
    | Type ->
      (* Can't construct singletons for universe Type *)
      Term_var var, ty
    | _ ->
      let term_var = Term_var var in
      ( Term_sing_wrap { identity = term_var; e = term_var }
      , Value_ty_sing { identity = Context.eval cx term_var; ty } )
    end
  | Expr_ann { e; ty; span = _ } ->
    let ty, _ = check_universe cx ty in
    let ty = Context.eval cx ty in
    check cx e ty, ty
  | Expr_app { func; arg; span = _ } ->
    let func, func_ty = infer cx func in
    let func_ty = unfold func_ty |> Uvalue.ty_fun_val_exn in
    let arg = check cx arg func_ty.param_ty in
    Term_app { func; arg }, eval_closure1 func_ty.body_ty (Context.eval cx arg)
  | Expr_abs { var; param_ty; body; span = _ } ->
    let param_ty, _ = check_universe cx param_ty in
    let param_ty = Context.eval cx param_ty in
    let cx' = Context.bind var param_ty cx in
    let body, body_ty = infer cx' body in
    let body_ty : value_closure =
      { env = cx.value_env; body = Context.quote cx' body_ty }
    in
    Term_abs { var; body }, Value_ty_fun { var; param_ty; body_ty }
  | Expr_proj { mod_e; field; span = _ } ->
    let mod_e, mod_ty = infer cx mod_e in
    let mod_ty = unfold mod_ty |> Uvalue.ty_mod_val_exn in
    let field_index, _ =
      List.findi_exn mod_ty.ty_decls ~f:(fun _ ty_decl ->
        String.equal ty_decl.var.name field)
    in
    ( Term_proj { mod_e; field; field_index }
    , eval_ty_mod_closure (Context.eval cx mod_e) mod_ty field_index )
  | Expr_mod { decls; span = _ } ->
    (*
      Elaborate a module into a bunch of lets and then a module expression at the end.
    *)
    let decls_length = List.length decls in
    let tuple =
      List.mapi decls ~f:(fun i decl ->
        ({ name = decl.var.name; e = Term_var (Index.of_int (decls_length - i - 1)) }
         : term_field))
    in
    let mod_term = Term_mod { fields = tuple } in
    let _, stuff =
      List.fold_map decls ~init:cx ~f:(fun cx decl ->
        let e, ty = infer cx decl.e in
        let ty_decl : term_ty_decl = { var = decl.var; ty = Context.quote cx ty } in
        Context.bind decl.var ty cx, ((decl.var, e), ty_decl))
    in
    let lets, ty_decls = List.unzip stuff in
    let res_ty = Value_ty_mod { env = cx.value_env; ty_decls } in
    let res_term =
      List.fold_right lets ~init:mod_term ~f:(fun (var, rhs) body ->
        Term_let { var; rhs; body })
    in
    res_term, res_ty
  | Expr_ty_fun { var; param_ty; body_ty; span = _ } ->
    let param_ty, universe1 = check_universe cx param_ty in
    let body_ty, universe2 =
      check_universe (Context.bind var (Context.eval cx param_ty) cx) body_ty
    in
    ( Term_ty_fun { var; param_ty; body_ty }
    , Value_universe (Universe.max universe1 universe2) )
  | Expr_ty_mod { ty_decls; span = _ } ->
    let (_, universe), ty_decls =
      List.fold_map
        ty_decls
        ~init:(cx, Universe.Kind)
        ~f:(fun (cx, universe) { var; ty; span = _ } ->
          let ty, universe' = check_universe cx ty in
          ( (Context.bind var (Context.eval cx ty) cx, Universe.max universe universe')
          , ({ var; ty } : term_ty_decl) ))
    in
    Term_ty_mod { ty_decls }, Value_universe universe
  | Expr_let { var; rhs; body; span = _ } ->
    let rhs, rhs_ty = infer cx rhs in
    let cx' = Context.bind var rhs_ty cx in
    let body, body_ty = infer cx' body in
    ( Term_let { var; rhs; body }
    , eval (Env.push (Context.eval cx rhs) cx.value_env) (Context.quote cx' body_ty) )
  | Expr_ty_sing { e; span = _ } ->
    let e, ty = infer cx e in
    let universe = infer_value_universe cx.ty_env ty in
    if Universe.equal universe Type
    then fail_s [%message "Cannot form singletons with runtime terms"];
    ( Term_ty_sing { identity = e; ty = Context.quote cx ty }
    , Value_universe (Universe.decr_exn universe) )
  | Expr_bool { value; span = _ } -> Term_bool { value }, Value_core_ty Ty_bool
  | Expr_core_ty { ty; span = _ } -> Term_core_ty ty, Value_universe Type
  | Expr_universe { universe; span = _ } ->
    Term_universe universe, Value_universe (Universe.incr_exn universe)
  | Expr_if { cond; body1; body2; span = _ } ->
    let cond = check cx cond (Value_core_ty Ty_bool) in
    let body1, body1_ty = infer cx body1 in
    let body2, body2_ty = infer cx body2 in
    let universe1 = infer_value_universe cx.ty_env body1_ty in
    let universe2 = infer_value_universe cx.ty_env body2_ty in
    if not (Universe.equal universe1 Type)
    then
      fail_s
        [%message
          "The first branch did not have a type in universe Type" (universe1 : Universe.t)];
    if not (Universe.equal universe2 Type)
    then
      fail_s
        [%message
          "The second branch did not have a type in universe Type"
            (universe2 : Universe.t)];
    conv_ty cx.ty_env body1_ty body2_ty;
    Term_if { cond; body1; body2 }, body1_ty
  | Expr_ty_pack { ty; span = _ } ->
    let ty, _ = check_universe cx ty in
    (* Packs always have kind Type, no matter the universe of the inner type *)
    Term_ty_pack ty, Value_universe Type
  | Expr_pack { e; span = _ } ->
    let e, ty = infer cx e in
    Term_pack e, Value_ty_pack ty
  | Expr_bind _ -> fail_s [%message "Cannot infer bind expressions"]

and check (cx : Context.t) (e : expr) (ty : ty) : term =
  match e, unfold ty with
  | Expr_abs { var; param_ty; body; span = _ }, Uvalue_ty_fun ty ->
    let param_ty, _ = check_universe cx param_ty in
    conv_ty cx.ty_env (Context.eval cx param_ty) ty.param_ty;
    let body =
      check
        (Context.bind var ty.param_ty cx)
        body
        (eval_closure1 ty.body_ty (Context.next_var cx))
    in
    Term_abs { var; body }
  | Expr_let { var; rhs; body; span = _ }, _ ->
    let rhs, rhs_ty = infer cx rhs in
    let body = check (Context.bind var rhs_ty cx) body ty in
    Term_let { var; rhs; body }
  | Expr_if { cond; body1; body2; span = _ }, _ ->
    let cond = check cx cond (Value_core_ty Ty_bool) in
    let body1 = check cx body1 ty in
    let body2 = check cx body2 ty in
    Term_if { cond; body1; body2 }
  | Expr_pack { e; span = _ }, Uvalue_ty_pack ty ->
    let e = check cx e ty in
    Term_pack e
  | Expr_bind { var; rhs; body; span = _ }, _ ->
    check_ty_ignorable cx.ty_env ty;
    let rhs, rhs_ty = infer cx rhs in
    let rhs_inner_ty =
      match unfold rhs_ty with
      | Uvalue_ty_pack ty -> ty
      | _ -> fail_s [%message "Expected a pack type" (rhs_ty : ty)]
    in
    let body = check (Context.bind var rhs_inner_ty cx) body ty in
    Term_bind { var; rhs; body }
  | _ ->
    let e, ty' = infer cx e in
    coerce cx e ty' ty

and check_universe (cx : Context.t) (ty : expr) : term * Universe.t =
  let ty, kind = infer cx ty in
  let ty, kind = coerce_singleton (Context.size cx) ty kind in
  match kind with
  | Uvalue_universe u -> ty, u
  | _ -> fail_s [%message "Kind was not a universe" (ty : term_ty) (kind : uvalue)]
;;
