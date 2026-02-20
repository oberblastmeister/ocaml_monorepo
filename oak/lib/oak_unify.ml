open Prelude
open Oak_syntax

open struct
  module Spanned = Utility.Spanned
  module Common = Oak_common
  module Name_list = Common.Name_list
  module Diagnostic = Oak_diagnostic
  module Pretty = Oak_pretty
  module Context = Oak_context
  module Infer_simple = Oak_infer_simple
  module Universe = Common.Universe
  module Evaluate = Oak_evaluate
  module Close = Evaluate.Close
end

exception Type_mismatch of Diagnostic.Part.t

let raise_type_mismatch part = raise_notrace (Type_mismatch part)

let rec occurs_check_adjust (cx : Context.t) (meta : meta_unsolved) (ty : ty) : ty =
  match Context.unfold cx ty with
  | Value_ignore | Value_mod _ | Value_abs _ | Value_sing_in _ -> failwith "not a type"
  | Value_core_ty _ | Value_universe _ -> ty
  | Value_neutral neutral ->
    let neutral = occurs_check_adjust_neutral cx meta neutral in
    Value_neutral (Whnf_neutral.to_neutral neutral)
  | Value_ty_meta meta' ->
    if meta'.meta.id = meta.meta.id
    then
      raise_type_mismatch
        (Diagnostic.Part.create
           (Doc.string "Occurs check failed: meta variable "
            ^^ Meta.pp meta.meta
            ^^ Doc.string " occurs in its own solution"));
    if meta.meta.context_size < meta'.meta.context_size
    then Meta_unsolved.adjust_context_size meta' meta.meta.context_size;
    Value_ty_meta meta'.meta
  | Value_ty_sing { identity; ty } ->
    let identity = occurs_check_adjust cx meta identity in
    let ty = occurs_check_adjust cx meta ty in
    Value_ty_sing { identity; ty }
  | Value_ty_fun ({ var; param_ty; icit; body_ty = _ } as ty) ->
    let param_ty = occurs_check_adjust cx meta param_ty in
    let cx' = Context.bind var param_ty cx in
    let body_ty =
      occurs_check_adjust cx' meta (Evaluate.Fun_ty.app ty (Context.next_free cx))
      |> Context.quote cx'
      |> Evaluate.close_single (Context.next_level cx)
    in
    Value_ty_fun { var; param_ty; icit; body_ty = { env = Env.empty; body = body_ty } }
  | Value_ty_pack ty ->
    let ty = occurs_check_adjust cx meta ty in
    Value_ty_pack ty
  | Value_ty_mod ty ->
    let _, ty_decls =
      List.fold_map
        ty.ty_decls
        ~init:(cx, ty.env, Close.empty)
        ~f:(fun (cx, closure_env, close) { var; ty } ->
          let ty = occurs_check_adjust cx meta (Evaluate.eval closure_env ty) in
          ( ( Context.bind var ty cx
            , Env.push ty closure_env
            , Close.add_exn (Context.next_level cx) Index.zero (Close.lift 1 close) )
          , ({ var; ty = Context.quote cx ty |> Evaluate.close close } : term_ty_decl) ))
    in
    Value_ty_mod { env = Env.empty; ty_decls }

and occurs_check_adjust_neutral
      (cx : Context.t)
      (meta : meta_unsolved)
      (neutral : whnf_neutral)
  : whnf_neutral
  =
  if neutral.head.level >= meta.meta.context_size
  then
    raise_type_mismatch
      (Diagnostic.Part.create
         (Doc.string "Free variable "
          ^^ Context.pp_value cx (Value.free neutral.head)
          ^^ Doc.string " is out of scope of meta variable "
          ^^ Meta.pp meta.meta));
  let spine =
    Bwd.map neutral.spine ~f:(fun elim ->
      match elim with
      | Whnf_elim_app { arg; icit } ->
        let arg = occurs_check_adjust cx meta arg in
        Whnf_elim_app { arg; icit }
      | Whnf_elim_proj _ -> elim)
  in
  { head = neutral.head; spine }
;;

let solve_meta (cx : Context.t) (meta : meta_unsolved) (ty : ty) =
  let universe = Infer_simple.infer_value_universe cx.ty_env ty in
  if not (Universe.is_type universe)
  then
    raise_type_mismatch
      (Diagnostic.Part.create
         (Context.pp_value cx ty
          ^^ Doc.string " was not of kind Type when solving meta variable "
          ^^ Context.pp_value cx (Value_ty_meta meta.meta)));
  let ty = occurs_check_adjust cx meta ty in
  Meta_unsolved.link_to meta ty
;;

(* precondition: e1 and e2 must have type ty. ty must be an element of some universe. *)
let rec unify (cx : Context.t) (e1 : value) (e2 : value) (ty : value) : unit =
  match Context.unfold cx ty with
  | Value_ignore | Value_mod _ | Value_abs _ | Value_sing_in _ ->
    raise_s [%message "Not a type" (ty : value)]
  (* These have kind Type, so can be ignored *)
  | Value_ty_meta _ | Value_ty_pack _ | Value_core_ty _ -> ()
  (* Singletons can be immediately eliminated using unwrap, and since both values have the same singleton type they must be equal. *)
  | Value_ty_sing _ -> ()
  | Value_universe _ ->
    (* both types are elements of some universe *)
    unify_ty cx e1 e2
  | Value_neutral neutral ->
    (* If e1 and e2 have a type that is neutral, we can think of their type as being abstract. *)
    let universe =
      Infer_simple.infer_neutral cx.ty_env (Whnf_neutral.to_neutral neutral)
      |> Context.unfold cx
      |> Whnf.universe_val_exn
    in
    if not (Universe.equal universe Universe.type_)
    then begin
      let ty1 = Context.unfold cx e1 |> Whnf.neutral_val_exn in
      let ty2 = Context.unfold cx e2 |> Whnf.neutral_val_exn in
      let _ = unify_neutral cx ty1 ty2 in
      ()
    end
  | Value_ty_fun ty ->
    let var_value = Context.next_free cx in
    unify
      (Context.bind ty.var ty.param_ty cx)
      (Evaluate.Value.app e1 var_value ty.icit)
      (Evaluate.Value.app e2 var_value ty.icit)
      (Evaluate.Fun_ty.app ty var_value)
  | Value_ty_mod ty ->
    let closure_env = ty.env in
    let _ =
      List.foldi ty.ty_decls ~init:closure_env ~f:(fun field_index closure_env ty_decl ->
        let e1 = Evaluate.Value.proj e1 ty_decl.var.name field_index in
        let e2 = Evaluate.Value.proj e2 ty_decl.var.name field_index in
        unify cx e1 e2 (Evaluate.eval closure_env ty_decl.ty);
        Env.push e1 closure_env)
    in
    ()

(*
  precondition: both ty1 and ty2 must be an element of some universe, but they may be different universes.
*)
and unify_ty (cx : Context.t) (ty1 : ty) (ty2 : ty) : unit =
  match Context.unfold cx ty1, Context.unfold cx ty2 with
  | Value_ty_meta meta, ty2 -> solve_meta cx meta (Whnf.to_value ty2)
  | ty1, Value_ty_meta meta -> solve_meta cx meta (Whnf.to_value ty1)
  | Value_ty_sing ty1, Value_ty_sing ty2 ->
    unify_ty cx ty1.ty ty2.ty;
    (* we now know that ty1 = ty2 *)
    unify cx ty1.identity ty2.identity ty1.ty
  | Value_universe universe1, Value_universe universe2 ->
    if not (Universe.equal universe1 universe2)
    then
      raise_type_mismatch
        (Diagnostic.Part.create
           (Doc.string "Universes were not equal: "
            ^^ Universe.pp universe1
            ^^ Doc.string " != "
            ^^ Universe.pp universe2))
  | Value_ty_fun ty1, Value_ty_fun ty2 ->
    unify_ty cx ty1.param_ty ty2.param_ty;
    unify_ty
      (Context.bind ty1.var ty1.param_ty cx)
      (Evaluate.Fun_ty.app ty1 (Context.next_free cx))
      (Evaluate.Fun_ty.app ty2 (Context.next_free cx))
  | Value_ty_pack ty1, Value_ty_pack ty2 -> unify_ty cx ty1 ty2
  | Value_ty_mod ty1, Value_ty_mod ty2 ->
    let zipped_ty_decls =
      match List.zip ty1.ty_decls ty2.ty_decls with
      | Ok t -> t
      | Unequal_lengths ->
        raise_type_mismatch
          (Diagnostic.Part.create
             (Doc.string "Record had different number of declarations. This record had"
              ^^ Doc.space
              ^^ Doc.string (Int.to_string (List.length ty1.ty_decls))
              ^^ Doc.indent 2 (Doc.break1 ^^ Context.pp_value cx (Value_ty_mod ty1))
              ^^ Context.pp_value cx (Value_ty_mod ty2)
              ^^ Doc.break1
              ^^ Doc.string "while this record had"
              ^^ Doc.space
              ^^ Doc.string (Int.to_string (List.length ty1.ty_decls))
              ^^ Doc.indent 2 (Doc.break1 ^^ Context.pp_value cx (Value_ty_mod ty2))))
    in
    let _ =
      List.fold
        zipped_ty_decls
        ~init:(ty1.env, ty2.env, cx)
        ~f:(fun (closure_env1, closure_env2, cx) (ty_decl1, ty_decl2) ->
          let name1 = ty_decl1.var.name in
          let name2 = ty_decl2.var.name in
          if not (String.equal name1 name2)
          then
            raise_type_mismatch
              (Diagnostic.Part.create
                 (Doc.string "Declaration name not equal: "
                  ^^ Doc.string name1
                  ^^ Doc.string " != "
                  ^^ Doc.string name2));
          let ty1 = Evaluate.eval closure_env1 ty_decl1.ty in
          let ty2 = Evaluate.eval closure_env2 ty_decl2.ty in
          unify_ty cx ty1 ty2;
          let var_value = Context.next_free cx in
          ( Env.push var_value closure_env1
          , Env.push var_value closure_env2
          , Context.bind ty_decl1.var ty1 cx ))
    in
    ()
  | Value_core_ty ty1, Value_core_ty ty2 ->
    if not (Core_ty.equal ty1 ty2)
    then
      raise_type_mismatch
        (Diagnostic.Part.create
           (Doc.string "Base types were not equal: "
            ^^ Context.pp_value cx (Value_core_ty ty1)
            ^^ Doc.string " != "
            ^^ Context.pp_value cx (Value_core_ty ty2)))
  | Value_neutral ty1, Value_neutral ty2 ->
    (* ty1 and ty2 are elements of some universe, and the kind of a universe is at least Kind *)
    let _ = unify_neutral cx ty1 ty2 in
    ()
  | _ ->
    raise_type_mismatch
      (Diagnostic.Part.create
         (Doc.string "Types were not equal: "
          ^^ Context.pp_value cx ty1
          ^^ Doc.string " != "
          ^^ Context.pp_value cx ty2))

(*
  Precondition: if e1 : t1 and e2 : t2 then t1 : U_i and t2 : U_j where i > 0 and j > 0, so both types must have universe at least Kind.
  In other words, both inputs must be at least types.
  Checks if e1 = e2. Returns the kind that they are equivalent at.
*)
and unify_neutral (cx : Context.t) (ty1 : whnf_neutral) (ty2 : whnf_neutral) : value =
  if not (Level.equal ty1.head ty2.head)
  then
    raise_type_mismatch
      (Diagnostic.Part.create
         (Doc.string "Variables were not equal: "
          ^^ Context.pp_value cx (Value.free ty1.head)
          ^^ Doc.string " != "
          ^^ Context.pp_value cx (Value.free ty2.head)));
  let spine1 = Bwd.to_list ty1.spine in
  let spine2 = Bwd.to_list ty2.spine in
  let zipped_spines =
    match List.zip spine1 spine2 with
    | Unequal_lengths ->
      raise_type_mismatch
        (Diagnostic.Part.create
           (Doc.string "Types were not equal (spine lengths differ)"))
    | Ok t -> t
  in
  List.fold
    zipped_spines
    ~init:(Bwd.Empty, Context.level_var_ty cx ty1.head)
    ~f:(fun (spine, ty) (elim1, elim2) ->
      let ty =
        match elim1, elim2 with
        | ( Whnf_elim_app { arg = arg1; icit = icit1 }
          , Whnf_elim_app { arg = arg2; icit = icit2 } ) ->
          assert (Icit.equal icit1 icit2);
          let func_kind = Context.unfold cx ty |> Whnf.ty_fun_val_exn in
          unify cx arg1 arg2 func_kind.param_ty;
          Evaluate.Fun_ty.app func_kind arg1
        | ( Whnf_elim_proj { field = field1; field_index = field_index1; _ }
          , Whnf_elim_proj { field = field2; field_index = field_index2; _ } ) ->
          if not (field_index1 = field_index2)
          then
            raise_type_mismatch
              (Diagnostic.Part.create
                 (Doc.string "Fields were not equal in a projection: "
                  ^^ Doc.string field1
                  ^^ Doc.string " != "
                  ^^ Doc.string field2));
          Evaluate.Ty.proj
            cx.ty_env
            (Value_neutral { head = ty1.head; spine })
            ty
            field_index1
        | _ ->
          raise_type_mismatch
            (Diagnostic.Part.create
               (Doc.string "Types were not equal: "
                ^^ Context.pp_value cx (Value_neutral (Whnf_neutral.to_neutral ty1))
                ^^ Doc.string " != "
                ^^ Context.pp_value cx (Value_neutral (Whnf_neutral.to_neutral ty2))))
      in
      spine <: Whnf_elim.to_elim elim1, ty)
  |> snd
;;

let rec coerce_singleton cx (e : term) (ty : ty) : term * whnf =
  match Context.unfold cx ty with
  | Value_ty_sing { identity = _; ty = kind } ->
    coerce_singleton cx (Term_sing_out e) kind
  | ty -> e, ty
;;

(*
  Check whether A <= B, and coerces the term if it is.
  Returns none when the types were equal, to prevent useless eta expansion
*)
let rec sub cx (e : term) (ty1 : ty) (ty2 : ty) : term option =
  match Context.unfold cx ty1, Context.unfold cx ty2 with
  | Value_universe ty1, Value_universe ty2 ->
    if not (Universe.equal ty1 ty2)
    then
      raise_type_mismatch
        (Diagnostic.Part.create
           (Doc.string "Universes were not equal: "
            ^^ Common.Universe.pp ty1
            ^^ Doc.string " != "
            ^^ Common.Universe.pp ty2));
    None
  | Value_core_ty ty1, Value_core_ty ty2 ->
    if not (Core_ty.equal ty1 ty2)
    then
      raise_type_mismatch
        (Diagnostic.Part.create
           (Doc.string "Core types were not equal: "
            ^^ Context.pp_value cx (Value_core_ty ty1)
            ^^ Doc.string " != "
            ^^ Context.pp_value cx (Value_core_ty ty2)));
    None
  | Value_ty_sing ty1, Value_ty_sing ty2 ->
    let e' = sub cx (Term_sing_out e) ty1.ty ty2.ty in
    begin match e' with
    | None ->
      unify cx ty1.identity ty2.identity ty1.ty;
      None
    | Some e' ->
      unify cx (Evaluate.eval Env.empty e') ty2.identity ty2.ty;
      Some (Term_sing_in e')
    end
  | Value_ty_sing _, _ ->
    let e, ty1 = coerce_singleton cx e ty1 in
    Some (coerce cx e (Whnf.to_value ty1) ty2)
  | _, Value_ty_sing ty2 ->
    let e = coerce cx e ty1 ty2.ty in
    unify cx (Evaluate.eval Env.empty e) ty2.identity ty2.ty;
    Some (Term_sing_in e)
  | Value_ty_fun ty1, Value_ty_fun ty2 ->
    let var = ty2.var in
    let free = Level.of_int (Context.size cx) in
    let arg_var_value = Value.free free in
    let cx = Context.bind var ty2.param_ty cx in
    let arg_var_term = Context.quote cx arg_var_value in
    let arg = sub cx arg_var_term ty2.param_ty ty1.param_ty in
    begin match arg with
    | None ->
      let body =
        sub
          cx
          (Term_app { func = e; arg = arg_var_term; icit = ty1.icit })
          (Evaluate.Fun_ty.app ty1 arg_var_value)
          (Evaluate.Fun_ty.app ty2 arg_var_value)
      in
      Option.map body ~f:(fun body ->
        Term_abs { var; body = Evaluate.close_single free body; icit = ty2.icit })
    | Some arg ->
      let arg_value = Evaluate.eval Env.empty arg in
      let body =
        coerce
          cx
          (Term_app { func = e; arg; icit = ty1.icit })
          (Evaluate.Fun_ty.app ty1 arg_value)
          (Evaluate.Fun_ty.app ty2 arg_value)
      in
      Some (Term_abs { var; body = Evaluate.close_single free body; icit = ty2.icit })
    end
  | Value_ty_mod ty1, Value_ty_mod ty2 ->
    let value = Evaluate.eval Env.empty e in
    let ty1_map =
      List.fold_mapi ty1.ty_decls ~init:ty1.env ~f:(fun field_index closure_env ty_decl ->
        let proj_ty = Evaluate.eval closure_env ty_decl.ty in
        let field_name = ty_decl.var.name in
        ( Env.push (Evaluate.Value.proj value field_name field_index) closure_env
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
              raise_type_mismatch
                (Diagnostic.Part.create
                   (Doc.string "Module is not a subtype: could not find field "
                    ^^ Doc.string field_name))
            | Some t -> t
          in
          let proj_term =
            Term_proj { mod_e = e; field = field_name; field_index = ty1_field_index }
          in
          let coerced_proj_term =
            sub cx proj_term ty1_proj_ty (Evaluate.eval closure_env ty2_decl.ty)
          in
          let did_coerce = did_coerce || Option.is_some coerced_proj_term in
          let coerced_proj_term = Option.value coerced_proj_term ~default:proj_term in
          let field : term_field = { name = field_name; e = coerced_proj_term } in
          ( (did_coerce, Env.push (Evaluate.eval Env.empty coerced_proj_term) closure_env)
          , field ))
    in
    if not did_coerce then None else Some (Term_mod { fields })
  | _ ->
    (* Otherwise fallback to checking for equivalence *)
    unify_ty cx ty1 ty2;
    None

and coerce cx e ty1 ty2 = sub cx e ty1 ty2 |> Option.value ~default:e

let catch_type_mismatch f =
  try Ok (f ()) with
  | Type_mismatch part -> Error part
;;

let unify cx e1 e2 ty = catch_type_mismatch (fun () -> unify cx e1 e2 ty)
let unify_ty cx ty1 ty2 = catch_type_mismatch (fun () -> unify_ty cx ty1 ty2)
let unify_neutral cx ty1 ty2 = catch_type_mismatch (fun () -> unify_neutral cx ty1 ty2)
let sub cx e ty1 ty2 = catch_type_mismatch (fun () -> sub cx e ty1 ty2)
let coerce cx e ty1 ty2 = catch_type_mismatch (fun () -> coerce cx e ty1 ty2)
