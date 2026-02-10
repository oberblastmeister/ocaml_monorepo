open Prelude
open Oak_syntax
open Oak_evaluate

open struct
  module Spanned = Utility.Spanned
  module Common = Oak_common
  module Name_list = Common.Name_list
  module Diagnostic = Oak_diagnostic
  module Pretty = Oak_pretty
  module Context = Oak_context
  module Infer_simple = Oak_infer_simple
end

exception Type_mismatch of Diagnostic.Part.t

let raise_type_mismatch part = raise_notrace (Type_mismatch part)

(* precondition: e1 and e2 must have type ty. ty must be an element of some universe. *)
let rec conv (cx : Context.t) (e1 : value) (e2 : value) (ty : value) : unit =
  match unfold ty with
  | Uvalue_ignore | Uvalue_mod _ | Uvalue_abs _ | Uvalue_sing_in _ ->
    raise_s [%message "Not a type" (ty : value)]
  (* These have kind Type, so can be ignored *)
  | Uvalue_ty_pack _ | Uvalue_core_ty _ -> ()
  (* Singletons can be immediately eliminated using unwrap, and since both values have the same singleton type they must be equal. *)
  | Uvalue_ty_sing _ -> ()
  | Uvalue_universe _ ->
    (* both types are elements of some universe *)
    conv_ty cx e1 e2
  | Uvalue_neutral uneutral ->
    (* If e1 and e2 have a type that is neutral, we can think of their type as being abstract. *)
    let universe =
      Infer_simple.infer_neutral cx.ty_env (Uneutral.to_neutral uneutral)
      |> unfold
      |> Uvalue.universe_val_exn
    in
    begin match universe with
    (* abstract types can be ignored *)
    | Type -> ()
    | Kind | Sig ->
      let ty1 = unfold e1 |> Uvalue.neutral_val_exn in
      let ty2 = unfold e2 |> Uvalue.neutral_val_exn in
      let _ = conv_neutral cx ty1 ty2 in
      ()
    end
  | Uvalue_ty_fun ty ->
    let var_value = Context.next_var cx in
    conv
      (Context.bind ty.var ty.param_ty cx)
      (app_value e1 var_value)
      (app_value e2 var_value)
      (eval_closure1 ty.body_ty var_value)
  | Uvalue_ty_mod ty ->
    let closure_env = ty.env in
    let _ =
      List.foldi ty.ty_decls ~init:closure_env ~f:(fun field_index closure_env ty_decl ->
        let e1 = proj_value e1 ty_decl.var.name field_index in
        let e2 = proj_value e2 ty_decl.var.name field_index in
        conv cx e1 e2 (eval closure_env ty_decl.ty);
        Env.push e1 closure_env)
    in
    ()

(*
  precondition: both ty1 and ty2 must be an element of some universe, but they may be different universes.
*)
and conv_ty (cx : Context.t) (ty1 : ty) (ty2 : ty) : unit =
  match unfold ty1, unfold ty2 with
  | Uvalue_ty_sing ty1, Uvalue_ty_sing ty2 ->
    conv_ty cx ty1.ty ty2.ty;
    (* we now know that ty1 = ty2 *)
    conv cx ty1.identity ty2.identity ty1.ty
  | Uvalue_universe universe1, Uvalue_universe universe2 ->
    if not (Universe.equal universe1 universe2)
    then
      raise_type_mismatch
        (Diagnostic.Part.create
           (Doc.string "Universes were not equal: "
            ^^ Universe.pp universe1
            ^^ Doc.string " != "
            ^^ Universe.pp universe2))
  | Uvalue_ty_fun ty1, Uvalue_ty_fun ty2 ->
    conv_ty cx ty1.param_ty ty2.param_ty;
    conv_ty
      (Context.bind ty1.var ty1.param_ty cx)
      (eval_closure1 ty1.body_ty (Context.next_var cx))
      (eval_closure1 ty2.body_ty (Context.next_var cx))
  | Uvalue_ty_pack ty1, Uvalue_ty_pack ty2 -> conv_ty cx ty1 ty2
  | Uvalue_ty_mod ty1, Uvalue_ty_mod ty2 ->
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
          let ty1 = eval closure_env1 ty_decl1.ty in
          let ty2 = eval closure_env2 ty_decl2.ty in
          conv_ty cx ty1 ty2;
          let var_value = Context.next_var cx in
          ( Env.push var_value closure_env1
          , Env.push var_value closure_env2
          , Context.bind ty_decl1.var ty1 cx ))
    in
    ()
  | Uvalue_core_ty ty1, Uvalue_core_ty ty2 ->
    if not (Core_ty.equal ty1 ty2)
    then
      raise_type_mismatch
        (Diagnostic.Part.create
           (Doc.string "Base types were not equal: "
            ^^ Context.pp_value cx (Value_core_ty ty1)
            ^^ Doc.string " != "
            ^^ Context.pp_value cx (Value_core_ty ty2)))
  | Uvalue_neutral ty1, Uvalue_neutral ty2 ->
    (* ty1 and ty2 are elements of some universe, and the kind of a universe is at least Kind *)
    let _ = conv_neutral cx ty1 ty2 in
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
and conv_neutral (cx : Context.t) (ty1 : uneutral) (ty2 : uneutral) : value =
  if not (Level.equal ty1.head ty2.head)
  then
    raise_type_mismatch
      (Diagnostic.Part.create
         (Doc.string "Variables were not equal: "
          ^^ Context.pp_value cx (Value.var ty1.head)
          ^^ Doc.string " != "
          ^^ Context.pp_value cx (Value.var ty2.head)));
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
    ~init:(Bwd.Empty, Context.var_ty cx (Index.of_level (Context.size cx) ty1.head))
    ~f:(fun (spine, ty) (elim1, elim2) ->
      let ty =
        match elim1, elim2 with
        | Uelim_app arg1, Uelim_app arg2 ->
          let func_kind = unfold ty |> Uvalue.ty_fun_val_exn in
          conv cx arg1 arg2 func_kind.param_ty;
          eval_closure1 func_kind.body_ty arg1
        | ( Uelim_proj { field = field1; field_index = field_index1; _ }
          , Uelim_proj { field = field2; field_index = field_index2; _ } ) ->
          let kind = unfold ty |> Uvalue.ty_mod_val_exn in
          if not (field_index1 = field_index2)
          then
            raise_type_mismatch
              (Diagnostic.Part.create
                 (Doc.string "Fields were not equal in a projection: "
                  ^^ Doc.string field1
                  ^^ Doc.string " != "
                  ^^ Doc.string field2));
          eval_ty_mod_closure (Value_neutral { head = ty1.head; spine }) kind field_index1
        | _ ->
          raise_type_mismatch
            (Diagnostic.Part.create
               (Doc.string "Types were not equal: "
                ^^ Context.pp_value cx (Value_neutral (Uneutral.to_neutral ty1))
                ^^ Doc.string " != "
                ^^ Context.pp_value cx (Value_neutral (Uneutral.to_neutral ty2))))
      in
      spine <: Uelim.to_elim elim1, ty)
  |> snd
;;

let rec coerce_singleton context_size (e : term) (ty : ty) : term * uvalue =
  match unfold ty with
  | Uvalue_ty_sing { identity; ty = kind } ->
    let identity = quote context_size identity in
    coerce_singleton context_size (Term_sing_out { identity; e }) kind
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
      raise_type_mismatch
        (Diagnostic.Part.create
           (Doc.string "Universes were not equal: "
            ^^ Common.Universe.pp ty1
            ^^ Doc.string " != "
            ^^ Common.Universe.pp ty2));
    None
  | Uvalue_core_ty ty1, Uvalue_core_ty ty2 ->
    if not (Core_ty.equal ty1 ty2)
    then
      raise_type_mismatch
        (Diagnostic.Part.create
           (Doc.string "Core types were not equal: "
            ^^ Context.pp_value cx (Value_core_ty ty1)
            ^^ Doc.string " != "
            ^^ Context.pp_value cx (Value_core_ty ty2)));
    None
  | Uvalue_ty_sing ty1, Uvalue_ty_sing ty2 ->
    let e' =
      sub cx (Term_sing_out { identity = Context.quote cx ty1.identity; e }) ty1.ty ty2.ty
    in
    begin match e' with
    | None ->
      conv cx ty1.identity ty2.identity ty1.ty;
      None
    | Some e' ->
      conv cx (Context.eval cx e') ty2.identity ty2.ty;
      Some (Term_sing_in e')
    end
  | Uvalue_ty_sing _, _ ->
    let e, ty1 = coerce_singleton (Context.size cx) e ty1 in
    sub cx e (Uvalue.to_value ty1) ty2
  | _, Uvalue_ty_sing ty2 ->
    let e = coerce cx e ty1 ty2.ty in
    conv cx (Context.eval cx e) ty2.identity ty2.ty;
    Some (Term_sing_in e)
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
    conv_ty cx ty1 ty2;
    None

and coerce cx e ty1 ty2 = sub cx e ty1 ty2 |> Option.value ~default:e

let catch_type_mismatch f =
  try Ok (f ()) with
  | Type_mismatch part -> Error part
;;

let conv cx e1 e2 ty = catch_type_mismatch (fun () -> conv cx e1 e2 ty)
let conv_ty cx ty1 ty2 = catch_type_mismatch (fun () -> conv_ty cx ty1 ty2)
let conv_neutral cx ty1 ty2 = catch_type_mismatch (fun () -> conv_neutral cx ty1 ty2)
let sub cx e ty1 ty2 = catch_type_mismatch (fun () -> sub cx e ty1 ty2)
let coerce cx e ty1 ty2 = catch_type_mismatch (fun () -> coerce cx e ty1 ty2)
