open Prelude
open Oak_syntax

open struct
  module Bwd = Utility.Bwd
  module Spanned = Utility.Spanned
  module Common = Oak_common
  module Name_list = Common.Name_list
  module Diagnostic = Oak_diagnostic
  module Context = Oak_context
  module Evaluate = Oak_evaluate
  module Close = Evaluate.Close
  module Typed = Oak_typed
end

let is_id_coe = function
  | Typed.Id_coe -> true
  | Typed.Fun_coe _ | Typed.Struct_coe _ -> false
;;

let mk_fun_coe (arg_coe : Typed.runtime_coe) (ret_coe : Typed.runtime_coe)
  : Typed.runtime_coe
  =
  if is_id_coe arg_coe && is_id_coe ret_coe
  then Typed.Id_coe
  else Typed.Fun_coe { arg_coe; ret_coe }
;;

let mk_struct_coe is_same_shape (field_coes : Typed.runtime_field_coe list)
  : Typed.runtime_coe
  =
  if is_same_shape
  then begin
    if List.for_all field_coes ~f:(fun field_coe -> is_id_coe field_coe.coe)
    then Id_coe
    else Struct_coe field_coes
  end
  else Typed.Struct_coe field_coes
;;

let rec coerce_singleton (cx : Context.t) (e : term) (ty : ty) : term * ty =
  match Context.whnf_ty cx ty with
  | Ty_sing { identity = _; ty = kind } -> coerce_singleton cx (Term_sing_out e) kind
  | ty -> e, ty
;;

let rec unify_value (cx : Context.t) (e1 : value) (e2 : value) (ty : ty) : unit =
  match Context.whnf_ty cx ty with
  (* These are all transparent *)
  | Ty_pack _ | Ty_core _ | Ty_sing _ -> ()
  | Ty_universe _props ->
    unify_ty cx (Evaluate.Value.decode e1) (Evaluate.Value.decode e2)
  | Ty_struct ty ->
    let closure_env = ty.env in
    let _ =
      List.foldi
        ty.field_specs
        ~init:closure_env
        ~f:(fun index closure_env (field_spec : term_field_spec) ->
          let field : field_loc = { name = field_spec.name.name; index } in
          let e1 = Evaluate.Value.proj e1 field in
          let e2 = Evaluate.Value.proj e2 field in
          unify_value cx e1 e2 (Evaluate.eval_ty closure_env field_spec.ty);
          Seq.push e1 closure_env)
    in
    ()
  | Ty_fun ty ->
    let var_value = Context.next_free cx in
    let arg : value_arg = { e = var_value; param_modifiers = ty.param_modifiers } in
    unify_value
      (Context.bind ty.name ty.param_ty cx)
      (Evaluate.Value.app e1 arg)
      (Evaluate.Value.app e2 arg)
      (Evaluate.Fun_ty.app ty arg)
  | Ty_decode ty ->
    let props = Evaluate.infer_neutral_universe cx.ty_env ty in
    if not (Size.is_type props.size)
    then begin
      let e1 = Context.whnf_value cx e1 |> Value.neutral_val_exn in
      let e2 = Context.whnf_value cx e2 |> Value.neutral_val_exn in
      unify_neutral cx e1 e2
    end

and unify_ty (cx : Context.t) (ty1 : ty) (ty2 : ty) =
  match Context.whnf_ty cx ty1, Context.whnf_ty cx ty2 with
  | Ty_universe props1, Ty_universe props2 -> unify_ty_props cx props1 props2
  | Ty_sing ty1, Ty_sing ty2 ->
    unify_ty cx ty1.ty ty2.ty;
    unify_value cx ty1.identity ty2.identity ty1.ty
  | Ty_fun ty1, Ty_fun ty2 ->
    unify_ty cx ty1.param_ty ty2.param_ty;
    unify_param_modifiers cx ty1.param_modifiers ty2.param_modifiers;
    let arg = { e = Context.next_free cx; param_modifiers = ty1.param_modifiers } in
    unify_ty
      (Context.bind ty1.name ty1.param_ty cx)
      (Evaluate.Fun_ty.app ty1 arg)
      (Evaluate.Fun_ty.app ty2 arg)
  | Ty_struct ty1, Ty_struct ty2 ->
    let zipped_ty_decls =
      match List.zip ty1.field_specs ty2.field_specs with
      | Ok t -> t
      | Unequal_lengths ->
        Context.throw
          cx
          [ Diagnostic.Part.create
              (Doc.string "Record had different number of declarations. This record had"
               ^^ Doc.space
               ^^ Doc.string (Int.to_string (List.length ty1.field_specs))
               ^^ Doc.indent 2 (Doc.break1 ^^ Context.pp_ty cx (Ty_struct ty1))
               ^^ Context.pp_ty cx (Ty_struct ty2)
               ^^ Doc.break1
               ^^ Doc.string "while this record had"
               ^^ Doc.space
               ^^ Doc.string (Int.to_string (List.length ty1.field_specs))
               ^^ Doc.indent 2 (Doc.break1 ^^ Context.pp_ty cx (Ty_struct ty2)))
          ]
    in
    let _ =
      List.fold
        zipped_ty_decls
        ~init:(ty1.env, ty2.env, cx)
        ~f:(fun (closure_env1, closure_env2, cx) (field_spec1, field_spec2) ->
          let name1 = field_spec1.name.name in
          let name2 = field_spec2.name.name in
          if not (String.equal name1 name2)
          then
            Context.throw
              cx
              [ Diagnostic.Part.create
                  (Doc.string "Declaration name not equal: "
                   ^^ Doc.string name1
                   ^^ Doc.string " != "
                   ^^ Doc.string name2)
              ];
          unify_relevancy cx field_spec1.relevancy field_spec2.relevancy;
          let ty1 = Evaluate.eval_ty closure_env1 field_spec1.ty in
          let ty2 = Evaluate.eval_ty closure_env2 field_spec2.ty in
          unify_ty cx ty1 ty2;
          let var_value = Context.next_free cx in
          ( Seq.push var_value closure_env1
          , Seq.push var_value closure_env2
          , Context.bind field_spec1.name ty1 cx ))
    in
    ()
  | Ty_core ty1, Ty_core ty2 ->
    if not (Core_ty.equal ty1 ty2)
    then
      Context.throw
        cx
        [ Diagnostic.Part.create
            (Doc.string "Base types were not equal: "
             ^^ Context.pp_ty cx (Ty_core ty1)
             ^^ Doc.string " != "
             ^^ Context.pp_ty cx (Ty_core ty2))
        ]
  | Ty_pack ty1, Ty_pack ty2 -> unify_ty cx ty1 ty2
  | Ty_decode ty1, Ty_decode ty2 ->
    (* both ty1 and ty2 are whnf, or otherwise the decode is not whnf *)
    unify_neutral cx ty1 ty2
  | _, _ ->
    Context.throw
      cx
      [ Diagnostic.Part.create
          (Doc.string "Types were not equal: "
           ^^ Context.pp_ty cx ty1
           ^^ Doc.string " != "
           ^^ Context.pp_ty cx ty2)
      ]

and unify_param_modifiers
      (cx : Context.t)
      (param_modifiers1 : Param_modifiers.t)
      (param_modifiers2 : Param_modifiers.t)
  =
  if not (Icit.equal param_modifiers1.icit param_modifiers2.icit)
  then
    Context.throw
      cx
      [ Diagnostic.Part.create
          (Doc.string "Icitness was not equal: "
           ^^ Icit.pp param_modifiers1.icit
           ^^ Doc.string " != "
           ^^ Icit.pp param_modifiers2.icit)
      ];
  unify_relevancy cx param_modifiers1.relevancy param_modifiers2.relevancy

and unify_relevancy (cx : Context.t) (relevancy1 : Relevancy.t) (relevancy2 : Relevancy.t)
  =
  if not (Relevancy.equal relevancy1 relevancy2)
  then
    Context.throw
      cx
      [ Diagnostic.Part.create
          (Doc.string "Relevancy was not equal: "
           ^^ Relevancy.pp relevancy1
           ^^ Doc.string " != "
           ^^ Relevancy.pp relevancy2)
      ]

and unify_ty_props (cx : Context.t) (props1 : Ty_props.t) (props2 : Ty_props.t) =
  if not (Size.equal props1.size props2.size)
  then
    Context.throw
      cx
      [ Diagnostic.Part.create
          (Doc.string "Sizes were not equal: "
           ^^ Size.pp props1.size
           ^^ Doc.string " != "
           ^^ Size.pp props2.size)
      ]

(* precondition: should be whnf *)
and unify_neutral (cx : Context.t) (e1 : neutral) (e2 : neutral) : unit =
  unify_head cx e1.head e2.head;
  let spine1 = Bwd.to_list e1.spine in
  let spine2 = Bwd.to_list e2.spine in
  let zipped_spines =
    match List.zip spine1 spine2 with
    | Unequal_lengths ->
      Context.throw
        cx
        [ Diagnostic.Part.create
            (Doc.string "Types were not equal (spine lengths differ)")
        ]
    | Ok t -> t
  in
  let _ =
    List.fold
      zipped_spines
      ~init:(Bwd.Empty, Evaluate.infer_head cx.ty_env e1.head)
      ~f:(fun (spine, ty) (frame1, frame2) ->
        let ty =
          match frame1, frame2 with
          | Out, _ | _, Out -> failwith "should be whnf"
          | App arg1, App arg2 ->
            let fun_ty = Context.whnf_ty cx ty |> Ty.ty_fun_val_exn in
            unify_value cx arg1.e arg2.e fun_ty.param_ty;
            Evaluate.Fun_ty.app fun_ty arg1
          | Proj field1, Proj field2 ->
            if not (field1.index = field2.index)
            then
              Context.throw
                cx
                [ Diagnostic.Part.create
                    (Doc.string "Fields were not equal in a projection: "
                     ^^ Doc.string field1.name
                     ^^ Doc.string " != "
                     ^^ Doc.string field2.name)
                ];
            Evaluate.Ty.proj cx.ty_env (Value_neutral { head = e1.head; spine }) ty field1
          | _ ->
            Context.throw
              cx
              [ Diagnostic.Part.create
                  (Doc.string "Types were not equal: "
                   ^^ Context.pp_value cx (Value_neutral e1)
                   ^^ Doc.string " != "
                   ^^ Context.pp_value cx (Value_neutral e2))
              ]
        in
        spine <: frame1, ty)
  in
  ()

and unify_head (cx : Context.t) (e1 : head) (e2 : head) : unit =
  match e1, e2 with
  | Free e1, Free e2 ->
    if not (Level.equal e1 e2)
    then
      Context.throw
        cx
        [ Diagnostic.Part.create
            (Doc.string "Variables were not equal: "
             ^^ Context.pp_value cx (Value.free e1)
             ^^ Doc.string " != "
             ^^ Context.pp_value cx (Value.free e2))
        ]
  | _ -> failwith ""

(* TODO: fix this, the runtime_coe is wrong *)
(* postcondition: if term is None then runtime_coe must be Id_coe *)
and sub (cx : Context.t) (e : term) (ty1 : ty) (ty2 : ty)
  : term option * Typed.runtime_coe
  =
  match Context.whnf_ty cx ty1, Context.whnf_ty cx ty2 with
  | Ty_universe props1, Ty_universe props2 ->
    (* TODO: maybe do cumulativity here *)
    unify_ty_props cx props1 props2;
    None, Typed.Id_coe
  | Ty_core ty1, Ty_core ty2 ->
    if not (Core_ty.equal ty1 ty2)
    then
      Context.throw
        cx
        [ Diagnostic.Part.create
            (Doc.string "Base types were not equal: "
             ^^ Context.pp_ty cx (Ty_core ty1)
             ^^ Doc.string " != "
             ^^ Context.pp_ty cx (Ty_core ty2))
        ];
    None, Typed.Id_coe
  | Ty_sing ty1, Ty_sing ty2 ->
    let e', coe = sub cx (Term_sing_out e) ty1.ty ty2.ty in
    begin match e' with
    | None ->
      unify_value cx ty1.identity ty2.identity ty1.ty;
      None, coe
    | Some e' ->
      unify_value cx (Evaluate.eval_value Seq.empty e') ty2.identity ty2.ty;
      Some (Term_sing_in e'), coe
    end
  | Ty_sing _, _ ->
    let e, ty1 = coerce_singleton cx e ty1 in
    let e', coe = sub cx e ty1 ty2 in
    Some (Option.value ~default:e e'), coe
  | _, Ty_sing ty2 ->
    let e', coe = sub cx e ty1 ty2.ty in
    let e' = Option.value ~default:e e' in
    unify_value cx (Evaluate.eval_value Seq.empty e') ty2.identity ty2.ty;
    Some (Term_sing_in e'), coe
  | Ty_fun ty1, Ty_fun ty2 ->
    unify_param_modifiers cx ty1.param_modifiers ty2.param_modifiers;
    let free = Context.next_level cx in
    let arg_var_value = Context.next_free cx in
    let cx = Context.bind ty2.name ty2.param_ty cx in
    let arg_var_term = Evaluate.Value.quote arg_var_value in
    let arg', arg_coe = sub cx arg_var_term ty2.param_ty ty1.param_ty in
    let arg_term = Option.value ~default:arg_var_term arg' in
    let arg_value = Evaluate.eval_value Seq.empty arg_term in
    let app_term =
      Term_app { func = e; arg = { e = arg_term; param_modifiers = ty1.param_modifiers } }
    in
    let body', ret_coe =
      sub
        cx
        app_term
        (Evaluate.Fun_ty.app ty1 { e = arg_value; param_modifiers = ty1.param_modifiers })
        (Evaluate.Fun_ty.app ty2 { e = arg_value; param_modifiers = ty2.param_modifiers })
    in
    let runtime_coe = mk_fun_coe arg_coe ret_coe in
    let body_term = Option.value ~default:app_term body' in
    if Option.is_none arg' && Option.is_none body'
    then None, Typed.Id_coe
    else
      ( Some
          (Term_fun
             { name = ty2.name
             ; param_modifiers = ty2.param_modifiers
             ; body = Evaluate.close_single free body_term
             })
      , runtime_coe )
  | Ty_struct ty1, Ty_struct ty2 ->
    let value = Evaluate.eval_value Seq.empty e in
    let _, ty1_map =
      List.foldi
        ty1.field_specs
        ~init:(ty1.env, String.Map.empty)
        ~f:(fun index (closure_env, acc) field_spec ->
          let proj_ty = Evaluate.eval_ty closure_env field_spec.ty in
          let field_name = field_spec.name.name in
          let field_loc = ({ name = field_name; index } : field_loc) in
          let proj_value = Evaluate.Value.proj value field_loc in
          ( Seq.push proj_value closure_env
          , Map.set acc ~key:field_name ~data:(field_loc, proj_ty, field_spec.relevancy) ))
    in
    let did_coerce, _, field_impls, field_coes =
      List.foldi
        ty2.field_specs
        ~init:(false, ty2.env, Bwd.Empty, Bwd.Empty)
        ~f:(fun index (did_coerce, closure_env, field_impls, field_coes) field_spec2 ->
          let field_name = field_spec2.name.name in
          let field_loc1, ty1_proj_ty, relevancy1 =
            match Map.find ty1_map field_name with
            | Some t -> t
            | None ->
              Context.throw
                cx
                [ Diagnostic.Part.create
                    (Doc.string "Struct is not a subtype: could not find field "
                     ^^ Doc.string field_name)
                ]
          in
          unify_relevancy cx relevancy1 field_spec2.relevancy;
          let proj_term = Term_proj { strukt = e; field = field_loc1 } in
          let coerced_proj, field_coe =
            sub cx proj_term ty1_proj_ty (Evaluate.eval_ty closure_env field_spec2.ty)
          in
          let coerced_proj_term = Option.value ~default:proj_term coerced_proj in
          let did_coerce = did_coerce || Option.is_some coerced_proj in
          let field_impl : term_field_impl =
            { name = field_name
            ; e = coerced_proj_term
            ; relevancy = field_spec2.relevancy
            }
          in
          let field_coe : Typed.runtime_field_coe =
            { field = { name = field_name; index }; coe = field_coe }
          in
          ( did_coerce
          , Seq.push (Evaluate.eval_value Seq.empty coerced_proj_term) closure_env
          , Bwd.snoc field_impls field_impl
          , Bwd.snoc field_coes field_coe ))
    in
    let field_impls = Bwd.to_list field_impls in
    let field_coes = Bwd.to_list field_coes in
    let is_same_shape =
      match
        List.for_all2 ty1.field_specs ty2.field_specs ~f:(fun field_spec1 field_spec2 ->
          String.equal field_spec1.name.name field_spec2.name.name)
      with
      | Ok x -> x
      | Unequal_lengths -> false
    in
    let runtime_coe = mk_struct_coe is_same_shape field_coes in
    if not did_coerce
    then None, Typed.Id_coe
    else Some (Term_struct { field_impls }), runtime_coe
  | _ ->
    unify_ty cx ty1 ty2;
    None, Typed.Id_coe
;;

let coerce cx e ty1 ty2 =
  let e', _ = sub cx e ty1 ty2 in
  Option.value e' ~default:e
;;
