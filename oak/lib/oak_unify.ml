open Prelude
module Core = Oak_core
module Bwd = Utility.Bwd
module Cow_slice = Utility.Cow_slice
module Common = Oak_common
module Core_ty = Common.Core_ty
module Icit = Common.Icit
module Relevancy = Common.Relevancy
module Size = Common.Size
module Diagnostic = Oak_diagnostic
module Context = Oak_context
module State = Oak_elaborate_state
module Typed = Oak_typed

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

let rec coerce_singleton (cx : Context.t) (e : Core.term) (ty : Core.ty)
  : Core.term * Core.ty
  =
  match Core.Ty.whnf cx.ty_env ty with
  | Ty_sing { identity = _; ty = kind } -> coerce_singleton cx (Term_sing_out e) kind
  | ty -> e, ty
;;

let rec unify_value
          (st : State.t)
          (cx : Context.t)
          (e1 : Core.value)
          (e2 : Core.value)
          (ty : Core.ty)
  : unit
  =
  match Core.Ty.whnf cx.ty_env ty with
  (* These are all transparent *)
  | Ty_pack _ | Ty_core _ | Ty_sing _ -> ()
  | Ty_universe _props -> unify_ty st cx (Core.Value.decode e1) (Core.Value.decode e2)
  | Ty_struct ty ->
    let _ =
      Cow_slice.foldi
        ty.field_specs
        ~init:(Cow_slice.create (Cow_slice.length ty.field_specs))
        ~f:(fun index running_field_impls (field_spec : Core.term_field_spec) ->
          let field : Core.field_loc = { name = field_spec.name.name; index } in
          let e1 = Core.Value.proj e1 field in
          let e2 = Core.Value.proj e2 field in
          let running_struct_value = Core.Value.create_struct running_field_impls in
          unify_value st cx e1 e2 (Core.Ty_struct.proj running_struct_value ty field).ty;
          Cow_slice.push_full_slice_exn
            running_field_impls
            (Core.Value_field_impl.create field.name e1))
    in
    ()
  | Ty_fun ty ->
    let var_value = Context.next_free cx in
    let arg : Core.value_arg = { e = var_value; icit = ty.param.modifiers.icit } in
    unify_value
      st
      (Context.bind ty.param.name ty.param.ty cx)
      (Core.Value.app e1 arg)
      (Core.Value.app e2 arg)
      (Core.Ty_fun.app ty arg)
  | Ty_decode ty ->
    let props = Core.Neutral.infer_universe cx.ty_env ty in
    if not (Size.is_type props.size)
    then begin
      let e1 = Core.Value.whnf cx.ty_env e1 |> Core.Value.neutral_val_exn in
      let e2 = Core.Value.whnf cx.ty_env e2 |> Core.Value.neutral_val_exn in
      unify_neutral st cx e1 e2
    end

and unify_ty (st : State.t) (cx : Context.t) (ty1 : Core.ty) (ty2 : Core.ty) =
  match Core.Ty.whnf cx.ty_env ty1, Core.Ty.whnf cx.ty_env ty2 with
  | Ty_universe props1, Ty_universe props2 -> unify_ty_props st props1 props2
  | Ty_sing ty1, Ty_sing ty2 ->
    unify_ty st cx ty1.ty ty2.ty;
    unify_value st cx ty1.identity ty2.identity ty1.ty
  | Ty_fun ty1, Ty_fun ty2 ->
    unify_param st cx ty1.param ty2.param;
    let arg : Core.value_arg =
      { e = Context.next_free cx; icit = ty1.param.modifiers.icit }
    in
    unify_ty
      st
      (Context.bind ty1.param.name ty1.param.ty cx)
      (Core.Ty_fun.app ty1 arg)
      (Core.Ty_fun.app ty2 arg)
  | Ty_struct ty1, Ty_struct ty2 ->
    let zipped_field_specs =
      match Cow_slice.zip ty1.field_specs ty2.field_specs with
      | Some t -> t
      | None ->
        State.throw
          st
          [ Diagnostic.Part.create
              (Doc.string "Record had different number of declarations. This record had"
               ^^ Doc.space
               ^^ Doc.string (Int.to_string (Cow_slice.length ty1.field_specs))
               ^^ Doc.indent 2 (Doc.break1 ^^ Context.pp_ty cx (Ty_struct ty1))
               ^^ Context.pp_ty cx (Ty_struct ty2)
               ^^ Doc.break1
               ^^ Doc.string "while this record had"
               ^^ Doc.space
               ^^ Doc.string (Int.to_string (Cow_slice.length ty2.field_specs))
               ^^ Doc.indent 2 (Doc.break1 ^^ Context.pp_ty cx (Ty_struct ty2)))
          ]
    in
    let _ =
      Cow_slice.foldi
        zipped_field_specs
        ~init:
          ( ~running_field_impls:(Cow_slice.create (Cow_slice.length zipped_field_specs))
          , ~cx )
        ~f:(fun index (~running_field_impls, ~cx) (field_spec1, field_spec2) ->
          let name1 = field_spec1.name.name in
          let name2 = field_spec2.name.name in
          if not (String.equal name1 name2)
          then
            State.throw
              st
              [ Diagnostic.Part.create
                  (Doc.string "Declaration name not equal: "
                   ^^ Doc.string name1
                   ^^ Doc.string " != "
                   ^^ Doc.string name2)
              ];
          unify_relevancy st field_spec1.relevancy field_spec2.relevancy;
          let field = Core.Field_loc.create name1 index in
          let running_struct_value = Core.Value.create_struct running_field_impls in
          let ty1 = (Core.Ty_struct.proj running_struct_value ty1 field).ty in
          let ty2 = (Core.Ty_struct.proj running_struct_value ty2 field).ty in
          unify_ty st cx ty1 ty2;
          let var_value = Context.next_free cx in
          ( ~running_field_impls:(Cow_slice.push_full_slice_exn
                                    running_field_impls
                                    (Core.Value_field_impl.create name1 var_value))
          , ~cx:(Context.bind field_spec1.name ty1 cx) ))
    in
    ()
  | Ty_core ty1, Ty_core ty2 ->
    if not (Core_ty.equal ty1 ty2)
    then
      State.throw
        st
        [ Diagnostic.Part.create
            (Doc.string "Base types were not equal: "
             ^^ Context.pp_ty cx (Ty_core ty1)
             ^^ Doc.string " != "
             ^^ Context.pp_ty cx (Ty_core ty2))
        ]
  | Ty_pack ty1, Ty_pack ty2 -> unify_ty st cx ty1 ty2
  | Ty_decode ty1, Ty_decode ty2 ->
    (* both ty1 and ty2 are whnf, or otherwise the decode is not whnf *)
    unify_neutral st cx ty1 ty2
  | _, _ ->
    State.throw
      st
      [ Diagnostic.Part.create
          (Doc.string "Types were not equal: "
           ^^ Context.pp_ty cx ty1
           ^^ Doc.string " != "
           ^^ Context.pp_ty cx ty2)
      ]

and unify_icit (st : State.t) (icit1 : Icit.t) (icit2 : Icit.t) =
  if not (Icit.equal icit1 icit2)
  then
    State.throw
      st
      [ Diagnostic.Part.create
          (Doc.string "Icitness was not equal: "
           ^^ Icit.pp icit1
           ^^ Doc.string " != "
           ^^ Icit.pp icit2)
      ]

and unify_param
      (st : State.t)
      (cx : Context.t)
      (param1 : Core.value_param)
      (param2 : Core.value_param)
  : unit
  =
  unify_ty st cx param1.ty param2.ty;
  unify_param_modifiers st param1.modifiers param2.modifiers

and unify_param_modifiers
      (st : State.t)
      (param_modifiers1 : Common.Param_modifiers.t)
      (param_modifiers2 : Common.Param_modifiers.t)
  : unit
  =
  unify_icit st param_modifiers1.icit param_modifiers2.icit;
  unify_relevancy st param_modifiers1.relevancy param_modifiers2.relevancy

and unify_relevancy (st : State.t) (relevancy1 : Relevancy.t) (relevancy2 : Relevancy.t)
  : unit
  =
  if not (Relevancy.equal relevancy1 relevancy2)
  then
    State.throw
      st
      [ Diagnostic.Part.create
          (Doc.string "Relevancy was not equal: "
           ^^ Relevancy.pp relevancy1
           ^^ Doc.string " != "
           ^^ Relevancy.pp relevancy2)
      ]

and unify_ty_props (st : State.t) (props1 : Core.Ty_props.t) (props2 : Core.Ty_props.t)
  : unit
  =
  if not (Size.equal props1.size props2.size)
  then
    State.throw
      st
      [ Diagnostic.Part.create
          (Doc.string "Sizes were not equal: "
           ^^ Size.pp props1.size
           ^^ Doc.string " != "
           ^^ Size.pp props2.size)
      ]

(* precondition: should be whnf *)
and unify_neutral (st : State.t) (cx : Context.t) (e1 : Core.neutral) (e2 : Core.neutral)
  : unit
  =
  unify_head st cx e1.head e2.head;
  let spine1 = Bwd.to_list e1.spine in
  let spine2 = Bwd.to_list e2.spine in
  let zipped_spines =
    match List.zip spine1 spine2 with
    | Unequal_lengths ->
      State.throw
        st
        [ Diagnostic.Part.create
            (Doc.string "Types were not equal (spine lengths differ)")
        ]
    | Ok t -> t
  in
  let _ =
    List.fold
      zipped_spines
      ~init:(~spine:Bwd.Empty, ~ty:(Core.Head.infer_ty cx.ty_env e1.head))
      ~f:(fun (~spine, ~ty) (frame1, frame2) ->
        let ty =
          match frame1, frame2 with
          | Out, _ | _, Out -> failwith "should be whnf"
          | App arg1, App arg2 ->
            let fun_ty = Core.Ty.whnf cx.ty_env ty |> Core.Ty.ty_fun_val_exn in
            unify_value st cx arg1.e arg2.e fun_ty.param.ty;
            Core.Ty_fun.app fun_ty arg1
          | Proj field1, Proj field2 ->
            if not (field1.index = field2.index)
            then
              State.throw
                st
                [ Diagnostic.Part.create
                    (Doc.string "Fields were not equal in a projection: "
                     ^^ Doc.string field1.name
                     ^^ Doc.string " != "
                     ^^ Doc.string field2.name)
                ];
            (Core.Ty.proj cx.ty_env (Value_neutral { head = e1.head; spine }) ty field1)
              .ty
          | _ ->
            State.throw
              st
              [ Diagnostic.Part.create
                  (Doc.string "Types were not equal: "
                   ^^ Context.pp_value cx (Value_neutral e1)
                   ^^ Doc.string " != "
                   ^^ Context.pp_value cx (Value_neutral e2))
              ]
        in
        ~spine:(spine <: frame1), ~ty)
  in
  ()

and unify_head (st : State.t) (cx : Context.t) (e1 : Core.head) (e2 : Core.head) : unit =
  match e1, e2 with
  | Free e1, Free e2 ->
    if not (Core.Level.equal e1 e2)
    then
      State.throw
        st
        [ Diagnostic.Part.create
            (Doc.string "Variables were not equal: "
             ^^ Context.pp_value cx (Core.Value.free e1)
             ^^ Doc.string " != "
             ^^ Context.pp_value cx (Core.Value.free e2))
        ]
  | _ -> failwith ""

(* TODO: fix this, the runtime_coe is wrong *)
(* postcondition: if term is None then runtime_coe must be Id_coe *)
and sub (st : State.t) (cx : Context.t) (e : Core.term) (ty1 : Core.ty) (ty2 : Core.ty)
  : Core.term option * Typed.runtime_coe
  =
  match Core.Ty.whnf cx.ty_env ty1, Core.Ty.whnf cx.ty_env ty2 with
  | Ty_universe props1, Ty_universe props2 ->
    (* TODO: maybe do cumulativity here *)
    unify_ty_props st props1 props2;
    None, Typed.Id_coe
  | Ty_core ty1, Ty_core ty2 ->
    if not (Core_ty.equal ty1 ty2)
    then
      State.throw
        st
        [ Diagnostic.Part.create
            (Doc.string "Base types were not equal: "
             ^^ Context.pp_ty cx (Ty_core ty1)
             ^^ Doc.string " != "
             ^^ Context.pp_ty cx (Ty_core ty2))
        ];
    None, Typed.Id_coe
  | Ty_sing ty1, Ty_sing ty2 ->
    let e', coe = sub st cx (Term_sing_out e) ty1.ty ty2.ty in
    begin match e' with
    | None ->
      unify_value st cx ty1.identity ty2.identity ty1.ty;
      None, coe
    | Some e' ->
      unify_value st cx (Core.Term.eval Core.Value_env.empty e') ty2.identity ty2.ty;
      ((Some (Term_sing_in e'), coe) : Core.term option * Typed.runtime_coe)
    end
  | Ty_sing _, _ ->
    let e, ty1 = coerce_singleton cx e ty1 in
    let e', coe = sub st cx e ty1 ty2 in
    Some (Option.value ~default:e e'), coe
  | _, Ty_sing ty2 ->
    let e', coe = sub st cx e ty1 ty2.ty in
    let e' = Option.value ~default:e e' in
    unify_value st cx (Core.Term.eval Core.Value_env.empty e') ty2.identity ty2.ty;
    ((Some (Term_sing_in e'), coe) : Core.term option * Typed.runtime_coe)
  | Ty_fun ty1, Ty_fun ty2 ->
    unify_param_modifiers st ty1.param.modifiers ty2.param.modifiers;
    let free = Context.next_level cx in
    let arg_var_value = Context.next_free cx in
    let cx = Context.bind ty2.param.name ty2.param.ty cx in
    let arg_var_term = Core.Value.quote arg_var_value in
    let arg', arg_coe = sub st cx arg_var_term ty2.param.ty ty1.param.ty in
    let arg_term = Option.value ~default:arg_var_term arg' in
    let arg_value = Core.Term.eval Core.Value_env.empty arg_term in
    let app_term : Core.term =
      Term_app
        { func = e
        ; arg = ({ e = arg_term; icit = ty1.param.modifiers.icit } : Core.term_arg)
        }
    in
    let body', ret_coe =
      sub
        st
        cx
        app_term
        (Core.Ty_fun.app
           ty1
           ({ e = arg_value; icit = ty1.param.modifiers.icit } : Core.value_arg))
        (Core.Ty_fun.app
           ty2
           ({ e = arg_value; icit = ty2.param.modifiers.icit } : Core.value_arg))
    in
    let runtime_coe = mk_fun_coe arg_coe ret_coe in
    let body_term = Option.value ~default:app_term body' in
    if Option.is_none arg' && Option.is_none body'
    then None, Typed.Id_coe
    else
      (( Some
           (Term_fun
              { name = ty2.param.name
              ; icit = ty2.param.modifiers.icit
              ; body = Core.Term.close_single free body_term
              })
       , runtime_coe )
       : Core.term option * Typed.runtime_coe)
  | Ty_struct ty1, Ty_struct ty2 ->
    let value1 = Core.Term.eval Core.Value_env.empty e in
    let ty1_name_to_index =
      let tbl = String.Table.create () in
      Core.Ty_struct.field_spec_views ty1
      |> Cow_slice.iteri ~f:(fun index field_spec ->
        Hashtbl.add_exn tbl ~key:field_spec.name.name ~data:index);
      tbl
    in
    let ty2_field_spec_views = Core.Ty_struct.field_spec_views ty2 in
    let ~did_coerce, ~running_field_impls2, ~running_field_coes =
      Cow_slice.foldi
        ty2_field_spec_views
        ~init:
          ( ~did_coerce:false
          , ~running_field_impls2:(Cow_slice.create (Cow_slice.length ty2_field_spec_views))
          , ~running_field_coes:(Cow_slice.create (Cow_slice.length ty2_field_spec_views))
          )
        ~f:(fun index (~did_coerce, ~running_field_impls2, ~running_field_coes) field2 ->
          let field2 = Core.Field_loc.create field2.name.name index in
          let field1 =
            match Hashtbl.find ty1_name_to_index field2.name with
            | Some index -> Core.Field_loc.create field2.name index
            | None ->
              State.throw
                st
                [ Diagnostic.Part.create
                    (Doc.string "Source struct is missing field "
                     ^^ Doc.string field2.name
                     ^^ Doc.string " required by the target signature")
                ]
          in
          let running_struct_value2 = Core.Value.create_struct running_field_impls2 in
          let field_impl1 = Core.Value.proj value1 field1 in
          let field_spec1 = Core.Ty_struct.proj value1 ty1 field1 in
          let field_spec2 = Core.Ty_struct.proj running_struct_value2 ty2 field2 in
          unify_relevancy st field_spec1.relevancy field_spec2.relevancy;
          let coerced_field_impl2, field_coe =
            sub st cx (Core.Value.quote field_impl1) field_spec1.ty field_spec2.ty
          in
          let did_coerce = did_coerce || Option.is_some coerced_field_impl2 in
          let coerced_field_impl2 =
            Option.value ~default:(Core.Value.quote field_impl1) coerced_field_impl2
          in
          let value_field_impl2 : Core.value_field_impl =
            { name = field2.name
            ; e = Core.Term.eval Core.Value_env.empty coerced_field_impl2
            }
          in
          let field_coe : Typed.runtime_field_coe = { field = field2; coe = field_coe } in
          ( ~did_coerce
          , ~running_field_impls2:(Cow_slice.push_full_slice_exn
                                     running_field_impls2
                                     value_field_impl2)
          , ~running_field_coes:(Cow_slice.push_full_slice_exn
                                   running_field_coes
                                   field_coe) ))
    in
    let field_coes = Cow_slice.to_list running_field_coes in
    let is_same_shape =
      Cow_slice.for_all2
        (Core.Ty_struct.field_spec_views ty1)
        ty2_field_spec_views
        ~f:(fun field1 field2 -> String.equal field1.name.name field2.name.name)
      |> Option.value ~default:false
    in
    let runtime_coe = mk_struct_coe is_same_shape field_coes in
    if not did_coerce
    then None, Typed.Id_coe
    else
      ( Some (Core.Value.quote (Value_struct { field_impls = running_field_impls2 }))
      , runtime_coe )
  | _ ->
    unify_ty st cx ty1 ty2;
    None, Typed.Id_coe
;;

let coerce st cx e ty1 ty2 =
  let e', _ = sub st cx e ty1 ty2 in
  Option.value e' ~default:e
;;
