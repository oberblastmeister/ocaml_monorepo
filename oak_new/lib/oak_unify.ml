open Prelude
open Oak_syntax

open struct
  module Spanned = Utility.Spanned
  module Common = Oak_common
  module Name_list = Common.Name_list
  module Diagnostic = Oak_diagnostic
  module Context = Oak_context
  module Evaluate = Oak_evaluate
  module Close = Evaluate.Close
  module Infer_ty = Oak_infer_ty
end

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
    let arg : value_arg = { e = var_value; param_props = ty.param_props } in
    unify_value
      (Context.bind ty.name ty.param_ty cx)
      (Evaluate.Value.app e1 arg)
      (Evaluate.Value.app e2 arg)
      (Evaluate.Fun_ty.app ty arg)
  | Ty_decode ty ->
    let props = Infer_ty.infer_neutral_universe cx.ty_env ty in
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
    unify_param_props cx ty1.param_props ty2.param_props;
    let arg = { e = Context.next_free cx; param_props = ty1.param_props } in
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
  | _, _ -> failwith ""

and unify_param_props (cx : Context.t) (props1 : param_props) (props2 : param_props) =
  if not (Icit.equal props1.icit props2.icit)
  then
    Context.throw
      cx
      [ Diagnostic.Part.create
          (Doc.string "Icitness was not equal: "
           ^^ Icit.pp props1.icit
           ^^ Doc.string " != "
           ^^ Icit.pp props2.icit)
      ];
  unify_relevancy cx props1.relevancy props2.relevancy

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
  if not (Level.equal e1.head e2.head)
  then
    Context.throw
      cx
      [ Diagnostic.Part.create
          (Doc.string "Variables were not equal: "
           ^^ Context.pp_value cx (Value.free e1.head)
           ^^ Doc.string " != "
           ^^ Context.pp_value cx (Value.free e2.head))
      ];
  let spine1 = Bwd.to_list e1.spine in
  let spine2 = Bwd.to_list e1.spine in
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
      ~init:(Bwd.Empty, Context.level_var_ty cx e1.head)
      ~f:(fun (spine, ty) (frame1, frame2) ->
        let ty =
          match frame1, frame2 with
          | Out, _ | _, Out -> failwith "should be whnf"
          | App arg1, App arg2 ->
            let func_kind = Context.whnf_ty cx ty |> Ty.ty_fun_val_exn in
            unify_value cx arg1.e arg2.e func_kind.param_ty;
            Evaluate.Fun_ty.app func_kind arg1
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
;;
