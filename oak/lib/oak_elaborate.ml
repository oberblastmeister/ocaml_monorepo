open Prelude
module Core = Oak_core
module Bwd = Utility.Bwd
module Cow_slice = Utility.Cow_slice
module Span = Utility.Span
module Common = Oak_common
module Icit = Common.Icit
module Literal = Common.Literal
module Size = Common.Size
module Diagnostic = Oak_diagnostic
module Unify = Oak_unify
module Context = Oak_context
module State = Oak_elaborate_state
module Close = Core.Close
module Abstract = Oak_abstract
module Typed = Oak_typed

let expr_ann (cx : Context.t) (span : Span.t) (term : Core.term) (ty : Core.ty)
  : Typed.expr_ann
  =
  { span; context = { ty_env = cx.ty_env; name_list = cx.name_list }; ty; term }
;;

let ty_ann
      (cx : Context.t)
      (span : Span.t)
      (term : Core.term_ty)
      (ty_props : Core.Ty_props.t)
  : Typed.ty_ann
  =
  { span; context = { ty_env = cx.ty_env; name_list = cx.name_list }; ty_props; term }
;;

let extract_fun_ty (st : State.t) (cx : Context.t) (func_ty : Core.ty) : Core.ty_fun =
  match Core.Ty.whnf cx.ty_env func_ty with
  | Ty_fun func_ty -> func_ty
  | ty ->
    State.throw
      st
      [ Diagnostic.Part.create
          (Doc.string "Expected function type, got " ^^ Context.pp_ty cx ty)
      ]
;;

let extract_struct_ty (st : State.t) (cx : Context.t) (strukt_ty : Core.ty)
  : Core.ty_struct
  =
  match Core.Ty.whnf cx.ty_env strukt_ty with
  | Ty_struct strukt_ty -> strukt_ty
  | ty ->
    State.throw
      st
      [ Diagnostic.Part.create
          (Doc.string "Expected struct type, got " ^^ Context.pp_ty cx ty)
      ]
;;

let extract_pack_ty (st : State.t) (cx : Context.t) (packed_ty : Core.ty) : Core.ty =
  match Core.Ty.whnf cx.ty_env packed_ty with
  | Ty_pack ty -> ty
  | ty ->
    State.throw
      st
      [ Diagnostic.Part.create
          (Doc.string "Expected pack type, got " ^^ Context.pp_ty cx ty)
      ]
;;

let eval_expr (e : Typed.expr) : Core.value =
  Core.Term.eval Core.Value_env.empty (Typed.Expr.term e)
;;

let infer_literal_ty (st : State.t) (span : Span.t) (literal : Literal.t) : Core.ty =
  match literal with
  | Literal.Unit -> Ty_core Unit
  | Literal.Bool _ -> Ty_core Bool
  | Literal.Int _ -> Ty_core Int
  | Literal.String _ ->
    State.throw
      st
      [ Diagnostic.Part.create
          ~snippet:(State.snippet st span)
          (Doc.string "String literals are not supported in elaboration yet")
      ]
;;

let maybe_sing_in (is_abstract : bool) (rhs : Core.term) : Core.term =
  if is_abstract then rhs else Term_sing_in rhs
;;

let rec synthesize_transparent_ty (st : State.t) (cx : Context.t) (ty : Core.ty)
  : Core.term
  =
  match Core.Ty.whnf cx.ty_env ty with
  | Ty_universe _ ->
    State.throw
      st
      [ Diagnostic.Part.create
          (Doc.string "Universes are not transparent: " ^^ Context.pp_ty cx ty)
      ]
  | Ty_sing { identity; ty = _ } -> Term_sing_in (Core.Value.quote identity)
  | Ty_struct ty ->
    let field_locations = Core.Ty_struct.field_locations ty in
    let ~field_impls, .. =
      Cow_slice.fold
        field_locations
        ~init:
          ( ~cx
          , ~running_field_impls:(Cow_slice.create (Cow_slice.length field_locations))
          , ~field_impls:(Cow_slice.create (Cow_slice.length field_locations)) )
        ~f:(fun (~cx, ~running_field_impls, ~field_impls) (field : Core.field_loc) ->
          let running_struct_value =
            Core.Value.create_struct running_field_impls
          in
          let field_spec = Core.Ty_struct.proj running_struct_value ty field in
          let synthesized_term = synthesize_transparent_ty st cx field_spec.ty in
          let term_field_impl : Core.term_field_impl =
            { name = field_spec.name.name; e = synthesized_term }
          in
          (* Make sure to push the synthesized term instead of just a free variable because the resulting structure should be non dependent, each field cannot depend on the previous one *)
          ( ~cx:(Context.bind field_spec.name field_spec.ty cx)
          , ~running_field_impls:
              (Cow_slice.push_full_slice_exn
                 running_field_impls
                 (Core.Value_field_impl.create
                    field.name
                    (Core.Term.eval Core.Value_env.empty synthesized_term)))
          , ~field_impls:(Cow_slice.push_full_slice_exn field_impls term_field_impl) ))
    in
    Term_struct { field_impls }
  | Ty_fun ({ param = { name; param; modifiers }; _ } as ty) ->
    let body =
      synthesize_transparent_ty
        st
        (Context.bind name param cx)
        (Core.Ty_fun.app
           ty
           ({ e = Context.next_free cx; icit = modifiers.icit } : Core.value_arg))
    in
    Term_fun
      { name
      ; icit = modifiers.icit
      ; body = Core.Term.close_single (Context.next_level cx) body
      }
  | Ty_core _ | Ty_pack _ -> Term_ignore
  | Ty_decode e ->
    let ty_props = Core.Neutral.infer_universe cx.ty_env e in
    if Size.is_type ty_props.size
    then Term_ignore
    else
      State.throw
        st
        [ Diagnostic.Part.create
            (Doc.string "This type was not transparent since its universe was not Type: "
             ^^ Context.pp_value cx (Value_neutral e))
        ]
;;

exception Same_signature

let rec apply_patch
          (st : State.t)
          (cx : Context.t)
          (path : string list)
          (term_to_coerce_to_original_ty : Core.term)
          (original_ty : Core.ty)
          (patch_with : Core.term)
          (patch_with_ty : Core.ty)
  : Core.term * Core.ty
  =
  match path with
  | [] -> failwith "expected nonempty list"
  | path_part :: path ->
    let original_ty = extract_struct_ty st cx original_ty in
    let field_locations = Core.Ty_struct.field_locations original_ty in
    let ~coerced_field_impls, ~patched_field_specs, ~did_find_field, .. =
      Cow_slice.fold
        field_locations
        ~init:
          ( ~cx
          , ~running_field_impls:(Cow_slice.create (Cow_slice.length field_locations))
          , ~close:Close.empty
          , ~coerced_field_impls:(Cow_slice.create (Cow_slice.length field_locations))
          , ~patched_field_specs:(Cow_slice.create (Cow_slice.length field_locations))
          , ~did_find_field:false )
        ~f:
          (fun
            ( ~cx
            , ~running_field_impls
            , ~close
            , ~coerced_field_impls
            , ~patched_field_specs
            , ~did_find_field )
            field
          ->
          let running_struct_value =
            Core.Value.create_struct running_field_impls
          in
          let original_field_spec =
            Core.Ty_struct.proj running_struct_value original_ty field
          in
          let original_field_ty = original_field_spec.ty in
          let coerced_term, patched_field_ty, did_find_field =
            if (not did_find_field) && String.equal field.name path_part
            then begin
              let coerced_term, patched_field_ty =
                match path with
                | [] -> begin
                  match Core.Ty.whnf cx.ty_env original_field_ty with
                  | Ty_sing original_ty ->
                    (* already a singleton, just check for equality *)
                    let patch_with_coerced =
                      Unify.coerce st cx patch_with patch_with_ty original_ty.ty
                    in
                    Unify.unify_value
                      st
                      cx
                      (Core.Term.eval Core.Value_env.empty patch_with_coerced)
                      original_ty.identity
                      original_ty.ty;
                    raise_notrace Same_signature
                  | _ ->
                    let patch_with_coerced =
                      Unify.coerce st cx patch_with patch_with_ty original_field_ty
                      |> Core.Term.eval Core.Value_env.empty
                    in
                    (( Term_sing_out (Term_free (Context.next_level cx))
                     , Ty_sing { identity = patch_with_coerced; ty = original_field_ty }
                     )
                     : Core.term * Core.ty)
                end
                | _ :: _ -> begin
                  match Core.Ty.whnf cx.ty_env original_field_ty with
                  | Ty_sing original_field_ty ->
                    let coerced_term, patched_field_ty =
                      apply_patch
                        st
                        cx
                        path
                        (Term_sing_out (Term_free (Context.next_level cx)))
                        original_field_ty.ty
                        patch_with
                        patch_with_ty
                    in
                    let coerced_term : Core.term = Term_sing_in coerced_term in
                    let coerced_identity =
                      Unify.coerce
                        st
                        cx
                        (Core.Value.quote original_field_ty.identity)
                        original_field_ty.ty
                        patched_field_ty
                    in
                    let patched_field_ty : Core.ty =
                      Ty_sing
                        { identity = Core.Term.eval Core.Value_env.empty coerced_identity
                        ; ty = patched_field_ty
                        }
                    in
                    coerced_term, patched_field_ty
                  | _ ->
                    let coerced_term, patched_field_ty =
                      apply_patch
                        st
                        cx
                        path
                        (Term_free (Context.next_level cx))
                        original_field_ty
                        patch_with
                        patch_with_ty
                    in
                    coerced_term, patched_field_ty
                end
              in
              coerced_term, patched_field_ty, true
            end
            else begin
              ( Core.Term.of_level (Context.next_level cx)
              , original_field_ty
              , did_find_field )
            end
          in
          let coerced_field_impl : Core.term_field_impl =
            { name = field.name
            ; e =
                coerced_term
                |> Core.Term.close_single (Context.next_level cx)
                |> Core.Term.eval
                     (Core.Value_env.push
                        (Core.Term.eval
                           Core.Value_env.empty
                           (Term_proj { strukt = term_to_coerce_to_original_ty; field }))
                        Core.Value_env.empty)
                |> Core.Value.quote
            }
          in
          let patched_field_spec : Core.term_field_spec =
            { name = original_field_spec.name
            ; ty = Core.Ty.quote patched_field_ty |> Core.Term_ty.close close
            ; relevancy = original_field_spec.relevancy
            }
          in
          let field_impl =
            Core.Value_field_impl.create
              field.name
              (Core.Term.eval Core.Value_env.empty coerced_term)
          in
          ( ~cx:(Context.bind (Core.Name.create field.name Span.empty) patched_field_ty cx)
          , ~running_field_impls:(Cow_slice.push_full_slice_exn running_field_impls field_impl)
          , ~close:(Close.push_exn (Context.next_level cx) close)
          , ~coerced_field_impls:
              (Cow_slice.push_full_slice_exn coerced_field_impls coerced_field_impl)
          , ~patched_field_specs:
              (Cow_slice.push_full_slice_exn patched_field_specs patched_field_spec)
          , ~did_find_field ))
    in
    if not did_find_field
    then
      State.throw
        st
        [ Diagnostic.Part.create
            (Doc.string "Field "
             ^^ Doc.string path_part
             ^^ Doc.string " not found in struct")
        ];
    ( (Term_struct { field_impls = coerced_field_impls } : Core.term)
    , (Ty_struct (Core.Ty_struct.of_iterated_binders patched_field_specs) : Core.ty) )
;;

let with_elab_context (st : State.t) (span : Span.t) (message : string) ~f =
  State.with_context
    st
    (Diagnostic.Part.create ~snippet:(State.snippet st span) (Doc.string message))
    ~f
;;

let rec coerce_singleton (cx : Context.t) (e : Core.term) (ty : Core.ty)
  : Core.term * Core.ty
  =
  match Core.Ty.whnf cx.ty_env ty with
  | Ty_sing { identity = _; ty = kind } -> coerce_singleton cx (Term_sing_out e) kind
  | ty -> e, ty
;;

(* postcondition: the type in Typed.expr should be the type of the core term *)
let rec infer (st : State.t) (cx : Context.t) (e : Abstract.expr) : Typed.expr =
  match e with
  | Expr_data_rec { decls; span } -> failwith ""
  | Expr_data data -> failwith ""
  | Expr_var { index; span } ->
    let term : Core.term = Term_free (Core.Index.to_level (Context.size cx) index) in
    let ty = Core.Ty_env.get_index_exn cx.ty_env index in
    let term, ty = coerce_singleton cx term ty in
    Typed.Expr_var { index; ann = expr_ann cx span term ty }
  | Expr_ann { e; ty; span = _ } ->
    let ty_typed = check_universe st cx ty in
    let ty = Core.Term_ty.eval Core.Value_env.empty (Typed.Ty.term ty_typed) in
    let e_typed = check st cx e ty in
    Typed.Expr_ann { e = e_typed; ty = ty_typed; ann = Typed.Expr.ann e_typed }
  | Expr_core_ty { ty; span } ->
    let typed_ty =
      Typed.Ty_core { ty; ann = ty_ann cx span (Term_ty_core ty) { size = Size.type_ } }
    in
    Typed.Expr.of_ty typed_ty
  | Expr_error { span } ->
    State.throw
      st
      [ Diagnostic.Part.create
          ~snippet:(State.snippet st span)
          (Doc.string "Cannot infer error term")
      ]
  | Expr_app { func; arg; param_modifiers; span } ->
    let func = infer st cx func in
    let func_ty =
      with_elab_context st span "while inferring the function application" ~f:(fun () ->
        extract_fun_ty st cx (Typed.Expr.ty func))
    in
    if not (Icit.equal func_ty.param.modifiers.icit param_modifiers.icit)
    then
      State.throw
        st
        [ Diagnostic.Part.create
            ~snippet:(State.snippet st span)
            (Doc.string "Expected "
             ^^ Icit.pp func_ty.param.modifiers.icit
             ^^ Doc.string " argument, got "
             ^^ Icit.pp param_modifiers.icit
             ^^ Doc.string " argument")
        ];
    let arg = check st cx arg func_ty.param.param in
    let term_arg : Core.term_arg =
      { e = Typed.Expr.term arg; icit = func_ty.param.modifiers.icit }
    in
    let term : Core.term = Term_app { func = Typed.Expr.term func; arg = term_arg } in
    let value_arg : Core.value_arg =
      { e = eval_expr arg; icit = func_ty.param.modifiers.icit }
    in
    let ty = Core.Ty_fun.app func_ty value_arg in
    Typed.Expr_app { func; arg; param_modifiers; ann = expr_ann cx span term ty }
  | Expr_fun { name; param_ty = Some param_ty; param_modifiers; body; span } ->
    let param_ty_typed = check_universe st cx param_ty in
    let param_ty =
      Core.Term_ty.eval Core.Value_env.empty (Typed.Ty.term param_ty_typed)
    in
    let cx' = Context.bind name param_ty cx in
    let body = infer st cx' body in
    let body_ty : Core.ty_closure =
      { env = Core.Value_env.empty
      ; body =
          Core.Ty.quote (Typed.Expr.ty body)
          |> Core.Term_ty.close_single (Context.next_level cx)
      }
    in
    let term : Core.term =
      Term_fun
        { name
        ; icit = param_modifiers.icit
        ; body = Core.Term.close_single (Context.next_level cx) (Typed.Expr.term body)
        }
    in
    let ty : Core.ty =
      Ty_fun { param = { name; modifiers = param_modifiers; param = param_ty }; body_ty }
    in
    Typed.Expr_fun
      { name
      ; param_ty = Some param_ty_typed
      ; param_modifiers
      ; body
      ; ann = expr_ann cx span term ty
      }
  | Expr_fun { span; _ } ->
    State.throw
      st
      [ Diagnostic.Part.create
          ~snippet:(State.snippet st span)
          (Doc.string "Cannot infer lambda without parameter type annotation")
      ]
  | Expr_ty_fun { name; param_ty; param_modifiers; body_ty; span } ->
    let param_ty_typed = check_universe st cx param_ty in
    let param_ty =
      Core.Term_ty.eval Core.Value_env.empty (Typed.Ty.term param_ty_typed)
    in
    let body_ty_typed = check_universe st (Context.bind name param_ty cx) body_ty in
    let typed_ty =
      Typed.Ty_fun
        { name
        ; param_ty = param_ty_typed
        ; param_modifiers
        ; body_ty = body_ty_typed
        ; ann =
            ty_ann
              cx
              span
              (Term_ty_fun
                 { param =
                     { name
                     ; param = Typed.Ty.term param_ty_typed
                     ; modifiers = param_modifiers
                     }
                 ; body_ty =
                     Typed.Ty.term body_ty_typed
                     |> Core.Term_ty.close_single (Context.next_level cx)
                 })
              { size =
                  Size.max
                    (Typed.Ty.props param_ty_typed).size
                    (Typed.Ty.props body_ty_typed).size
              }
        }
    in
    Typed.Expr.of_ty typed_ty
  | Expr_proj { strukt; field; span } ->
    let strukt = infer st cx strukt in
    let struct_ty =
      with_elab_context st span "while projecting a struct field" ~f:(fun () ->
        extract_struct_ty st cx (Typed.Expr.ty strukt))
    in
    let field_loc =
      match
        Cow_slice.find_mapi struct_ty.field_specs ~f:(fun index field_spec ->
          if String.equal field_spec.name.name field
          then Some ({ name = field; index } : Core.field_loc)
          else None)
      with
      | Some field_loc -> field_loc
      | None ->
        State.throw
          st
          [ Diagnostic.Part.create
              ~snippet:(State.snippet st span)
              (Doc.string "Struct does not have field " ^^ Doc.string field)
          ]
    in
    let term : Core.term =
      Term_proj { strukt = Typed.Expr.term strukt; field = field_loc }
    in
    let ty =
      (Core.Ty.proj cx.ty_env (eval_expr strukt) (Typed.Expr.ty strukt) field_loc).ty
    in
    let term, ty = coerce_singleton cx term ty in
    Typed.Expr_proj { strukt; field; ann = expr_ann cx span term ty }
  | Expr_struct { decls; span; is_dependent = true } ->
    let decl_count = List.length decls in
    let ~typed_decls, ~let_bindings, ~field_specs, .. =
      List.fold
        decls
        ~init:
          ( ~cx
          , ~close:Close.empty
          , ~typed_decls:Bwd.Empty
          , ~let_bindings:Bwd.Empty
          , ~field_specs:(Cow_slice.create decl_count) )
        ~f:(fun (~cx, ~close, ~typed_decls, ~let_bindings, ~field_specs) decl ->
          let e = infer st cx decl.e in
          let ty =
            if decl.is_abstract
            then Typed.Expr.ty e
            else begin
              let rhs_value = eval_expr e in
              Ty_sing { identity = rhs_value; ty = Typed.Expr.ty e }
            end
          in
          let typed_decl : Typed.expr_decl =
            { name = decl.name; relevancy = decl.relevancy; e; span = decl.span }
          in
          let rhs =
            maybe_sing_in decl.is_abstract (Typed.Expr.term e) |> Core.Term.close close
          in
          let field_spec : Core.term_field_spec =
            { name = decl.name
            ; ty = Core.Ty.quote ty |> Core.Term_ty.close close
            ; relevancy = decl.relevancy
            }
          in
          let level = Context.next_level cx in
          ( ~cx:(Context.bind decl.name ty cx)
          , ~close:(Close.push_exn level close)
          , ~typed_decls:(Bwd.snoc typed_decls typed_decl)
          , ~let_bindings:(Bwd.snoc let_bindings (decl.name, rhs))
          , ~field_specs:(Cow_slice.push_full_slice_exn field_specs field_spec) ))
    in
    let typed_decls = Bwd.to_list typed_decls in
    let let_bindings = Bwd.to_list let_bindings in
    let term : Core.term =
      List.fold_right
        let_bindings
        ~init:
          (Term_struct
             { field_impls =
                 List.mapi decls ~f:(fun i { name; relevancy = _; _ } ->
                   ({ name = name.name
                    ; e = Term_bound (Core.Index.of_int (decl_count - i - 1))
                    }
                    : Core.term_field_impl))
                 |> Cow_slice.of_list
             })
        ~f:(fun (name, rhs) body -> (Term_let { name; rhs; body } : Core.term))
    in
    let ty : Core.ty = Ty_struct (Core.Ty_struct.of_iterated_binders field_specs) in
    Typed.Expr_struct
      { decls = typed_decls; ann = expr_ann cx span term ty; is_dependent = true }
  | Expr_struct { decls; span; is_dependent = false } ->
    let num_decls = List.length decls in
    let ~typed_decls, ~field_impls, ~field_specs =
      List.fold
        decls
        ~init:
          ( ~typed_decls:Bwd.Empty
          , ~field_impls:(Cow_slice.create num_decls)
          , ~field_specs:(Cow_slice.create num_decls) )
        ~f:(fun (~typed_decls, ~field_impls, ~field_specs) decl ->
          let e = infer st cx decl.e in
          let ty =
            if decl.is_abstract
            then Typed.Expr.ty e
            else begin
              let rhs_value = eval_expr e in
              Ty_sing { identity = rhs_value; ty = Typed.Expr.ty e }
            end
          in
          let typed_decl : Typed.expr_decl =
            { name = decl.name; relevancy = decl.relevancy; e; span = decl.span }
          in
          let rhs = maybe_sing_in decl.is_abstract (Typed.Expr.term e) in
          let field_spec : Core.term_field_spec =
            { name = decl.name; ty = Core.Ty.quote ty; relevancy = decl.relevancy }
          in
          let field_impl : Core.term_field_impl = { name = decl.name.name; e = rhs } in
          ( ~typed_decls:(Bwd.snoc typed_decls typed_decl)
          , ~field_impls:(Cow_slice.push_full_slice_exn field_impls field_impl)
          , ~field_specs:(Cow_slice.push_full_slice_exn field_specs field_spec) ))
    in
    let typed_decls = Bwd.to_list typed_decls in
    let term : Core.term = Term_struct { field_impls } in
    (*
      This is non dependent, the field_specs don't have any bound variables, so they can be weakened to ones that do take bound variables
    *)
    let ty : Core.ty = Ty_struct { env = Core.Value_env.empty; field_specs } in
    Typed.Expr_struct
      { decls = typed_decls; ann = expr_ann cx span term ty; is_dependent = false }
  | Expr_ty_struct { field_specs; span } ->
    let num_field_specs = List.length field_specs in
    let ~typed_field_specs, ~field_specs, ~size, .. =
      List.fold
        field_specs
        ~init:
          ( ~cx
          , ~close:Close.empty
          , ~typed_field_specs:Bwd.Empty
          , ~field_specs:(Cow_slice.create num_field_specs)
          , ~size:Size.sig_ )
        ~f:(fun (~cx, ~close, ~typed_field_specs, ~field_specs, ~size) field_spec ->
          let typed_field_ty, typed_rhs, ty =
            match field_spec.ty, field_spec.rhs with
            | Some ty, rhs ->
              let typed_ty = check_universe st cx ty in
              let field_ty =
                Core.Term_ty.eval Core.Value_env.empty (Typed.Ty.term typed_ty)
              in
              let typed_rhs, ty =
                match rhs with
                | None -> None, field_ty
                | Some rhs ->
                  let rhs = check st cx rhs field_ty in
                  let rhs_value = eval_expr rhs in
                  Some rhs, Ty_sing { identity = rhs_value; ty = field_ty }
              in
              Some typed_ty, typed_rhs, ty
            | None, Some rhs ->
              let rhs = infer st cx rhs in
              let field_ty = Typed.Expr.ty rhs in
              let rhs_value = eval_expr rhs in
              None, Some rhs, Ty_sing { identity = rhs_value; ty = field_ty }
            | None, None ->
              failwith "rename should reject signature fields without a type or rhs"
          in
          let typed_field_spec : Typed.field_spec =
            { name = field_spec.name
            ; relevancy = field_spec.relevancy
            ; ty = typed_field_ty
            ; rhs = typed_rhs
            ; span = field_spec.span
            }
          in
          let field_spec : Core.term_field_spec =
            { name = field_spec.name
            ; ty = Core.Ty.quote ty |> Core.Term_ty.close close
            ; relevancy = field_spec.relevancy
            }
          in
          let level = Context.next_level cx in
          ( ~cx:(Context.bind field_spec.name ty cx)
          , ~close:(Close.push_exn level close)
          , ~typed_field_specs:(Bwd.snoc typed_field_specs typed_field_spec)
          , ~field_specs:(Cow_slice.push_full_slice_exn field_specs field_spec)
          , ~size:(Size.max
                     size
                     (match typed_field_ty with
                      | Some typed_ty -> (Typed.Ty.props typed_ty).size
                      | None -> (Core.Ty.infer_props cx.ty_env ty).size)) ))
    in
    let typed_field_specs = Bwd.to_list typed_field_specs in
    let typed_ty =
      Typed.Ty_struct
        { field_specs = typed_field_specs
        ; ann =
            ty_ann
              cx
              span
              (Term_ty_struct (Core.Term_ty_struct.of_iterated_binders field_specs))
              { size }
        }
    in
    Typed.Expr.of_ty typed_ty
  | Expr_let { name; rhs; relevancy; is_abstract; body; span } ->
    let rhs = infer st cx rhs in
    let rhs_value = eval_expr rhs in
    let rhs_ty : Core.ty =
      if is_abstract
      then Typed.Expr.ty rhs
      else Ty_sing { identity = rhs_value; ty = Typed.Expr.ty rhs }
    in
    let cx' = Context.bind name rhs_ty cx in
    let body = infer st cx' body in
    let term : Core.term =
      Term_let
        { name
        ; rhs = maybe_sing_in is_abstract (Typed.Expr.term rhs)
        ; body = Core.Term.close_single (Context.next_level cx) (Typed.Expr.term body)
        }
    in
    let ty =
      Core.Term_ty.eval
        (Core.Value_env.push
           (if is_abstract then rhs_value else Value_sing_in rhs_value)
           Core.Value_env.empty)
        (Core.Ty.quote (Typed.Expr.ty body)
         |> Core.Term_ty.close_single (Context.next_level cx))
    in
    Typed.Expr_let
      { name; rhs; relevancy; is_abstract; body; ann = expr_ann cx span term ty }
  | Expr_universe { size; span } ->
    let typed_ty =
      Typed.Ty_universe
        { size
        ; ann = ty_ann cx span (Term_ty_universe { size }) { size = Size.incr size }
        }
    in
    Typed.Expr.of_ty typed_ty
  | Expr_if { cond; body1; body2; span } ->
    let cond = check st cx cond (Ty_core Bool) in
    let body1 = infer st cx body1 in
    let body2 = infer st cx body2 in
    let body1_props = Core.Ty.infer_props cx.ty_env (Typed.Expr.ty body1) in
    let body2_props = Core.Ty.infer_props cx.ty_env (Typed.Expr.ty body2) in
    if not (Size.is_type body1_props.size)
    then
      State.throw
        st
        [ Diagnostic.Part.create
            ~snippet:(State.snippet st (Typed.Expr.span body1))
            (Doc.string "The first branch did not have a type in universe Type")
        ];
    if not (Size.is_type body2_props.size)
    then
      State.throw
        st
        [ Diagnostic.Part.create
            ~snippet:(State.snippet st (Typed.Expr.span body2))
            (Doc.string "The second branch did not have a type in universe Type")
        ];
    with_elab_context st span "in the if expression" ~f:(fun () ->
      Unify.unify_ty st cx (Typed.Expr.ty body1) (Typed.Expr.ty body2));
    Typed.Expr_if
      { cond; body1; body2; ann = expr_ann cx span Term_ignore (Typed.Expr.ty body1) }
  | Expr_ty_pack { ty; span } ->
    let typed_ty = check_universe st cx ty in
    let typed_ty =
      Typed.Ty_pack
        { ty = typed_ty
        ; ann =
            ty_ann cx span (Term_ty_pack (Typed.Ty.term typed_ty)) { size = Size.type_ }
        }
    in
    Typed.Expr.of_ty typed_ty
  | Expr_pack { e; span } ->
    let e = infer st cx e in
    let ty : Core.ty = Ty_pack (Typed.Expr.ty e) in
    Typed.Expr_pack { e; ann = expr_ann cx span Term_ignore ty }
  | Expr_bind { span; _ } ->
    State.throw
      st
      [ Diagnostic.Part.create
          ~snippet:(State.snippet st span)
          (Doc.string "Cannot infer bind expressions")
      ]
  | Expr_literal { literal; span } ->
    let ty = infer_literal_ty st span literal in
    Typed.Expr_literal { literal; ann = expr_ann cx span Term_ignore ty }
  | Expr_rec { decls; span } ->
    (* let typed_tys, tys =
      List.unzip
        (List.map decls ~f:(fun decl ->
           let typed_ty = check_universe cx decl.ty in
           let ty = Core.Term_ty.eval Core.Value_env.empty (Typed.Ty.term typed_ty) in
           check_ty_transparent cx ty;
           typed_ty, (decl.name, ty)))
    in
    let cx' =
      List.fold tys ~init:cx ~f:(fun cx_acc (name, ty) -> Context.bind name ty cx_acc)
    in
    let typed_decls =
      List.map3_exn decls typed_tys tys ~f:(fun decl typed_ty (_, ty) ->
        let e = check cx' decl.e ty in
        ({ name = decl.name; ty = typed_ty; e } : Typed.expr_rec_decl))
    in
    let field_specs =
      List.map tys ~f:(fun (name, ty) ->
        { name
        ; ty = Core.Ty.quote ty
        ; relevancy = Relevancy.Relevant
        })
    in
    let ty = Ty_struct { env = Core.Value_env.empty; field_specs } in
    (* Typed.Expr_rec { decls = typed_decls; ann = expr_ann cx span placeholder_term ty } *) *)
    failwith ""
  | Expr_where { e; path; rhs; span } ->
    let e_typed = check_universe st cx e in
    let rhs_typed = infer st cx rhs in
    let original_ty = Core.Term_ty.eval Core.Value_env.empty (Typed.Ty.term e_typed) in
    let patched_ty =
      try
        apply_patch
          st
          cx
          (Non_empty_list.to_list path)
          (Term_free (Context.next_level cx))
          original_ty
          (Typed.Expr.term rhs_typed)
          (Typed.Expr.ty rhs_typed)
        |> snd
      with
      | Same_signature -> original_ty
    in
    Typed.Expr.of_ty
      (Ty_where
         { e = e_typed
         ; path
         ; rhs = rhs_typed
         ; ann = ty_ann cx span (Core.Ty.quote patched_ty) (Typed.Ty.props e_typed)
         })

and check (st : State.t) (cx : Context.t) (e : Abstract.expr) (ty : Core.ty) : Typed.expr =
  match e with
  | Expr_error { span } ->
    State.throw
      st
      [ Diagnostic.Part.create
          ~snippet:(State.snippet st span)
          (Doc.string "Cannot check error term")
      ]
  (* | Expr_struct { decls; span; is_dependent = false } ->
    Context.throw cx [ Diagnostic.Part.create (Doc.string "Cannot check structs yet") ] *)
  | Expr_fun { name; param_ty; param_modifiers; body; span } ->
    let fun_ty =
      with_elab_context
        st
        span
        "while checking the function against the expected type"
        ~f:(fun () -> extract_fun_ty st cx ty)
    in
    State.with_context
      st
      (Diagnostic.Part.create
         ~snippet:(State.snippet st span)
         (Doc.string "while checking binder"))
      ~f:(fun () -> Unify.unify_param_modifiers st fun_ty.param.modifiers param_modifiers);
    let param_ty =
      match param_ty with
      | None -> None
      | Some param_ty ->
        let param_ty_typed = check_universe st cx param_ty in
        with_elab_context
          st
          span
          "while checking the function parameter annotation"
          ~f:(fun () ->
            Unify.unify_ty
              st
              cx
              (Core.Term_ty.eval Core.Value_env.empty (Typed.Ty.term param_ty_typed))
              fun_ty.param.param);
        Some param_ty_typed
    in
    let body =
      check
        st
        (Context.bind name fun_ty.param.param cx)
        body
        (Core.Ty_fun.app
           fun_ty
           ({ e = Context.next_free cx; icit = fun_ty.param.modifiers.icit }
            : Core.value_arg))
    in
    let term : Core.term =
      Term_fun
        { name
        ; icit = fun_ty.param.modifiers.icit
        ; body = Core.Term.close_single (Context.next_level cx) (Typed.Expr.term body)
        }
    in
    Typed.Expr_fun
      { name; param_ty; param_modifiers; body; ann = expr_ann cx span term ty }
  | Expr_if { cond; body1; body2; span } ->
    let cond = check st cx cond (Ty_core Bool) in
    let body1 = check st cx body1 ty in
    let body2 = check st cx body2 ty in
    Typed.Expr_if { cond; body1; body2; ann = expr_ann cx span Term_ignore ty }
  | Expr_pack { e; span } ->
    let inner_ty =
      with_elab_context st span "while checking the pack expression" ~f:(fun () ->
        extract_pack_ty st cx ty)
    in
    let e = check st cx e inner_ty in
    Typed.Expr_pack { e; ann = expr_ann cx span Term_ignore ty }
  | Expr_bind { name; rhs; body; span } ->
    let result_term : Core.term =
      with_elab_context st span "while checking the bind expression" ~f:(fun () ->
        synthesize_transparent_ty st cx ty)
    in
    let rhs_typed = infer st cx rhs in
    let rhs_inner_ty =
      with_elab_context
        st
        (Typed.Expr.span rhs_typed)
        "while checking the right-hand side of the bind expression"
        ~f:(fun () -> extract_pack_ty st cx (Typed.Expr.ty rhs_typed))
    in
    let body_typed = check st (Context.bind name rhs_inner_ty cx) body ty in
    Typed.Expr_bind
      { name; rhs = rhs_typed; body = body_typed; ann = expr_ann cx span result_term ty }
  | _ ->
    let e_typed = infer st cx e in
    let term_opt, coe =
      with_elab_context
        st
        (Typed.Expr.span e_typed)
        "while checking the expression against the expected type"
        ~f:(fun () ->
          Unify.sub st cx (Typed.Expr.term e_typed) (Typed.Expr.ty e_typed) ty)
    in
    begin match term_opt with
    | None -> e_typed
    | Some term ->
      Typed.Expr_coe
        { expr = e_typed; coe; ann = expr_ann cx (Typed.Expr.span e_typed) term ty }
    end

and check_universe (st : State.t) (cx : Context.t) (ty : Abstract.expr) : Typed.ty =
  let typed_ty = infer st cx ty in
  let universe = Typed.Expr.ty typed_ty in
  let props =
    match Core.Ty.whnf cx.ty_env universe with
    | Ty_universe props -> props
    | ty ->
      State.throw
        st
        [ Diagnostic.Part.create
            ~snippet:(State.snippet st (Typed.Expr.span typed_ty))
            (Doc.string "Type was not a universe: " ^^ Context.pp_ty cx ty)
        ]
  in
  Ty_decode
    { expr = typed_ty
    ; ann =
        ty_ann
          cx
          (Typed.Expr.span typed_ty)
          (Term_ty_decode (Typed.Expr.term typed_ty))
          props
    }

and check_data_param (st : State.t) (cx : Context.t) ({ name; ty } : Abstract.data_param)
  : Typed.data_param
  =
  let ty = check_universe st cx ty in
  { name; ty }

and check_data_params (st : State.t) (cx : Context.t) (data : Abstract.data_param list)
  : params:Abstract.data_param list * telescope:Core.ty list
  =
  let params =
    List.fold
      data
      ~init:(~cx, ~close:Close.empty, ~params:Bwd.Empty, ~telescope:Bwd.Empty)
      ~f:(fun (~cx, ~close, ~params, ~telescope) param ->
        let param = check_data_param st cx param in
        let ty = Typed.Ty.term param.ty |> Core.Term_ty.close close in
        ( ~cx
        , ~close:(Close.push_exn (Context.next_level cx) close)
        , ~params:(Bwd.snoc params param)
        , ~telescope:(Bwd.snoc telescope ty) ))
  in
  (* let field_specs =
    List.map data.decls ~f:(fun { name; data = { params; _ }; _ } -> ())
  in *)
  failwith ""
;;

let infer source e =
  let st = State.create source in
  let cx = Context.empty in
  match infer st cx e with
  | typed -> Ok typed
  | exception State.Error diagnostic -> Error diagnostic
;;
