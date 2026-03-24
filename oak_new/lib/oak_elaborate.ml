open Prelude
open Oak_core_syntax

open struct
  module Bwd = Utility.Bwd
  module Span = Utility.Span
  module Spanned = Utility.Spanned
  module Common = Oak_common
  module Name_list = Common.Name_list
  module Diagnostic = Oak_diagnostic
  module Unify = Oak_unify
  module Pretty = Oak_pretty
  module Context = Oak_context
  module Evaluate = Oak_core_evaluate
  module Close = Evaluate.Close
  module Abstract = Oak_abstract
  module Typed = Oak_typed
end

let expr_ann (cx : Context.t) (span : Span.t) (term : term) (ty : ty) : Typed.expr_ann =
  { span; context = { ty_env = cx.ty_env; name_list = cx.name_list }; ty; term }
;;

let ty_ann (cx : Context.t) (span : Span.t) (term : term_ty) (ty_props : Ty_props.t)
  : Typed.ty_ann
  =
  { span; context = { ty_env = cx.ty_env; name_list = cx.name_list }; ty_props; term }
;;

let extract_fun_ty (cx : Context.t) (func_ty : ty) : ty_fun =
  match Context.whnf_ty cx func_ty with
  | Ty_fun func_ty -> func_ty
  | ty ->
    Context.throw
      cx
      [ Diagnostic.Part.create
          (Doc.string "Expected function type, got " ^^ Context.pp_ty cx ty)
      ]
;;

let extract_struct_ty (cx : Context.t) (strukt_ty : ty) : ty_struct =
  match Context.whnf_ty cx strukt_ty with
  | Ty_struct strukt_ty -> strukt_ty
  | ty ->
    Context.throw
      cx
      [ Diagnostic.Part.create
          (Doc.string "Expected struct type, got " ^^ Context.pp_ty cx ty)
      ]
;;

let extract_pack_ty (cx : Context.t) (packed_ty : ty) : ty =
  match Context.whnf_ty cx packed_ty with
  | Ty_pack ty -> ty
  | ty ->
    Context.throw
      cx
      [ Diagnostic.Part.create
          (Doc.string "Expected pack type, got " ^^ Context.pp_ty cx ty)
      ]
;;

let eval_expr (e : Typed.expr) : value = Evaluate.eval_value Seq.empty (Typed.Expr.term e)

let infer_literal_ty (cx : Context.t) (span : Span.t) (literal : Literal.t) : ty =
  match literal with
  | Literal.Unit -> Ty_core Unit
  | Literal.Bool _ -> Ty_core Bool
  | Literal.Int _ -> Ty_core Int
  | Literal.String _ ->
    Context.throw
      cx
      [ Diagnostic.Part.create
          ~snippet:(Context.snippet cx span)
          (Doc.string "String literals are not supported in elaboration yet")
      ]
;;

let maybe_sing_in (is_abstract : bool) (rhs : term) : term =
  if is_abstract then rhs else Term_sing_in rhs
;;

let rec synthesize_transparent_ty (cx : Context.t) (ty : ty) : term =
  match Context.whnf_ty cx ty with
  | Ty_universe _ ->
    Context.throw
      cx
      [ Diagnostic.Part.create
          (Doc.string "Universes are not transparent: " ^^ Context.pp_ty cx ty)
      ]
  | Ty_sing { identity; ty = _ } -> Term_sing_in (Evaluate.Value.quote identity)
  | Ty_struct { env; field_specs } ->
    let _, _, field_impls =
      List.fold
        field_specs
        ~init:(cx, env, Bwd.Empty)
        ~f:(fun (cx, closure_env, field_impls) { name; ty; relevancy } ->
          let ty = Evaluate.eval_ty closure_env ty in
          let e = synthesize_transparent_ty cx ty in
          let field_impl : term_field_impl = { name = name.name; e; relevancy } in
          ( Context.bind name ty cx
          , Seq.push (Context.next_free cx) closure_env
          , Bwd.snoc field_impls field_impl ))
    in
    let field_impls = Bwd.to_list field_impls in
    Term_struct { field_impls }
  | Ty_fun ({ name; param_ty; param_modifiers; _ } as ty) ->
    let body =
      synthesize_transparent_ty
        (Context.bind name param_ty cx)
        (Evaluate.Fun_ty.app ty { e = Context.next_free cx; param_modifiers })
    in
    Term_fun
      { name; param_modifiers; body = Evaluate.close_single (Context.next_level cx) body }
  | Ty_core _ | Ty_pack _ -> Term_ignore
  | Ty_decode e ->
    let ty_props = Evaluate.infer_neutral_universe cx.ty_env e in
    if Size.is_type ty_props.size
    then Term_ignore
    else
      Context.throw
        cx
        [ Diagnostic.Part.create
            (Doc.string "This type was not transparent since its universe was not Type: "
             ^^ Context.pp_value cx (Value_neutral e))
        ]
;;

exception Same_signature

let rec apply_patch
          (cx : Context.t)
          (path : string list)
          (term_to_coerce_to_original_ty : term)
          (original_ty : ty)
          (patch_with : term)
          (patch_with_ty : ty)
  : term * ty
  =
  match path with
  | [] -> failwith "expected nonempty list"
  | path_part :: path ->
    let original_ty = extract_struct_ty cx original_ty in
    let _, _, _, coerced_fields, patched_field_specs, did_find_field =
      List.foldi
        original_ty.field_specs
        ~init:(cx, original_ty.env, Close.empty, Bwd.Empty, Bwd.Empty, false)
        ~f:
          (fun
            index
            (cx, closure_env, close, coerced_fields, patched_field_specs, did_find_field)
            { name; ty = original_field_ty; relevancy }
          ->
          let original_field_ty = Evaluate.eval_ty closure_env original_field_ty in
          let (coerced_term, patched_field_ty), did_find_field =
            if (not did_find_field) && String.equal name.name path_part
            then
              ( begin match path with
                | [] -> begin
                  match Context.whnf_ty cx original_field_ty with
                  | Ty_sing original_ty ->
                    (* already a singleton, just check for equality *)
                    let patch_with_coerced =
                      Unify.coerce cx patch_with patch_with_ty original_ty.ty
                    in
                    Unify.unify_value
                      cx
                      (Evaluate.eval_value Seq.empty patch_with_coerced)
                      original_ty.identity
                      original_ty.ty;
                    raise_notrace Same_signature
                  | _ ->
                    let patch_with_coerced =
                      Unify.coerce cx patch_with patch_with_ty original_field_ty
                      |> Evaluate.eval_value Seq.empty
                    in
                    ( Term_sing_out (Term_free (Context.next_level cx))
                    , Ty_sing { identity = patch_with_coerced; ty = original_field_ty } )
                end
                | _ :: _ -> begin
                  match Context.whnf_ty cx original_field_ty with
                  | Ty_sing original_field_ty ->
                    let coerced_term, patched_field_ty =
                      apply_patch
                        cx
                        path
                        (Term_sing_out (Term_free (Context.next_level cx)))
                        original_field_ty.ty
                        patch_with
                        patch_with_ty
                    in
                    let coerced_term = Term_sing_in coerced_term in
                    let coerced_identity =
                      Unify.coerce
                        cx
                        (Evaluate.Value.quote original_field_ty.identity)
                        original_field_ty.ty
                        patched_field_ty
                    in
                    let patched_field_ty =
                      Ty_sing
                        { identity = Evaluate.eval_value Seq.empty coerced_identity
                        ; ty = patched_field_ty
                        }
                    in
                    coerced_term, patched_field_ty
                  | _ ->
                    let coerced_term, patched_field_ty =
                      apply_patch
                        cx
                        path
                        (Term_free (Context.next_level cx))
                        original_field_ty
                        patch_with
                        patch_with_ty
                    in
                    coerced_term, patched_field_ty
                end
                end
              , true )
            else (Term_free (Context.next_level cx), original_field_ty), did_find_field
          in
          let cx' = Context.bind name patched_field_ty cx in
          let coerced_field : term_field_impl =
            { name = name.name
            ; e =
                coerced_term
                |> Evaluate.close_single (Context.next_level cx)
                |> Evaluate.eval_value
                     (Seq.push
                        (Evaluate.eval_value
                           Seq.empty
                           (Term_proj
                              { strukt = term_to_coerce_to_original_ty
                              ; field = { name = name.name; index }
                              }))
                        Seq.empty)
                |> Evaluate.Value.quote
            ; relevancy
            }
          in
          let patched_field_spec : term_field_spec =
            { name
            ; ty = Evaluate.Ty.quote patched_field_ty |> Evaluate.close_ty close
            ; relevancy
            }
          in
          let closure_env' =
            Seq.push (Evaluate.eval_value Seq.empty coerced_term) closure_env
          in
          let close' =
            Close.add_exn (Context.next_level cx) Index.zero (Close.lift 1 close)
          in
          let coerced_fields' = Bwd.snoc coerced_fields coerced_field in
          let patched_field_specs' = Bwd.snoc patched_field_specs patched_field_spec in
          cx', closure_env', close', coerced_fields', patched_field_specs', did_find_field)
    in
    if not did_find_field
    then
      Context.throw
        cx
        [ Diagnostic.Part.create
            (Doc.string "Field "
             ^^ Doc.string path_part
             ^^ Doc.string " not found in struct")
        ];
    ( Term_struct { field_impls = Bwd.to_list coerced_fields }
    , Ty_struct { env = Seq.empty; field_specs = Bwd.to_list patched_field_specs } )
;;

let with_elab_context (cx : Context.t) (span : Span.t) (message : string) ~f =
  Context.with_context
    cx
    (Diagnostic.Part.create ~snippet:(Context.snippet cx span) (Doc.string message))
    ~f
;;

let rec coerce_singleton cx (e : term) (ty : ty) : term * ty =
  match Context.whnf_ty cx ty with
  | Ty_sing { identity = _; ty = kind } -> coerce_singleton cx (Term_sing_out e) kind
  | ty -> e, ty
;;

(* postcondition: the type in Typed.expr should be the type of the core term *)
let rec infer (cx : Context.t) (e : Abstract.expr) : Typed.expr =
  match e with
  | Expr_data_rec { decls; span } -> 
    failwith ""
  | Expr_data data -> failwith ""
  | Expr_var { index; span } ->
    let term = Term_free (Index.to_level (Context.size cx) index) in
    let ty = Seq.get_index_exn cx.ty_env index in
    let term, ty = coerce_singleton cx term ty in
    Typed.Expr_var { index; ann = expr_ann cx span term ty }
  | Expr_ann { e; ty; span = _ } ->
    let ty_typed = check_universe cx ty in
    let ty = Evaluate.eval_ty Seq.empty (Typed.Ty.term ty_typed) in
    let e_typed = check cx e ty in
    Typed.Expr_ann { e = e_typed; ty = ty_typed; ann = Typed.Expr.ann e_typed }
  | Expr_core_ty { ty; span } ->
    let typed_ty =
      Typed.Ty_core { ty; ann = ty_ann cx span (Term_ty_core ty) { size = Size.type_ } }
    in
    Typed.Expr.of_ty typed_ty
  | Expr_error { span } ->
    Context.throw
      cx
      [ Diagnostic.Part.create
          ~snippet:(Context.snippet cx span)
          (Doc.string "Cannot infer error term")
      ]
  | Expr_app { func; arg; param_modifiers; span } ->
    let func = infer cx func in
    let func_ty =
      with_elab_context cx span "while inferring the function application" ~f:(fun () ->
        extract_fun_ty cx (Typed.Expr.ty func))
    in
    if not (Icit.equal func_ty.param_modifiers.icit param_modifiers.icit)
    then
      Context.throw
        cx
        [ Diagnostic.Part.create
            ~snippet:(Context.snippet cx span)
            (Doc.string "Expected "
             ^^ Icit.pp func_ty.param_modifiers.icit
             ^^ Doc.string " argument, got "
             ^^ Icit.pp param_modifiers.icit
             ^^ Doc.string " argument")
        ];
    let arg = check cx arg func_ty.param_ty in
    let term_arg : term_arg =
      { e = Typed.Expr.term arg; param_modifiers = func_ty.param_modifiers }
    in
    let term = Term_app { func = Typed.Expr.term func; arg = term_arg } in
    let value_arg : value_arg =
      { e = eval_expr arg; param_modifiers = func_ty.param_modifiers }
    in
    let ty = Evaluate.Fun_ty.app func_ty value_arg in
    Typed.Expr_app { func; arg; param_modifiers; ann = expr_ann cx span term ty }
  | Expr_fun { name; param_ty = Some param_ty; param_modifiers; body; span } ->
    let param_ty_typed = check_universe cx param_ty in
    let param_ty = Evaluate.eval_ty Seq.empty (Typed.Ty.term param_ty_typed) in
    let cx' = Context.bind name param_ty cx in
    let body = infer cx' body in
    let body_ty : ty_closure =
      { env = Seq.empty
      ; body =
          Evaluate.Ty.quote (Typed.Expr.ty body)
          |> Evaluate.close_ty_single (Context.next_level cx)
      }
    in
    let term =
      Term_fun
        { name
        ; param_modifiers
        ; body = Evaluate.close_single (Context.next_level cx) (Typed.Expr.term body)
        }
    in
    let ty = Ty_fun { name; param_modifiers; param_ty; body_ty } in
    Typed.Expr_fun
      { name
      ; param_ty = Some param_ty_typed
      ; param_modifiers
      ; body
      ; ann = expr_ann cx span term ty
      }
  | Expr_fun { span; _ } ->
    Context.throw
      cx
      [ Diagnostic.Part.create
          ~snippet:(Context.snippet cx span)
          (Doc.string "Cannot infer lambda without parameter type annotation")
      ]
  | Expr_ty_fun { name; param_ty; param_modifiers; body_ty; span } ->
    let param_ty_typed = check_universe cx param_ty in
    let param_ty = Evaluate.eval_ty Seq.empty (Typed.Ty.term param_ty_typed) in
    let body_ty_typed = check_universe (Context.bind name param_ty cx) body_ty in
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
                 { name
                 ; param_ty = Typed.Ty.term param_ty_typed
                 ; param_modifiers
                 ; body_ty =
                     Typed.Ty.term body_ty_typed
                     |> Evaluate.close_ty_single (Context.next_level cx)
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
    let strukt = infer cx strukt in
    let struct_ty =
      with_elab_context cx span "while projecting a struct field" ~f:(fun () ->
        extract_struct_ty cx (Typed.Expr.ty strukt))
    in
    let field_loc =
      match
        List.find_mapi struct_ty.field_specs ~f:(fun index field_spec ->
          if String.equal field_spec.name.name field
          then Some ({ name = field; index } : field_loc)
          else None)
      with
      | Some field_loc -> field_loc
      | None ->
        Context.throw
          cx
          [ Diagnostic.Part.create
              ~snippet:(Context.snippet cx span)
              (Doc.string "Struct does not have field " ^^ Doc.string field)
          ]
    in
    let term = Term_proj { strukt = Typed.Expr.term strukt; field = field_loc } in
    let ty =
      Evaluate.Ty.proj cx.ty_env (eval_expr strukt) (Typed.Expr.ty strukt) field_loc
    in
    let term, ty = coerce_singleton cx term ty in
    Typed.Expr_proj { strukt; field; ann = expr_ann cx span term ty }
  | Expr_struct { decls; span; is_dependent = true } ->
    let decl_count = List.length decls in
    let _, _, typed_decls, let_bindings, field_specs =
      List.fold
        decls
        ~init:(cx, Close.empty, Bwd.Empty, Bwd.Empty, Bwd.Empty)
        ~f:(fun (cx_acc, close, typed_decls, let_bindings, field_specs) decl ->
          let e = infer cx_acc decl.e in
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
            maybe_sing_in decl.is_abstract (Typed.Expr.term e) |> Evaluate.close close
          in
          let field_spec : term_field_spec =
            { name = decl.name
            ; ty = Evaluate.Ty.quote ty |> Evaluate.close_ty close
            ; relevancy = decl.relevancy
            }
          in
          let level = Context.next_level cx_acc in
          ( Context.bind decl.name ty cx_acc
          , Close.add_exn level Index.zero (Close.lift 1 close)
          , Bwd.snoc typed_decls typed_decl
          , Bwd.snoc let_bindings (decl.name, rhs)
          , Bwd.snoc field_specs field_spec ))
    in
    let typed_decls = Bwd.to_list typed_decls in
    let let_bindings = Bwd.to_list let_bindings in
    let field_specs = Bwd.to_list field_specs in
    let term =
      List.fold_right
        let_bindings
        ~init:
          (Term_struct
             { field_impls =
                 List.mapi decls ~f:(fun i { name; relevancy; _ } ->
                   ({ name = name.name
                    ; e = Term_bound (Index.of_int (decl_count - i - 1))
                    ; relevancy
                    }
                    : term_field_impl))
             })
        ~f:(fun (name, rhs) body -> Term_let { name; rhs; body })
    in
    let ty = Ty_struct { env = Seq.empty; field_specs } in
    Typed.Expr_struct
      { decls = typed_decls; ann = expr_ann cx span term ty; is_dependent = true }
  | Expr_struct { decls; span; is_dependent = false } ->
    let _, typed_decls, field_impls, field_specs =
      List.foldi
        decls
        ~init:(Close.empty, Bwd.Empty, Bwd.Empty, Bwd.Empty)
        ~f:(fun index (close, typed_decls, field_impls, field_specs) decl ->
          let e = infer cx decl.e in
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
          let field_spec : term_field_spec =
            { name = decl.name
            ; ty = Evaluate.Ty.quote ty |> Evaluate.close_ty close
            ; relevancy = decl.relevancy
            }
          in
          let field_impl : term_field_impl =
            { name = decl.name.name; e = rhs; relevancy = decl.relevancy }
          in
          ( Close.add_exn
              (Level.of_int (Context.size cx + index))
              Index.zero
              (Close.lift 1 close)
          , Bwd.snoc typed_decls typed_decl
          , Bwd.snoc field_impls field_impl
          , Bwd.snoc field_specs field_spec ))
    in
    let typed_decls = Bwd.to_list typed_decls in
    let field_impls = Bwd.to_list field_impls in
    let field_specs = Bwd.to_list field_specs in
    let term = Term_struct { field_impls } in
    let ty = Ty_struct { env = Seq.empty; field_specs } in
    Typed.Expr_struct
      { decls = typed_decls; ann = expr_ann cx span term ty; is_dependent = false }
  | Expr_ty_struct { field_specs; span } ->
    let _, _, typed_field_specs, field_specs, size =
      List.fold
        field_specs
        ~init:(cx, Close.empty, Bwd.Empty, Bwd.Empty, Size.sig_)
        ~f:(fun (cx_acc, close, typed_field_specs, field_specs, size) field_spec ->
          let typed_field_ty, typed_rhs, ty =
            match field_spec.ty, field_spec.rhs with
            | Some ty, rhs ->
              let typed_ty = check_universe cx_acc ty in
              let field_ty = Evaluate.eval_ty Seq.empty (Typed.Ty.term typed_ty) in
              let typed_rhs, ty =
                match rhs with
                | None -> None, field_ty
                | Some rhs ->
                  let rhs = check cx_acc rhs field_ty in
                  let rhs_value = eval_expr rhs in
                  Some rhs, Ty_sing { identity = rhs_value; ty = field_ty }
              in
              Some typed_ty, typed_rhs, ty
            | None, Some rhs ->
              let rhs = infer cx_acc rhs in
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
          let field_spec : term_field_spec =
            { name = field_spec.name
            ; ty = Evaluate.Ty.quote ty |> Evaluate.close_ty close
            ; relevancy = field_spec.relevancy
            }
          in
          let level = Context.next_level cx_acc in
          ( Context.bind field_spec.name ty cx_acc
          , Close.add_exn level Index.zero (Close.lift 1 close)
          , Bwd.snoc typed_field_specs typed_field_spec
          , Bwd.snoc field_specs field_spec
          , Size.max
              size
              (match typed_field_ty with
               | Some typed_ty -> (Typed.Ty.props typed_ty).size
               | None -> (Evaluate.infer_props cx_acc.ty_env ty).size) ))
    in
    let typed_field_specs = Bwd.to_list typed_field_specs in
    let field_specs = Bwd.to_list field_specs in
    let typed_ty =
      Typed.Ty_struct
        { field_specs = typed_field_specs
        ; ann = ty_ann cx span (Term_ty_struct { field_specs }) { size }
        }
    in
    Typed.Expr.of_ty typed_ty
  | Expr_let { name; rhs; relevancy; is_abstract; body; span } ->
    let rhs = infer cx rhs in
    let rhs_value = eval_expr rhs in
    let rhs_ty =
      if is_abstract
      then Typed.Expr.ty rhs
      else Ty_sing { identity = rhs_value; ty = Typed.Expr.ty rhs }
    in
    let cx' = Context.bind name rhs_ty cx in
    let body = infer cx' body in
    let term =
      Term_let
        { name
        ; rhs = maybe_sing_in is_abstract (Typed.Expr.term rhs)
        ; body = Evaluate.close_single (Context.next_level cx) (Typed.Expr.term body)
        }
    in
    let ty =
      Evaluate.eval_ty
        (Seq.push (if is_abstract then rhs_value else Value_sing_in rhs_value) Seq.empty)
        (Evaluate.Ty.quote (Typed.Expr.ty body)
         |> Evaluate.close_ty_single (Context.next_level cx))
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
    let cond = check cx cond (Ty_core Bool) in
    let body1 = infer cx body1 in
    let body2 = infer cx body2 in
    let body1_props = Evaluate.infer_props cx.ty_env (Typed.Expr.ty body1) in
    let body2_props = Evaluate.infer_props cx.ty_env (Typed.Expr.ty body2) in
    if not (Size.is_type body1_props.size)
    then
      Context.throw
        cx
        [ Diagnostic.Part.create
            ~snippet:(Context.snippet cx (Typed.Expr.span body1))
            (Doc.string "The first branch did not have a type in universe Type")
        ];
    if not (Size.is_type body2_props.size)
    then
      Context.throw
        cx
        [ Diagnostic.Part.create
            ~snippet:(Context.snippet cx (Typed.Expr.span body2))
            (Doc.string "The second branch did not have a type in universe Type")
        ];
    with_elab_context cx span "in the if expression" ~f:(fun () ->
      Unify.unify_ty cx (Typed.Expr.ty body1) (Typed.Expr.ty body2));
    Typed.Expr_if
      { cond; body1; body2; ann = expr_ann cx span Term_ignore (Typed.Expr.ty body1) }
  | Expr_ty_pack { ty; span } ->
    let typed_ty = check_universe cx ty in
    let typed_ty =
      Typed.Ty_pack
        { ty = typed_ty
        ; ann =
            ty_ann cx span (Term_ty_pack (Typed.Ty.term typed_ty)) { size = Size.type_ }
        }
    in
    Typed.Expr.of_ty typed_ty
  | Expr_pack { e; span } ->
    let e = infer cx e in
    let ty = Ty_pack (Typed.Expr.ty e) in
    Typed.Expr_pack { e; ann = expr_ann cx span Term_ignore ty }
  | Expr_bind { span; _ } ->
    Context.throw
      cx
      [ Diagnostic.Part.create
          ~snippet:(Context.snippet cx span)
          (Doc.string "Cannot infer bind expressions")
      ]
  | Expr_literal { literal; span } ->
    let ty = infer_literal_ty cx span literal in
    Typed.Expr_literal { literal; ann = expr_ann cx span Term_ignore ty }
  | Expr_rec { decls; span } ->
    (* let typed_tys, tys =
      List.unzip
        (List.map decls ~f:(fun decl ->
           let typed_ty = check_universe cx decl.ty in
           let ty = Evaluate.eval_ty Seq.empty (Typed.Ty.term typed_ty) in
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
        ; ty = Evaluate.quote_ty (Context.size cx) ty
        ; relevancy = Relevancy.Relevant
        })
    in
    let ty = Ty_struct { env = Seq.empty; field_specs } in
    (* Typed.Expr_rec { decls = typed_decls; ann = expr_ann cx span placeholder_term ty } *) *)
    failwith ""
  | Expr_where { e; path; rhs; span } ->
    let e_typed = check_universe cx e in
    let rhs_typed = infer cx rhs in
    let original_ty = Evaluate.eval_ty Seq.empty (Typed.Ty.term e_typed) in
    let patched_ty =
      try
        apply_patch
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
         ; ann = ty_ann cx span (Evaluate.Ty.quote patched_ty) (Typed.Ty.props e_typed)
         })

and check (cx : Context.t) (e : Abstract.expr) (ty : ty) : Typed.expr =
  match e with
  | Expr_error { span } ->
    Context.throw
      cx
      [ Diagnostic.Part.create
          ~snippet:(Context.snippet cx span)
          (Doc.string "Cannot check error term")
      ]
  (* | Expr_struct { decls; span; is_dependent = false } ->
    Context.throw cx [ Diagnostic.Part.create (Doc.string "Cannot check structs yet") ] *)
  | Expr_fun { name; param_ty; param_modifiers; body; span } ->
    let fun_ty =
      with_elab_context
        cx
        span
        "while checking the function against the expected type"
        ~f:(fun () -> extract_fun_ty cx ty)
    in
    Context.with_context
      cx
      (Diagnostic.Part.create
         ~snippet:(Context.snippet cx span)
         (Doc.string "while checking binder"))
      ~f:(fun () -> Unify.unify_param_modifiers cx fun_ty.param_modifiers param_modifiers);
    let param_ty =
      match param_ty with
      | None -> None
      | Some param_ty ->
        let param_ty_typed = check_universe cx param_ty in
        with_elab_context
          cx
          span
          "while checking the function parameter annotation"
          ~f:(fun () ->
            Unify.unify_ty
              cx
              (Evaluate.eval_ty Seq.empty (Typed.Ty.term param_ty_typed))
              fun_ty.param_ty);
        Some param_ty_typed
    in
    let body =
      check
        (Context.bind name fun_ty.param_ty cx)
        body
        (Evaluate.Fun_ty.app
           fun_ty
           { e = Context.next_free cx; param_modifiers = fun_ty.param_modifiers })
    in
    let term =
      Term_fun
        { name
        ; param_modifiers = fun_ty.param_modifiers
        ; body = Evaluate.close_single (Context.next_level cx) (Typed.Expr.term body)
        }
    in
    Typed.Expr_fun
      { name; param_ty; param_modifiers; body; ann = expr_ann cx span term ty }
  | Expr_if { cond; body1; body2; span } ->
    let cond = check cx cond (Ty_core Bool) in
    let body1 = check cx body1 ty in
    let body2 = check cx body2 ty in
    Typed.Expr_if { cond; body1; body2; ann = expr_ann cx span Term_ignore ty }
  | Expr_pack { e; span } ->
    let inner_ty =
      with_elab_context cx span "while checking the pack expression" ~f:(fun () ->
        extract_pack_ty cx ty)
    in
    let e = check cx e inner_ty in
    Typed.Expr_pack { e; ann = expr_ann cx span Term_ignore ty }
  | Expr_bind { name; rhs; body; span } ->
    let result_term =
      with_elab_context cx span "while checking the bind expression" ~f:(fun () ->
        synthesize_transparent_ty cx ty)
    in
    let rhs_typed = infer cx rhs in
    let rhs_inner_ty =
      with_elab_context
        cx
        (Typed.Expr.span rhs_typed)
        "while checking the right-hand side of the bind expression"
        ~f:(fun () -> extract_pack_ty cx (Typed.Expr.ty rhs_typed))
    in
    let body_typed = check (Context.bind name rhs_inner_ty cx) body ty in
    Typed.Expr_bind
      { name; rhs = rhs_typed; body = body_typed; ann = expr_ann cx span result_term ty }
  | _ ->
    let e_typed = infer cx e in
    let term_opt, coe =
      with_elab_context
        cx
        (Typed.Expr.span e_typed)
        "while checking the expression against the expected type"
        ~f:(fun () -> Unify.sub cx (Typed.Expr.term e_typed) (Typed.Expr.ty e_typed) ty)
    in
    begin match term_opt with
    | None -> e_typed
    | Some term ->
      Typed.Expr_coe
        { expr = e_typed; coe; ann = expr_ann cx (Typed.Expr.span e_typed) term ty }
    end

and check_universe (cx : Context.t) (ty : Abstract.expr) : Typed.ty =
  let typed_ty = infer cx ty in
  let universe = Typed.Expr.ty typed_ty in
  let props =
    match Context.whnf_ty cx universe with
    | Ty_universe props -> props
    | ty ->
      Context.throw
        cx
        [ Diagnostic.Part.create
            ~snippet:(Context.snippet cx (Typed.Expr.span typed_ty))
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
;;

let infer source e =
  let cx = Context.create source in
  match infer cx e with
  | typed -> Ok typed
  | exception Context.Error diagnostic -> Error diagnostic
;;
