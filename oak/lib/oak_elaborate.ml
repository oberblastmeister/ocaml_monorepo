open Prelude
open Oak_syntax

open struct
  module Span = Utility.Span
  module Spanned = Utility.Spanned
  module Common = Oak_common
  module Name_list = Common.Name_list
  module Diagnostic = Oak_diagnostic
  module Unify = Oak_unify
  module Pretty = Oak_pretty
  module Context = Oak_context
  module Universe = Common.Universe
  module Infer_simple = Oak_infer_simple
  module Evaluate = Oak_evaluate
  module Close = Evaluate.Close
end

exception Error of Diagnostic.t

let raise_error diagnostic = raise_notrace (Error diagnostic)

module Meta_list = struct
  type t = { mutable metas : meta list }

  let create () = { metas = [] }

  let fresh t (cx : Context.t) var created_at =
    let id = !(cx.next_meta_id) in
    incr cx.next_meta_id;
    let meta =
      { var; created_at; id; context_size = Context.size cx; state = Meta_unsolved }
    in
    t.metas <- meta :: t.metas;
    Term_ty_meta meta
  ;;

  let check_all_solved t (cx : Context.t) =
    List.iter t.metas ~f:(fun meta ->
      match meta.state with
      | Meta_solved _ -> failwith "Meta cannot be solved yet"
      | Meta_link ty ->
        (* Now this allows free variables to be closed *)
        meta.state <- Meta_solved ty
      | Meta_unsolved ->
        raise_error
          { code = None
          ; parts =
              [ Diagnostic.Part.create
                  ~snippet:(Context.snippet cx meta.created_at)
                  (Doc.string "Unsolved meta variable: " ^^ Meta.pp meta)
              ]
          })
  ;;
end

exception Type_not_ignorable of Diagnostic.Part.t

let raise_type_not_ignorable e = raise_notrace (Type_not_ignorable e)

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
  
  Even though we could ignore singletons, we don't because it makes the implementation a lot harder.
  Since we don't track the result in Value_sing_out, we have to reconstruct it through the type.
  This means that if we were to allow ignoring singletons, we need to add the type to Value_ignore.
*)
let rec check_ty_ignorable (cx : Context.t) (ty : ty) : unit =
  match Context.unfold cx ty with
  | Value_ignore | Value_mod _ | Value_abs _ | Value_sing_in _ ->
    raise_s [%message "Not a type" (ty : value)]
  | Value_ty_pack _ | Value_core_ty _ -> ()
  | Value_neutral neutral ->
    let kind =
      Infer_simple.infer_neutral cx.ty_env (Whnf_neutral.to_neutral neutral)
      |> Context.unfold cx
      |> Whnf.universe_val_exn
    in
    if not (Universe.equal kind Universe.type_)
    then
      raise_type_not_ignorable
        (Diagnostic.Part.create
           (Doc.string "Type was not ignorable: " ^^ Pretty.pp_value Name_list.empty ty))
  | Value_ty_fun ty ->
    check_ty_ignorable
      (Context.bind ty.var ty.param_ty cx)
      (Evaluate.Fun_ty.app ty (Context.next_free cx))
  | Value_ty_mod ty ->
    let _ =
      List.fold ty.ty_decls ~init:(cx, ty.env) ~f:(fun (cx, closure_env) ty_decl ->
        let ty = Evaluate.eval closure_env ty_decl.ty in
        check_ty_ignorable cx ty;
        Context.bind ty_decl.var ty cx, Env.push (Context.next_free cx) closure_env)
    in
    ()
  | _ ->
    raise_type_not_ignorable
      (Diagnostic.Part.create
         (Doc.string "Type was not ignorable: " ^^ Pretty.pp_value Name_list.empty ty))
;;

let check_type_ignorable cx ty =
  match check_ty_ignorable cx ty with
  | () -> Ok ()
  | exception Type_not_ignorable part -> Error part
;;

(* TODO: these should not unwrap types with exn *)
let rec infer (cx : Context.t) (e : expr) : term * ty =
  match e with
  | Expr_error { span } ->
    raise_error
      { code = None
      ; parts =
          [ Diagnostic.Part.create
              ~snippet:(Context.snippet cx span)
              (Doc.string "Cannot infer error term")
          ]
      }
  | Expr_var { var; span = _ } ->
    Term_free (Index.to_level (Context.size cx) var), Env.find_index_exn cx.ty_env var
  | Expr_ann { e; ty; span = _ } ->
    let ty, _ = check_universe cx ty in
    let ty = Evaluate.eval Env.empty ty in
    check cx e ty, ty
  | Expr_app _ | Expr_proj _ ->
    let meta_list = Meta_list.create () in
    let e, ty = infer_spine cx meta_list e in
    Meta_list.check_all_solved meta_list cx;
    e, ty
  | Expr_abs { var; param_ty = Some param_ty; icit; body; span } ->
    let param_ty, _ = check_universe cx param_ty in
    let param_ty = Evaluate.eval Env.empty param_ty in
    if Icit.equal icit Impl
    then begin
      match Unify.unify_ty cx param_ty (Value_universe Universe.type_) with
      | Ok () -> ()
      | Error part ->
        raise_error
          { code = None
          ; parts =
              [ part
              ; Diagnostic.Part.create
                  ~kind:Note
                  ~snippet:(Context.snippet cx span)
                  (Doc.string "Implicit parameter type annotation must be a type")
              ]
          }
    end;
    let cx' = Context.bind var param_ty cx in
    let body, body_ty = infer cx' body in
    let body_ty : value_closure =
      { env = Env.empty
      ; body = Context.quote cx' body_ty |> Evaluate.close_single (Context.next_level cx)
      }
    in
    ( Term_abs { var; icit; body = Evaluate.close_single (Context.next_level cx) body }
    , Value_ty_fun { var; param_ty; icit; body_ty } )
  | Expr_abs { var = _; param_ty = None; icit = _; body = _; span } ->
    raise_error
      { code = None
      ; parts =
          [ Diagnostic.Part.create
              ~snippet:(Context.snippet cx span)
              (Doc.string "Cannot infer lambda without parameter type annotation")
          ]
      }
  | Expr_mod { decls; span = _ } ->
    (*
      Elaborate a module into a bunch of lets and then a module expression at the end.
    *)
    let decls_length = List.length decls in
    let tuple =
      List.mapi decls ~f:(fun i decl ->
        ({ name = decl.var.name; e = Term_bound (Index.of_int (decls_length - i - 1)) }
         : term_field))
    in
    let mod_term = Term_mod { fields = tuple } in
    let _, stuff =
      List.fold_map decls ~init:(cx, Close.empty) ~f:(fun (cx, close) decl ->
        let e, ty = infer cx decl.e in
        let let_tuple = decl.var, Evaluate.close close e in
        let ty_decl : term_ty_decl =
          { var = decl.var; ty = Context.quote cx ty |> Evaluate.close close }
        in
        ( ( Context.bind decl.var ty cx
          , Close.add_exn (Context.next_level cx) Index.zero (Close.lift 1 close) )
        , (let_tuple, ty_decl) ))
    in
    let lets, ty_decls = List.unzip stuff in
    let res_ty = Value_ty_mod { env = Env.empty; ty_decls } in
    let res_term =
      List.fold_right lets ~init:mod_term ~f:(fun (var, rhs) body ->
        Term_let { var; rhs; body })
    in
    res_term, res_ty
  | Expr_ty_fun { var; param_ty; icit; body_ty; span = _ } ->
    let param_ty, universe1 = check_universe cx param_ty in
    let body_ty, universe2 =
      check_universe (Context.bind var (Evaluate.eval Env.empty param_ty) cx) body_ty
    in
    ( Term_ty_fun
        { var
        ; param_ty
        ; icit
        ; body_ty = Evaluate.close_single (Context.next_level cx) body_ty
        }
    , Value_universe (Universe.max universe1 universe2) )
  | Expr_ty_mod { ty_decls; span = _ } ->
    let (_, _, universe), ty_decls =
      List.fold_map
        ty_decls
        ~init:(cx, Close.empty, Universe.kind_)
        ~f:(fun (cx, close, universe) { var; ty; span = _ } ->
          let ty, universe' = check_universe cx ty in
          ( ( Context.bind var (Evaluate.eval Env.empty ty) cx
            , Close.add_exn (Context.next_level cx) Index.zero (Close.lift 1 close)
            , Universe.max universe universe' )
          , ({ var; ty = Evaluate.close close ty } : term_ty_decl) ))
    in
    Term_ty_mod { ty_decls }, Value_universe universe
  | Expr_let { var; rhs; body; span = _ } ->
    let rhs, rhs_ty = infer cx rhs in
    let cx' = Context.bind var rhs_ty cx in
    let body, body_ty = infer cx' body in
    ( Term_let { var; rhs; body = Evaluate.close_single (Context.next_level cx) body }
    , Evaluate.eval
        (Env.push (Evaluate.eval Env.empty rhs) Env.empty)
        (Evaluate.close_single (Context.next_level cx) (Context.quote cx' body_ty)) )
  | Expr_ty_sing { identity; span = _ } ->
    let identity, identity_ty = infer cx identity in
    ( Term_ty_sing { identity; ty = Context.quote cx identity_ty }
    , Value_universe Universe.kind_ )
  | Expr_alias { identity; span = _ } ->
    let identity, identity_ty = infer cx identity in
    ( Term_sing_in identity
    , Value_ty_sing { identity = Evaluate.eval Env.empty identity; ty = identity_ty } )
  | Expr_literal { literal; span = _ } ->
    let ty = Infer_simple.infer_literal literal in
    Term_literal literal, Value_core_ty ty
  | Expr_core_ty { ty; span = _ } -> Term_core_ty ty, Value_universe Universe.type_
  | Expr_universe { universe; span = _ } ->
    Term_universe universe, Value_universe (Universe.incr universe)
  | Expr_if { cond; body1; body2; span = _ } ->
    let body1_span = Expr.span body1 in
    let body2_span = Expr.span body2 in
    let cond = check cx cond (Value_core_ty Bool) in
    let body1, body1_ty = infer cx body1 in
    let body2, body2_ty = infer cx body2 in
    let universe1 = Infer_simple.infer_value_universe cx.ty_env body1_ty in
    let universe2 = Infer_simple.infer_value_universe cx.ty_env body2_ty in
    if not (Universe.equal universe1 Universe.type_)
    then
      raise_error
        { code = None
        ; parts =
            [ Diagnostic.Part.create
                ~snippet:(Context.snippet cx body1_span)
                (Doc.string "The first branch did not have a type in universe Type")
            ]
        };
    if not (Universe.equal universe2 Universe.type_)
    then
      raise_error
        { code = None
        ; parts =
            [ Diagnostic.Part.create
                ~snippet:(Context.snippet cx body2_span)
                (Doc.string "The second branch did not have a type in universe Type")
            ]
        };
    (match Unify.unify_ty cx body1_ty body2_ty with
     | Ok () -> ()
     | Error part ->
       raise_error
         { code = None
         ; parts =
             [ part
             ; Diagnostic.Part.create
                 ~kind:Note
                 ~snippet:(Context.snippet cx body1_span)
                 (Doc.string "first branch has type " ^^ Context.pp_value cx body1_ty)
             ; Diagnostic.Part.create
                 ~kind:Note
                 ~snippet:(Context.snippet cx body2_span)
                 (Doc.string "second branch has type " ^^ Context.pp_value cx body2_ty)
             ]
         });
    Term_if { cond; body1; body2 }, body1_ty
  | Expr_ty_pack { ty; span = _ } ->
    let ty, _ = check_universe cx ty in
    (* Packs always have kind Type, no matter the universe of the inner type *)
    Term_ty_pack ty, Value_universe Universe.type_
  | Expr_pack { e; span = _ } ->
    let e, ty = infer cx e in
    Term_pack e, Value_ty_pack ty
  | Expr_bind { span; _ } ->
    raise_error
      { code = None
      ; parts =
          [ Diagnostic.Part.create
              ~snippet:(Context.snippet cx span)
              (Doc.string "Cannot infer bind expressions")
          ]
      }

and check (cx : Context.t) (e : expr) (ty : ty) : term =
  match e, Context.unfold cx ty with
  | Expr_abs { var; param_ty; icit; body; span }, Value_ty_fun ty ->
    (match param_ty with
     | Some param_ty ->
       let param_ty, _ = check_universe cx param_ty in
       (match Unify.unify_ty cx (Evaluate.eval Env.empty param_ty) ty.param_ty with
        | Ok () -> ()
        | Error part ->
          raise_error
            { code = None
            ; parts =
                [ part
                ; Diagnostic.Part.create
                    ~kind:Note
                    ~snippet:(Context.snippet cx span)
                    (Doc.string "parameter type annotation did not match expected type")
                ]
            })
     | None -> ());
    if not (Icit.equal icit ty.icit)
    then
      raise_error
        { code = None
        ; parts =
            [ Diagnostic.Part.create
                ~kind:Note
                ~snippet:(Context.snippet cx span)
                (Doc.string "Expected "
                 ^^ Doc.string (Icit.to_string ty.icit)
                 ^^ Doc.string " binder")
            ]
        };
    let body =
      check
        (Context.bind var ty.param_ty cx)
        body
        (Evaluate.Fun_ty.app ty (Context.next_free cx))
    in
    Term_abs { var; icit; body = Evaluate.close_single (Context.next_level cx) body }
  | Expr_let { var; rhs; body; span = _ }, _ ->
    let rhs, rhs_ty = infer cx rhs in
    let body = check (Context.bind var rhs_ty cx) body ty in
    Term_let { var; rhs; body = Evaluate.close_single (Context.next_level cx) body }
  | Expr_if { cond; body1; body2; span = _ }, _ ->
    let cond = check cx cond (Value_core_ty Bool) in
    let body1 = check cx body1 ty in
    let body2 = check cx body2 ty in
    Term_if { cond; body1; body2 }
  | Expr_pack { e; span = _ }, Value_ty_pack ty ->
    let e = check cx e ty in
    Term_pack e
  | Expr_bind { var; rhs; body; span }, _ ->
    (match check_type_ignorable cx ty with
     | Ok () -> ()
     | Error part ->
       raise_error
         { code = None
         ; parts =
             [ part
             ; Diagnostic.Part.create
                 ~kind:Note
                 ~snippet:(Context.snippet cx span)
                 (Doc.string "in bind expression")
             ]
         });
    let rhs, rhs_ty = infer cx rhs in
    let rhs_inner_ty =
      match Context.unfold cx rhs_ty with
      | Value_ty_pack ty -> ty
      | _ ->
        raise_error
          { code = None
          ; parts =
              [ Diagnostic.Part.create
                  ~snippet:(Context.snippet cx span)
                  (Doc.string "Expected a pack type")
              ]
          }
    in
    let body = check (Context.bind var rhs_inner_ty cx) body ty in
    Term_bind { var; rhs; body = Evaluate.close_single (Context.next_level cx) body }
  | _ ->
    let span = Expr.span e in
    let e, ty' = infer cx e in
    (match Unify.coerce cx e ty' ty with
     | Ok term -> term
     | Error part ->
       raise_error
         { code = None
         ; parts =
             [ part
             ; Diagnostic.Part.create
                 ~kind:Note
                 ~snippet:(Context.snippet cx span)
                 (Doc.string "failed to coerce inferred type"
                  ^^ Doc.indent 2 (Doc.break1 ^^ Context.pp_value cx ty')
                  ^^ Doc.break1
                  ^^ Doc.string "when checking against type"
                  ^^ Doc.indent 2 (Doc.break1 ^^ Context.pp_value cx ty))
             ]
         })

and insert_implicit_args
      (cx : Context.t)
      (meta_list : Meta_list.t)
      (span : Span.t)
      (e : term)
      (ty : ty)
  : term * whnf
  =
  let e, ty = Unify.coerce_singleton cx e ty in
  match ty with
  | Value_ty_fun ({ var; icit = Impl; _ } as ty) ->
    let meta = Meta_list.fresh meta_list cx var span in
    insert_implicit_args
      cx
      meta_list
      span
      (Term_app { func = e; icit = Impl; arg = meta })
      (Evaluate.Fun_ty.app ty (Evaluate.eval Env.empty meta))
  | _ -> e, ty

and extract_fun_ty cx span func (func_ty : ty) =
  let func, func_ty = Unify.coerce_singleton cx func func_ty in
  match func_ty with
  | Value_ty_fun t -> func, t
  | other ->
    raise_error
      { code = None
      ; parts =
          [ Diagnostic.Part.create
              ~snippet:(Context.snippet cx span)
              (Doc.string "Expected function type, got "
               ^^ Context.pp_value cx (Whnf.to_value other))
          ]
      }

and extract_mod_ty cx span mod_e mod_ty =
  let mod_e, mod_ty = Unify.coerce_singleton cx mod_e mod_ty in
  let mod_ty =
    match mod_ty with
    | Value_ty_mod t -> t
    | _ ->
      raise_error
        { code = None
        ; parts =
            [ Diagnostic.Part.create
                ~snippet:(Context.snippet cx span)
                (Doc.string "Expected mod type, got "
                 ^^ Context.pp_value cx (Whnf.to_value mod_ty))
            ]
        }
  in
  mod_e, mod_ty

and infer_spine (cx : Context.t) (meta_list : Meta_list.t) (e : expr) : term * ty =
  match e with
  | Expr_app { func; arg; icit; span } ->
    let func, func_ty = infer_spine cx meta_list func in
    let func, func_ty =
      begin match icit with
      | Expl ->
        let func, func_ty = insert_implicit_args cx meta_list span func func_ty in
        extract_fun_ty cx span func (Whnf.to_value func_ty)
      | Impl -> extract_fun_ty cx span func func_ty
      end
    in
    if not (Icit.equal func_ty.icit icit)
    then begin
      raise_error
        { code = None
        ; parts =
            [ Diagnostic.Part.create
                ~snippet:(Context.snippet cx span)
                (Doc.string "Expected "
                 ^^ Icit.pp func_ty.icit
                 ^^ Doc.string " argument, got "
                 ^^ Icit.pp icit
                 ^^ Doc.string " argument")
            ]
        }
    end;
    let arg = check cx arg func_ty.param_ty in
    let e = Term_app { func; icit; arg } in
    let ty = Evaluate.Fun_ty.app func_ty (Evaluate.eval Env.empty arg) in
    e, ty
  | Expr_proj { mod_e; field; span } ->
    let mod_e, mod_ty = infer_spine cx meta_list mod_e in
    let mod_e, mod_ty = insert_implicit_args cx meta_list span mod_e mod_ty in
    let mod_e, mod_ty = extract_mod_ty cx span mod_e (Whnf.to_value mod_ty) in
    let field_index, _ =
      match
        List.findi mod_ty.ty_decls ~f:(fun _ ty_decl ->
          String.equal ty_decl.var.name field)
      with
      | Some i -> i
      | None ->
        raise_error
          { code = None
          ; parts =
              [ Diagnostic.Part.create
                  ~snippet:(Context.snippet cx span)
                  (Doc.string "Module does not have field " ^^ Doc.string field)
              ]
          }
    in
    let e = Term_proj { mod_e; field; field_index } in
    let ty =
      Evaluate.Ty.proj
        cx.ty_env
        (Evaluate.eval Env.empty mod_e)
        (Value_ty_mod mod_ty)
        field_index
    in
    e, ty
  | _ -> infer cx e

and check_universe (cx : Context.t) (ty_expr : expr) : term * Universe.t =
  let ty, kind = infer cx ty_expr in
  let ty, kind = Unify.coerce_singleton cx ty kind in
  match kind with
  | Value_universe u -> ty, u
  | _ ->
    raise_error
      { code = None
      ; parts =
          [ Diagnostic.Part.create
              ~snippet:(Context.snippet cx (Expr.span ty_expr))
              (Doc.string "Not a type")
          ]
      }
;;

let infer source e =
  let cx = Context.create source in
  match infer cx e with
  | r -> Ok r
  | exception Error diagnostic -> Error diagnostic
;;
