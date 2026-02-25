open Prelude

open struct
  module Surface = Oak_surface
  module Syntax = Oak_syntax
  module Spanned = Utility.Spanned
  module Span = Utility.Span
  module Level = Syntax.Level
  module Index = Syntax.Index
  module Var = Surface.Var
  module Diagnostic = Oak_diagnostic
  module Doc = Utility.Pp.Doc
  module Source = Oak_source
  module Common = Oak_common
  module Abstract = Oak_abstract
end

module Error = struct
  type t = string Spanned.t
end

module State = struct
  type t =
    { var_map : Syntax.Level.t list Var.Table.t
    ; mutable var_stack : Var.t list
    ; mutable errors : Error.t list
    ; mutable context_size : int
    }

  let create () =
    { var_map = Var.Table.create (); var_stack = []; errors = []; context_size = 0 }
  ;;

  let push_var st var =
    Hashtbl.add_multi st.var_map ~key:var ~data:(Level.of_int st.context_size);
    st.var_stack <- var :: st.var_stack;
    st.context_size <- st.context_size + 1
  ;;

  let pop_var st =
    let var = List.hd_exn st.var_stack in
    st.var_stack <- List.tl_exn st.var_stack;
    Hashtbl.remove_multi st.var_map var;
    st.context_size <- st.context_size - 1
  ;;

  let with_var st var ~f =
    push_var st var;
    let result = f () in
    pop_var st;
    result
  ;;

  let add_error st error = st.errors <- error :: st.errors
end

let var_info (var : Var.t) : Syntax.Var_info.t = { name = var.name; pos = var.span.start }

let check_vars_distinct st vars =
  let used_vars = Surface.Var.Hash_set.create () in
  let duplicate = ref false in
  List.iter vars ~f:(fun var ->
    if Hash_set.mem used_vars var
    then begin
      duplicate := true;
      State.add_error st (Spanned.create "Duplicate variable in module" var.span)
    end;
    Hash_set.add used_vars var);
  !duplicate
;;

let rec rename_expr st (expr : Surface.expr) : Abstract.expr =
  match expr with
  | Surface.Expr_var var ->
    let span = var.span in
    if String.equal var.name "_"
    then begin
      State.add_error st (Spanned.create "Cannot use underscore as a variable" span);
      Abstract.Expr_error { span }
    end
    else begin
      match Hashtbl.find_multi st.var_map var with
      | level :: _ -> Expr_var { var = Index.of_level st.context_size level; span }
      | [] ->
        State.add_error st (Spanned.create ("Failed to find variable: " ^ var.name) span);
        Abstract.Expr_error { span }
    end
  | Surface.Expr_ann { e; ty; span } ->
    let e = rename_expr st e in
    let ty = rename_expr st ty in
    Expr_ann { e; ty; span }
  | Surface.Expr_app { func; args; span = _ } ->
    let func = rename_expr st func in
    List.fold args ~init:func ~f:(fun func arg ->
      let icit, arg =
        match arg with
        | Surface.Expr_brack { e; _ } -> Syntax.Icit.Impl, rename_expr st e
        | _ -> Syntax.Icit.Expl, rename_expr st arg
      in
      Abstract.Expr_app
        { func
        ; arg
        ; icit
        ; span = Span.combine (Abstract.Expr.span func) (Abstract.Expr.span arg)
        })
  | Surface.Expr_brack { e = _; span } ->
    State.add_error st (Spanned.create "Invalid bracket expression" span);
    Expr_error { span }
  | Surface.Expr_abs { params; ret_ty = _; body; span } ->
    let all_vars =
      Non_empty_list.to_list params
      |> List.concat_map ~f:(fun (param : Surface.param) ->
        Non_empty_list.to_list param.vars
        |> List.map ~f:(fun var -> var, param.ann, param.icit))
    in
    rename_abs st all_vars body span
  | Surface.Expr_ty_fun { param_tys; body_ty; span } ->
    let all_params =
      Non_empty_list.to_list param_tys
      |> List.concat_map ~f:(fun (param_ty : Surface.param_ty) ->
        let ty =
          Option.value
            param_ty.ty
            ~default:(Expr_universe { universe = Syntax.Universe.type_; span })
        in
        match param_ty.vars with
        | [] -> [ None, ty, param_ty.icit ]
        | vars -> List.map vars ~f:(fun var -> Some var, ty, param_ty.icit))
    in
    rename_ty_fun st all_params body_ty span
  | Surface.Expr_proj { mod_e; field; span } ->
    let mod_e = rename_expr st mod_e in
    Expr_proj { mod_e; field; span }
  | Surface.Expr_mod { decls; span } ->
    let vars =
      List.filter_map decls ~f:(fun decl ->
        match decl with
        | Block_decl_let { var; _ } | Block_decl_bind { var; _ } -> Some var
        | Block_decl_expr _ -> None)
    in
    let duplicate = check_vars_distinct st vars in
    if duplicate
    then Expr_error { span }
    else begin
      let decls = rename_decls st decls in
      Expr_mod { decls; span }
    end
  | Surface.Expr_ty_mod { ty_decls; span } ->
    let ty_decls = rename_ty_decls st ty_decls in
    Expr_ty_mod { ty_decls; span }
  | Surface.Expr_block { decls; ret; span } -> rename_block st decls ret span
  | Expr_alias { e; span } ->
    let e = rename_expr st e in
    Abstract.Expr_alias { identity = e; span }
  | Surface.Expr_ty_sing { identity; span } ->
    let identity = rename_expr st identity in
    Expr_ty_sing { identity; span }
  | Surface.Expr_literal { literal; span } -> Expr_literal { literal; span }
  | Surface.Expr_core_ty { ty; span } -> Expr_core_ty { ty; span }
  | Surface.Expr_universe { universe; span } -> Expr_universe { universe; span }
  | Surface.Expr_if { cond; body1; body2; span } ->
    let cond = rename_expr st cond in
    let body1 = rename_expr st body1 in
    let body2 = rename_expr st body2 in
    Expr_if { cond; body1; body2; span }
  | Surface.Expr_ty_pack { ty; span } ->
    let ty = rename_expr st ty in
    Expr_ty_pack { ty; span }
  | Surface.Expr_pack { e; span } ->
    let e = rename_expr st e in
    Expr_pack { e; span }
  | Surface.Expr_paren { e; span = _ } -> rename_expr st e
  | Surface.Expr_where { e; patches; span } ->
    let e = rename_expr st e in
    List.fold patches ~init:e ~f:(fun e (patch : Surface.where_patch) ->
      let rhs = rename_expr st patch.rhs in
      Abstract.Expr_where { e; path = patch.path; rhs; span })
  | Surface.Expr_rec { decls; span } ->
    List.iter decls ~f:(fun decl ->
      if decl.is_alias
      then
        State.add_error st (Spanned.create "Cannot have alias inside rec block" decl.span));
    let vars = List.map decls ~f:(fun decl -> decl.var) in
    let duplicate = check_vars_distinct st vars in
    let num_decls = List.length decls in
    let tys =
      List.filter_map decls ~f:(fun decl ->
        if Option.is_none decl.ann
        then
          State.add_error
            st
            (Spanned.create "type annotations required for recursive block" decl.span);
        decl.ann)
    in
    let missing_annotation = List.length tys <> num_decls in
    if duplicate || missing_annotation
    then Expr_error { span }
    else begin
      let tys = List.map tys ~f:(fun ty -> rename_expr st ty) in
      (* push *)
      List.iter decls ~f:(fun decl -> State.push_var st decl.var);
      let rhs_exprs = List.map decls ~f:(fun decl -> rename_expr st decl.rhs) in
      List.iter decls ~f:(fun _ -> State.pop_var st);
      (* pop *)
      let decls =
        List.zip_exn tys rhs_exprs
        |> List.zip_exn vars
        |> List.map ~f:(fun (var, (ty, rhs)) ->
          ({ var = var_info var; ty; e = rhs } : Abstract.expr_rec_decl))
      in
      Expr_rec { decls; span }
    end

and rename_abs st vars body span =
  match vars with
  | [] -> rename_expr st body
  | (var, ann, icit) :: rest ->
    let param_ty = Option.map ann ~f:(rename_expr st) in
    State.with_var st var ~f:(fun () ->
      let body = rename_abs st rest body span in
      Abstract.Expr_abs { var = var_info var; param_ty; icit; body; span })

and rename_ty_fun st params body_ty span =
  match params with
  | [] -> rename_expr st body_ty
  | (var, ty, icit) :: rest ->
    let param_ty = rename_expr st ty in
    let var = Option.value var ~default:{ Var.name = "_"; span } in
    let body_ty =
      State.with_var st var ~f:(fun () -> rename_ty_fun st rest body_ty span)
    in
    Abstract.Expr_ty_fun { var = var_info var; param_ty; icit; body_ty; span }

and rename_block st decls ret span =
  match decls with
  | [] -> rename_expr st ret
  | Surface.Block_decl_let { var; ann; rhs; is_alias; _ } :: rest ->
    (* Important that the annotation goes first, before the alias. *)
    let rhs =
      match ann with
      | Some ty ->
        let ty = rename_expr st ty in
        let rhs = rename_expr st rhs in
        Abstract.Expr_ann { e = rhs; ty; span }
      | None -> rename_expr st rhs
    in
    let rhs =
      if is_alias
      then Abstract.Expr_alias { identity = rhs; span = Abstract.Expr.span rhs }
      else rhs
    in
    State.with_var st var ~f:(fun () ->
      let body = rename_block st rest ret span in
      Abstract.Expr_let { var = var_info var; rhs; body; span })
  | Surface.Block_decl_bind { var; rhs; span = _ } :: rest ->
    let rhs = rename_expr st rhs in
    State.with_var st var ~f:(fun () ->
      let body = rename_block st rest ret span in
      Abstract.Expr_bind { var = var_info var; rhs; body; span })
  | Surface.Block_decl_expr { e; span } :: rest ->
    let e = rename_expr st e in
    State.with_var st { name = "<generated>"; span } ~f:(fun () ->
      let body = rename_block st rest ret span in
      Abstract.Expr_let { var = Abstract.Var_info.generated; rhs = e; body; span })

and rename_decls st decls =
  match decls with
  | [] -> []
  | (decl : Surface.block_decl) :: rest -> begin
    match decl with
    | Block_decl_let { var; ann; is_alias; rhs; span } ->
      let rhs =
        match ann with
        | Some ty ->
          let ty = rename_expr st ty in
          let rhs = rename_expr st rhs in
          Abstract.Expr_ann { e = rhs; ty; span }
        | None -> rename_expr st rhs
      in
      let rhs =
        if is_alias
        then Abstract.Expr_alias { identity = rhs; span = Abstract.Expr.span rhs }
        else rhs
      in
      let d : Abstract.expr_decl = { var = var_info var; e = rhs; span } in
      State.with_var st var ~f:(fun () -> d :: rename_decls st rest)
    | Block_decl_bind { span; _ } ->
      State.add_error
        st
        (Spanned.create "Bind declarations are not allowed at the top level" span);
      rename_decls st rest
    | Block_decl_expr { span; _ } ->
      State.add_error
        st
        (Spanned.create "Expression declarations are not allowed at the top level" span);
      rename_decls st rest
  end

and rename_ty_decls st ty_decls =
  match ty_decls with
  | [] -> []
  | (decl : Surface.ty_decl) :: rest ->
    let ty = rename_expr st decl.ty in
    let d : Abstract.expr_ty_decl = { var = var_info decl.var; ty; span = decl.span } in
    State.with_var st decl.var ~f:(fun () -> d :: rename_ty_decls st rest)
;;

let error_to_diagnostic (source : Source.t) (e : Error.t) : Diagnostic.t =
  let start = source.token_offsets.(e.span.start) in
  let stop = source.token_offsets.(e.span.stop) in
  { code = None
  ; parts =
      [ { kind = Error
        ; message = Doc.string e.value
        ; snippet = Some { file = source.filename; start; stop }
        }
      ]
  }
;;

let rename source expr =
  let st = State.create () in
  let expr = rename_expr st expr in
  let diagnostics = List.rev st.errors |> List.map ~f:(error_to_diagnostic source) in
  diagnostics, expr
;;
