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
end

module Error = struct
  type t = string Spanned.t
end

module State = struct
  type t =
    { var_map : Syntax.Level.t Var.Table.t
    ; mutable errors : Error.t list
    ; mutable context_size : int
    }

  let with_var st var ~f =
    let prev = Hashtbl.find st.var_map var in
    Hashtbl.set st.var_map ~key:var ~data:(Level.of_int st.context_size);
    st.context_size <- st.context_size + 1;
    let result = f () in
    st.context_size <- st.context_size - 1;
    (match prev with
     | Some prev -> Hashtbl.set st.var_map ~key:var ~data:prev
     | None -> Hashtbl.remove st.var_map var);
    result
  ;;

  let add_error st error = st.errors <- error :: st.errors
end

let var_info (var : Var.t) : Syntax.Var_info.t = { name = var.name; pos = var.span.start }

let rec rename_expr st (expr : Surface.expr) : Syntax.expr =
  match expr with
  | Surface.Expr_var var ->
    let span = var.span in
    if String.equal var.name "_"
    then begin
      State.add_error st (Spanned.create "Cannot use underscore as a variable" span);
      Syntax.Expr_error { span }
    end
    else begin
      match Hashtbl.find st.var_map var with
      | Some level -> Expr_var { var = Index.of_level st.context_size level; span }
      | None ->
        State.add_error st (Spanned.create ("Failed to find variable: " ^ var.name) span);
        Syntax.Expr_error { span }
    end
  | Surface.Expr_ann { e; ty; span } ->
    let e = rename_expr st e in
    let ty = rename_expr st ty in
    Expr_ann { e; ty; span }
  | Surface.Expr_app { func; args; span = _ } ->
    let func = rename_expr st func in
    List.fold args ~init:func ~f:(fun func arg ->
      let arg = rename_expr st arg in
      Syntax.Expr_app
        { func; arg; span = Span.combine (Syntax.Expr.span func) (Syntax.Expr.span arg) })
  | Surface.Expr_abs { params; ret_ty = _; body; span } ->
    let all_vars =
      Non_empty_list.to_list params
      |> List.concat_map ~f:(fun (param : Surface.param) ->
        let ann = param.ann in
        Non_empty_list.to_list param.vars |> List.map ~f:(fun var -> var, ann))
    in
    rename_abs st all_vars body span
  | Surface.Expr_ty_fun { param_tys; body_ty; span } ->
    let all_params =
      Non_empty_list.to_list param_tys
      |> List.concat_map ~f:(fun (param_ty : Surface.param_ty) ->
        let ty = param_ty.ty in
        match param_ty.vars with
        | [] -> [ None, ty ]
        | vars -> List.map vars ~f:(fun var -> Some var, ty))
    in
    rename_ty_fun st all_params body_ty span
  | Surface.Expr_proj { mod_e; field; span } ->
    let mod_e = rename_expr st mod_e in
    Expr_proj { mod_e; field; span }
  | Surface.Expr_mod { decls; span } ->
    let decls = rename_decls st decls in
    Expr_mod { decls; span }
  | Surface.Expr_ty_mod { ty_decls; span } ->
    let ty_decls = rename_ty_decls st ty_decls in
    Expr_ty_mod { ty_decls; span }
  | Surface.Expr_block { decls; ret; span } -> rename_block st decls ret span
  | Surface.Expr_ty_sing { identity; span } ->
    let identity = rename_expr st identity in
    Expr_ty_sing { identity; span }
  | Surface.Expr_bool { value; span } -> Expr_bool { value; span }
  | Surface.Expr_unit { span } -> Expr_unit { span }
  | Surface.Expr_number { value = _; span = _ } -> failwith "todo: Expr_number"
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
  | Surface.Expr_bind { var; rhs; body; span } ->
    let rhs = rename_expr st rhs in
    State.with_var st var ~f:(fun () ->
      let body = rename_expr st body in
      Syntax.Expr_bind { var = var_info var; rhs; body; span })

and rename_abs st vars body span =
  match vars with
  | [] -> rename_expr st body
  | (var, ann) :: rest ->
    let param_ty = Option.map ann ~f:(rename_expr st) in
    State.with_var st var ~f:(fun () ->
      let body = rename_abs st rest body span in
      Syntax.Expr_abs { var = var_info var; param_ty; body; span })

and rename_ty_fun st params body_ty span =
  match params with
  | [] -> rename_expr st body_ty
  | (var, ty) :: rest ->
    let param_ty = rename_expr st ty in
    let var = Option.value var ~default:{ Var.name = "_"; span } in
    let body_ty =
      State.with_var st var ~f:(fun () -> rename_ty_fun st rest body_ty span)
    in
    Syntax.Expr_ty_fun { var = var_info var; param_ty; body_ty; span }

and rename_block st decls ret span =
  match decls with
  | [] -> rename_expr st ret
  | Surface.Block_decl_let { var; ann; rhs; span = decl_span } :: rest ->
    let rhs =
      match ann with
      | Some ty ->
        let ty = rename_expr st ty in
        let rhs = rename_expr st rhs in
        Syntax.Expr_ann { e = rhs; ty; span = decl_span }
      | None -> rename_expr st rhs
    in
    State.with_var st var ~f:(fun () ->
      let body = rename_block st rest ret span in
      Syntax.Expr_let { var = var_info var; rhs; body; span })
  | Surface.Block_decl_bind { var; rhs; span = _ } :: rest ->
    let rhs = rename_expr st rhs in
    State.with_var st var ~f:(fun () ->
      let body = rename_block st rest ret span in
      Syntax.Expr_bind { var = var_info var; rhs; body; span })

and rename_decls st decls =
  match decls with
  | [] -> []
  | (decl : Surface.decl) :: rest ->
    let e =
      match decl.ann with
      | Some ty ->
        let ty = rename_expr st ty in
        let rhs = rename_expr st decl.e in
        Syntax.Expr_ann { e = rhs; ty; span = decl.span }
      | None -> rename_expr st decl.e
    in
    let d : Syntax.expr_decl = { var = var_info decl.var; e; span = decl.span } in
    State.with_var st decl.var ~f:(fun () -> d :: rename_decls st rest)

and rename_ty_decls st ty_decls =
  match ty_decls with
  | [] -> []
  | (decl : Surface.ty_decl) :: rest ->
    let ty = rename_expr st decl.ty in
    let d : Syntax.expr_ty_decl = { var = var_info decl.var; ty; span = decl.span } in
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
  let st : State.t = { var_map = Var.Table.create (); errors = []; context_size = 0 } in
  let expr = rename_expr st expr in
  let diagnostics = List.rev st.errors |> List.map ~f:(error_to_diagnostic source) in
  diagnostics, expr
;;
