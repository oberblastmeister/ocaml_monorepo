open Prelude
module Surface = Oak_surface
module Spanned = Utility.Spanned
module Span = Utility.Span
module Diagnostic = Oak_diagnostic
module Doc = Utility.Pp.Doc
module Source = Oak_source
module Common = Oak_common
module Level = Common.Level
module Index = Common.Index
module Abstract = Oak_abstract
module Name = Common.Name
module Relevancy = Common.Relevancy

module Error = struct
  type t = string Spanned.t
end

module State = struct
  type t =
    { var_map : Level.t list Name.Table.t
    ; mutable var_stack : Name.t list
    ; mutable errors : Error.t list
    ; mutable context_size : int
    }

  let create () =
    { var_map = Name.Table.create (); var_stack = []; errors = []; context_size = 0 }
  ;;

  let push_var st name =
    Hashtbl.add_multi st.var_map ~key:name ~data:(Level.of_int st.context_size);
    st.var_stack <- name :: st.var_stack;
    st.context_size <- st.context_size + 1
  ;;

  let pop_var st =
    let name = List.hd_exn st.var_stack in
    st.var_stack <- List.tl_exn st.var_stack;
    Hashtbl.remove_multi st.var_map name;
    st.context_size <- st.context_size - 1
  ;;

  let with_var st name ~f =
    push_var st name;
    let result = f () in
    pop_var st;
    result
  ;;

  let add_error st error = st.errors <- error :: st.errors
end

let generated_name span = Name.create "<generated>" span

let check_names_distinct st names ~error_message =
  let used_names = Name.Hash_set.create () in
  let duplicate = ref false in
  List.iter names ~f:(fun name ->
    if Hash_set.mem used_names name
    then begin
      duplicate := true;
      State.add_error st (Spanned.create error_message name.span)
    end;
    Hash_set.add used_names name);
  !duplicate
;;

let rec rename_expr st (expr : Surface.expr) : Abstract.expr =
  match expr with
  | Surface.Expr_var name ->
    let span = name.span in
    if String.equal name.name "_"
    then begin
      State.add_error st (Spanned.create "Cannot use underscore as a variable" span);
      Abstract.Expr_error { span }
    end
    else begin
      match Hashtbl.find_multi st.var_map name with
      | level :: _ -> Expr_var { index = Index.of_level st.context_size level; span }
      | [] ->
        State.add_error st (Spanned.create ("Failed to find variable: " ^ name.name) span);
        Abstract.Expr_error { span }
    end
  | Surface.Expr_ann { e; ty; span } ->
    let e = rename_expr st e in
    let ty = rename_expr st ty in
    Expr_ann { e; ty; span }
  | Surface.Expr_app { func; args; span = _ } ->
    let func = rename_expr st func in
    List.fold
      args
      ~init:func
      ~f:(fun func ({ arg; relevancy; icit } : Surface.expr_arg) ->
        let arg = rename_expr st arg in
        Abstract.Expr_app
          { func
          ; arg
          ; param_modifiers = ({ icit; relevancy } : Common.Param_modifiers.t)
          ; span = Span.combine (Abstract.Expr.span func) (Abstract.Expr.span arg)
          })
  | Surface.Expr_brack { span; _ } ->
    State.add_error st (Spanned.create "Invalid bracket expression" span);
    Expr_error { span }
  | Surface.Expr_fun { params; ret_ty = _; body; span } ->
    let all_params =
      Non_empty_list.to_list params
      |> List.concat_map ~f:(fun (param : Surface.param) ->
        Non_empty_list.to_list param.names
        |> List.map ~f:(fun name -> name, param.ann, param.icit, param.relevancy))
    in
    rename_fun st all_params body span
  | Surface.Expr_ty_fun { param_tys; body_ty; span } ->
    let all_params =
      Non_empty_list.to_list param_tys
      |> List.concat_map ~f:(fun (param_ty : Surface.param_ty) ->
        let ty =
          Option.value
            param_ty.ty
            ~default:(Surface.Expr_universe { size = Common.Size.type_; span })
        in
        match param_ty.names with
        | [] -> [ None, ty, param_ty.icit, param_ty.relevancy ]
        | names ->
          List.map names ~f:(fun name -> Some name, ty, param_ty.icit, param_ty.relevancy))
    in
    rename_ty_fun st all_params body_ty span
  | Surface.Expr_proj { strukt; field; span } ->
    let strukt = rename_expr st strukt in
    Expr_proj { strukt; field; span }
  | Surface.Expr_struct { decls; is_dependent; span } ->
    let names =
      List.filter_map decls ~f:(function
        | Surface.Block_decl_val decl -> Some decl.name
        | Surface.Block_decl_bind { name; _ } -> Some name
        | Surface.Block_decl_do _ -> None)
    in
    if check_names_distinct st names ~error_message:"Duplicate variable in struct"
    then Expr_error { span }
    else (
      let decls =
        if is_dependent then rename_decls st decls else rename_decls_nondependent st decls
      in
      Expr_struct { decls; is_dependent; span })
  | Surface.Expr_ty_struct { field_specs; span } ->
    let names = List.map field_specs ~f:(fun decl -> decl.name) in
    if check_names_distinct st names ~error_message:"Duplicate variable in signature"
    then Expr_error { span }
    else (
      let field_specs = rename_field_specs st field_specs in
      Expr_ty_struct { field_specs; span })
  | Surface.Expr_block { decls; ret; span } -> rename_block st decls ret span
  | Surface.Expr_literal { literal; span } -> Expr_literal { literal; span }
  | Surface.Expr_core_ty { ty; span } -> Expr_core_ty { ty; span }
  | Surface.Expr_universe { size; span } -> Expr_universe { size; span }
  | Surface.Expr_if { cond; body1; body2; span } ->
    let cond = rename_expr st cond in
    let body1 = rename_expr st body1 in
    let body2 = rename_expr st body2 in
    Expr_if { cond; body1; body2; span }
  | Surface.Expr_ty_pack { ty; span } ->
    let ty = rename_expr st ty in
    Expr_ty_pack { ty; span }
  | Surface.Expr_alias { e; span = _ } -> rename_expr st e
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
    let names = List.map decls ~f:(fun decl -> decl.name) in
    let duplicate =
      check_names_distinct st names ~error_message:"Duplicate variable in struct"
    in
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
      let tys = List.map tys ~f:(rename_expr st) in
      List.iter decls ~f:(fun decl -> State.push_var st decl.name);
      let rhs_exprs = List.map decls ~f:(fun decl -> rename_expr st decl.rhs) in
      List.iter decls ~f:(fun _ -> State.pop_var st);
      let decls =
        List.zip_exn tys rhs_exprs
        |> List.zip_exn names
        |> List.map ~f:(fun (name, (ty, rhs)) ->
          ({ name; ty; e = rhs } : Abstract.expr_rec_decl))
      in
      Expr_rec { decls; span }
    end
  | Surface.Expr_data { params; body; span } ->
    (match rename_data_expr st { params; body; span } with
     | Some data -> Expr_data data
     | None -> Expr_error { span })
  | Surface.Expr_data_rec { decls; span } ->
    let names = List.map decls ~f:(fun decl -> decl.name) in
    if check_names_distinct st names ~error_message:"Duplicate data declaration"
    then Expr_error { span }
    else begin
      (* First pass to check that the parameters don't reference the data declaration names *)
      List.iter decls ~f:(fun decl ->
        let _ = rename_data_params st decl.data.params in
        List.iter decl.data.params ~f:(fun _ -> State.pop_var st));
      (* Now push the data declarations. The parameters are pushed after inside of rename_data_expr *)
      List.iter decls ~f:(fun decl -> State.push_var st decl.name);
      let decls =
        List.map decls ~f:(fun (decl : Surface.data_decl) ->
          Option.map (rename_data_expr st decl.data) ~f:(fun data ->
            ({ name = decl.name; data; span = decl.span } : Abstract.data_decl)))
      in
      List.iter decls ~f:(fun _ -> State.pop_var st);
      match Option.all decls with
      | Some decls -> Expr_data_rec { decls; span }
      | None -> Expr_error { span }
    end

and rename_rhs st ann rhs span =
  match ann with
  | Some ty ->
    let ty = rename_expr st ty in
    let rhs = rename_expr st rhs in
    Abstract.Expr_ann { e = rhs; ty; span }
  | None -> rename_expr st rhs

and rename_fun st params body span =
  match params with
  | [] -> rename_expr st body
  | (name, ann, icit, relevancy) :: rest ->
    let param_ty = Option.map ann ~f:(rename_expr st) in
    let param_modifiers : Common.Param_modifiers.t = { icit; relevancy } in
    State.with_var st name ~f:(fun () ->
      let body = rename_fun st rest body span in
      Abstract.Expr_fun { name; param_ty; param_modifiers; body; span })

and rename_ty_fun st params body_ty span =
  match params with
  | [] -> rename_expr st body_ty
  | (name, ty, icit, relevancy) :: rest ->
    let param_ty = rename_expr st ty in
    let name = Option.value name ~default:(Name.create "_" span) in
    let param_modifiers : Common.Param_modifiers.t = { icit; relevancy } in
    let body_ty =
      State.with_var st name ~f:(fun () -> rename_ty_fun st rest body_ty span)
    in
    Abstract.Expr_ty_fun { name; param_ty; param_modifiers; body_ty; span }

and rename_data_expr st ({ params; body; span } : Surface.expr_data)
  : Abstract.expr_data option
  =
  (*
    We rename the parameters twice because we first need to check that the parameters don't reference the data declaration names.
  *)
  let params = rename_data_params st params in
  let body = rename_data_body st body ~span in
  List.iter params ~f:(fun _ -> State.pop_var st);
  Option.map body ~f:(fun body -> ({ params; body; span } : Abstract.expr_data))

and rename_data_params st (params : Surface.param list) : Abstract.data_param list =
  let params =
    List.concat_map params ~f:(fun (param : Surface.param) ->
      Non_empty_list.to_list param.names
      |> List.map ~f:(fun name -> name, param.ann, param.span))
  in
  let rec loop params =
    match params with
    | [] -> []
    | (name, ann, span) :: rest ->
      let ty =
        match ann with
        | Some ty -> rename_expr st ty
        | None ->
          State.add_error
            st
            (Spanned.create "Data parameters require type annotations" span);
          Expr_error { span }
      in
      State.push_var st name;
      ({ name; ty } : Abstract.data_param) :: loop rest
  in
  loop params

and rename_data_body st body ~span : Abstract.data_body option =
  let fields, constructors = List.partition_map body ~f:Fn.id in
  if List.is_empty constructors
  then (
    let names = List.map fields ~f:(fun (field : Surface.data_field) -> field.name) in
    if check_names_distinct st names ~error_message:"Duplicate field in data record"
    then None
    else
      Some
        (Data_record
           { fields =
               List.map fields ~f:(fun ({ name; ty } : Surface.data_field) ->
                 ({ name; ty = rename_expr st ty } : Abstract.data_field))
           }))
  else if List.is_empty fields
  then (
    let names =
      List.map constructors ~f:(fun (constructor : Surface.data_constructor) ->
        constructor.name)
    in
    if
      check_names_distinct st names ~error_message:"Duplicate constructor in data variant"
    then None
    else
      Some
        (Data_variant
           { constructors =
               List.map constructors ~f:(fun ({ name; ty } : Surface.data_constructor) ->
                 ({ name; ty = Option.map ty ~f:(rename_expr st) }
                  : Abstract.data_constructor))
           }))
  else (
    State.add_error
      st
      (Spanned.create "Data body cannot mix fields and constructors" span);
    None)

and rename_block st decls ret span =
  match decls with
  | [] -> rename_expr st ret
  | Surface.Block_decl_val ({ name; ann; rhs; relevancy; is_abstract; _ } as decl) :: rest
    ->
    let rhs = rename_rhs st ann rhs decl.span in
    State.with_var st name ~f:(fun () ->
      let body = rename_block st rest ret span in
      Abstract.Expr_let { name; rhs; relevancy; is_abstract; body; span })
  | Surface.Block_decl_bind { name; rhs; span = decl_span } :: rest ->
    let rhs = rename_expr st rhs in
    State.with_var st name ~f:(fun () ->
      let body = rename_block st rest ret span in
      Abstract.Expr_bind { name; rhs; body; span = decl_span })
  | Surface.Block_decl_do { e; span = decl_span } :: rest ->
    let e = rename_expr st e in
    let name = generated_name decl_span in
    State.with_var st name ~f:(fun () ->
      let body = rename_block st rest ret span in
      Abstract.Expr_let
        { name
        ; rhs = e
        ; relevancy = Relevancy.Relevant
        ; is_abstract = false
        ; body
        ; span = decl_span
        })

and rename_decls st decls =
  match decls with
  | [] -> []
  | (decl : Surface.block_decl) :: rest -> begin
    match decl with
    | Surface.Block_decl_val { name; ann; rhs; relevancy; is_abstract; span } ->
      let rhs = rename_rhs st ann rhs span in
      let d : Abstract.expr_decl = { name; relevancy; e = rhs; is_abstract; span } in
      State.with_var st name ~f:(fun () -> d :: rename_decls st rest)
    | Surface.Block_decl_bind { span; _ } ->
      State.add_error
        st
        (Spanned.create "Bind declarations are not allowed inside structs" span);
      rename_decls st rest
    | Surface.Block_decl_do { span; _ } ->
      State.add_error
        st
        (Spanned.create "Do declarations are not allowed inside structs" span);
      rename_decls st rest
  end

and rename_decls_nondependent st decls =
  match decls with
  | [] -> []
  | (decl : Surface.block_decl) :: rest -> begin
    match decl with
    | Surface.Block_decl_val { name; ann; rhs; relevancy; is_abstract; span } ->
      let rhs = rename_rhs st ann rhs span in
      let d : Abstract.expr_decl = { name; relevancy; e = rhs; is_abstract; span } in
      d :: rename_decls_nondependent st rest
    | Surface.Block_decl_bind { span; _ } ->
      State.add_error
        st
        (Spanned.create "Bind declarations are not allowed inside structs" span);
      rename_decls_nondependent st rest
    | Surface.Block_decl_do { span; _ } ->
      State.add_error
        st
        (Spanned.create "Do declarations are not allowed inside structs" span);
      rename_decls_nondependent st rest
  end

and rename_field_specs st field_specs =
  match field_specs with
  | [] -> []
  | (decl : Surface.field_spec) :: rest ->
    if Option.is_none decl.ty && Option.is_none decl.rhs
    then
      State.add_error
        st
        (Spanned.create
           "Signature declarations require either a type annotation or a definition"
           decl.span);
    let ty = Option.map decl.ty ~f:(rename_expr st) in
    let rhs = Option.map decl.rhs ~f:(rename_expr st) in
    let d : Abstract.expr_field_spec =
      { name = decl.name; relevancy = decl.relevancy; ty; rhs; span = decl.span }
    in
    State.with_var st decl.name ~f:(fun () -> d :: rename_field_specs st rest)
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
