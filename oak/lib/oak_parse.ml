open Prelude

open struct
  module Source = Oak_source
  module Shrub = Shrubbery.Syntax
  module Token = Shrubbery.Token
  module Fail = Utility.Fail
  module Surface = Oak_surface
  module Span = Utility.Span
  module Spanned = Utility.Spanned
  module File_span = Utility.File_span
  module Diagnostic = Oak_diagnostic
  module Snippet = Utility.Diagnostic.Snippet
end

module Error = struct
  type t = string Spanned.t [@@deriving sexp_of]

  let token error token = { Spanned.value = error; span = Span.single token }
  let create = Spanned.create
end

module State = struct
  type t =
    { mutable errors : Error.t list
    ; next_non_trivia : int array Lazy.t
    ; tokens : Token.t array
    }

  let create tokens =
    { errors = []
    ; tokens
    ; next_non_trivia = lazy (Token.calculate_next_non_trivia tokens)
    }
  ;;
end

exception Error of Error.t

let ( let@ ) f k = f k

module Match = struct
  include Shrubbery.Match

  let builtin h name item =
    let token = Item.token h item in
    Shrubbery.Match.Token.equals h (Ident name) token;
    token
  ;;

  let var h item =
    let token = Item.token h item in
    match token.token with
    | Ident name -> ({ name; span = Span.single token.index } : Surface.Var.t)
    | _ -> Fail.fail h
  ;;

  let operator h name item =
    let token = Item.token h item in
    Shrubbery.Match.Token.equals h (Operator name) token;
    token
  ;;

  let equal_sign h item =
    let token = Item.token h item in
    Shrubbery.Match.Token.equals h Equal token;
    token
  ;;

  let colon h item =
    let token = Item.token h item in
    Shrubbery.Match.Token.equals h Colon token;
    token
  ;;

  let brace h item =
    let delim = Item.tree h item in
    Shrubbery.Match.Token.equals h LBrace delim.ldelim;
    delim
  ;;
end

let error e = raise_notrace (Error e)
let run_or_error e f = Fail.run_or_thunk ~f ~default:(fun () -> error (Lazy.force e))

module Items = struct
  type t =
    { items : Shrub.item Fail.List.t
    ; last_index : int
    }

  let peek t = Fail.List.peek t.items

  let get_span t =
    Option.map (peek t) ~f:(fun t -> (Shrub.Item.first_token t).index)
    |> Option.value ~default:t.last_index
  ;;

  let next env t = Fail.List.next env t.items
  let next_exn t = Fail.List.next_exn t.items
  let take t = Fail.List.take t.items

  let create items =
    { items = Fail.List.create (Non_empty_list.to_list items)
    ; last_index = Shrub.Item.span (Non_empty_list.last items) |> Span.stop
    }
  ;;

  let one_of t fs = Fail.List.one_of t.items fs
  let run t ~f = Fail.List.run t.items ~f
  let run_or_thunk t ~default ~f = Fail.List.run_or_thunk t.items ~default ~f
  let check t ~f = take t |> List.hd |> Option.iter ~f

  let run_or_index t ~default ~f =
    Fail.List.run_or_peek
      t.items
      ~default:(fun items ->
        let index =
          Option.map (List.hd items) ~f:(fun t -> (Shrub.Item.first_token t).index)
          |> Option.value ~default:t.last_index
        in
        default index)
      ~f
  ;;
end

let rec parse_root st (root : Shrub.root) : Surface.expr =
  match root with
  | None -> error (Error.create "Empty root" Span.empty)
  | Some group -> parse_expr_group st group

and parse_expr_group st (group : Shrub.group) : Surface.expr =
  let items = Items.create group in
  let expr = parse_expr st items in
  Items.check items ~f:(fun item ->
    error
      (Error.token
         "Unconsumed tokens when parsing expression"
         (Shrub.Item.first_token item).index));
  expr

and parse_expr st (items : Items.t) : Surface.expr =
  Items.run_or_index
    items
    ~default:(fun index -> error (Error.token "Expected expression" index))
    ~f:(fun h -> parse_expr_ann st h items)

and parse_expr_ann st h (items : Items.t) : Surface.expr =
  let e = parse_keyword st h items in
  let ann = Items.run items ~f:(fun h -> parse_annotation_cont st h items) in
  match ann with
  | Some ty ->
    Surface.Expr_ann
      { e; ty; span = Span.combine (Surface.expr_span e) (Surface.expr_span ty) }
  | None -> e

and parse_keyword st h (items : Items.t) : Surface.expr =
  match Items.peek items with
  | Some (Token { token = Ident keyword; index = keyword_index }) -> begin
    match keyword with
    | "fun" ->
      let _ = Items.next_exn items in
      let params =
        Items.run_or_index
          items
          ~default:(fun index ->
            error (Error.token "Expected a function parameter" index))
          ~f:(fun h -> Fail.List.some items.items (fun () -> parse_param st h items))
      in
      let ret_ty = Items.run items ~f:(fun h -> parse_annotation_cont st h items) in
      let _ =
        Items.run_or_index
          items
          ~default:(fun index -> error (Error.token "Expected arrow" index))
          ~f:(fun h -> Match.operator h "->" (Items.next h items))
      in
      let body = parse_expr st items in
      Expr_abs
        { params
        ; ret_ty
        ; body
        ; span = Span.combine (Span.single keyword_index) (Surface.expr_span body)
        }
    | "mod" ->
      let _ = Items.next_exn items in
      let block =
        Items.run_or_index
          items
          ~default:(fun index -> error (Error.token "Expected {" index))
          ~f:(fun h -> Match.brace h (Items.next h items))
      in
      let decls =
        List.map block.groups ~f:(fun { Shrub.group; sep = _ } -> parse_mod_decl st group)
      in
      let span =
        Span.combine (Span.single keyword_index) (Span.single block.rdelim.index)
      in
      Expr_mod { decls; span }
    | "sig" ->
      let _ = Items.next_exn items in
      let block =
        Items.run_or_index
          items
          ~default:(fun index -> error (Error.token "Expected {" index))
          ~f:(fun h -> Match.brace h (Items.next h items))
      in
      let ty_decls =
        List.map block.groups ~f:(fun { Shrub.group; sep = _ } -> parse_sig_decl st group)
      in
      let span =
        Span.combine (Span.single keyword_index) (Span.single block.rdelim.index)
      in
      Expr_ty_mod { ty_decls; span }
    | "Fun" ->
      let _ = Items.next_exn items in
      let param_tys =
        Items.run_or_index
          items
          ~default:(fun index -> error (Error.token "Expected a type parameter" index))
          ~f:(fun h -> Fail.List.some items.items (fun () -> parse_param_ty st h items))
      in
      let _ =
        Items.run_or_index
          items
          ~default:(fun index -> error (Error.token "Expected arrow" index))
          ~f:(fun h -> Match.operator h "->" (Items.next h items))
      in
      let body_ty = parse_expr st items in
      Expr_ty_fun
        { param_tys
        ; body_ty
        ; span = Span.combine (Span.single keyword_index) (Surface.expr_span body_ty)
        }
    | "pack" ->
      let _ = Items.next_exn items in
      let e = parse_atom st items in
      Expr_pack
        { e; span = Span.combine (Span.single keyword_index) (Surface.expr_span e) }
    | _ -> parse_app st h items
  end
  | _ -> parse_app st h items

and parse_mod_decl st (group : Shrub.group) : Surface.decl =
  let items = Items.create group in
  match Items.peek items with
  | Some (Token { token = Ident "let"; index = let_index }) ->
    let _ = Items.next_exn items in
    let var =
      Items.run_or_index
        items
        ~default:(fun index -> error (Error.token "Expected variable name" index))
        ~f:(fun h -> Match.var h (Items.next h items))
    in
    let ann = Items.run items ~f:(fun h -> parse_annotation_cont st h items) in
    let _ =
      Items.run_or_index
        items
        ~default:(fun index -> error (Error.token "Expected =" index))
        ~f:(fun h -> Match.equal_sign h (Items.next h items))
    in
    let e = parse_expr st items in
    Items.check items ~f:(fun item ->
      error
        (Error.token
           "Unconsumed tokens in module declaration"
           (Shrub.Item.first_token item).index));
    let span = Span.combine (Span.single let_index) (Surface.expr_span e) in
    { var; ann; e; span }
  | _ ->
    error
      (Error.token "Expected module declaration" (Shrub.Group.first_token group).index)

and parse_sig_decl st (group : Shrub.group) : Surface.ty_decl =
  let items = Items.create group in
  match Items.peek items with
  | Some (Token { token = Ident "let"; index = let_index }) ->
    let _ = Items.next_exn items in
    let var =
      Items.run_or_index
        items
        ~default:(fun index -> error (Error.token "Expected variable name" index))
        ~f:(fun h -> Match.var h (Items.next h items))
    in
    let ty =
      Items.run_or_index
        items
        ~default:(fun index -> error (Error.token "Expected :" index))
        ~f:(fun h -> parse_annotation_cont st h items)
    in
    Items.check items ~f:(fun item ->
      error
        (Error.token
           "Unconsumed tokens in signature declaration"
           (Shrub.Item.first_token item).index));
    let span = Span.combine (Span.single let_index) (Surface.expr_span ty) in
    { var; ty; span }
  | _ ->
    error
      (Error.token "Expected signature declaration" (Shrub.Group.first_token group).index)

and parse_param_ty st h (items : Items.t) : Surface.param_ty =
  match Items.peek items with
  | Some (Delim delim) ->
    let _ = Items.next_exn items in
    parse_paren_param_ty st delim
  | _ ->
    let ty =
      Items.run_or_index
        items
        ~default:(fun _index -> Fail.fail h)
        ~f:(fun h -> parse_dot st h items)
    in
    { vars = []; ty; span = Surface.expr_span ty }

and has_colon (group : Shrub.group) : bool =
  Non_empty_list.exists group ~f:(fun item ->
    match item with
    | Shrub.Token { token = Colon; _ } -> true
    | _ -> false)

and parse_paren_param_ty st (delim : Shrub.item_delim) : Surface.param_ty =
  let group =
    run_or_error
      (lazy
        (Error.token
           "Invalid type parameter syntax, did not expect comma"
           delim.ldelim.index))
      (fun h -> Match.Item_delim.single_group h delim)
  in
  if has_colon group
  then begin
    let items = Items.create group in
    let vars =
      Items.run_or_index
        items
        ~default:(fun index -> error (Error.token "Expected variable name" index))
        ~f:(fun h ->
          Fail.List.some items.items (fun () -> Match.var h (Items.next h items))
          |> Non_empty_list.to_list)
    in
    let ty =
      Items.run_or_index
        items
        ~default:(fun index -> error (Error.token "Expected :" index))
        ~f:(fun h -> parse_annotation_cont st h items)
    in
    Items.check items ~f:(fun item ->
      error
        (Error.token
           "Unconsumed tokens in type parameter"
           (Shrub.Item.first_token item).index));
    let span = Span.combine (List.hd_exn vars).span (Surface.expr_span ty) in
    { vars; ty; span }
  end
  else begin
    let ty = parse_paren st delim in
    { vars = []; ty; span = Surface.expr_span ty }
  end

and parse_param st h (items : Items.t) : Surface.param =
  match Items.next h items with
  | Delim delim -> parse_paren_param st delim
  | Token { token = Ident var; index = var_index } ->
    let span = Span.single var_index in
    let var = Surface.Var.create var span in
    { vars = [ var ]; ann = None; span }
  | _ -> Fail.fail h

and parse_paren_param st (delim : Shrub.item_delim) : Surface.param =
  let items =
    Items.create
      (run_or_error
         (lazy
           (Error.token
              "Invalid parameter syntax, did not expect comma"
              delim.ldelim.index))
         (fun h -> Match.Item_delim.single_group h delim))
  in
  let vars =
    Items.run_or_index
      items
      ~default:(fun index -> error (Error.token "Expected parameter name" index))
      ~f:(fun h ->
        Fail.List.some items.items (fun () -> Match.var h (Items.next h items)))
  in
  let ann_span, ann =
    Items.run items ~f:(fun h -> parse_annotation_cont st h items)
    |> Option.map ~f:(fun e -> Surface.expr_span e, Some e)
    |> Option.value ~default:((Non_empty_list.hd vars).span, None)
  in
  Items.check items ~f:(fun item ->
    error
      (Error.token "Unconsumed tokens in parameters" (Shrub.Item.first_token item).index));
  { vars; ann; span = Span.combine (Non_empty_list.hd vars).span ann_span }

and parse_annotation_cont st h (items : Items.t) : Surface.expr =
  let _ = Match.colon h (Items.next h items) in
  let ty = parse_keyword st h items in
  ty

and parse_app st h (items : Items.t) : Surface.expr =
  let func = parse_dot st h items in
  let args = Fail.List.many items.items (fun () -> parse_dot st h items) in
  match args with
  | [] -> func
  | _ ->
    Expr_app
      { func
      ; args
      ; span =
          Span.combine (Surface.expr_span func) (Surface.expr_span (List.last_exn args))
      }

and parse_dot st h (items : Items.t) : Surface.expr =
  let expr = parse_atom_fail st h items in
  parse_dot_cont st items expr

and parse_dot_cont st (items : Items.t) (expr : Surface.expr) : Surface.expr =
  match Items.peek items with
  | Some (Token { token = Dot; index = dot_index }) ->
    let _ = Items.next_exn items in
    let ident_index, ident =
      run_or_error
        (lazy (Error.token "Expected identifier after dot token" dot_index))
        (fun h ->
           let tok = Items.next h items |> Match.Item.token h in
           tok.index, Match.Token.ident h tok)
    in
    parse_dot_cont
      st
      items
      (Expr_proj
         { mod_e = expr
         ; field = ident
         ; span = Span.combine (Surface.expr_span expr) (Span.single ident_index)
         })
  | _ -> expr

and parse_atom_fail st h (items : Items.t) : Surface.expr =
  match Items.next h items with
  | Delim delim -> begin
    match delim.ldelim.token with
    | LBrace -> parse_block st delim
    | LParen -> parse_paren st delim
    | _ -> error (Error.token "Unexpected delimiter" delim.ldelim.index)
  end
  | Token { token = Ident ident; index } -> begin
    let span = Span.single index in
    match ident with
    | "Bool" -> Expr_core_ty { ty = Bool; span }
    | "Type" -> Expr_universe { universe = Type; span }
    | "Kind" -> Expr_universe { universe = Kind; span }
    | "#t" -> Expr_bool { value = true; span }
    | "#f" -> Expr_bool { value = false; span }
    | _ -> Expr_var { name = ident; span }
  end
  | Token { token = Number n; index } ->
    Expr_number { value = n; span = Span.single index }
  | _ -> Fail.fail h

and parse_atom st (items : Items.t) : Surface.expr =
  Items.run_or_index
    items
    ~default:(fun index -> error (Error.token "Expected atom" index))
    ~f:(fun h -> parse_atom_fail st h items)

and parse_block st (block : Shrub.item_delim) : Surface.expr =
  match block.groups with
  | [] -> error (Error.token "Empty block" block.ldelim.index)
  | groups ->
    let decl_groups = List.drop_last_exn groups in
    let { Shrub.group = ret_group; sep = _ } = List.last_exn groups in
    let decls =
      List.map decl_groups ~f:(fun { Shrub.group; sep = _ } -> parse_block_decl st group)
    in
    let ret = parse_expr_group st ret_group in
    let span =
      Span.combine (Span.single block.ldelim.index) (Span.single block.rdelim.index)
    in
    Surface.Expr_block { decls; ret; span }

and parse_block_decl st (group : Shrub.group) : Surface.block_decl =
  let items = Items.create group in
  match Items.peek items with
  | Some (Token { token = Ident "let"; index = let_index }) ->
    let _ = Items.next_exn items in
    let var =
      Items.run_or_index
        items
        ~default:(fun index -> error (Error.token "Expected variable name" index))
        ~f:(fun h -> Match.var h (Items.next h items))
    in
    let ann = Items.run items ~f:(fun h -> parse_annotation_cont st h items) in
    let _ =
      Items.run_or_index
        items
        ~default:(fun index -> error (Error.token "Expected =" index))
        ~f:(fun h -> Match.equal_sign h (Items.next h items))
    in
    let rhs_items = Items.take items in
    let rhs =
      match Non_empty_list.of_list rhs_items with
      | None -> error (Error.token "Expected expression after =" let_index)
      | Some group -> parse_expr_group st group
    in
    let span = Span.combine (Span.single let_index) (Surface.expr_span rhs) in
    Block_decl_let { var; ann; rhs; span }
  | Some (Token { token = Ident "bind"; index = bind_index }) ->
    let _ = Items.next_exn items in
    let var =
      Items.run_or_index
        items
        ~default:(fun index -> error (Error.token "Expected variable name" index))
        ~f:(fun h -> Match.var h (Items.next h items))
    in
    let _ =
      Items.run_or_index
        items
        ~default:(fun index -> error (Error.token "Expected =" index))
        ~f:(fun h -> Match.equal_sign h (Items.next h items))
    in
    let rhs_items = Items.take items in
    let rhs =
      match Non_empty_list.of_list rhs_items with
      | None -> error (Error.token "Expected expression after =" bind_index)
      | Some group -> parse_expr_group st group
    in
    let span = Span.combine (Span.single bind_index) (Surface.expr_span rhs) in
    Block_decl_bind { var; rhs; span }
  | _ ->
    error (Error.token "Expected block declaration" (Shrub.Group.first_token group).index)

and parse_paren st (parens : Shrub.item_delim) : Surface.expr =
  match parens.groups with
  | [] ->
    let span =
      Span.combine (Span.single parens.ldelim.index) (Span.single parens.rdelim.index)
    in
    Expr_unit { span }
  | [ { group; sep = _ } ] ->
    let items = Items.create group in
    Items.run_or_thunk
      items
      ~f:(fun h ->
        let _ = Items.next h items |> Match.equal_sign h in
        let e = parse_expr st items in
        Expr_ty_sing
          { e
          ; span =
              Span.combine
                (Span.single parens.ldelim.index)
                (Span.single parens.rdelim.index)
          })
      ~default:(fun () -> parse_expr st items)
  | _ ->
    error (Error.token "Unexpected comma in parenthesized expression" parens.ldelim.index)
;;

let parse_root st (root : Shrub.root) =
  match parse_root st root with
  | exception Error e -> [ e ], None
  | x -> [], Some x
;;

let error_to_diagnostic (source : Source.t) (e : Error.t) : Diagnostic.t =
  let start = source.token_offsets.(e.span.start) in
  let stop = source.token_offsets.(e.span.stop) in
  { code = Some Parse_error
  ; parts =
      [ { kind = Error
        ; message = Doc.string e.value
        ; snippet = Some { file = source.filename; start; stop }
        }
      ]
  }
;;

let shrub_error_to_diagnostic (source : Source.t) (e : Shrubbery.Delimit.Error.t)
  : Diagnostic.t
  =
  (* shrub errors are byte positions *)
  let to_snippet ({ start; stop } : Span.t) : File_span.t =
    { file = source.filename; start; stop }
  in
  match e with
  | Mismatching_delimiters { ldelim; rdelim } ->
    { code = Some Parse_error
    ; parts =
        [ { kind = Error
          ; message = Doc.string "mismatching delimiters"
          ; snippet = Some (to_snippet rdelim)
          }
        ; { kind = Note
          ; message = Doc.string "opening delimiter here"
          ; snippet = Some (to_snippet ldelim)
          }
        ]
    }
  | Expecting_delimiter span ->
    { code = Some Parse_error
    ; parts =
        [ { kind = Error
          ; message = Doc.string "expecting delimiter"
          ; snippet = Some (to_snippet span)
          }
        ]
    }
;;

let parse ~file s =
  let tts, root, shrub_errors = Shrubbery.Parser.parse s in
  let tokens = Shrubbery.Token_tree.Root.to_list tts |> Array.of_list in
  let token_offsets = Shrubbery.Token.calculate_offsets tokens in
  let st = State.create tokens in
  let errors, expr = parse_root st root in
  let source = { Source.content = s; filename = file; tts; tokens; token_offsets } in
  let diagnostics =
    List.map shrub_errors ~f:(shrub_error_to_diagnostic source)
    @ List.map errors ~f:(error_to_diagnostic source)
  in
  source, diagnostics, expr
;;
