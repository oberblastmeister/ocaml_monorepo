(* TODO: refactor this, the code is shit *)
open Prelude

open struct
  module Source = Oak_source
  module Shrub = Shrubbery.Syntax
  module Token = Shrubbery.Token
  module Surface = Oak_surface
  module Span = Utility.Span
  module Spanned = Utility.Spanned
  module File_span = Utility.File_span
  module Diagnostic = Oak_diagnostic
  module Snippet = Utility.Diagnostic.Snippet
  module Universe = Oak_common.Universe
end

module Error = struct
  type t = string Spanned.t [@@deriving sexp_of]

  let token error token = { Spanned.value = error; span = Span.single token }
  let create = Spanned.create
end

module Parser_data = struct
  type t = { last_index : int }
end

module Parser_initial = Utility.Fail_state.Make (struct
    module State = struct
      type t = Shrub.Item.t list
    end

    module Data = Parser_data
  end)

module Parser = struct
  include Parser_initial

  module State = struct
    include Parser_initial.State

    let create group =
      let last_index = Shrub.Item.span (Non_empty_list.last group) |> Span.stop in
      let data : Parser_data.t = { last_index } in
      let items = Non_empty_list.to_list group in
      { Parser_initial.State.state = items; data }
    ;;

    let peek t = List.hd t.state
    let next_exn t = t.state <- List.tl_exn t.state
    let is_empty t = List.is_empty t.state

    let curr_pos t =
      match peek t with
      | Some item -> (Shrub.Item.first_token item).index
      | None -> t.data.last_index
    ;;

    let take t =
      let items = t.state in
      t.state <- [];
      items
    ;;
  end

  let peek t = state t |> State.peek
  let next_exn t = state t |> State.next_exn

  let next p =
    match state p |> State.peek with
    | None -> fail p
    | Some x ->
      state p |> State.next_exn;
      x
  ;;

  let token p =
    match next p with
    | Token ti -> ti
    | _ -> fail p
  ;;

  let operator p s =
    let ti = token p in
    match ti.token with
    | Operator op when String.equal op s -> ti
    | _ -> fail p
  ;;

  let brace p =
    match next p with
    | Delim delim when Token.equal delim.ldelim.token LBrace -> delim
    | _ -> fail p
  ;;

  let var p =
    let ti = token p in
    match ti.token with
    | Ident name -> ({ name; span = Span.single ti.index } : Surface.Var.t)
    | _ -> fail p
  ;;

  let colon p =
    let ti = token p in
    match ti.token with
    | Colon -> ti
    | _ -> fail p
  ;;

  let equal_sign p =
    let ti = token p in
    match ti.token with
    | Equal -> ti
    | _ -> fail p
  ;;

  let colon_equal p =
    let ti = token p in
    match ti.token with
    | Colon_equal -> ti
    | _ -> fail p
  ;;
end

exception Error of Error.t

module State = struct
  type t =
    { mutable errors : Error.t list
    ; tokens : Token.t array
    }

  let create tokens = { errors = []; tokens }
  let error (_ : t) p s = raise_notrace (Error (Error.token s (Parser.State.curr_pos p)))
end

let error e = raise_notrace (Error e)

let rec parse_root st (root : Shrub.root) : Surface.expr =
  match root with
  | None -> error (Error.create "Empty root" Span.empty)
  | Some group -> parse_expr_group st group

and parse_expr_group st (group : Shrub.group) : Surface.expr =
  let p = Parser.State.create group in
  let expr = parse_expr st p in
  if not (Parser.State.is_empty p)
  then
    error
      (Error.token "Unconsumed tokens when parsing expression" (Parser.State.curr_pos p));
  expr

and parse_expr st (p : Parser.State.t) : Surface.expr = parse_fun st p

and parse_fun st (p : Parser.State.t) : Surface.expr =
  match Parser.State.peek p with
  | Some (Token { token = Ident "fun"; index = keyword_index }) ->
    let _ = Parser.State.next_exn p in
    let params =
      Parser.run_or_thunk
        p
        (fun p -> Parser.some p (fun () -> parse_param st p))
        (fun () -> State.error st p "Expected a function parameter")
    in
    let ret_ty = Parser.run p (fun p -> parse_annotation_cont st p) in
    let _ =
      Parser.run_or_thunk
        p
        (fun p -> Parser.operator p "->")
        (fun () -> State.error st p "Expected arrow")
    in
    let body = parse_expr st p in
    Expr_abs
      { params
      ; ret_ty
      ; body
      ; span = Span.combine (Span.single keyword_index) (Surface.expr_span body)
      }
  | _ -> parse_fun_ty st p

and parse_fun_ty st (p : Parser.State.t) : Surface.expr =
  let ty = parse_keyword st p in
  parse_fun_ty_cont st p [ ty ]

and parse_fun_ty_cont st p (tys : _ Non_empty_list.t) : Surface.expr =
  match Parser.State.peek p with
  | Some (Token { token = Operator "->"; index = _ }) ->
    let _ = Parser.State.next_exn p in
    let ty = parse_keyword st p in
    parse_fun_ty_cont st p (Non_empty_list.cons ty tys)
  | _ ->
    let param_tys, ret_ty = List.rev (Non_empty_list.tl tys), Non_empty_list.hd tys in
    (match Non_empty_list.of_list param_tys with
     | None -> ret_ty
     | Some param_tys ->
       let span =
         Span.combine
           (Surface.expr_span (Non_empty_list.hd param_tys))
           (Surface.expr_span ret_ty)
       in
       let param_tys = Non_empty_list.map param_tys ~f:(parse_param_ty_from_expr st) in
       Surface.Expr_ty_fun { param_tys; body_ty = ret_ty; span })

and parse_param_ty_from_expr st (expr : Surface.expr) : Surface.param_ty =
  match expr with
  | Expr_paren { e = Expr_ann { e; ty; span = _ }; span } ->
    let vars = parse_expr_vars st e in
    { vars; ty = Some ty; icit = Expl; span }
  | Expr_brack { e = Expr_ann { e; ty; span = _ }; span } ->
    let vars = parse_expr_vars st e in
    { vars; ty = Some ty; icit = Impl; span }
  | Expr_brack { e; span } ->
    let vars = parse_expr_vars st e in
    { vars; ty = None; icit = Impl; span }
  | e -> { vars = []; ty = Some e; icit = Expl; span = Surface.expr_span e }

and parse_expr_vars _st (e : Surface.expr) : Surface.Var.t list =
  let vars =
    match e with
    | Expr_app { func; args; span = _ } -> func :: args
    | Expr_var _ -> [ e ]
    | _ -> error (Error.create "invalid parameter syntax" (Surface.expr_span e))
  in
  let vars =
    List.map vars ~f:(fun var ->
      match var with
      | Expr_var var -> var
      | _ -> error (Error.create "invalid parameter syntax" (Surface.expr_span var)))
  in
  vars

and parse_keyword st (p : Parser.State.t) : Surface.expr =
  let e =
    match Parser.State.peek p with
    | Some (Token { token = Ident keyword; index = keyword_index }) -> begin
      match keyword with
      | "mod" ->
        let _ = Parser.State.next_exn p in
        let block =
          Parser.run_or_thunk
            p
            (fun p -> Parser.brace p)
            (fun () -> State.error st p "Expected {")
        in
        let decls =
          List.map block.groups ~f:(fun { Shrub.group; sep = _ } ->
            parse_block_decl st group)
        in
        let span =
          Span.combine (Span.single keyword_index) (Span.single block.rdelim.index)
        in
        Surface.Expr_mod { decls; span }
      | "sig" ->
        let _ = Parser.State.next_exn p in
        let block =
          Parser.run_or_thunk
            p
            (fun p -> Parser.brace p)
            (fun () -> State.error st p "Expected {")
        in
        let ty_decls =
          List.map block.groups ~f:(fun { Shrub.group; sep = _ } ->
            parse_sig_decl st group)
        in
        let span =
          Span.combine (Span.single keyword_index) (Span.single block.rdelim.index)
        in
        Surface.Expr_ty_mod { ty_decls; span }
      | "alias" ->
        let _ = Parser.State.next_exn p in
        let e = parse_atom st p in
        Expr_alias { e; span = Surface.expr_span e }
      | "pack" ->
        let _ = Parser.State.next_exn p in
        let e = parse_atom st p in
        Expr_pack
          { e; span = Span.combine (Span.single keyword_index) (Surface.expr_span e) }
      | "Pack" ->
        let _ = Parser.State.next_exn p in
        let ty = parse_atom st p in
        Expr_ty_pack
          { ty; span = Span.combine (Span.single keyword_index) (Surface.expr_span ty) }
      | "rec" ->
        let _ = Parser.State.next_exn p in
        let block =
          Parser.run_or_thunk
            p
            (fun p -> Parser.brace p)
            (fun () -> State.error st p "Expected {")
        in
        let decls =
          List.map block.groups ~f:(fun { Shrub.group; sep = _ } ->
            match parse_block_decl st group with
            | Block_decl_let decl -> decl
            | _ ->
              error
                (Error.token
                   "Expected let declaration in rec block"
                   (Shrub.Group.first_token group).index))
        in
        let span =
          Span.combine (Span.single keyword_index) (Span.single block.rdelim.index)
        in
        Expr_rec { decls; span }
      | _ -> parse_app st p
    end
    | _ -> parse_app st p
  in
  parse_keyword_cont st p e

and parse_keyword_cont st p (e : Surface.expr) : Surface.expr =
  match Parser.State.peek p with
  | Some (Token { token = Ident "where"; _ }) ->
    let _ = Parser.State.next_exn p in
    let block =
      Parser.run_or_thunk
        p
        (fun p -> Parser.brace p)
        (fun () -> State.error st p "Expected { after where")
    in
    let patches =
      List.map block.groups ~f:(fun { Shrub.group; sep = _ } ->
        parse_where_patch st group)
    in
    let span = Span.combine (Surface.expr_span e) (Span.single block.rdelim.index) in
    parse_keyword_cont st p (Surface.Expr_where { e; patches; span })
  | _ -> e

and parse_where_patch st (group : Shrub.group) : Surface.where_patch =
  let p = Parser.State.create group in
  let first_var =
    Parser.run_or_thunk
      p
      (fun p -> Parser.var p)
      (fun () ->
         error
           (Error.token "Expected variable name in where patch" (Parser.State.curr_pos p)))
  in
  let path : _ Non_empty_list.t = first_var.name :: parse_dot_path p in
  let _ =
    Parser.run_or_thunk
      p
      (fun p -> Parser.colon_equal p)
      (fun () ->
         error (Error.token "Expected := in where patch" (Parser.State.curr_pos p)))
  in
  let rhs_items = Parser.State.take p in
  let rhs =
    match Non_empty_list.of_list rhs_items with
    | None -> error (Error.token "Expected expression after :=" first_var.span.start)
    | Some group -> parse_expr_group st group
  in
  let span = Span.combine first_var.span (Surface.expr_span rhs) in
  { path; rhs; span }

and parse_dot_path (p : Parser.State.t) : string list =
  match Parser.State.peek p with
  | Some (Token { token = Dot; _ }) ->
    Parser.State.next_exn p;
    (match Parser.State.peek p with
     | Some (Token { token = Ident name; _ }) ->
       Parser.State.next_exn p;
       name :: parse_dot_path p
     | _ -> error (Error.token "Expected identifier after dot" (Parser.State.curr_pos p)))
  | _ -> []

and parse_mod_decl st (group : Shrub.group) : Surface.decl =
  let p = Parser.State.create group in
  match Parser.State.peek p with
  | Some (Token { token = Ident "let"; index = let_index }) ->
    Parser.State.next_exn p;
    let var =
      Parser.run_or_thunk
        p
        (fun p -> Parser.var p)
        (fun () -> error (Error.token "Expected variable name" (Parser.State.curr_pos p)))
    in
    let ann = Parser.run p (fun p -> parse_annotation_cont st p) in
    let is_alias =
      match Parser.State.peek p with
      | Some (Token { token = Equal; _ }) ->
        Parser.State.next_exn p;
        false
      | Some (Token { token = Colon_equal; _ }) ->
        Parser.State.next_exn p;
        true
      | _ -> error (Error.token "Expected = or :=" (Parser.State.curr_pos p))
    in
    let e = parse_expr st p in
    if not (Parser.State.is_empty p)
    then
      error
        (Error.token "Unconsumed tokens in module declaration" (Parser.State.curr_pos p));
    let span = Span.combine (Span.single let_index) (Surface.expr_span e) in
    { var; ann; is_alias; e; span }
  | _ ->
    error
      (Error.token "Expected module declaration" (Shrub.Group.first_token group).index)

and parse_sig_decl st (group : Shrub.group) : Surface.ty_decl =
  let p = Parser.State.create group in
  match Parser.State.peek p with
  | Some (Token { token = Ident "let"; index = let_index }) ->
    Parser.State.next_exn p;
    let var =
      Parser.run_or_thunk
        p
        (fun p -> Parser.var p)
        (fun () -> error (Error.token "Expected variable name" (Parser.State.curr_pos p)))
    in
    let ty =
      Parser.run_or_thunk
        p
        (fun p -> parse_annotation_cont st p)
        (fun () -> error (Error.token "Expected :" (Parser.State.curr_pos p)))
    in
    if not (Parser.State.is_empty p)
    then
      error
        (Error.token
           "Unconsumed tokens in signature declaration"
           (Parser.State.curr_pos p));
    let span = Span.combine (Span.single let_index) (Surface.expr_span ty) in
    { var; ty; span }
  | _ ->
    error
      (Error.token "Expected signature declaration" (Shrub.Group.first_token group).index)

and parse_param st (p : Parser.t) : Surface.param =
  match Parser.next p with
  | Delim delim -> parse_paren_param st delim
  | Token { token = Ident var; index = var_index } ->
    let span = Span.single var_index in
    let var = Surface.Var.create var span in
    { vars = [ var ]; ann = None; icit = Expl; span }
  | _ -> Parser.fail p

and parse_paren_param st (delim : Shrub.item_delim) : Surface.param =
  let group =
    match delim.groups with
    | [ { group; sep = None } ] -> group
    | _ ->
      error
        (Error.token "Invalid parameter syntax, did not expect comma" delim.ldelim.index)
  in
  let p = Parser.State.create group in
  let icit =
    match delim.ldelim.token with
    | LParen -> Surface.Icit.Expl
    | LBrack -> Impl
    | _ -> error (Error.token "invalid param syntax" delim.ldelim.index)
  in
  let vars =
    Parser.run_or_thunk
      p
      (fun p -> Parser.some p (fun () -> Parser.var p))
      (fun () -> error (Error.token "Expected parameter name" (Parser.State.curr_pos p)))
  in
  let ann_span, ann =
    Parser.run p (fun p -> parse_annotation_cont st p)
    |> Option.map ~f:(fun e -> Surface.expr_span e, Some e)
    |> Option.value ~default:((Non_empty_list.hd vars).span, None)
  in
  if not (Parser.State.is_empty p)
  then error (Error.token "Unconsumed tokens in parameters" (Parser.State.curr_pos p));
  { vars; ann; span = Span.combine (Non_empty_list.hd vars).span ann_span; icit }

and parse_annotation_cont st (p : Parser.t) : Surface.expr =
  let _ = Parser.colon p in
  parse_expr st (Parser.state p)

and parse_app st (p : Parser.State.t) : Surface.expr =
  let func = parse_dot st p in
  let args = Parser.many p (fun p -> parse_dot_fail st p) in
  match args with
  | [] -> func
  | _ ->
    Expr_app
      { func
      ; args
      ; span =
          Span.combine (Surface.expr_span func) (Surface.expr_span (List.last_exn args))
      }

and parse_dot st (p : Parser.State.t) : Surface.expr =
  Parser.run_or_thunk
    p
    (fun p -> parse_dot_fail st p)
    (fun () -> State.error st p "Expected expression")

and parse_dot_fail st (p : Parser.t) : Surface.expr =
  let expr = parse_atom_fail st p in
  parse_dot_cont st (Parser.state p) expr

and parse_dot_cont st (p : Parser.State.t) (expr : Surface.expr) : Surface.expr =
  match Parser.State.peek p with
  | Some (Token { token = Dot; index = dot_index }) ->
    Parser.State.next_exn p;
    let ident_index, ident =
      Parser.run_or_thunk
        p
        (fun p ->
           let tok = Parser.token p in
           match tok.token with
           | Ident name -> tok.index, name
           | _ -> Parser.fail p)
        (fun () -> error (Error.token "Expected identifier after dot token" dot_index))
    in
    parse_dot_cont
      st
      p
      (Expr_proj
         { mod_e = expr
         ; field = ident
         ; span = Span.combine (Surface.expr_span expr) (Span.single ident_index)
         })
  | _ -> expr

and parse_atom_fail st (p : Parser.t) : Surface.expr =
  match Parser.next p with
  | Delim delim -> begin
    match delim.ldelim.token with
    | LBrace -> parse_block st delim
    | LParen -> parse_paren st delim
    | LBrack -> parse_brack st delim
    | _ -> error (Error.token "Unexpected delimiter" delim.ldelim.index)
  end
  | Token { token = Ident ident; index } -> begin
    let span = Span.single index in
    match ident with
    | "where" -> Parser.fail p
    | "Bool" -> Expr_core_ty { ty = Bool; span }
    | "Int" -> Expr_core_ty { ty = Int; span }
    | "Unit" -> Expr_core_ty { ty = Unit; span }
    | "Type" -> Expr_universe { universe = Universe.type_; span }
    | "Kind" -> Expr_universe { universe = Universe.kind_; span }
    | "#t" -> Expr_literal { literal = Bool true; span }
    | "#f" -> Expr_literal { literal = Bool false; span }
    | _ -> Expr_var { name = ident; span }
  end
  | Token { token = Number n; index } ->
    Expr_literal
      { literal =
          Int
            (Int.of_string_opt n
             |> Option.value_or_thunk ~default:(fun () ->
               error (Error.token "Invalid number" index)))
      ; span = Span.single index
      }
  | Token { token = String s; index } ->
    Expr_literal { literal = String s; span = Span.single index }
  | _ -> Parser.fail p

and parse_atom st (p : Parser.State.t) : Surface.expr =
  Parser.run_or_thunk
    p
    (fun p -> parse_atom_fail st p)
    (fun () -> error (Error.token "Expected atom" (Parser.State.curr_pos p)))

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
  let p = Parser.State.create group in
  match Parser.State.peek p with
  | Some (Token { token = Ident "let"; index = let_index }) ->
    Parser.State.next_exn p;
    let var =
      Parser.run_or_thunk
        p
        (fun p -> Parser.var p)
        (fun () -> error (Error.token "Expected variable name" (Parser.State.curr_pos p)))
    in
    let ann = Parser.run p (fun p -> parse_annotation_cont st p) in
    let is_alias =
      match Parser.State.peek p with
      | Some (Token { token = Equal; _ }) ->
        Parser.State.next_exn p;
        false
      | Some (Token { token = Colon_equal; _ }) ->
        Parser.State.next_exn p;
        true
      | _ -> error (Error.token "Expected = or :=" (Parser.State.curr_pos p))
    in
    let rhs_items = Parser.State.take p in
    let rhs =
      match Non_empty_list.of_list rhs_items with
      | None -> error (Error.token "Expected expression after =" let_index)
      | Some group -> parse_expr_group st group
    in
    let span = Span.combine (Span.single let_index) (Surface.expr_span rhs) in
    Block_decl_let { var; ann; is_alias; rhs; span }
  | Some (Token { token = Ident "bind"; index = bind_index }) ->
    Parser.State.next_exn p;
    let var =
      Parser.run_or_thunk
        p
        (fun p -> Parser.var p)
        (fun () -> error (Error.token "Expected variable name" (Parser.State.curr_pos p)))
    in
    let _ =
      Parser.run_or_thunk
        p
        (fun p -> Parser.equal_sign p)
        (fun () -> error (Error.token "Expected =" (Parser.State.curr_pos p)))
    in
    let rhs_items = Parser.State.take p in
    let rhs =
      match Non_empty_list.of_list rhs_items with
      | None -> error (Error.token "Expected expression after =" bind_index)
      | Some group -> parse_expr_group st group
    in
    let span = Span.combine (Span.single bind_index) (Surface.expr_span rhs) in
    Block_decl_bind { var; rhs; span }
  | _ ->
    let e = parse_expr_group st group in
    let span = Surface.expr_span e in
    Block_decl_expr { e; span }

and parse_expr_ann st (p : Parser.State.t) : Surface.expr =
  let e = parse_expr st p in
  let ann = Parser.run p (fun p -> parse_annotation_cont st p) in
  match ann with
  | Some ty ->
    Surface.Expr_ann
      { e; ty; span = Span.combine (Surface.expr_span e) (Surface.expr_span ty) }
  | None -> e

and parse_paren st (parens : Shrub.item_delim) : Surface.expr =
  match parens.groups with
  | [] ->
    let span =
      Span.combine (Span.single parens.ldelim.index) (Span.single parens.rdelim.index)
    in
    Expr_literal { literal = Unit; span }
  | [ { group; sep = _ } ] ->
    let p = Parser.State.create group in
    (match Parser.State.peek p with
     | Some (Token { token = Equal; _ }) ->
       Parser.State.next_exn p;
       let identity = parse_expr_ann st p in
       Surface.Expr_ty_sing
         { identity
         ; span =
             Span.combine
               (Span.single parens.ldelim.index)
               (Span.single parens.rdelim.index)
         }
     | _ ->
       let e = parse_expr_ann st p in
       Surface.Expr_paren { e; span = Surface.expr_span e })
  | _ ->
    error (Error.token "Unexpected comma in parenthesized expression" parens.ldelim.index)

and parse_brack st (brack : Shrub.item_delim) : Surface.expr =
  match brack.groups with
  | [ { group; sep = _ } ] ->
    let p = Parser.State.create group in
    let e = parse_expr_ann st p in
    Surface.Expr_brack { e; span = Surface.expr_span e }
  | _ -> error (Error.token "Invalid bracket expression" brack.ldelim.index)
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
