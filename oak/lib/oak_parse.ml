open Prelude
module Shrub = Shrubbery.Syntax
module Token = Shrubbery.Token
module Fail = Utility.Fail
module Syntax = Oak_syntax
module Span = Location.Span
module Spanned = Location.Spanned
module Diagnostic = Oak_diagnostic

module Error = struct
  type t = string Spanned.t [@@deriving sexp_of]

  let create error token = { Spanned.value = error; span = Span.single token }
end

module State = struct
  type t =
    { mutable errors : Error.t list
    ; next_non_trivia : int array Lazy.t
    ; tokens : Token.t array
    }

  let add_error t e = t.errors <- e :: t.errors

  let create tokens =
    { errors = []
    ; tokens
    ; next_non_trivia = lazy (Token.calculate_next_non_trivia tokens)
    }
  ;;

  let next_non_trivia t index = (Lazy.force t.next_non_trivia).(index)
end

exception Error of Error.t

let ( let@ ) f k = f ~f:k

module Match = struct
  include Shrubbery.Match

  let builtin h name item =
    let token = Item.token h item in
    Shrubbery.Match.Token.equals h (Ident name) token;
    token
  ;;

  let ident h item =
    let token = Item.token h item in
    match token.token with
    | Ident name -> name
    | _ -> Fail.fail h
  ;;

  let var h item =
    let token = Item.token h item in
    match token.token with
    | Ident name -> Syntax.Var.create_initial name token.index
    | _ -> Fail.fail h
  ;;

  let builtin_operator h name item =
    let token = Item.token h item in
    Shrubbery.Match.Token.equals h (Operator name) token;
    token
  ;;
end

module Items = struct
  type t =
    { items : Shrub.item Fail.List.t
    ; last_index : int
    }

  let peek env t = Fail.List.peek env t.items

  let get_span t =
    Fail.run_or_thunk
      ~f:(fun h -> (Shrub.Item.first_token (peek h t)).index)
      ~default:(fun () -> t.last_index)
  ;;

  let next env t = Fail.List.next env t.items
  let take t = Fail.List.take t.items
  let empty env t = Fail.List.empty env t.items

  let create items =
    { items = Fail.List.create (Non_empty_list.to_list items)
    ; last_index = Shrub.Item.span (Non_empty_list.last items) |> Span.stop
    }
  ;;

  let optional t f = Fail.List.optional t.items f
  let either t f g = Fail.List.either t.items f g
  let many_rev t f = Fail.List.many_rev t.items f
  let guard env t b = Fail.List.guard env t.items b
  let many t f = Fail.List.many t.items f
  let some_rev t f = Fail.List.some_rev t.items f
  let some t f = Fail.List.some t.items f
  let one_of t fs = Fail.List.one_of t.items fs
  let run t ~f = Fail.List.run t.items ~f
  let run_or_thunk t ~default ~f = Fail.List.run_or_thunk t.items ~default ~f
  let run_exn t ~f = Fail.List.run_exn t.items ~f
end

let error e = raise_notrace (Error e)
let run_or_error e ~f = Fail.run_or_thunk ~f ~default:(fun () -> error (Lazy.force e))

let cut e ~f =
  match Fail.optional f with
  | None -> error (Lazy.force e)
  | Some x -> x
;;

let cut_list l e ~f =
  match Items.optional l f with
  | None -> error (Lazy.force e)
  | Some x -> x
;;

let rec parse_root st (block : Shrub.block) : Syntax.expr =
  let group, groups =
    match block.groups with
    | group :: groups -> group, groups
    | [] ->
      error (Spanned.create "Root should have exactly one group" (Shrub.Block.span block))
  in
  List.hd groups
  |> Option.iter ~f:(fun group ->
    error
      (Error.create
         "Root cannot have more than one group"
         (Shrub.Group.first_token group.group).index));
  parse_expr_group st group.group

and parse_decl st (group : Shrub.group) : Syntax.expr_decl =
  run_or_error
    (lazy (Error.create "Invalid declaration" (Shrub.Group.first_token group).index))
    ~f:(fun h -> Fail.one_of [ (fun () -> parse_let_decl st h group) ])

and parse_expr_group st (group : Shrub.group) : Syntax.expr =
  let@ h = Fail.run_or_thunk ~default:(fun () -> parse_simple_expr st group) in
  Fail.one_of
    [ (fun () -> parse_mod_expr st h group)
    ; (fun () -> parse_do_expr st h group)
    ; (fun () -> parse_fun_expr st h group)
    ; (fun () -> parse_sig_expr st h group)
    ]

and parse_do_expr st h (group : Shrub.group) : Syntax.expr =
  let items = Items.create group.items in
  let do_tok = Items.next h items |> Match.builtin h "do" in
  let h = () in
  ignore h;
  let block =
    Option.value_or_thunk group.block ~default:(fun () ->
      error (Error.create "Expected block after do" do_tok.index))
  in
  if not (List.is_empty group.alts)
  then error (Error.create "Do expression should not have any alternatives" do_tok.index);
  parse_expr_token_block st block

and parse_fun_expr st h (group : Shrub.group) : Syntax.expr =
  let items = Items.create group.items in
  let item = Items.next h items in
  let fun_tok, purity =
    Fail.one_of
      [ (fun () -> Match.builtin h "fun" item, Syntax.Purity.Pure)
      ; (fun () -> Match.builtin h "funct" item, Syntax.Purity.Impure)
      ]
  in
  (* Commit: no more Failures after matching fun/funct *)
  let h = () in
  ignore h;
  let params_item =
    run_or_error
      ~f:(fun h -> Items.next h items |> Match.Item.tree h)
      (lazy (Error.create "Expected parameter list after fun/funct" fun_tok.index))
  in
  let params =
    List.map params_item.groups ~f:(fun param_group ->
      let param_items = Items.create param_group.group.items in
      let var =
        run_or_error
          ~f:(fun h -> Match.var h (Items.next h param_items))
          (lazy (Error.create "Expected parameter name" (Items.get_span param_items)))
      in
      let ty = parse_atom_expr st param_items in
      Items.take param_items
      |> List.hd
      |> Option.iter ~f:(fun item ->
        error
          (Error.create
             "Unconsumed tokens in parameter"
             (Shrub.Item.first_token item).index));
      let span =
        Span.combine (Span.single (Option.value_exn var.pos)) (Syntax.expr_span ty)
      in
      ({ var; ty; span } : Syntax.expr_param))
  in
  (* Optional return type annotation - only try if there are remaining items *)
  let return_ty =
    Items.run_or_thunk
      items
      ~f:(fun h ->
        let _ = Items.peek h items in
        Some (parse_atom_expr st items))
      ~default:(fun () -> None)
  in
  let block =
    Option.value_or_thunk group.block ~default:(fun () ->
      error (Error.create "Expected block after fun parameters" fun_tok.index))
  in
  if not (List.is_empty group.alts)
  then
    error (Error.create "Fun expression should not have any alternatives" fun_tok.index);
  let body = parse_expr_token_block st block in
  let span = Span.combine (Span.single fun_tok.index) (Syntax.expr_span body) in
  match return_ty with
  | None -> Syntax.Expr_abs { params; body; purity; span }
  | Some ty ->
    Syntax.Expr_abs
      { params
      ; body = Syntax.Expr_seal { e = body; ty; span = Syntax.expr_span body }
      ; purity
      ; span
      }

and parse_mod_expr st h (group : Shrub.group) : Syntax.expr =
  let items = Items.create group.items in
  let mod_tok = Items.next h items |> Match.builtin h "mod" in
  let h = () in
  ignore h;
  let block =
    Option.value_or_thunk group.block ~default:(fun () ->
      error (Error.create "Expected block after mod" mod_tok.index))
  in
  if not (List.is_empty group.alts)
  then
    error (Error.create "Mod expression should not have any alternatives" mod_tok.index);
  let decls = List.map block.block.groups ~f:(fun group -> parse_decl st group.group) in
  let var = Syntax.Mod_var.create_initial mod_tok.index in
  let span =
    List.last decls
    |> Option.map ~f:(fun decl -> Span.combine (Span.single mod_tok.index) decl.span)
    |> Option.value
         ~default:
           (Span.combine (Span.single mod_tok.index) (Span.single block.token.index))
  in
  Syntax.Expr_mod { var; decls; span }

and parse_sig_expr st h (group : Shrub.group) : Syntax.expr =
  let items = Items.create group.items in
  let sig_tok = Items.next h items |> Match.builtin h "sig" in
  let h = () in
  ignore h;
  Items.take items
  |> List.hd
  |> Option.iter ~f:(fun item ->
    error (Error.create "Unexpected tokens after sig" (Shrub.Item.first_token item).index));
  let block =
    Option.value_or_thunk group.block ~default:(fun () ->
      error (Error.create "Expected block after sig" sig_tok.index))
  in
  if not (List.is_empty group.alts)
  then
    error (Error.create "Sig expression should not have any alternatives" sig_tok.index);
  let ty_decls =
    List.map block.block.groups ~f:(fun group -> parse_ty_decl st group.group)
  in
  let var = Syntax.Mod_var.create_initial sig_tok.index in
  let span =
    List.last ty_decls
    |> Option.map ~f:(fun decl -> Span.combine (Span.single sig_tok.index) decl.span)
    |> Option.value
         ~default:
           (Span.combine (Span.single sig_tok.index) (Span.single block.token.index))
  in
  Syntax.Expr_ty_mod { var; ty_decls; span }

and parse_ty_decl st (group : Shrub.group) : Syntax.expr_ty_decl =
  run_or_error
    (lazy (Error.create "Invalid type declaration" (Shrub.Group.first_token group).index))
    ~f:(fun h -> parse_let_ty_decl st h group)

and parse_let_ty_decl st h (group : Shrub.group) : Syntax.expr_ty_decl =
  let items = Items.create group.items in
  let let_tok = Match.builtin h "let" (Items.next h items) in
  let h = () in
  ignore h;
  let (field_tok : Token.ti), (field : string) =
    let@ h =
      run_or_error (lazy (Error.create "Expected field name after let" let_tok.index))
    in
    let tok = Match.Item.token h (Items.next h items) in
    let var = Match.Token.ident h tok in
    tok, var
  in
  let ty = parse_atom_expr st items in
  Items.take items
  |> List.hd
  |> Option.iter ~f:(fun item ->
    error
      (Error.create
         "Unconsumed tokens in type declaration"
         (Shrub.Item.first_token item).index));
  let span = Span.combine (Span.single let_tok.index) (Syntax.expr_span ty) in
  ({ field; field_pos = field_tok.index; ty; span } : Syntax.expr_ty_decl)

and parse_simple_expr_rec st (items : Items.t) : Syntax.expr =
  let@ h =
    run_or_error (lazy (error (Error.create "Invalid expression" (Items.get_span items))))
  in
  let atom = Items.one_of items [ (fun () -> parse_atom_expr st items) ] in
  parse_expr_right_cont st atom items

and parse_expr_right_cont st expr (items : Items.t) : Syntax.expr =
  let@ h = Items.run_or_thunk items ~default:(Fn.const expr) in
  Items.one_of
    items
    [ (fun () ->
        let _ = Match.builtin_operator h "::" (Items.next h items) in
        let ty = parse_atom_expr st items in
        Syntax.Expr_seal
          { e = expr
          ; ty
          ; span = Span.combine (Syntax.expr_span expr) (Syntax.expr_span ty)
          })
    ]

and parse_atom_expr st (items : Items.t) : Syntax.expr =
  let base = parse_base_atom_expr st items in
  (* Check for function application: base(args) *)
  parse_atom_app_cont st base items

(* Parse a base atom expression without application continuation *)
and parse_base_atom_expr st (items : Items.t) : Syntax.expr =
  let@ h =
    run_or_error (lazy (Error.create "expected atom expression" (Items.get_span items)))
  in
  Items.one_of
    items
    [ (fun () -> parse_lit st h items)
    ; (fun () -> parse_delimited_expr st h items)
    ; (fun () -> parse_fun_ty st h items)
    ; (fun () -> parse_var_expr st h items)
    ]

and parse_atom_app_cont st func (items : Items.t) : Syntax.expr =
  let app_opt =
    Items.run_or_thunk
      items
      ~f:(fun h ->
        let args_item = Items.next h items |> Match.Item.tree h in
        let args =
          List.map args_item.groups ~f:(fun group -> parse_expr_group st group.group)
        in
        let span =
          Span.combine (Syntax.expr_span func) (Span.single args_item.rdelim.index)
        in
        Some (Syntax.Expr_app { func; args; span }))
      ~default:(fun () -> None)
  in
  match app_opt with
  | Some app -> parse_atom_app_cont st app items
  | None -> func

and parse_fun_ty st h items : Syntax.expr =
  let item = Items.next h items in
  let fun_tok, purity =
    Fail.one_of
      [ (fun () -> Match.builtin h "Fun" item, Syntax.Purity.Pure)
      ; (fun () -> Match.builtin h "Funct" item, Syntax.Purity.Impure)
      ]
  in
  (* Commit: no more Failures after matching Fun/Funct *)
  let h = () in
  ignore h;
  let params_item =
    run_or_error
      ~f:(fun h -> Items.next h items |> Match.Item.tree h)
      (lazy (Error.create "Expected parameter list after Fun/Funct" fun_tok.index))
  in
  let params =
    List.map params_item.groups ~f:(fun param_group ->
      let param_items = Items.create param_group.group.items in
      (* Use parse_base_atom_expr to avoid consuming parentheses as function application *)
      let first_atom = parse_base_atom_expr st param_items in
      (* Check if there's another token after the first atom *)
      let has_more =
        Fail.run_or_thunk
          ~f:(fun h ->
            let _ = Items.peek h param_items in
            true)
          ~default:(fun () -> false)
      in
      let var, ty =
        if has_more
        then begin
          (* first_atom is the variable name, parse the type *)
          let var =
            match first_atom with
            | Syntax.Expr_var { var = Var v; _ } -> v
            | _ ->
              error
                (Error.create
                   "Expected variable name"
                   (Syntax.expr_span first_atom).start)
          in
          let ty = parse_base_atom_expr st param_items in
          var, ty
        end
        else begin
          (* first_atom is the type, no variable binding *)
          let var = Syntax.Var.create_initial "_" (Syntax.expr_span first_atom).start in
          var, first_atom
        end
      in
      Items.take param_items
      |> List.hd
      |> Option.iter ~f:(fun item ->
        error
          (Error.create
             "Unconsumed tokens in parameter"
             (Shrub.Item.first_token item).index));
      let span =
        Span.combine (Span.single (Option.value_exn var.pos)) (Syntax.expr_span ty)
      in
      ({ var; ty; span } : Syntax.expr_param))
  in
  let body_ty = parse_atom_expr st items in
  let span = Span.combine (Span.single fun_tok.index) (Syntax.expr_span body_ty) in
  Syntax.Expr_ty_fun { params; body_ty; purity; span }

(* a simple expr is an expr without a block or alternatives *)
and parse_simple_expr st group : Syntax.expr =
  Option.iter group.block ~f:(fun block ->
    error (Error.create "Expression should not have block" block.token.index));
  List.hd group.alts
  |> Option.iter ~f:(fun alt ->
    error (Error.create "Expression should not have alt" alt.token.index));
  parse_simple_expr_items st group.items

and parse_simple_expr_items st (items : Shrub.item Non_empty_list.t) : Syntax.expr =
  let items = Items.create items in
  let expr = parse_simple_expr_rec st items in
  let items_left = Items.take items in
  List.hd items_left
  |> Option.iter ~f:(fun item ->
    error
      (Error.create "Unconsumed tokens in expression" (Shrub.Item.first_token item).index));
  expr

and parse_delimited_expr st h items : Syntax.expr =
  let delim_item = Items.next h items |> Match.Item.tree h in
  let h = () in
  ignore h;
  match delim_item.groups with
  | [] ->
    Syntax.Expr_unit
      { span =
          Span.combine
            (Span.single delim_item.ldelim.index)
            (Span.single delim_item.rdelim.index)
      }
  | [ group ] -> parse_expr_group st group.group
  | _g1 :: g2 :: _ ->
    error
      (Error.create
         "Parenthesized expression with multiple expressions"
         (Shrub.Group.first_token g2.group).index)

and parse_lit st h items : Syntax.expr =
  let item = Items.next h items in
  Fail.one_of
    [ (fun () ->
        let tok = Match.builtin h "Int" item in
        Syntax.Expr_core_ty { ty = Ty_int; span = Span.single tok.index })
    ; (fun () ->
        let tok = Match.builtin h "Bool" item in
        Syntax.Expr_core_ty { ty = Ty_bool; span = Span.single tok.index })
    ; (fun () ->
        let tok = Match.builtin h "Unit" item in
        Syntax.Expr_core_ty { ty = Ty_unit; span = Span.single tok.index })
    ; (fun () ->
        let tok = Match.builtin h "Type" item in
        Syntax.Expr_universe { univ = Type; span = Span.single tok.index })
    ; (fun () ->
        let tok = Match.builtin h "Kind" item in
        Syntax.Expr_universe { univ = Kind; span = Span.single tok.index })
    ; (fun () ->
        let tok = Match.builtin h "Sig" item in
        Syntax.Expr_universe { univ = Sig; span = Span.single tok.index })
    ; (fun () -> parse_int_expr st h item)
    ]

and parse_int_expr _st h (item : Shrub.item) : Syntax.expr =
  let num_tok = Match.Item.token h item in
  let num = Token.number_val num_tok.token |> Fail.unwrap h in
  let@ () = cut (lazy (Error.create "Invalid number" num_tok.index)) in
  let num = Int.of_string_opt num |> Fail.unwrap h in
  Syntax.Expr_int { value = num; span = Span.single num_tok.index }

and parse_var_expr _st h items : Syntax.expr =
  let item = Items.next h items in
  let var = Match.var h item in
  Syntax.Expr_var { var = Var var; span = Span.single (Option.value_exn var.pos) }

and parse_expr_token_block st (block : Shrub.token_block) : Syntax.expr =
  parse_expr_block st block.block

and parse_expr_block st (block : Shrub.block) : Syntax.expr =
  let groups, last_group =
    match List.last block.groups with
    | None -> error (Error.create "Empty block" block.lbrace.index)
    | Some x -> List.drop_last_exn block.groups, x
  in
  let decls = List.map groups ~f:(fun g -> parse_decl st g.group) in
  let body = parse_expr_group st last_group.group in
  let expr =
    List.fold_right decls ~init:body ~f:(fun decl expr ->
      let var = Syntax.Var.create_initial decl.field decl.field_pos in
      let rhs = decl.e in
      let body = expr in
      Syntax.Expr_let { var; rhs; body; span = decl.span })
  in
  expr

and parse_let_decl st h (group : Shrub.group) : Syntax.expr_decl =
  let items = Items.create group.items in
  let let_tok = Match.builtin h "let" (Items.next h items) in
  let h = () in
  ignore h;
  let (field_tok : Token.ti), (field : string) =
    let@ h =
      run_or_error (lazy (Error.create "Expected variable after let" let_tok.index))
    in
    let tok = Match.Item.token h (Items.next h items) in
    let var = Match.Token.ident h tok in
    tok, var
  in
  let block =
    Option.value_or_thunk group.block ~default:(fun () ->
      error (Error.create "Expected block after variable" field_tok.index))
  in
  let body = parse_expr_token_block st block in
  let span = Span.combine (Span.single let_tok.index) (Syntax.expr_span body) in
  ({ let_pos = let_tok.index; field; field_pos = field_tok.index; e = body; span }
   : Syntax.expr_decl)
;;

let parse_block st root_block =
  match parse_root st root_block with
  | exception Error e ->
    ( [ e ]
    , Syntax.Expr_hole
        { span =
            Span.combine
              (Span.single root_block.lbrace.index)
              (Span.single root_block.rbrace.index)
        } )
  | x -> [], x
;;

let error_to_diagnostic ~file ~offsets (e : Error.t) : Diagnostic.t =
  let start = offsets.(e.span.start) in
  let stop = offsets.(e.span.stop) in
  { code = Some Parse_error
  ; parts =
      [ { kind = Error
        ; message = Diagnostic.Text.of_string e.value
        ; snippet = Some { file; start; stop }
        }
      ]
  }
;;

let shrub_error_to_diagnostic ~file (e : Shrubbery.Delimit.Error.t) : Diagnostic.t =
  (* shrub errors are byte positions *)
  let to_snippet ({ start; stop } : Span.t) : Diagnostic.Snippet.t =
    { file; start; stop }
  in
  match e with
  | Mismatching_delimiters { ldelim; rdelim } ->
    { code = Some Parse_error
    ; parts =
        [ { kind = Error
          ; message = Diagnostic.Text.of_string "mismatching delimiters"
          ; snippet = Some (to_snippet rdelim)
          }
        ; { kind = Note
          ; message = Diagnostic.Text.of_string "opening delimiter here"
          ; snippet = Some (to_snippet ldelim)
          }
        ]
    }
  | Expecting_delimiter span ->
    { code = Some Parse_error
    ; parts =
        [ { kind = Error
          ; message = Diagnostic.Text.of_string "expecting delimiter"
          ; snippet = Some (to_snippet span)
          }
        ]
    }
;;

let parse ~file s =
  let tts, block, shrub_errors = Shrubbery.Parser.parse s in
  let tokens = Shrubbery.Token_tree.Root.to_list tts |> Array.of_list in
  let offsets = Shrubbery.Token.calculate_offsets tokens in
  let st = State.create tokens in
  let errors, expr = parse_block st block in
  let diagnostics =
    List.map shrub_errors ~f:(shrub_error_to_diagnostic ~file)
    @ List.map errors ~f:(error_to_diagnostic ~file ~offsets)
  in
  tts, diagnostics, expr
;;
