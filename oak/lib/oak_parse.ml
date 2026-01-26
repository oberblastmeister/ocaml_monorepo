open Prelude
module Shrub = Shrubbery.Syntax
module Token = Shrubbery.Token
module Fail = Utility.Fail
module Syntax = Oak_syntax
module Span = Location.Span

module Error = struct
  type t =
    { error : string
    ; token : int
    }
  [@@deriving sexp_of]

  let create error token = { error; token }
end

exception Error of Error.t

let next = Fail.List.next
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
end

let error e = raise_notrace (Error e)

let cut e ~f =
  match Fail.optional f with
  | None -> error (Lazy.force e)
  | Some x -> x
;;

let cut_list l e ~f =
  match Fail.List.optional l f with
  | None -> error (Lazy.force e)
  | Some x -> x
;;

let token_after (token : Token.ti) = { token with index = token.index + 1 }

let rec parse_decl (group : Shrub.group) : Syntax.expr_decl =
  Fail.run ~f:(fun h -> Fail.one_of [ (fun () -> parse_let_decl h group) ])
  |> Option.value_or_thunk ~default:(fun () ->
    error (Error.create "Invalid declaration" (Shrub.Group.first_token group).index))

and parse_expr_rec (items : Shrub.item Fail.List.t) : Syntax.expr =
  let@ h = Fail.run_exn in
  Fail.List.one_of
    items
    [ (fun () -> parse_lit h items)
    ; (fun () -> error (Error.create "Invalid expression" (failwith "")))
    ]

and parse_expr_group (group : Shrub.group) : Syntax.expr =
  let expr = parse_expr_items group.items in
  (* let items = Group.items *)
  failwith ""

and parse_expr_items (items : Shrub.item Non_empty_list.t) : Syntax.expr =
  let@ h = Fail.run_exn in
  let@ items = Fail.List.create h (Non_empty_list.to_list items) in
  let expr = parse_expr_rec items in
  let items_left = Fail.List.take items in
  if not (List.is_empty items_left)
  then
    error
      (Error.create
         "Invalid expression"
         (Shrub.Item.first_token (List.hd_exn items_left)).index);
  expr

and parse_lit h items : Syntax.expr =
  let item = Fail.List.next h items in
  Fail.one_of [ (fun () -> parse_int_expr h item); (fun () -> parse_unit_expr h item) ]

and parse_unit_expr h (group : Shrub.item) : Syntax.expr =
  (* let item = Non_empty_list.hd group.items |> Mathc *)
  failwith ""

and parse_int_expr h (item : Shrub.item) : Syntax.expr =
  let num_tok = Match.Item.token h item in
  let num = Token.number_val num_tok.token |> Fail.unwrap h in
  let@ () = cut (lazy (Error.create "Invalid number" num_tok.index)) in
  let num = Int.of_string_opt num |> Fail.unwrap h in
  Syntax.Expr_int { value = num; span = Span.single num_tok.index }

and parse_expr_token_block (block : Shrub.token_block) : Syntax.expr =
  parse_expr_block block.block

and parse_expr_block (block : Shrub.block) : Syntax.expr =
  let groups, last_group =
    match List.last block.groups with
    | None -> error (Error.create "Empty block" block.lbrace.index)
    | Some x -> List.drop_last_exn block.groups, x
  in
  let decls = List.map groups ~f:(fun g -> parse_decl g.group) in
  let body = parse_expr_group last_group.group in
  let expr =
    List.fold_right decls ~init:body ~f:(fun decl expr ->
      let var = Syntax.Var.create_initial decl.field decl.field_pos in
      let rhs = decl.e in
      let body = expr in
      Syntax.Expr_let { var; rhs; body; span = decl.span })
  in
  expr

and parse_let_decl h (group : Shrub.group) : Syntax.expr_decl =
  let let_tok, field_tok, field =
    let@ items = Fail.List.create h (Utility.Non_empty_list.to_list group.items) in
    let let_tok = Match.builtin h "let" (next h items) in
    let field_tok, field =
      let@ () =
        cut_list items (lazy (Error.create "failed to parse let" (let_tok.index + 1)))
      in
      let tok = Match.Item.token h (next h items) in
      let var = Match.Token.ident h tok in
      tok, var
    in
    let_tok, field_tok, field
  in
  let block =
    let@ () = cut (lazy (Error.create "didn't have block" (field_tok.index + 1))) in
    Fail.unwrap h group.block
  in
  let body = parse_expr_token_block block in
  let span = Span.combine (Span.single let_tok.index) (Syntax.expr_span body) in
  ({ let_pos = let_tok.index; field; field_pos = field_tok.index; e = body; span }
   : Syntax.expr_decl)
;;
