open Core
module Shrub = Shrubbery.Syntax
module Token = Shrubbery.Token
module Fail = Utility.Fail
module Syntax = Oak_syntax

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
    | Ident name -> Syntax.Var.create_initial name token
    | _ -> Fail.fail h
  ;;
end

let error e = raise_notrace (Error (Lazy.force e))

let cut l e ~f =
  match Fail.List.optional l f with
  | None -> error e
  | Some x -> x
;;

let token_after (token : Token.ti) = { token with index = token.index + 1 }

let rec parse_decl (h : Fail.t) (group : Shrub.group) : Syntax.expr_decl =
  Fail.one_of
    [ (fun () -> parse_let_decl h group)
      (* ; (fun () -> error (lazy (Error.create "Invalid declaration" _))) *)
    ]

and parse_expr_block h block : Syntax.expr = failwith ""

and parse_let_decl h (group : Shrub.group) : Syntax.expr_decl =
  let@ items = Fail.List.create h (Utility.Non_empty_list.to_list group.items) in
  let let_tok = Match.builtin h "let" (next h items) in
  let@ () = Fail.cannot_fail in
  let field_tok, field =
    let@ () = cut items (lazy (Error.create "failed to parse let" (let_tok.index + 1))) in
    let tok = Match.Item.token h (next h items) in
    let var = Match.Token.ident h tok in
    tok, var
  in
  let block =
    let@ () = cut items (lazy (Error.create "didn't have block" (field_tok.index + 1))) in
    Fail.unwrap h group.block
  in
  let body = parse_expr_block h block in
  ({ field; e = body } : Syntax.expr_decl)
;;
