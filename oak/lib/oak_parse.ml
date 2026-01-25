open Core
module Shrub = Shrubbery.Syntax
module Token = Shrubbery.Token
module Fail = Utility.Fail

module Error = struct
  type t = { error : string; token : Token.ti } [@@deriving sexp_of]

  let create error token = { error; token }
end

exception Error of Error.t

let next = Fail.List.next

module Syntax = Oak_syntax

let ( let@ ) f k = f ~f:k

module Match = struct
  include Shrubbery.Match

  let ident h name item =
    let token = Item.token h item in
    Shrubbery.Match.token h (Ident name) token;
    token

  let var h item =
    let token = Item.token h item in
    match token.token with
    | Ident name -> Syntax.Var.create_initial name token
    | _ -> Fail.fail h
end

let cut l error ~f =
  match Fail.List.optional l f with
  | None -> raise_notrace (Error (Lazy.force error))
  | Some x -> x

let rec parse_decl group = Fail.one_of [ (fun () -> parse_let group) ]

and parse_let h (group : Shrub.group) =
  let@ items = Fail.List.create h group.items in
  let let_tok = Match.ident h "let" (next h items) in
  let var =
    let@ () = cut items (lazy (Error.create "expected var" let_tok)) in
    Match.var h (next h items)
  in
  let block = Fail.unwrap h group.block in
  ()
