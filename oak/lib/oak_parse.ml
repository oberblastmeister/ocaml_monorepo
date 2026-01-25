open Core
module Shrub = Shrubbery.Syntax
module Token = Shrubbery.Token
module Fail = Utility.Fail
module Syntax = Oak_syntax

module Error = struct
  type t =
    { error : string
    ; token : Token.ti
    }
  [@@deriving sexp_of]

  let create error token = { error; token }
end

exception Error of Error.t

let next = Fail.List.next
let ( let@ ) f k = f ~f:k

module Match = struct
  include Shrubbery.Match

  let ident h name item =
    let token = Item.token h item in
    Shrubbery.Match.token h (Ident name) token;
    token
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

let rec parse_decl group =
  Fail.one_of
    [ (fun () -> parse_let_decl group)
    (* ; (fun () -> error (lazy (Error.create "Invalid declaration" _))) *)
    ]

and parse_let_decl h (group : Shrub.group) =
  let@ items = Fail.List.create h group.items in
  let let_tok = Match.ident h "let" (next h items) in
  let@ () = cut items (lazy (Error.create "failed to parse let" let_tok)) in
  let var = Match.var h (next h items) in
  let block = Fail.unwrap h group.block in
  ()
;;
