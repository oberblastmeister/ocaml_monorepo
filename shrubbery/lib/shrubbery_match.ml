open Prelude
module Syntax = Shrubbery_syntax
module Token = Shrubbery_token
module Fail = Utility.Fail

let token env t1 (t2 : Token.ti) = if Token.equal t1 t2.token then () else Fail.fail env

module Item = struct
  let token env (t : Syntax.item) =
    match t with
    | Token t -> t
    | Delim _ -> Fail.fail env
  ;;

  let tree env (t : Syntax.item) =
    match t with
    | Token _ -> Fail.fail env
    | Delim t -> t
  ;;
end
