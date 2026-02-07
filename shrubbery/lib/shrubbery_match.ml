open Prelude
module Syntax = Shrubbery_syntax
module Fail = Utility.Fail

module Token = struct
  let equals env t1 (t2 : Shrubbery_token.ti) =
    if Shrubbery_token.equal t1 t2.token then () else Fail.fail env
  ;;

  let ident env (t : Shrubbery_token.ti) =
    match t.token with
    | Shrubbery_token.Ident s -> s
    | _ -> Fail.fail env
  ;;
end

module Item_delim = struct
  let single_group env (d : Syntax.item_delim) =
    match d.groups with
    | [ { group; sep = None } ] -> group
    | _ -> Fail.fail env
  ;;
end

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
