open Core
module Syntax := Oak_syntax
module Diagnostic := Oak_diagnostic

module Error : sig
  type t =
    { error : string
    ; token : int
    }
  [@@deriving sexp_of]
end

val parse
  :  file:string
  -> string
  -> Shrubbery.Token_tree.t list * Diagnostic.t list * Syntax.expr
