open Prelude
module Token := Shrubbery_token
module Token_tree := Shrubbery_token_tree
module Span := Utility.Span

module Error : sig
  type t =
    | Mismatching_delimiters of
        { ldelim : Span.t
        ; rdelim : Span.t
        }
    | Expecting_delimiter of Span.t
  [@@deriving sexp_of]
end

val delimit : Token.t array -> Token_tree.t list * Error.t list
