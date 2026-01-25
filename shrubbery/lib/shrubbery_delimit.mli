open Prelude
module Token := Shrubbery_token
module Token_tree := Shrubbery_token_tree

module Error : sig
  type t =
    | Mismatching_delimiters of
        { ldelim : Token.ti
        ; rdelim : Token.ti
        }
    | Expecting_delimiter of Token.ti
  [@@deriving sexp_of]
end

val delimit : Token.t array -> Token_tree.t list * Error.t list
