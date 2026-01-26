open Prelude
module Token_tree := Shrubbery_token_tree
module Token := Shrubbery_token

type group =
  { items : item Non_empty_list.t
  ; block : token_block option
  ; alts : alt list
  }

and item =
  | Token of Token.ti
  | Delim of item_delim

and item_delim =
  { ldelim : Token.ti
  ; groups : group_sep list
  ; rdelim : Token.ti
  }

and token_block =
  { token : Token.ti
  ; block : block
  }

and block =
  { lbrace : Token.ti
  ; groups : group_sep list
  ; rbrace : Token.ti
  }

and group_sep =
  { group : group
  ; sep : Token.ti option
  }

and alt = token_block [@@deriving sexp_of, equal, compare]

module Minimal_sexp_of : sig
  val sexp_of_block : block -> Sexp.t
end
