open Prelude
module Token_tree := Shrubbery_token_tree
module Token := Shrubbery_token

type group =
  { items : item list
  ; block : token_block option
  ; alts : alt list
  }

and item =
  | Token of Token.t
  | Tree of
      { ldelim : Token.t
      ; groups : group_sep list
      ; rdelim : Token.t
      }

and token_block =
  { token : Token.t
  ; block : block
  }

and block =
  { lbrace : Token.t
  ; groups : group_sep list
  ; rbrace : Token.t
  }

and group_sep =
  { group : group
  ; sep : Token.t option
  }

and alt = token_block [@@deriving sexp, equal, compare]

val block_to_list : block -> Token.t list
