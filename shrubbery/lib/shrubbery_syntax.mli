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
  | Delim of item_delim

and item_delim =
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

module Indexed : sig
  type group =
    { items : item list
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

  and alt = token_block [@@deriving sexp, equal, compare]

  val remove_trivia_block : block -> block
  val remove_trivia_group : group -> group
end

val block_to_indexed : block -> Indexed.block
val group_to_indexed : group -> Indexed.group
val block_of_indexed : Indexed.block -> block
val group_of_indexed : Indexed.group -> group
