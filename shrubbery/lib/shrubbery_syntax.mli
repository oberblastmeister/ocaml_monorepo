open Prelude
module Span := Utility.Span
module Token_tree := Shrubbery_token_tree
module Token := Shrubbery_token

type group = item Non_empty_list.t

and item =
  | Token of Token.ti
  | Delim of item_delim

and item_delim =
  { ldelim : Token.ti
  ; groups : group_sep list
  ; rdelim : Token.ti
  }

and group_sep =
  { group : group
  ; sep : Token.ti option
  }

and root = group option

module Root : sig
  type t = root
end

module Minimal_sexp_of : sig
  val sexp_of_group : group -> Sexp.t
  val sexp_of_root : root -> Sexp.t
end

module Item : sig
  type t = item

  val span : t -> Span.t
  val first_token : t -> Token.ti
end

module Group : sig
  type t = group

  val first_token : t -> Token.ti
end
