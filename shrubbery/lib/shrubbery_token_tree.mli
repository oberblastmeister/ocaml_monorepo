open Prelude
module Token := Shrubbery_token

type t =
  | Token of Token.t
  | Tree of
      { ldelim : Token.t
      ; tts : t list
      ; rdelim : Token.t
      }
[@@deriving sexp, equal, compare]

type root = t list [@@deriving sexp, equal, compare]

module Indexed : sig
  type t =
    | Token of Token.ti
    | Tree of
        { ldelim : Token.ti
        ; tts : t list
        ; rdelim : Token.ti
        }
  [@@deriving sexp, equal, compare]

  type root = t list [@@deriving sexp, equal, compare]

  val is_trivia_token : t -> bool
  val first_token : t -> Token.ti
end

val is_trivia_token : t -> bool
val to_indexed : t -> Indexed.t
val of_indexed : Indexed.t -> t
val remove_trivia : t -> t option
val remove_trivia_root : t list -> t list
val root_to_indexed : root -> Indexed.root
val root_to_list : root -> Token.t list
val to_list : t -> Token.t list
