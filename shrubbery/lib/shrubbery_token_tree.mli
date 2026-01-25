open Prelude
module Token := Shrubbery_token

type t =
  | Token of Token.t
  | Delim of
      { ldelim : Token.t
      ; tts : t list
      ; rdelim : Token.t
      }
[@@deriving sexp_of, equal, compare]

val remove_trivia : t -> t option

module Indexed : sig
  type t =
    | Token of Token.ti
    | Delim of
        { ldelim : Token.ti
        ; tts : t list
        ; rdelim : Token.ti
        }
  [@@deriving sexp_of, equal, compare]

  module Root : sig
    type nonrec t = t list [@@deriving sexp_of, equal, compare]

    val remove_trivia : t -> t
  end

  val is_trivia_token : t -> bool
  val first_token : t -> Token.ti
end

val is_trivia_token : t -> bool
val to_indexed : t -> Indexed.t
val of_indexed : Indexed.t -> t
val remove_trivia : t -> t option
val to_list : t -> Token.t list

module Root : sig
  type t' := t
  type nonrec t = t list [@@deriving sexp_of, equal, compare]

  val to_indexed : t -> Indexed.Root.t
  val to_list : t -> Token.t list
  val remove_trivia : t -> t
end
