open Prelude
module Span = Utility.Span

module Core_ty = struct
  type t =
    | Bool
    | Unit
    | Int
  [@@deriving sexp_of, equal, compare]

  let to_string = function
    | Bool -> "Bool"
    | Unit -> "Unit"
    | Int -> "Int"
  ;;

  let pp t = Doc.string (to_string t)
end

module Name = struct
  module T = struct
    type t =
      { name : string
      ; span : Span.t [@equal.ignore] [@compare.ignore] [@hash.ignore]
      }
    [@@deriving sexp_of, compare, equal, hash]
  end

  include T
  include Comparable.Make_plain (T)
  include Hashable.Make_plain (T)

  let create name span = { name; span }
end

module Size : sig
  type t = private int [@@deriving sexp_of, compare, equal]

  val minimum : t
  val to_int : t -> int
  val of_int : int -> t
  val incr : t -> t
  val decr : t -> t
  val decr_exn : t -> t
  val min : t -> t -> t
  val max : t -> t -> t
  val to_string : t -> string
  val is_type : t -> bool
  val type_ : t
  val sig_ : t
  val pp : t -> Doc.t
end = struct
  include Int

  let minimum = 0
  let to_int t = t
  let of_int_exn t = t
  let incr u = u + 1
  let decr_exn u = u - 1
  let decr u = Int.max 0 (u - 1)

  let to_string = function
    | 0 -> "Type"
    | 1 -> "Sig"
    | _n -> "SIG"
  ;;

  let sexp_of_t t = Sexp.Atom (to_string t)
  let pp t = Doc.string (to_string t)
  let is_type t = t = 0
  let type_ = 0
  let sig_ = 1
end

module Level = struct
  type t = { level : int } [@@unboxed] [@@deriving sexp_of, equal, compare]

  let of_int level =
    assert (level >= 0);
    { level }
  ;;
end

module Index = struct
  type t = { index : int } [@@unboxed] [@@deriving sexp_of, equal, compare]

  let zero = { index = 0 }

  let of_int index =
    assert (index >= 0);
    { index }
  ;;

  let to_level context_size { index } = Level.of_int (context_size - index - 1)
  let of_level context_size { Level.level } = of_int (context_size - level - 1)
end

module Literal = struct
  type t =
    | Unit
    | Bool of bool
    | Int of int
    | String of string
  [@@deriving sexp_of]
end

module Icit = struct
  type t =
    | Impl
    | Expl
  [@@deriving sexp_of, equal, compare]

  let to_string = function
    | Impl -> "implicit"
    | Expl -> "explicit"
  ;;

  let pp t = Doc.string (to_string t)
end

module Relevancy = struct
  type t =
    | Relevant
    | Irrelevant
  [@@deriving sexp_of, equal, compare]

  let to_string = function
    | Relevant -> "relevant"
    | Irrelevant -> "irrelevant"
  ;;

  let pp = function
    | Relevant -> Doc.string "relevant"
    | Irrelevant -> Doc.string "irrelevant"
  ;;
end

module Param_modifiers = struct
  type t =
    { icit : Icit.t
    ; relevancy : Relevancy.t
    }
  [@@deriving sexp_of]
end
