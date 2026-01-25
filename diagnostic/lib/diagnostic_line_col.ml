open Core

type t =
  { line : int
  ; col : int
  }
[@@deriving sexp_of, equal, compare]
