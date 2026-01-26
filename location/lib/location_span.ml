open Core

type t =
  { start : int
  ; stop : int
  }
[@@deriving sexp_of, equal, compare]