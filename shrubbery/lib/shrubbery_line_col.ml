open Prelude

type t =
  { line : int
  ; col : int (* offset in bytes from the start of the line *)
  }
[@@deriving sexp, equal, compare]
