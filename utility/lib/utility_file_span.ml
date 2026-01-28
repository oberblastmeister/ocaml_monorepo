open Core
module Span = Utility_span

type t =
  { start : int
  ; stop : int
  ; file : Filename.t
  }
[@@deriving sexp_of]

let to_span { start; stop; file = _ } = { Span.start; stop }
