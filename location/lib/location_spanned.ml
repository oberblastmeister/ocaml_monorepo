open Core
module Span = Location_span

type 'a t =
  { span : Span.t
  ; value : 'a
  }
[@@deriving sexp_of, equal, compare]