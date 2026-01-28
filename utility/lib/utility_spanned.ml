open Core
module Span = Utility_span

type 'a t =
  { span : Span.t
  ; value : 'a
  }
[@@deriving sexp_of, equal, compare]

let create value span = { span; value }
