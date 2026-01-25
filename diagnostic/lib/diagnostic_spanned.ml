open Core

module Span = Diagnostic_span

type 'a t =
  { span : Span.t
  ; value : 'a
  }
[@@deriving sexp_of, equal, compare]
