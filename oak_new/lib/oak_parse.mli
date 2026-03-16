open Prelude
module Source := Oak_source
module Diagnostic := Oak_diagnostic
module Surface := Oak_surface

val parse
  :  file:Filename.t
  -> string
  -> Source.t * Diagnostic.t list * Surface.expr option
