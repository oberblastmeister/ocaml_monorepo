module Source := Oak_source
module Surface := Oak_surface
module Diagnostic := Oak_diagnostic
module Abstract := Oak_abstract

val rename : Source.t -> Surface.expr -> Diagnostic.t list * Abstract.expr
