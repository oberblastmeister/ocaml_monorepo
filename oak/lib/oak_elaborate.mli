module Source := Oak_source
module Abstract := Oak_abstract
module Typed := Oak_typed
module Diagnostic := Oak_diagnostic

val infer : Source.t -> Abstract.expr -> (Typed.expr, Diagnostic.t) result
