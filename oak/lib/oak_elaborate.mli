module Source := Oak_source
module Syntax := Oak_syntax
module Context := Oak_context
module Diagnostic := Oak_diagnostic
module Abstract := Oak_abstract

val infer : Source.t -> Abstract.expr -> (Syntax.term * Syntax.value, Diagnostic.t) result
