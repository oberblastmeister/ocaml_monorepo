module Source := Oak_source
module Syntax := Oak_syntax
module Context := Oak_context
module Diagnostic := Oak_diagnostic

val infer : Source.t -> Syntax.expr -> (Syntax.term * Syntax.value, Diagnostic.t) result
