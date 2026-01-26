open Core
module Syntax := Oak_syntax
module Diagnostic := Oak_diagnostic

val parse
  :  file:string
  -> string
  -> Shrubbery.Token_tree.t list * Diagnostic.t list * Syntax.expr
