module Delimit := Shrubbery_delimit
module Token_tree := Shrubbery_token_tree
module Syntax := Shrubbery_syntax

val parse : string -> Token_tree.t list * Syntax.group option * Delimit.Error.t list
