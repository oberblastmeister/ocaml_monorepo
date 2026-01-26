module Delimit := Shrubbery_delimit
module Token_tree := Shrubbery_token_tree
module Syntax := Shrubbery_syntax

val parse : string -> Token_tree.t list * Syntax.block * Delimit.Error.t list
