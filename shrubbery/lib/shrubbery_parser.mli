module Delimit := Shrubbery_delimit
module Token_tree := Shrubbery_token_tree
module Syntax := Shrubbery_syntax

val parse : ?remove_trivia:bool -> string -> Syntax.block * Delimit.Error.t list
