module Token_tree := Shrubbery_token_tree
module Shrub := Shrubbery_shrub

val parse : Token_tree.t list -> Shrub.block
