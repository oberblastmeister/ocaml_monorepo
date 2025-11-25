module Token := Shrubbery_token
module Token_tree := Shrubbery_token_tree

val insert_virtual_tokens
  :  Token.t array
  -> Token_tree.Indexed.t list
  -> Token_tree.t list
