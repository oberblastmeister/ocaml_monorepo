module Syntax := Shrubbery_syntax
module Token := Shrubbery_token
module Fail := Utility.Fail

val token : Fail.t -> Token.t -> Token.ti -> unit

module Item : sig
  val token : Fail.t -> Syntax.item -> Token.ti
  val tree : Fail.t -> Syntax.item -> Syntax.item_delim
end
