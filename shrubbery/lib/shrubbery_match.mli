module Syntax := Shrubbery_syntax
module Token := Shrubbery_token
module Fail := Utility.Fail

module Item : sig
  val token : Fail.t -> Syntax.item -> Token.ti
  val tree : Fail.t -> Syntax.item -> Syntax.item_delim
end

module Token : sig
  val equals : Fail.t -> Token.t -> Token.ti -> unit
  val ident : Fail.t -> Token.ti -> string
end
