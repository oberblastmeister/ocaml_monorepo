type t

val fail : t -> 'a
val unwrap : t -> 'a option -> 'a
val optional : t -> (t -> 'a) -> 'a option
val either : t -> (t -> 'a) -> (t -> 'b) -> ('b, 'a) Base.Either.t
val many_rev : t -> (t -> 'a) -> 'a list
val many : t -> (t -> 'a) -> 'a list
val some_rev : t -> (t -> 'a) -> 'a list
val some : t -> (t -> 'a) -> 'a list
val guard : t -> bool -> unit
val one_of : t -> (t -> 'a) list -> 'a
val run : (t -> 'a) -> 'a option

module List : sig
  type env := t
  type 'a t = 'a list ref

  val next : env -> 'a t -> 'a
  val take : 'a t -> 'a list
  val empty : env -> 'a t -> unit
  val create : env -> 'a list -> f:(env -> 'a t -> unit) -> unit
  val backtrack : env -> 'a t -> (env -> 'b) -> 'b
end

module Syntax : sig
  val ( <|> ) : t -> (t -> 'a) -> (t -> 'a) -> 'a
end
