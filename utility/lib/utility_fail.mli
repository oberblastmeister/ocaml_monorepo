type t

exception Fail

val env : t
val fail : t -> 'a
val unwrap : t -> 'a option -> 'a
val optional : (unit -> 'a) -> 'a option
val either : (unit -> 'a) -> (unit -> 'b) -> ('b, 'a) Base.Either.t
val many_rev : (unit -> 'a) -> 'a list
val many : (unit -> 'a) -> 'a list
val some_rev : (unit -> 'a) -> 'a list
val some : (unit -> 'a) -> 'a list
val guard : t -> bool -> unit
val one_of : (unit -> 'a) list -> 'a
val run : f:(t -> 'a) -> 'a option
val run_exn : f:(t -> 'a) -> 'a
val run_or_thunk : default:(unit -> 'a) -> f:(t -> 'a) -> 'a
val cannot_fail : f:(unit -> 'a) -> 'a

module List : sig
  type env := t
  type 'a t = 'a list ref [@@deriving sexp_of]

  val next : env -> 'a t -> 'a
  val peek : env -> 'a t -> 'a
  val take : 'a t -> 'a list
  val empty : env -> 'a t -> unit
  val create : 'a list -> 'a t
  val optional : 'a t -> (unit -> 'b) -> 'b option
  val either : 'a t -> (unit -> 'b) -> (unit -> 'c) -> ('c, 'b) Base.Either.t
  val many_rev : 'a t -> (unit -> 'b) -> 'b list
  val many : 'a t -> (unit -> 'b) -> 'b list
  val some_rev : 'a t -> (unit -> 'b) -> 'b list
  val some : 'a t -> (unit -> 'b) -> 'b list
  val guard : env -> 'a t -> bool -> unit
  val one_of : 'a t -> (unit -> 'b) list -> 'b
end

module Syntax : sig
  val ( <|> ) : (unit -> 'a) -> (unit -> 'a) -> 'a
end
