open Prelude
open Oak_syntax

val eval : Env.t -> term -> value
val eval_closure1 : value_closure -> value -> value
val unfold : Env.t -> value -> whnf
val quote : int -> value -> term

module Mod : sig
  val proj : value_mod -> int -> value
end

module Mod_ty : sig
  val proj : value -> value_ty_mod_closure -> int -> value
end

module Abs : sig
  val app : value_abs -> value -> value
end

module Fun_ty : sig
  val app : value_ty_fun -> value -> value
end

module Value : sig
  val proj : value -> string -> int -> value
  val app : value -> value -> Icit.t -> value
  val out : value -> value
end

module Ty : sig
  val proj : env -> value -> value -> int -> value
  val app : env -> value -> value -> value
  val out : env -> value -> value
end
