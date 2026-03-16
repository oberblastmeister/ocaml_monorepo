open Prelude
open Oak_syntax

val eval : env -> term -> value
val eval_ty : env -> term_ty -> ty
val eval_closure1 : value_closure -> value -> value
val whnf : ty_env -> value -> value
val whnf_ty : ty_env -> ty -> ty
val quote : int -> value -> term

module Close : sig
  type t

  val empty : t
  val lift : int -> t -> t
  val singleton : Level.t -> Index.t -> t
  val add_exn : Level.t -> Index.t -> t -> t
  val compose : second:t -> first:t -> t
  val find : t -> Level.t -> Index.t option
end

val close : Close.t -> term -> term
val close_single : Level.t -> term -> term

module Struct : sig
  val proj : value_struct -> field_loc -> value
end

module Struct_ty : sig
  val proj : value -> ty_struct -> field_loc -> ty
end

module Fun : sig
  val app : value_fun -> value_arg -> value
end

module Fun_ty : sig
  val app : ty_fun -> value_arg -> ty
end

module Value : sig
  val proj : value -> field_loc -> value
  val app : value -> value_arg -> value
  val out : value -> value
end

module Ty : sig
  val proj : ty_env -> value -> ty -> field_loc -> ty
  val app : ty_env -> ty -> value_arg -> ty
  val out : ty_env -> ty -> ty
end
