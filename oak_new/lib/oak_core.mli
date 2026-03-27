module Syntax := Oak_core_syntax
module Common := Oak_common
module Level = Common.Level
module Index = Common.Index
module Name = Common.Name

type ty_props = Syntax.ty_props
type term = Syntax.term
type term_data = Syntax.term_data
type term_data_rec = Syntax.term_data_rec
type term_data_decl = Syntax.term_data_decl
type term_data_param = Syntax.term_data_param
type term_data_body = Syntax.term_data_body
type term_data_field = Syntax.term_data_field
type term_data_constructor = Syntax.term_data_constructor
type field_loc = Syntax.field_loc
type term_field_impl = Syntax.term_field_impl
type term_field_spec = Syntax.term_field_spec
type term_ty = Syntax.term_ty
type term_ty_struct = Syntax.term_ty_struct
type value = Syntax.value
type ty = Syntax.ty
type value_data_rec = Syntax.value_data_rec
type value_data_decl = Syntax.value_data_decl
type value_data = Syntax.value_data
type ty_sing = Syntax.ty_sing
type ty_struct = Syntax.ty_struct
type head = Syntax.head
type neutral = Syntax.neutral
type spine = Syntax.spine
type term_arg = Syntax.term_arg
type value_arg = Syntax.value_arg
type frame = Syntax.frame
type value_struct = Syntax.value_struct
type value_fun = Syntax.value_fun
type ty_fun = Syntax.ty_fun
type ty_closure = Syntax.ty_closure
type value_closure = Syntax.value_closure
type value_field_impl = Syntax.value_field_impl

module type ENV = sig
  type t
  type value

  val empty : t
  val push : value -> t -> t
  val pop : t -> (value * t) option
  val pop_exn : t -> value * t
  val get_index : t -> Index.t -> value option
  val get_level : t -> Level.t -> value option
  val get_index_exn : t -> Index.t -> value
  val get_level_exn : t -> Level.t -> value
  val iter : t -> f:(value -> unit) -> unit
  val to_list : t -> value list
  val of_list : value list -> t
  val length : t -> int
end

module Value_env : ENV with type value = value
module Ty_env : ENV with type value = ty
module Name_env : ENV with type value = Name.t

module Erased_env : sig
  type t = private int
  type value = unit

  val empty : t
  val push : t -> t
  val pop : t -> t option
  val pop_exn : t -> t
  val get_index : t -> Index.t -> bool
  val get_level : t -> Level.t -> bool
end

module Erased_env_ENV : ENV with type t = Erased_env.t and type value = unit

type value_env = Value_env.t
type ty_env = Ty_env.t
type name_env = Name_env.t
type erased_env = Erased_env.t

module Close : sig
  type t

  val empty : t
  val lift : int -> t -> t
  val singleton : Level.t -> Index.t -> t
  val add_exn : Level.t -> Index.t -> t -> t
  val compose : second:t -> first:t -> t
  val find : t -> Level.t -> Index.t option
end

module Field_loc : sig
  type t = field_loc
end

module Term : sig
  type t = term
end

module Term_ty : sig
  type t = term_ty
end

module Value : sig
  type t = value

  val quote : value -> term
  val proj : value -> field_loc -> value
  val app : value -> value_arg -> value
  val out : value -> value
  val decode : value -> ty
end

module Ty : sig
  type t = ty

  val quote : ty -> term_ty
  val proj : ty_env -> value -> ty -> field_loc -> ty
  val app : ty_env -> ty -> value_arg -> ty
  val out : ty_env -> ty -> ty
end

module Struct : sig
  type t = value_struct

  val proj : t -> field_loc -> value
end

module Ty_struct : sig
  type t = ty_struct

  val proj : value -> t -> field_loc -> ty
end

module Fun : sig
  type t = value_fun

  val app : value_fun -> value_arg -> value
end

module Ty_fun : sig
  type t = ty_fun

  val app : ty_fun -> value_arg -> ty
end
