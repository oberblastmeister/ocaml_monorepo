module Syntax := Oak_core_syntax
module Common := Oak_common
module Icit = Common.Icit
module Level = Common.Level
module Index = Common.Index
module Name = Common.Name
module Relevancy = Common.Relevancy

type ty_props = Syntax.ty_props [@@deriving sexp_of]
type term = Syntax.term [@@deriving sexp_of]
type term_data = Syntax.term_data [@@deriving sexp_of]
type term_data_rec = Syntax.term_data_rec [@@deriving sexp_of]
type term_data_decl = Syntax.term_data_decl [@@deriving sexp_of]
type term_data_param = Syntax.term_data_param [@@deriving sexp_of]
type term_data_body = Syntax.term_data_body [@@deriving sexp_of]
type term_data_field = Syntax.term_data_field [@@deriving sexp_of]
type term_data_constructor = Syntax.term_data_constructor [@@deriving sexp_of]
type field_loc = Syntax.field_loc [@@deriving sexp_of]
type term_field_impl = Syntax.term_field_impl [@@deriving sexp_of]
type term_field_spec = Syntax.term_field_spec [@@deriving sexp_of]
type term_ty = Syntax.term_ty [@@deriving sexp_of]
type term_ty_struct = Syntax.term_ty_struct [@@deriving sexp_of]
type value = Syntax.value [@@deriving sexp_of]
type ty = Syntax.ty [@@deriving sexp_of]
type value_data_rec = Syntax.value_data_rec [@@deriving sexp_of]
type value_data_decl = Syntax.value_data_decl [@@deriving sexp_of]
type value_data = Syntax.value_data [@@deriving sexp_of]
type ty_sing = Syntax.ty_sing [@@deriving sexp_of]
type ty_struct = Syntax.ty_struct [@@deriving sexp_of]
type head = Syntax.head [@@deriving sexp_of]
type neutral = Syntax.neutral [@@deriving sexp_of]
type spine = Syntax.spine [@@deriving sexp_of]
type term_arg = Syntax.term_arg [@@deriving sexp_of]
type value_arg = Syntax.value_arg [@@deriving sexp_of]
type frame = Syntax.frame [@@deriving sexp_of]
type value_struct = Syntax.value_struct [@@deriving sexp_of]
type value_fun = Syntax.value_fun [@@deriving sexp_of]
type value_param = Syntax.value_param [@@deriving sexp_of]
type term_param = Syntax.term_param [@@deriving sexp_of]
type ty_fun = Syntax.ty_fun [@@deriving sexp_of]
type ty_closure = Syntax.ty_closure [@@deriving sexp_of]
type value_closure = Syntax.value_closure [@@deriving sexp_of]
type value_field_impl = Syntax.value_field_impl [@@deriving sexp_of]
type value_field_spec = Syntax.value_field_spec [@@deriving sexp_of]

module type ENV = sig
  type t [@@deriving sexp_of]
  type value [@@deriving sexp_of]

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

module Value_env : ENV with type value = value and type t = Syntax.env
module Ty_env : ENV with type value = ty and type t = Syntax.ty_env
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

type value_env = Value_env.t [@@deriving sexp_of]
type ty_env = Ty_env.t [@@deriving sexp_of]
type name_env = Name_env.t [@@deriving sexp_of]
type erased_env = Erased_env.t [@@deriving sexp_of]

module Ty_props : sig
  type t = ty_props [@@deriving sexp_of]
end

module Close : sig
  type t [@@deriving sexp_of]

  val empty : t
  val lift : int -> t -> t
  val singleton : Level.t -> Index.t -> t
  val add_exn : Level.t -> Index.t -> t -> t
  val compose : second:t -> first:t -> t
  val find : t -> Level.t -> Index.t option
  val push_exn : Level.t -> t -> t
end

module Field_loc : sig
  type t = field_loc [@@deriving sexp_of]

  val create : string -> int -> t
end

module Term : sig
  type t = term [@@deriving sexp_of]

  val of_level : Level.t -> term
  val close : Close.t -> term -> term
  val close_single : Level.t -> term -> term
  val eval : value_env -> term -> value
end

module Term_ty : sig
  type t = term_ty [@@deriving sexp_of]

  val close : Close.t -> term_ty -> term_ty
  val close_single : Level.t -> term_ty -> term_ty
  val eval : value_env -> term_ty -> ty
end

module Value_field_impl : sig
  type t = value_field_impl [@@deriving sexp_of]

  val create : string -> value -> t
end

module Value : sig
  type t = value [@@deriving sexp_of]

  val create_struct : value_field_impl list -> value
  val whnf : ty_env -> value -> value
  val free : Level.t -> value
  val free_of_size : int -> value
  val neutral_val_exn : value -> neutral
  val quote : value -> term
  val proj : value -> field_loc -> value
  val app : value -> value_arg -> value
  val out : value -> value
  val decode : value -> ty
end

module Ty : sig
  type t = ty [@@deriving sexp_of]

  val infer_props : ty_env -> ty -> Ty_props.t
  val whnf : ty_env -> ty -> ty
  val ty_fun_val_exn : ty -> ty_fun
  val ty_struct_val_exn : ty -> ty_struct
  val ty_sing_val_exn : ty -> ty_sing
  val ty_universe_val_exn : ty -> Ty_props.t
  val quote : ty -> term_ty
  val proj : ty_env -> value -> ty -> field_loc -> value_field_spec
  val app : ty_env -> ty -> value_arg -> ty
  val out : ty_env -> ty -> ty
end

module Neutral : sig
  type t = neutral [@@deriving sexp_of]

  val infer_ty : ty_env -> neutral -> ty
  val infer_universe : ty_env -> neutral -> Ty_props.t
  val whnf : ty_env -> neutral -> value
end

module Head : sig
  val infer_ty : ty_env -> head -> ty
end

module Struct : sig
  type t = value_struct [@@deriving sexp_of]

  val proj : t -> field_loc -> value
end

module Term_ty_struct : sig
  type t = term_ty_struct [@@deriving sexp_of]

  val of_iterated_binders : term_field_spec list -> term_ty_struct
end

module Ty_struct : sig
  type t = ty_struct [@@deriving sexp_of]

  val field_locations : t -> field_loc list
  val proj : value -> t -> field_loc -> value_field_spec
  val of_iterated_binders : term_field_spec list -> ty_struct
end

module Fun : sig
  type t = value_fun [@@deriving sexp_of]

  val app : value_fun -> value -> value
end

module Ty_fun : sig
  type t = ty_fun [@@deriving sexp_of]

  val app : ty_fun -> value_arg -> ty
end
