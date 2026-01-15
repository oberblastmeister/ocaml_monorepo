open Core

module Purity : sig
  type t = Pure | Impure [@@deriving sexp_of, compare, equal]

  include Base.Comparable.S with type t := t

  val merge : t -> t -> t
end

module Universe : sig
  type t = Type | Kind | KIND [@@deriving sexp, equal, compare]

  include Base.Comparable.S with type t := t

  val minimum : t
  val maximum : t
  val to_int : t -> int
  val of_int_exn : int -> t
  val lub : t -> t -> t
  val incr_exn : t -> t
end

module Var : sig
  type t = private { id : int; name : string } [@@deriving sexp, compare, equal]

  include Comparable.S with type t := t

  val create : string -> t
  val make_fresh : t -> t
end

module Record_var : sig
  type t = private int

  include Comparable.S with type t := t

  val create : unit -> t
end

module Cvar : sig
  type t =
    | Var of Var.t
    | Record_field of { var : Record_var.t; field : string }
  [@@deriving sexp, compare, equal]

  include Comparable.S with type t := t

  val create : string -> t
  val create_field : string -> t
end

type expr =
  | Expr_var of Cvar.t
  | Expr_seal of { e : expr; ty : expr }
  | Expr_app of { e : expr; es : expr list }
  | Expr_abs of { params : param list; body : expr; purity : Purity.t }
  | Expr_ty_fun of expr_ty_fun
  | Expr_proj of { e : expr; field : string }
  | Expr_mod of { var : Record_var.t; decls : decl list }
  | Expr_ty_mod of ty_mod
  | Expr_let of { var : Var.t; rhs : expr; body : expr }
  | Expr_ty_sing of expr_ty_sing
  | Expr_bool of bool
  | Expr_unit
  | Expr_core_ty of core_ty
  | Expr_universe of Universe.t
  | Expr_if of { e1 : expr; e2 : expr; e3 : expr }

and expr_ty_sing = { e : expr; ty : expr }
and expr_ty_fun = { params : param list; ty : expr; purity : Purity.t }

and path =
  | Path_long_ident of long_ident
  | Path_core_ty of core_ty
  | Path_universe of Universe.t
  | Path_ty_sing of path_ty_sing
  | Path_ty_mod of path_ty_mod
  | Path_ty_fun of path_ty_fun

and long_ident =
  | Long_ident_app of { e : long_ident; args : value list }
  | Long_ident_proj of { e : long_ident; field : string }
  | Long_ident_var of Cvar.t

and path_ty_sing = { e : path; ty : path }
and path_ty_mod
and path_ty_fun
and path_ty_decl = { field : string; ty : path }
and path_param = { var : Var.t; ty : path }

and value =
  | Value_irrelevant
  | Value_path of path
  | Value_mod of value_mod
  | Value_abs of value_abs

and value_mod = { decls : value_decl list }
and value_abs
and value_decl = { field : string; e : value }
and ty_mod = { var : Record_var.t; ty_decls : ty_decl list }
and core_ty = Ty_bool | Ty_unit
and param = { var : Var.t; ty : expr }
and decl = { field : string; e : expr }
and ty_decl = { field : string; ty : expr } [@@deriving sexp_of]

val path_var : Var.t -> value
val eval_subst_value : value Var.Map.t -> value -> value
val eval_subst_path : value Var.Map.t -> path -> path
val subst_record_var_value : Record_var.t Record_var.Map.t -> value -> value
val subst_record_var_path : Record_var.t Record_var.Map.t -> path -> path

module Value_abs : sig
  type t = value_abs [@@deriving sexp_of]

  type data = { params : path_param list; body : value; purity : Purity.t }
  [@@deriving sexp_of]

  val unpack : t -> f:(data -> 'a) -> 'a
  val unpack_advanced : t -> f:(t -> 'a) -> 'a
  val pack : data -> t
end

module Path_ty_mod : sig
  type t = path_ty_mod [@@deriving sexp_of]

  type data = { var : Record_var.t; ty_decls : path_ty_decl list }
  [@@deriving sexp_of]

  val unpack : t -> f:(data -> 'a) -> 'a
  val unpack_advanced : t -> f:(t -> 'a) -> 'a
  val pack : data -> t
end

module Path_ty_fun : sig
  type t = path_ty_fun [@@deriving sexp_of]

  type data = { params : path_param list; ty : path; purity : Purity.t }
  [@@deriving sexp_of]

  val unpack : t -> f:(data -> 'a) -> 'a
  val unpack_advanced : t -> f:(t -> 'a) -> 'a
  val pack : data -> t
end

val is_path_universe : path -> bool
val path_universe_exn : path -> Universe.t
val path_ty_fun_exn : path -> path_ty_fun
val path_ty_mod_exn : path -> path_ty_mod
val value_path_exn : value -> path
