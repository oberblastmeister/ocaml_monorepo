open Core

module Purity : sig
  type t =
    | Pure
    | Impure
  [@@deriving sexp_of, compare, equal]

  include Base.Comparable.S with type t := t

  val merge : t -> t -> t
end

module Universe : sig
  type t =
    | Type
    | Kind
    | Sig
  [@@deriving sexp, equal, compare]

  include Base.Comparable.S with type t := t

  val minimum : t
  val maximum : t
  val to_int : t -> int
  val of_int_exn : int -> t
  val lub : t -> t -> t
  val incr_exn : t -> t
end

module Var : sig
  type t = private
    { id : int
    ; name : string
    }
  [@@deriving sexp, compare, equal]

  include Comparable.S with type t := t

  val create : string -> t
  val make_fresh : t -> t
end

module Mod_var : sig
  type t = private int

  include Comparable.S with type t := t

  val create : unit -> t
end

module Cvar : sig
  type t =
    | Var of Var.t
    | Mod_var of Mod_var.t
  [@@deriving sexp, compare, equal]

  include Comparable.S with type t := t

  val create_var : string -> t
  val create_mod_var : unit -> t
end

type core_ty =
  | Ty_bool
  | Ty_unit
[@@deriving sexp_of, equal, compare]

type expr =
  | Expr_var of Cvar.t
  | Expr_seal of
      { e : expr
      ; ty : expr
      }
  | Expr_app of
      { func : expr
      ; args : expr list
      }
  | Expr_abs of
      { params : expr_param list
      ; body : expr
      ; purity : Purity.t
      }
  | Expr_ty_fun of expr_ty_fun
  | Expr_proj of
      { mod_e : expr
      ; field : string
      }
  | Expr_mod of
      { var : Mod_var.t
      ; decls : expr_decl list
      }
  | Expr_ty_mod of expr_ty_mod
  | Expr_let of
      { var : Var.t
      ; rhs : expr
      ; body : expr
      }
  | Expr_ty_sing of expr_ty_sing
  | Expr_bool of bool
  | Expr_unit
  | Expr_core_ty of core_ty
  | Expr_universe of Universe.t
  | Expr_if of
      { cond : expr
      ; body1 : expr
      ; body2 : expr
      }

and expr_decl =
  { field : string
  ; e : expr
  }

and expr_ty_sing =
  { e : expr
  ; ty : expr
  }

and expr_ty_mod =
  { var : Mod_var.t
  ; ty_decls : expr_ty_decl list
  }

and expr_ty_fun =
  { params : expr_param list
  ; body_ty : expr
  ; purity : Purity.t
  }

and expr_ty_decl =
  { field : string
  ; ty : expr
  }

and expr_param =
  { var : Var.t
  ; ty : expr
  }

and value =
  | Value_irrelevant
  | Value_mod of value_mod
  | Value_abs of value_abs
  | Value_ty of ty

and ty =
  | Value_core_ty of core_ty
  | Value_path of path
  | Value_univ of Universe.t
  | Value_ty_sing of value_ty_sing
  | Value_ty_mod of value_ty_mod
  | Value_ty_fun of value_ty_fun

and path =
  | Path_var of Cvar.t
  | Path_app of
      { func : path
      ; args : value list
      }
  | Path_proj of
      { mod_e : path
      ; field : string
      }

and value_mod = { decls : value_decl list }

and value_abs =
  { binder : value_abs_binder
  ; purity : Purity.t
  }

and value_ty_sing =
  { e : value
  ; ty : ty
  }

and value_ty_mod = { binder : value_ty_mod_binder }
and value_ty_mod_binder

and value_ty_decl =
  { field : string
  ; ty : ty
  }

and value_ty_fun =
  { binder : value_ty_fun_binder
  ; purity : Purity.t
  }

and value_abs_binder
and value_ty_fun_binder

and value_decl =
  { field : string
  ; e : value
  }

and value_param =
  { var : Var.t
  ; ty : ty
  }
[@@deriving sexp_of]

type subst

module Value_abs_binder : sig
  type t = value_abs_binder [@@deriving sexp_of]

  type data =
    { params : value_param list
    ; body : value
    }
  [@@deriving sexp_of]

  val unpack : t -> f:(data -> 'a) -> 'a
  val unpack_advanced : t -> data
  val pack : data -> t
end

module Value_ty_mod_binder : sig
  type t = value_ty_mod_binder [@@deriving sexp_of]

  type data =
    { var : Mod_var.t
    ; ty_decls : value_ty_decl list
    ; ty_decls_map : value_ty_decl String.Map.t
    }
  [@@deriving sexp_of]

  val unpack : t -> f:(data -> 'a) -> 'a
  val unpack_advanced : t -> data
  val pack : data -> t
end

module Value_ty_fun_binder : sig
  type t = value_ty_fun_binder [@@deriving sexp_of]

  type data =
    { params : value_param list
    ; body_ty : ty
    }
  [@@deriving sexp_of]

  val unpack : t -> f:(data -> 'a) -> 'a
  val unpack_advanced : t -> data
  val pack : data -> t
end

module Subst : sig
  type t = subst

  val empty : t
  val add_exn : t -> Cvar.t -> value -> t
  val singleton : Cvar.t -> value -> t
  val of_alist_exn : (Cvar.t * value) list -> t
  val compose : second:t -> first:t -> t
end

module Path : sig
  type t = path

  val eval : subst -> t -> value
end

module Value : sig
  type t = value

  val eval : subst -> t -> value
  val ty_var : Var.t -> ty
  val ty_mod_var : Mod_var.t -> ty
  val var : Var.t -> value
  val mod_var : Mod_var.t -> value
  val get_ty_exn : value -> ty
  val get_ty_fun_exn : ty -> value_ty_fun
  val get_ty_mod_exn : ty -> value_ty_mod
  val get_ty_univ_exn : ty -> Universe.t
  val app_exn : value -> value list -> value
  val proj_exn : value -> string -> value
end

val proj_value_exn : value -> string -> value
val app_value_exn : value -> value list -> value

module Ty : sig
  type t = ty

  val eval : subst -> t -> value
end
