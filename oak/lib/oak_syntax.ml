open Core

module Purity = struct
  type t = Pure | Impure [@@deriving sexp_of, compare, equal]

  let merge x y =
    match (x, y) with
    | Impure, _ -> Impure
    | _, Impure -> Impure
    | Pure, Pure -> Pure
end

module Transparency = struct
  type t = Opaque | Transparent [@@deriving sexp_of, compare, equal]

  let merge x y =
    match (x, y) with
    | Opaque, _ -> Opaque
    | _, Opaque -> Opaque
    | Transparent, Transparent -> Transparent
end

module Var = struct
  module T = struct
    type t = { id : int; name : string } [@@deriving sexp, compare, equal]
  end

  include Comparable.Make (T)
  include T

  let create name id = { name; id }
end

(* TODO: add names to all these *)
type expr =
  | Expr_var of Var.t
  | Expr_seal of expr * expr
  | Expr_app of expr * expr list
  | Expr_abs of Var.t list * expr * Purity.t
  | Expr_ty_fun of ty_fun
  | Expr_proj of expr * Var.t
  | Expr_mod of decl list
  | Expr_ty_mod of ty_mod
  | Expr_let of Var.t * expr * expr
  | Expr_type
  | Expr_kind
  | Expr_ty_sing of expr * expr
  | Expr_bool of bool
  | Expr_ty_bool
  | Expr_unit
  | Expr_ty_unit
  | Expr_if of expr * expr * expr

and path =
  | Path_var of Var.t
  | Path_simple_ty of simple_ty
  | Path_type
  | Path_kind
  | Path_sing of expr
  | Path_ty_mod of ty_mod

and ty_mod = decl list
and ty_fun = param list * expr
and simple_ty = Ty_bool | Ty_unit
and param = Var.t option * expr
and decl = string * Var.t * expr [@@deriving sexp_of]
