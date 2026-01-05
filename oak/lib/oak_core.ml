open Core

open struct
  module Syntax = Oak_syntax
end

module Purity = Syntax.Purity

type expr =
  | Expr_var of string
  | Expr_abs of var_ty list * expr
  | Expr_ty_abs of var_ty * expr
  | Expr_pack of ty * expr * ty_exists
  | Expr_app of expr * expr list
  | Expr_app_ty of expr * ty
  | Expr_unpack of (string * string) * expr * expr
  | Expr_let of string * expr * expr
  | Expr_struct of var_expr list
  | Expr_if of expr * expr * expr

and ty =
  | Ty_var of string
  | Ty_bool
  | Ty_unit
  | Ty_sing of ty
  | Ty_fun of ty list * ty
  | Ty_abs of var_kind * ty
  | Ty_app of ty * ty
  | Ty_record of var_ty list
  | Ty_struct of var_ty list
  | Ty_proj of ty * string
  | Ty_forall of var_ty * ty
  | Ty_exists of ty_exists

and ty_exists = var_ty * ty

and kind =
  | Kind_type
  | Kind_sing of ty
  | Kind_record of var_kind list
  | Kind_fun of var_kind * kind

and var_expr = string * expr
and var_ty = string * ty
and var_kind = string * kind [@@deriving sexp_of]
