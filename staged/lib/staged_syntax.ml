open Prelude
module Var = Staged_var

type bin_op =
  | Add
  | Sub
[@@deriving sexp_of]

module Stage = struct
  type t =
    | Runtime
    | Comptime
  [@@deriving sexp_of, equal, compare]
end

let%test_unit "stage" = [%test_eq: int] (Stage.compare Runtime Comptime) (-1)

type expr =
  | Expr_fun of expr_fun
  | Expr_app of
      { fn : expr
      ; arg : expr
      ; ann : ann
      }
  | Expr_int of int
  | Expr_bin of
      { lhs : expr
      ; op : bin_op
      ; rhs : expr
      }
  | Expr_let of
      { var : Var.t
      ; expr : expr
      ; body : expr
      ; ann : ann
      }
  | Expr_var of
      { var : Var.t
      ; ann : ann
      }

and expr_fun =
  { param_var : Var.t
  ; param_ty : ty
  ; stage : Stage.t
  ; body : expr
  ; ann : ann
  }

and ty =
  | Ty_fun of ty_fun
  | Ty_int

and ty_fun =
  { param : ty
  ; stage : Stage.t
  ; ret : ty
  }

and kind = Kind_type of Stage.t [@@deriving sexp_of]
and ann = ty option

(* equivalent to getting the stage of the kind of the type *)
let ty_stage ty =
  match ty with
  | Ty_fun t -> t.stage
  | Ty_int -> Runtime
;;

let rec get_ty_exn e =
  match e with
  | Expr_fun { ann; _ } -> Option.value_exn ann
  | Expr_app { ann; _ } -> Option.value_exn ann
  | Expr_int _ -> Ty_int
  | Expr_bin _ -> Ty_int
  | Expr_let { ann; _ } -> Option.value_exn ann
  | Expr_var { ann; _ } -> Option.value_exn ann
;;

let get_ty_stage ty =
  match ty with
  | Ty_fun { stage; _ } -> stage
  | Ty_int -> Runtime
;;

let ty_fun_exn = function
  | Ty_fun t -> t
  | ty -> raise_s [%message "Expected function type" (ty : ty)]
;;
