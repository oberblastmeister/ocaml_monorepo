open Prelude

open struct
  module Span = Utility.Span
  module Common = Oak_common
end

module Var_info = Common.Var_info
module Core_ty = Common.Core_ty
module Universe = Common.Universe
module Index = Common.Index
module Level = Common.Level
module Literal = Common.Literal
module Icit = Common.Icit

type expr =
  | Expr_var of
      { var : Index.t
      ; span : Span.t
      }
  | Expr_ann of
      { e : expr
      ; ty : expr
      ; span : Span.t
      }
  | Expr_app of
      { func : expr
      ; arg : expr
      ; icit : Icit.t
      ; span : Span.t
      }
  | Expr_abs of
      { var : Var_info.t
      ; param_ty : expr option
      ; icit : Icit.t
      ; body : expr
      ; span : Span.t
      }
  | Expr_ty_fun of
      { var : Var_info.t
      ; param_ty : expr
      ; icit : Icit.t
      ; body_ty : expr
      ; span : Span.t
      }
  | Expr_proj of
      { mod_e : expr
      ; field : string
      ; span : Span.t
      }
  | Expr_mod of
      { decls : expr_decl list
      ; span : Span.t
      }
  | Expr_ty_mod of
      { ty_decls : expr_ty_decl list
      ; span : Span.t
      }
  | Expr_let of
      { var : Var_info.t
      ; rhs : expr
      ; body : expr
      ; span : Span.t
      }
  | Expr_alias of
      { identity : expr
      ; span : Span.t
      }
    (* Also known as the singleton type, or the static extent.  *)
  | Expr_ty_sing of
      { identity : expr
      ; span : Span.t
      }
  | Expr_core_ty of
      { ty : Core_ty.t
      ; span : Span.t
      }
  | Expr_universe of
      { universe : Universe.t
      ; span : Span.t
      }
  | Expr_if of
      { cond : expr
      ; body1 : expr
      ; body2 : expr
      ; span : Span.t
      }
  | Expr_ty_pack of
      { ty : expr
      ; span : Span.t
      }
  | Expr_pack of
      { e : expr
      ; span : Span.t
      }
  | Expr_bind of
      { var : Var_info.t
      ; rhs : expr
      ; body : expr
      ; span : Span.t
      }
  | Expr_literal of
      { literal : Literal.t
      ; span : Span.t
      }
  | Expr_error of { span : Span.t }
  | Expr_rec of
      { decls : expr_rec_decl list
      ; span : Span.t
      }
  | Expr_where of
      { e : expr
      ; path : string Non_empty_list.t
      ; rhs : expr
      ; span : Span.t
      }

and expr_rec_decl =
  { var : Var_info.t
  ; ty : expr
  ; e : expr
  }

and expr_decl =
  { var : Var_info.t
  ; e : expr
  ; span : Span.t
  }

and expr_ty_decl =
  { var : Var_info.t
  ; ty : expr
  ; span : Span.t
  }
[@@deriving sexp_of]

module Expr = struct
  let span = function
    | Expr_error { span; _ }
    | Expr_var { span; _ }
    | Expr_ann { span; _ }
    | Expr_app { span; _ }
    | Expr_abs { span; _ }
    | Expr_ty_fun { span; _ }
    | Expr_proj { span; _ }
    | Expr_mod { span; _ }
    | Expr_ty_mod { span; _ }
    | Expr_let { span; _ }
    | Expr_ty_sing { span; _ }
    | Expr_core_ty { span; _ }
    | Expr_universe { span; _ }
    | Expr_if { span; _ }
    | Expr_ty_pack { span; _ }
    | Expr_pack { span; _ }
    | Expr_alias { span; _ }
    | Expr_literal { span; _ }
    | Expr_rec { span; _ }
    | Expr_where { span; _ }
    | Expr_bind { span; _ } -> span
  ;;
end
