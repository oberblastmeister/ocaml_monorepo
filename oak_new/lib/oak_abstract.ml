open Prelude

open struct
  module Span = Utility.Span
  module Common = Oak_common
end

module Core_ty = Common.Core_ty
module Name = Common.Name
module Size = Common.Size
module Index = Common.Index
module Level = Common.Level
module Literal = Common.Literal
module Icit = Common.Icit
module Relevancy = Common.Relevancy

type expr =
  | Expr_var of
      { index : Index.t
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
  | Expr_fun of
      { name : Name.t
      ; param_ty : expr option
      ; relevancy : Relevancy.t
      ; icit : Icit.t
      ; body : expr
      ; span : Span.t
      }
  | Expr_ty_fun of
      { name : Name.t
      ; param_ty : expr
      ; relevancy : Relevancy.t
      ; icit : Icit.t
      ; body_ty : expr
      ; span : Span.t
      }
  | Expr_proj of
      { strukt : expr
      ; field : string
      ; span : Span.t
      }
  | Expr_struct of
      { decls : expr_decl list
      ; span : Span.t
      }
  | Expr_ty_struct of
      { field_specs : expr_field_spec list
      ; span : Span.t
      }
  | Expr_let of
      { name : Name.t
      ; rhs : expr
      ; relevancy : Relevancy.t
      ; is_abstract : bool
      ; body : expr
      ; span : Span.t
      }
  | Expr_core_ty of
      { ty : Core_ty.t
      ; span : Span.t
      }
  | Expr_universe of
      { size : Size.t
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
      { name : Name.t
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
  { name : Name.t
  ; ty : expr
  ; e : expr
  }

and expr_decl =
  { name : Name.t
  ; relevancy : Relevancy.t
  ; e : expr
  ; span : Span.t
  }

and expr_field_spec =
  { name : Name.t
  ; relevancy : Relevancy.t
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
    | Expr_fun { span; _ }
    | Expr_ty_fun { span; _ }
    | Expr_proj { span; _ }
    | Expr_struct { span; _ }
    | Expr_ty_struct { span; _ }
    | Expr_let { span; _ }
    | Expr_core_ty { span; _ }
    | Expr_universe { span; _ }
    | Expr_if { span; _ }
    | Expr_ty_pack { span; _ }
    | Expr_pack { span; _ }
    | Expr_literal { span; _ }
    | Expr_rec { span; _ }
    | Expr_where { span; _ }
    | Expr_bind { span; _ } -> span
  ;;
end
