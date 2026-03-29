open Prelude
module Span = Utility.Span
module Common = Oak_common
module Core_ty = Common.Core_ty
module Name = Common.Name
module Size = Common.Size
module Index = Common.Index
module Literal = Common.Literal
module Relevancy = Common.Relevancy
module Param_modifiers = Common.Param_modifiers

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
      ; param_modifiers : Param_modifiers.t
      ; span : Span.t
      }
  | Expr_fun of
      { name : Name.t
      ; param_ty : expr option
      ; param_modifiers : Param_modifiers.t
      ; body : expr
      ; span : Span.t
      }
  | Expr_ty_fun of
      { name : Name.t
      ; param_ty : expr
      ; param_modifiers : Param_modifiers.t
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
      ; is_dependent : bool
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
  | Expr_data_rec of expr_data_rec
  | Expr_data of expr_data

and data_decl =
  { name : Name.t
  ; data : expr_data
  ; span : Span.t
  }

and expr_data_rec =
  { decls : data_decl list
  ; span : Span.t
  }

and expr_data =
  { params : data_param list
  ; body : data_body
  ; span : Span.t
  }

and data_body =
  | Data_record of { fields : data_field list }
  | Data_variant of { constructors : data_constructor list }

and data_field =
  { name : Name.t
  ; ty : expr
  }

and data_constructor =
  { name : Name.t
  ; ty : expr option
  }

and data_param =
  { name : Name.t
  ; ty : expr
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
  ; is_abstract : bool
  ; span : Span.t
  }

and expr_field_spec =
  { name : Name.t
  ; relevancy : Relevancy.t
  ; ty : expr option
  ; rhs : expr option
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
    | Expr_data { span; _ }
    | Expr_data_rec { span; _ }
    | Expr_bind { span; _ } -> span
  ;;
end
