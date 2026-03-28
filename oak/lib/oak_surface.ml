open Prelude
module Span = Utility.Span
module Common = Oak_common
module Core_ty = Common.Core_ty
module Size = Common.Size
module Literal = Common.Literal
module Icit = Common.Icit
module Name = Common.Name
module Relevancy = Common.Relevancy

type expr =
  | Expr_var of Name.t
  | Expr_ann of
      { e : expr
      ; ty : expr
      ; span : Span.t
      }
  | Expr_app of
      { func : expr
      ; args : expr_arg list
      ; span : Span.t
      }
  | Expr_fun of
      { params : param Non_empty_list.t
      ; ret_ty : expr option
      ; body : expr
      ; span : Span.t
      }
  | Expr_ty_fun of
      { param_tys : param_ty Non_empty_list.t
      ; body_ty : expr
      ; span : Span.t
      }
  | Expr_proj of
      { strukt : expr
      ; field : string
      ; span : Span.t
      }
  | Expr_struct of
      { decls : block_decl list
      ; is_dependent : bool
      ; span : Span.t
      }
  | Expr_ty_struct of
      { field_specs : field_spec list
      ; span : Span.t
      }
  | Expr_block of
      { decls : block_decl list
      ; ret : expr
      ; span : Span.t
      }
  | Expr_literal of
      { literal : Literal.t
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
  | Expr_alias of
      { e : expr
      ; span : Span.t
      }
  | Expr_pack of
      { e : expr
      ; span : Span.t
      }
  | Expr_paren of
      { e : expr
      ; span : Span.t
      }
  | Expr_brack of
      { e : expr
      ; span : Span.t
      }
  | Expr_rec of
      { decls : val_decl list
      ; span : Span.t
      }
  | Expr_where of
      { e : expr
      ; patches : where_patch list
      ; span : Span.t
      }
  | Expr_data_rec of
      { decls : data_decl list
      ; span : Span.t
      }
  | Expr_data of expr_data

and data_decl =
  { name : Name.t
  ; data : expr_data
  ; span : Span.t
  }

and expr_data =
  { params : param list
  ; body : (data_field, data_constructor) Either.t list
  ; span : Span.t
  }

and data_field =
  { name : Name.t
  ; ty : expr
  }

and data_constructor =
  { name : Name.t
  ; ty : expr option
  }

and expr_arg =
  { arg : expr
  ; relevancy : Relevancy.t
  ; icit : Icit.t
  }

and where_patch =
  { path : string Non_empty_list.t
  ; rhs : expr
  ; span : Span.t
  }

and block_decl =
  | Block_decl_val of val_decl
  | Block_decl_bind of
      { name : Name.t
      ; rhs : expr
      ; span : Span.t
      }
  | Block_decl_do of
      { e : expr
      ; span : Span.t
      }

and val_decl =
  { relevancy : Relevancy.t
  ; name : Name.t
  ; ann : expr option
  ; is_abstract : bool
  ; rhs : expr
  ; span : Span.t
  }

and field_spec =
  { relevancy : Relevancy.t
  ; name : Name.t
  ; ty : expr option
  ; rhs : expr option
  ; span : Span.t
  }
[@@deriving sexp_of]

and param =
  { relevancy : Relevancy.t
  ; names : Name.t Non_empty_list.t
  ; ann : expr option
  ; icit : Icit.t
  ; span : Span.t
  }

and param_ty =
  { relevancy : Relevancy.t
  ; names : Name.t list
  ; ty : expr option (* can only be none when icit is Impl *)
  ; icit : Icit.t
  ; span : Span.t
  }

let expr_span (e : expr) : Span.t =
  match e with
  | Expr_var { span; _ }
  | Expr_ann { span; _ }
  | Expr_app { span; _ }
  | Expr_fun { span; _ }
  | Expr_ty_fun { span; _ }
  | Expr_proj { span; _ }
  | Expr_struct { span; _ }
  | Expr_ty_struct { span; _ }
  | Expr_block { span; _ }
  | Expr_core_ty { span; _ }
  | Expr_universe { span; _ }
  | Expr_if { span; _ }
  | Expr_ty_pack { span; _ }
  | Expr_alias { span; _ }
  | Expr_pack { span; _ }
  | Expr_literal { span; _ }
  | Expr_brack { span; _ }
  | Expr_rec { span; _ }
  | Expr_paren { span; _ }
  | Expr_data_rec { span; _ }
  | Expr_data { span; _ }
  | Expr_where { span; _ } -> span
;;
