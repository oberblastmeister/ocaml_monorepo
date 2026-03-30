open Prelude
module Span = Utility.Span
module Common = Oak_common
module Core = Oak_core
module Core_ty = Common.Core_ty
module Name = Common.Name
module Size = Common.Size
module Index = Common.Index
module Literal = Common.Literal
module Relevancy = Common.Relevancy
module Param_modifiers = Common.Param_modifiers
module Context = Oak_context

type expr_ann =
  { span : Span.t
  ; cx : Context.t
  ; ty : Core.ty
  ; term : Core.term
  }
[@@deriving sexp_of]

type ty_ann =
  { span : Span.t
  ; cx : Context.t
  ; ty_props : Core.Ty_props.t
  ; term : Core.term_ty
  }
[@@deriving sexp_of]

type expr =
  | Expr_var of
      { index : Index.t
      ; ann : expr_ann
      }
  | Expr_ann of
      { e : expr
      ; ty : ty
      ; ann : expr_ann
      }
  | Expr_app of
      { func : expr
      ; arg : expr
      ; param_modifiers : Param_modifiers.t
      ; ann : expr_ann
      }
  | Expr_fun of
      { name : Name.t
      ; param_ty : ty option
      ; param_modifiers : Param_modifiers.t
      ; body : expr
      ; ann : expr_ann
      }
  | Expr_proj of
      { strukt : expr
      ; field : string
      ; ann : expr_ann
      }
  | Expr_struct of
      { decls : expr_decl list
      ; is_dependent : bool
      ; ann : expr_ann
      }
  | Expr_let of
      { name : Name.t
      ; rhs : expr
      ; relevancy : Relevancy.t
      ; is_abstract : bool
      ; body : expr
      ; ann : expr_ann
      }
  | Expr_if of
      { cond : expr
      ; body1 : expr
      ; body2 : expr
      ; ann : expr_ann
      }
  | Expr_pack of
      { e : expr
      ; ann : expr_ann
      }
  | Expr_bind of
      { name : Name.t
      ; rhs : expr
      ; body : expr
      ; ann : expr_ann
      }
  | Expr_literal of
      { literal : Literal.t
      ; ann : expr_ann
      }
  | Expr_error of { ann : expr_ann }
  | Expr_rec of
      { decls : expr_rec_decl list
      ; ann : expr_ann
      }
  | Expr_encode_ty of
      { ty : ty
      ; ann : expr_ann
      }
  | Expr_coe of
      { expr : expr
      ; coe : runtime_coe
      ; ann : expr_ann
      }
  | Expr_data_rec of expr_data_rec
  | Expr_data of
      { data : expr_data
      ; ann : expr_ann
      }

and expr_data_rec =
  { decls : data_decl list
  ; span : Span.t
  ; ann : expr_ann
  }

and ty =
  | Ty_decode of
      { expr : expr
      ; ann : ty_ann
      }
  | Ty_fun of
      { name : Name.t
      ; param_ty : ty
      ; param_modifiers : Param_modifiers.t
      ; body_ty : ty
      ; ann : ty_ann
      }
  | Ty_struct of
      { field_specs : field_spec list
      ; ann : ty_ann
      }
  | Ty_core of
      { ty : Core_ty.t
      ; ann : ty_ann
      }
  | Ty_universe of
      { size : Size.t
      ; ann : ty_ann
      }
  | Ty_pack of
      { ty : ty
      ; ann : ty_ann
      }
  | Ty_where of
      { e : ty
      ; path : string Non_empty_list.t
      ; rhs : expr
      ; ann : ty_ann
      }

and data_decl =
  { name : Name.t
  ; data : expr_data
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
  ; ty : ty
  }

and data_constructor =
  { name : Name.t
  ; ty : ty option
  }

and data_param =
  { name : Name.t
  ; ty : ty
  }

and runtime_coe =
  | Fun_coe of
      { (* this one is contravariant *)
        arg_coe : runtime_coe
      ; ret_coe : runtime_coe
      }
  | Id_coe
  | Struct_coe of runtime_field_coe list

and runtime_field_coe =
  { field : Core.field_loc
  ; coe : runtime_coe
  }

and expr_rec_decl =
  { name : Name.t
  ; ty : ty
  ; e : expr
  }

and expr_decl =
  { name : Name.t
  ; relevancy : Relevancy.t
  ; e : expr
  ; span : Span.t
  }

and field_spec =
  { name : Name.t
  ; relevancy : Relevancy.t
  ; ty : ty option
  ; rhs : expr option
  ; span : Span.t
  }
[@@deriving sexp_of]

module Ty_ann = struct
  type t = ty_ann
end

module Expr_ann = struct
  type t = expr_ann

  let of_ty_ann (ann : ty_ann) =
    { span = ann.span
    ; cx = ann.cx
    ; ty = Ty_universe ann.ty_props
    ; term = Term_encode_ty { ty = ann.term; props = ann.ty_props }
    }
  ;;
end

module Ty = struct
  type t = ty

  let ann = function
    | Ty_decode { ann; _ }
    | Ty_fun { ann; _ }
    | Ty_struct { ann; _ }
    | Ty_core { ann; _ }
    | Ty_universe { ann; _ }
    | Ty_pack { ann; _ }
    | Ty_where { ann; _ } -> ann
  ;;

  let props t = (ann t).ty_props
  let span t = (ann t).span
  let term t = (ann t).term
end

module Expr = struct
  type t = expr

  let ann = function
    | Expr_var { ann; _ }
    | Expr_ann { ann; _ }
    | Expr_app { ann; _ }
    | Expr_fun { ann; _ }
    | Expr_proj { ann; _ }
    | Expr_struct { ann; _ }
    | Expr_let { ann; _ }
    | Expr_if { ann; _ }
    | Expr_pack { ann; _ }
    | Expr_bind { ann; _ }
    | Expr_literal { ann; _ }
    | Expr_error { ann; _ }
    | Expr_rec { ann; _ }
    | Expr_coe { ann; _ }
    | Expr_data_rec { ann; _ }
    | Expr_data { ann; _ }
    | Expr_encode_ty { ann; _ } -> ann
  ;;

  let ty t = (ann t).ty
  let span t = (ann t).span
  let term t = (ann t).term

  let of_ty ty =
    let ann = Expr_ann.of_ty_ann (Ty.ann ty) in
    Expr_encode_ty { ty; ann }
  ;;
end
