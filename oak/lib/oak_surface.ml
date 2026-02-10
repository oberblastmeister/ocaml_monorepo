open Prelude

open struct
  module Span = Utility.Span
  module Common = Oak_common
end

module Core_ty = Common.Core_ty
module Universe = Common.Universe

module Var = struct
  module T = struct
    type t =
      { name : string
      ; span : Span.t [@equal.ignore] [@compare.ignore] [@hash.ignore]
      }
    [@@deriving sexp_of, compare, equal, hash]
  end

  include T
  include Comparable.Make_plain (T)
  include Hashable.Make_plain (T)

  let create name span = { name; span }
end

type expr =
  | Expr_var of Var.t
  | Expr_ann of
      { e : expr
      ; ty : expr
      ; span : Span.t
      }
  | Expr_app of
      { func : expr
      ; args : expr list
      ; span : Span.t
      }
  | Expr_abs of
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
      { mod_e : expr
      ; field : string
      ; span : Span.t
      }
  | Expr_mod of
      { decls : decl list
      ; span : Span.t
      }
  | Expr_ty_mod of
      { ty_decls : ty_decl list
      ; span : Span.t
      }
  | Expr_block of
      { decls : block_decl list
      ; ret : expr
      ; span : Span.t
      }
    (* Also known as the singleton type, or the static extent.  *)
  | Expr_ty_sing of
      { e : expr
      ; span : Span.t
      }
  | Expr_bool of
      { value : bool
      ; span : Span.t
      }
  | Expr_unit of { span : Span.t }
  | Expr_number of
      { value : string
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
      { var : Var.t
      ; rhs : expr
      ; body : expr
      ; span : Span.t
      }

and block_decl =
  | Block_decl_let of
      { var : Var.t
      ; ann : expr option
      ; rhs : expr
      ; span : Span.t
      }
  | Block_decl_bind of
      { var : Var.t
      ; rhs : expr
      ; span : Span.t
      }

and decl =
  { var : Var.t
  ; ann : expr option
  ; e : expr
  ; span : Span.t
  }

and ty_decl =
  { var : Var.t
  ; ty : expr
  ; span : Span.t
  }
[@@deriving sexp_of]

and param =
  { vars : Var.t Non_empty_list.t
  ; ann : expr option
  ; span : Span.t
  }

and param_ty =
  { vars : Var.t list
  ; ty : expr
  ; span : Span.t
  }

let expr_span (e : expr) : Span.t =
  match e with
  | Expr_var { span; _ }
  | Expr_ann { span; _ }
  | Expr_app { span; _ }
  | Expr_abs { span; _ }
  | Expr_ty_fun { span; _ }
  | Expr_proj { span; _ }
  | Expr_mod { span; _ }
  | Expr_ty_mod { span; _ }
  | Expr_block { span; _ }
  | Expr_ty_sing { span; _ }
  | Expr_bool { span; _ }
  | Expr_unit { span; _ }
  | Expr_number { span; _ }
  | Expr_core_ty { span; _ }
  | Expr_universe { span; _ }
  | Expr_if { span; _ }
  | Expr_ty_pack { span; _ }
  | Expr_pack { span; _ }
  | Expr_bind { span; _ } -> span
;;
