open Prelude

open struct
  module Span = Utility.Span
  module Bwd = Utility.Bwd
  module Common = Oak_common
end

module Name = Common.Name
module Core_ty = Common.Core_ty
module Size = Common.Size
module Index = Common.Index
module Level = Common.Level
module Literal = Common.Literal
module Icit = Common.Icit
module Relevancy = Common.Relevancy
module Param_modifiers = Common.Param_modifiers

module Seq = struct
  include Utility.Seq

  let get_index t (i : Index.t) = Utility.Seq.get t i.index
  let get_level t l = get_index t (Index.of_level (Utility.Seq.length t) l)
  let get_index_exn t (i : Index.t) = Utility.Seq.get_exn t i.index
  let get_level_exn t l = get_index_exn t (Index.of_level (Utility.Seq.length t) l)
end

type ty_props = { size : Size.t } [@@deriving sexp_of]

module Ty_props = struct
  type t = ty_props [@@deriving sexp_of]
end

type term =
  | Term_bound of Index.t
  | Term_free of Level.t
  | Term_app of
      { func : term
      ; arg : term_arg
      }
  | Term_fun of
      { name : Name.t
      ; param_modifiers : Param_modifiers.t
      ; body : term
      }
  | Term_proj of
      { strukt : term
      ; field : field_loc
      }
  | Term_struct of { field_impls : term_field_impl list }
  | Term_encode_ty of
      { ty : term_ty
      ; props : Ty_props.t
      }
  | Term_sing_in of term
  | Term_sing_out of term
  | Term_let of
      { name : Name.t
      ; rhs : term
      ; body : term
      }
  | Term_ignore
  | Term_data of term_data
  | Term_data_rec of term_data_rec

(* binds num_params variables *)
and term_data =
  { num_params : int
  ; body : term_data_body
  ; ty : term_ty
  }

(* binds one variable which is self *)
and term_data_rec =
  { decls : term_data_decl list
  ; ty : term_ty
  }

(* binds num_params variables *)
and term_data_decl =
  { name : Name.t
  ; num_params : int
  ; body : term_data_body
  }

and term_data_param =
  { name : Name.t
  ; ty : term_ty
  }

and term_data_body =
  | Term_data_record of { fields : term_data_field list }
  | Term_data_variant of { constructor : term_data_constructor list }

and term_data_field =
  { name : Name.t
  ; ty : term_ty
  }

and term_data_constructor =
  { name : Name.t
  ; ty : term_ty option
  }

and field_loc =
  { name : string
  ; index : int
  }

and term_field_impl =
  { name : string
  ; e : term
  ; relevancy : Relevancy.t
  }

and term_field_spec =
  { name : Name.t
  ; ty : term_ty
  ; relevancy : Relevancy.t
  }

and term_ty =
  | Term_ty_decode of term
  | Term_ty_fun of
      { name : Name.t
      ; param_ty : term_ty
      ; param_modifiers : Param_modifiers.t
      ; body_ty : term_ty
      }
  | Term_ty_struct of term_ty_struct
  | Term_ty_sing of
      { identity : term
      ; ty : term_ty
      }
  | Term_ty_pack of term_ty
  | Term_ty_core of Core_ty.t
  | Term_ty_universe of Ty_props.t

and term_ty_struct = { field_specs : term_field_spec list }

and value =
  | Value_ignore
  | Value_struct of value_struct
  | Value_fun of value_fun
  | Value_sing_in of value
  | Value_neutral of neutral
  | Value_encode_ty of
      { ty : ty
      ; props : Ty_props.t
      }

and ty =
  | Ty_universe of Ty_props.t
  | Ty_sing of ty_sing
  | Ty_struct of ty_struct
  | Ty_fun of ty_fun
  | Ty_core of Core_ty.t
  | Ty_pack of ty
  | Ty_decode of neutral

(* env takes one argument, which is self, env is scoped in decls *)
and value_data_rec =
  { env : env
  ; decls : term_data_decl list
  ; ty : ty
    (* ty should be a non-dependent structure type with Type valued function types *)
  }

and value_data_decl =
  { name : Name.t
  ; data : value_data
  }

(* env takes zero arguments env is scoped in decls *)
and value_data =
  { env : env
  ; num_params : int
  ; body : term_data_body
  ; ty : ty (* ty should be Type valued function type *)
  }

and ty_sing =
  { identity : value
  ; ty : ty
  }

and ty_struct =
  { env : env
  ; field_specs : term_field_spec list
  }

and head =
  | Free of Level.t
  | Data of value_data
  | Data_rec of value_data_rec

and neutral =
  { head : head
  ; spine : spine
  }

and spine = frame Bwd.t

and term_arg =
  { e : term
  ; param_modifiers : Param_modifiers.t
  }

and value_arg =
  { e : value
  ; param_modifiers : Param_modifiers.t
  }

and frame =
  | App of value_arg
  | Proj of field_loc
  | Out

and value_struct = { field_impls : value_field_impl list }

and value_fun =
  { name : Name.t
  ; param_modifiers : Param_modifiers.t
  ; body : value_closure
  }

and ty_fun =
  { name : Name.t
  ; param_modifiers : Param_modifiers.t
  ; param_ty : ty
  ; body_ty : ty_closure
  }

and ty_closure =
  { env : env
  ; body : term_ty
  }

and value_closure =
  { env : env
  ; body : term
  }

and value_field_impl =
  { name : string
  ; e : value
  ; relevancy : Relevancy.t
  }

and env = value Seq.t
and ty_env = ty Seq.t [@@deriving sexp_of]

module Neutral = struct
  type t = neutral

  let of_head head : t = { head; spine = Bwd.Empty }
end

module Value = struct
  type t = value

  let of_head head = Value_neutral (Neutral.of_head head)
  let free head = of_head (Free head)
  let free_of_size size = free (Level.of_int size)

  let abs_val_exn = function
    | Value_fun v -> v
    | _ -> failwith "not a fun value"
  ;;

  let mod_val_exn = function
    | Value_struct v -> v
    | _ -> failwith "not a struct value"
  ;;

  let neutral_val_exn = function
    | Value_neutral v -> v
    | _ -> failwith "not a neutral value"
  ;;

  let var_val_exn = function
    | Value_neutral { head = var; spine = Empty } -> var
    | _ -> failwith "not a neutral var"
  ;;
end

module Ty = struct
  let ty_fun_val_exn = function
    | Ty_fun v -> v
    | _ -> failwith "not a ty fun value"
  ;;

  let ty_struct_val_exn = function
    | Ty_struct v -> v
    | _ -> failwith "not a ty mod value"
  ;;

  let ty_sing_val_exn = function
    | Ty_sing sing -> sing
    | _ -> failwith "not a ty sing"
  ;;

  let ty_universe_val_exn = function
    | Ty_universe u -> u
    | _ -> failwith "not a universe value"
  ;;
end
