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

module type Seq = sig
  type 'a t [@@deriving sexp]

  val empty : 'a t
  val push : 'a -> 'a t -> 'a t
  val pop : 'a t -> ('a * 'a t) option
  val pop_exn : 'a t -> 'a * 'a t
  val get_index : 'a t -> Index.t -> 'a option
  val get_level : 'a t -> Level.t -> 'a option
  val get_index_exn : 'a t -> Index.t -> 'a
  val get_level_exn : 'a t -> Level.t -> 'a
  val iter : 'a t -> f:('a -> unit) -> unit
  val to_list : 'a t -> 'a list
  val of_list : 'a list -> 'a t
  val length : 'a t -> int
end

module List_seq = struct
  type 'a t = 'a list [@@deriving sexp]

  let empty = []
  let push x xs = x :: xs

  let pop = function
    | [] -> None
    | x :: xs -> Some (x, xs)
  ;;

  let pop_exn = function
    | [] -> failwith "empty sequence"
    | x :: xs -> x, xs
  ;;

  let get xs i = List.nth xs i
  let get_exn xs i = List.nth_exn xs i
  let iter xs ~f = List.iter xs ~f
  let to_list xs = xs
  let of_list xs = xs
  let length = List.length
  let get_index xs (i : Index.t) = get xs i.index
  let get_level xs (l : Level.t) = get xs (Index.of_level (length xs) l).index
  let get_index_exn xs (i : Index.t) = get_exn xs i.index
  let get_level_exn xs (l : Level.t) = get_exn xs (Index.of_level (length xs) l).index
end

module Seq : Seq = List_seq

module Ty_props = struct
  type t = { size : Size.t } [@@deriving sexp_of]
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
      ; param_props : param_props
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

and param_props =
  { icit : Icit.t
  ; relevancy : Relevancy.t
  }

and field_loc =
  { name : string
  ; index : int
  }

and term_field_impl =
  { name : string
  ; e : term
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
      ; param_props : param_props
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

and ty_sing =
  { identity : value
  ; ty : ty
  }

and ty_struct =
  { env : env
  ; field_specs : term_field_spec list
  }

and neutral =
  { head : Level.t
  ; spine : spine
  }

and spine = frame Bwd.t

and term_arg =
  { e : term
  ; param_props : param_props
  }

and value_arg =
  { e : value
  ; param_props : param_props
  }

and frame =
  | App of value_arg
  | Proj of field_loc
  | Out

and value_struct = { field_impls : value_field_impl list }

and value_fun =
  { name : Name.t
  ; param_props : param_props
  ; body : value_closure
  }

and ty_fun =
  { name : Name.t
  ; param_props : param_props
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
  }

and env = value Seq.t
and ty_env = ty Seq.t [@@deriving sexp_of]

module Value = struct
  type t = value

  let free head = Value_neutral { head; spine = Empty }
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
