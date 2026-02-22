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

(* internal syntax *)
type term =
  | Term_bound of Index.t
  | Term_free of Level.t
  | Term_app of
      { func : term
      ; arg : term
      ; icit : Icit.t (* implicit applications are irrelevant *)
      }
  | Term_abs of
      { var : Var_info.t
      ; body : term
      ; icit : Icit.t (* implicit applications are irrelevant *)
      }
  (* | Term_ty_meta of value_meta ref *)
  | Term_ty_fun of
      { var : Var_info.t
      ; param_ty : term_ty
      ; icit : Icit.t
      ; body_ty : term_ty
      }
  | Term_proj of
      { mod_e : term
      ; field : string
      ; field_index : int
      }
  | Term_mod of { fields : term_field list }
  | Term_ty_mod of term_ty_mod
  | Term_let of
      { var : Var_info.t
      ; rhs : term
      ; body : term
      }
  | Term_ty_sing of
      { identity : term
      ; ty : term
      }
  | Term_sing_in of term
  | Term_sing_out of term
  | Term_ty_pack of term_ty
  | Term_pack of term
  | Term_bind of
      { var : Var_info.t
      ; rhs : term
      ; body : term
      }
  | Term_universe of Universe.t
  | Term_core_ty of Core_ty.t
  | Term_literal of Literal.t
  (*
      This is not present in the surface language.
      We would need to use some quantitative type theory to handle this properly
      so we can ensure that it stays irrelevant.
    *)
  | Term_ignore
    (* Cannot be written in the source syntax for now, can only be used in an irrelevant way *)
  | Term_if of
      { cond : term
      ; body1 : term
      ; body2 : term
      }
  | Term_ty_meta of meta
  (* This binds n variables at once *)
  | Term_rec of term_rec_decl list

and term_ty = term

and term_field =
  { name : string
  ; e : term
  }

and term_rec_decl =
  { var : Var_info.t
  ; e : term
  }

and term_ty_mod = { ty_decls : term_ty_decl list }

(* The var represents both the field name and the binder name *)
and term_ty_decl =
  { var : Var_info.t
  ; ty : term_ty
  }
[@@deriving sexp_of]

(* values *)
and ('neutral, 'meta) value_gen =
  (*
    Value_ignore is similar to declval in c++.
    It is okay to come up with a garbage value if it is only for the sake of typechecking.
    See the * value and the static lock in https://arxiv.org/pdf/2010.08599.
    
    TODO: need to keep track of the type here for singletons.
    
    If type is not None, then it may contain singletons.
  *)
  | Value_ignore
  | Value_mod of value_mod
  | Value_abs of value_abs
  | Value_sing_in of value
  | Value_core_ty of Core_ty.t
  | Value_neutral of 'neutral
  | Value_universe of Universe.t
  | Value_ty_meta of 'meta
  | Value_ty_sing of value_ty_sing
  | Value_ty_mod of value_ty_mod_closure
  | Value_ty_fun of value_ty_fun
  | Value_ty_pack of ty (* no value pack, because it will be ignored *)

and value = (neutral, meta) value_gen
and ty = value
and whnf = (whnf_neutral, meta_unsolved) value_gen

and elim =
  | Elim_app of
      { arg : value
      ; icit : Icit.t
      }
  | Elim_proj of
      { field : string
      ; field_index : int
      }
  | Elim_out

(* It's elim without sing_out *)
and whnf_elim =
  | Whnf_elim_app of
      { arg : value
      ; icit : Icit.t
      }
  | Whnf_elim_proj of
      { field : string
      ; field_index : int
      }

and neutral =
  { head : Level.t
  ; spine : spine
  }

and whnf_neutral =
  { head : Level.t
  ; spine : whnf_spine
  }

and spine = elim Bwd.t
and whnf_spine = whnf_elim Bwd.t

and meta =
  { var : Var_info.t
  ; created_at : Span.t
  ; id : int
  ; mutable context_size : int
  ; mutable state : meta_state
  }

(* Can only range over values of kind Type for now *)
and meta_state =
  | Meta_unsolved
  | Meta_link of ty
  | Meta_solved of ty

and meta_unsolved = { meta : meta }

(*
  These is an ordered tuple.
  The field names are just for printing to the user.
  Reordering the fields is not automatic and requires a coercion.
*)
and value_mod = { fields : value_field list }

and value_abs =
  { var : Var_info.t
  ; body : value_closure
  ; icit : Icit.t
  }

and value_ty_sing =
  { identity : value
  ; ty : ty
  }

and value_ty_mod_closure =
  { env : env
  ; ty_decls : term_ty_decl list
  }

and value_ty_fun =
  { var : Var_info.t
  ; param_ty : ty
  ; icit : Icit.t
  ; body_ty : value_closure
  }

(* technically, clsoures  *)
and value_closure =
  { env : env
  ; body : term
  }

and value_field =
  { name : string
  ; e : value
  }

and value_param =
  { var : Var_info.t
  ; ty : ty
  }

and env_list =
  | Env_empty
  | Env_push of
      { value : value
      ; rest : env_list
      }
[@@deriving sexp_of]

and env =
  { size : int
  ; list : env_list
  }

module Meta = struct
  type t = meta

  let pp (meta : t) =
    Doc.char '?'
    ^^ Doc.string meta.var.name
    ^^ Doc.char '_'
    ^^ Doc.string (Int.to_string meta.id)
  ;;
end

module Meta_unsolved = struct
  type t = meta_unsolved

  let to_meta (t : t) = t.meta

  let adjust_context_size t new_size =
    begin match t.meta.state with
    | Meta_unsolved -> ()
    | _ -> failwith "should be meta unsolved"
    end;
    t.meta.context_size <- new_size
  ;;

  let link_to (t : t) ty =
    match t.meta.state with
    | Meta_unsolved -> t.meta.state <- Meta_link ty
    | _ -> failwith "should be meta unsolved"
  ;;
end

module Whnf_elim = struct
  type t = whnf_elim

  let to_elim = function
    | Whnf_elim_app { arg; icit } -> Elim_app { arg; icit }
    | Whnf_elim_proj { field; field_index } -> Elim_proj { field; field_index }
  ;;
end

module Whnf_neutral = struct
  type t = whnf_neutral

  let rec to_neutral ({ head; spine } : t) =
    { head; spine = Bwd.map ~f:Whnf_elim.to_elim spine }
  ;;
end

module Value_gen = struct
  type ('neutral, 'meta) t = ('neutral, 'meta) value_gen [@@deriving sexp_of]

  let map ~map_neutral ~map_meta t =
    match t with
    | Value_ignore -> Value_ignore
    | Value_mod m -> Value_mod m
    | Value_abs abs -> Value_abs abs
    | Value_sing_in v -> Value_sing_in v
    | Value_core_ty ct -> Value_core_ty ct
    | Value_neutral n -> Value_neutral (map_neutral n)
    | Value_universe u -> Value_universe u
    | Value_ty_meta m -> Value_ty_meta (map_meta m)
    | Value_ty_sing s -> Value_ty_sing s
    | Value_ty_mod m -> Value_ty_mod { env = m.env; ty_decls = m.ty_decls }
    | Value_ty_fun f -> Value_ty_fun f
    | Value_ty_pack ty -> Value_ty_pack ty
  ;;
end

module Value = struct
  type t = value

  let free v = Value_neutral { head = v; spine = Empty }
end

module Whnf = struct
  type t = whnf

  let to_value t =
    Value_gen.map ~map_neutral:Whnf_neutral.to_neutral ~map_meta:Meta_unsolved.to_meta t
  ;;

  let abs_val_exn = function
    | Value_abs v -> v
    | _ -> failwith "not an abs value"
  ;;

  let ty_fun_val_exn = function
    | Value_ty_fun v -> v
    | _ -> failwith "not a ty fun value"
  ;;

  let ty_mod_val_exn = function
    | Value_ty_mod v -> v
    | _ -> failwith "not a ty mod value"
  ;;

  let mod_val_exn = function
    | Value_mod v -> v
    | _ -> failwith "not a mod value"
  ;;

  let neutral_val_exn = function
    | Value_neutral v -> v
    | _ -> failwith "not a neutral value"
  ;;

  let var_val_exn = function
    | Value_neutral { head = var; spine = Empty } -> var
    | _ -> failwith "not a neutral var"
  ;;

  let ty_sing_val_exn = function
    | Value_ty_sing sing -> sing
    | _ -> failwith "not a ty sing"
  ;;

  let universe_val_exn = function
    | Value_universe u -> u
    | _ -> failwith "not a universe value"
  ;;
end

(* might want to change this to use random access lists, they allow pushing i values in O(log(n)) and indexing in O(log(n)) *)
module Env = struct
  type t = env [@@deriving sexp_of]

  let empty = { size = 0; list = Env_empty }
  let push value env = { size = env.size + 1; list = Env_push { value; rest = env.list } }

  let pop_exn t =
    match t.list with
    | Env_empty -> failwith "Env.pop_exn: list was empty"
    | Env_push { value = _; rest } -> { size = t.size - 1; list = rest }
  ;;

  let size env = env.size

  let rec find_exn_list env index =
    let index = index.Index.index in
    match env with
    | Env_empty ->
      raise (Not_found_s [%message "Index not found" (env : env_list) (index : int)])
    | Env_push { value; rest } ->
      if index = 0 then value else find_exn_list rest (Index.of_int (index - 1))
  ;;

  let find_index_exn env index = find_exn_list env.list index

  let find_level_exn env (level : Level.t) =
    find_index_exn env (Index.of_int (env.size - level.level - 1))
  ;;

  let next_free t = Value.free (Level.of_int t.size)
end
