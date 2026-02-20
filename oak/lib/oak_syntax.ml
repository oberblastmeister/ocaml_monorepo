open Prelude

open struct
  module Span = Utility.Span
  module Common = Oak_common
end

module Core_ty = Common.Core_ty
module Universe = Common.Universe
module Index = Common.Index
module Level = Common.Level
module Literal = Common.Literal
module Icit = Common.Icit

let index = Index.of_int
let level = Level.of_int

module Var_info = struct
  type t =
    { name : string
    ; pos : int
    }
  [@@deriving sexp_of]

  let generated = { name = "<generated>"; pos = 0 }
end

(* Substitutes free variables into bound variables *)
module Close = struct
  type t =
    { map : int Int.Map.t
    ; lift : int
    }
  [@@deriving sexp_of]

  let empty = { map = Int.Map.empty; lift = 0 }
  let lift n (close : t) = { close with lift = close.lift + n }

  let singleton (level : Level.t) (index : Index.t) : t =
    { map = Int.Map.singleton level.level index.index; lift = 0 }
  ;;

  let add_exn (level : Level.t) (index : Index.t) (close : t) =
    { close with
      map = Map.add_exn close.map ~key:level.level ~data:(index.index - close.lift)
    }
  ;;

  let compose ~(second : t) ~(first : t) =
    let map =
      Map.merge first.map second.map ~f:(fun ~key:_ e ->
        Some
          (match e with
           | `Right v -> v - first.lift + second.lift
           | `Left v -> v
           | `Both (v1, _v2) -> v1))
    in
    { first with map }
  ;;

  let find (close : t) (level : Level.t) =
    Option.map
      ~f:(fun i -> Index.of_int (i + close.lift))
      (Map.find close.map level.level)
  ;;
end

(* raw syntax *)
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

and term_close =
  { e : term
  ; close : Close.t
  }

and term_ty = term

and term_field =
  { name : string
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
and meta = { mutable state : meta_state }

(* Can only range over values of kind Type for now *)
and meta_state =
  | Meta_unsolved of meta_state_unsolved
  | Meta_link of ty
  | Meta_solved of ty

and meta_state_unsolved =
  { var : Var_info.t
  ; id : int
  ; context_size : int
  }

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

let rec term_close e close =
  match e with
  | Term_bound v -> Term_bound v
  | Term_free i ->
    Close.find close i |> Option.value_map ~default:e ~f:(fun v -> Term_bound v)
  | Term_app { func; arg; icit } ->
    Term_app { func = term_close func close; arg = term_close arg close; icit }
  | Term_abs { var; body; icit } ->
    Term_abs { var; body = term_close body (Close.lift 1 close); icit }
  | Term_ty_fun { var; param_ty; icit; body_ty } ->
    Term_ty_fun
      { var
      ; param_ty = term_close param_ty close
      ; icit
      ; body_ty = term_close body_ty (Close.lift 1 close)
      }
  | Term_proj { mod_e; field; field_index } ->
    Term_proj { mod_e = term_close mod_e close; field; field_index }
  | Term_mod { fields } ->
    Term_mod
      { fields = List.map fields ~f:(fun { name; e } -> { name; e = term_close e close })
      }
  | Term_ty_mod { ty_decls } ->
    let _, ty_decls =
      List.fold_map ty_decls ~init:0 ~f:(fun under { var; ty } ->
        under + 1, { var; ty = term_close ty (Close.lift under close) })
    in
    Term_ty_mod { ty_decls }
  | Term_let { var; rhs; body } ->
    Term_let
      { var; rhs = term_close rhs close; body = term_close body (Close.lift 1 close) }
  | Term_ty_sing { identity; ty } ->
    Term_ty_sing { identity = term_close identity close; ty = term_close ty close }
  | Term_sing_in e -> Term_sing_in (term_close e close)
  | Term_sing_out e -> Term_sing_out (term_close e close)
  | Term_ty_pack ty -> Term_ty_pack (term_close ty close)
  | Term_pack e -> Term_pack (term_close e close)
  | Term_bind { var; rhs; body } ->
    Term_bind
      { var; rhs = term_close rhs close; body = term_close body (Close.lift 1 close) }
  | Term_universe u -> Term_universe u
  | Term_core_ty ty -> Term_core_ty ty
  | Term_literal lit -> Term_literal lit
  | Term_ignore -> Term_ignore
  | Term_if { cond; body1; body2 } ->
    Term_if
      { cond = term_close cond close
      ; body1 = term_close body1 close
      ; body2 = term_close body2 close
      }
  | Term_ty_meta meta -> Term_ty_meta meta
;;

module Term = struct
  let close c e = term_close e c

  let close_single (level : Level.t) e =
    term_close e (Close.singleton level (Index.of_int 0))
  ;;
end

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
    | Expr_bind { span; _ } -> span
  ;;
end

module Meta_unsolved = struct
  type t = meta_unsolved

  let to_meta (t : t) = t.meta

  (* let get (t : t) = match t.meta.state with
      | Meta_unsolved t -> t
      | _ -> failwith "should be meta unsolved" *)
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
