open Prelude

open struct
  module Span = Utility.Span
  module Common = Oak_common
end

module Core_ty = Common.Core_ty
module Universe = Common.Universe

module Level = struct
  type t = { level : int } [@@unboxed] [@@deriving sexp_of, equal, compare]

  let of_int level =
    assert (level >= 0);
    { level }
  ;;
end

module Index = struct
  type t = { index : int } [@@unboxed] [@@deriving sexp_of, equal, compare]

  let of_int index =
    assert (index >= 0);
    { index }
  ;;

  let to_level context_size { index } = Level.of_int (context_size - index - 1)
  let of_level context_size { Level.level } = of_int (context_size - level - 1)
end

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

type 'a subst =
  | Shift of { amount : int }
  | Push of
      { times : int
      ; value : 'a
      ; subst : 'a subst
      }
[@@deriving sexp_of]

let rec apply_subst_index subst (var : Index.t) =
  match subst, var with
  | Push { times; value; subst }, { index; _ } ->
    assert (times > 0);
    assert (index >= 0);
    if index = 0 then value else apply_subst_index subst (Index.of_int (index - 1))
  | Shift { amount }, { index; _ } -> Index.of_int (index + amount)
;;

module Make_subst (Value : sig
    type t

    module Info : sig
      type t
    end

    val apply : t subst -> t -> t
    val of_index : Info.t -> int -> t
  end) =
struct
  type t = Value.t subst

  let push times value subst =
    assert (times >= 0);
    if times = 0 then subst else Push { times; value; subst }
  ;;

  let shift amount =
    assert (amount >= 0);
    Shift { amount }
  ;;

  let id = shift 0

  let rec compose ~first ~second =
    match first, second with
    | Push { times; value; subst }, subst' ->
      push times (Value.apply subst' value) (compose ~first:subst ~second:subst')
    | Shift { amount = i }, Shift { amount = j } -> Shift { amount = i + j }
    | Shift { amount = i }, Push { times; value; subst } ->
      if i < 0
      then assert false
      else if i = 0
      then second
      else begin
        if times < 0
        then assert false
        else if i > times
        then compose ~first:(Shift { amount = i - times }) ~second:subst
        else push (times - i) value subst
      end
  ;;

  let under info times subst =
    assert (times >= 0);
    let subst = compose ~first:subst ~second:(Shift { amount = times }) in
    let rec loop times subst =
      if times = 0
      then subst
      else
        loop
          (times - 1)
          (Push { times = 1; value = Value.of_index info (times - 1); subst })
    in
    loop times subst
  ;;

  let under1 info subst =
    Push
      { times = 1
      ; value = Value.of_index info 0
      ; subst = compose ~first:subst ~second:(Shift { amount = 1 })
      }
  ;;

  let apply = Value.apply
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
      ; span : Span.t
      }
  | Expr_abs of
      { var : Var_info.t
      ; param_ty : expr
      ; body : expr
      ; span : Span.t
      }
  | Expr_ty_fun of
      { var : Var_info.t
      ; param_ty : expr
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
    (* Also known as the singleton type, or the static extent.  *)
  | Expr_ty_sing of
      { e : expr
      ; span : Span.t
      }
  | Expr_bool of
      { value : bool
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
and term =
  | Term_var of Index.t
  | Term_app of
      { func : term
      ; arg : term
      }
  | Term_abs of
      { var : Var_info.t
      ; body : term
      }
  | Term_ty_fun of
      { var : Var_info.t
      ; param_ty : term_ty
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
  | Term_sing_wrap of
      { identity : term
      ; e : term
      }
    (* Used for subtyping coercions. See sub and eval functions. *)
  | Term_weaken of term
  | Term_sing_unwrap of
      { identity : term
      ; e : term
      }
  | Term_ty_pack of term_ty
  | Term_pack of term
  | Term_bind of
      { var : Var_info.t
      ; rhs : term
      ; body : term
      }
  | Term_universe of Universe.t
  | Term_core_ty of Core_ty.t
    (*
      This is not present in the surface language.
      We would need to use some quantitative type theory to handle this properly
      so we can ensure that it stays irrelevant.
    *)
  | Term_ignore
    (* Cannot be written in the source syntax for now, can only be used in an irrelevant way *)
  | Term_bool of { value : bool }
  | Term_if of
      { cond : term
      ; body1 : term
      ; body2 : term
      }

and term_ty = term

and term_field =
  { name : string
  ; e : term
  }

and term_ty_mod = { ty_decls : term_ty_decl list }

and term_ty_decl =
  { var : Var_info.t
  ; ty : term_ty
  }
[@@deriving sexp_of]

(* values *)
type value =
  (*
    Value_ignore is similar to declval in c++.
    It is okay to come up with a garbage value if it is only for the sake of typechecking.
    See the * value and the static lock in https://arxiv.org/pdf/2010.08599
  *)
  | Value_ignore
  | Value_mod of value_mod
  | Value_abs of value_abs
  | Value_sing_wrap of
      { identity : value
      ; e : value
      }
  | Value_core_ty of Core_ty.t
  | Value_neutral of neutral
  | Value_universe of Universe.t
  | Value_ty_sing of value_ty_sing
  | Value_ty_mod of value_ty_mod_closure
  | Value_ty_fun of value_ty_fun
  | Value_ty_pack of ty (* no value pack, because it will be ignored *)

and ty = value

and neutral =
  | Neutral_var of Level.t
  | Neutral_app of
      { func : neutral
      ; arg : value
      }
  | Neutral_proj of
      { mod_e : neutral
      ; field : string
      ; field_index : int
      }
    (*
        We technically only need Value_sing_wrap, because we can always apply the extension rule for unwrap.
        However, we keep it around because always applying this rule amounts to unfolding all types,
        which is bad for error messages.
      *)
  | Neutral_sing_unwrap of
      { identity : value
      ; e : neutral
      }

(*
  These is an ordered tuple.
  The field names are just for printing to the user.
  Reordering the fields is not automatic and requires a coercion.
*)
and value_mod = { fields : value_field list }

and value_abs =
  { var : Var_info.t
  ; body : value_closure
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

(* The u stands for unfolded *)
type uvalue =
  | Uvalue_ignore
  | Uvalue_mod of value_mod
  | Uvalue_abs of value_abs
  | Uvalue_sing_wrap of
      { identity : value
      ; e : value
      }
  | Uvalue_core_ty of Core_ty.t
  | Uvalue_neutral of uneutral
  | Uvalue_universe of Universe.t
  | Uvalue_ty_sing of value_ty_sing
  | Uvalue_ty_mod of value_ty_mod_closure
  | Uvalue_ty_fun of value_ty_fun
  | Uvalue_ty_pack of ty

(* It's neutral without sing_unwrap *)
and uneutral =
  | Uneutral_var of Level.t
  | Uneutral_app of
      { func : uneutral
      ; arg : value
      }
  | Uneutral_proj of
      { mod_e : uneutral
      ; field : string
      ; field_index : int
      }
[@@deriving sexp_of]

module Value = struct
  type t = value

  let var v = Value_neutral (Neutral_var v)
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

  let find_exn env index = find_exn_list env.list index
  let next_var t = Value.var (Level.of_int t.size)
end

module Neutral = struct
  type t = neutral
end

module Uneutral = struct
  type t = uneutral

  let rec to_neutral = function
    | Uneutral_var var -> Neutral_var var
    | Uneutral_app { func; arg } -> Neutral_app { func = to_neutral func; arg }
    | Uneutral_proj { mod_e; field; field_index } ->
      Neutral_proj { mod_e = to_neutral mod_e; field; field_index }
  ;;
end

module Uvalue = struct
  type t = uvalue

  let abs_val_exn = function
    | Uvalue_abs v -> v
    | _ -> failwith "not an abs value"
  ;;

  let ty_fun_val_exn = function
    | Uvalue_ty_fun v -> v
    | _ -> failwith "not a ty fun value"
  ;;

  let ty_mod_val_exn = function
    | Uvalue_ty_mod v -> v
    | _ -> failwith "not a ty mod value"
  ;;

  let mod_val_exn = function
    | Uvalue_mod v -> v
    | _ -> failwith "not a mod value"
  ;;

  let neutral_val_exn = function
    | Uvalue_neutral v -> v
    | _ -> failwith "not a neutral value"
  ;;

  let var_val_exn = function
    | Uvalue_neutral (Uneutral_var var) -> var
    | _ -> failwith "not a neutral var"
  ;;

  let ty_sing_val_exn = function
    | Uvalue_ty_sing sing -> sing
    | _ -> failwith "not a ty sing"
  ;;

  let universe_val_exn = function
    | Uvalue_universe u -> u
    | _ -> failwith "not a universe value"
  ;;

  let to_value = function
    | Uvalue_ignore -> Value_ignore
    | Uvalue_mod v -> Value_mod v
    | Uvalue_abs v -> Value_abs v
    | Uvalue_sing_wrap { identity; e } -> Value_sing_wrap { identity; e }
    | Uvalue_core_ty ty -> Value_core_ty ty
    | Uvalue_neutral n -> Value_neutral (Uneutral.to_neutral n)
    | Uvalue_universe u -> Value_universe u
    | Uvalue_ty_sing v -> Value_ty_sing v
    | Uvalue_ty_mod v -> Value_ty_mod v
    | Uvalue_ty_fun v -> Value_ty_fun v
    | Uvalue_ty_pack ty -> Value_ty_pack ty
  ;;
end

module Value_abs = struct
  type t = value_abs
end

module Index_subst = Make_subst (struct
    type t = Index.t

    module Info = Unit

    let apply = apply_subst_index
    let of_index () i = Index.of_int i
  end)
