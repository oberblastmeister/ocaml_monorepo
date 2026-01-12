open Core

module Purity = struct
  module T = struct
    type t = Pure | Impure [@@deriving sexp_of, compare, equal]
  end

  include T
  include Base.Comparable.Make (T)

  let merge x y =
    match (x, y) with
    | Impure, _ -> Impure
    | _, Impure -> Impure
    | Pure, Pure -> Pure
end

module Transparency = struct
  module T = struct
    type t = Transparent | Opaque [@@deriving sexp_of, compare, equal]
  end

  include T
  include Base.Comparable.Make (T)

  let merge x y =
    match (x, y) with
    | Opaque, _ -> Opaque
    | _, Opaque -> Opaque
    | Transparent, Transparent -> Transparent
end

module Universe = struct
  module T = struct
    type t = Type | Kind | KIND [@@deriving sexp, equal, compare]
  end

  include T
  include Base.Comparable.Make (T)

  let minimum = Type
  let maximum = KIND
  let to_int = function Type -> 0 | Kind -> 1 | KIND -> 2

  let of_int_exn = function
    | 0 -> Type
    | 1 -> Kind
    | 2 -> KIND
    | _ -> failwith "invalid universe"

  let lub u v = of_int_exn (Int.max (to_int u) (to_int v))
  let incr_exn u = of_int_exn (to_int u + 1)
end

module Var = struct
  module T = struct
    type t = { id : int; name : string } [@@deriving sexp, compare, equal]
  end

  include Comparable.Make (T)
  include T

  let create name id = { name; id }
end

(* TODO: add names to all these *)
type expr =
  | Expr_var of Var.t
  | Expr_seal of { e : expr; ty : expr }
  | Expr_app of { e : expr; es : expr list }
  | Expr_abs of { params : param list; body : expr; purity : Purity.t }
  | Expr_ty_fun of expr_ty_fun
  | Expr_proj of { e : expr; field : string }
  | Expr_mod of decl list
  | Expr_ty_mod of ty_mod
  | Expr_let of { var : Var.t; rhs : expr; body : expr }
  | Expr_ty_sing of expr_ty_sing
  | Expr_bool of bool
  | Expr_unit
  | Expr_core_ty of core_ty
  | Expr_universe of Universe.t
  (* invariant: path for expr_irrelevant is normalized *)
  | Expr_irrelevant of path
  | Expr_if of { e1 : expr; e2 : expr; e3 : expr }

and expr_ty_sing = { e : expr; ty : expr }
and expr_ty_fun = { params : param list; ty : expr; purity : Purity.t }

and path =
  | Path_var of Var.t
  | Path_core_ty of core_ty
  | Path_universe of Universe.t
  | Path_ty_sing of path_ty_sing
  | Path_ty_mod of path_ty_decl list
  | Path_ty_fun of path_ty_fun
  | Path_app of { e : path; es : value list }
  | Path_proj of { e : path; field : string }

and path_ty_sing = { e : path; ty : path }
and path_ty_fun = { params : path_param list; ty : path; purity : Purity.t }
and path_ty_decl = { field : string; var : Var.t; ty : path }
and path_param = { var : Var.t; ty : path }

and value =
  | Value_irrelevant
  | Value_path of path
  | Value_mod of value_decl list
  | Value_abs of { params : path_param list; body : value; purity : Purity.t }

and value_decl = { field : string; var : Var.t; e : value }
and ty_mod = ty_decl list
and core_ty = Ty_bool | Ty_unit
and param = { var : Var.t; ty : expr }
and decl = { field : string; var : Var.t; e : expr }
and ty_decl = { field : string; var : Var.t; ty : expr } [@@deriving sexp_of]

let path_universe_exn = function
  | Path_universe u -> u
  | _ -> failwith "not a universe type"

let path_ty_fun_exn = function
  | Path_ty_fun p -> p
  | _ -> failwith "not a function type"

let path_ty_mod_exn = function
  | Path_ty_mod p -> p
  | _ -> failwith "not a module type"

let value_path_exn = function
  | Value_path p -> p
  | Value_abs _ | Value_mod _ | Value_irrelevant _ -> failwith "not a path"

(* let rec path_to_expr = function
  | Path_var v -> Expr_var v
  | Path_core_ty ty -> Expr_core_ty ty
  | Path_universe u -> Expr_universe u
  | Path_ty_sing { e; ty } ->
      Expr_ty_sing { e = path_to_expr e; ty = path_to_expr ty }
  | Path_ty_mod decls -> Expr_ty_mod (List.map decls ~f:path_ty_decl_to_ty_decl)
  | Path_ty_fun { params; ty; purity } ->
      Expr_ty_fun
        {
          params = List.map params ~f:path_param_to_param;
          ty = path_to_expr ty;
          purity;
        }
  | Path_app { e; es } ->
      Expr_app { e = path_to_expr e; es = List.map es ~f:value_to_expr }
  | Path_proj { e; field } -> Expr_proj { e = path_to_expr e; field }

and value_to_expr = function
  | Value_path p -> path_to_expr p
  | Value_irrelevant ty -> Expr_irrelevant ty
  | Value_mod decls ->
      Expr_mod
        (List.map decls ~f:(fun { field; var; e } ->
             ({ field; var; e = value_to_expr e } : decl)))
  | Value_abs { params; body; purity } ->
      Expr_abs
        {
          params = List.map params ~f:path_param_to_expr;
          body = value_to_expr body;
          purity;
        }

and path_param_to_expr ({ var; ty } : path_param) : param =
  { var; ty = path_to_expr ty }

and path_ty_decl_to_ty_decl { field; var; ty } : ty_decl =
  { field; var; ty = path_to_expr ty }

and path_param_to_param { var; ty } : param = { var; ty = path_to_expr ty } *)
