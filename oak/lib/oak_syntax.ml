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

module Universe = struct
  module T = struct
    (* rename KIND *)
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
  let stamp = ref 0

  module T = struct
    type t = { id : int; name : string } [@@deriving sexp, compare, equal]
  end

  include Comparable.Make (T)
  include T

  let create name =
    let id = !stamp in
    incr stamp;
    { name; id }

  let make_fresh var =
    let id = !stamp in
    incr stamp;
    { var with id }
end

module Record_var = struct
  let stamp = ref 0

  include Int

  let create () =
    let id = !stamp in
    incr stamp;
    id
end

(* compound variable *)
module Cvar = struct
  module T = struct
    type t =
      | Var of Var.t
      | Record_field of { var : Record_var.t; field : string }
    [@@deriving sexp, compare, equal]
  end

  include Comparable.Make (T)
  include T

  let create name = Var (Var.create name)
  let create_field field = Record_field { var = Record_var.create (); field }
end

(* TODO: add names to all these *)
type expr =
  | Expr_var of Cvar.t
  | Expr_seal of { e : expr; ty : expr }
  | Expr_app of { e : expr; es : expr list }
  | Expr_abs of { params : param list; body : expr; purity : Purity.t }
  | Expr_ty_fun of expr_ty_fun
  | Expr_proj of { e : expr; field : string }
  | Expr_mod of { var : Record_var.t; decls : decl list }
  | Expr_ty_mod of ty_mod
  | Expr_let of { var : Var.t; rhs : expr; body : expr }
  | Expr_ty_sing of expr_ty_sing
  | Expr_bool of bool
  | Expr_unit
  | Expr_core_ty of core_ty
  | Expr_universe of Universe.t
  (* invariant: path for expr_irrelevant is normalized *)
  | Expr_if of { e1 : expr; e2 : expr; e3 : expr }

and expr_ty_sing = { e : expr; ty : expr }
and expr_ty_fun = { params : param list; ty : expr; purity : Purity.t }

(* separate out the long idents *)
and path =
  | Path_long_ident of long_ident
  | Path_core_ty of core_ty
  | Path_universe of Universe.t
  | Path_ty_sing of path_ty_sing
  | Path_ty_mod of path_ty_mod
  | Path_ty_fun of path_ty_fun

and long_ident =
  | Long_ident_app of { e : long_ident; args : value list }
  | Long_ident_proj of { e : long_ident; field : string }
  | Long_ident_var of Cvar.t

and path_ty_sing = { e : path; ty : path }
and path_ty_mod = { var : Record_var.t; ty_decls : path_ty_decl list }
and path_ty_fun = { params : path_param list; ty : path; purity : Purity.t }
and path_ty_decl = { field : string; ty : path }
and path_param = { var : Var.t; ty : path }

and value =
  | Value_irrelevant
  | Value_path of path
  | Value_mod of value_mod
  | Value_abs of value_abs

(* TODO: for value mod it should be non dependent *)
and value_mod = { decls : value_decl list }
and value_abs = { params : path_param list; body : value; purity : Purity.t }
and value_decl = { field : string; e : value }
and ty_mod = { var : Record_var.t; ty_decls : ty_decl list }
and core_ty = Ty_bool | Ty_unit
and param = { var : Var.t; ty : expr }
and decl = { field : string; e : expr }
and ty_decl = { field : string; ty : expr } [@@deriving sexp_of]

let path_var var = Value_path (Path_long_ident (Long_ident_var (Var var)))
let is_path_universe = function Path_universe _ -> true | _ -> false

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

let make_params_fresh (subst, params) =
  List.fold_map params ~init:subst ~f:(fun subst { var; ty } ->
      let fresh_var = Var.make_fresh var in
      ( Map.set subst ~key:var ~data:fresh_var,
        ({ var = fresh_var; ty } : path_param) ))

let rec subst_var_value (subst : Var.t Var.Map.t) (value : value) : value =
  match value with
  | Value_irrelevant -> Value_irrelevant
  | Value_path path -> Value_path (subst_var_path subst path)
  | Value_mod { decls } ->
      let decls =
        List.map decls ~f:(fun { field; e } ->
            { field; e = subst_var_value subst e })
      in
      Value_mod { decls }
  | Value_abs { params; body; purity } ->
      let subst, params = make_params_fresh (subst, params) in
      let body = subst_var_value subst body in
      Value_abs { params; body; purity }

and subst_var_long_ident (subst : Var.t Var.Map.t) (lident : long_ident) :
    long_ident =
  match lident with
  | Long_ident_var (Var var) ->
      Map.find subst var
      |> Option.map ~f:(fun var -> Long_ident_var (Var var))
      |> Option.value ~default:lident
  | Long_ident_var (Record_field _) -> lident
  | Long_ident_app { e; args } ->
      let e = subst_var_long_ident subst e in
      let args = List.map args ~f:(subst_var_value subst) in
      Long_ident_app { e; args }
  | Long_ident_proj { e; field } ->
      Long_ident_proj { e = subst_var_long_ident subst e; field }

and subst_var_path (subst : Var.t Var.Map.t) (p : path) : path =
  match p with
  | Path_long_ident long_ident ->
      Path_long_ident (subst_var_long_ident subst long_ident)
  | Path_core_ty _ | Path_universe _ -> p
  | Path_ty_sing { e; ty } ->
      let e = subst_var_path subst e in
      let ty = subst_var_path subst ty in
      Path_ty_sing { e; ty }
  | Path_ty_mod { var; ty_decls } ->
      let ty_decls =
        List.map ty_decls ~f:(fun ty_decl ->
            { ty_decl with ty = subst_var_path subst ty_decl.ty })
      in
      Path_ty_mod { var; ty_decls }
  | Path_ty_fun { params; ty; purity } ->
      let subst, params = make_params_fresh (subst, params) in
      let ty = subst_var_path subst ty in
      Path_ty_fun { params; ty; purity }

let rec subst_record_var_value (subst : Record_var.t Record_var.Map.t)
    (value : value) : value =
  match value with
  | Value_irrelevant -> Value_irrelevant
  | Value_path path -> Value_path (subst_record_var_path subst path)
  | Value_mod { decls } ->
      let decls =
        List.map decls ~f:(fun { field; e } ->
            { field; e = subst_record_var_value subst e })
      in
      Value_mod { decls }
  | Value_abs { params; body; purity } ->
      let params =
        List.map params ~f:(fun { var; ty } : path_param ->
            { var; ty = subst_record_var_path subst ty })
      in
      let body = subst_record_var_value subst body in
      Value_abs { params; body; purity }

and subst_record_var_long_ident (subst : Record_var.t Record_var.Map.t)
    (lident : long_ident) : long_ident =
  match lident with
  | Long_ident_var (Var _) -> lident
  | Long_ident_var (Record_field { var; field }) ->
      Map.find subst var
      |> Option.map ~f:(fun var -> Long_ident_var (Record_field { var; field }))
      |> Option.value ~default:lident
  | Long_ident_app { e; args } ->
      let e = subst_record_var_long_ident subst e in
      let args = List.map args ~f:(subst_record_var_value subst) in
      Long_ident_app { e; args }
  | Long_ident_proj { e; field } ->
      Long_ident_proj { e = subst_record_var_long_ident subst e; field }

and subst_record_var_path (subst : Record_var.t Record_var.Map.t) (p : path) :
    path =
  match p with
  | Path_long_ident long_ident ->
      Path_long_ident (subst_record_var_long_ident subst long_ident)
  | Path_core_ty _ | Path_universe _ -> p
  | Path_ty_sing { e; ty } ->
      let e = subst_record_var_path subst e in
      let ty = subst_record_var_path subst ty in
      Path_ty_sing { e; ty }
  | Path_ty_mod { var; ty_decls } ->
      let fresh_var = Record_var.create () in
      let subst = Map.set subst ~key:var ~data:fresh_var in
      let ty_decls =
        List.map ty_decls ~f:(fun ty_decl ->
            { ty_decl with ty = subst_record_var_path subst ty_decl.ty })
      in
      Path_ty_mod { var = fresh_var; ty_decls }
  | Path_ty_fun { params; ty; purity } ->
      let params =
        List.map params ~f:(fun { var; ty } : path_param ->
            { var; ty = subst_record_var_path subst ty })
      in
      let ty = subst_record_var_path subst ty in
      Path_ty_fun { params; ty; purity }

let unpack_value_abs (abs : value_abs) ~f =
  let { params; body; purity } = abs in
  let subst, params = make_params_fresh (Var.Map.empty, params) in
  let body = subst_var_value subst body in
  f { params; body; purity }

let unpack_path_ty_fun (ty_fun : path_ty_fun) ~f =
  let ({ params; ty; purity } : path_ty_fun) = ty_fun in
  let subst, params = make_params_fresh (Var.Map.empty, params) in
  let ty = subst_var_path subst ty in
  f ({ params; ty; purity } : path_ty_fun)

let unpack_path_ty_mod (ty_mod : path_ty_mod) ~f =
  let { var; ty_decls } = ty_mod in
  let fresh_var = Record_var.create () in
  let subst = Record_var.Map.singleton var fresh_var in
  let ty_decls =
    List.map ty_decls ~f:(fun { field; ty } ->
        { field; ty = subst_record_var_path subst ty })
  in
  f { var = fresh_var; ty_decls }

let rec eval_subst_long_ident' (subst : value Var.Map.t) (p : long_ident) :
    value =
  match p with
  | Long_ident_app { e; args } ->
      let e = eval_subst_long_ident' subst e in
      let args = List.map args ~f:(eval_subst_value subst) in
      begin match e with
      | Value_mod _ -> failwith "invalid value for applications"
      | Value_path (Path_long_ident e) ->
          Value_path (Path_long_ident (Long_ident_app { e; args }))
      | Value_path _ -> failwith "invalid path"
      | Value_irrelevant -> Value_irrelevant
      | Value_abs { params; body; purity } ->
          assert (Purity.equal purity Pure);
          let subst =
            List.fold (List.zip_exn params args) ~init:subst
              ~f:(fun subst (param, arg) ->
                Map.add_exn subst ~key:param.var ~data:arg)
          in
          eval_subst_value subst body
      end
  | Long_ident_proj { e; field } ->
      let e = eval_subst_long_ident' subst e in
      begin match e with
      | Value_mod { decls } ->
          let decl =
            List.find decls ~f:(fun decl -> String.equal decl.field field)
            |> Option.value_exn ~message:"field should exist"
          in
          decl.e
      | Value_path (Path_long_ident e) ->
          Value_path (Path_long_ident (Long_ident_proj { e; field }))
      | Value_path _ -> failwith "invalid path"
      | Value_irrelevant -> Value_irrelevant
      | Value_abs _ -> failwith "invalid value for projection"
      end
  | Long_ident_var (Var var) ->
      Map.find subst var
      |> Option.value ~default:(Value_path (Path_long_ident p))
  | Long_ident_var (Record_field _) -> Value_path (Path_long_ident p)

and eval_subst_long_ident (subst : value Var.Map.t) (p : long_ident) :
    long_ident =
  match eval_subst_long_ident' subst p with
  | Value_path (Path_long_ident p) -> p
  | _ -> failwith "invalid substitution"

(* p and subst must be well typed in some context *)
and eval_subst_path (subst : value Var.Map.t) (p : path) : path =
  match p with
  | Path_core_ty _ | Path_universe _ -> p
  | Path_ty_sing { e; ty } ->
      let e = eval_subst_path subst e in
      let ty = eval_subst_path subst ty in
      Path_ty_sing { e; ty }
  | Path_ty_mod ty_mod ->
      unpack_path_ty_mod ty_mod ~f:(fun { var; ty_decls } ->
          let ty_decls =
            List.map ty_decls ~f:(fun { field; ty } ->
                ({ field; ty = eval_subst_path subst ty } : path_ty_decl))
          in
          Path_ty_mod { var; ty_decls })
  | Path_ty_fun ty_fun ->
      unpack_path_ty_fun ty_fun ~f:(fun { params; ty; purity } ->
          let params =
            List.map params ~f:(fun { var; ty } : path_param ->
                { var; ty = eval_subst_path subst ty })
          in
          let ty = eval_subst_path subst ty in
          Path_ty_fun { params; ty; purity })
  | Path_long_ident lident ->
      Path_long_ident (eval_subst_long_ident subst lident)

(* does not evaluate under lambdas, which is okay because they will be evaluated when they are applied *)
and eval_subst_value (subst : value Var.Map.t) (v : value) : value =
  match v with
  | Value_mod { decls } ->
      let decls =
        List.map decls ~f:(fun { field; e } : value_decl ->
            let e = eval_subst_value subst e in
            { field; e })
      in
      Value_mod { decls }
  | Value_abs _ -> v
  | Value_irrelevant -> Value_irrelevant
  | Value_path p -> Value_path (eval_subst_path subst p)

module Subst = struct end
(* module Subst = struct
  type kind =
    | Var_subst of value Var.Map.t
    | Record_var_fresh_subst of Record_var.t Record_var.Map.t
    | Record_var_subst of path Record_var.Map.t

  type t = kind list

  let of_var_subst var_subst = [ Var_subst var_subst ]

  let make_var_fresh var =
    [ of_var_subst (Var.Map.singleton var (path_var (Var.make_fresh var))) ]

  let of_record_var_subst record_var_subst =
    [ Record_var_fresh_subst record_var_subst ]

  let make_record_var_fresh record_var =
    [
      of_record_var_subst
        (Record_var.Map.singleton record_var (Record_var.create ()));
    ]

  let compose ~second ~first = List.append first second
  let apply_value (subst : t) (value : value) : value = failwith ""
  let apply_path (subst : t) (value : value) : value = failwith ""
end *)

module Value_abs = struct
  type t = value_abs [@@deriving sexp_of]

  type data = value_abs = {
    params : path_param list;
    body : value;
    purity : Purity.t;
  }
  [@@deriving sexp_of]

  let unpack = unpack_value_abs
  let unpack_advanced t ~f = f t
  let pack t = t
end

module Path_ty_mod = struct
  type t = path_ty_mod [@@deriving sexp_of]

  type data = path_ty_mod = { var : Record_var.t; ty_decls : path_ty_decl list }
  [@@deriving sexp_of]

  let unpack = unpack_path_ty_mod
  let unpack_advanced t ~f = f t
  let pack t = t
end

module Path_ty_fun = struct
  type t = path_ty_fun [@@deriving sexp_of]

  type data = path_ty_fun = {
    params : path_param list;
    ty : path;
    purity : Purity.t;
  }
  [@@deriving sexp_of]

  let unpack = unpack_path_ty_fun
  let unpack_advanced t ~f = f t
  let pack t = t
end
