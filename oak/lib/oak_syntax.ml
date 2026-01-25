open Core
module Token = Shrubbery.Token

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
    type t = Type | Kind | Sig [@@deriving sexp_of, equal, compare]
  end

  include T
  include Base.Comparable.Make (T)

  let minimum = Type
  let maximum = Sig
  let to_int = function Type -> 0 | Kind -> 1 | Sig -> 2

  let of_int_exn = function
    | 0 -> Type
    | 1 -> Kind
    | 2 -> Sig
    | _ -> failwith "invalid universe"

  let incr_exn u = of_int_exn (to_int u + 1)
end

module Var = struct
  let stamp = ref 0

  module T = struct
    type t = { id : int; name : string; token : Token.ti option }
    [@@deriving sexp_of, compare, equal]
  end

  include Comparable.Make_plain (T)
  include T

  let create_initial name token = { name; id = -1; token = Some token }

  let create ?token name =
    let id = !stamp in
    incr stamp;
    { name; id; token }

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
    [@@deriving sexp_of, compare, equal]
  end

  include Comparable.Make_plain (T)
  include T
end

type core_ty = Ty_bool | Ty_unit [@@deriving sexp_of, equal, compare]

(* TODO: add names to all these *)
type expr =
  | Expr_var of Cvar.t
  | Expr_seal of { e : expr; ty : expr }
  | Expr_app of { func : expr; args : expr list }
  | Expr_abs of { params : param list; body : expr; purity : Purity.t }
  | Expr_ty_fun of expr_ty_fun
  | Expr_proj of { mod_e : expr; field : string }
  | Expr_mod of { var : Record_var.t; decls : decl list }
  | Expr_ty_mod of ty_mod
  | Expr_let of { var : Var.t; rhs : expr; body : expr }
  | Expr_ty_sing of expr_ty_sing
  | Expr_bool of bool
  | Expr_unit
  | Expr_core_ty of core_ty
  | Expr_universe of Universe.t
  (* invariant: path for expr_irrelevant is normalized *)
  | Expr_if of { cond : expr; body1 : expr; body2 : expr }

(* this is wrong, should be value instead of expr *)
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
  | Long_ident_app of long_ident_app
  | Long_ident_proj of long_ident_proj
  | Long_ident_var of Cvar.t

and long_ident_proj = { mod_e : long_ident; field : string }
and long_ident_app = { func : long_ident; args : value list }
and path_ty_sing = { e : path; ty : path }

and path_ty_mod_binder = {
  var : Record_var.t;
  ty_decls : path_ty_decl list;
  ty_decls_map : path_ty_decl String.Map.t;
}

and path_ty_mod = { binder : path_ty_mod_binder }

(* invariant: vars inside of params must be disjoint *)
and path_ty_fun_binder = { params : path_param list; ty : path }
and path_ty_fun = { binder : path_ty_fun_binder; purity : Purity.t }
and path_ty_decl = { field : string; ty : path }
and path_param = { var : Var.t; ty : path }

and value =
  | Value_irrelevant
  | Value_path of path
  | Value_mod of value_mod
  | Value_abs of value_abs

(* and value_new = 
| Value_irrelevant
| Value_var of Cvar.t
| Value_app of { func : value; args : value list }
| Value_proj of { mod_e : value; field : string }
| Value_mod of { decls : value_decl list }
| Value_abs of value_abs
| Value_core_ty of core_ty
| Value_universe of Universe.t
| Value_ty_sing of path_ty_sing
| Value_ty_mod of path_ty_mod
| Value_ty_fun of path_ty_fun *)

(* value_mod is non dependent *)
and value_mod = { decls : value_decl list }
and value_abs_binder = { params : path_param list; body : value }
and value_abs = { binder : value_abs_binder; purity : Purity.t }
and value_decl = { field : string; e : value }
and ty_mod = { var : Record_var.t; ty_decls : ty_decl list }
and param = { var : Var.t; ty : expr }
and decl = { field : string; e : expr }
and ty_decl = { field : string; ty : expr } [@@deriving sexp_of]

let path_var var = Value_path (Path_long_ident (Long_ident_var (Var var)))

let path_record_field_var var field =
  Value_path (Path_long_ident (Long_ident_var (Record_field { var; field })))

let is_path_universe = function Path_universe _ -> true | _ -> false

let path_long_ident_exn = function
  | Path_long_ident lident -> lident
  | _ -> failwith "not a long ident"

let path_universe_exn = function
  | Path_universe u -> u
  | _ -> failwith "not a universe type"

let path_ty_fun_exn = function
  | Path_ty_fun p -> p
  | _ -> failwith "not a function type"

let path_ty_mod_exn = function
  | Path_ty_mod p -> p
  | _ -> failwith "not a module type"

let path_ty_fun_binder_exn = function
  | Path_ty_fun { binder; _ } -> binder
  | _ -> failwith "not a function type"

let path_ty_mod_binder_exn = function
  | Path_ty_mod { binder } -> binder
  | _ -> failwith "not a module type"

let value_path_exn = function Value_path p -> p | _ -> failwith "not a path"

let value_lident_exn = function
  | Value_path (Path_long_ident lident) -> lident
  | _ -> failwith "not a long ident"

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
  | Value_abs { binder = { params; body }; purity } ->
      let subst, params = make_params_fresh (subst, params) in
      let body = subst_var_value subst body in
      Value_abs { binder = { params; body }; purity }

and make_params_fresh (subst, params) =
  List.fold_map params ~init:subst ~f:(fun subst { var; ty } ->
      let fresh_var = Var.make_fresh var in
      ( Map.set subst ~key:var ~data:fresh_var,
        ({ var = fresh_var; ty = subst_var_path subst ty } : path_param) ))

and subst_var_long_ident (subst : Var.t Var.Map.t) (lident : long_ident) :
    long_ident =
  match lident with
  | Long_ident_var (Var var) ->
      Map.find subst var
      |> Option.map ~f:(fun var -> Long_ident_var (Var var))
      |> Option.value ~default:lident
  | Long_ident_var (Record_field _) -> lident
  | Long_ident_app { func; args } ->
      let func = subst_var_long_ident subst func in
      let args = List.map args ~f:(subst_var_value subst) in
      Long_ident_app { func; args }
  | Long_ident_proj { mod_e; field } ->
      Long_ident_proj { mod_e = subst_var_long_ident subst mod_e; field }

and subst_var_path (subst : Var.t Var.Map.t) (p : path) : path =
  match p with
  | Path_long_ident long_ident ->
      Path_long_ident (subst_var_long_ident subst long_ident)
  | Path_core_ty _ | Path_universe _ -> p
  | Path_ty_sing { e; ty } ->
      let e = subst_var_path subst e in
      let ty = subst_var_path subst ty in
      Path_ty_sing { e; ty }
  | Path_ty_mod { binder = { var; ty_decls; ty_decls_map = _ } } ->
      let ty_decls =
        List.map ty_decls ~f:(fun ty_decl ->
            { ty_decl with ty = subst_var_path subst ty_decl.ty })
      in
      let ty_decls_map =
        String.Map.of_list_with_key_exn ty_decls ~get_key:(fun decl ->
            decl.field)
      in
      Path_ty_mod { binder = { var; ty_decls; ty_decls_map } }
  | Path_ty_fun { binder = { params; ty }; purity } ->
      let subst, params = make_params_fresh (subst, params) in
      let ty = subst_var_path subst ty in
      Path_ty_fun { binder = { params; ty }; purity }

(* TODO: the replace action is defined incorrectly *)
module Record_field_action = struct
  type t = Replace of long_ident | Replace_var of Record_var.t
  [@@deriving sexp_of]
end

let rec subst_record_var_value (subst : Record_field_action.t Record_var.Map.t)
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
  | Value_abs { binder = { params; body }; purity } ->
      let params =
        List.map params ~f:(fun { var; ty } : path_param ->
            { var; ty = subst_record_var_path subst ty })
      in
      let body = subst_record_var_value subst body in
      Value_abs { binder = { params; body }; purity }

and subst_record_var_long_ident (subst : Record_field_action.t Record_var.Map.t)
    (lident : long_ident) : long_ident =
  match lident with
  | Long_ident_var (Var _) -> lident
  | Long_ident_var (Record_field { var; field }) ->
      Map.find subst var
      |> Option.map ~f:(function
        | Replace lident -> lident
        | Replace_var var -> Long_ident_var (Record_field { var; field }))
      |> Option.value ~default:lident
  | Long_ident_app { func; args } ->
      let func = subst_record_var_long_ident subst func in
      let args = List.map args ~f:(subst_record_var_value subst) in
      Long_ident_app { func; args }
  | Long_ident_proj { mod_e; field } ->
      Long_ident_proj { mod_e = subst_record_var_long_ident subst mod_e; field }

and subst_record_var_path (subst : Record_field_action.t Record_var.Map.t)
    (p : path) : path =
  match p with
  | Path_long_ident long_ident ->
      Path_long_ident (subst_record_var_long_ident subst long_ident)
  | Path_core_ty _ | Path_universe _ -> p
  | Path_ty_sing { e; ty } ->
      let e = subst_record_var_path subst e in
      let ty = subst_record_var_path subst ty in
      Path_ty_sing { e; ty }
  | Path_ty_mod { binder = { var; ty_decls; ty_decls_map = _ } } ->
      let fresh_var = Record_var.create () in
      let subst =
        Map.set subst ~key:var ~data:(Record_field_action.Replace_var fresh_var)
      in
      let ty_decls =
        List.map ty_decls ~f:(fun ty_decl ->
            { ty_decl with ty = subst_record_var_path subst ty_decl.ty })
      in
      let ty_decls_map =
        String.Map.of_list_with_key_exn ty_decls ~get_key:(fun decl ->
            decl.field)
      in
      Path_ty_mod { binder = { var = fresh_var; ty_decls; ty_decls_map } }
  | Path_ty_fun { binder = { params; ty }; purity } ->
      let params =
        List.map params ~f:(fun { var; ty } : path_param ->
            { var; ty = subst_record_var_path subst ty })
      in
      let ty = subst_record_var_path subst ty in
      Path_ty_fun { binder = { params; ty }; purity }

let unpack_value_abs_binder (abs : value_abs_binder) ~f =
  let { params; body } = abs in
  let subst, params = make_params_fresh (Var.Map.empty, params) in
  let body = subst_var_value subst body in
  f { params; body }

let unpack_path_ty_fun_binder (ty_fun : path_ty_fun_binder) ~f =
  let ({ params; ty } : path_ty_fun_binder) = ty_fun in
  let subst, params = make_params_fresh (Var.Map.empty, params) in
  let ty = subst_var_path subst ty in
  f ({ params; ty } : path_ty_fun_binder)

let unpack_path_ty_mod_binder (ty_mod : path_ty_mod_binder) ~f =
  let { var; ty_decls; ty_decls_map = _ } = ty_mod in
  let fresh_var = Record_var.create () in
  let subst =
    Record_var.Map.singleton var (Record_field_action.Replace_var fresh_var)
  in
  let ty_decls =
    List.map ty_decls ~f:(fun { field; ty } ->
        { field; ty = subst_record_var_path subst ty })
  in
  let ty_decls_map =
    String.Map.of_list_with_key_exn ty_decls ~get_key:(fun decl -> decl.field)
  in
  f { var = fresh_var; ty_decls; ty_decls_map }

let rec eval_subst_long_ident_gen' (subst : Cvar.t -> value option)
    (p : long_ident) : value =
  match p with
  | Long_ident_app { func; args } ->
      let func = eval_subst_long_ident_gen' subst func in
      let args = List.map args ~f:(eval_subst_value_gen subst) in
      app_value_exn func args
  | Long_ident_proj { mod_e; field } ->
      let mod_e = eval_subst_long_ident_gen' subst mod_e in
      proj_value_exn mod_e field
  | Long_ident_var var ->
      subst var |> Option.value ~default:(Value_path (Path_long_ident p))

and proj_value_exn (mod_e : value) (field : string) : value =
  match mod_e with
  | Value_mod { decls } ->
      let decl =
        List.find decls ~f:(fun decl -> String.equal decl.field field)
        |> Option.value_exn ~message:"field should exist"
      in
      decl.e
  | Value_path (Path_long_ident mod_e) ->
      Value_path (Path_long_ident (Long_ident_proj { mod_e; field }))
  | Value_path _ -> failwith "invalid path"
  | Value_irrelevant -> Value_irrelevant
  | Value_abs _ -> failwith "invalid value for projection"

and app_value_exn (func : value) (args : value list) : value =
  match func with
  | Value_mod _ -> failwith "invalid value for applications"
  | Value_path (Path_long_ident func) ->
      Value_path (Path_long_ident (Long_ident_app { func; args }))
  | Value_path _ -> failwith "invalid path"
  | Value_irrelevant -> Value_irrelevant
  | Value_abs { binder; purity } ->
      assert (Purity.equal purity Pure);
      unpack_value_abs_binder binder ~f:(fun { params; body } ->
          let subst =
            List.zip_exn params args
            |> List.map ~f:(fun (param, arg) -> (param.var, arg))
            |> Var.Map.of_alist_exn
          in
          eval_subst_value subst body)

and eval_subst_long_ident_gen (subst : Cvar.t -> value option) (p : long_ident)
    : long_ident =
  match eval_subst_long_ident_gen' subst p with
  | Value_path (Path_long_ident p) -> p
  | _ -> failwith "invalid substitution"

(* p and subst must be well typed in some context *)
and eval_subst_path_gen (subst : Cvar.t -> value option) (p : path) : path =
  match p with
  | Path_core_ty _ | Path_universe _ -> p
  | Path_ty_sing { e; ty } ->
      let e = eval_subst_path_gen subst e in
      let ty = eval_subst_path_gen subst ty in
      Path_ty_sing { e; ty }
  | Path_ty_mod { binder } ->
      unpack_path_ty_mod_binder binder
        ~f:(fun { var; ty_decls; ty_decls_map = _ } ->
          let ty_decls =
            List.map ty_decls ~f:(fun { field; ty } ->
                ({ field; ty = eval_subst_path_gen subst ty } : path_ty_decl))
          in
          let ty_decls_map =
            String.Map.of_list_with_key_exn ty_decls ~get_key:(fun decl ->
                decl.field)
          in
          Path_ty_mod { binder = { var; ty_decls; ty_decls_map } })
  | Path_ty_fun { binder; purity } ->
      unpack_path_ty_fun_binder binder ~f:(fun { params; ty } ->
          let params =
            List.map params ~f:(fun { var; ty } : path_param ->
                { var; ty = eval_subst_path_gen subst ty })
          in
          let ty = eval_subst_path_gen subst ty in
          Path_ty_fun { binder = { params; ty }; purity })
  | Path_long_ident lident ->
      Path_long_ident (eval_subst_long_ident_gen subst lident)

and eval_subst_value_gen (subst : Cvar.t -> value option) (v : value) : value =
  match v with
  | Value_mod { decls } ->
      let decls =
        List.map decls ~f:(fun { field; e } : value_decl ->
            let e = eval_subst_value_gen subst e in
            { field; e })
      in
      Value_mod { decls }
  | Value_abs { binder; purity } ->
      let binder =
        unpack_value_abs_binder binder ~f:(fun { params; body } ->
            { params; body = eval_subst_value_gen subst body })
      in
      Value_abs { binder; purity }
  | Value_irrelevant -> Value_irrelevant
  | Value_path p -> Value_path (eval_subst_path_gen subst p)

and eval_subst_long_ident' (subst : value Var.Map.t) (p : long_ident) : value =
  let subst_fn = function Cvar.Var var -> Map.find subst var | _ -> None in
  eval_subst_long_ident_gen' subst_fn p

and eval_subst_long_ident (subst : value Var.Map.t) (p : long_ident) :
    long_ident =
  let subst_fn = function Cvar.Var var -> Map.find subst var | _ -> None in
  eval_subst_long_ident_gen subst_fn p

and eval_subst_path (subst : value Var.Map.t) (p : path) : path =
  let subst_fn = function Cvar.Var var -> Map.find subst var | _ -> None in
  eval_subst_path_gen subst_fn p

and eval_subst_value (subst : value Var.Map.t) (v : value) : value =
  let subst_fn = function Cvar.Var var -> Map.find subst var | _ -> None in
  eval_subst_value_gen subst_fn v

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

module Value_abs_binder = struct
  type t = value_abs_binder [@@deriving sexp_of]

  type data = value_abs_binder = { params : path_param list; body : value }
  [@@deriving sexp_of]

  let unpack = unpack_value_abs_binder
  let unpack_advanced t ~f = f t
  let pack t = t
end

(* TODO: check invariants for pack *)
module Path_ty_mod_binder = struct
  type t = path_ty_mod_binder [@@deriving sexp_of]

  type data = path_ty_mod_binder = {
    var : Record_var.t;
    ty_decls : path_ty_decl list;
    ty_decls_map : path_ty_decl String.Map.t;
  }
  [@@deriving sexp_of]

  let unpack = unpack_path_ty_mod_binder
  let unpack_advanced t ~f = f t
  let pack t = t
end

(* TODO: check invariants for pack *)
module Path_ty_fun_binder = struct
  type t = path_ty_fun_binder [@@deriving sexp_of]

  type data = path_ty_fun_binder = { params : path_param list; ty : path }
  [@@deriving sexp_of]

  let unpack = unpack_path_ty_fun_binder
  let unpack_advanced t ~f = f t
  let pack t = t
end
