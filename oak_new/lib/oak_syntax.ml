open Core

module Purity = struct
  module T = struct
    type t =
      | Pure
      | Impure
    [@@deriving sexp_of, compare, equal]
  end

  include T
  include Base.Comparable.Make (T)

  let merge x y =
    match x, y with
    | Impure, _ -> Impure
    | _, Impure -> Impure
    | Pure, Pure -> Pure
  ;;
end

module Universe = struct
  module T = struct
    type t =
      | Type
      | Kind
      | Sig
    [@@deriving sexp, equal, compare]
  end

  include T
  include Base.Comparable.Make (T)

  let minimum = Type
  let maximum = Sig

  let to_int = function
    | Type -> 0
    | Kind -> 1
    | Sig -> 2
  ;;

  let of_int_exn = function
    | 0 -> Type
    | 1 -> Kind
    | 2 -> Sig
    | _ -> failwith "invalid universe"
  ;;

  let lub u v = of_int_exn (Int.max (to_int u) (to_int v))
  let incr_exn u = of_int_exn (to_int u + 1)
end

module Var = struct
  let stamp = ref 0

  module T = struct
    type t =
      { id : int
      ; name : string
      }
    [@@deriving sexp, compare, equal]
  end

  include Comparable.Make (T)
  include T

  let create name =
    let id = !stamp in
    incr stamp;
    { name; id }
  ;;

  let make_fresh var =
    let id = !stamp in
    incr stamp;
    { var with id }
  ;;
end

(* invariant: module variables are always projected immediately *)
module Mod_var = struct
  let stamp = ref 0

  include Int

  let create () =
    let id = !stamp in
    incr stamp;
    id
  ;;
end

(* compound variable *)
module Cvar = struct
  module T = struct
    type t =
      | Var of Var.t
      | Mod_var of Mod_var.t
    [@@deriving sexp, compare, equal]
  end

  include Comparable.Make (T)
  include T

  let create_var name = Var (Var.create name)
  let create_mod_var () = Mod_var (Mod_var.create ())
end

type core_ty =
  | Ty_bool
  | Ty_unit
[@@deriving sexp_of, equal, compare]

(* We don't have packing and unpacking of expressions because we only traverse over expressions once
   we should have the precondition that every binder is unique.
*)
type expr =
  | Expr_var of Cvar.t
  | Expr_seal of
      { e : expr
      ; ty : expr
      }
  | Expr_app of
      { func : expr
      ; args : expr list
      }
  | Expr_abs of
      { params : expr_param list
      ; body : expr
      ; purity : Purity.t
      }
  | Expr_ty_fun of expr_ty_fun
  | Expr_proj of
      { mod_e : expr
      ; field : string
      }
  | Expr_mod of
      { var : Mod_var.t
      ; decls : expr_decl list
      }
  | Expr_ty_mod of expr_ty_mod
  | Expr_let of
      { var : Var.t
      ; rhs : expr
      ; body : expr
      }
  | Expr_ty_sing of expr_ty_sing
  | Expr_bool of bool
  | Expr_unit
  | Expr_core_ty of core_ty
  | Expr_universe of Universe.t
  | Expr_if of
      { cond : expr
      ; body1 : expr
      ; body2 : expr
      }

and expr_decl =
  { field : string
  ; e : expr
  }

and expr_ty_sing =
  { e : expr
  ; ty : expr
  }

and expr_ty_mod =
  { var : Mod_var.t
  ; ty_decls : expr_ty_decl list
  }

and expr_ty_fun =
  { params : expr_param list
  ; body_ty : expr
  ; purity : Purity.t
  }

and expr_ty_decl =
  { field : string
  ; ty : expr
  }

and expr_param =
  { var : Var.t
  ; ty : expr
  }

and value =
  | Value_irrelevant
  | Value_mod of value_mod
  | Value_abs of value_abs
  | Value_ty of ty (* TODO: remove this *)

and ty =
  | Value_core_ty of core_ty
  | Value_path of path
  | Value_univ of Universe.t
  | Value_ty_sing of value_ty_sing
  | Value_ty_mod of value_ty_mod
  | Value_ty_fun of value_ty_fun

and path =
  | Path_var of Cvar.t
  | Path_app of
      { func : path
      ; args : value list
      }
  | Path_proj of
      { mod_e : path
      ; field : string
      }

(* non dependent mod_e *)
(* list decls are in reverse *)
and value_mod = { decls : value_decl list }

and value_abs =
  { binder : value_abs_binder
  ; purity : Purity.t
  }

and value_ty_sing =
  { e : value
  ; ty : ty
  }

and value_ty_mod = { binder : value_ty_mod_binder }

and value_ty_mod_binder =
  { var : Mod_var.t
  ; ty_decls : value_ty_decl list
  ; ty_decls_map : value_ty_decl String.Map.t
  }

and value_ty_decl =
  { field : string
  ; ty : ty
  }

and value_ty_fun =
  { binder : value_ty_fun_binder
  ; purity : Purity.t
  }

and value_abs_binder =
  { params : value_param list
  ; body : value
  }

and value_ty_fun_binder =
  { params : value_param list
  ; body_ty : ty
  }

and value_decl =
  { field : string
  ; e : value
  }

and value_param =
  { var : Var.t
  ; ty : ty
  }
[@@deriving sexp_of]

type subst = value Cvar.Map.t

let value_ty_var v = Value_path (Path_var (Var v))
let value_ty_mod_var v = Value_path (Path_var (Mod_var v))
let value_var v = Value_ty (value_ty_var v)
let value_mod_var v = Value_ty (value_ty_mod_var v)

let value_ty_exn = function
  | Value_ty v -> v
  | _ -> failwith "not a type"
;;

let value_ty_univ_exn = function
  | Value_univ u -> u
  | _ -> failwith "not a type"
;;

let value_ty_fun_exn = function
  | Value_ty_fun f -> f
  | _ -> failwith "not a type"
;;

let value_ty_mod_exn = function
  | Value_ty_mod m -> m
  | _ -> failwith "not a type"
;;

let rec eval_value subst (value : value) =
  match value with
  | Value_irrelevant -> Value_irrelevant
  | Value_mod { decls } ->
    let decls =
      List.map decls ~f:(fun { field; e } ->
        let e = eval_value subst e in
        ({ field; e } : value_decl))
    in
    Value_mod { decls }
  | Value_abs { binder = { params; body }; purity } ->
    let subst, params = make_params_fresh (subst, params) in
    let body = eval_value subst body in
    Value_abs { binder = { params; body }; purity }
  | Value_ty v -> eval_ty subst v

and eval_ty subst (ty : ty) : value =
  match ty with
  | Value_core_ty _ -> Value_ty ty
  | Value_path path -> eval_path subst path
  | Value_univ _ -> Value_ty ty
  | Value_ty_sing { e; ty } ->
    let e = eval_value subst e in
    let ty = eval_ty subst ty |> value_ty_exn in
    Value_ty (Value_ty_sing { e; ty })
  | Value_ty_mod { binder = { var; ty_decls; ty_decls_map = _ } } ->
    let subst, var = make_mod_var_fresh (subst, var) in
    let ty_decls =
      List.map ty_decls ~f:(fun { field; ty } ->
        let ty = eval_ty subst ty |> value_ty_exn in
        ({ field; ty } : value_ty_decl))
    in
    let ty_decls_map =
      String.Map.of_list_with_key_exn ty_decls ~get_key:(fun decl -> decl.field)
    in
    Value_ty (Value_ty_mod { binder = { var; ty_decls; ty_decls_map } })
  | Value_ty_fun { binder = { params; body_ty }; purity } ->
    let subst, params = make_params_fresh (subst, params) in
    let body_ty = eval_ty subst body_ty |> value_ty_exn in
    Value_ty (Value_ty_fun { binder = { params; body_ty }; purity })

and eval_path subst (path : path) : value =
  match path with
  | Path_var var ->
    Map.find subst var |> Option.value ~default:(Value_ty (Value_path path))
  | Path_app { func; args } ->
    let func = eval_path subst func in
    let args = List.map args ~f:(eval_value subst) in
    app_value_exn func args
  | Path_proj { mod_e; field } ->
    let mod_e = eval_path subst mod_e in
    proj_value_exn mod_e field

and app_value_exn (func : value) (args : value list) : value =
  match func with
  | Value_mod _ -> failwith "invalid value for applications"
  | Value_ty (Value_path func) -> Value_ty (Value_path (Path_app { func; args }))
  | Value_ty _ -> failwith "invalid ty"
  | Value_irrelevant -> Value_irrelevant
  | Value_abs { binder = { params; body }; purity = _ } ->
    let subst =
      List.zip_exn params args
      |> List.map ~f:(fun (param, arg) -> Cvar.Var param.var, arg)
      |> Cvar.Map.of_alist_exn
    in
    eval_value subst body

and proj_value_exn (mod_e : value) (field : string) : value =
  match mod_e with
  | Value_mod { decls } ->
    let decl =
      List.find decls ~f:(fun decl -> String.equal decl.field field)
      |> Option.value_exn ~message:"field should exist"
    in
    decl.e
  | Value_ty (Value_path mod_e) -> Value_ty (Value_path (Path_proj { mod_e; field }))
  | Value_ty _ -> failwith "invalid path"
  | Value_irrelevant -> Value_irrelevant
  | Value_abs _ -> failwith "invalid value for projection"

and make_mod_var_fresh ((subst, var) : subst * Mod_var.t) =
  let fresh_var = Mod_var.create () in
  Map.set subst ~key:(Mod_var var) ~data:(value_mod_var fresh_var), fresh_var

and make_params_fresh ((subst, params) : subst * value_param list) =
  List.fold_map params ~init:subst ~f:(fun subst ({ var; ty } : value_param) ->
    let fresh_var = Var.make_fresh var in
    ( Map.set subst ~key:(Var var) ~data:(value_var fresh_var)
    , ({ var = fresh_var; ty = eval_ty subst ty |> value_ty_exn } : value_param) ))
;;

let unpack_value_abs_binder (abs : value_abs_binder) ~f =
  let { params; body } = abs in
  let subst, params = make_params_fresh (Cvar.Map.empty, params) in
  let body = eval_value subst body in
  f { params; body }
;;

let unpack_value_ty_fun_binder (ty_fun : value_ty_fun_binder) ~f =
  let ({ params; body_ty } : value_ty_fun_binder) = ty_fun in
  let subst, params = make_params_fresh (Cvar.Map.empty, params) in
  let body_ty = eval_ty subst body_ty |> value_ty_exn in
  f ({ params; body_ty } : value_ty_fun_binder)
;;

let unpack_value_ty_mod_binder (ty_mod : value_ty_mod_binder) ~f =
  let { var; ty_decls; ty_decls_map = _ } = ty_mod in
  let subst, var = make_mod_var_fresh (Cvar.Map.empty, var) in
  let ty_decls =
    List.map ty_decls ~f:(fun { field; ty } ->
      ({ field; ty = eval_ty subst ty |> value_ty_exn } : value_ty_decl))
  in
  let ty_decls_map =
    String.Map.of_list_with_key_exn ty_decls ~get_key:(fun decl -> decl.field)
  in
  f { var; ty_decls; ty_decls_map }
;;

module Value_abs_binder = struct
  type t = value_abs_binder [@@deriving sexp_of]

  type data = value_abs_binder =
    { params : value_param list
    ; body : value
    }
  [@@deriving sexp_of]

  let unpack = unpack_value_abs_binder
  let unpack_advanced t = t
  let pack t = t
end

(* TODO: check invariants for pack *)
module Value_ty_mod_binder = struct
  type t = value_ty_mod_binder [@@deriving sexp_of]

  type data = value_ty_mod_binder =
    { var : Mod_var.t
    ; ty_decls : value_ty_decl list
    ; ty_decls_map : value_ty_decl String.Map.t
    }
  [@@deriving sexp_of]

  let unpack = unpack_value_ty_mod_binder
  let unpack_advanced t = t
  let pack t = t
end

(* TODO: check invariants for pack *)
module Value_ty_fun_binder = struct
  type t = value_ty_fun_binder [@@deriving sexp_of]

  type data = value_ty_fun_binder =
    { params : value_param list
    ; body_ty : ty
    }
  [@@deriving sexp_of]

  let unpack = unpack_value_ty_fun_binder
  let unpack_advanced t = t
  let pack t = t
end

module Subst = struct
  type t = value Cvar.Map.t [@@deriving sexp_of]

  let empty = Cvar.Map.empty
  let add_exn t var value = Map.add_exn t ~key:var ~data:value

  let singleton var value =
    match value with
    (* compress a little bit *)
    | Value_ty (Value_path (Path_var var')) when Cvar.equal var var' -> Cvar.Map.empty
    | _ -> Cvar.Map.singleton var value
  ;;

  let of_alist_exn alist = Cvar.Map.of_alist_exn alist

  let compose ~second ~first =
    let first = Map.map first ~f:(eval_value second) in
    Map.merge_skewed second first ~combine:(fun ~key:_ _v2 v1 -> v1)
  ;;
end

module Path = struct
  type t = path

  let eval = eval_path
end

module Value = struct
  type t = value

  let eval = eval_value
  let ty_var = value_ty_var
  let ty_mod_var = value_ty_mod_var
  let var = value_var
  let mod_var = value_mod_var
  let get_ty_exn = value_ty_exn
  let get_ty_fun_exn = value_ty_fun_exn
  let get_ty_mod_exn = value_ty_mod_exn
  let get_ty_univ_exn = value_ty_univ_exn
  let app_exn = app_value_exn
  let proj_exn = proj_value_exn
end

module Ty = struct
  type t = ty

  let eval = eval_ty
end
