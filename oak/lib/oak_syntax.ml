open Prelude
module Span = Utility.Span
module Token = Shrubbery.Token
module Pos = Utility.Pos

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

  let to_string = function
    | Type -> "Type"
    | Kind -> "Kind"
    | Sig -> "Sig"
  ;;
end

module Var = struct
  let stamp = ref 1

  module T = struct
    type t =
      { id : int
      ; name : string [@equal.ignore] [@compare.ignore]
      ; pos : int option [@equal.ignore] [@compare.ignore]
      }
    [@@deriving sexp_of, compare, equal]
  end

  include Comparable.Make_plain (T)
  include T

  let to_string t = sprintf "%s_%d" t.name t.id
  let create_initial name pos = { name; id = 0; pos = Some pos }

  let create ?pos name =
    let id = !stamp in
    incr stamp;
    { name; id; pos }
  ;;

  let make_fresh var =
    let id = !stamp in
    incr stamp;
    { var with id }
  ;;
end

(* invariant: module variables are always projected immediately *)
module Mod_var = struct
  let stamp = ref 1

  module T = struct
    type t =
      { id : int
      ; pos : Pos.t option [@equal.ignore] [@compare.ignore]
      }
    [@@deriving sexp_of, compare, equal]
  end

  include Comparable.Make_plain (T)
  include T

  let to_string t = sprintf "m_%d" t.id
  let create_initial pos = { id = 0; pos = Some pos }

  let create ?pos () =
    let id = !stamp in
    incr stamp;
    { id; pos }
  ;;
end

(* compound variable *)
module Cvar = struct
  module T = struct
    type t =
      | Var of Var.t
      | Mod_var of Mod_var.t
    [@@deriving sexp_of, compare, equal]
  end

  include Comparable.Make_plain (T)
  include T

  let to_string = function
    | Var v -> Var.to_string v
    | Mod_var v -> Mod_var.to_string v
  ;;

  let create_var name = Var (Var.create name)
  let create_mod_var () = Mod_var (Mod_var.create ())
end

type core_ty =
  | Ty_bool
  | Ty_unit
  | Ty_int
[@@deriving sexp_of, equal, compare]

(* We don't have packing and unpacking of expressions because we only traverse over expressions once
   we should have the precondition that every binder is unique.
   
   All of these spans are token spans, not byte position spans.
*)
type expr =
  | Expr_var of
      { var : Cvar.t
      ; span : Span.t
      }
  | Expr_seal of
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
      { params : expr_param list
      ; body : expr
      ; purity : Purity.t
      ; span : Span.t
      }
  | Expr_ty_fun of expr_ty_fun
  | Expr_proj of
      { mod_e : expr
      ; field : string
      ; span : Span.t
      }
  | Expr_mod of
      { var : Mod_var.t
      ; decls : expr_decl list
      ; span : Span.t
      }
  | Expr_ty_mod of expr_ty_mod
  | Expr_let of
      { var : Var.t
      ; rhs : expr
      ; body : expr
      ; span : Span.t
      }
  | Expr_ty_sing of expr_ty_sing
  | Expr_bool of
      { value : bool
      ; span : Span.t
      }
  | Expr_unit of { span : Span.t }
  | Expr_int of
      { value : int
      ; span : Span.t
      }
  | Expr_core_ty of
      { ty : core_ty
      ; span : Span.t
      }
  | Expr_universe of
      { univ : Universe.t
      ; span : Span.t
      }
  | Expr_if of
      { cond : expr
      ; body1 : expr
      ; body2 : expr
      ; span : Span.t
      }
  | Expr_hole of { span : Span.t }

and expr_decl =
  { let_pos : int
  ; field : string
  ; field_pos : int
  ; e : expr
  ; span : Span.t
  }

and expr_ty_sing =
  { e : expr
  ; ty : expr
  ; span : Span.t
  }

and expr_ty_mod =
  { var : Mod_var.t
  ; ty_decls : expr_ty_decl list
  ; span : Span.t
  }

and expr_ty_fun =
  { params : expr_param list
  ; body_ty : expr
  ; purity : Purity.t
  ; span : Span.t
  }

and expr_ty_decl =
  { field : string
  ; field_pos : int
  ; ty : expr
  ; span : Span.t
  }

and expr_param =
  { var : Var.t
  ; ty : expr
  ; span : Span.t
  }

and value =
  | Value_irrelevant
  | Value_mod of value_mod
  | Value_abs of value_abs
  | Value_ty of ty

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

module Pretty = struct
  let parens doc =
    Doc.group
      (Doc.char '(' ^^ Doc.indent 2 (Doc.break0 ^^ doc) ^^ Doc.break0 ^^ Doc.char ')')
  ;;

  let parens_list docs = parens (Doc.concat ~sep:(Doc.char ',' ^^ Doc.break1) docs)

  let block docs =
    Doc.group
      (Doc.string ":"
       ^^ Doc.when_flat (Doc.string " { ")
       ^^ Doc.indent
            2
            (Doc.break0
             ^^ Doc.concat ~sep:(Doc.when_flat (Doc.char ';') ^^ Doc.break1) docs)
       ^^ Doc.when_flat (Doc.string " }"))
  ;;

  let rec pp_path (path : path) =
    match path with
    | Path_var var -> Doc.string (Cvar.to_string var)
    | Path_app { func; args } -> pp_path func ^^ parens_list (List.map args ~f:pp_value)
    | Path_proj { mod_e; field } -> pp_path mod_e ^^ Doc.char '.' ^^ Doc.string field

  and pp_value (value : value) =
    match value with
    | Value_irrelevant -> Doc.string "_"
    | Value_mod { decls } -> Doc.string "mod" ^^ block (List.map decls ~f:pp_decl)
    | Value_abs { binder = { params; body }; purity } ->
      let keyword =
        match purity with
        | Pure -> "funct"
        | Impure -> "fun"
      in
      Doc.group
        (Doc.string keyword
         ^^ parens_list (List.map params ~f:pp_param)
         ^^ Doc.string ":"
         ^^ Doc.indent 2 (Doc.break1 ^^ pp_value body))
    | Value_ty ty -> pp_ty ty

  and pp_ty (ty : ty) =
    match ty with
    | Value_core_ty core_ty -> Doc.string (core_ty_to_string core_ty)
    | Value_path path -> pp_path path
    | Value_univ univ -> Doc.string (Universe.to_string univ)
    | Value_ty_sing { e; ty } -> Doc.string "Is" ^^ parens_list [ pp_value e; pp_ty ty ]
    | Value_ty_mod { binder = { var; ty_decls; ty_decls_map = _ } } ->
      Doc.string "sig"
      ^^ Doc.blank1
      ^^ pp_mod_var var
      ^^ block (List.map ty_decls ~f:pp_ty_decl)
    | Value_ty_fun { binder = { params; body_ty }; purity } ->
      let keyword =
        match purity with
        | Pure -> "Funct"
        | Impure -> "Fun"
      in
      Doc.string keyword
      ^^ parens_list (List.map params ~f:pp_param)
      ^^ Doc.blank1
      ^^ pp_ty body_ty

  and pp_param (param : value_param) =
    if String.equal param.var.name "_"
    then pp_ty param.ty
    else
      Doc.string param.var.name
      ^^ Doc.string "_"
      ^^ Doc.string (Int.to_string param.var.id)
      ^^ Doc.blank1
      ^^ pp_ty param.ty

  and pp_mod_var var = Doc.string (Mod_var.to_string var)

  and pp_decl (decl : value_decl) =
    Doc.group
      (Doc.string "let "
       ^^ Doc.string decl.field
       ^^ Doc.string " ="
       ^^ Doc.indent 2 (Doc.break1 ^^ pp_value decl.e))

  and pp_ty_decl (ty_decl : value_ty_decl) =
    Doc.group
      (Doc.string "let "
       ^^ Doc.string ty_decl.field
       ^^ Doc.indent 2 (Doc.break1 ^^ pp_ty ty_decl.ty))

  and core_ty_to_string = function
    | Ty_bool -> "Bool"
    | Ty_int -> "Int"
    | Ty_unit -> "Unit"
  ;;
end

module Path = struct
  type t = path

  let pp = Pretty.pp_path
  let eval = eval_path
end

module Value = struct
  type t = value

  let pp = Pretty.pp_value
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

  let pp = Pretty.pp_ty
  let eval = eval_ty
end

let expr_span (e : expr) : Span.t =
  match e with
  | Expr_var { span; _ }
  | Expr_seal { span; _ }
  | Expr_app { span; _ }
  | Expr_abs { span; _ }
  | Expr_ty_fun { span; _ }
  | Expr_proj { span; _ }
  | Expr_mod { span; _ }
  | Expr_ty_mod { span; _ }
  | Expr_let { span; _ }
  | Expr_ty_sing { span; _ }
  | Expr_bool { span; _ }
  | Expr_unit { span }
  | Expr_int { span; _ }
  | Expr_core_ty { span; _ }
  | Expr_universe { span; _ }
  | Expr_hole { span; _ }
  | Expr_if { span; _ } -> span
;;
