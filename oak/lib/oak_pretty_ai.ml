(* (*
Write a pretty printer for the types value, ty, and path inside of oak_syntax.ml
let comma_sep ppf () = pf ppf ",@ "

modules are printed like this
mod:
  let decl1: 134
  let decl2 =
    mod:
      let decl4: 234
      let decl5: 234
  let decl3: 234
  
signatures are printed in a similar way

some types have a compact and expanded representation, depending on how much space there is in the document which is controled by fmt.

for example, singletons are shown as
Is(Int, Type)
in compact form and
Is(
  Int,
  Type
)
in expanded form.

Is(
  mod:
    let x = 123
    let y = 2343243
    let z = 123423,
  sig:
    let x Int
    let y Int
    let z Int
)

Fun(
  sig:
    let x Int
    let y Int
    let z Int,
  sig:
    let x Type
    let z Type
)

Similarly for functions
Fun(Type, Type) Type
for compact form
and
Fun(
  Type,
  Type
) Type
for expanded form.

These are all controlled by the line break directives in the fmt library.

For projections it should look like
a.b.c.d
for compact
and
a
  .b
  .c
  .d
for expanded

Make sure to put parenthesis around things as necessary, but only use the minimal amount of parenthesis needed.
*)

open Prelude
module Syntax = Oak_syntax
open Syntax
open Fmt

let comma_sep ppf () = pf ppf ",@ "

let rec pp_value ppf = function
  | Value_irrelevant -> string ppf "irrelevant"
  | Value_mod { decls } -> pf ppf "@[<v 2>mod:@ %a@]" (list ~sep:cut pp_value_decl) decls
  | Value_abs { binder; purity } ->
    let { Value_abs_binder.params; body } = Value_abs_binder.unpack_advanced binder in
    let name =
      match purity with
      | Pure -> "fun"
      | Impure -> "funct"
    in
    pf
      ppf
      "@[<hov 2>%s(@,%a):@ %a@]"
      name
      (list ~sep:comma_sep pp_value_param)
      params
      pp_value
      body
  | Value_ty t -> pp_ty ppf t

and pp_value_decl ppf { field; e } = pf ppf "@[<hov 2>let %s: %a@]" field pp_value e
and pp_value_param ppf { var; ty } = pf ppf "@[<h>%s %a@]" var.name pp_ty ty

and pp_ty ppf = function
  | Value_core_ty Ty_bool -> string ppf "Bool"
  | Value_core_ty Ty_int -> string ppf "Int"
  | Value_core_ty Ty_unit -> string ppf "Unit"
  | Value_path p -> pp_path ppf p
  | Value_univ Type -> string ppf "Type"
  | Value_univ Kind -> string ppf "Kind"
  | Value_univ Sig -> string ppf "Sig"
  | Value_ty_sing { e; ty } -> pf ppf "@[<hov 2>Is(@,%a,@ %a@,)@]" pp_value e pp_ty ty
  | Value_ty_mod { binder } ->
    let { Value_ty_mod_binder.var = _; ty_decls; ty_decls_map = _ } =
      Value_ty_mod_binder.unpack_advanced binder
    in
    pf ppf "@[<v 2>sig:@ %a@]" (list ~sep:cut pp_value_ty_decl) ty_decls;
  | Value_ty_fun { binder; purity } ->
    let { Value_ty_fun_binder.params; body_ty } =
      Value_ty_fun_binder.unpack_advanced binder
    in
    let name =
      match purity with
      | Pure -> "Fun"
      | Impure -> "Funct"
    in
    pf
      ppf
      "@[<hov 2>%s(@,%a@,)@ %a@]"
      name
      (list ~sep:comma_sep pp_value_param)
      params
      pp_ty
      body_ty

and pp_value_ty_decl ppf { field; ty } = pf ppf "@[<hov 2>let %s %a@]" field pp_ty ty

and pp_path ppf p =
  let rec collect acc = function
    | Path_proj { mod_e; field } -> collect (field :: acc) mod_e
    | other -> other, acc
  in
  let root, fields = collect [] p in
  match fields with
  | [] -> pp_path_simple ppf root
  | _ ->
    pf ppf "@[<hv 2>%a" pp_path_simple root;
    List.iter fields ~f:(fun f -> pf ppf "@,.%s" f);
    pf ppf "@]"

and pp_path_simple ppf = function
  | Path_var (Var v) -> string ppf v.name
  | Path_var (Mod_var m) -> pf ppf "m%d" m.id
  | Path_app { func; args } ->
    pf ppf "%a(@[<hov>%a@])" pp_path func (list ~sep:comma_sep pp_value) args
  | Path_proj _ as p -> pp_path ppf p
;;

open Syntax

let print_ty ty = Stdlib.Format.printf "%a@." pp_ty ty
let print_value v = Stdlib.Format.printf "%a@." pp_value v

let%expect_test "simple types" =
  print_ty (Value_univ Universe.Type);
  [%expect {| Type |}];
  print_ty (Value_core_ty Ty_int);
  [%expect {| Int |}]
;;

let%expect_test "function type" =
  let var = Var.create "x" in
  let params = [ { var; ty = Value_core_ty Ty_int } ] in
  let body_ty = Value_core_ty Ty_bool in
  let binder = Value_ty_fun_binder.pack { params; body_ty } in
  print_ty (Value_ty_fun { binder; purity = Purity.Pure });
  [%expect {| Fun(x Int) Bool |}]
;;

let%expect_test "singleton" =
  (* Is(Int, Type) *)
  let e = Value_ty (Value_core_ty Ty_int) in
  let ty = Value_univ Universe.Type in
  print_ty (Value_ty_sing { e; ty });
  [%expect {| Is(Int, Type) |}]
;;

let%expect_test "path application" =
  (* List(Int) *)
  let list_var = Var.create "List" in
  let func = Path_var (Cvar.Var list_var) in
  let args = [ Value_ty (Value_core_ty Ty_int) ] in
  print_ty (Value_path (Path_app { func; args }));
  [%expect {| List(Int) |}]
;;

let%expect_test "module value" =
  let decls = [ { field = "x"; e = Value_ty (Value_core_ty Ty_int) } ] in
  print_value (Value_mod { decls });
  [%expect
    {|
    mod:
      let x: Int |}]
;;

let%expect_test "signature" =
  let binder =
    Value_ty_mod_binder.pack
      { var = Mod_var.create ()
      ; ty_decls = [ { field = "T"; ty = Value_univ Universe.Type } ]
      ; ty_decls_map = String.Map.empty
      }
  in
  print_ty (Value_ty_mod { binder });
  [%expect
    {|
    sig:
      let T Type |}]
;;

let%expect_test "function value" =
  let var = Var.create "x" in
  let params = [ { var; ty = Value_core_ty Ty_int } ] in
  let body = Value_ty (Value_core_ty Ty_int) in
  let binder = Value_abs_binder.pack { params; body } in
  print_value (Value_abs { binder; purity = Purity.Pure });
  [%expect {| fun(x Int): Int |}]
;; *)
