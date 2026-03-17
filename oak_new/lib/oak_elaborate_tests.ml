open Prelude
open Core
module Syntax = Oak_syntax
module Typed = Oak_typed
module Snippet = Utility.Diagnostic.Snippet
module Pretty = Oak_pretty
module Common = Oak_common
module Diagnostic = Oak_diagnostic

let check ?(print_term = false) ?(show_singletons = false) s =
  let file = "<input>" in
  let source, parse_diagnostics, expr = Oak_parse.parse ~file s in
  let files = String.Map.of_alist_exn [ file, Snippet.File.create s ] in
  if not (List.is_empty parse_diagnostics)
  then Diagnostic.print_many ~files ~color:false parse_diagnostics
  else (
    match expr with
    | None -> print_string "no expression\n"
    | Some expr ->
      let rename_diagnostics, renamed = Oak_rename.rename source expr in
      if not (List.is_empty rename_diagnostics)
      then Diagnostic.print_many ~files ~color:false rename_diagnostics
      else (
        match Oak_elaborate.infer source renamed with
        | Ok typed ->
          if print_term
          then
            print_s
              [%message
                (Typed.Expr.term typed : Syntax.term) (Typed.Expr.ty typed : Syntax.ty)];
          Pp.render_to_stdout
            ~color:false
            (Pretty.pp_ty ~show_singletons Common.Name_list.empty (Typed.Expr.ty typed));
          Out_channel.newline stdout
        | Error diagnostic ->
          Diagnostic.print ~color:false ~files diagnostic;
          Out_channel.newline stdout))
;;

let%expect_test "smoke" =
  check
    {|
fun x -> x
    |};
  [%expect
    {|
    error: Cannot infer lambda without parameter type annotation
     --> <input>:2:1
      |
    2 | fun x -> x
      | ^^^^^^^^^^
    |}];
  check
    {|
fun (x : #t) -> x
    |};
  [%expect
    {|
    error: Type was not a universe: Bool
     --> <input>:2:10
      |
    2 | fun (x : #t) -> x
      |          ^^
    |}]
;;

let%expect_test "id" =
  check
    {|
fun (x : Bool) -> x
    |};
  [%expect {| (x : Bool) -> Bool |}]
;;

let%expect_test "application" =
  check
    {|
(fun (x : Bool) -> x) #t
    |};
  [%expect {| Bool |}]
;;

let%expect_test "block lets" =
  check
    {|
{
  val T : Type = Bool
  val x : T = #t
  x
}
    |};
  [%expect {| Bool |}]
;;

let%expect_test "struct concrete fields" =
  check
    {|
struct {
  val T : Type = Bool
  val x : T = #t
}
    |};
  [%expect {| sig { let T : (= Pack Bool); let x : (= ignore) } |}]
;;

let%expect_test "struct abstract field" =
  check
    {|
struct {
  abstract val T : Type = Bool
  val x : T = #t
}
    |};
  [%expect {| error: Types were not equal: Bool != T |}]
;;

let%expect_test "struct annotation subtyping" =
  check
    {|
(struct {
  val T : Type = Bool
  val x : T = #t
} : sig {
  val T : Type
  val x : T
})
    |};
  [%expect {| sig { let T : Type; let x : T } |}]
;;

let%expect_test "field reordering" =
  check
    {|
(struct {
  val y = #t
  val T : Type = Bool
  val x : T = #t
} : sig {
  val x : Bool
  val T : Type
  val y : T
})
    |};
  [%expect {| sig { let x : Bool; let T : Type; let y : T } |}]
;;
