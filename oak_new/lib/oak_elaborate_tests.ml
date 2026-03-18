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
  [%expect {| sig { val T = Bool; val x = ignore } |}]
;;

let%expect_test "struct abstract field" =
  check
    {|
struct {
  abstract val T : Type = Bool
  val x : T = #t
}
    |};
  [%expect
    {|
    error: Types were not equal: Bool != T
    error: while checking the expression against the expected type
     --> <input>:4:15
      |
    4 |   val x : T = #t
      |               ^^
    |}]
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
  [%expect {| sig { val T : Type; val x : T } |}]
;;

let%expect_test "nondependent struct punning" =
  check
    {|
{
  val x : Type = Bool
  struct (val x)
}
    |};
  [%expect {| sig { val x = Bool } |}]
;;

let%expect_test "nondependent struct fields do not bind later fields" =
  check
    {|
{
  val x : Type = Bool
  struct (val x = Unit, val y = x)
}
    |};
  [%expect {| sig { val x = Unit; val y = Bool } |}]
;;

let%expect_test "nondependent struct checking reorders fields" =
  check
    {|
struct {
  val T : Type = Bool
  val x : sig {
    val T = Bool
    val y : T = #t
  } = struct (val y = #t, val T)
}
    |};
  [%expect {| sig { val T = Bool; val x = struct (val y = ignore, val T = T) } |}]
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
  [%expect {| sig { val x : Bool; val T : Type; val y : T } |}]
;;

let%expect_test "universes" =
  check
    {|
struct {
  val x = Type
  val y = Type
}
    |};
  [%expect {| sig { val x = Type; val y = Type } |}];
  check
    {|
struct {
  val x = Type
  val y = Type
  val z = Sig
}
    |};
  [%expect {| sig { val x = Type; val y = Type; val z = Sig } |}];
  check
    {|
struct {
  val S1 = sig {
    val x : Type = Int
    val y : Type = Int
  }
  
  val S2 = sig {
    val z : Sig = Type
    val w : Sig
    val x = sig {
      val x : Type
      val z = x
    }
  }
}
    |};
  [%expect
    {|
    sig {
      val S1 = sig { val x = Int; val y = Int }
      val S2 = sig { val z = Type; val w : Sig; val x = sig { val x : Type; val z = x } }
    }
    |}];
  check
    {|
struct {
  val S1 = sig {
    val T = Int
  }
}
      |};
  [%expect {| sig { val S1 = sig { val T = Int } } |}];
  check
    {|
struct {
  val S1 = sig {
    val T
  }
}
      |};
  [%expect
    {|
    error: Signature declarations require either a type annotation or a definition
     --> <input>:4:5
      |
    4 |     val T
      |     ^^^^^
    |}]
;;

let%expect_test "eta laws" =
  check
    {|
struct {
  val f : Type -> Type = fun x -> Unit
  val x : sig { val f = f } = struct { val f : Type -> Type = fun x -> f x }
}
    |};
  [%expect {| sig { val f = fun x -> Unit; val x = struct (val f = fun x -> f x) } |}]
;;

let%expect_test "eta laws 2" =
  check
    {|
struct {
  val f : Type -> Type = fun x -> Unit
  val x : sig { val f = f } = struct { val f : Type -> Type = fun x -> f x }
  val y : sig { val f = fun (x : Type) -> f x } = struct { val f = f }
  val S = sig { val T : Type; val U : Type -> Type; val x : T }
  val m : S = struct {
    val T : Type = Unit
    val U : Type -> Type = fun (x : Type) -> Unit
    val x : T = ()
  }
  val z1 : sig { val v = m } = struct { val v : S = struct { val T = m.T; val U = m.U; val x = m.x } }
  val z3 : sig { val v = m } = struct { val v = m }
  val z5 : sig { val v = m } = struct { val v : S = struct { val T = m.T; val U = fun (x : Type) -> m.U x; val x = m.x } }
}
|};
  [%expect
    {|
    sig {
      val f = fun x -> Unit
      val x = struct (val f = fun x -> f x)
      val y = struct (val f = f)
      val S = sig { val T : Type; val U : Type -> Type; val x : T }
      val m = struct (val T = Unit, val U = fun x -> Unit, val x = ignore)
      val z1 = struct (val v = struct (val T = m.T, val U = m.U, val x = m.x))
      val z3 = struct (val v = m)
      val z5 = struct (val v = struct (val T = m.T, val U = fun x -> m.U x, val x = m.x))
    }
    |}]
;;
