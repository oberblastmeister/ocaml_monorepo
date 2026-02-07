(* open Prelude
module Syntax = Oak_syntax
module Parse = Oak_parse
module Diagnostic = Oak_diagnostic

let check s =
  let file = "<input>" in
  let _tts, diagnostics, expr = Parse.parse ~file s in
  let files = String.Map.of_alist_exn [ file, Diagnostic.Snippet.File.create s ] in
  if not (List.is_empty diagnostics)
  then begin
    List.iter diagnostics ~f:(fun diagnostic ->
      Diagnostic.print ~color:false ~files diagnostic;
      print_string "\n\n")
  end
  else print_s [%sexp (expr : Syntax.expr)]
;;

let%expect_test "error" =
  check
    {|
        mod {
        let 1234234
        }
        |};
  [%expect
    {|
    error[E0001]: Expected variable after let
     --> <input>:3:9
      |
    3 |         let 1234234
      |         ^^^
    |}]
;;

let%expect_test "mismatching" =
  check
    {|
      let first ( }
      |};
  [%expect
    {|
    error[E0001]: mismatching delimiters
     --> <input>:2:19
      |
    2 |       let first ( }
      |                   ^
    note: opening delimiter here
     --> <input>:2:17
      |
    2 |       let first ( }
      |                 ^

    error[E0001]: Unconsumed tokens in expression
     --> <input>:2:11
      |
    2 |       let first ( }
      |           ^^^^^
    |}]
;;

let%expect_test "mod with let declarations" =
  check
    {|
mod {

let first (12341342 :: Int)

let second 13432

let third 1323243

}
        |};
  [%expect
    {|
    (Expr_mod (var ((id 0) (pos (1))))
     (decls
      (((let_pos 6) (field first) (field_pos 8)
        (e
         (Expr_seal (e (Expr_int (value 12341342) (span ((start 11) (stop 12)))))
          (ty (Expr_core_ty (ty Ty_int) (span ((start 15) (stop 16)))))
          (span ((start 11) (stop 16)))))
        (span ((start 6) (stop 16))))
       ((let_pos 20) (field second) (field_pos 22)
        (e (Expr_int (value 13432) (span ((start 24) (stop 25)))))
        (span ((start 20) (stop 25))))
       ((let_pos 28) (field third) (field_pos 30)
        (e (Expr_int (value 1323243) (span ((start 32) (stop 33)))))
        (span ((start 28) (stop 33))))))
     (span ((start 1) (stop 33))))
    |}]
;;

let%expect_test "function types taking variables" =
  check
    {|
Fun(x Int, A Type) A
    |};
  [%expect
    {|
    (Expr_ty_fun
     ((params
       (((var ((id 0) (name x) (pos (3))))
         (ty (Expr_core_ty (ty Ty_int) (span ((start 5) (stop 6)))))
         (span ((start 3) (stop 6))))
        ((var ((id 0) (name A) (pos (8))))
         (ty (Expr_universe (univ Type) (span ((start 10) (stop 11)))))
         (span ((start 8) (stop 11))))))
      (body_ty
       (Expr_var (var (Var ((id 0) (name A) (pos (13)))))
        (span ((start 13) (stop 14)))))
      (purity Pure) (span ((start 1) (stop 14)))))
    |}]
;;

let%expect_test "function type with mixed params" =
  (* Mix of bound and unbound parameters *)
  check {| Fun(x Int, Bool) Int |};
  [%expect
    {|
    (Expr_ty_fun
     ((params
       (((var ((id 0) (name x) (pos (3))))
         (ty (Expr_core_ty (ty Ty_int) (span ((start 5) (stop 6)))))
         (span ((start 3) (stop 6))))
        ((var ((id 0) (name _) (pos (8))))
         (ty (Expr_core_ty (ty Ty_bool) (span ((start 8) (stop 9)))))
         (span ((start 8) (stop 9))))))
      (body_ty (Expr_core_ty (ty Ty_int) (span ((start 11) (stop 12)))))
      (purity Pure) (span ((start 1) (stop 12)))))
    |}]
;;

let%expect_test "function type param with non-variable name" =
  (* Using a non-variable as the name should error *)
  check {| Fun(123 Int) Int |};
  [%expect
    {|
    error[E0001]: Expected variable name
     --> <input>:1:6
      |
    1 |  Fun(123 Int) Int
      |      ^^^
    |}]
;;

let%expect_test "function type param with extra tokens" =
  check {| Fun(x Int Int) Int |};
  [%expect
    {|
    error[E0001]: Unconsumed tokens in parameter
     --> <input>:1:12
      |
    1 |  Fun(x Int Int) Int
      |            ^^^
    |}]
;;

let%expect_test "function types" =
  check
    {|
Fun(Int, Bool) Fun(Int) Int
        |};
  check
    {|
Funct(Int, Int, Bool) Funct(Int) Int
        |};
  [%expect
    {|
    (Expr_ty_fun
     ((params
       (((var ((id 0) (name _) (pos (3))))
         (ty (Expr_core_ty (ty Ty_int) (span ((start 3) (stop 4)))))
         (span ((start 3) (stop 4))))
        ((var ((id 0) (name _) (pos (6))))
         (ty (Expr_core_ty (ty Ty_bool) (span ((start 6) (stop 7)))))
         (span ((start 6) (stop 7))))))
      (body_ty
       (Expr_ty_fun
        ((params
          (((var ((id 0) (name _) (pos (11))))
            (ty (Expr_core_ty (ty Ty_int) (span ((start 11) (stop 12)))))
            (span ((start 11) (stop 12))))))
         (body_ty (Expr_core_ty (ty Ty_int) (span ((start 14) (stop 15)))))
         (purity Pure) (span ((start 9) (stop 15))))))
      (purity Pure) (span ((start 1) (stop 15)))))
    (Expr_ty_fun
     ((params
       (((var ((id 0) (name _) (pos (3))))
         (ty (Expr_core_ty (ty Ty_int) (span ((start 3) (stop 4)))))
         (span ((start 3) (stop 4))))
        ((var ((id 0) (name _) (pos (6))))
         (ty (Expr_core_ty (ty Ty_int) (span ((start 6) (stop 7)))))
         (span ((start 6) (stop 7))))
        ((var ((id 0) (name _) (pos (9))))
         (ty (Expr_core_ty (ty Ty_bool) (span ((start 9) (stop 10)))))
         (span ((start 9) (stop 10))))))
      (body_ty
       (Expr_ty_fun
        ((params
          (((var ((id 0) (name _) (pos (14))))
            (ty (Expr_core_ty (ty Ty_int) (span ((start 14) (stop 15)))))
            (span ((start 14) (stop 15))))))
         (body_ty (Expr_core_ty (ty Ty_int) (span ((start 17) (stop 18)))))
         (purity Impure) (span ((start 12) (stop 18))))))
      (purity Impure) (span ((start 1) (stop 18)))))
    |}];
  check
    {|
Fun(Int, Bool) -
      |};
  [%expect
    {|
    error[E0001]: expected atom expression
     --> <input>:2:16
      |
    2 | Fun(Int, Bool) -
      |                ^
    |}]
;;

let%expect_test "function expressions" =
  check
    {|
fun(x Int, y Int, z Int) {
  x
}
|};
  [%expect
    {|
    (Expr_abs
     (params
      (((var ((id 0) (name x) (pos (3))))
        (ty (Expr_core_ty (ty Ty_int) (span ((start 5) (stop 6)))))
        (span ((start 3) (stop 6))))
       ((var ((id 0) (name y) (pos (8))))
        (ty (Expr_core_ty (ty Ty_int) (span ((start 10) (stop 11)))))
        (span ((start 8) (stop 11))))
       ((var ((id 0) (name z) (pos (13))))
        (ty (Expr_core_ty (ty Ty_int) (span ((start 15) (stop 16)))))
        (span ((start 13) (stop 16))))))
     (body
      (Expr_var (var (Var ((id 0) (name x) (pos (21)))))
       (span ((start 21) (stop 22)))))
     (purity Pure) (span ((start 1) (stop 22))))
    |}]
;;

let%expect_test "function expression with return type annotation" =
  check
    {|
fun(x Int) Int {
  x
}
|};
  [%expect
    {|
    (Expr_abs
     (params
      (((var ((id 0) (name x) (pos (3))))
        (ty (Expr_core_ty (ty Ty_int) (span ((start 5) (stop 6)))))
        (span ((start 3) (stop 6))))))
     (body
      (Expr_seal
       (e
        (Expr_var (var (Var ((id 0) (name x) (pos (13)))))
         (span ((start 13) (stop 14)))))
       (ty (Expr_core_ty (ty Ty_int) (span ((start 8) (stop 9)))))
       (span ((start 13) (stop 14)))))
     (purity Pure) (span ((start 1) (stop 14))))
    |}]
;;

let%expect_test "impure function expression" =
  check
    {|
funct(x Int) {
  x
}
|};
  [%expect
    {|
    (Expr_abs
     (params
      (((var ((id 0) (name x) (pos (3))))
        (ty (Expr_core_ty (ty Ty_int) (span ((start 5) (stop 6)))))
        (span ((start 3) (stop 6))))))
     (body
      (Expr_var (var (Var ((id 0) (name x) (pos (11)))))
       (span ((start 11) (stop 12)))))
     (purity Impure) (span ((start 1) (stop 12))))
    |}]
;;

let%expect_test "function expression in let binding" =
  check
    {|
mod {
  let add fun(x Int, y Int) {
    x
  }
}
|};
  [%expect
    {|
    (Expr_mod (var ((id 0) (pos (1))))
     (decls
      (((let_pos 6) (field add) (field_pos 8)
        (e
         (Expr_abs
          (params
           (((var ((id 0) (name x) (pos (12))))
             (ty (Expr_core_ty (ty Ty_int) (span ((start 14) (stop 15)))))
             (span ((start 12) (stop 15))))
            ((var ((id 0) (name y) (pos (17))))
             (ty (Expr_core_ty (ty Ty_int) (span ((start 19) (stop 20)))))
             (span ((start 17) (stop 20))))))
          (body
           (Expr_var (var (Var ((id 0) (name x) (pos (25)))))
            (span ((start 25) (stop 26)))))
          (purity Pure) (span ((start 10) (stop 26)))))
        (span ((start 6) (stop 26))))))
     (span ((start 1) (stop 26))))
    |}]
;;

let%expect_test "variable expressions" =
  check {| foo |};
  [%expect
    {|
    (Expr_var (var (Var ((id 0) (name foo) (pos (1)))))
     (span ((start 1) (stop 2))))
    |}];
  check {| (foo) |};
  [%expect
    {|
    (Expr_var (var (Var ((id 0) (name foo) (pos (2)))))
     (span ((start 2) (stop 3))))
    |}]
;;

let%expect_test "function expression missing params" =
  check
    {|
fun {
  x
}
|};
  [%expect
    {|
    error[E0001]: expected atom expression
     --> <input>:3:4
      |
    3 |   x
      |    ^...
    |}]
;;

let%expect_test "function expression missing body" =
  check {| fun(x Int) |};
  [%expect
    {|
    error[E0001]: Expected block after fun parameters
     --> <input>:1:2
      |
    1 |  fun(x Int)
      |  ^^^
    |}]
;;

let%expect_test "function expression param missing type" =
  check
    {|
fun(x) {
  x
}
|};
  [%expect
    {|
    error[E0001]: expected atom expression
     --> <input>:2:6
      |
    2 | fun(x) {
      |      ^
    |}]
;;

let%expect_test "function expression param missing name" =
  check
    {|
fun(Int) {
  x
}
|};
  [%expect
    {|
    error[E0001]: expected atom expression
     --> <input>:2:8
      |
    2 | fun(Int) {
      |        ^
    |}]
;;

let%expect_test "function expression param with extra tokens" =
  check
    {|
fun(x Int Int) {
  x
}
|};
  [%expect
    {|
    error[E0001]: Unconsumed tokens in parameter
     --> <input>:2:11
      |
    2 | fun(x Int Int) {
      |           ^^^
    |}]
;;

let%expect_test "signatures" =
  check
    {|
sig {
  let hello Int
  let another Bool
  let T Type
  let unit Unit
  let wow Sig
  let wow Kind
}
    |};
  [%expect
    {|
    (Expr_ty_mod
     ((var ((id 0) (pos (1))))
      (ty_decls
       (((field hello) (field_pos 8)
         (ty (Expr_core_ty (ty Ty_int) (span ((start 10) (stop 11)))))
         (span ((start 6) (stop 11))))
        ((field another) (field_pos 16)
         (ty (Expr_core_ty (ty Ty_bool) (span ((start 18) (stop 19)))))
         (span ((start 14) (stop 19))))
        ((field T) (field_pos 24)
         (ty (Expr_universe (univ Type) (span ((start 26) (stop 27)))))
         (span ((start 22) (stop 27))))
        ((field unit) (field_pos 32)
         (ty (Expr_core_ty (ty Ty_unit) (span ((start 34) (stop 35)))))
         (span ((start 30) (stop 35))))
        ((field wow) (field_pos 40)
         (ty (Expr_universe (univ Sig) (span ((start 42) (stop 43)))))
         (span ((start 38) (stop 43))))
        ((field wow) (field_pos 48)
         (ty (Expr_universe (univ Kind) (span ((start 50) (stop 51)))))
         (span ((start 46) (stop 51))))))
      (span ((start 1) (stop 51)))))
    |}]
;;

let%expect_test "signature missing block" =
  check {| sig |};
  [%expect
    {|
    error[E0001]: Expected block after sig
     --> <input>:1:2
      |
    1 |  sig
      |  ^^^
    |}]
;;

let%expect_test "signature with extra tokens" =
  check
    {|
sig foo {
  let x Int
}
|};
  [%expect
    {|
    error[E0001]: Expected block after sig
     --> <input>:2:1
      |
    2 | sig foo {
      | ^^^
    |}]
;;

let%expect_test "signature decl missing field name" =
  check
    {|
sig {
  let Int
}
|};
  [%expect
    {|
    error[E0001]: expected atom expression
     --> <input>:3:10
      |
    3 |   let Int
      |          ^...
    |}]
;;

let%expect_test "signature decl with extra tokens" =
  check
    {|
sig {
  let x Int Int
}
|};
  [%expect
    {|
    error[E0001]: Unconsumed tokens in type declaration
     --> <input>:3:13
      |
    3 |   let x Int Int
      |             ^^^
    |}]
;;

let%expect_test "empty signature" =
  check
    {|
sig {
}
|};
  [%expect
    {|
    (Expr_ty_mod
     ((var ((id 0) (pos (1)))) (ty_decls ()) (span ((start 1) (stop 4)))))
    |}]
;;

let%expect_test "function application" =
  check
    {|
hello(1234, 1234, hello(1234, 1233, 4))
    |};
  [%expect
    {|
    (Expr_app
     (func
      (Expr_var (var (Var ((id 0) (name hello) (pos (1)))))
       (span ((start 1) (stop 2)))))
     (args
      ((Expr_int (value 1234) (span ((start 3) (stop 4))))
       (Expr_int (value 1234) (span ((start 6) (stop 7))))
       (Expr_app
        (func
         (Expr_var (var (Var ((id 0) (name hello) (pos (9)))))
          (span ((start 9) (stop 10)))))
        (args
         ((Expr_int (value 1234) (span ((start 11) (stop 12))))
          (Expr_int (value 1233) (span ((start 14) (stop 15))))
          (Expr_int (value 4) (span ((start 17) (stop 18))))))
        (span ((start 9) (stop 19))))))
     (span ((start 1) (stop 20))))
    |}]
;;

let%expect_test "function application with single argument" =
  check {| f(x) |};
  [%expect
    {|
    (Expr_app
     (func
      (Expr_var (var (Var ((id 0) (name f) (pos (1)))))
       (span ((start 1) (stop 2)))))
     (args
      ((Expr_var (var (Var ((id 0) (name x) (pos (3)))))
        (span ((start 3) (stop 4))))))
     (span ((start 1) (stop 5))))
    |}]
;;

let%expect_test "function application with no arguments" =
  check {| f() |};
  [%expect
    {|
    (Expr_app
     (func
      (Expr_var (var (Var ((id 0) (name f) (pos (1)))))
       (span ((start 1) (stop 2)))))
     (args ()) (span ((start 1) (stop 4))))
    |}]
;;

let%expect_test "chained function applications" =
  check {| f(x)(y)(z) |};
  [%expect
    {|
    (Expr_app
     (func
      (Expr_app
       (func
        (Expr_app
         (func
          (Expr_var (var (Var ((id 0) (name f) (pos (1)))))
           (span ((start 1) (stop 2)))))
         (args
          ((Expr_var (var (Var ((id 0) (name x) (pos (3)))))
            (span ((start 3) (stop 4))))))
         (span ((start 1) (stop 5)))))
       (args
        ((Expr_var (var (Var ((id 0) (name y) (pos (6)))))
          (span ((start 6) (stop 7))))))
       (span ((start 1) (stop 8)))))
     (args
      ((Expr_var (var (Var ((id 0) (name z) (pos (9)))))
        (span ((start 9) (stop 10))))))
     (span ((start 1) (stop 11))))
    |}]
;;

let%expect_test "application in type position - module" =
  check
    {|
mod {
  let x Option(Int)
}
|};
  [%expect
    {|
    (Expr_mod (var ((id 0) (pos (1))))
     (decls
      (((let_pos 6) (field x) (field_pos 8)
        (e
         (Expr_app
          (func
           (Expr_var (var (Var ((id 0) (name Option) (pos (10)))))
            (span ((start 10) (stop 11)))))
          (args ((Expr_core_ty (ty Ty_int) (span ((start 12) (stop 13))))))
          (span ((start 10) (stop 14)))))
        (span ((start 6) (stop 14))))))
     (span ((start 1) (stop 14))))
    |}]
;;

let%expect_test "application in function type return" =
  check
    {|
Fun(Int) List(Bool)
|};
  [%expect
    {|
    (Expr_ty_fun
     ((params
       (((var ((id 0) (name _) (pos (3))))
         (ty (Expr_core_ty (ty Ty_int) (span ((start 3) (stop 4)))))
         (span ((start 3) (stop 4))))))
      (body_ty
       (Expr_app
        (func
         (Expr_var (var (Var ((id 0) (name List) (pos (6)))))
          (span ((start 6) (stop 7)))))
        (args ((Expr_core_ty (ty Ty_bool) (span ((start 8) (stop 9))))))
        (span ((start 6) (stop 10)))))
      (purity Pure) (span ((start 1) (stop 10)))))
    |}]
;;

let%expect_test "nested applications in type" =
  check
    {|
Fun(Option(List(Int))) Result(Option(Bool), String)
|};
  [%expect
    {|
    (Expr_ty_fun
     ((params
       (((var ((id 0) (name Option) (pos (3))))
         (ty
          (Expr_app
           (func
            (Expr_var (var (Var ((id 0) (name List) (pos (5)))))
             (span ((start 5) (stop 6)))))
           (args ((Expr_core_ty (ty Ty_int) (span ((start 7) (stop 8))))))
           (span ((start 5) (stop 9)))))
         (span ((start 3) (stop 9))))))
      (body_ty
       (Expr_app
        (func
         (Expr_var (var (Var ((id 0) (name Result) (pos (12)))))
          (span ((start 12) (stop 13)))))
        (args
         ((Expr_app
           (func
            (Expr_var (var (Var ((id 0) (name Option) (pos (14)))))
             (span ((start 14) (stop 15)))))
           (args ((Expr_core_ty (ty Ty_bool) (span ((start 16) (stop 17))))))
           (span ((start 14) (stop 18))))
          (Expr_var (var (Var ((id 0) (name String) (pos (20)))))
           (span ((start 20) (stop 21))))))
        (span ((start 12) (stop 22)))))
      (purity Pure) (span ((start 1) (stop 22)))))
    |}]
;;

let%expect_test "application with complex expressions as arguments" =
  check
    {|
f(fun(x Int) { x }, g(y), z)
|};
  [%expect
    {|
    (Expr_app
     (func
      (Expr_var (var (Var ((id 0) (name f) (pos (1)))))
       (span ((start 1) (stop 2)))))
     (args
      ((Expr_abs
        (params
         (((var ((id 0) (name x) (pos (5))))
           (ty (Expr_core_ty (ty Ty_int) (span ((start 7) (stop 8)))))
           (span ((start 5) (stop 8))))))
        (body
         (Expr_var (var (Var ((id 0) (name x) (pos (12)))))
          (span ((start 12) (stop 13)))))
        (purity Pure) (span ((start 3) (stop 13))))
       (Expr_app
        (func
         (Expr_var (var (Var ((id 0) (name g) (pos (17)))))
          (span ((start 17) (stop 18)))))
        (args
         ((Expr_var (var (Var ((id 0) (name y) (pos (19)))))
           (span ((start 19) (stop 20))))))
        (span ((start 17) (stop 21))))
       (Expr_var (var (Var ((id 0) (name z) (pos (23)))))
        (span ((start 23) (stop 24))))))
     (span ((start 1) (stop 25))))
    |}]
;;

let%expect_test "application in signature type" =
  check
    {|
sig {
  let x List(Int)
}
|};
  [%expect
    {|
    (Expr_ty_mod
     ((var ((id 0) (pos (1))))
      (ty_decls
       (((field x) (field_pos 8)
         (ty
          (Expr_app
           (func
            (Expr_var (var (Var ((id 0) (name List) (pos (10)))))
             (span ((start 10) (stop 11)))))
           (args ((Expr_core_ty (ty Ty_int) (span ((start 12) (stop 13))))))
           (span ((start 10) (stop 14)))))
         (span ((start 6) (stop 14))))))
      (span ((start 1) (stop 14)))))
    |}]
;;

let%expect_test "application with mismatched delimiter" =
  check {| f(x, y] |};
  [%expect
    {|
    error[E0001]: mismatching delimiters
     --> <input>:1:8
      |
    1 |  f(x, y]
      |        ^
    note: opening delimiter here
     --> <input>:1:3
      |
    1 |  f(x, y]
      |   ^
    |}]
;;

let%expect_test "function without closing paren" =
  check {|f(x, y|};
  [%expect
    {|
    error[E0001]: expecting delimiter
     --> <input>:1:7
      |
    1 | f(x, y
      |       ^
    |}]
;; *)
