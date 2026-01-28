open Prelude
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
        mod: {
        let: 1234234
        }
        |};
  [%expect
    {|
    error[E0001]: Expected variable after let
     --> <input>:3:9
      |
    3 |         let: 1234234
      |         ^^^
    |}]
;;

let%expect_test "mismatching" =
  check
    {|
      let first : ( }
      |};
  [%expect
    {|
    error[E0001]: mismatching delimiters
     --> <input>:2:21
      |
    2 |       let first : ( }
      |                     ^
    note: opening delimiter here
     --> <input>:2:19
      |
    2 |       let first : ( }
      |                   ^

    error[E0001]: Expression should not have block
     --> <input>:2:17
      |
    2 |       let first : ( }
      |                 ^
    |}]
;;

let%expect_test "mod with let declarations" =
  check
    {|
mod: {

let first: (12341342 :: Int)

let second: 13432

let third: 1323243

}
        |};
  [%expect
    {|
        (Expr_mod (var ((id -1) (pos (3))))
         (decls
          (((let_pos 9) (field first) (field_pos 11)
            (e
             (Expr_seal (e (Expr_int (value 12341342) (span ((start 16) (stop 17)))))
              (ty (Expr_core_ty (ty Ty_int) (span ((start 20) (stop 21)))))
              (span ((start 16) (stop 21)))))
            (span ((start 9) (stop 21))))
           ((let_pos 25) (field second) (field_pos 27)
            (e (Expr_int (value 13432) (span ((start 31) (stop 32)))))
            (span ((start 25) (stop 32))))
           ((let_pos 35) (field third) (field_pos 37)
            (e (Expr_int (value 1323243) (span ((start 41) (stop 42)))))
            (span ((start 35) (stop 42))))))
         (span ((start 3) (stop 42))))
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
       (((var ((id -1) (name x) (pos (5))))
         (ty (Expr_core_ty (ty Ty_int) (span ((start 7) (stop 8)))))
         (span ((start 5) (stop 8))))
        ((var ((id -1) (name A) (pos (10))))
         (ty (Expr_universe (univ Type) (span ((start 12) (stop 13)))))
         (span ((start 10) (stop 13))))))
      (body_ty
       (Expr_var (var (Var ((id -1) (name A) (pos (15)))))
        (span ((start 15) (stop 16)))))
      (purity Pure) (span ((start 3) (stop 16)))))
    |}]
;;

let%expect_test "function type with mixed params" =
  (* Mix of bound and unbound parameters *)
  check {| Fun(x Int, Bool) Int |};
  [%expect
    {|
    (Expr_ty_fun
     ((params
       (((var ((id -1) (name x) (pos (4))))
         (ty (Expr_core_ty (ty Ty_int) (span ((start 6) (stop 7)))))
         (span ((start 4) (stop 7))))
        ((var ((id -1) (name _) (pos (9))))
         (ty (Expr_core_ty (ty Ty_bool) (span ((start 9) (stop 10)))))
         (span ((start 9) (stop 10))))))
      (body_ty (Expr_core_ty (ty Ty_int) (span ((start 12) (stop 13)))))
      (purity Pure) (span ((start 2) (stop 13)))))
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
           (((var ((id -1) (name _) (pos (5))))
             (ty (Expr_core_ty (ty Ty_int) (span ((start 5) (stop 6)))))
             (span ((start 5) (stop 6))))
            ((var ((id -1) (name _) (pos (8))))
             (ty (Expr_core_ty (ty Ty_bool) (span ((start 8) (stop 9)))))
             (span ((start 8) (stop 9))))))
          (body_ty
           (Expr_ty_fun
            ((params
              (((var ((id -1) (name _) (pos (13))))
                (ty (Expr_core_ty (ty Ty_int) (span ((start 13) (stop 14)))))
                (span ((start 13) (stop 14))))))
             (body_ty (Expr_core_ty (ty Ty_int) (span ((start 16) (stop 17)))))
             (purity Pure) (span ((start 11) (stop 17))))))
          (purity Pure) (span ((start 3) (stop 17)))))
        (Expr_ty_fun
         ((params
           (((var ((id -1) (name _) (pos (5))))
             (ty (Expr_core_ty (ty Ty_int) (span ((start 5) (stop 6)))))
             (span ((start 5) (stop 6))))
            ((var ((id -1) (name _) (pos (8))))
             (ty (Expr_core_ty (ty Ty_int) (span ((start 8) (stop 9)))))
             (span ((start 8) (stop 9))))
            ((var ((id -1) (name _) (pos (11))))
             (ty (Expr_core_ty (ty Ty_bool) (span ((start 11) (stop 12)))))
             (span ((start 11) (stop 12))))))
          (body_ty
           (Expr_ty_fun
            ((params
              (((var ((id -1) (name _) (pos (16))))
                (ty (Expr_core_ty (ty Ty_int) (span ((start 16) (stop 17)))))
                (span ((start 16) (stop 17))))))
             (body_ty (Expr_core_ty (ty Ty_int) (span ((start 19) (stop 20)))))
             (purity Impure) (span ((start 14) (stop 20))))))
          (purity Impure) (span ((start 3) (stop 20)))))
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
fun(x Int, y Int, z Int):
  x
|};
  [%expect
    {|
    (Expr_abs
     (params
      (((var ((id -1) (name x) (pos (5))))
        (ty (Expr_core_ty (ty Ty_int) (span ((start 7) (stop 8)))))
        (span ((start 5) (stop 8))))
       ((var ((id -1) (name y) (pos (10))))
        (ty (Expr_core_ty (ty Ty_int) (span ((start 12) (stop 13)))))
        (span ((start 10) (stop 13))))
       ((var ((id -1) (name z) (pos (15))))
        (ty (Expr_core_ty (ty Ty_int) (span ((start 17) (stop 18)))))
        (span ((start 15) (stop 18))))))
     (body
      (Expr_var (var (Var ((id -1) (name x) (pos (23)))))
       (span ((start 23) (stop 24)))))
     (purity Pure) (span ((start 3) (stop 24))))
    |}]
;;

let%expect_test "function expression with return type annotation" =
  check
    {|
fun(x Int) Int:
  x
|};
  [%expect
    {|
    (Expr_abs
     (params
      (((var ((id -1) (name x) (pos (5))))
        (ty (Expr_core_ty (ty Ty_int) (span ((start 7) (stop 8)))))
        (span ((start 5) (stop 8))))))
     (body
      (Expr_seal
       (e
        (Expr_var (var (Var ((id -1) (name x) (pos (15)))))
         (span ((start 15) (stop 16)))))
       (ty (Expr_core_ty (ty Ty_int) (span ((start 10) (stop 11)))))
       (span ((start 15) (stop 16)))))
     (purity Pure) (span ((start 3) (stop 16))))
    |}]
;;

let%expect_test "impure function expression" =
  check
    {|
funct(x Int):
  x
|};
  [%expect
    {|
    (Expr_abs
     (params
      (((var ((id -1) (name x) (pos (5))))
        (ty (Expr_core_ty (ty Ty_int) (span ((start 7) (stop 8)))))
        (span ((start 5) (stop 8))))))
     (body
      (Expr_var (var (Var ((id -1) (name x) (pos (13)))))
       (span ((start 13) (stop 14)))))
     (purity Impure) (span ((start 3) (stop 14))))
    |}]
;;

let%expect_test "function expression in let binding" =
  check
    {|
mod: {
  let add:
    fun(x Int, y Int):
      x
}
|};
  [%expect
    {|
    (Expr_mod (var ((id -1) (pos (3))))
     (decls
      (((let_pos 9) (field add) (field_pos 11)
        (e
         (Expr_abs
          (params
           (((var ((id -1) (name x) (pos (18))))
             (ty (Expr_core_ty (ty Ty_int) (span ((start 20) (stop 21)))))
             (span ((start 18) (stop 21))))
            ((var ((id -1) (name y) (pos (23))))
             (ty (Expr_core_ty (ty Ty_int) (span ((start 25) (stop 26)))))
             (span ((start 23) (stop 26))))))
          (body
           (Expr_var (var (Var ((id -1) (name x) (pos (31)))))
            (span ((start 31) (stop 32)))))
          (purity Pure) (span ((start 16) (stop 32)))))
        (span ((start 9) (stop 32))))))
     (span ((start 3) (stop 32))))
    |}]
;;

let%expect_test "variable expressions" =
  check {| foo |};
  [%expect
    {|
    (Expr_var (var (Var ((id -1) (name foo) (pos (2)))))
     (span ((start 2) (stop 3))))
    |}];
  check {| (foo) |};
  [%expect
    {|
    (Expr_var (var (Var ((id -1) (name foo) (pos (3)))))
     (span ((start 3) (stop 4))))
    |}]
;;

let%expect_test "function expression missing params" =
  check
    {|
fun:
  x
|};
  [%expect
    {|
    error[E0001]: Expected parameter list after fun/funct
     --> <input>:2:1
      |
    2 | fun:
      | ^^^
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
fun(x):
  x
|};
  [%expect
    {|
    error[E0001]: expected atom expression
     --> <input>:2:6
      |
    2 | fun(x):
      |      ^
    |}]
;;

let%expect_test "function expression param missing name" =
  check
    {|
fun(Int):
  x
|};
  [%expect
    {|
    error[E0001]: expected atom expression
     --> <input>:2:8
      |
    2 | fun(Int):
      |        ^
    |}]
;;

let%expect_test "function expression param with extra tokens" =
  check
    {|
fun(x Int Int):
  x
|};
  [%expect
    {|
    error[E0001]: Unconsumed tokens in parameter
     --> <input>:2:11
      |
    2 | fun(x Int Int):
      |           ^^^
    |}]
;;

let%expect_test "signatures" =
  check
    {|
sig:
  let hello Int
  let another Bool
  let T Type
  let unit Unit
  let wow Sig
  let wow Kind
    |};
  [%expect
    {|
    (Expr_ty_mod
     ((var ((id -1) (pos (3))))
      (ty_decls
       (((field hello) (field_pos 10)
         (ty (Expr_core_ty (ty Ty_int) (span ((start 12) (stop 13)))))
         (span ((start 8) (stop 13))))
        ((field another) (field_pos 18)
         (ty (Expr_core_ty (ty Ty_bool) (span ((start 20) (stop 21)))))
         (span ((start 16) (stop 21))))
        ((field T) (field_pos 26)
         (ty (Expr_universe (univ Type) (span ((start 28) (stop 29)))))
         (span ((start 24) (stop 29))))
        ((field unit) (field_pos 34)
         (ty (Expr_core_ty (ty Ty_unit) (span ((start 36) (stop 37)))))
         (span ((start 32) (stop 37))))
        ((field wow) (field_pos 42)
         (ty (Expr_universe (univ Sig) (span ((start 44) (stop 45)))))
         (span ((start 40) (stop 45))))
        ((field wow) (field_pos 50)
         (ty (Expr_universe (univ Kind) (span ((start 52) (stop 53)))))
         (span ((start 48) (stop 53))))))
      (span ((start 3) (stop 53)))))
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
sig foo:
  let x Int
|};
  [%expect
    {|
    error[E0001]: Unexpected tokens after sig
     --> <input>:2:5
      |
    2 | sig foo:
      |     ^^^
    |}]
;;

let%expect_test "signature decl missing field name" =
  check
    {|
sig:
  let Int
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
sig:
  let x Int Int
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
sig: {
}
|};
  [%expect
    {|
    (Expr_ty_mod
     ((var ((id -1) (pos (3)))) (ty_decls ()) (span ((start 3) (stop 5)))))
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
      (Expr_var (var (Var ((id -1) (name hello) (pos (3)))))
       (span ((start 3) (stop 4)))))
     (args
      ((Expr_int (value 1234) (span ((start 5) (stop 6))))
       (Expr_int (value 1234) (span ((start 8) (stop 9))))
       (Expr_app
        (func
         (Expr_var (var (Var ((id -1) (name hello) (pos (11)))))
          (span ((start 11) (stop 12)))))
        (args
         ((Expr_int (value 1234) (span ((start 13) (stop 14))))
          (Expr_int (value 1233) (span ((start 16) (stop 17))))
          (Expr_int (value 4) (span ((start 19) (stop 20))))))
        (span ((start 11) (stop 21))))))
     (span ((start 3) (stop 22))))
    |}]
;;

let%expect_test "function application with single argument" =
  check {| f(x) |};
  [%expect
    {|
    (Expr_app
     (func
      (Expr_var (var (Var ((id -1) (name f) (pos (2)))))
       (span ((start 2) (stop 3)))))
     (args
      ((Expr_var (var (Var ((id -1) (name x) (pos (4)))))
        (span ((start 4) (stop 5))))))
     (span ((start 2) (stop 6))))
    |}]
;;

let%expect_test "function application with no arguments" =
  check {| f() |};
  [%expect
    {|
    (Expr_app
     (func
      (Expr_var (var (Var ((id -1) (name f) (pos (2)))))
       (span ((start 2) (stop 3)))))
     (args ()) (span ((start 2) (stop 5))))
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
          (Expr_var (var (Var ((id -1) (name f) (pos (2)))))
           (span ((start 2) (stop 3)))))
         (args
          ((Expr_var (var (Var ((id -1) (name x) (pos (4)))))
            (span ((start 4) (stop 5))))))
         (span ((start 2) (stop 6)))))
       (args
        ((Expr_var (var (Var ((id -1) (name y) (pos (7)))))
          (span ((start 7) (stop 8))))))
       (span ((start 2) (stop 9)))))
     (args
      ((Expr_var (var (Var ((id -1) (name z) (pos (10)))))
        (span ((start 10) (stop 11))))))
     (span ((start 2) (stop 12))))
    |}]
;;

let%expect_test "application in type position - module" =
  check
    {|
mod:
  let x: Option(Int)
|};
  [%expect
    {|
    (Expr_mod (var ((id -1) (pos (3))))
     (decls
      (((let_pos 8) (field x) (field_pos 10)
        (e
         (Expr_app
          (func
           (Expr_var (var (Var ((id -1) (name Option) (pos (14)))))
            (span ((start 14) (stop 15)))))
          (args ((Expr_core_ty (ty Ty_int) (span ((start 16) (stop 17))))))
          (span ((start 14) (stop 18)))))
        (span ((start 8) (stop 18))))))
     (span ((start 3) (stop 18))))
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
       (((var ((id -1) (name _) (pos (5))))
         (ty (Expr_core_ty (ty Ty_int) (span ((start 5) (stop 6)))))
         (span ((start 5) (stop 6))))))
      (body_ty
       (Expr_app
        (func
         (Expr_var (var (Var ((id -1) (name List) (pos (8)))))
          (span ((start 8) (stop 9)))))
        (args ((Expr_core_ty (ty Ty_bool) (span ((start 10) (stop 11))))))
        (span ((start 8) (stop 12)))))
      (purity Pure) (span ((start 3) (stop 12)))))
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
       (((var ((id -1) (name Option) (pos (5))))
         (ty
          (Expr_app
           (func
            (Expr_var (var (Var ((id -1) (name List) (pos (7)))))
             (span ((start 7) (stop 8)))))
           (args ((Expr_core_ty (ty Ty_int) (span ((start 9) (stop 10))))))
           (span ((start 7) (stop 11)))))
         (span ((start 5) (stop 11))))))
      (body_ty
       (Expr_app
        (func
         (Expr_var (var (Var ((id -1) (name Result) (pos (14)))))
          (span ((start 14) (stop 15)))))
        (args
         ((Expr_app
           (func
            (Expr_var (var (Var ((id -1) (name Option) (pos (16)))))
             (span ((start 16) (stop 17)))))
           (args ((Expr_core_ty (ty Ty_bool) (span ((start 18) (stop 19))))))
           (span ((start 16) (stop 20))))
          (Expr_var (var (Var ((id -1) (name String) (pos (22)))))
           (span ((start 22) (stop 23))))))
        (span ((start 14) (stop 24)))))
      (purity Pure) (span ((start 3) (stop 24)))))
    |}]
;;

let%expect_test "application with complex expressions as arguments" =
  check
    {|
f(fun(x Int): x, g(y), z)
|};
  [%expect
    {|
    (Expr_app
     (func
      (Expr_var (var (Var ((id -1) (name f) (pos (3)))))
       (span ((start 3) (stop 4)))))
     (args
      ((Expr_abs
        (params
         (((var ((id -1) (name x) (pos (7))))
           (ty (Expr_core_ty (ty Ty_int) (span ((start 9) (stop 10)))))
           (span ((start 7) (stop 10))))))
        (body
         (Expr_var (var (Var ((id -1) (name x) (pos (14)))))
          (span ((start 14) (stop 15)))))
        (purity Pure) (span ((start 5) (stop 15))))
       (Expr_app
        (func
         (Expr_var (var (Var ((id -1) (name g) (pos (18)))))
          (span ((start 18) (stop 19)))))
        (args
         ((Expr_var (var (Var ((id -1) (name y) (pos (20)))))
           (span ((start 20) (stop 21))))))
        (span ((start 18) (stop 22))))
       (Expr_var (var (Var ((id -1) (name z) (pos (24)))))
        (span ((start 24) (stop 25))))))
     (span ((start 3) (stop 26))))
    |}]
;;

let%expect_test "application in signature type" =
  check
    {|
sig:
  let x List(Int)
|};
  [%expect
    {|
    (Expr_ty_mod
     ((var ((id -1) (pos (3))))
      (ty_decls
       (((field x) (field_pos 10)
         (ty
          (Expr_app
           (func
            (Expr_var (var (Var ((id -1) (name List) (pos (12)))))
             (span ((start 12) (stop 13)))))
           (args ((Expr_core_ty (ty Ty_int) (span ((start 14) (stop 15))))))
           (span ((start 12) (stop 16)))))
         (span ((start 8) (stop 16))))))
      (span ((start 3) (stop 16)))))
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
;;
