open Prelude
module Syntax = Oak_syntax
module Parse = Oak_parse
module Diagnostic = Oak_diagnostic

let check s =
  let file = "<input>" in
  let _tts, diagnostics, expr = Parse.parse ~file s in
  let files = String.Map.of_alist_exn [ file, Diagnostic.Snippet.File.create s ] in
  if not (List.is_empty diagnostics)
  then
    List.iter diagnostics ~f:(fun diagnostic ->
      Diagnostic.print ~color:false ~files diagnostic;
      print_endline "")
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
    error[E0001]: invalid atom expression
     --> <input>:2:16
      |
    2 | Fun(Int, Bool) -
      |                ^
    |}]
;;

let%expect_test "function expressions" =
  check
    {|
// aweaewf
    |}
;;
