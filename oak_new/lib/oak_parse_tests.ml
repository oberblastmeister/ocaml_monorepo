open Core
module Snippet = Utility.Diagnostic.Snippet

let check s =
  let file = "<input>" in
  let _source, diagnostics, expr = Oak_parse.parse ~file s in
  let files = String.Map.of_alist_exn [ file, Snippet.File.create s ] in
  if not (List.is_empty diagnostics)
  then
    List.iter diagnostics ~f:(fun diagnostic ->
      Oak_diagnostic.print ~color:false ~files diagnostic;
      print_string "\n\n")
  else print_s [%sexp (expr : Oak_surface.expr option)]
;;

let%expect_test "smoke" =
  check
    {|
(fun x -> x : f Int -> g Bool -> g Int)
    |};
  [%expect
    {|
    ((Expr_paren
      (e
       (Expr_ann
        (e
         (Expr_fun
          (params
           (((relevancy Relevant)
             (names (((name x) (span ((start 4) (stop 5)))))) (ann ())
             (icit Expl) (span ((start 4) (stop 5))))))
          (ret_ty ()) (body (Expr_var ((name x) (span ((start 8) (stop 9))))))
          (span ((start 2) (stop 9)))))
        (ty
         (Expr_ty_fun
          (param_tys
           (((relevancy Relevant) (names ())
             (ty
              ((Expr_app
                (func (Expr_var ((name f) (span ((start 12) (stop 13))))))
                (args
                 (((arg (Expr_core_ty (ty Int) (span ((start 14) (stop 15)))))
                   (relevancy Relevant) (icit Expl))))
                (span ((start 12) (stop 15))))))
             (icit Expl) (span ((start 12) (stop 15))))
            ((relevancy Relevant) (names ())
             (ty
              ((Expr_app
                (func (Expr_var ((name g) (span ((start 18) (stop 19))))))
                (args
                 (((arg (Expr_core_ty (ty Bool) (span ((start 20) (stop 21)))))
                   (relevancy Relevant) (icit Expl))))
                (span ((start 18) (stop 21))))))
             (icit Expl) (span ((start 18) (stop 21))))))
          (body_ty
           (Expr_app (func (Expr_var ((name g) (span ((start 24) (stop 25))))))
            (args
             (((arg (Expr_core_ty (ty Int) (span ((start 26) (stop 27)))))
               (relevancy Relevant) (icit Expl))))
            (span ((start 24) (stop 27)))))
          (span ((start 12) (stop 27)))))
        (span ((start 2) (stop 27)))))
      (span ((start 2) (stop 27)))))
    |}]
;;

let%expect_test "simple function" =
  check
    {|
fun x y z -> x
      |};
  [%expect
    {|
    ((Expr_fun
      (params
       (((relevancy Relevant) (names (((name x) (span ((start 3) (stop 4))))))
         (ann ()) (icit Expl) (span ((start 3) (stop 4))))
        ((relevancy Relevant) (names (((name y) (span ((start 5) (stop 6))))))
         (ann ()) (icit Expl) (span ((start 5) (stop 6))))
        ((relevancy Relevant) (names (((name z) (span ((start 7) (stop 8))))))
         (ann ()) (icit Expl) (span ((start 7) (stop 8))))))
      (ret_ty ()) (body (Expr_var ((name x) (span ((start 11) (stop 12))))))
      (span ((start 1) (stop 12)))))
    |}];
  check
    {|
      fun (x w a : Bool) (y : Bool) z -> x
      |};
  [%expect
    {|
    ((Expr_fun
      (params
       (((relevancy Relevant)
         (names
          (((name x) (span ((start 5) (stop 6))))
           ((name w) (span ((start 7) (stop 8))))
           ((name a) (span ((start 9) (stop 10))))))
         (ann ((Expr_core_ty (ty Bool) (span ((start 13) (stop 14))))))
         (icit Expl) (span ((start 5) (stop 14))))
        ((relevancy Relevant) (names (((name y) (span ((start 17) (stop 18))))))
         (ann ((Expr_core_ty (ty Bool) (span ((start 21) (stop 22))))))
         (icit Expl) (span ((start 17) (stop 22))))
        ((relevancy Relevant) (names (((name z) (span ((start 24) (stop 25))))))
         (ann ()) (icit Expl) (span ((start 24) (stop 25))))))
      (ret_ty ()) (body (Expr_var ((name x) (span ((start 28) (stop 29))))))
      (span ((start 2) (stop 29)))))
    |}];
  check
    {|
(fun x y -> x : Bool -> x)
      |};
  [%expect
    {|
    ((Expr_paren
      (e
       (Expr_ann
        (e
         (Expr_fun
          (params
           (((relevancy Relevant)
             (names (((name x) (span ((start 4) (stop 5)))))) (ann ())
             (icit Expl) (span ((start 4) (stop 5))))
            ((relevancy Relevant)
             (names (((name y) (span ((start 6) (stop 7)))))) (ann ())
             (icit Expl) (span ((start 6) (stop 7))))))
          (ret_ty ()) (body (Expr_var ((name x) (span ((start 10) (stop 11))))))
          (span ((start 2) (stop 11)))))
        (ty
         (Expr_ty_fun
          (param_tys
           (((relevancy Relevant) (names ())
             (ty ((Expr_core_ty (ty Bool) (span ((start 14) (stop 15))))))
             (icit Expl) (span ((start 14) (stop 15))))))
          (body_ty (Expr_var ((name x) (span ((start 18) (stop 19))))))
          (span ((start 14) (stop 19)))))
        (span ((start 2) (stop 19)))))
      (span ((start 2) (stop 19)))))
    |}]
;;

let%expect_test "blocks" =
  check
    {|
  fun x y -> {
    val z = x
    val w = y
    w
  }
    |};
  [%expect
    {|
    ((Expr_fun
      (params
       (((relevancy Relevant) (names (((name x) (span ((start 4) (stop 5))))))
         (ann ()) (icit Expl) (span ((start 4) (stop 5))))
        ((relevancy Relevant) (names (((name y) (span ((start 6) (stop 7))))))
         (ann ()) (icit Expl) (span ((start 6) (stop 7))))))
      (ret_ty ())
      (body
       (Expr_block
        (decls
         ((Block_decl_val
           ((relevancy Relevant) (name ((name z) (span ((start 15) (stop 16)))))
            (ann ()) (is_abstract false)
            (rhs (Expr_var ((name x) (span ((start 19) (stop 20))))))
            (span ((start 13) (stop 20)))))
          (Block_decl_val
           ((relevancy Relevant) (name ((name w) (span ((start 25) (stop 26)))))
            (ann ()) (is_abstract false)
            (rhs (Expr_var ((name y) (span ((start 29) (stop 30))))))
            (span ((start 23) (stop 30)))))))
        (ret (Expr_var ((name w) (span ((start 33) (stop 34))))))
        (span ((start 10) (stop 37)))))
      (span ((start 2) (stop 37)))))
    |}];
  check
    {|
fun x y z w -> {
  val w = {
    val x = x
    x
  }
  { w }
}
      |};
  [%expect
    {|
    ((Expr_fun
      (params
       (((relevancy Relevant) (names (((name x) (span ((start 3) (stop 4))))))
         (ann ()) (icit Expl) (span ((start 3) (stop 4))))
        ((relevancy Relevant) (names (((name y) (span ((start 5) (stop 6))))))
         (ann ()) (icit Expl) (span ((start 5) (stop 6))))
        ((relevancy Relevant) (names (((name z) (span ((start 7) (stop 8))))))
         (ann ()) (icit Expl) (span ((start 7) (stop 8))))
        ((relevancy Relevant) (names (((name w) (span ((start 9) (stop 10))))))
         (ann ()) (icit Expl) (span ((start 9) (stop 10))))))
      (ret_ty ())
      (body
       (Expr_block
        (decls
         ((Block_decl_val
           ((relevancy Relevant) (name ((name w) (span ((start 18) (stop 19)))))
            (ann ()) (is_abstract false)
            (rhs
             (Expr_block
              (decls
               ((Block_decl_val
                 ((relevancy Relevant)
                  (name ((name x) (span ((start 27) (stop 28))))) (ann ())
                  (is_abstract false)
                  (rhs (Expr_var ((name x) (span ((start 31) (stop 32))))))
                  (span ((start 25) (stop 32)))))))
              (ret (Expr_var ((name x) (span ((start 35) (stop 36))))))
              (span ((start 22) (stop 39)))))
            (span ((start 16) (stop 39)))))))
        (ret
         (Expr_block (decls ())
          (ret (Expr_var ((name w) (span ((start 44) (stop 45))))))
          (span ((start 42) (stop 47)))))
        (span ((start 13) (stop 49)))))
      (span ((start 1) (stop 49)))))
    |}]
;;

let%expect_test "base types" =
  check
    {|
{
  val x = ()
  val y : Bool = #t
  val z = #f
  val another = first.T#first
  ()
}
    |};
  [%expect
    {|
    ((Expr_block
      (decls
       ((Block_decl_val
         ((relevancy Relevant) (name ((name x) (span ((start 6) (stop 7)))))
          (ann ()) (is_abstract false)
          (rhs (Expr_literal (literal Unit) (span ((start 10) (stop 12)))))
          (span ((start 4) (stop 12)))))
        (Block_decl_val
         ((relevancy Relevant) (name ((name y) (span ((start 17) (stop 18)))))
          (ann ((Expr_core_ty (ty Bool) (span ((start 21) (stop 22))))))
          (is_abstract false)
          (rhs
           (Expr_literal (literal (Bool true)) (span ((start 25) (stop 26)))))
          (span ((start 15) (stop 26)))))
        (Block_decl_val
         ((relevancy Relevant) (name ((name z) (span ((start 31) (stop 32)))))
          (ann ()) (is_abstract false)
          (rhs
           (Expr_literal (literal (Bool false)) (span ((start 35) (stop 36)))))
          (span ((start 29) (stop 36)))))
        (Block_decl_val
         ((relevancy Relevant)
          (name ((name another) (span ((start 41) (stop 42))))) (ann ())
          (is_abstract false)
          (rhs
           (Expr_proj
            (strukt (Expr_var ((name first) (span ((start 45) (stop 46))))))
            (field T#first) (span ((start 45) (stop 48)))))
          (span ((start 39) (stop 48)))))))
      (ret (Expr_literal (literal Unit) (span ((start 51) (stop 53)))))
      (span ((start 1) (stop 55)))))
    |}]
;;

let%expect_test "modules" =
  check
    {|
struct {
  val first = {
    val x = 1234
    val y = 234
    ()
  }
  
  val second = 1324
}
    |};
  [%expect
    {|
    ((Expr_struct
      (decls
       ((Block_decl_val
         ((relevancy Relevant) (name ((name first) (span ((start 8) (stop 9)))))
          (ann ()) (is_abstract false)
          (rhs
           (Expr_block
            (decls
             ((Block_decl_val
               ((relevancy Relevant)
                (name ((name x) (span ((start 17) (stop 18))))) (ann ())
                (is_abstract false)
                (rhs
                 (Expr_literal (literal (Int 1234))
                  (span ((start 21) (stop 22)))))
                (span ((start 15) (stop 22)))))
              (Block_decl_val
               ((relevancy Relevant)
                (name ((name y) (span ((start 27) (stop 28))))) (ann ())
                (is_abstract false)
                (rhs
                 (Expr_literal (literal (Int 234)) (span ((start 31) (stop 32)))))
                (span ((start 25) (stop 32)))))))
            (ret (Expr_literal (literal Unit) (span ((start 35) (stop 37)))))
            (span ((start 12) (stop 40)))))
          (span ((start 6) (stop 40)))))
        (Block_decl_val
         ((relevancy Relevant)
          (name ((name second) (span ((start 47) (stop 48))))) (ann ())
          (is_abstract false)
          (rhs (Expr_literal (literal (Int 1324)) (span ((start 51) (stop 52)))))
          (span ((start 45) (stop 52)))))))
      (span ((start 1) (stop 54)))))
    |}]
;;

let%expect_test "sig" =
  check
    {|
sig {
  val x : Bool
  
  val y : Bool
}
    |};
  [%expect
    {|
    ((Expr_ty_struct
      (field_specs
       (((relevancy Relevant) (name ((name x) (span ((start 8) (stop 9)))))
         (ty ((Expr_core_ty (ty Bool) (span ((start 12) (stop 13)))))) (rhs ())
         (span ((start 6) (stop 13))))
        ((relevancy Relevant) (name ((name y) (span ((start 20) (stop 21)))))
         (ty ((Expr_core_ty (ty Bool) (span ((start 24) (stop 25)))))) (rhs ())
         (span ((start 18) (stop 25))))))
      (span ((start 1) (stop 27)))))
    |}]
;;

let%expect_test "function types" =
  check
    {|
(a : Bool) -> (x y z : Bool) -> Bool -> x
    |};
  [%expect
    {|
    ((Expr_ty_fun
      (param_tys
       (((relevancy Relevant) (names (((name a) (span ((start 2) (stop 3))))))
         (ty ((Expr_core_ty (ty Bool) (span ((start 6) (stop 7)))))) (icit Expl)
         (span ((start 2) (stop 7))))
        ((relevancy Relevant)
         (names
          (((name x) (span ((start 12) (stop 13))))
           ((name y) (span ((start 14) (stop 15))))
           ((name z) (span ((start 16) (stop 17))))))
         (ty ((Expr_core_ty (ty Bool) (span ((start 20) (stop 21))))))
         (icit Expl) (span ((start 12) (stop 21))))
        ((relevancy Relevant) (names ())
         (ty ((Expr_core_ty (ty Bool) (span ((start 25) (stop 26))))))
         (icit Expl) (span ((start 25) (stop 26))))))
      (body_ty (Expr_var ((name x) (span ((start 29) (stop 30))))))
      (span ((start 2) (stop 30)))))
    |}]
;;

let%expect_test "paren exprs" =
  check
    {|
    {
  val awe = fun x -> (x.y.z.w)
  ()
    }
    |};
  [%expect
    {|
    ((Expr_block
      (decls
       ((Block_decl_val
         ((relevancy Relevant) (name ((name awe) (span ((start 7) (stop 8)))))
          (ann ()) (is_abstract false)
          (rhs
           (Expr_fun
            (params
             (((relevancy Relevant)
               (names (((name x) (span ((start 13) (stop 14)))))) (ann ())
               (icit Expl) (span ((start 13) (stop 14))))))
            (ret_ty ())
            (body
             (Expr_paren
              (e
               (Expr_proj
                (strukt
                 (Expr_proj
                  (strukt
                   (Expr_proj
                    (strukt (Expr_var ((name x) (span ((start 18) (stop 19))))))
                    (field y) (span ((start 18) (stop 21)))))
                  (field z) (span ((start 18) (stop 23)))))
                (field w) (span ((start 18) (stop 25)))))
              (span ((start 18) (stop 25)))))
            (span ((start 11) (stop 25)))))
          (span ((start 5) (stop 25)))))))
      (ret (Expr_literal (literal Unit) (span ((start 29) (stop 31)))))
      (span ((start 2) (stop 34)))))
    |}]
;;

let%expect_test "function application" =
  check
    {|
      {
        val app = fun f x y z -> (f w z).x.y.w x (a b c) z
        ()
      }
    |};
  [%expect
    {|
    ((Expr_block
      (decls
       ((Block_decl_val
         ((relevancy Relevant) (name ((name app) (span ((start 7) (stop 8)))))
          (ann ()) (is_abstract false)
          (rhs
           (Expr_fun
            (params
             (((relevancy Relevant)
               (names (((name f) (span ((start 13) (stop 14)))))) (ann ())
               (icit Expl) (span ((start 13) (stop 14))))
              ((relevancy Relevant)
               (names (((name x) (span ((start 15) (stop 16)))))) (ann ())
               (icit Expl) (span ((start 15) (stop 16))))
              ((relevancy Relevant)
               (names (((name y) (span ((start 17) (stop 18)))))) (ann ())
               (icit Expl) (span ((start 17) (stop 18))))
              ((relevancy Relevant)
               (names (((name z) (span ((start 19) (stop 20)))))) (ann ())
               (icit Expl) (span ((start 19) (stop 20))))))
            (ret_ty ())
            (body
             (Expr_app
              (func
               (Expr_proj
                (strukt
                 (Expr_proj
                  (strukt
                   (Expr_proj
                    (strukt
                     (Expr_paren
                      (e
                       (Expr_app
                        (func
                         (Expr_var ((name f) (span ((start 24) (stop 25))))))
                        (args
                         (((arg
                            (Expr_var ((name w) (span ((start 26) (stop 27))))))
                           (relevancy Relevant) (icit Expl))
                          ((arg
                            (Expr_var ((name z) (span ((start 28) (stop 29))))))
                           (relevancy Relevant) (icit Expl))))
                        (span ((start 24) (stop 29)))))
                      (span ((start 24) (stop 29)))))
                    (field x) (span ((start 24) (stop 32)))))
                  (field y) (span ((start 24) (stop 34)))))
                (field w) (span ((start 24) (stop 36)))))
              (args
               (((arg (Expr_var ((name x) (span ((start 37) (stop 38))))))
                 (relevancy Relevant) (icit Expl))
                ((arg
                  (Expr_app
                   (func (Expr_var ((name a) (span ((start 40) (stop 41))))))
                   (args
                    (((arg (Expr_var ((name b) (span ((start 42) (stop 43))))))
                      (relevancy Relevant) (icit Expl))
                     ((arg (Expr_var ((name c) (span ((start 44) (stop 45))))))
                      (relevancy Relevant) (icit Expl))))
                   (span ((start 40) (stop 45)))))
                 (relevancy Relevant) (icit Expl))
                ((arg (Expr_var ((name z) (span ((start 47) (stop 48))))))
                 (relevancy Relevant) (icit Expl))))
              (span ((start 24) (stop 48)))))
            (span ((start 11) (stop 48)))))
          (span ((start 5) (stop 48)))))))
      (ret (Expr_literal (literal Unit) (span ((start 51) (stop 53)))))
      (span ((start 2) (stop 56)))))
    |}]
;;

let%expect_test "awefaewf" =
  check
    {|
{
  val testing = fun x -> {
    bind awef = (pack x)
    pack awef
  }
  testing
}
  |};
  [%expect
    {|
    ((Expr_block
      (decls
       ((Block_decl_val
         ((relevancy Relevant)
          (name ((name testing) (span ((start 6) (stop 7))))) (ann ())
          (is_abstract false)
          (rhs
           (Expr_fun
            (params
             (((relevancy Relevant)
               (names (((name x) (span ((start 12) (stop 13)))))) (ann ())
               (icit Expl) (span ((start 12) (stop 13))))))
            (ret_ty ())
            (body
             (Expr_block
              (decls
               ((Block_decl_bind
                 (name ((name awef) (span ((start 21) (stop 22)))))
                 (rhs
                  (Expr_paren
                   (e
                    (Expr_pack
                     (e (Expr_var ((name x) (span ((start 28) (stop 29))))))
                     (span ((start 26) (stop 29)))))
                   (span ((start 26) (stop 29)))))
                 (span ((start 19) (stop 29))))))
              (ret
               (Expr_pack
                (e (Expr_var ((name awef) (span ((start 35) (stop 36))))))
                (span ((start 33) (stop 36)))))
              (span ((start 16) (stop 39)))))
            (span ((start 10) (stop 39)))))
          (span ((start 4) (stop 39)))))))
      (ret (Expr_var ((name testing) (span ((start 42) (stop 43))))))
      (span ((start 1) (stop 45)))))
    |}]
;;

let%expect_test "error: empty input" =
  check {||};
  [%expect
    {|
    error[E0001]: Empty root
     --> <input>:1:1
      |
    1 |
      | ^
    |}]
;;

let%expect_test "error: fun missing arrow" =
  check {|fun x y z|};
  [%expect
    {|
    error[E0001]: Expected arrow
     --> <input>:1:10
      |
    1 | fun x y z
      |          ^
    |}]
;;

let%expect_test "error: fun missing body" =
  check {|fun x ->|};
  [%expect
    {|
    error[E0001]: Expected expression
     --> <input>:1:9
      |
    1 | fun x ->
      |         ^
    |}]
;;

let%expect_test "error: fun missing params" =
  check {|fun -> x|};
  [%expect
    {|
    error[E0001]: Expected a function parameter
     --> <input>:1:5
      |
    1 | fun -> x
      |     ^^
    |}]
;;

let%expect_test "error: struct missing brace" =
  check {|struct x|};
  [%expect
    {|
    error[E0001]: Expected {
     --> <input>:1:8
      |
    1 | struct x
      |        ^
    |}]
;;

let%expect_test "error: sig missing brace" =
  check {|sig x|};
  [%expect
    {|
    error[E0001]: Expected {
     --> <input>:1:5
      |
    1 | sig x
      |     ^
    |}]
;;

let%expect_test "error: empty block" =
  check {|{}|};
  [%expect
    {|
    error[E0001]: Empty block
     --> <input>:1:1
      |
    1 | {}
      | ^
    |}]
;;

let%expect_test "single-group block treats let as variable" =
  (* When a block has only one group, it's parsed as a return expression,
     so `let` is treated as a regular variable name, not a keyword *)
  check {|{ val x y }|};
  [%expect
    {|
    ((Expr_block (decls ())
      (ret
       (Expr_app (func (Expr_var ((name val) (span ((start 2) (stop 3))))))
        (args
         (((arg (Expr_var ((name x) (span ((start 4) (stop 5))))))
           (relevancy Relevant) (icit Expl))
          ((arg (Expr_var ((name y) (span ((start 6) (stop 7))))))
           (relevancy Relevant) (icit Expl))))
        (span ((start 2) (stop 7)))))
      (span ((start 0) (stop 9)))))
    |}]
;;

let%expect_test "error: let missing rhs" =
  check {|{ val x = }|};
  [%expect
    {|
    error[E0001]: Unconsumed tokens when parsing expression
     --> <input>:1:9
      |
    1 | { val x = }
      |         ^
    |}]
;;

let%expect_test "top-level application" =
  check {|x y z|};
  [%expect
    {|
    ((Expr_app (func (Expr_var ((name x) (span ((start 0) (stop 1))))))
      (args
       (((arg (Expr_var ((name y) (span ((start 2) (stop 3))))))
         (relevancy Relevant) (icit Expl))
        ((arg (Expr_var ((name z) (span ((start 4) (stop 5))))))
         (relevancy Relevant) (icit Expl))))
      (span ((start 0) (stop 5)))))
    |}]
;;

let%expect_test "error: struct missing decl prefix" =
  check {|struct { x }|};
  [%expect
    {|
    error[E0001]: Expected block declaration
     --> <input>:1:10
      |
    1 | struct { x }
      |          ^
    |}]
;;

let%expect_test "error: sig missing type" =
  check {|sig { val x }|};
  [%expect
    {|
    ((Expr_ty_struct
      (field_specs
       (((relevancy Relevant) (name ((name x) (span ((start 6) (stop 7)))))
         (ty ()) (rhs ()) (span ((start 4) (stop 7))))))
      (span ((start 0) (stop 9)))))
    |}]
;;

let%expect_test "single variable" =
  check {|x|};
  [%expect {| ((Expr_var ((name x) (span ((start 0) (stop 1)))))) |}]
;;

let%expect_test "nested application" =
  check {|(f x) y|};
  [%expect
    {|
    ((Expr_app
      (func
       (Expr_paren
        (e
         (Expr_app (func (Expr_var ((name f) (span ((start 1) (stop 2))))))
          (args
           (((arg (Expr_var ((name x) (span ((start 3) (stop 4))))))
             (relevancy Relevant) (icit Expl))))
          (span ((start 1) (stop 4)))))
        (span ((start 1) (stop 4)))))
      (args
       (((arg (Expr_var ((name y) (span ((start 6) (stop 7))))))
         (relevancy Relevant) (icit Expl))))
      (span ((start 1) (stop 7)))))
    |}]
;;

let%expect_test "chained projections" =
  check {|a.b.c.d|};
  [%expect
    {|
    ((Expr_proj
      (strukt
       (Expr_proj
        (strukt
         (Expr_proj (strukt (Expr_var ((name a) (span ((start 0) (stop 1))))))
          (field b) (span ((start 0) (stop 3)))))
        (field c) (span ((start 0) (stop 5)))))
      (field d) (span ((start 0) (stop 7)))))
    |}]
;;

let%expect_test "nested Fun" =
  check {|Bool -> Bool -> Bool|};
  [%expect
    {|
    ((Expr_ty_fun
      (param_tys
       (((relevancy Relevant) (names ())
         (ty ((Expr_core_ty (ty Bool) (span ((start 0) (stop 1)))))) (icit Expl)
         (span ((start 0) (stop 1))))
        ((relevancy Relevant) (names ())
         (ty ((Expr_core_ty (ty Bool) (span ((start 4) (stop 5)))))) (icit Expl)
         (span ((start 4) (stop 5))))))
      (body_ty (Expr_core_ty (ty Bool) (span ((start 8) (stop 9)))))
      (span ((start 0) (stop 9)))))
    |}]
;;

let%expect_test "fun with annotated return and body" =
  check {|(fun (x : Bool) -> x : Bool -> x)|};
  [%expect
    {|
    ((Expr_paren
      (e
       (Expr_ann
        (e
         (Expr_fun
          (params
           (((relevancy Relevant)
             (names (((name x) (span ((start 4) (stop 5))))))
             (ann ((Expr_core_ty (ty Bool) (span ((start 8) (stop 9))))))
             (icit Expl) (span ((start 4) (stop 9))))))
          (ret_ty ()) (body (Expr_var ((name x) (span ((start 13) (stop 14))))))
          (span ((start 1) (stop 14)))))
        (ty
         (Expr_ty_fun
          (param_tys
           (((relevancy Relevant) (names ())
             (ty ((Expr_core_ty (ty Bool) (span ((start 17) (stop 18))))))
             (icit Expl) (span ((start 17) (stop 18))))))
          (body_ty (Expr_var ((name x) (span ((start 21) (stop 22))))))
          (span ((start 17) (stop 22)))))
        (span ((start 1) (stop 22)))))
      (span ((start 1) (stop 22)))))
    |}]
;;

let%expect_test "error: block without return expression" =
  (* The last group in a block is treated as the return expression.
     When it starts with `let`, parsing fails because `let` falls through
     to application and `=` is unconsumed. *)
  check
    {|
{
  val x = #t
  val y = #f
}
    |};
  [%expect
    {|
    error[E0001]: Unconsumed tokens when parsing expression
     --> <input>:4:9
      |
    4 |   val y = #f
      |         ^
    |}]
;;

let%expect_test "let with annotation" =
  check
    {|
{
  val x : Bool = #t
  x
}
    |};
  [%expect
    {|
    ((Expr_block
      (decls
       ((Block_decl_val
         ((relevancy Relevant) (name ((name x) (span ((start 6) (stop 7)))))
          (ann ((Expr_core_ty (ty Bool) (span ((start 10) (stop 11))))))
          (is_abstract false)
          (rhs
           (Expr_literal (literal (Bool true)) (span ((start 14) (stop 15)))))
          (span ((start 4) (stop 15)))))))
      (ret (Expr_var ((name x) (span ((start 18) (stop 19))))))
      (span ((start 1) (stop 21)))))
    |}]
;;

let%expect_test "application of block" =
  check
    {|
f { val x = #t
    x }
    |};
  [%expect
    {|
    ((Expr_app (func (Expr_var ((name f) (span ((start 1) (stop 2))))))
      (args
       (((arg
          (Expr_block
           (decls
            ((Block_decl_val
              ((relevancy Relevant) (name ((name x) (span ((start 7) (stop 8)))))
               (ann ()) (is_abstract false)
               (rhs
                (Expr_literal (literal (Bool true))
                 (span ((start 11) (stop 12)))))
               (span ((start 5) (stop 12)))))))
           (ret (Expr_var ((name x) (span ((start 15) (stop 16))))))
           (span ((start 3) (stop 18)))))
         (relevancy Relevant) (icit Expl))))
      (span ((start 1) (stop 18)))))
    |}]
;;

let%expect_test "pack in application" =
  check {|(pack x) y|};
  [%expect
    {|
    ((Expr_app
      (func
       (Expr_paren
        (e
         (Expr_pack (e (Expr_var ((name x) (span ((start 3) (stop 4))))))
          (span ((start 1) (stop 4)))))
        (span ((start 1) (stop 4)))))
      (args
       (((arg (Expr_var ((name y) (span ((start 6) (stop 7))))))
         (relevancy Relevant) (icit Expl))))
      (span ((start 1) (stop 7)))))
    |}]
;;

let%expect_test "pack low precedence" =
  check
    {|
    pack x y z
    |};
  [%expect
    {|
    error[E0001]: Unconsumed tokens when parsing expression
     --> <input>:2:12
      |
    2 |     pack x y z
      |            ^
    |}]
;;

let%expect_test "impl" =
  check
    {|
a b c [a b c]
    |};
  [%expect
    {|
    ((Expr_app (func (Expr_var ((name a) (span ((start 1) (stop 2))))))
      (args
       (((arg (Expr_var ((name b) (span ((start 3) (stop 4))))))
         (relevancy Relevant) (icit Expl))
        ((arg (Expr_var ((name c) (span ((start 5) (stop 6))))))
         (relevancy Relevant) (icit Expl))
        ((arg
          (Expr_app (func (Expr_var ((name a) (span ((start 8) (stop 9))))))
           (args
            (((arg (Expr_var ((name b) (span ((start 10) (stop 11))))))
              (relevancy Relevant) (icit Expl))
             ((arg (Expr_var ((name c) (span ((start 12) (stop 13))))))
              (relevancy Relevant) (icit Expl))))
           (span ((start 8) (stop 13)))))
         (relevancy Irrelevant) (icit Impl))))
      (span ((start 1) (stop 13)))))
    |}]
;;

let%expect_test "error: block expr decl missing do" =
  check
    {|
{
  f
  f
  f "aewfaewf"
}
    |};
  [%expect
    {|
    error[E0001]: Expected block declaration
     --> <input>:3:3
      |
    3 |   f
      |   ^
    |}]
;;

let%expect_test "recursive blocks" =
  check
    {|
rec {
  val first = fun x -> second x
  
  val second : Int -> Int = fun x -> first x
}
    |};
  [%expect
    {|
    ((Expr_rec
      (decls
       (((relevancy Relevant) (name ((name first) (span ((start 8) (stop 9)))))
         (ann ()) (is_abstract false)
         (rhs
          (Expr_fun
           (params
            (((relevancy Relevant)
              (names (((name x) (span ((start 14) (stop 15)))))) (ann ())
              (icit Expl) (span ((start 14) (stop 15))))))
           (ret_ty ())
           (body
            (Expr_app
             (func (Expr_var ((name second) (span ((start 18) (stop 19))))))
             (args
              (((arg (Expr_var ((name x) (span ((start 20) (stop 21))))))
                (relevancy Relevant) (icit Expl))))
             (span ((start 18) (stop 21)))))
           (span ((start 12) (stop 21)))))
         (span ((start 6) (stop 21))))
        ((relevancy Relevant)
         (name ((name second) (span ((start 28) (stop 29)))))
         (ann
          ((Expr_ty_fun
            (param_tys
             (((relevancy Relevant) (names ())
               (ty ((Expr_core_ty (ty Int) (span ((start 32) (stop 33))))))
               (icit Expl) (span ((start 32) (stop 33))))))
            (body_ty (Expr_core_ty (ty Int) (span ((start 36) (stop 37)))))
            (span ((start 32) (stop 37))))))
         (is_abstract false)
         (rhs
          (Expr_fun
           (params
            (((relevancy Relevant)
              (names (((name x) (span ((start 42) (stop 43)))))) (ann ())
              (icit Expl) (span ((start 42) (stop 43))))))
           (ret_ty ())
           (body
            (Expr_app
             (func (Expr_var ((name first) (span ((start 46) (stop 47))))))
             (args
              (((arg (Expr_var ((name x) (span ((start 48) (stop 49))))))
                (relevancy Relevant) (icit Expl))))
             (span ((start 46) (stop 49)))))
           (span ((start 40) (stop 49)))))
         (span ((start 26) (stop 49))))))
      (span ((start 1) (stop 51)))))
    |}]
;;

let%expect_test "record patching" =
  check
    {|
sig { val T : Type; val U : Type -> Type } where { T = Int; U = List }
|};
  [%expect
    {|
    ((Expr_where
      (e
       (Expr_ty_struct
        (field_specs
         (((relevancy Relevant) (name ((name T) (span ((start 7) (stop 8)))))
           (ty ((Expr_universe (size Type) (span ((start 11) (stop 12))))))
           (rhs ()) (span ((start 5) (stop 12))))
          ((relevancy Relevant) (name ((name U) (span ((start 16) (stop 17)))))
           (ty
            ((Expr_ty_fun
              (param_tys
               (((relevancy Relevant) (names ())
                 (ty ((Expr_universe (size Type) (span ((start 20) (stop 21))))))
                 (icit Expl) (span ((start 20) (stop 21))))))
              (body_ty (Expr_universe (size Type) (span ((start 24) (stop 25)))))
              (span ((start 20) (stop 25))))))
           (rhs ()) (span ((start 14) (stop 25))))))
        (span ((start 1) (stop 27)))))
      (patches
       (((path (T)) (rhs (Expr_core_ty (ty Int) (span ((start 36) (stop 37)))))
         (span ((start 32) (stop 37))))
        ((path (U)) (rhs (Expr_var ((name List) (span ((start 43) (stop 44))))))
         (span ((start 39) (stop 44))))))
      (span ((start 1) (stop 46)))))
    |}];
  check
    {|
Some_signature where { T.First.Second = Int; U = List }
      |};
  [%expect
    {|
    ((Expr_where
      (e (Expr_var ((name Some_signature) (span ((start 1) (stop 2))))))
      (patches
       (((path (T First Second))
         (rhs (Expr_core_ty (ty Int) (span ((start 15) (stop 16)))))
         (span ((start 7) (stop 16))))
        ((path (U)) (rhs (Expr_var ((name List) (span ((start 22) (stop 23))))))
         (span ((start 18) (stop 23))))))
      (span ((start 1) (stop 25)))))
    |}];
  check
    {|
      Some_signature where { T.First.Second = Int; u = List} -> Int
      |};
  check
    {|
      f a b c where { T = Int } -> f a b c where { T = Int } -> Int
      |};
  [%expect
    {|
    ((Expr_ty_fun
      (param_tys
       (((relevancy Relevant) (names ())
         (ty
          ((Expr_where
            (e (Expr_var ((name Some_signature) (span ((start 2) (stop 3))))))
            (patches
             (((path (T First Second))
               (rhs (Expr_core_ty (ty Int) (span ((start 16) (stop 17)))))
               (span ((start 8) (stop 17))))
              ((path (u))
               (rhs (Expr_var ((name List) (span ((start 23) (stop 24))))))
               (span ((start 19) (stop 24))))))
            (span ((start 2) (stop 25))))))
         (icit Expl) (span ((start 2) (stop 25))))))
      (body_ty (Expr_core_ty (ty Int) (span ((start 28) (stop 29)))))
      (span ((start 2) (stop 29)))))
    ((Expr_ty_fun
      (param_tys
       (((relevancy Relevant) (names ())
         (ty
          ((Expr_where
            (e
             (Expr_app (func (Expr_var ((name f) (span ((start 2) (stop 3))))))
              (args
               (((arg (Expr_var ((name a) (span ((start 4) (stop 5))))))
                 (relevancy Relevant) (icit Expl))
                ((arg (Expr_var ((name b) (span ((start 6) (stop 7))))))
                 (relevancy Relevant) (icit Expl))
                ((arg (Expr_var ((name c) (span ((start 8) (stop 9))))))
                 (relevancy Relevant) (icit Expl))))
              (span ((start 2) (stop 9)))))
            (patches
             (((path (T))
               (rhs (Expr_core_ty (ty Int) (span ((start 18) (stop 19)))))
               (span ((start 14) (stop 19))))))
            (span ((start 2) (stop 21))))))
         (icit Expl) (span ((start 2) (stop 21))))
        ((relevancy Relevant) (names ())
         (ty
          ((Expr_where
            (e
             (Expr_app (func (Expr_var ((name f) (span ((start 24) (stop 25))))))
              (args
               (((arg (Expr_var ((name a) (span ((start 26) (stop 27))))))
                 (relevancy Relevant) (icit Expl))
                ((arg (Expr_var ((name b) (span ((start 28) (stop 29))))))
                 (relevancy Relevant) (icit Expl))
                ((arg (Expr_var ((name c) (span ((start 30) (stop 31))))))
                 (relevancy Relevant) (icit Expl))))
              (span ((start 24) (stop 31)))))
            (patches
             (((path (T))
               (rhs (Expr_core_ty (ty Int) (span ((start 40) (stop 41)))))
               (span ((start 36) (stop 41))))))
            (span ((start 24) (stop 43))))))
         (icit Expl) (span ((start 24) (stop 43))))))
      (body_ty (Expr_core_ty (ty Int) (span ((start 46) (stop 47)))))
      (span ((start 2) (stop 47)))))
    |}];
  check
    {|
sig { val first : Type; val second : Type } where { T = Int; }
    |};
  [%expect
    {|
    ((Expr_where
      (e
       (Expr_ty_struct
        (field_specs
         (((relevancy Relevant) (name ((name first) (span ((start 7) (stop 8)))))
           (ty ((Expr_universe (size Type) (span ((start 11) (stop 12))))))
           (rhs ()) (span ((start 5) (stop 12))))
          ((relevancy Relevant)
           (name ((name second) (span ((start 16) (stop 17)))))
           (ty ((Expr_universe (size Type) (span ((start 20) (stop 21))))))
           (rhs ()) (span ((start 14) (stop 21))))))
        (span ((start 1) (stop 23)))))
      (patches
       (((path (T)) (rhs (Expr_core_ty (ty Int) (span ((start 32) (stop 33)))))
         (span ((start 28) (stop 33))))))
      (span ((start 1) (stop 36)))))
    |}]
;;

let%expect_test "block do" =
  check
    {|
{
  val first = 1234
  do some_call a b c
  do another 1234
  first
}
    |};
  [%expect
    {|
    ((Expr_block
      (decls
       ((Block_decl_val
         ((relevancy Relevant) (name ((name first) (span ((start 6) (stop 7)))))
          (ann ()) (is_abstract false)
          (rhs (Expr_literal (literal (Int 1234)) (span ((start 10) (stop 11)))))
          (span ((start 4) (stop 11)))))
        (Block_decl_do
         (e
          (Expr_app
           (func (Expr_var ((name some_call) (span ((start 16) (stop 17))))))
           (args
            (((arg (Expr_var ((name a) (span ((start 18) (stop 19))))))
              (relevancy Relevant) (icit Expl))
             ((arg (Expr_var ((name b) (span ((start 20) (stop 21))))))
              (relevancy Relevant) (icit Expl))
             ((arg (Expr_var ((name c) (span ((start 22) (stop 23))))))
              (relevancy Relevant) (icit Expl))))
           (span ((start 16) (stop 23)))))
         (span ((start 14) (stop 23))))
        (Block_decl_do
         (e
          (Expr_app
           (func (Expr_var ((name another) (span ((start 28) (stop 29))))))
           (args
            (((arg
               (Expr_literal (literal (Int 1234)) (span ((start 30) (stop 31)))))
              (relevancy Relevant) (icit Expl))))
           (span ((start 28) (stop 31)))))
         (span ((start 26) (stop 31))))))
      (ret (Expr_var ((name first) (span ((start 34) (stop 35))))))
      (span ((start 1) (stop 37)))))
    |}]
;;

let%expect_test "abstract val declaration" =
  check
    {|
  {
    abstract val first = 1234124
    abstract val another = 1234
    another
  }
    |};
  [%expect
    {|
    ((Expr_block
      (decls
       ((Block_decl_val
         ((relevancy Relevant) (name ((name first) (span ((start 9) (stop 10)))))
          (ann ()) (is_abstract true)
          (rhs
           (Expr_literal (literal (Int 1234124)) (span ((start 13) (stop 14)))))
          (span ((start 5) (stop 14)))))
        (Block_decl_val
         ((relevancy Relevant)
          (name ((name another) (span ((start 21) (stop 22))))) (ann ())
          (is_abstract true)
          (rhs (Expr_literal (literal (Int 1234)) (span ((start 25) (stop 26)))))
          (span ((start 17) (stop 26)))))))
      (ret (Expr_var ((name another) (span ((start 29) (stop 30))))))
      (span ((start 2) (stop 33)))))
    |}]
;;

let%expect_test "transparent signatures" =
  check
    {|
sig {
  val first : Type
  val second : Type = Int
  val third : Type = Bool
}
    |};
  [%expect
    {|
    ((Expr_ty_struct
      (field_specs
       (((relevancy Relevant) (name ((name first) (span ((start 8) (stop 9)))))
         (ty ((Expr_universe (size Type) (span ((start 12) (stop 13))))))
         (rhs ()) (span ((start 6) (stop 13))))
        ((relevancy Relevant)
         (name ((name second) (span ((start 18) (stop 19)))))
         (ty ((Expr_universe (size Type) (span ((start 22) (stop 23))))))
         (rhs ((Expr_core_ty (ty Int) (span ((start 26) (stop 27))))))
         (span ((start 16) (stop 27))))
        ((relevancy Relevant) (name ((name third) (span ((start 32) (stop 33)))))
         (ty ((Expr_universe (size Type) (span ((start 36) (stop 37))))))
         (rhs ((Expr_core_ty (ty Bool) (span ((start 40) (stop 41))))))
         (span ((start 30) (stop 41))))))
      (span ((start 1) (stop 43)))))
    |}]
;;

let%expect_test "irrelevancy" =
  check
    {|
sig {
  val f : (type A B : Type) -> A -> B -> B
  val second : (type A B : Type) -> type A -> B -> B
  type third a b c = Int
  type first : Type = Int
  type second : Type -> Type
}
    |};
  check {|
struct {
  val first = fun (type A : Type) (type B : Type) (A : Type) -> A
  type some_type = first (type Int)
}
    |};
  [%expect {|
    ((Expr_ty_struct
      (field_specs
       (((relevancy Relevant) (name ((name f) (span ((start 8) (stop 9)))))
         (ty
          ((Expr_ty_fun
            (param_tys
             (((relevancy Irrelevant)
               (names
                (((name A) (span ((start 15) (stop 16))))
                 ((name B) (span ((start 17) (stop 18))))))
               (ty ((Expr_universe (size Type) (span ((start 21) (stop 22))))))
               (icit Expl) (span ((start 13) (stop 22))))
              ((relevancy Relevant) (names ())
               (ty ((Expr_var ((name A) (span ((start 26) (stop 27)))))))
               (icit Expl) (span ((start 26) (stop 27))))
              ((relevancy Relevant) (names ())
               (ty ((Expr_var ((name B) (span ((start 30) (stop 31)))))))
               (icit Expl) (span ((start 30) (stop 31))))))
            (body_ty (Expr_var ((name B) (span ((start 34) (stop 35))))))
            (span ((start 13) (stop 35))))))
         (rhs ()) (span ((start 6) (stop 35))))
        ((relevancy Relevant)
         (name ((name second) (span ((start 40) (stop 41)))))
         (ty
          ((Expr_ty_fun
            (param_tys
             (((relevancy Irrelevant)
               (names
                (((name A) (span ((start 47) (stop 48))))
                 ((name B) (span ((start 49) (stop 50))))))
               (ty ((Expr_universe (size Type) (span ((start 53) (stop 54))))))
               (icit Expl) (span ((start 45) (stop 54))))
              ((relevancy Irrelevant) (names ())
               (ty ((Expr_var ((name A) (span ((start 60) (stop 61)))))))
               (icit Expl) (span ((start 60) (stop 61))))
              ((relevancy Relevant) (names ())
               (ty ((Expr_var ((name B) (span ((start 64) (stop 65)))))))
               (icit Expl) (span ((start 64) (stop 65))))))
            (body_ty (Expr_var ((name B) (span ((start 68) (stop 69))))))
            (span ((start 45) (stop 69))))))
         (rhs ()) (span ((start 38) (stop 69))))
        ((relevancy Irrelevant)
         (name ((name third) (span ((start 74) (stop 75)))))
         (ty
          ((Expr_app (func (Expr_var ((name a) (span ((start 76) (stop 77))))))
            (args
             (((arg (Expr_var ((name b) (span ((start 78) (stop 79))))))
               (relevancy Relevant) (icit Expl))
              ((arg (Expr_var ((name c) (span ((start 80) (stop 81))))))
               (relevancy Relevant) (icit Expl))))
            (span ((start 76) (stop 81))))))
         (rhs ((Expr_core_ty (ty Int) (span ((start 84) (stop 85))))))
         (span ((start 72) (stop 85))))
        ((relevancy Irrelevant)
         (name ((name first) (span ((start 90) (stop 91)))))
         (ty ((Expr_universe (size Type) (span ((start 94) (stop 95))))))
         (rhs ((Expr_core_ty (ty Int) (span ((start 98) (stop 99))))))
         (span ((start 88) (stop 99))))
        ((relevancy Irrelevant)
         (name ((name second) (span ((start 104) (stop 105)))))
         (ty
          ((Expr_ty_fun
            (param_tys
             (((relevancy Relevant) (names ())
               (ty ((Expr_universe (size Type) (span ((start 108) (stop 109))))))
               (icit Expl) (span ((start 108) (stop 109))))))
            (body_ty (Expr_universe (size Type) (span ((start 112) (stop 113)))))
            (span ((start 108) (stop 113))))))
         (rhs ()) (span ((start 102) (stop 113))))))
      (span ((start 1) (stop 115)))))
    ((Expr_struct
      (decls
       ((Block_decl_val
         ((relevancy Relevant) (name ((name first) (span ((start 8) (stop 9)))))
          (ann ()) (is_abstract false)
          (rhs
           (Expr_fun
            (params
             (((relevancy Irrelevant)
               (names (((name A) (span ((start 17) (stop 18))))))
               (ann ((Expr_universe (size Type) (span ((start 21) (stop 22))))))
               (icit Expl) (span ((start 17) (stop 22))))
              ((relevancy Irrelevant)
               (names (((name B) (span ((start 27) (stop 28))))))
               (ann ((Expr_universe (size Type) (span ((start 31) (stop 32))))))
               (icit Expl) (span ((start 27) (stop 32))))
              ((relevancy Relevant)
               (names (((name A) (span ((start 35) (stop 36))))))
               (ann ((Expr_universe (size Type) (span ((start 39) (stop 40))))))
               (icit Expl) (span ((start 35) (stop 40))))))
            (ret_ty ())
            (body (Expr_var ((name A) (span ((start 44) (stop 45))))))
            (span ((start 12) (stop 45)))))
          (span ((start 6) (stop 45)))))
        (Block_decl_val
         ((relevancy Irrelevant)
          (name ((name some_type) (span ((start 50) (stop 51))))) (ann ())
          (is_abstract false)
          (rhs
           (Expr_app
            (func (Expr_var ((name first) (span ((start 54) (stop 55))))))
            (args
             (((arg (Expr_core_ty (ty Int) (span ((start 59) (stop 60)))))
               (relevancy Irrelevant) (icit Expl))))
            (span ((start 54) (stop 60)))))
          (span ((start 48) (stop 60)))))))
      (span ((start 1) (stop 63)))))
    |}];
  check {|
sig {
  val f : [type A B : Type] -> [C D E] -> A
  val another : Type = f [type A B] [C D]
}
    |};
  [%expect {|
    ((Expr_ty_struct
      (field_specs
       (((relevancy Relevant) (name ((name f) (span ((start 8) (stop 9)))))
         (ty
          ((Expr_ty_fun
            (param_tys
             (((relevancy Irrelevant)
               (names
                (((name A) (span ((start 15) (stop 16))))
                 ((name B) (span ((start 17) (stop 18))))))
               (ty ((Expr_universe (size Type) (span ((start 21) (stop 22))))))
               (icit Impl) (span ((start 13) (stop 22))))
              ((relevancy Irrelevant)
               (names
                (((name C) (span ((start 27) (stop 28))))
                 ((name D) (span ((start 29) (stop 30))))
                 ((name E) (span ((start 31) (stop 32))))))
               (ty ()) (icit Impl) (span ((start 27) (stop 32))))))
            (body_ty (Expr_var ((name A) (span ((start 36) (stop 37))))))
            (span ((start 13) (stop 37))))))
         (rhs ()) (span ((start 6) (stop 37))))
        ((relevancy Relevant)
         (name ((name another) (span ((start 42) (stop 43)))))
         (ty ((Expr_universe (size Type) (span ((start 46) (stop 47))))))
         (rhs
          ((Expr_app (func (Expr_var ((name f) (span ((start 50) (stop 51))))))
            (args
             (((arg
                (Expr_app
                 (func (Expr_var ((name type) (span ((start 53) (stop 54))))))
                 (args
                  (((arg (Expr_var ((name A) (span ((start 55) (stop 56))))))
                    (relevancy Relevant) (icit Expl))
                   ((arg (Expr_var ((name B) (span ((start 57) (stop 58))))))
                    (relevancy Relevant) (icit Expl))))
                 (span ((start 53) (stop 58)))))
               (relevancy Irrelevant) (icit Impl))
              ((arg
                (Expr_app
                 (func (Expr_var ((name C) (span ((start 61) (stop 62))))))
                 (args
                  (((arg (Expr_var ((name D) (span ((start 63) (stop 64))))))
                    (relevancy Relevant) (icit Expl))))
                 (span ((start 61) (stop 64)))))
               (relevancy Irrelevant) (icit Impl))))
            (span ((start 50) (stop 64))))))
         (span ((start 40) (stop 64))))))
      (span ((start 1) (stop 67)))))
    |}]
;;
