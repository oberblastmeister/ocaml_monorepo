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

let bruh = (fun x -> x : int -> int)

let%expect_test "smoke" =
  check
    {|
(fun x -> x : f Int -> g Bool -> g Int)
    |};
  [%expect
    {|
    ((Expr_ann
      (e
       (Expr_abs
        (params
         (((vars (((name x) (span ((start 4) (stop 5)))))) (ann ())
           (span ((start 4) (stop 5))))))
        (ret_ty ()) (body (Expr_var ((name x) (span ((start 8) (stop 9))))))
        (span ((start 2) (stop 9)))))
      (ty
       (Expr_ty_fun
        (param_tys
         (((vars ())
           (ty
            (Expr_app (func (Expr_var ((name f) (span ((start 12) (stop 13))))))
             (args ((Expr_core_ty (ty Int) (span ((start 14) (stop 15))))))
             (span ((start 12) (stop 15)))))
           (span ((start 12) (stop 15))))
          ((vars ())
           (ty
            (Expr_app (func (Expr_var ((name g) (span ((start 18) (stop 19))))))
             (args ((Expr_core_ty (ty Bool) (span ((start 20) (stop 21))))))
             (span ((start 18) (stop 21)))))
           (span ((start 18) (stop 21))))))
        (body_ty
         (Expr_app (func (Expr_var ((name g) (span ((start 24) (stop 25))))))
          (args ((Expr_core_ty (ty Int) (span ((start 26) (stop 27))))))
          (span ((start 24) (stop 27)))))
        (span ((start 12) (stop 27)))))
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
    ((Expr_abs
      (params
       (((vars (((name x) (span ((start 3) (stop 4)))))) (ann ())
         (span ((start 3) (stop 4))))
        ((vars (((name y) (span ((start 5) (stop 6)))))) (ann ())
         (span ((start 5) (stop 6))))
        ((vars (((name z) (span ((start 7) (stop 8)))))) (ann ())
         (span ((start 7) (stop 8))))))
      (ret_ty ()) (body (Expr_var ((name x) (span ((start 11) (stop 12))))))
      (span ((start 1) (stop 12)))))
    |}];
  check
    {|
      fun (x w a : Bool) (y : Bool) z -> x
      |};
  [%expect
    {|
    ((Expr_abs
      (params
       (((vars
          (((name x) (span ((start 5) (stop 6))))
           ((name w) (span ((start 7) (stop 8))))
           ((name a) (span ((start 9) (stop 10))))))
         (ann ((Expr_core_ty (ty Bool) (span ((start 13) (stop 14))))))
         (span ((start 5) (stop 14))))
        ((vars (((name y) (span ((start 17) (stop 18))))))
         (ann ((Expr_core_ty (ty Bool) (span ((start 21) (stop 22))))))
         (span ((start 17) (stop 22))))
        ((vars (((name z) (span ((start 24) (stop 25)))))) (ann ())
         (span ((start 24) (stop 25))))))
      (ret_ty ()) (body (Expr_var ((name x) (span ((start 28) (stop 29))))))
      (span ((start 2) (stop 29)))))
    |}];
  check
    {|
(fun x y -> x : Bool -> x)
      |};
  [%expect
    {|
    ((Expr_ann
      (e
       (Expr_abs
        (params
         (((vars (((name x) (span ((start 4) (stop 5)))))) (ann ())
           (span ((start 4) (stop 5))))
          ((vars (((name y) (span ((start 6) (stop 7)))))) (ann ())
           (span ((start 6) (stop 7))))))
        (ret_ty ()) (body (Expr_var ((name x) (span ((start 10) (stop 11))))))
        (span ((start 2) (stop 11)))))
      (ty
       (Expr_ty_fun
        (param_tys
         (((vars ()) (ty (Expr_core_ty (ty Bool) (span ((start 14) (stop 15)))))
           (span ((start 14) (stop 15))))))
        (body_ty (Expr_var ((name x) (span ((start 18) (stop 19))))))
        (span ((start 14) (stop 19)))))
      (span ((start 2) (stop 19)))))
    |}]
;;

let%expect_test "blocks" =
  check
    {|
  fun x y -> {
    let z = x
    let w = y
    w
  }
    |};
  [%expect
    {|
    ((Expr_abs
      (params
       (((vars (((name x) (span ((start 4) (stop 5)))))) (ann ())
         (span ((start 4) (stop 5))))
        ((vars (((name y) (span ((start 6) (stop 7)))))) (ann ())
         (span ((start 6) (stop 7))))))
      (ret_ty ())
      (body
       (Expr_block
        (decls
         ((Block_decl_let (var ((name z) (span ((start 15) (stop 16)))))
           (ann ()) (rhs (Expr_var ((name x) (span ((start 19) (stop 20))))))
           (span ((start 13) (stop 20))))
          (Block_decl_let (var ((name w) (span ((start 25) (stop 26)))))
           (ann ()) (rhs (Expr_var ((name y) (span ((start 29) (stop 30))))))
           (span ((start 23) (stop 30))))))
        (ret (Expr_var ((name w) (span ((start 33) (stop 34))))))
        (span ((start 10) (stop 37)))))
      (span ((start 2) (stop 37)))))
    |}];
  check
    {|
fun x y z w -> {
  let w = {
    let x = x
    x
  }
  { w }
}
      |};
  [%expect
    {|
    ((Expr_abs
      (params
       (((vars (((name x) (span ((start 3) (stop 4)))))) (ann ())
         (span ((start 3) (stop 4))))
        ((vars (((name y) (span ((start 5) (stop 6)))))) (ann ())
         (span ((start 5) (stop 6))))
        ((vars (((name z) (span ((start 7) (stop 8)))))) (ann ())
         (span ((start 7) (stop 8))))
        ((vars (((name w) (span ((start 9) (stop 10)))))) (ann ())
         (span ((start 9) (stop 10))))))
      (ret_ty ())
      (body
       (Expr_block
        (decls
         ((Block_decl_let (var ((name w) (span ((start 18) (stop 19)))))
           (ann ())
           (rhs
            (Expr_block
             (decls
              ((Block_decl_let (var ((name x) (span ((start 27) (stop 28)))))
                (ann ())
                (rhs (Expr_var ((name x) (span ((start 31) (stop 32))))))
                (span ((start 25) (stop 32))))))
             (ret (Expr_var ((name x) (span ((start 35) (stop 36))))))
             (span ((start 22) (stop 39)))))
           (span ((start 16) (stop 39))))))
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
  let x = ()
  let y : Bool = #t
  let z = #f
  let another = first.T#first
  ()
}
    |};
  [%expect
    {|
    ((Expr_block
      (decls
       ((Block_decl_let (var ((name x) (span ((start 6) (stop 7))))) (ann ())
         (rhs (Expr_unit (span ((start 10) (stop 12)))))
         (span ((start 4) (stop 12))))
        (Block_decl_let (var ((name y) (span ((start 17) (stop 18)))))
         (ann ((Expr_core_ty (ty Bool) (span ((start 21) (stop 22))))))
         (rhs (Expr_bool (value true) (span ((start 25) (stop 26)))))
         (span ((start 15) (stop 26))))
        (Block_decl_let (var ((name z) (span ((start 31) (stop 32))))) (ann ())
         (rhs (Expr_bool (value false) (span ((start 35) (stop 36)))))
         (span ((start 29) (stop 36))))
        (Block_decl_let (var ((name another) (span ((start 41) (stop 42)))))
         (ann ())
         (rhs
          (Expr_proj
           (mod_e (Expr_var ((name first) (span ((start 45) (stop 46))))))
           (field T#first) (span ((start 45) (stop 48)))))
         (span ((start 39) (stop 48))))))
      (ret (Expr_unit (span ((start 51) (stop 53)))))
      (span ((start 1) (stop 55)))))
    |}]
;;

let%expect_test "modules" =
  check
    {|
mod {
  let first = {
    let x = 1234
    let y = 234
    ()
  }
  
  let second = 1324
}
    |};
  [%expect
    {|
    ((Expr_mod
      (decls
       (((var ((name first) (span ((start 8) (stop 9))))) (ann ())
         (e
          (Expr_block
           (decls
            ((Block_decl_let (var ((name x) (span ((start 17) (stop 18)))))
              (ann ())
              (rhs (Expr_number (value 1234) (span ((start 21) (stop 22)))))
              (span ((start 15) (stop 22))))
             (Block_decl_let (var ((name y) (span ((start 27) (stop 28)))))
              (ann ())
              (rhs (Expr_number (value 234) (span ((start 31) (stop 32)))))
              (span ((start 25) (stop 32))))))
           (ret (Expr_unit (span ((start 35) (stop 37)))))
           (span ((start 12) (stop 40)))))
         (span ((start 6) (stop 40))))
        ((var ((name second) (span ((start 47) (stop 48))))) (ann ())
         (e (Expr_number (value 1324) (span ((start 51) (stop 52)))))
         (span ((start 45) (stop 52))))))
      (span ((start 1) (stop 54)))))
    |}]
;;

let%expect_test "sig" =
  check
    {|
sig {
  let x : Bool
  
  let y : Bool
}
    |};
  [%expect
    {|
    ((Expr_ty_mod
      (ty_decls
       (((var ((name x) (span ((start 8) (stop 9)))))
         (ty (Expr_core_ty (ty Bool) (span ((start 12) (stop 13)))))
         (span ((start 6) (stop 13))))
        ((var ((name y) (span ((start 20) (stop 21)))))
         (ty (Expr_core_ty (ty Bool) (span ((start 24) (stop 25)))))
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
       (((vars (((name a) (span ((start 2) (stop 3))))))
         (ty (Expr_core_ty (ty Bool) (span ((start 6) (stop 7)))))
         (span ((start 2) (stop 7))))
        ((vars
          (((name x) (span ((start 12) (stop 13))))
           ((name y) (span ((start 14) (stop 15))))
           ((name z) (span ((start 16) (stop 17))))))
         (ty (Expr_core_ty (ty Bool) (span ((start 20) (stop 21)))))
         (span ((start 12) (stop 21))))
        ((vars ()) (ty (Expr_core_ty (ty Bool) (span ((start 25) (stop 26)))))
         (span ((start 25) (stop 26))))))
      (body_ty (Expr_var ((name x) (span ((start 29) (stop 30))))))
      (span ((start 2) (stop 30)))))
    |}]
;;

let%expect_test "paren exprs" =
  check
    {|
    {
  let awe = fun x -> (x.y.z.w)
  ()
    }
    |};
  [%expect
    {|
    ((Expr_block
      (decls
       ((Block_decl_let (var ((name awe) (span ((start 7) (stop 8))))) (ann ())
         (rhs
          (Expr_abs
           (params
            (((vars (((name x) (span ((start 13) (stop 14)))))) (ann ())
              (span ((start 13) (stop 14))))))
           (ret_ty ())
           (body
            (Expr_proj
             (mod_e
              (Expr_proj
               (mod_e
                (Expr_proj
                 (mod_e (Expr_var ((name x) (span ((start 18) (stop 19))))))
                 (field y) (span ((start 18) (stop 21)))))
               (field z) (span ((start 18) (stop 23)))))
             (field w) (span ((start 18) (stop 25)))))
           (span ((start 11) (stop 25)))))
         (span ((start 5) (stop 25))))))
      (ret (Expr_unit (span ((start 29) (stop 31)))))
      (span ((start 2) (stop 34)))))
    |}]
;;

let%expect_test "function application" =
  check
    {|
      {
        let app = fun f x y z -> (f w z).x.y.w x (a b c) z
        ()
      }
    |};
  [%expect
    {|
    ((Expr_block
      (decls
       ((Block_decl_let (var ((name app) (span ((start 7) (stop 8))))) (ann ())
         (rhs
          (Expr_abs
           (params
            (((vars (((name f) (span ((start 13) (stop 14)))))) (ann ())
              (span ((start 13) (stop 14))))
             ((vars (((name x) (span ((start 15) (stop 16)))))) (ann ())
              (span ((start 15) (stop 16))))
             ((vars (((name y) (span ((start 17) (stop 18)))))) (ann ())
              (span ((start 17) (stop 18))))
             ((vars (((name z) (span ((start 19) (stop 20)))))) (ann ())
              (span ((start 19) (stop 20))))))
           (ret_ty ())
           (body
            (Expr_app
             (func
              (Expr_proj
               (mod_e
                (Expr_proj
                 (mod_e
                  (Expr_proj
                   (mod_e
                    (Expr_app
                     (func (Expr_var ((name f) (span ((start 24) (stop 25))))))
                     (args
                      ((Expr_var ((name w) (span ((start 26) (stop 27)))))
                       (Expr_var ((name z) (span ((start 28) (stop 29)))))))
                     (span ((start 24) (stop 29)))))
                   (field x) (span ((start 24) (stop 32)))))
                 (field y) (span ((start 24) (stop 34)))))
               (field w) (span ((start 24) (stop 36)))))
             (args
              ((Expr_var ((name x) (span ((start 37) (stop 38)))))
               (Expr_app
                (func (Expr_var ((name a) (span ((start 40) (stop 41))))))
                (args
                 ((Expr_var ((name b) (span ((start 42) (stop 43)))))
                  (Expr_var ((name c) (span ((start 44) (stop 45)))))))
                (span ((start 40) (stop 45))))
               (Expr_var ((name z) (span ((start 47) (stop 48)))))))
             (span ((start 24) (stop 48)))))
           (span ((start 11) (stop 48)))))
         (span ((start 5) (stop 48))))))
      (ret (Expr_unit (span ((start 51) (stop 53)))))
      (span ((start 2) (stop 56)))))
    |}]
;;

let%expect_test "awefaewf" =
  check
    {|
{
  let testing = fun x -> {
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
       ((Block_decl_let (var ((name testing) (span ((start 6) (stop 7)))))
         (ann ())
         (rhs
          (Expr_abs
           (params
            (((vars (((name x) (span ((start 12) (stop 13)))))) (ann ())
              (span ((start 12) (stop 13))))))
           (ret_ty ())
           (body
            (Expr_block
             (decls
              ((Block_decl_bind (var ((name awef) (span ((start 21) (stop 22)))))
                (rhs
                 (Expr_pack
                  (e (Expr_var ((name x) (span ((start 28) (stop 29))))))
                  (span ((start 26) (stop 29)))))
                (span ((start 19) (stop 29))))))
             (ret
              (Expr_pack
               (e (Expr_var ((name awef) (span ((start 35) (stop 36))))))
               (span ((start 33) (stop 36)))))
             (span ((start 16) (stop 39)))))
           (span ((start 10) (stop 39)))))
         (span ((start 4) (stop 39))))))
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
     --> <input>:1:8
      |
    1 | fun -> x
      |        ^
    |}]
;;

let%expect_test "error: mod missing brace" =
  check {|mod x|};
  [%expect
    {|
    error[E0001]: Expected {
     --> <input>:1:6
      |
    1 | mod x
      |      ^
    |}]
;;

let%expect_test "error: sig missing brace" =
  check {|sig x|};
  [%expect
    {|
    error[E0001]: Expected {
     --> <input>:1:6
      |
    1 | sig x
      |      ^
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
  check {|{ let x y }|};
  [%expect
    {|
    ((Expr_block (decls ())
      (ret
       (Expr_app (func (Expr_var ((name let) (span ((start 2) (stop 3))))))
        (args
         ((Expr_var ((name x) (span ((start 4) (stop 5)))))
          (Expr_var ((name y) (span ((start 6) (stop 7)))))))
        (span ((start 2) (stop 7)))))
      (span ((start 0) (stop 9)))))
    |}]
;;

let%expect_test "error: let missing rhs" =
  check {|{ let x = }|};
  [%expect
    {|
    error[E0001]: Unconsumed tokens when parsing expression
     --> <input>:1:9
      |
    1 | { let x = }
      |         ^
    |}]
;;

let%expect_test "top-level application" =
  check {|x y z|};
  [%expect
    {|
    ((Expr_app (func (Expr_var ((name x) (span ((start 0) (stop 1))))))
      (args
       ((Expr_var ((name y) (span ((start 2) (stop 3)))))
        (Expr_var ((name z) (span ((start 4) (stop 5)))))))
      (span ((start 0) (stop 5)))))
    |}]
;;

let%expect_test "error: mod bad decl" =
  check {|mod { x }|};
  [%expect
    {|
    error[E0001]: Expected module declaration
     --> <input>:1:7
      |
    1 | mod { x }
      |       ^
    |}]
;;

let%expect_test "error: sig missing type" =
  check {|sig { let x }|};
  [%expect
    {|
    error[E0001]: Expected :
     --> <input>:1:12
      |
    1 | sig { let x }
      |            ^
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
       (Expr_app (func (Expr_var ((name f) (span ((start 1) (stop 2))))))
        (args ((Expr_var ((name x) (span ((start 3) (stop 4)))))))
        (span ((start 1) (stop 4)))))
      (args ((Expr_var ((name y) (span ((start 6) (stop 7)))))))
      (span ((start 1) (stop 7)))))
    |}]
;;

let%expect_test "chained projections" =
  check {|a.b.c.d|};
  [%expect
    {|
    ((Expr_proj
      (mod_e
       (Expr_proj
        (mod_e
         (Expr_proj (mod_e (Expr_var ((name a) (span ((start 0) (stop 1))))))
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
       (((vars ()) (ty (Expr_core_ty (ty Bool) (span ((start 0) (stop 1)))))
         (span ((start 0) (stop 1))))
        ((vars ()) (ty (Expr_core_ty (ty Bool) (span ((start 4) (stop 5)))))
         (span ((start 4) (stop 5))))))
      (body_ty (Expr_core_ty (ty Bool) (span ((start 8) (stop 9)))))
      (span ((start 0) (stop 9)))))
    |}]
;;

let%expect_test "fun with annotated return and body" =
  check {|(fun (x : Bool) -> x : Bool -> x)|};
  [%expect
    {|
    ((Expr_ann
      (e
       (Expr_abs
        (params
         (((vars (((name x) (span ((start 4) (stop 5))))))
           (ann ((Expr_core_ty (ty Bool) (span ((start 8) (stop 9))))))
           (span ((start 4) (stop 9))))))
        (ret_ty ()) (body (Expr_var ((name x) (span ((start 13) (stop 14))))))
        (span ((start 1) (stop 14)))))
      (ty
       (Expr_ty_fun
        (param_tys
         (((vars ()) (ty (Expr_core_ty (ty Bool) (span ((start 17) (stop 18)))))
           (span ((start 17) (stop 18))))))
        (body_ty (Expr_var ((name x) (span ((start 21) (stop 22))))))
        (span ((start 17) (stop 22)))))
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
  let x = #t
  let y = #f
}
    |};
  [%expect
    {|
    error[E0001]: Unconsumed tokens when parsing expression
     --> <input>:4:9
      |
    4 |   let y = #f
      |         ^
    |}]
;;

let%expect_test "let with annotation" =
  check
    {|
{
  let x : Bool = #t
  x
}
    |};
  [%expect
    {|
    ((Expr_block
      (decls
       ((Block_decl_let (var ((name x) (span ((start 6) (stop 7)))))
         (ann ((Expr_core_ty (ty Bool) (span ((start 10) (stop 11))))))
         (rhs (Expr_bool (value true) (span ((start 14) (stop 15)))))
         (span ((start 4) (stop 15))))))
      (ret (Expr_var ((name x) (span ((start 18) (stop 19))))))
      (span ((start 1) (stop 21)))))
    |}]
;;

let%expect_test "application of block" =
  check
    {|
f { let x = #t
    x }
    |};
  [%expect
    {|
    ((Expr_app (func (Expr_var ((name f) (span ((start 1) (stop 2))))))
      (args
       ((Expr_block
         (decls
          ((Block_decl_let (var ((name x) (span ((start 7) (stop 8))))) (ann ())
            (rhs (Expr_bool (value true) (span ((start 11) (stop 12)))))
            (span ((start 5) (stop 12))))))
         (ret (Expr_var ((name x) (span ((start 15) (stop 16))))))
         (span ((start 3) (stop 18))))))
      (span ((start 1) (stop 18)))))
    |}]
;;

let%expect_test "pack in application" =
  check {|(pack x) y|};
  [%expect
    {|
    ((Expr_app
      (func
       (Expr_pack (e (Expr_var ((name x) (span ((start 3) (stop 4))))))
        (span ((start 1) (stop 4)))))
      (args ((Expr_var ((name y) (span ((start 6) (stop 7)))))))
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
