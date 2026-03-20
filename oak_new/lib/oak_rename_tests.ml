open Core
module Snippet = Utility.Diagnostic.Snippet
module Diagnostic = Oak_diagnostic
module Abstract = Oak_abstract

let check s =
  let file = "<input>" in
  let source, parse_diagnostics, expr = Oak_parse.parse ~file s in
  let files = String.Map.of_alist_exn [ file, Snippet.File.create s ] in
  if not (List.is_empty parse_diagnostics)
  then Diagnostic.print_many ~files ~color:false parse_diagnostics
  else (
    match expr with
    | None -> print_string "no expression\n"
    | Some expr ->
      let diagnostics, result = Oak_rename.rename source expr in
      if not (List.is_empty diagnostics)
      then Diagnostic.print_many ~files ~color:false diagnostics
      else print_s [%sexp (result : Abstract.expr)])
;;

let%expect_test "variable" =
  check {|fun (x : Bool) -> x|};
  [%expect
    {|
    (Expr_fun (name ((name x) (span ((start 3) (stop 4)))))
     (param_ty ((Expr_core_ty (ty Bool) (span ((start 7) (stop 8))))))
     (param_modifiers ((icit Expl) (relevancy Relevant)))
     (body (Expr_var (index ((index 0))) (span ((start 12) (stop 13)))))
     (span ((start 0) (stop 13))))
    |}]
;;

let%expect_test "multiple params" =
  check {|fun (x : Bool) (y : Bool) -> y|};
  [%expect
    {|
    (Expr_fun (name ((name x) (span ((start 3) (stop 4)))))
     (param_ty ((Expr_core_ty (ty Bool) (span ((start 7) (stop 8))))))
     (param_modifiers ((icit Expl) (relevancy Relevant)))
     (body
      (Expr_fun (name ((name y) (span ((start 11) (stop 12)))))
       (param_ty ((Expr_core_ty (ty Bool) (span ((start 15) (stop 16))))))
       (param_modifiers ((icit Expl) (relevancy Relevant)))
       (body (Expr_var (index ((index 0))) (span ((start 20) (stop 21)))))
       (span ((start 0) (stop 21)))))
     (span ((start 0) (stop 21))))
    |}]
;;

let%expect_test "unbound variable" =
  check {|fun (x : Bool) -> y|};
  [%expect
    {|
    error: Failed to find variable: y
     --> <input>:1:19
      |
    1 | fun (x : Bool) -> y
      |                   ^
    |}]
;;

let%expect_test "function type" =
  check {|Bool -> Bool|};
  [%expect
    {|
    (Expr_ty_fun (name ((name _) (span ((start 0) (stop 5)))))
     (param_ty (Expr_core_ty (ty Bool) (span ((start 0) (stop 1)))))
     (param_modifiers ((icit Expl) (relevancy Relevant)))
     (body_ty (Expr_core_ty (ty Bool) (span ((start 4) (stop 5)))))
     (span ((start 0) (stop 5))))
    |}]
;;

let%expect_test "block with val" =
  check
    {|
fun (x : Bool) -> {
  val y : Bool = x
  y
}
        |};
  [%expect
    {|
    (Expr_fun (name ((name x) (span ((start 4) (stop 5)))))
     (param_ty ((Expr_core_ty (ty Bool) (span ((start 8) (stop 9))))))
     (param_modifiers ((icit Expl) (relevancy Relevant)))
     (body
      (Expr_let (name ((name y) (span ((start 18) (stop 19)))))
       (rhs
        (Expr_ann
         (e (Expr_var (index ((index 0))) (span ((start 26) (stop 27)))))
         (ty (Expr_core_ty (ty Bool) (span ((start 22) (stop 23)))))
         (span ((start 16) (stop 27)))))
       (relevancy Relevant) (is_abstract false)
       (body (Expr_var (index ((index 0))) (span ((start 30) (stop 31)))))
       (span ((start 13) (stop 33)))))
     (span ((start 1) (stop 33))))
    |}]
;;

let%expect_test "bool literal" =
  check {|true|};
  [%expect {| (Expr_literal (literal (Bool true)) (span ((start 0) (stop 1)))) |}]
;;

let%expect_test "application" =
  check {|fun (f : Bool) (x : Bool) -> f x|};
  [%expect
    {|
    (Expr_fun (name ((name f) (span ((start 3) (stop 4)))))
     (param_ty ((Expr_core_ty (ty Bool) (span ((start 7) (stop 8))))))
     (param_modifiers ((icit Expl) (relevancy Relevant)))
     (body
      (Expr_fun (name ((name x) (span ((start 11) (stop 12)))))
       (param_ty ((Expr_core_ty (ty Bool) (span ((start 15) (stop 16))))))
       (param_modifiers ((icit Expl) (relevancy Relevant)))
       (body
        (Expr_app
         (func (Expr_var (index ((index 1))) (span ((start 20) (stop 21)))))
         (arg (Expr_var (index ((index 0))) (span ((start 22) (stop 23)))))
         (param_modifiers ((icit Expl) (relevancy Relevant)))
         (span ((start 20) (stop 23)))))
       (span ((start 0) (stop 23)))))
     (span ((start 0) (stop 23))))
    |}]
;;

let%expect_test "block with binds and vals" =
  check
    {|
fun (x : Bool) -> {
  val y : Bool = x
  bind z = pack y
  val w : Bool = z
  w
}
        |};
  [%expect
    {|
    (Expr_fun (name ((name x) (span ((start 4) (stop 5)))))
     (param_ty ((Expr_core_ty (ty Bool) (span ((start 8) (stop 9))))))
     (param_modifiers ((icit Expl) (relevancy Relevant)))
     (body
      (Expr_let (name ((name y) (span ((start 18) (stop 19)))))
       (rhs
        (Expr_ann
         (e (Expr_var (index ((index 0))) (span ((start 26) (stop 27)))))
         (ty (Expr_core_ty (ty Bool) (span ((start 22) (stop 23)))))
         (span ((start 16) (stop 27)))))
       (relevancy Relevant) (is_abstract false)
       (body
        (Expr_bind (name ((name z) (span ((start 32) (stop 33)))))
         (rhs
          (Expr_pack
           (e (Expr_var (index ((index 0))) (span ((start 38) (stop 39)))))
           (span ((start 36) (stop 39)))))
         (body
          (Expr_let (name ((name w) (span ((start 44) (stop 45)))))
           (rhs
            (Expr_ann
             (e (Expr_var (index ((index 0))) (span ((start 52) (stop 53)))))
             (ty (Expr_core_ty (ty Bool) (span ((start 48) (stop 49)))))
             (span ((start 42) (stop 53)))))
           (relevancy Relevant) (is_abstract false)
           (body (Expr_var (index ((index 0))) (span ((start 56) (stop 57)))))
           (span ((start 13) (stop 59)))))
         (span ((start 30) (stop 39)))))
       (span ((start 13) (stop 59)))))
     (span ((start 1) (stop 59))))
    |}]
;;

let%expect_test "multiple errors" =
  check {|fun (x : Bool) -> a b|};
  [%expect
    {|
    error: Failed to find variable: a
     --> <input>:1:19
      |
    1 | fun (x : Bool) -> a b
      |                   ^

    error: Failed to find variable: b
     --> <input>:1:21
      |
    1 | fun (x : Bool) -> a b
      |                     ^
    |}]
;;

let%expect_test "underscore variable" =
  check {|_|};
  [%expect
    {|
    error: Cannot use underscore as a variable
     --> <input>:1:1
      |
    1 | _
      | ^
    |}]
;;

let%expect_test "smoke" =
  check
    {|
struct {
  val x = Bool
  val y = x
  val f : x -> x = fun z -> z
  val r = f Bool
}
        |};
  [%expect
    {|
    (Expr_struct
     (decls
      (((name ((name x) (span ((start 8) (stop 9))))) (relevancy Relevant)
        (e (Expr_core_ty (ty Bool) (span ((start 12) (stop 13)))))
        (is_abstract false) (span ((start 6) (stop 13))))
       ((name ((name y) (span ((start 18) (stop 19))))) (relevancy Relevant)
        (e (Expr_var (index ((index 0))) (span ((start 22) (stop 23)))))
        (is_abstract false) (span ((start 16) (stop 23))))
       ((name ((name f) (span ((start 28) (stop 29))))) (relevancy Relevant)
        (e
         (Expr_ann
          (e
           (Expr_fun (name ((name z) (span ((start 42) (stop 43)))))
            (param_ty ()) (param_modifiers ((icit Expl) (relevancy Relevant)))
            (body (Expr_var (index ((index 0))) (span ((start 46) (stop 47)))))
            (span ((start 40) (stop 47)))))
          (ty
           (Expr_ty_fun (name ((name _) (span ((start 32) (stop 37)))))
            (param_ty
             (Expr_var (index ((index 1))) (span ((start 32) (stop 33)))))
            (param_modifiers ((icit Expl) (relevancy Relevant)))
            (body_ty
             (Expr_var (index ((index 2))) (span ((start 36) (stop 37)))))
            (span ((start 32) (stop 37)))))
          (span ((start 26) (stop 47)))))
        (is_abstract false) (span ((start 26) (stop 47))))
       ((name ((name r) (span ((start 52) (stop 53))))) (relevancy Relevant)
        (e
         (Expr_app
          (func (Expr_var (index ((index 0))) (span ((start 56) (stop 57)))))
          (arg (Expr_core_ty (ty Bool) (span ((start 58) (stop 59)))))
          (param_modifiers ((icit Expl) (relevancy Relevant)))
          (span ((start 56) (stop 59)))))
        (is_abstract false) (span ((start 50) (stop 59))))))
     (is_dependent true) (span ((start 1) (stop 61))))
    |}]
;;

let%expect_test "nondependent struct does not bind later fields" =
  check {|struct (val x = Bool, val y = x)|};
  [%expect
    {|
    error: Failed to find variable: x
     --> <input>:1:31
      |
    1 | struct (val x = Bool, val y = x)
      |                               ^
    |}]
;;

let%expect_test "record dot" =
  check
    {|
struct {
  val M1 : sig {
    val M : sig {
      val M : sig {
        val T : Type
      }
    }
  } = struct {
    val M = struct {
      val M = struct {
        val T = Bool
      }
    }
  }

  val M2 = M1.M.M.T
}
        |};
  [%expect
    {|
    (Expr_struct
     (decls
      (((name ((name M1) (span ((start 8) (stop 9))))) (relevancy Relevant)
        (e
         (Expr_ann
          (e
           (Expr_struct
            (decls
             (((name ((name M) (span ((start 65) (stop 66)))))
               (relevancy Relevant)
               (e
                (Expr_struct
                 (decls
                  (((name ((name M) (span ((start 76) (stop 77)))))
                    (relevancy Relevant)
                    (e
                     (Expr_struct
                      (decls
                       (((name ((name T) (span ((start 87) (stop 88)))))
                         (relevancy Relevant)
                         (e
                          (Expr_core_ty (ty Bool) (span ((start 91) (stop 92)))))
                         (is_abstract false) (span ((start 85) (stop 92))))))
                      (is_dependent true) (span ((start 80) (stop 95)))))
                    (is_abstract false) (span ((start 74) (stop 95))))))
                 (is_dependent true) (span ((start 69) (stop 98)))))
               (is_abstract false) (span ((start 63) (stop 98))))))
            (is_dependent true) (span ((start 58) (stop 101)))))
          (ty
           (Expr_ty_struct
            (field_specs
             (((name ((name M) (span ((start 19) (stop 20)))))
               (relevancy Relevant)
               (ty
                ((Expr_ty_struct
                  (field_specs
                   (((name ((name M) (span ((start 30) (stop 31)))))
                     (relevancy Relevant)
                     (ty
                      ((Expr_ty_struct
                        (field_specs
                         (((name ((name T) (span ((start 41) (stop 42)))))
                           (relevancy Relevant)
                           (ty
                            ((Expr_universe (size Type)
                              (span ((start 45) (stop 46))))))
                           (rhs ()) (span ((start 39) (stop 46))))))
                        (span ((start 34) (stop 49))))))
                     (rhs ()) (span ((start 28) (stop 49))))))
                  (span ((start 23) (stop 52))))))
               (rhs ()) (span ((start 17) (stop 52))))))
            (span ((start 12) (stop 55)))))
          (span ((start 6) (stop 101)))))
        (is_abstract false) (span ((start 6) (stop 101))))
       ((name ((name M2) (span ((start 107) (stop 108))))) (relevancy Relevant)
        (e
         (Expr_proj
          (strukt
           (Expr_proj
            (strukt
             (Expr_proj
              (strukt
               (Expr_var (index ((index 0))) (span ((start 111) (stop 112)))))
              (field M) (span ((start 111) (stop 114)))))
            (field M) (span ((start 111) (stop 116)))))
          (field T) (span ((start 111) (stop 118)))))
        (is_abstract false) (span ((start 105) (stop 118))))))
     (is_dependent true) (span ((start 1) (stop 120))))
    |}]
;;

let%expect_test "rec" =
  check
    {|
rec {
  val first = second
  val second = first
}
    |};
  [%expect
    {|
    error: type annotations required for recursive block
     --> <input>:3:3
      |
    3 |   val first = second
      |   ^^^^^^^^^^^^^^^^^^

    error: type annotations required for recursive block
     --> <input>:4:3
      |
    4 |   val second = first
      |   ^^^^^^^^^^^^^^^^^^
    |}];
  check
    {|
rec {
  val first : Int = second
  val second : Int = first
}
    |};
  [%expect
    {|
    (Expr_rec
     (decls
      (((name ((name first) (span ((start 8) (stop 9)))))
        (ty (Expr_core_ty (ty Int) (span ((start 12) (stop 13)))))
        (e (Expr_var (index ((index 0))) (span ((start 16) (stop 17))))))
       ((name ((name second) (span ((start 22) (stop 23)))))
        (ty (Expr_core_ty (ty Int) (span ((start 26) (stop 27)))))
        (e (Expr_var (index ((index 1))) (span ((start 30) (stop 31))))))))
     (span ((start 1) (stop 33))))
    |}];
  check
    {|
rec {
  val first : Int = 1
  val first : Int = 2
  val second : Int = 3
  val second : Int = 4
}
      |};
  [%expect
    {|
    error: Duplicate variable in struct
     --> <input>:4:7
      |
    4 |   val first : Int = 2
      |       ^^^^^

    error: Duplicate variable in struct
     --> <input>:6:7
      |
    6 |   val second : Int = 4
      |       ^^^^^^
    |}];
  check
    {|
  rec {
    val first : first = 1
    val second : second = 2
  }
        |};
  [%expect
    {|
    error: Failed to find variable: first
     --> <input>:3:17
      |
    3 |     val first : first = 1
      |                 ^^^^^

    error: Failed to find variable: second
     --> <input>:4:18
      |
    4 |     val second : second = 2
      |                  ^^^^^^
    |}]
;;

let%expect_test "abstract val" =
  check
    {|
{
  abstract val first = 1
  val second = first
  second
}
    |};
  [%expect
    {|
    (Expr_let (name ((name first) (span ((start 8) (stop 9)))))
     (rhs (Expr_literal (literal (Int 1)) (span ((start 12) (stop 13)))))
     (relevancy Relevant) (is_abstract true)
     (body
      (Expr_let (name ((name second) (span ((start 18) (stop 19)))))
       (rhs (Expr_var (index ((index 0))) (span ((start 22) (stop 23)))))
       (relevancy Relevant) (is_abstract false)
       (body (Expr_var (index ((index 0))) (span ((start 26) (stop 27)))))
       (span ((start 1) (stop 29)))))
     (span ((start 1) (stop 29))))
    |}]
;;

let%expect_test "irrelevancy" =
  check
    {|
sig {
  val f : (type A : Type) -> A -> A
  type T : Type = Int
}
    |};
  check
    {|
struct {
  val f = fun (type A : Type) [B] -> A
  type T = f [Int]
}
    |};
  [%expect
    {|
    (Expr_ty_struct
     (field_specs
      (((name ((name f) (span ((start 8) (stop 9))))) (relevancy Relevant)
        (ty
         ((Expr_ty_fun (name ((name A) (span ((start 15) (stop 16)))))
           (param_ty (Expr_universe (size Type) (span ((start 19) (stop 20)))))
           (param_modifiers ((icit Expl) (relevancy Irrelevant)))
           (body_ty
            (Expr_ty_fun (name ((name _) (span ((start 13) (stop 29)))))
             (param_ty
              (Expr_var (index ((index 0))) (span ((start 24) (stop 25)))))
             (param_modifiers ((icit Expl) (relevancy Relevant)))
             (body_ty
              (Expr_var (index ((index 1))) (span ((start 28) (stop 29)))))
             (span ((start 13) (stop 29)))))
           (span ((start 13) (stop 29))))))
        (rhs ()) (span ((start 6) (stop 29))))
       ((name ((name T) (span ((start 34) (stop 35))))) (relevancy Irrelevant)
        (ty ((Expr_universe (size Type) (span ((start 38) (stop 39))))))
        (rhs ((Expr_core_ty (ty Int) (span ((start 42) (stop 43))))))
        (span ((start 32) (stop 43))))))
     (span ((start 1) (stop 45))))
    (Expr_struct
     (decls
      (((name ((name f) (span ((start 8) (stop 9))))) (relevancy Relevant)
        (e
         (Expr_fun (name ((name A) (span ((start 17) (stop 18)))))
          (param_ty ((Expr_universe (size Type) (span ((start 21) (stop 22))))))
          (param_modifiers ((icit Expl) (relevancy Irrelevant)))
          (body
           (Expr_fun (name ((name B) (span ((start 25) (stop 26)))))
            (param_ty ()) (param_modifiers ((icit Impl) (relevancy Irrelevant)))
            (body (Expr_var (index ((index 1))) (span ((start 30) (stop 31)))))
            (span ((start 12) (stop 31)))))
          (span ((start 12) (stop 31)))))
        (is_abstract false) (span ((start 6) (stop 31))))
       ((name ((name T) (span ((start 36) (stop 37))))) (relevancy Irrelevant)
        (e
         (Expr_app
          (func (Expr_var (index ((index 0))) (span ((start 40) (stop 41)))))
          (arg (Expr_core_ty (ty Int) (span ((start 43) (stop 44)))))
          (param_modifiers ((icit Impl) (relevancy Irrelevant)))
          (span ((start 40) (stop 44)))))
        (is_abstract false) (span ((start 34) (stop 44))))))
     (is_dependent true) (span ((start 1) (stop 47))))
    |}]
;;

let%expect_test "structure check duplicates" =
  check
    {|
struct {
  val T = 1234
  val T = 1324
}
    |};
  [%expect {|
    error: Duplicate variable in struct
     --> <input>:4:7
      |
    4 |   val T = 1324
      |       ^
    |}]
;;

let%expect_test "signature check duplicates" =
  check
    {|
sig {
  val T : Type
  val T : Type
}
    |};
  [%expect
    {|
    error: Duplicate variable in signature
     --> <input>:4:7
      |
    4 |   val T : Type
      |       ^
    |}]
;;
