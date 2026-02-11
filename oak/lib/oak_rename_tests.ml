open Core
module Snippet = Utility.Diagnostic.Snippet
module Diagnostic = Oak_diagnostic

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
      else print_s [%sexp (result : Oak_syntax.expr)])
;;

(* let%test_module "rename" =
  (module struct
    let%expect_test "variable" =
      check {|fun (x : Bool) -> x|};
      [%expect
        {|
        (Expr_abs (var ((name x) (pos 3)))
         (param_ty ((Expr_core_ty (ty Bool) (span ((start 7) (stop 8))))))
         (body (Expr_var (var ((index 0))) (span ((start 12) (stop 13)))))
         (span ((start 0) (stop 13))))
        |}]
    ;;

    let%expect_test "multiple params" =
      check {|fun (x : Bool) (y : Bool) -> y|};
      [%expect
        {|
        (Expr_abs (var ((name x) (pos 3)))
         (param_ty ((Expr_core_ty (ty Bool) (span ((start 7) (stop 8))))))
         (body
          (Expr_abs (var ((name y) (pos 11)))
           (param_ty ((Expr_core_ty (ty Bool) (span ((start 15) (stop 16))))))
           (body (Expr_var (var ((index 0))) (span ((start 20) (stop 21)))))
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
      check {|Fun Bool -> Bool|};
      [%expect
        {|
        (Expr_ty_fun (var ((name _) (pos 0)))
         (param_ty (Expr_core_ty (ty Bool) (span ((start 2) (stop 3)))))
         (body_ty (Expr_core_ty (ty Bool) (span ((start 6) (stop 7)))))
         (span ((start 0) (stop 7))))
        |}]
    ;;

    let%expect_test "multi function type" =
      check
        {|
        Fun (x y z : Bool) (Bool Bool) Bool Bool Bool -> Bool
        |};
      [%expect
        {|
        (Expr_ty_fun (var ((name x) (pos 5)))
         (param_ty (Expr_core_ty (ty Bool) (span ((start 13) (stop 14)))))
         (body_ty
          (Expr_ty_fun (var ((name y) (pos 7)))
           (param_ty (Expr_core_ty (ty Bool) (span ((start 13) (stop 14)))))
           (body_ty
            (Expr_ty_fun (var ((name z) (pos 9)))
             (param_ty (Expr_core_ty (ty Bool) (span ((start 13) (stop 14)))))
             (body_ty
              (Expr_ty_fun (var ((name _) (pos 2)))
               (param_ty
                (Expr_app
                 (func (Expr_core_ty (ty Bool) (span ((start 17) (stop 18)))))
                 (arg (Expr_core_ty (ty Bool) (span ((start 19) (stop 20)))))
                 (span ((start 17) (stop 20)))))
               (body_ty
                (Expr_ty_fun (var ((name _) (pos 2)))
                 (param_ty (Expr_core_ty (ty Bool) (span ((start 22) (stop 23)))))
                 (body_ty
                  (Expr_ty_fun (var ((name _) (pos 2)))
                   (param_ty (Expr_core_ty (ty Bool) (span ((start 24) (stop 25)))))
                   (body_ty
                    (Expr_ty_fun (var ((name _) (pos 2)))
                     (param_ty
                      (Expr_core_ty (ty Bool) (span ((start 26) (stop 27)))))
                     (body_ty (Expr_core_ty (ty Bool) (span ((start 30) (stop 31)))))
                     (span ((start 2) (stop 31)))))
                   (span ((start 2) (stop 31)))))
                 (span ((start 2) (stop 31)))))
               (span ((start 2) (stop 31)))))
             (span ((start 2) (stop 31)))))
           (span ((start 2) (stop 31)))))
         (span ((start 2) (stop 31))))
        |}]
    ;;

    let%expect_test "function type with named param" =
      check {|Fun (x : Bool) -> Bool|};
      [%expect
        {|
        (Expr_ty_fun (var ((name x) (pos 3)))
         (param_ty (Expr_core_ty (ty Bool) (span ((start 7) (stop 8)))))
         (body_ty (Expr_core_ty (ty Bool) (span ((start 12) (stop 13)))))
         (span ((start 0) (stop 13))))
        |}]
    ;;

    let%expect_test "block with let" =
      check
        {|
fun (x : Bool) -> {
  let y : Bool = x
  y
}
        |};
      [%expect
        {|
        (Expr_abs (var ((name x) (pos 4)))
         (param_ty ((Expr_core_ty (ty Bool) (span ((start 8) (stop 9))))))
         (body
          (Expr_let (var ((name y) (pos 18)))
           (rhs
            (Expr_ann (e (Expr_var (var ((index 0))) (span ((start 26) (stop 27)))))
             (ty (Expr_core_ty (ty Bool) (span ((start 22) (stop 23)))))
             (span ((start 16) (stop 27)))))
           (body (Expr_var (var ((index 0))) (span ((start 30) (stop 31)))))
           (span ((start 13) (stop 33)))))
         (span ((start 1) (stop 33))))
        |}]
    ;;

    let%expect_test "bool literal" =
      check {|#t|};
      [%expect {| (Expr_bool (value true) (span ((start 0) (stop 1)))) |}]
    ;;

    let%expect_test "if expression" =
      check {|if #t #f #t|};
      [%expect
        {|
        error: Failed to find variable: if
         --> <input>:1:1
          |
        1 | if #t #f #t
          | ^^
        |}]
    ;;

    let%expect_test "application" =
      check {|fun (f : Bool) (x : Bool) -> f x|};
      [%expect
        {|
        (Expr_abs (var ((name f) (pos 3)))
         (param_ty ((Expr_core_ty (ty Bool) (span ((start 7) (stop 8))))))
         (body
          (Expr_abs (var ((name x) (pos 11)))
           (param_ty ((Expr_core_ty (ty Bool) (span ((start 15) (stop 16))))))
           (body
            (Expr_app
             (func (Expr_var (var ((index 1))) (span ((start 20) (stop 21)))))
             (arg (Expr_var (var ((index 0))) (span ((start 22) (stop 23)))))
             (span ((start 20) (stop 23)))))
           (span ((start 0) (stop 23)))))
         (span ((start 0) (stop 23))))
        |}]
    ;;

    let%expect_test "block with binds and lets" =
      check
        {|
fun (x : Bool) -> {
  let y : Bool = x
  bind z = pack y
  let w : Bool = z
  w
}
        |};
      [%expect
        {|
        (Expr_abs (var ((name x) (pos 4)))
         (param_ty ((Expr_core_ty (ty Bool) (span ((start 8) (stop 9))))))
         (body
          (Expr_let (var ((name y) (pos 18)))
           (rhs
            (Expr_ann (e (Expr_var (var ((index 0))) (span ((start 26) (stop 27)))))
             (ty (Expr_core_ty (ty Bool) (span ((start 22) (stop 23)))))
             (span ((start 16) (stop 27)))))
           (body
            (Expr_bind (var ((name z) (pos 32)))
             (rhs
              (Expr_pack
               (e (Expr_var (var ((index 0))) (span ((start 38) (stop 39)))))
               (span ((start 36) (stop 39)))))
             (body
              (Expr_let (var ((name w) (pos 44)))
               (rhs
                (Expr_ann
                 (e (Expr_var (var ((index 0))) (span ((start 52) (stop 53)))))
                 (ty (Expr_core_ty (ty Bool) (span ((start 48) (stop 49)))))
                 (span ((start 42) (stop 53)))))
               (body (Expr_var (var ((index 0))) (span ((start 56) (stop 57)))))
               (span ((start 13) (stop 59)))))
             (span ((start 13) (stop 59)))))
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
        mod {
          let x = Bool
          let y = x
          let f : Fun (= x) -> (= x) = fun x -> x
          let r = f Bool
        }
        |};
      [%expect
        {|
        (Expr_mod
         (decls
          (((var ((name x) (pos 9)))
            (e (Expr_core_ty (ty Bool) (span ((start 13) (stop 14)))))
            (span ((start 7) (stop 14))))
           ((var ((name y) (pos 19)))
            (e (Expr_var (var ((index 0))) (span ((start 23) (stop 24)))))
            (span ((start 17) (stop 24))))
           ((var ((name f) (pos 29)))
            (e
             (Expr_ann
              (e
               (Expr_abs (var ((name x) (pos 53))) (param_ty ())
                (body (Expr_var (var ((index 0))) (span ((start 57) (stop 58)))))
                (span ((start 51) (stop 58)))))
              (ty
               (Expr_ty_fun (var ((name _) (pos 33)))
                (param_ty
                 (Expr_ty_sing
                  (identity
                   (Expr_var (var ((index 1))) (span ((start 38) (stop 39)))))
                  (span ((start 35) (stop 40)))))
                (body_ty
                 (Expr_ty_sing
                  (identity
                   (Expr_var (var ((index 2))) (span ((start 46) (stop 47)))))
                  (span ((start 43) (stop 48)))))
                (span ((start 33) (stop 48)))))
              (span ((start 27) (stop 58)))))
            (span ((start 27) (stop 58))))
           ((var ((name r) (pos 63)))
            (e
             (Expr_app
              (func (Expr_var (var ((index 0))) (span ((start 67) (stop 68)))))
              (arg (Expr_core_ty (ty Bool) (span ((start 69) (stop 70)))))
              (span ((start 67) (stop 70)))))
            (span ((start 61) (stop 70))))))
         (span ((start 2) (stop 73))))
        |}]
    ;;

    let%expect_test "record dot" =
      check
        {|
        mod {
          let M1 : sig {
            let M : sig {
              let M : sig {
                let T : Type
              }
            }
          } = mod {
            let M = mod {
              let M = mod {
                let T = Bool
              }
            }
          }
          
          let M2 = M1.M.M.T
        }
        |};
      [%expect {|
        (Expr_mod
         (decls
          (((var ((name M1) (pos 9)))
            (e
             (Expr_ann
              (e
               (Expr_mod
                (decls
                 (((var ((name M) (pos 66)))
                   (e
                    (Expr_mod
                     (decls
                      (((var ((name M) (pos 77)))
                        (e
                         (Expr_mod
                          (decls
                           (((var ((name T) (pos 88)))
                             (e
                              (Expr_core_ty (ty Bool) (span ((start 92) (stop 93)))))
                             (span ((start 86) (stop 93))))))
                          (span ((start 81) (stop 96)))))
                        (span ((start 75) (stop 96))))))
                     (span ((start 70) (stop 99)))))
                   (span ((start 64) (stop 99))))))
                (span ((start 59) (stop 102)))))
              (ty
               (Expr_ty_mod
                (ty_decls
                 (((var ((name M) (pos 20)))
                   (ty
                    (Expr_ty_mod
                     (ty_decls
                      (((var ((name M) (pos 31)))
                        (ty
                         (Expr_ty_mod
                          (ty_decls
                           (((var ((name T) (pos 42)))
                             (ty
                              (Expr_universe (universe Type)
                               (span ((start 46) (stop 47)))))
                             (span ((start 40) (stop 47))))))
                          (span ((start 35) (stop 50)))))
                        (span ((start 29) (stop 50))))))
                     (span ((start 24) (stop 53)))))
                   (span ((start 18) (stop 53))))))
                (span ((start 13) (stop 56)))))
              (span ((start 7) (stop 102)))))
            (span ((start 7) (stop 102))))
           ((var ((name M2) (pos 109)))
            (e
             (Expr_proj
              (mod_e
               (Expr_proj
                (mod_e
                 (Expr_proj
                  (mod_e
                   (Expr_var (var ((index 0))) (span ((start 113) (stop 114)))))
                  (field M) (span ((start 113) (stop 116)))))
                (field M) (span ((start 113) (stop 118)))))
              (field T) (span ((start 113) (stop 120)))))
            (span ((start 107) (stop 120))))))
         (span ((start 2) (stop 123))))
        |}]
    ;;
  end)
;; *)
