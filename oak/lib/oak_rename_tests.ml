open Core
module Snippet = Utility.Diagnostic.Snippet

let check s =
  let file = "<input>" in
  let tts, parse_diagnostics, expr = Oak_parse.parse ~file s in
  let files = String.Map.of_alist_exn [ file, Snippet.File.create s ] in
  if not (List.is_empty parse_diagnostics)
  then
    List.iter parse_diagnostics ~f:(fun diagnostic ->
      Oak_diagnostic.print ~color:false ~files diagnostic;
      print_string "\n\n")
  else (
    match expr with
    | None -> print_string "no expression\n"
    | Some expr ->
      let tokens = Shrubbery.Token_tree.Root.to_list tts |> Array.of_list in
      let offsets = Shrubbery.Token.calculate_offsets tokens in
      let diagnostics, result = Oak_rename.rename ~file ~offsets expr in
      if not (List.is_empty diagnostics)
      then
        List.iter diagnostics ~f:(fun diagnostic ->
          Oak_diagnostic.print ~color:false ~files diagnostic;
          print_string "\n\n")
      else print_s [%sexp (result : Oak_syntax.expr)])
;;

let%test_module "rename" =
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
              (Expr_ty_fun (var ((name _) (pos 0)))
               (param_ty
                (Expr_app
                 (func (Expr_core_ty (ty Bool) (span ((start 17) (stop 18)))))
                 (arg (Expr_core_ty (ty Bool) (span ((start 19) (stop 20)))))
                 (span ((start 17) (stop 20)))))
               (body_ty
                (Expr_ty_fun (var ((name _) (pos 0)))
                 (param_ty (Expr_core_ty (ty Bool) (span ((start 22) (stop 23)))))
                 (body_ty
                  (Expr_ty_fun (var ((name _) (pos 0)))
                   (param_ty (Expr_core_ty (ty Bool) (span ((start 24) (stop 25)))))
                   (body_ty
                    (Expr_ty_fun (var ((name _) (pos 0)))
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
  end)
;;
