open Prelude
module Infer = Staged_infer
module Evaluate = Staged_evaluate
module Syntax = Staged_syntax

module Syntax_helpers = struct
  module Expr = struct
    let app fn args = Syntax.Expr_app { fn; args; ann = None }

    let efun ?(stage = Syntax.Stage.Runtime) params body =
      Syntax.Expr_fun
        { params = List.map params ~f:(fun (var, ty) -> ({ var; ty } : Syntax.param))
        ; stage
        ; body
        ; ann = None
        }
    ;;

    let ecfun = efun ~stage:Comptime
    let add lhs rhs = Syntax.Expr_bin { lhs; op = Add; rhs }
    let var var = Syntax.Expr_var { var; ann = None }
    let int i = Syntax.Expr_int i
    let elet var expr body = Syntax.Expr_let { var; expr; body; ann = None }
  end

  module Ty = struct
    let tfun ?(stage = Syntax.Stage.Runtime) params ret =
      Syntax.Ty_fun { params; stage; ret }
    ;;

    let int = Syntax.Ty_int
  end
end

let check expr =
  let expr = Infer.infer expr |> Or_error.ok_exn in
  let res = Evaluate.evaluate expr in
  print_s [%sexp (res : Syntax.expr)]
;;

module Import = struct
  open Syntax_helpers
  module E = Expr
  module T = Ty
end

let%expect_test "simple" =
  let open Import in
  check (E.int 1234);
  [%expect {| (Expr_int 1234) |}];
  check (E.app (E.efun [ "x", T.int ] (E.var "x")) [ E.int 1 ]);
  [%expect
    {|
    (Expr_app
     (fn
      (Expr_fun
       ((params (((var x) (ty Ty_int)))) (stage Runtime)
        (body (Expr_var (var x) (ann (Ty_int))))
        (ann ((Ty_fun ((params (Ty_int)) (stage Runtime) (ret Ty_int))))))))
     (args ((Expr_int 1))) (ann (Ty_int)))
    |}];
  check (E.app (E.ecfun [ "x", T.int ] (E.var "x")) [ E.int 1 ]);
  [%expect
    {|
    (Expr_let (var x) (expr (Expr_int 1))
     (body (Expr_var (var x) (ann (Ty_int)))) (ann (Ty_int)))
    |}]
;;

let%expect_test "captured" =
  let open Import in
  check
    (E.elet
       "f"
       (E.ecfun
          [ "x", T.int ]
          (E.elet
             "y"
             (E.add (E.int 1) (E.int 2))
             (E.ecfun [ "z", T.int ] (E.add (E.add (E.var "z") (E.var "y")) (E.var "x")))))
       (E.elet
          "g"
          (E.app (E.var "f") [ E.add (E.int 9) (E.int 1234) ])
          (E.elet
             "r1"
             (E.app (E.var "g") [ E.add (E.int 10) (E.int 0) ])
             (E.elet
                "r2"
                (E.app (E.var "g") [ E.add (E.int 0) (E.int 2) ])
                (E.add (E.var "r1") (E.var "r2"))))));
  [%expect
    {|
    (Expr_let (var x)
     (expr (Expr_bin (lhs (Expr_int 9)) (op Add) (rhs (Expr_int 1234))))
     (body
      (Expr_let (var y)
       (expr (Expr_bin (lhs (Expr_int 1)) (op Add) (rhs (Expr_int 2))))
       (body
        (Expr_let (var z)
         (expr (Expr_bin (lhs (Expr_int 10)) (op Add) (rhs (Expr_int 0))))
         (body
          (Expr_let (var r1)
           (expr
            (Expr_bin
             (lhs
              (Expr_bin (lhs (Expr_var (var z) (ann (Ty_int)))) (op Add)
               (rhs (Expr_var (var y) (ann (Ty_int))))))
             (op Add) (rhs (Expr_var (var x) (ann (Ty_int))))))
           (body
            (Expr_let (var z)
             (expr (Expr_bin (lhs (Expr_int 0)) (op Add) (rhs (Expr_int 2))))
             (body
              (Expr_let (var r2)
               (expr
                (Expr_bin
                 (lhs
                  (Expr_bin (lhs (Expr_var (var z) (ann (Ty_int)))) (op Add)
                   (rhs (Expr_var (var y) (ann (Ty_int))))))
                 (op Add) (rhs (Expr_var (var x) (ann (Ty_int))))))
               (body
                (Expr_bin (lhs (Expr_var (var r1) (ann (Ty_int)))) (op Add)
                 (rhs (Expr_var (var r2) (ann (Ty_int))))))
               (ann (Ty_int))))
             (ann (Ty_int))))
           (ann (Ty_int))))
         (ann (Ty_int))))
       (ann (Ty_int))))
     (ann (Ty_int)))
    |}]
;;
