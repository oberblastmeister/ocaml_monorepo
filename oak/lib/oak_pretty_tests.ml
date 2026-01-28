open Prelude
module Syntax = Oak_syntax

let render ~width doc = Pp.render_to_string ~width doc

let check ?(widths = [ 80; 40; 20 ]) doc =
  List.iter widths ~f:(fun width ->
    printf "--- width %d ---\n" width;
    print_endline (render ~width doc))
;;

let%test_module "Pretty" =
  (module struct
    let check_value ?(widths = [ 80; 40; 20 ]) value =
      check ~widths (Syntax.Value.pp value)
    ;;

    let check_ty ?(widths = [ 80; 40; 20 ]) ty = check ~widths (Syntax.Ty.pp ty)
    let check_path ?(widths = [ 80; 40; 20 ]) path = check ~widths (Syntax.Path.pp path)

    let%expect_test "irrelevant" =
      check_value Value_irrelevant;
      [%expect
        {|
        --- width 80 ---
        _
        --- width 40 ---
        _
        --- width 20 ---
        _ |}]
    ;;

    let%expect_test "core types" =
      check_ty (Value_core_ty Ty_bool);
      [%expect
        {|
        --- width 80 ---
        Bool
        --- width 40 ---
        Bool
        --- width 20 ---
        Bool |}]
    ;;

    let%expect_test "core type int" =
      check_ty (Value_core_ty Ty_int);
      [%expect
        {|
        --- width 80 ---
        Int
        --- width 40 ---
        Int
        --- width 20 ---
        Int |}]
    ;;

    let%expect_test "core type unit" =
      check_ty (Value_core_ty Ty_unit);
      [%expect
        {|
        --- width 80 ---
        Unit
        --- width 40 ---
        Unit
        --- width 20 ---
        Unit |}]
    ;;

    let%expect_test "universe Type" =
      check_ty (Value_univ Type);
      [%expect
        {|
        --- width 80 ---
        Type
        --- width 40 ---
        Type
        --- width 20 ---
        Type |}]
    ;;

    let%expect_test "universe Kind" =
      check_ty (Value_univ Kind);
      [%expect
        {|
        --- width 80 ---
        Kind
        --- width 40 ---
        Kind
        --- width 20 ---
        Kind |}]
    ;;

    let%expect_test "mod with decls" =
      let v =
        Syntax.Value_mod
          { decls =
              [ { field = "x"; e = Value_ty (Value_core_ty Ty_int) }
              ; { field = "y"; e = Value_ty (Value_core_ty Ty_bool) }
              ]
          }
      in
      check_value v;
      [%expect
        {|
        --- width 80 ---
        mod: { let x = Int; let y = Bool }
        --- width 40 ---
        mod: { let x = Int; let y = Bool }
        --- width 20 ---
        mod:
          let x = Int
          let y = Bool
        |}]
    ;;

    let%expect_test "mod with many decls" =
      let v =
        Syntax.Value_mod
          { decls =
              [ { field = "alpha"; e = Value_ty (Value_core_ty Ty_int) }
              ; { field = "beta"; e = Value_ty (Value_core_ty Ty_bool) }
              ; { field = "gamma"; e = Value_ty (Value_core_ty Ty_unit) }
              ; { field = "delta"; e = Value_ty (Value_univ Type) }
              ]
          }
      in
      check_value v;
      [%expect
        {|
        --- width 80 ---
        mod: { let alpha = Int; let beta = Bool; let gamma = Unit; let delta = Type }
        --- width 40 ---
        mod:
          let alpha = Int
          let beta = Bool
          let gamma = Unit
          let delta = Type
        --- width 20 ---
        mod:
          let alpha = Int
          let beta = Bool
          let gamma = Unit
          let delta = Type
        |}]
    ;;

    let%expect_test "pure function (funct)" =
      let x = Syntax.Var.create_initial "x" 0 in
      let v =
        Syntax.Value_abs
          { binder =
              Syntax.Value_abs_binder.pack
                { params = [ { var = x; ty = Value_core_ty Ty_int } ]
                ; body = Value_ty (Value_core_ty Ty_int)
                }
          ; purity = Pure
          }
      in
      check_value v;
      [%expect
        {|
        --- width 80 ---
        funct(x_0 Int): Int
        --- width 40 ---
        funct(x_0 Int): Int
        --- width 20 ---
        funct(x_0 Int): Int
        |}]
    ;;

    let%expect_test "impure function (fun)" =
      let x = Syntax.Var.create_initial "x" 0 in
      let v =
        Syntax.Value_abs
          { binder =
              Syntax.Value_abs_binder.pack
                { params = [ { var = x; ty = Value_core_ty Ty_bool } ]
                ; body = Value_irrelevant
                }
          ; purity = Impure
          }
      in
      check_value v;
      [%expect
        {|
        --- width 80 ---
        fun(x_0 Bool): _
        --- width 40 ---
        fun(x_0 Bool): _
        --- width 20 ---
        fun(x_0 Bool): _
        |}]
    ;;

    let%expect_test "function with multiple params" =
      let x = Syntax.Var.create_initial "x" 0 in
      let y = Syntax.Var.create_initial "y" 0 in
      let z = Syntax.Var.create_initial "z" 0 in
      let v =
        Syntax.Value_abs
          { binder =
              Syntax.Value_abs_binder.pack
                { params =
                    [ { var = x; ty = Value_core_ty Ty_int }
                    ; { var = y; ty = Value_core_ty Ty_bool }
                    ; { var = z; ty = Value_core_ty Ty_unit }
                    ]
                ; body = Value_ty (Value_core_ty Ty_int)
                }
          ; purity = Pure
          }
      in
      check_value v;
      [%expect
        {|
        --- width 80 ---
        funct(x_0 Int, y_0 Bool, z_0 Unit): Int
        --- width 40 ---
        funct(x_0 Int, y_0 Bool, z_0 Unit): Int
        --- width 20 ---
        funct(
          x_0 Int,
          y_0 Bool,
          z_0 Unit
        ):
          Int
        |}]
    ;;

    let%expect_test "function with underscore param" =
      let underscore = Syntax.Var.create_initial "_" 0 in
      let v =
        Syntax.Value_abs
          { binder =
              Syntax.Value_abs_binder.pack
                { params = [ { var = underscore; ty = Value_core_ty Ty_int } ]
                ; body = Value_irrelevant
                }
          ; purity = Pure
          }
      in
      check_value v;
      [%expect
        {|
        --- width 80 ---
        funct(Int): _
        --- width 40 ---
        funct(Int): _
        --- width 20 ---
        funct(Int): _
        |}]
    ;;

    let%expect_test "ty_sing (Is)" =
      check_ty (Value_ty_sing { e = Value_irrelevant; ty = Value_core_ty Ty_bool });
      [%expect
        {|
        --- width 80 ---
        Is(_, Bool)
        --- width 40 ---
        Is(_, Bool)
        --- width 20 ---
        Is(_, Bool)
        |}]
    ;;

    let%expect_test "ty_mod (sig)" =
      let mvar = Syntax.Mod_var.create_initial 0 in
      check_ty
        (Value_ty_mod
           { binder =
               Syntax.Value_ty_mod_binder.pack
                 { var = mvar
                 ; ty_decls =
                     [ { field = "x"; ty = Value_core_ty Ty_int }
                     ; { field = "y"; ty = Value_core_ty Ty_bool }
                     ]
                 ; ty_decls_map =
                     String.Map.of_alist_exn
                       [ ( "x"
                         , ({ field = "x"; ty = Value_core_ty Ty_int }
                            : Syntax.value_ty_decl) )
                       ; "y", { field = "y"; ty = Value_core_ty Ty_bool }
                       ]
                 }
           });
      [%expect
        {|
        --- width 80 ---
        sig m_0: { let x Int; let y Bool }
        --- width 40 ---
        sig m_0: { let x Int; let y Bool }
        --- width 20 ---
        sig m_0:
          let x Int
          let y Bool
        |}]
    ;;

    let%expect_test "ty_fun pure" =
      let a = Syntax.Var.create_initial "a" 0 in
      check_ty
        (Value_ty_fun
           { binder =
               Syntax.Value_ty_fun_binder.pack
                 { params = [ { var = a; ty = Value_core_ty Ty_int } ]
                 ; body_ty = Value_core_ty Ty_bool
                 }
           ; purity = Pure
           });
      [%expect
        {|
        --- width 80 ---
        Funct(a_0 Int) Bool
        --- width 40 ---
        Funct(a_0 Int) Bool
        --- width 20 ---
        Funct(a_0 Int) Bool
        |}]
    ;;

    let%expect_test "ty_fun impure" =
      let a = Syntax.Var.create_initial "a" 0 in
      check_ty
        (Value_ty_fun
           { binder =
               Syntax.Value_ty_fun_binder.pack
                 { params = [ { var = a; ty = Value_core_ty Ty_int } ]
                 ; body_ty = Value_core_ty Ty_bool
                 }
           ; purity = Impure
           });
      [%expect
        {|
        --- width 80 ---
        Fun(a_0 Int) Bool
        --- width 40 ---
        Fun(a_0 Int) Bool
        --- width 20 ---
        Fun(a_0 Int) Bool
        |}]
    ;;

    let%expect_test "ty_fun with multiple params" =
      let a = Syntax.Var.create_initial "a" 0 in
      let b = Syntax.Var.create_initial "b" 0 in
      let c = Syntax.Var.create_initial "c" 0 in
      check_ty
        (Value_ty_fun
           { binder =
               Syntax.Value_ty_fun_binder.pack
                 { params =
                     [ { var = a; ty = Value_core_ty Ty_int }
                     ; { var = b; ty = Value_core_ty Ty_bool }
                     ; { var = c; ty = Value_core_ty Ty_unit }
                     ]
                 ; body_ty = Value_univ Type
                 }
           ; purity = Pure
           });
      [%expect
        {|
        --- width 80 ---
        Funct(a_0 Int, b_0 Bool, c_0 Unit) Type
        --- width 40 ---
        Funct(a_0 Int, b_0 Bool, c_0 Unit) Type
        --- width 20 ---
        Funct(
          a_0 Int,
          b_0 Bool,
          c_0 Unit
        ) Type
        |}]
    ;;

    let%expect_test "nested: mod containing function" =
      let x = Syntax.Var.create_initial "x" 0 in
      let v =
        Syntax.Value_mod
          { decls =
              [ { field = "f"
                ; e =
                    Value_abs
                      { binder =
                          Syntax.Value_abs_binder.pack
                            { params = [ { var = x; ty = Value_core_ty Ty_int } ]
                            ; body = Value_ty (Value_core_ty Ty_bool)
                            }
                      ; purity = Pure
                      }
                }
              ; { field = "n"; e = Value_ty (Value_core_ty Ty_int) }
              ]
          }
      in
      check_value v;
      [%expect
        {|
        --- width 80 ---
        mod: { let f = funct(x_0 Int): Bool; let n = Int }
        --- width 40 ---
        mod:
          let f = funct(x_0 Int): Bool
          let n = Int
        --- width 20 ---
        mod:
          let f =
            funct(x_0 Int):
              Bool
          let n = Int
        |}]
    ;;

    let%expect_test "path var" =
      let x = Syntax.Var.create_initial "x" 0 in
      check_path (Path_var (Var x));
      [%expect
        {|
        --- width 80 ---
        x_0
        --- width 40 ---
        x_0
        --- width 20 ---
        x_0
        |}]
    ;;

    let%expect_test "path mod var" =
      let m = Syntax.Mod_var.create_initial 0 in
      check_path (Path_var (Mod_var m));
      [%expect
        {|
        --- width 80 ---
        m_0
        --- width 40 ---
        m_0
        --- width 20 ---
        m_0
        |}]
    ;;

    let%expect_test "path proj" =
      let m = Syntax.Mod_var.create_initial 0 in
      check_path (Path_proj { mod_e = Path_var (Mod_var m); field = "foo" });
      [%expect
        {|
        --- width 80 ---
        m_0.foo
        --- width 40 ---
        m_0.foo
        --- width 20 ---
        m_0.foo
        |}]
    ;;

    let%expect_test "path nested proj" =
      let m = Syntax.Mod_var.create_initial 0 in
      check_path
        (Path_proj
           { mod_e = Path_proj { mod_e = Path_var (Mod_var m); field = "inner" }
           ; field = "outer"
           });
      [%expect
        {|
        --- width 80 ---
        m_0.inner.outer
        --- width 40 ---
        m_0.inner.outer
        --- width 20 ---
        m_0.inner.outer
        |}]
    ;;

    let%expect_test "path app" =
      let f = Syntax.Var.create_initial "f" 0 in
      check_path
        (Path_app { func = Path_var (Var f); args = [ Value_ty (Value_core_ty Ty_int) ] });
      [%expect
        {|
        --- width 80 ---
        f_0(Int)
        --- width 40 ---
        f_0(Int)
        --- width 20 ---
        f_0(Int)
        |}]
    ;;

    let%expect_test "path app multiple args" =
      let f = Syntax.Var.create_initial "f" 0 in
      check_path
        (Path_app
           { func = Path_var (Var f)
           ; args =
               [ Value_ty (Value_core_ty Ty_int)
               ; Value_ty (Value_core_ty Ty_bool)
               ; Value_ty (Value_core_ty Ty_unit)
               ]
           });
      [%expect
        {|
        --- width 80 ---
        f_0(Int, Bool, Unit)
        --- width 40 ---
        f_0(Int, Bool, Unit)
        --- width 20 ---
        f_0(Int, Bool, Unit)
        |}]
    ;;

    let%expect_test "path app on proj" =
      let m = Syntax.Mod_var.create_initial 0 in
      check_path
        (Path_app
           { func = Path_proj { mod_e = Path_var (Mod_var m); field = "apply" }
           ; args = [ Value_ty (Value_core_ty Ty_int) ]
           });
      [%expect
        {|
        --- width 80 ---
        m_0.apply(Int)
        --- width 40 ---
        m_0.apply(Int)
        --- width 20 ---
        m_0.apply(Int)
        |}]
    ;;

    let%expect_test "path app wide args" =
      let f = Syntax.Var.create_initial "transform" 0 in
      let a = Syntax.Var.create_initial "alpha" 0 in
      let b = Syntax.Var.create_initial "beta" 0 in
      let c = Syntax.Var.create_initial "gamma" 0 in
      check_path
        (Path_app
           { func = Path_var (Var f)
           ; args =
               [ Value_ty (Value_path (Path_var (Var a)))
               ; Value_ty (Value_path (Path_var (Var b)))
               ; Value_ty (Value_path (Path_var (Var c)))
               ]
           });
      [%expect
        {|
        --- width 80 ---
        transform_0(alpha_0, beta_0, gamma_0)
        --- width 40 ---
        transform_0(alpha_0, beta_0, gamma_0)
        --- width 20 ---
        transform_0(
          alpha_0,
          beta_0,
          gamma_0
        )
        |}]
    ;;

    let%expect_test "path chained apps" =
      let f = Syntax.Var.create_initial "apply" 0 in
      let a = Syntax.Var.create_initial "alpha" 0 in
      let b = Syntax.Var.create_initial "beta" 0 in
      let c = Syntax.Var.create_initial "gamma" 0 in
      check_path
        (Path_app
           { func =
               Path_app
                 { func =
                     Path_app
                       { func = Path_var (Var f)
                       ; args = [ Value_ty (Value_path (Path_var (Var a))) ]
                       }
                 ; args = [ Value_ty (Value_path (Path_var (Var b))) ]
                 }
           ; args = [ Value_ty (Value_path (Path_var (Var c))) ]
           });
      [%expect
        {|
        --- width 80 ---
        apply_0(alpha_0)(beta_0)(gamma_0)
        --- width 40 ---
        apply_0(alpha_0)(beta_0)(gamma_0)
        --- width 20 ---
        apply_0(alpha_0)(
          beta_0
        )(gamma_0)
        |}]
    ;;

    let%expect_test "path apps with projections" =
      let m = Syntax.Mod_var.create_initial 0 in
      (* m.make(Int).transform(Bool).result *)
      check_path
        (Path_proj
           { mod_e =
               Path_app
                 { func =
                     Path_proj
                       { mod_e =
                           Path_app
                             { func =
                                 Path_proj
                                   { mod_e = Path_var (Mod_var m); field = "make" }
                             ; args = [ Value_ty (Value_core_ty Ty_int) ]
                             }
                       ; field = "transform"
                       }
                 ; args = [ Value_ty (Value_core_ty Ty_bool) ]
                 }
           ; field = "result"
           });
      [%expect
        {|
        --- width 80 ---
        m_0.make(Int).transform(Bool).result
        --- width 40 ---
        m_0.make(Int).transform(Bool).result
        --- width 20 ---
        m_0.make(Int).transform(
          Bool
        ).result
        |}]
    ;;

    let%expect_test "path apps and projections interleaved" =
      let m = Syntax.Mod_var.create_initial 0 in
      let a = Syntax.Var.create_initial "a" 0 in
      let b = Syntax.Var.create_initial "b" 0 in
      let c = Syntax.Var.create_initial "c" 0 in
      let arg_var v : Syntax.value = Value_ty (Value_path (Path_var (Var v))) in
      (* m.foo.bar.baz(a, b, c).qux.quux.corge(a, b, c).grault.garply.waldo(a, b, c) *)
      let base = Syntax.Path_var (Mod_var m) in
      let p1 =
        Syntax.Path_proj
          { mod_e =
              Path_proj
                { mod_e = Path_proj { mod_e = base; field = "foo" }; field = "bar" }
          ; field = "baz"
          }
      in
      let app1 =
        Syntax.Path_app { func = p1; args = [ arg_var a; arg_var b; arg_var c ] }
      in
      let p2 =
        Syntax.Path_proj
          { mod_e =
              Path_proj
                { mod_e = Path_proj { mod_e = app1; field = "qux" }; field = "quux" }
          ; field = "corge"
          }
      in
      let app2 =
        Syntax.Path_app { func = p2; args = [ arg_var a; arg_var b; arg_var c ] }
      in
      let p3 =
        Syntax.Path_proj
          { mod_e =
              Path_proj
                { mod_e = Path_proj { mod_e = app2; field = "grault" }; field = "garply" }
          ; field = "waldo"
          }
      in
      let app3 =
        Syntax.Path_app { func = p3; args = [ arg_var a; arg_var b; arg_var c ] }
      in
      check_path app3;
      [%expect
        {|
        --- width 80 ---
        m_0.foo.bar.baz(a_0, b_0, c_0).qux.quux.corge(a_0, b_0, c_0).grault.garply.waldo(
          a_0,
          b_0,
          c_0
        )
        --- width 40 ---
        m_0.foo.bar.baz(a_0, b_0, c_0).qux.quux.corge(
          a_0,
          b_0,
          c_0
        ).grault.garply.waldo(a_0, b_0, c_0)
        --- width 20 ---
        m_0.foo.bar.baz(
          a_0,
          b_0,
          c_0
        ).qux.quux.corge(
          a_0,
          b_0,
          c_0
        ).grault.garply.waldo(
          a_0,
          b_0,
          c_0
        )
        |}]
    ;;

    let%expect_test "ty via path" =
      let x = Syntax.Var.create_initial "x" 0 in
      check_ty (Value_path (Path_var (Var x)));
      [%expect
        {|
        --- width 80 ---
        x_0
        --- width 40 ---
        x_0
        --- width 20 ---
        x_0
        |}]
    ;;

    let%expect_test "value wrapped ty" =
      check_value (Value_ty (Value_core_ty Ty_bool));
      [%expect
        {|
        --- width 80 ---
        Bool
        --- width 40 ---
        Bool
        --- width 20 ---
        Bool |}]
    ;;
  end)
;;
