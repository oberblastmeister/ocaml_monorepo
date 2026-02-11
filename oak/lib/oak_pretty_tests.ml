open Core
module Syntax = Oak_syntax
module Name_list = Oak_common.Name_list
module Pp = Utility.Pp
module Bwd = Utility.Bwd

let var name : Syntax.Var_info.t = { name; pos = 0 }
let closure body : Syntax.value_closure = { env = Syntax.Env.empty; body }

let check ?(show_singletons = false) ?(width = 80) ?(names = Name_list.empty) value =
  let doc = Oak_pretty.pp_value ~show_singletons names value in
  print_endline (Pp.render_to_string ~width doc)
;;

let check_widths ?(names = Name_list.empty) value widths =
  List.iter widths ~f:(fun width ->
    Printf.printf "--- width %d ---\n" width;
    let doc = Oak_pretty.pp_value names value in
    print_endline (Pp.render_to_string ~width doc))
;;

let%test_module "pretty print" =
  (module struct
    let%expect_test "core type" =
      check (Value_core_ty Bool);
      [%expect {| Bool |}]
    ;;

    let%expect_test "universes" =
      check (Value_universe Type);
      check (Value_universe Kind);
      check (Value_universe Sig);
      [%expect
        {|
        Type
        Kind
        Sig
        |}]
    ;;

    let%expect_test "simple function type (unnamed)" =
      (* Fun Bool -> Bool *)
      let ty =
        Syntax.Value_ty_fun
          { var = var "_"
          ; param_ty = Value_core_ty Bool
          ; body_ty = closure (Term_core_ty Bool)
          }
      in
      check ty;
      [%expect {| Bool -> Bool |}]
    ;;

    let%expect_test "named function type" =
      (* Fun (x : Bool) -> Bool *)
      let ty =
        Syntax.Value_ty_fun
          { var = var "x"
          ; param_ty = Value_core_ty Bool
          ; body_ty = closure (Term_core_ty Bool)
          }
      in
      check ty;
      [%expect {| (x : Bool) -> Bool |}]
    ;;

    let%expect_test "nested function type" =
      (* Fun Bool -> Fun Bool -> Bool *)
      let ty =
        Syntax.Value_ty_fun
          { var = var "_"
          ; param_ty = Value_core_ty Bool
          ; body_ty =
              closure
                (Term_ty_fun
                   { var = var "_"
                   ; param_ty = Term_core_ty Bool
                   ; body_ty = Term_core_ty Bool
                   })
          }
      in
      check ty;
      [%expect {| Bool -> Bool -> Bool |}]
    ;;

    let%expect_test "abstraction" =
      (* fun x -> x *)
      let v =
        Syntax.Value_abs { var = var "x"; body = closure (Term_var (Syntax.index 0)) }
      in
      check v;
      [%expect {| fun x -> x |}]
    ;;

    let%expect_test "neutral variable" =
      let names = Name_list.push "x" Name_list.empty in
      check ~names (Value_neutral { head = Syntax.level 0; spine = Empty });
      [%expect {| x |}]
    ;;

    let%expect_test "neutral application" =
      (* f x *)
      let names = Name_list.push "f" Name_list.empty in
      let names = Name_list.push "x" names in
      let v : Syntax.value =
        Value_neutral
          { head = Syntax.level 0
          ; spine =
              Bwd.snoc Bwd.Empty (Syntax.Elim_app (Syntax.Value.var (Syntax.level 1)))
          }
      in
      check ~names v;
      [%expect {| f x |}]
    ;;

    let%expect_test "neutral projection" =
      (* x.field *)
      let names = Name_list.push "x" Name_list.empty in
      let v : Syntax.value =
        Value_neutral
          { head = Syntax.level 0
          ; spine = Snoc (Empty, Elim_proj { field = "field"; field_index = 0 })
          }
      in
      check ~names v;
      [%expect {| x.field |}]
    ;;

    let%expect_test "module" =
      let v : Syntax.value =
        Value_mod
          { fields =
              [ { name = "x"; e = Value_core_ty Bool }
              ; { name = "y"; e = Value_universe Type }
              ]
          }
      in
      check v;
      [%expect {| mod { let x = Bool; let y = Type } |}]
    ;;

    let%expect_test "type module (sig)" =
      let v : Syntax.value =
        Value_ty_mod
          { env = Syntax.Env.empty
          ; ty_decls =
              [ { var = var "x"; ty = Term_core_ty Bool }
              ; { var = var "y"; ty = Term_universe Type }
              ]
          }
      in
      check v;
      [%expect {| sig { let x : Bool; let y : Type } |}]
    ;;

    let%expect_test "larger sig at different widths" =
      let v : Syntax.value =
        Value_ty_mod
          { env = Syntax.Env.empty
          ; ty_decls =
              [ { var = var "name"; ty = Term_core_ty Bool }
              ; { var = var "value"; ty = Term_core_ty Bool }
              ; { var = var "result"
                ; ty =
                    Term_ty_fun
                      { var = var "_"
                      ; param_ty = Term_core_ty Bool
                      ; body_ty =
                          Term_ty_fun
                            { var = var "_"
                            ; param_ty = Term_core_ty Bool
                            ; body_ty = Term_core_ty Bool
                            }
                      }
                }
              ; { var = var "extra"; ty = Term_universe Kind }
              ]
          }
      in
      check_widths v [ 80; 40; 20 ];
      [%expect
        {|
        --- width 80 ---
        sig {
          let name : Bool
          let value : Bool
          let result : Bool -> Bool -> Bool
          let extra : Kind
        }
        --- width 40 ---
        sig {
          let name : Bool
          let value : Bool
          let result : Bool -> Bool -> Bool
          let extra : Kind
        }
        --- width 20 ---
        sig {
          let name : Bool
          let value : Bool
          let result :
            Bool ->
            Bool ->
            Bool
          let extra : Kind
        }
        |}]
    ;;

    let%expect_test "type pack" =
      check (Value_ty_pack (Value_core_ty Bool));
      [%expect {| Pack Bool |}]
    ;;

    let%expect_test "singleton type (hidden)" =
      let v : Syntax.value =
        Value_ty_sing { identity = Value_core_ty Bool; ty = Value_universe Type }
      in
      check v;
      [%expect {| (= Bool) |}]
    ;;

    let%expect_test "singleton type (shown)" =
      let v : Syntax.value =
        Value_ty_sing { identity = Value_core_ty Bool; ty = Value_universe Type }
      in
      check ~show_singletons:true v;
      [%expect {| (= Bool) |}]
    ;;

    let%expect_test "multiple projections and applications" =
      (* f x.a.b y *)
      let names = Name_list.push "f" Name_list.empty in
      let names = Name_list.push "x" names in
      let names = Name_list.push "y" names in
      let x_a_b : Syntax.value =
        Value_neutral
          { head = Syntax.level 1
          ; spine =
              Bwd.snoc
                (Bwd.snoc Bwd.Empty (Syntax.Elim_proj { field = "a"; field_index = 0 }))
                (Syntax.Elim_proj { field = "b"; field_index = 1 })
          }
      in
      let v : Syntax.value =
        Value_neutral
          { head = Syntax.level 0
          ; spine =
              Bwd.snoc
                (Bwd.snoc Bwd.Empty (Syntax.Elim_app x_a_b))
                (Syntax.Elim_app (Syntax.Value.var (Syntax.level 2)))
          }
      in
      check ~names v;
      [%expect {| f x.a.b y |}]
    ;;

    let%expect_test "application then projections then applications" =
      (* (f a b c).a.b.c x y *)
      let names = Name_list.push "f" Name_list.empty in
      let names = Name_list.push "a" names in
      let names = Name_list.push "b" names in
      let names = Name_list.push "c" names in
      let names = Name_list.push "x" names in
      let names = Name_list.push "y" names in
      let v : Syntax.value =
        Value_neutral
          { head = Syntax.level 0
          ; spine =
              Bwd.snoc
                (Bwd.snoc
                   (Bwd.snoc
                      (Bwd.snoc
                         (Bwd.snoc
                            (Bwd.snoc
                               (Bwd.snoc
                                  (Bwd.snoc
                                     Bwd.Empty
                                     (Syntax.Elim_app (Syntax.Value.var (Syntax.level 1))))
                                  (Syntax.Elim_app (Syntax.Value.var (Syntax.level 2))))
                               (Syntax.Elim_app (Syntax.Value.var (Syntax.level 3))))
                            (Syntax.Elim_proj { field = "a"; field_index = 0 }))
                         (Syntax.Elim_proj { field = "b"; field_index = 1 }))
                      (Syntax.Elim_proj { field = "c"; field_index = 2 }))
                   (Syntax.Elim_app (Syntax.Value.var (Syntax.level 4))))
                (Syntax.Elim_app (Syntax.Value.var (Syntax.level 5)))
          }
      in
      check ~names v;
      [%expect {| (f a b c).a.b.c x y |}]
    ;;

    let%expect_test "complex type at different widths" =
      (* Fun (x : Fun Bool -> Bool) -> Fun (y : Bool) -> Bool *)
      let inner_fun =
        Syntax.Value_ty_fun
          { var = var "_"
          ; param_ty = Value_core_ty Bool
          ; body_ty = closure (Term_core_ty Bool)
          }
      in
      let ty =
        Syntax.Value_ty_fun
          { var = var "x"
          ; param_ty = inner_fun
          ; body_ty =
              closure
                (Term_ty_fun
                   { var = var "y"
                   ; param_ty = Term_core_ty Bool
                   ; body_ty = Term_core_ty Bool
                   })
          }
      in
      check_widths ty [ 80; 40; 20 ];
      [%expect
        {|
        --- width 80 ---
        (x : Bool -> Bool) -> (y : Bool) -> Bool
        --- width 40 ---
        (x : Bool -> Bool) -> (y : Bool) -> Bool
        --- width 20 ---
        (x : Bool -> Bool) ->
        (y : Bool) ->
        Bool
        |}]
    ;;
  end)
;;
