open Core
module Pp = Utility_pp
module Doc = Pp.Doc
open Doc.Syntax

module Lam = struct
  type t =
    | Var of string
    | App of t * t
    | Abs of string * t
    | Let of string * t * t
  [@@deriving sexp_of]

  let rec get_spine_rec (acc : t list) = function
    | App (f, x) -> get_spine_rec (x :: acc) f
    | f -> f, acc
  ;;

  let get_spine f = get_spine_rec [] f
end

let rec pp (e : Lam.t) : Doc.t =
  let open Doc.Syntax in
  match e with
  | Var x -> Doc.string x
  | Abs (x, body) ->
    Doc.group
      (Doc.char '\\'
       ^^ Doc.string x
       ^^ Doc.string "."
       ^^ Doc.indent 2 (Doc.break1 ^^ pp body))
  | App _ ->
    let f, args = Lam.get_spine e in
    Doc.group
      (pp1 f
       ^^ Doc.indent
            2
            (Doc.break1
             ^^ List.fold
                  (List.tl_exn args)
                  ~init:(pp1 (List.hd_exn args))
                  ~f:(fun doc arg -> doc ^^ Doc.break1 ^^ pp1 arg)))
  | Let (x, rhs, body) ->
    Doc.group
      (Doc.group
         (Doc.string "let"
          ^^ Doc.space
          ^^ Doc.string x
          ^^ Doc.space
          ^^ Doc.string "="
          ^^ Doc.indent 2 (Doc.break1 ^^ pp rhs)
          ^^ Doc.break1)
       ^^ Doc.string "in"
       ^^ Doc.break1
       ^^ pp body)

and pp1 (e : Lam.t) : Doc.t =
  match e with
  | Abs _ | App _ | Let _ ->
    Doc.group (Doc.char '(' ^^ Doc.indent 1 (pp e) ^^ Doc.char ')')
  | Var _ -> pp e
;;

let check ?(width = 40) e = print_endline (Utility_pp.render_to_string ~width (pp e))

let check_widths expr widths =
  List.iter widths ~f:(fun w ->
    Printf.printf "--- width %d ---\n" w;
    check ~width:w expr)
;;

let%expect_test "variable" =
  check (Var "x");
  [%expect {| x |}]
;;

let%expect_test "simple abstraction" =
  check (Abs ("x", Var "x"));
  [%expect {| \x. x |}]
;;

let%expect_test "nested abstraction" =
  check (Abs ("f", Abs ("x", App (Var "f", Var "x"))));
  [%expect {| \f. \x. f x |}]
;;

let%expect_test "simple application" =
  check (App (Var "f", Var "x"));
  [%expect {| f x |}]
;;

let%expect_test "multi-arg application" =
  check (App (App (Var "f", Var "x"), Var "y"));
  [%expect {| f x y |}]
;;

let%expect_test "application right-associates with parens" =
  check (App (Var "f", App (Var "g", Var "x")));
  [%expect {| f (g x) |}]
;;

let%expect_test "abstraction in argument position gets parens" =
  check (App (Var "f", Abs ("x", Var "x")));
  [%expect {| f (\x. x) |}]
;;

let%expect_test "simple let" =
  check (Let ("x", Var "y", Var "x"));
  [%expect {| let x = y in x |}]
;;

let%expect_test "let with complex rhs" =
  check (Let ("f", Abs ("x", Var "x"), App (Var "f", Var "a")));
  [%expect {| let f = \x. x in f a |}]
;;

let%expect_test "nested lets" =
  check (Let ("x", Var "a", Let ("y", Var "b", App (Var "x", Var "y"))));
  [%expect {| let x = a in let y = b in x y |}]
;;

let%expect_test "church true" =
  let church_true = Lam.Abs ("t", Abs ("f", Var "t")) in
  check church_true;
  [%expect {| \t. \f. t |}]
;;

let%expect_test "church false" =
  let church_false = Lam.Abs ("t", Abs ("f", Var "f")) in
  check church_false;
  [%expect {| \t. \f. f |}]
;;

let%expect_test "church and" =
  let church_false = Lam.Abs ("t", Abs ("f", Var "f")) in
  let church_and = Lam.Abs ("p", Abs ("q", App (App (Var "p", Var "q"), church_false))) in
  check church_and;
  [%expect {| \p. \q. p q (\t. \f. f) |}]
;;

let%expect_test "I combinator" =
  let comb_i = Lam.Abs ("x", Var "x") in
  check comb_i;
  [%expect {| \x. x |}]
;;

let%expect_test "S combinator" =
  let comb_s =
    Lam.Abs
      ("x", Abs ("y", Abs ("z", App (App (Var "x", Var "z"), App (Var "y", Var "z")))))
  in
  check comb_s;
  [%expect {| \x. \y. \z. x z (y z) |}]
;;

let%expect_test "S K K = I (applied)" =
  let comb_k = Lam.Abs ("x", Abs ("y", Var "x")) in
  let comb_s =
    Lam.Abs
      ("x", Abs ("y", Abs ("z", App (App (Var "x", Var "z"), App (Var "y", Var "z")))))
  in
  check (App (App (comb_s, comb_k), comb_k));
  [%expect
    {|
    (\x. \y. \z. x z (y z))
      (\x. \y. x)
      (\x. \y. x)
    |}]
;;

let%expect_test "omega combinator" =
  let omega_half = Lam.Abs ("x", App (Var "x", Var "x")) in
  let omega = Lam.App (omega_half, omega_half) in
  check omega;
  [%expect {| (\x. x x) (\x. x x) |}]
;;

let%expect_test "Y combinator" =
  let inner = Lam.Abs ("x", App (Var "f", App (Var "x", Var "x"))) in
  let y_comb = Lam.Abs ("f", App (inner, inner)) in
  check y_comb;
  [%expect {| \f. (\x. f (x x)) (\x. f (x x)) |}]
;;

let%expect_test "narrow width forces breaks" =
  check ~width:15 (App (App (Var "f", Var "x"), Var "y"));
  [%expect {| f x y |}]
;;

let%expect_test "very narrow width" =
  check ~width:10 (App (App (App (Var "func", Var "aaa"), Var "bbb"), Var "ccc"));
  [%expect
    {|
    func
      aaa
      bbb
      ccc
    |}]
;;

let%expect_test "let with narrow width" =
  check ~width:15 (Let ("longname", Abs ("x", App (Var "f", Var "x")), Var "longname"));
  [%expect
    {|
    let longname =
      \x. f x
    in
    longname
    |}]
;;

let%expect_test "deeply nested application narrow" =
  check ~width:12 (App (App (App (App (Var "a", Var "b"), Var "c"), Var "d"), Var "e"));
  [%expect {| a b c d e |}]
;;

(* ---------- Width variation tests ---------- *)

let%expect_test "church_and at various widths" =
  let church_false = Lam.Abs ("t", Abs ("f", Var "f")) in
  let church_and = Lam.Abs ("p", Abs ("q", App (App (Var "p", Var "q"), church_false))) in
  check_widths church_and [ 80; 40; 20; 10 ];
  [%expect
    {|
    --- width 80 ---
    \p. \q. p q (\t. \f. f)
    --- width 40 ---
    \p. \q. p q (\t. \f. f)
    --- width 20 ---
    \p.
      \q.
        p q (\t. \f. f)
    --- width 10 ---
    \p.
      \q.
        p
          q
          (\t.
             \f.
               f)
    |}]
;;

let%expect_test "S K K at various widths" =
  let comb_k = Lam.Abs ("x", Abs ("y", Var "x")) in
  let comb_s =
    Lam.Abs
      ("x", Abs ("y", Abs ("z", App (App (Var "x", Var "z"), App (Var "y", Var "z")))))
  in
  let skk = Lam.App (App (comb_s, comb_k), comb_k) in
  check_widths skk [ 80; 40; 25; 15 ];
  [%expect
    {|
    --- width 80 ---
    (\x. \y. \z. x z (y z)) (\x. \y. x) (\x. \y. x)
    --- width 40 ---
    (\x. \y. \z. x z (y z))
      (\x. \y. x)
      (\x. \y. x)
    --- width 25 ---
    (\x. \y. \z. x z (y z))
      (\x. \y. x)
      (\x. \y. x)
    --- width 15 ---
    (\x.
       \y.
         \z.
           x
             z
             (y z))
      (\x. \y. x)
      (\x. \y. x)
    |}]
;;

let%expect_test "Y combinator at various widths" =
  let inner = Lam.Abs ("x", App (Var "f", App (Var "x", Var "x"))) in
  let y_comb = Lam.Abs ("f", App (inner, inner)) in
  check_widths y_comb [ 80; 40; 20; 10 ];
  [%expect
    {|
    --- width 80 ---
    \f. (\x. f (x x)) (\x. f (x x))
    --- width 40 ---
    \f. (\x. f (x x)) (\x. f (x x))
    --- width 20 ---
    \f.
      (\x. f (x x))
        (\x. f (x x))
    --- width 10 ---
    \f.
      (\x.
         f
           (x
              x))
        (\x.
           f
             (x
                x))
    |}]
;;

let%expect_test "nested let at various widths" =
  let nested_let = Lam.Let ("x", Var "a", Let ("y", Var "b", App (Var "x", Var "y"))) in
  check_widths nested_let [ 80; 20; 10 ];
  [%expect
    {|
    --- width 80 ---
    let x = a in let y = b in x y
    --- width 20 ---
    let x = a in
    let y = b in x y
    --- width 10 ---
    let x = a in
    let y = b in
    x y
    |}]
;;

let%expect_test "let with complex rhs at various widths" =
  let expr = Lam.Let ("f", Abs ("x", Var "x"), App (Var "f", Var "a")) in
  check_widths expr [ 80; 20; 10 ];
  [%expect
    {|
    --- width 80 ---
    let f = \x. x in f a
    --- width 20 ---
    let f = \x. x in f a
    --- width 10 ---
    let f =
      \x. x
    in
    f a
    |}]
;;

let%expect_test "omega at various widths" =
  let omega_half = Lam.Abs ("x", App (Var "x", Var "x")) in
  let omega = Lam.App (omega_half, omega_half) in
  check_widths omega [ 80; 20; 10 ];
  [%expect
    {|
    --- width 80 ---
    (\x. x x) (\x. x x)
    --- width 20 ---
    (\x. x x) (\x. x x)
    --- width 10 ---
    (\x. x x)
      (\x. x x)
    |}]
;;

let%expect_test "big application at various widths" =
  let big_app =
    Lam.App
      ( App (App (App (Var "function_name", Var "alpha"), Var "beta"), Var "gamma")
      , Var "delta" )
  in
  check_widths big_app [ 80; 40; 25; 15 ];
  [%expect
    {|
    --- width 80 ---
    function_name alpha beta gamma delta
    --- width 40 ---
    function_name alpha beta gamma delta
    --- width 25 ---
    function_name
      alpha
      beta
      gamma
      delta
    --- width 15 ---
    function_name
      alpha
      beta
      gamma
      delta
    |}]
;;

let%expect_test "let compose at various widths" =
  let compose =
    Lam.Let
      ( "compose"
      , Abs ("f", Abs ("g", Abs ("x", App (Var "f", App (Var "g", Var "x")))))
      , App (App (Var "compose", Abs ("x", Var "x")), Abs ("y", Var "y")) )
  in
  check_widths compose [ 80; 40; 30; 20 ];
  [%expect
    {|
    --- width 80 ---
    let compose = \f. \g. \x. f (g x) in compose (\x. x) (\y. y)
    --- width 40 ---
    let compose = \f. \g. \x. f (g x) in
    compose (\x. x) (\y. y)
    --- width 30 ---
    let compose =
      \f. \g. \x. f (g x)
    in
    compose (\x. x) (\y. y)
    --- width 20 ---
    let compose =
      \f.
        \g. \x. f (g x)
    in
    compose
      (\x. x)
      (\y. y)
    |}]
;;
