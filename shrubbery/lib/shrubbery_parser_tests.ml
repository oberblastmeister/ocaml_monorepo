open Prelude

open struct
  module Lexer = Shrubbery_lexer
  module Parser = Shrubbery_parser
  module Token = Shrubbery_token
  module Token_tree = Shrubbery_token_tree
  module Delimit = Shrubbery_delimit
  module Layout = Shrubbery_layout
  module Syntax = Shrubbery_syntax
end

let check s =
  let _tts, block, errors = Parser.parse s in
  print_s (Syntax.Minimal_sexp_of.sexp_of_block block);
  if not (List.is_empty errors) then print_s [%sexp (errors : Delimit.Error.t list)];
  ()
;;

let%expect_test "basic" =
  check
    {|
def first:
  x
  y
  z
    |};
  [%expect
    {| (_{ (((def first (: (_{ (((x) "_;") ((y) "_;") ((z))) _}))))) _}) |}]
;;

let%expect_test "more" =
  check
    {|
def first:
  call_function(
    do:
      a
      b; c
      d,
    do:
      x; y;
      z,
    do:
      a; b; c; d
  )
  |};
  [%expect
    {|
    (_{
     (((def first
        (:
         (_{
          (((call_function
             ("(" ((do (: (_{ (((a) "_;") ((b) ";") ((c) "_;") ((d))) _}))) ,)
              ((do (: (_{ (((x) ";") ((y) ";") ((z))) _}))) ,)
              ((do (: (_{ (((a) ";") ((b) ";") ((c) ";") ((d))) _})))) ")"))))
          _})))))
     _})
    |}]
;;

let%expect_test "invalid comma on toplevel" =
  check
    {|
def first:
  x; y; z
,
def another:
  x; y; z
    |};
  [%expect
    {|
    (_{
     (((def first (: (_{ (((x) ";") ((y) ";") ((z))) _}))) "_;")
      (("(Error ,)") "_;")
      ((def another (: (_{ (((x) ";") ((y) ";") ((z))) _})))))
     _})
    |}]
;;

let%expect_test "weird start indentation" =
  check
    {|
  def f:
    x; y; z
    
  def another:
    x
    y
  |};
  [%expect
    {|
    (_{
     (((def f (: (_{ (((x) ";") ((y) ";") ((z))) _}))))
      ((def another (: (_{ (((x) "_;") ((y))) _})))))
     _})
    |}]
;;

let%expect_test "weird start indentation dedented" =
  check
    {|
  def f:
    x; y; z
    
def another:
  x
  y
  
def g:
  x; y

  |};
  [%expect
    {|
    (_{
     (((def f (: (_{ (((x) ";") ((y) ";") ((z))) _}))) "_;")
      ((def another (: (_{ (((x) "_;") ((y))) _}))) "_;")
      ((def g (: (_{ (((x) ";") ((y))) _})))))
     _})
    |}]
;;

let%expect_test "fib" =
  check
    {|
def fib pos_int
| fib(0): 1
| fib(1): 1
| fib(n nat, n bool): fib(n - 1) + fib(n - 2)

def another:
  (x, y, z)

    |};
  [%expect
    {|
    (_{
     (((def fib pos_int (| (_{ (((fib ("(" ((0)) ")") (: (_{ (((1))) _}))))) _}))
        (| (_{ (((fib ("(" ((1)) ")") (: (_{ (((1))) _}))))) _}))
        (|
         (_{
          (((fib ("(" ((n nat) ,) ((n bool)) ")")
             (: (_{ (((fib ("(" ((n - 1)) ")") + fib ("(" ((n - 2)) ")")))) _})))))
          _})))
       "_;")
      ((def another (: (_{ (((("(" ((x) ,) ((y) ,) ((z)) ")")))) _})))))
     _})
    |}]
;;

let%expect_test "weird unbalanced braces" =
  check
    {|
hello_world } another { awefawe )
    |};
  [%expect
    {|
    (_{ (((hello_world "(Error })" another ({ ((awefawe)) ")")))) _})
    ((Mismatching_delimiters (ldelim ((start 23) (stop 24)))
      (rdelim ((start 33) (stop 34)))))
    |}]
;;

let%expect_test "explicit blocks" =
  check
    {|
def first: {
  first;
  second; third;
  fourth
}
    |};
  [%expect
    {|
    (_{
     (((def first
        (: ({ (((first) ";") ((second) ";") ((third) ";") ((fourth))) })))))
     _})
    |}]
;;

let%expect_test "semi after alternatives" =
  check
    {|
data Option(a):
| Some(a)
| None
;

data Either(a, b)
| Left(a)
| Right(b)

record Pair(a, b):
  fst a
  snd b

    |};
  [%expect
    {|
    (_{
     (((data Option ("(" ((a)) ")") (: (_{ () _}))
        (| (_{ (((Some ("(" ((a)) ")")))) _})) (| (_{ (((None))) _})))
       "_;")
      ((data Either ("(" ((a) ,) ((b)) ")")
        (| (_{ (((Left ("(" ((a)) ")")))) _}))
        (| (_{ (((Right ("(" ((b)) ")")))) _})))
       "_;")
      ((record Pair ("(" ((a) ,) ((b)) ")")
        (: (_{ (((fst a) "_;") ((snd b))) _})))))
     _})
    |}]
;;

let%expect_test "nested match" =
  check
    {|
def nested_match(x, y):
  match x:
  | Some(x):
    match y:
    | Some(y): print(x, y)
    | None: throw()
  | None:
    throw()

  |};
  [%expect
    {|
    (_{
     (((def nested_match ("(" ((x) ,) ((y)) ")")
        (:
         (_{
          (((match x (: (_{ () _}))
             (|
              (_{
               (((Some ("(" ((x)) ")") (: (_{ () _}))) "_;")
                ((match y (: (_{ () _}))
                  (|
                   (_{
                    (((Some ("(" ((y)) ")")
                       (: (_{ (((print ("(" ((x) ,) ((y)) ")")))) _})))))
                    _}))
                  (| (_{ (((None (: (_{ (((throw ("(" ")")))) _}))))) _})))))
               _}))
             (| (_{ (((None (: (_{ () _}))) "_;") ((throw ("(" ")")))) _})))))
          _})))))
     _})
    |}]
;;

let%expect_test "equal sign" =
  check
    {|
    let first: fun:
      let x: 1324234
      let y: 12341324
    |};
  [%expect
    {|
    (_{
     (((let first (: (_{ (((fun (: (_{ () _}))))) _}))))
      ((let x (: (_{ (((1324234))) _})))) ((let y (: (_{ (((12341324))) _})))))
     _})
    |}]
;;

let%expect_test "comma in block" =
  check
    {|
let testing:
  first
  second
  ,
  third
  fourth
    |};
  [%expect
    {|
    (_{
     (((let testing
        (:
         (_{
          (((first) "_;") ((second) "_;") (("(Error ,)") "_;") ((third) "_;")
           ((fourth)))
          _})))))
     _})
    |}]
;;
