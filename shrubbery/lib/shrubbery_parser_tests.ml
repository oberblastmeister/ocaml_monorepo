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
  let _tts, root, errors = Parser.parse s in
  print_s (Syntax.Minimal_sexp_of.sexp_of_root root);
  if not (List.is_empty errors) then print_s [%sexp (errors : Delimit.Error.t list)];
  ()
;;

let%expect_test "basic" =
  check
    {|
fun first {
  x
  y
  z
}
    |};
  [%expect {| ((fun first ({ ((x) "_;") ((y) "_;") ((z)) }))) |}]
;;

let%expect_test "more" =
  check
    {|
def first = {
  call_function(
    {
      a
      b; c
      d
    },
    {
      x; y
      z
    },
    {
      a; b; c; d
    }
  )
}
    |};
  [%expect
    {|
    ((def first =
      ({
       ((call_function
         ("(" ((({ ((a) "_;") ((b) ";") ((c) "_;") ((d)) })) ,)
          ((({ ((x) ";") ((y) "_;") ((z)) })) ,)
          ((({ ((a) ";") ((b) ";") ((c) ";") ((d)) }))) ")")))
       })))
    |}]
;;

let%expect_test "weird comma" =
  check
    {|
df first {
  x; y; z
}
,
def another {
  x; y; z
}
    |};
  [%expect
    {|
    ((df first ({ ((x) ";") ((y) ";") ((z)) }) , def another
      ({ ((x) ";") ((y) ";") ((z)) })))
    |}]
;;

let%expect_test "fib" =
  check
    {|
func fib = match pos_int {
  0 -> 1
  1 -> 1
  n -> fib (n - 1) + fib (n - 2)
}
    |};
  [%expect
    {|
    ((func fib = match pos_int
      ({ ((0 (operator ->) 1) "_;") ((1 (operator ->) 1) "_;")
       ((n (operator ->) fib ("(" ((n (operator -) 1)) ")") (operator +) fib
         ("(" ((n (operator -) 2)) ")")))
       })))
    |}]
;;

let%expect_test "data types" =
  check
    {|
let Option [a] = data {
  Some a
  None
}

let Either [a] [b] = data {
  Left a
  Right b
}
    |};
  [%expect
    {|
    ((let Option ([ ((a)) ]) = data ({ ((Some a) "_;") ((None)) }) let Either
      ([ ((a)) ]) ([ ((b)) ]) = data ({ ((Left a) "_;") ((Right b)) })))
    |}]
;;

let%expect_test "nested match" =
  check
    {|
func nested_match x y = match x {
  Some x -> match y {
    Some y -> print x y
    None -> throw ()
  }
  None -> throw ()
}
    |};
  [%expect
    {|
    ((func nested_match x y = match x
      ({
       ((Some x (operator ->) match y
         ({ ((Some y (operator ->) print x y) "_;")
          ((None (operator ->) throw ("(" ")"))) }))
        "_;")
       ((None (operator ->) throw ("(" ")"))) })))
    |}]
;;
