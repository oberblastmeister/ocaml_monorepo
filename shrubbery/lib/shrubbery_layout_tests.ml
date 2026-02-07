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

let check ?(remove_trivia = true) s =
  let tokens = Lexer.lex s |> Array.of_list in
  let tts, errors = Delimit.delimit tokens in
  let tts = Layout.insert_virtual_tokens tokens (Token_tree.Root.to_indexed tts) in
  let tts = if remove_trivia then Token_tree.Root.remove_trivia tts else tts in
  print_s [%sexp (tts : Token_tree.t list)];
  if not (List.is_empty errors) then print_s [%sexp (errors : Delimit.Error.t list)];
  ()
;;

let%expect_test "basic" =
  check
    {|
def first = {
  x
  y
  z
}
    |};
  [%expect {| (def first = ({ x "_;" y "_;" z }) _eof) |}]
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
    (def first =
     ({ call_function
      ("(" ({ a "_;" b ";" c "_;" d }) , ({ x ";" y "_;" z }) ,
       ({ a ";" b ";" c ";" d }) ")")
      })
     _eof)
    |}]
;;

let%expect_test "single block" =
  check
    {|
   {
     x; y
     z
   },
    |};
  [%expect {| (({ x ";" y "_;" z }) , _eof) |}]
;;

let%expect_test "comma on toplevel" =
  check
    {|
def first {
  x; y; z
}
,
def another {
  x; y; z
}
    |};
  [%expect {| (def first ({ x ";" y ";" z }) , def another ({ x ";" y ";" z }) _eof) |}]
;;

let%expect_test "weird indentation" =
  check
    {|
mod {
  let first = 123434
 let second = 1324
 let third = 1234
}
      |};
  [%expect
    {|
    (mod ({ let first = 123434 "_;" let second = 1324 "_;" let third = 1234 })
     _eof)
    |}]
;;

let%expect_test "weird semi" =
  check
    {|
program {
  x
  ; y
  z
  ; ; ; b ; ; ; w
  a
}
    |};
  [%expect
    {|
    (program ({ x "_;" ";" y "_;" z "_;" ";" ";" ";" b ";" ";" ";" w "_;" a })
     _eof)
    |}]
;;

let%expect_test "nested blocks" =
  check
    {|
{ { { { { { { {
                       first
                       second
                       third
}}} }} }}}
    |};
  [%expect
    {|
    (({ ({ ({ ({ ({ ({ ({ ({ first "_;" second "_;" third }) }) }) }) }) }) }) })
     _eof)
    |}]
;;

let%expect_test "nested blocks" =
  check
    {|
let w = {
  let z = {
    134
  }
  { 133 }
}
    |};
  [%expect {| (let w = ({ let z = ({ 134 }) "_;" ({ 133 }) }) _eof) |}]
;;
