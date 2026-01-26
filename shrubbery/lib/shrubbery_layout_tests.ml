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
def first:
  x
  y
  z
    |};
  ();
  [%expect {| (_{ "_;" def first : _{ x "_;" y "_;" z _} _} _eof) |}]
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
    (_{ "_;" def first : _{ call_function
     ("(" do : _{ a "_;" b ";" c "_;" d _} , do : _{ x ";" y ";" "_;" z _} , do :
      _{ a ";" b ";" c ";" d _} ")")
     _} _} _eof)
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
    (_{ "_;" def first : _{ x ";" y ";" z _} "_;" (error ,) "_;" def another : _{
     x ";" y ";" z _} _} _eof)
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
    (_{ def f : _{ x ";" y ";" z _} "_;" def another : _{ x "_;" y _} "_;" def g
     : _{ x ";" y _} _} _eof)
    |}]
;;

let%expect_test "weird semi" =
  check
    {|
program:
  x
  ; y
  z
  ; ; ; ; ; w
    |};
  [%expect
    {|
    (_{ "_;" program : _{ x "_;" ";" y "_;" z "_;" ";" ";" ";" ";" ";" w _} _}
     _eof)
    |}]
;;

let%expect_test "weird empty blocks" =
  check
    {|
: : :
                 first
                 second
                 third
    |};
  [%expect {| (_{ "_;" : _{ : _{ : _{ first "_;" second "_;" third _} _} _} _} _eof) |}]
;;

let%expect_test "weird empty blocks find first token" =
  check
    {|
 : :    : 
        x
        y
        z
    |};
  [%expect {| (_{ : _{ : _{ : _{ _} "_;" x "_;" y "_;" z _} _} _} _eof) |}];
  check
    {|
: :    :
      x
      y
      z
    |};
  [%expect {| (_{ "_;" : _{ : _{ : _{ _} _} x y z _} _} _eof) |}]
;;

let%expect_test "start block with equal" =
  check
    {|
def first =
  x
  y
  z

def second = x; y
    |};
  [%expect
    {| (_{ "_;" def first = x y z "_;" def second = x ";" y _} _eof) |}]
;;

let%expect_test "unnecessary semicolons" =
  check
    {|
def first: 
  x;
  y;
  z
    |};
  [%expect {| (_{ "_;" def first : _{ x ";" "_;" y ";" "_;" z _} _} _eof) |}]
;;

let%expect_test "" =
  check
    {|
hello_world } another
|};
  [%expect {| (_{ "_;" hello_world (error }) another _} _eof) |}]
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
    (_{ let first : _{ fun : _{ _} _} let x : _{ 1324234 _} let y : _{ 12341324
     _} _} _eof)
    |}]
;;

let%expect_test "top level mod" =
  check
    {|
mod: {

let first = 1

let second = 2

let third = 3234

}
    |};
  [%expect
    {| (_{ "_;" mod : ({ let first = 1 let second = 2 let third = 3234 }) _} _eof) |}]
;;
