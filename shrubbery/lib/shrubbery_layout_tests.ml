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
  [%expect
    {|
    ((Token _{) (Token "_;") (Token def) (Token first) (Token :) (Token _{)
     (Token x) (Token "_;") (Token y) (Token "_;") (Token z) (Token _})
     (Token _}) (Token _eof))
    |}]
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
    ((Token _{) (Token "_;") (Token def) (Token first) (Token :) (Token _{)
     (Token call_function)
     (Delim (ldelim "(")
      (tts
       ((Token do) (Token :) (Token _{) (Token a) (Token "_;") (Token b)
        (Token ";") (Token c) (Token "_;") (Token d) (Token _}) (Token ,)
        (Token do) (Token :) (Token _{) (Token x) (Token ";") (Token y)
        (Token ";") (Token "_;") (Token z) (Token _}) (Token ,) (Token do)
        (Token :) (Token _{) (Token a) (Token ";") (Token b) (Token ";")
        (Token c) (Token ";") (Token d) (Token _})))
      (rdelim ")"))
     (Token _}) (Token _}) (Token _eof))
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
    ((Token _{) (Token "_;") (Token def) (Token first) (Token :) (Token _{)
     (Token x) (Token ";") (Token y) (Token ";") (Token z) (Token _})
     (Token "_;") (Token ,) (Token "_;") (Token def) (Token another) (Token :)
     (Token _{) (Token x) (Token ";") (Token y) (Token ";") (Token z) (Token _})
     (Token _}) (Token _eof))
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
    ((Token _{) (Token def) (Token f) (Token :) (Token _{) (Token x) (Token ";")
     (Token y) (Token ";") (Token z) (Token _}) (Token "_;") (Token def)
     (Token another) (Token :) (Token _{) (Token x) (Token "_;") (Token y)
     (Token _}) (Token "_;") (Token def) (Token g) (Token :) (Token _{) (Token x)
     (Token ";") (Token y) (Token _}) (Token _}) (Token _eof))
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
    ((Token _{) (Token "_;") (Token program) (Token :) (Token _{) (Token x)
     (Token "_;") (Token ";") (Token y) (Token "_;") (Token z) (Token "_;")
     (Token ";") (Token ";") (Token ";") (Token ";") (Token ";") (Token w)
     (Token _}) (Token _}) (Token _eof))
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
  [%expect
    {|
    ((Token _{) (Token "_;") (Token :) (Token _{) (Token :) (Token _{) (Token :)
     (Token _{) (Token first) (Token "_;") (Token second) (Token "_;")
     (Token third) (Token _}) (Token _}) (Token _}) (Token _}) (Token _eof))
    |}]
;;

let%expect_test "weird empty blocks find first token" =
  check
    {|
 : :    : 
        x
        y
        z
    |};
  [%expect
    {|
    ((Token _{) (Token :) (Token _{) (Token :) (Token _{) (Token :) (Token _{)
     (Token _}) (Token "_;") (Token x) (Token "_;") (Token y) (Token "_;")
     (Token z) (Token _}) (Token _}) (Token _}) (Token _eof))
    |}];
  check
    {|
: :    :
      x
      y
      z
    |};
  [%expect
    {|
    ((Token _{) (Token "_;") (Token :) (Token _{) (Token :) (Token _{) (Token :)
     (Token _{) (Token _}) (Token _}) (Token x) (Token y) (Token z) (Token _})
     (Token _}) (Token _eof))
    |}]
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
    {|
    ((Token _{) (Token "_;") (Token def) (Token first) (Token =) (Token _{)
     (Token x) (Token "_;") (Token y) (Token "_;") (Token z) (Token _})
     (Token "_;") (Token def) (Token second) (Token =) (Token _{) (Token x)
     (Token ";") (Token y) (Token _}) (Token _}) (Token _eof))
    |}]
;;

let%expect_test "unnecessary semicolons" =
  check
    {|
def first: 
  x;
  y;
  z
    |};
  [%expect
    {|
    ((Token _{) (Token "_;") (Token def) (Token first) (Token :) (Token _{)
     (Token x) (Token ";") (Token "_;") (Token y) (Token ";") (Token "_;")
     (Token z) (Token _}) (Token _}) (Token _eof))
    |}]
;;

let%expect_test "" =
  check
    {|
hello_world } another
|};
  [%expect
    {|
    ((Token _{) (Token "_;") (Token hello_world) (Token }) (Token another)
     (Token _}) (Token _eof))
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
    ((Token _{) (Token let) (Token first) (Token :) (Token _{) (Token fun)
     (Token :) (Token _{) (Token _}) (Token _}) (Token let) (Token x) (Token :)
     (Token _{) (Token 1324234) (Token _}) (Token let) (Token y) (Token :)
     (Token _{) (Token 12341324) (Token _}) (Token _}) (Token _eof))
    |}]
;;
