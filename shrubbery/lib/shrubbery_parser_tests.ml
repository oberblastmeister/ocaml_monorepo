open Prelude

open struct
  module Lexer = Shrubbery_lexer
  module Parser = Shrubbery_parser
  module Token = Shrubbery_token
  module Token_tree = Shrubbery_token_tree
  module Delimit = Shrubbery_delimit
  module Layout = Shrubbery_layout
  module Shrub = Shrubbery_shrub
end

let check ?(remove_trivia = true) s =
  let tokens = Lexer.lex s |> Array.of_list in
  let tts, errors = Delimit.delimit tokens in
  let tts = Layout.insert_virtual_tokens tokens (Token_tree.root_to_indexed tts) in
  let tts = if remove_trivia then Token_tree.remove_trivia_root tts else tts in
  let block = Parser.parse tts in
  print_s [%sexp (block : Shrub.block)];
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
    {|
    ((lbrace VLBrace)
     (groups
      (((group ((items ()) (block ()) (alts ()))) (sep (VSemi)))
       ((group
         ((items ((Token (Ident def)) (Token (Ident first))))
          (block
           (((token Colon)
             (block
              ((lbrace VLBrace)
               (groups
                (((group ((items ((Token (Ident x)))) (block ()) (alts ())))
                  (sep (VSemi)))
                 ((group ((items ((Token (Ident y)))) (block ()) (alts ())))
                  (sep (VSemi)))
                 ((group ((items ((Token (Ident z)))) (block ()) (alts ())))
                  (sep ()))))
               (rbrace VRBrace))))))
          (alts ())))
        (sep ()))))
     (rbrace VRBrace))
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
    ((lbrace VLBrace)
     (groups
      (((group ((items ()) (block ()) (alts ()))) (sep (VSemi)))
       ((group
         ((items ((Token (Ident def)) (Token (Ident first))))
          (block
           (((token Colon)
             (block
              ((lbrace VLBrace)
               (groups
                (((group
                   ((items
                     ((Token (Ident call_function))
                      (Tree (ldelim LParen)
                       (groups
                        (((group
                           ((items ((Token (Ident do))))
                            (block
                             (((token Colon)
                               (block
                                ((lbrace VLBrace)
                                 (groups
                                  (((group
                                     ((items ((Token (Ident a)))) (block ())
                                      (alts ())))
                                    (sep (VSemi)))
                                   ((group
                                     ((items ((Token (Ident b)))) (block ())
                                      (alts ())))
                                    (sep (Semi)))
                                   ((group
                                     ((items ((Token (Ident c)))) (block ())
                                      (alts ())))
                                    (sep (VSemi)))
                                   ((group
                                     ((items ((Token (Ident d)))) (block ())
                                      (alts ())))
                                    (sep ()))))
                                 (rbrace VRBrace))))))
                            (alts ())))
                          (sep (Comma)))
                         ((group
                           ((items ((Token (Ident do))))
                            (block
                             (((token Colon)
                               (block
                                ((lbrace VLBrace)
                                 (groups
                                  (((group
                                     ((items ((Token (Ident x)))) (block ())
                                      (alts ())))
                                    (sep (Semi)))
                                   ((group
                                     ((items ((Token (Ident y)))) (block ())
                                      (alts ())))
                                    (sep (Semi)))
                                   ((group ((items ()) (block ()) (alts ())))
                                    (sep (VSemi)))
                                   ((group
                                     ((items ((Token (Ident z)))) (block ())
                                      (alts ())))
                                    (sep ()))))
                                 (rbrace VRBrace))))))
                            (alts ())))
                          (sep (Comma)))
                         ((group
                           ((items ((Token (Ident do))))
                            (block
                             (((token Colon)
                               (block
                                ((lbrace VLBrace)
                                 (groups
                                  (((group
                                     ((items ((Token (Ident a)))) (block ())
                                      (alts ())))
                                    (sep (Semi)))
                                   ((group
                                     ((items ((Token (Ident b)))) (block ())
                                      (alts ())))
                                    (sep (Semi)))
                                   ((group
                                     ((items ((Token (Ident c)))) (block ())
                                      (alts ())))
                                    (sep (Semi)))
                                   ((group
                                     ((items ((Token (Ident d)))) (block ())
                                      (alts ())))
                                    (sep ()))))
                                 (rbrace VRBrace))))))
                            (alts ())))
                          (sep ()))))
                       (rdelim RParen))))
                    (block ()) (alts ())))
                  (sep ()))))
               (rbrace VRBrace))))))
          (alts ())))
        (sep ()))))
     (rbrace VRBrace))
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
    ((lbrace VLBrace)
     (groups
      (((group ((items ()) (block ()) (alts ()))) (sep (VSemi)))
       ((group
         ((items ((Token (Ident def)) (Token (Ident first))))
          (block
           (((token Colon)
             (block
              ((lbrace VLBrace)
               (groups
                (((group ((items ((Token (Ident x)))) (block ()) (alts ())))
                  (sep (Semi)))
                 ((group ((items ((Token (Ident y)))) (block ()) (alts ())))
                  (sep (Semi)))
                 ((group ((items ((Token (Ident z)))) (block ()) (alts ())))
                  (sep ()))))
               (rbrace VRBrace))))))
          (alts ())))
        (sep (VSemi)))
       ((group ((items ((Token (Error ,)))) (block ()) (alts ()))) (sep (VSemi)))
       ((group
         ((items ((Token (Ident def)) (Token (Ident another))))
          (block
           (((token Colon)
             (block
              ((lbrace VLBrace)
               (groups
                (((group ((items ((Token (Ident x)))) (block ()) (alts ())))
                  (sep (Semi)))
                 ((group ((items ((Token (Ident y)))) (block ()) (alts ())))
                  (sep (Semi)))
                 ((group ((items ((Token (Ident z)))) (block ()) (alts ())))
                  (sep ()))))
               (rbrace VRBrace))))))
          (alts ())))
        (sep ()))))
     (rbrace VRBrace))
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
    ((lbrace VLBrace)
     (groups
      (((group
         ((items ((Token (Ident def)) (Token (Ident f))))
          (block
           (((token Colon)
             (block
              ((lbrace VLBrace)
               (groups
                (((group ((items ((Token (Ident x)))) (block ()) (alts ())))
                  (sep (Semi)))
                 ((group ((items ((Token (Ident y)))) (block ()) (alts ())))
                  (sep (Semi)))
                 ((group ((items ((Token (Ident z)))) (block ()) (alts ())))
                  (sep ()))))
               (rbrace VRBrace))))))
          (alts ())))
        (sep ()))
       ((group
         ((items ((Token (Ident def)) (Token (Ident another))))
          (block
           (((token Colon)
             (block
              ((lbrace VLBrace)
               (groups
                (((group ((items ((Token (Ident x)))) (block ()) (alts ())))
                  (sep (VSemi)))
                 ((group ((items ((Token (Ident y)))) (block ()) (alts ())))
                  (sep ()))))
               (rbrace VRBrace))))))
          (alts ())))
        (sep ()))))
     (rbrace VRBrace))
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
    ((lbrace VLBrace)
     (groups
      (((group
         ((items ((Token (Ident def)) (Token (Ident f))))
          (block
           (((token Colon)
             (block
              ((lbrace VLBrace)
               (groups
                (((group ((items ((Token (Ident x)))) (block ()) (alts ())))
                  (sep (Semi)))
                 ((group ((items ((Token (Ident y)))) (block ()) (alts ())))
                  (sep (Semi)))
                 ((group ((items ((Token (Ident z)))) (block ()) (alts ())))
                  (sep ()))))
               (rbrace VRBrace))))))
          (alts ())))
        (sep (VSemi)))
       ((group
         ((items ((Token (Ident def)) (Token (Ident another))))
          (block
           (((token Colon)
             (block
              ((lbrace VLBrace)
               (groups
                (((group ((items ((Token (Ident x)))) (block ()) (alts ())))
                  (sep (VSemi)))
                 ((group ((items ((Token (Ident y)))) (block ()) (alts ())))
                  (sep ()))))
               (rbrace VRBrace))))))
          (alts ())))
        (sep (VSemi)))
       ((group
         ((items ((Token (Ident def)) (Token (Ident g))))
          (block
           (((token Colon)
             (block
              ((lbrace VLBrace)
               (groups
                (((group ((items ((Token (Ident x)))) (block ()) (alts ())))
                  (sep (Semi)))
                 ((group ((items ((Token (Ident y)))) (block ()) (alts ())))
                  (sep ()))))
               (rbrace VRBrace))))))
          (alts ())))
        (sep ()))))
     (rbrace VRBrace))
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
    ((lbrace VLBrace)
     (groups
      (((group ((items ()) (block ()) (alts ()))) (sep (VSemi)))
       ((group
         ((items
           ((Token (Ident def)) (Token (Ident fib)) (Token (Ident pos_int))))
          (block ())
          (alts
           (((pipe Pipe)
             (block
              ((lbrace VLBrace)
               (groups
                (((group
                   ((items
                     ((Token (Ident fib))
                      (Tree (ldelim LParen)
                       (groups
                        (((group
                           ((items ((Token (Number 0)))) (block ()) (alts ())))
                          (sep ()))))
                       (rdelim RParen))))
                    (block
                     (((token Colon)
                       (block
                        ((lbrace VLBrace)
                         (groups
                          (((group
                             ((items ((Token (Number 1)))) (block ()) (alts ())))
                            (sep ()))))
                         (rbrace VRBrace))))))
                    (alts ())))
                  (sep ()))))
               (rbrace VRBrace))))
            ((pipe Pipe)
             (block
              ((lbrace VLBrace)
               (groups
                (((group
                   ((items
                     ((Token (Ident fib))
                      (Tree (ldelim LParen)
                       (groups
                        (((group
                           ((items ((Token (Number 1)))) (block ()) (alts ())))
                          (sep ()))))
                       (rdelim RParen))))
                    (block
                     (((token Colon)
                       (block
                        ((lbrace VLBrace)
                         (groups
                          (((group
                             ((items ((Token (Number 1)))) (block ()) (alts ())))
                            (sep ()))))
                         (rbrace VRBrace))))))
                    (alts ())))
                  (sep ()))))
               (rbrace VRBrace))))
            ((pipe Pipe)
             (block
              ((lbrace VLBrace)
               (groups
                (((group
                   ((items
                     ((Token (Ident fib))
                      (Tree (ldelim LParen)
                       (groups
                        (((group
                           ((items ((Token (Ident n)) (Token (Ident nat))))
                            (block ()) (alts ())))
                          (sep (Comma)))
                         ((group
                           ((items ((Token (Ident n)) (Token (Ident bool))))
                            (block ()) (alts ())))
                          (sep ()))))
                       (rdelim RParen))))
                    (block
                     (((token Colon)
                       (block
                        ((lbrace VLBrace)
                         (groups
                          (((group
                             ((items
                               ((Token (Ident fib))
                                (Tree (ldelim LParen)
                                 (groups
                                  (((group
                                     ((items
                                       ((Token (Ident n)) (Token (Operator -))
                                        (Token (Number 1))))
                                      (block ()) (alts ())))
                                    (sep ()))))
                                 (rdelim RParen))
                                (Token (Operator +)) (Token (Ident fib))
                                (Tree (ldelim LParen)
                                 (groups
                                  (((group
                                     ((items
                                       ((Token (Ident n)) (Token (Operator -))
                                        (Token (Number 2))))
                                      (block ()) (alts ())))
                                    (sep ()))))
                                 (rdelim RParen))))
                              (block ()) (alts ())))
                            (sep ()))))
                         (rbrace VRBrace))))))
                    (alts ())))
                  (sep ()))))
               (rbrace VRBrace))))))))
        (sep (VSemi)))
       ((group
         ((items ((Token (Ident def)) (Token (Ident another))))
          (block
           (((token Colon)
             (block
              ((lbrace VLBrace)
               (groups
                (((group
                   ((items
                     ((Tree (ldelim LParen)
                       (groups
                        (((group
                           ((items ((Token (Ident x)))) (block ()) (alts ())))
                          (sep (Comma)))
                         ((group
                           ((items ((Token (Ident y)))) (block ()) (alts ())))
                          (sep (Comma)))
                         ((group
                           ((items ((Token (Ident z)))) (block ()) (alts ())))
                          (sep ()))))
                       (rdelim RParen))))
                    (block ()) (alts ())))
                  (sep ()))))
               (rbrace VRBrace))))))
          (alts ())))
        (sep ()))))
     (rbrace VRBrace))
    |}]
;;

let%expect_test "weird unbalanced braces" =
  check
    {|
hello_world } another { awefawe )
    |};
  [%expect
    {|
    ((lbrace VLBrace)
     (groups
      (((group ((items ()) (block ()) (alts ()))) (sep (VSemi)))
       ((group
         ((items
           ((Token (Ident hello_world)) (Token (Error })) (Token (Ident another))
            (Tree (ldelim LBrace)
             (groups
              (((group ((items ((Token (Ident awefawe)))) (block ()) (alts ())))
                (sep ()))))
             (rdelim RParen))))
          (block ()) (alts ())))
        (sep ()))))
     (rbrace VRBrace))
    ((Mismatching_delimiters (ldelim ((token LBrace) (index 7)))
      (rdelim ((token RParen) (index 11)))))
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
    ((lbrace VLBrace)
     (groups
      (((group ((items ()) (block ()) (alts ()))) (sep (VSemi)))
       ((group
         ((items ((Token (Ident def)) (Token (Ident first))))
          (block
           (((token Colon)
             (block
              ((lbrace LBrace)
               (groups
                (((group ((items ((Token (Ident first)))) (block ()) (alts ())))
                  (sep (Semi)))
                 ((group ((items ((Token (Ident second)))) (block ()) (alts ())))
                  (sep (Semi)))
                 ((group ((items ((Token (Ident third)))) (block ()) (alts ())))
                  (sep (Semi)))
                 ((group ((items ((Token (Ident fourth)))) (block ()) (alts ())))
                  (sep ()))))
               (rbrace RBrace))))))
          (alts ())))
        (sep ()))))
     (rbrace VRBrace))
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
    ((lbrace VLBrace)
     (groups
      (((group ((items ()) (block ()) (alts ()))) (sep (VSemi)))
       ((group
         ((items
           ((Token (Ident data)) (Token (Ident Option))
            (Tree (ldelim LParen)
             (groups
              (((group ((items ((Token (Ident a)))) (block ()) (alts ())))
                (sep ()))))
             (rdelim RParen))))
          (block
           (((token Colon)
             (block ((lbrace VLBrace) (groups ()) (rbrace VRBrace))))))
          (alts
           (((pipe Pipe)
             (block
              ((lbrace VLBrace)
               (groups
                (((group
                   ((items
                     ((Token (Ident Some))
                      (Tree (ldelim LParen)
                       (groups
                        (((group
                           ((items ((Token (Ident a)))) (block ()) (alts ())))
                          (sep ()))))
                       (rdelim RParen))))
                    (block ()) (alts ())))
                  (sep ()))))
               (rbrace VRBrace))))
            ((pipe Pipe)
             (block
              ((lbrace VLBrace)
               (groups
                (((group ((items ((Token (Ident None)))) (block ()) (alts ())))
                  (sep ()))))
               (rbrace VRBrace))))))))
        (sep (VSemi)))
       ((group ((items ()) (block ()) (alts ()))) (sep (Semi)))
       ((group ((items ()) (block ()) (alts ()))) (sep (VSemi)))
       ((group
         ((items
           ((Token (Ident data)) (Token (Ident Either))
            (Tree (ldelim LParen)
             (groups
              (((group ((items ((Token (Ident a)))) (block ()) (alts ())))
                (sep (Comma)))
               ((group ((items ((Token (Ident b)))) (block ()) (alts ())))
                (sep ()))))
             (rdelim RParen))))
          (block ())
          (alts
           (((pipe Pipe)
             (block
              ((lbrace VLBrace)
               (groups
                (((group
                   ((items
                     ((Token (Ident Left))
                      (Tree (ldelim LParen)
                       (groups
                        (((group
                           ((items ((Token (Ident a)))) (block ()) (alts ())))
                          (sep ()))))
                       (rdelim RParen))))
                    (block ()) (alts ())))
                  (sep ()))))
               (rbrace VRBrace))))
            ((pipe Pipe)
             (block
              ((lbrace VLBrace)
               (groups
                (((group
                   ((items
                     ((Token (Ident Right))
                      (Tree (ldelim LParen)
                       (groups
                        (((group
                           ((items ((Token (Ident b)))) (block ()) (alts ())))
                          (sep ()))))
                       (rdelim RParen))))
                    (block ()) (alts ())))
                  (sep ()))))
               (rbrace VRBrace))))))))
        (sep (VSemi)))
       ((group
         ((items
           ((Token (Ident record)) (Token (Ident Pair))
            (Tree (ldelim LParen)
             (groups
              (((group ((items ((Token (Ident a)))) (block ()) (alts ())))
                (sep (Comma)))
               ((group ((items ((Token (Ident b)))) (block ()) (alts ())))
                (sep ()))))
             (rdelim RParen))))
          (block
           (((token Colon)
             (block
              ((lbrace VLBrace)
               (groups
                (((group
                   ((items ((Token (Ident fst)) (Token (Ident a)))) (block ())
                    (alts ())))
                  (sep (VSemi)))
                 ((group
                   ((items ((Token (Ident snd)) (Token (Ident b)))) (block ())
                    (alts ())))
                  (sep ()))))
               (rbrace VRBrace))))))
          (alts ())))
        (sep ()))))
     (rbrace VRBrace))
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
    ((lbrace VLBrace)
     (groups
      (((group ((items ()) (block ()) (alts ()))) (sep (VSemi)))
       ((group
         ((items
           ((Token (Ident def)) (Token (Ident nested_match))
            (Tree (ldelim LParen)
             (groups
              (((group ((items ((Token (Ident x)))) (block ()) (alts ())))
                (sep (Comma)))
               ((group ((items ((Token (Ident y)))) (block ()) (alts ())))
                (sep ()))))
             (rdelim RParen))))
          (block
           (((token Colon)
             (block
              ((lbrace VLBrace)
               (groups
                (((group
                   ((items ((Token (Ident match)) (Token (Ident x))))
                    (block
                     (((token Colon)
                       (block ((lbrace VLBrace) (groups ()) (rbrace VRBrace))))))
                    (alts
                     (((pipe Pipe)
                       (block
                        ((lbrace VLBrace)
                         (groups
                          (((group
                             ((items
                               ((Token (Ident Some))
                                (Tree (ldelim LParen)
                                 (groups
                                  (((group
                                     ((items ((Token (Ident x)))) (block ())
                                      (alts ())))
                                    (sep ()))))
                                 (rdelim RParen))))
                              (block
                               (((token Colon)
                                 (block
                                  ((lbrace VLBrace) (groups ()) (rbrace VRBrace))))))
                              (alts ())))
                            (sep (VSemi)))
                           ((group
                             ((items ((Token (Ident match)) (Token (Ident y))))
                              (block
                               (((token Colon)
                                 (block
                                  ((lbrace VLBrace) (groups ()) (rbrace VRBrace))))))
                              (alts
                               (((pipe Pipe)
                                 (block
                                  ((lbrace VLBrace)
                                   (groups
                                    (((group
                                       ((items
                                         ((Token (Ident Some))
                                          (Tree (ldelim LParen)
                                           (groups
                                            (((group
                                               ((items ((Token (Ident y))))
                                                (block ()) (alts ())))
                                              (sep ()))))
                                           (rdelim RParen))))
                                        (block
                                         (((token Colon)
                                           (block
                                            ((lbrace VLBrace)
                                             (groups
                                              (((group
                                                 ((items
                                                   ((Token (Ident print))
                                                    (Tree (ldelim LParen)
                                                     (groups
                                                      (((group
                                                         ((items
                                                           ((Token (Ident x))))
                                                          (block ()) (alts ())))
                                                        (sep (Comma)))
                                                       ((group
                                                         ((items
                                                           ((Token (Ident y))))
                                                          (block ()) (alts ())))
                                                        (sep ()))))
                                                     (rdelim RParen))))
                                                  (block ()) (alts ())))
                                                (sep ()))))
                                             (rbrace VRBrace))))))
                                        (alts ())))
                                      (sep ()))))
                                   (rbrace VRBrace))))
                                ((pipe Pipe)
                                 (block
                                  ((lbrace VLBrace)
                                   (groups
                                    (((group
                                       ((items ((Token (Ident None))))
                                        (block
                                         (((token Colon)
                                           (block
                                            ((lbrace VLBrace)
                                             (groups
                                              (((group
                                                 ((items
                                                   ((Token (Ident throw))
                                                    (Tree (ldelim LParen)
                                                     (groups ()) (rdelim RParen))))
                                                  (block ()) (alts ())))
                                                (sep ()))))
                                             (rbrace VRBrace))))))
                                        (alts ())))
                                      (sep ()))))
                                   (rbrace VRBrace))))))))
                            (sep ()))))
                         (rbrace VRBrace))))
                      ((pipe Pipe)
                       (block
                        ((lbrace VLBrace)
                         (groups
                          (((group
                             ((items ((Token (Ident None))))
                              (block
                               (((token Colon)
                                 (block
                                  ((lbrace VLBrace) (groups ()) (rbrace VRBrace))))))
                              (alts ())))
                            (sep (VSemi)))
                           ((group
                             ((items
                               ((Token (Ident throw))
                                (Tree (ldelim LParen) (groups ())
                                 (rdelim RParen))))
                              (block ()) (alts ())))
                            (sep ()))))
                         (rbrace VRBrace))))))))
                  (sep ()))))
               (rbrace VRBrace))))))
          (alts ())))
        (sep ()))))
     (rbrace VRBrace))
    |}]
;;
