open Prelude
module Token = Shrubbery_token

let rec take_while ~f i s =
  if i >= String.length s || not (f s.[i]) then i else take_while ~f (i + 1) s
;;

let is_operator_char c =
  match c with
  | '$' | '&' | '*' | '+' | '-' | '/' | '=' | '>' | '<' | '@' | '^' | '|' | ':' -> true
  | _ -> false
;;

let is_ident_start c = Char.is_alpha c || Char.equal c '_' || Char.equal c '#'

let is_ident_continue c =
  is_ident_start c || Char.is_digit c || Char.equal c '\'' || Char.equal c '?'
;;

let is_number_start c = Char.is_digit c

let is_number_continue c =
  is_number_start c || Char.equal c '_' || Char.is_alpha c || Char.equal c '.'
;;

let lex s =
  let rec lex (acc : Token.t list) (i : int) : Token.t list =
    if i >= String.length s
    then finish acc
    else begin
      match s.[i] with
      | '(' -> lex (Token.LParen :: acc) (i + 1)
      | ')' -> lex (Token.RParen :: acc) (i + 1)
      | '[' -> lex (Token.LBrack :: acc) (i + 1)
      | ']' -> lex (Token.RBrack :: acc) (i + 1)
      | '{' -> lex (Token.LBrace :: acc) (i + 1)
      | '}' -> lex (Token.RBrace :: acc) (i + 1)
      | ',' -> lex (Token.Comma :: acc) (i + 1)
      | ':' when i + 1 >= String.length s || not (is_operator_char s.[i + 1]) ->
        lex (Token.Colon :: acc) (i + 1)
      | ';' -> lex (Token.Semi :: acc) (i + 1)
      | '.' -> lex (Token.Dot :: acc) (i + 1)
      | '"' -> lex_string acc i (i + 1)
      | '=' when i + 1 >= String.length s || not (is_operator_char s.[i + 1]) ->
        lex (Token.Equal :: acc) (i + 1)
      | '|' when i + 1 >= String.length s || not (is_operator_char s.[i + 1]) ->
        lex (Token.Pipe :: acc) (i + 1)
      | ' ' -> lex_whitespace acc i (i + 1)
      | '\n' -> lex (Token.Newline :: acc) (i + 1)
      | '/' -> lex_comment acc i (i + 1)
      | '~' -> lex_keyword acc i (i + 1)
      | _ when is_number_start s.[i] -> lex_number acc i (i + 1)
      | _ when is_ident_start s.[i] -> lex_ident acc i (i + 1)
      | _ when is_operator_char s.[i] -> lex_operator acc i (i + 1)
      | c -> lex (Token.Error (String.of_char c) :: acc) (i + 1)
    end
  and lex_number acc start i =
    let i = take_while ~f:is_number_continue i s in
    lex (Token.Number (String.sub s ~pos:start ~len:(i - start)) :: acc) i
  and lex_string acc start i =
    let i = take_while ~f:(fun c -> not (Char.equal c '"' || Char.equal c '\n')) i s in
    let content = String.sub s ~pos:(start + 1) ~len:(i - (start + 1)) in
    if i >= String.length s || Char.equal s.[i] '\n'
    then lex (Error content :: acc) i
    else lex (String content :: acc) (i + 1)
  and lex_ident acc start i =
    let i = take_while ~f:is_ident_continue i s in
    lex (Token.Ident (String.sub s ~pos:start ~len:(i - start)) :: acc) i
  and lex_keyword acc start i =
    if i >= String.length s || not (is_ident_start s.[i])
    then lex (Token.Error (String.of_char s.[i]) :: acc) i
    else begin
      let i = i + 1 in
      let i = take_while ~f:is_ident_continue i s in
      lex (Token.Keyword (String.sub s ~pos:(start + 1) ~len:(i - (start + 1))) :: acc) i
    end
  and lex_operator acc start i =
    let i = take_while ~f:is_operator_char i s in
    lex (Token.Operator (String.sub s ~pos:start ~len:(i - start)) :: acc) i
  and lex_comment acc start i =
    if i >= String.length s || not (Char.equal s.[i] '/')
    then lex_operator acc start i
    else begin
      let i = i + 1 in
      let i = take_while ~f:(fun c -> not (Char.equal c '\n')) i s in
      lex (Token.Comment (String.sub s ~pos:(start + 2) ~len:(i - (start + 2))) :: acc) i
    end
  and lex_whitespace acc start i =
    let i = take_while ~f:(Char.equal ' ') i s in
    lex (Token.Whitespace (i - start) :: acc) i
  and finish acc = List.rev (Token.Veof :: acc) in
  lex [] 0
;;

let check s =
  let res = lex s in
  print_s [%sexp (res : Token.t list)]
;;

let%expect_test "smoke" =
  check
    {|
+ / + == awefpoiu'aewf? "aewf"first.second call_function(arg1, arg2) ~first ~second ~else
// awefaewfaewfawef
1.23__1.3_4.3
// another
    |};
  [%expect
    {|
    ("\n" (operator +) " " (operator /) " " (operator +) " " (operator ==) " "
     awefpoiu'aewf? " " "\"aewf\"" first . second " " call_function "(" arg1 ,
     " " arg2 ")" " " ~first " " ~second " " ~else "\n" "//  awefaewfaewfawef"
     "\n" 1.23__1.3_4.3 "\n" "//  another" "\n" "    " _eof)
    |}]
;;

let%expect_test "operator" =
  check
    {|
13242 :: Int
    |};
  [%expect {| ("\n" 13242 " " (operator ::) " " Int "\n" "    " _eof) |}]
;;

let%expect_test "underscore" =
  check
    {|
    _
    |};
  [%expect {| ("\n" "    " _ "\n" "    " _eof) |}]
;;

let%expect_test "waefaewf" =
  check
    {|
#t #f
    |};
  [%expect {| ("\n" #t " " #f "\n" "    " _eof) |}]
;;
