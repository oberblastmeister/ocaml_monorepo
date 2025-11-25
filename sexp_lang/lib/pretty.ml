open O

type ann =
  | Line
  | IndentLine
[@@deriving equal, compare, sexp]

module Delim = struct
  type t =
    | Paren
    | Brack
    | Brace
  [@@deriving sexp, equal, compare, sexp]
end

type t =
  | List of t list * Delim.t
  | Atom of string
  | Keyword of string
  | Ann of ann
[@@deriving equal, compare, sexp]

let is_ann = function
  | Ann _ -> true
  | _ -> false
;;

let rec to_sexp t =
  match t with
  | List (ts, _d) ->
    let ts = List.filter ts ~f:(fun t -> not (is_ann t)) in
    Sexp.List (List.map ts ~f:to_sexp)
  | Atom s -> Sexp.Atom s
  | Keyword s -> Sexp.Atom ("#:" ^ s)
  | Ann _ -> Sexp.List []
;;

let atom x = Atom x
let list ?(delim = Delim.Paren) xs = List (xs, delim)
let brack_list xs = List (xs, Brack)
let brace_list xs = List (xs, Brace)

let char_of_open_delim = function
  | Delim.Paren -> '('
  | Brack -> '['
  | Brace -> '{'
;;

let char_of_close_delim = function
  | Delim.Paren -> ')'
  | Brack -> ']'
  | Brace -> '}'
;;

let indent_increase = 2

let add_newline buffer indent =
  Buffer.add_char buffer '\n';
  Buffer.add_string buffer (String.make indent ' ')
;;

let print_ann buffer indent = function
  | Line ->
    add_newline buffer indent;
    indent
  | IndentLine ->
    let indent = indent + indent_increase in
    add_newline buffer indent;
    indent
;;

let print_to_buffer sexp =
  let buffer = Buffer.create 10 in
  let rec go indent = function
    | Atom s -> Buffer.add_string buffer s
    | Keyword s -> Buffer.add_string buffer ("#:" ^ s)
    | List (items, delim) ->
      Buffer.add_char buffer (char_of_open_delim delim);
      go_list indent items;
      Buffer.add_char buffer (char_of_close_delim delim)
    | Ann _ -> ()
  and go_list indent = function
    | Ann ann :: xs ->
      let indent = print_ann buffer indent ann in
      go_list indent xs
    | x :: Ann ann :: xs ->
      go indent x;
      let indent = print_ann buffer indent ann in
      go_list indent xs
    | x :: [] -> go indent x
    | x :: xs ->
      go indent x;
      Buffer.add_char buffer ' ';
      go_list indent xs
    | [] -> ()
  in
  go 0 sexp;
  buffer
;;

let to_string sexp = print_to_buffer sexp |> Buffer.contents

let%expect_test _ =
  let sexp =
    list
      [ Atom "define"
      ; list [ Atom "fun"; Atom "b"; Atom "c" ]
      ; Ann IndentLine
      ; list [ Atom "let"; Atom "x" ]
      ; Ann Line
      ; list [ Atom "let"; Atom "y" ]
      ; Ann Line
      ; list [ Atom "label"; Atom "block"; Ann IndentLine; list [ Atom "stuff" ] ]
      ; Ann Line
      ; list [ Atom "label"; Atom "block2"; Ann IndentLine; list [ Atom "more_stuff" ] ]
      ]
  in
  to_string sexp |> print_endline;
  [%expect
    {|
    (define (fun b c)
      (let x)
      (let y)
      (label block
        (stuff))
      (label block2
        (more_stuff))) |}]
;;
