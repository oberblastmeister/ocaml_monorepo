open Prelude

open struct
  module Line_col = Location.Line_col
end

type t =
  | LParen
  | RParen
  | LBrace
  | RBrace
  | LBrack
  | RBrack
  | Comma
  | Colon
  | Semi
  (* these are inserted by the layout calculator and never appear in the source code *)
  | VSemi
  | VLBrace
  | VRBrace
  | Equal
  | Pipe
  | Dot
  | String of string
  | Number of string
  | Operator of string
  | Comment of string
  | Whitespace of int
  | Newline
  | Ident of string
  | Keyword of string
  | Error of string
  | Veof
[@@deriving equal, compare]

let ident_val = function
  | Ident s -> Some s
  | _ -> None
;;

let number_val = function
  | Number s -> Some s
  | _ -> None
;;

let string_val = function
  | String s -> Some s
  | _ -> None
;;

let operator_val = function
  | Operator s -> Some s
  | _ -> None
;;

let is_trivia = function
  | Comment _ | Whitespace _ | Newline -> true
  | _ -> false
;;

let length = function
  | VSemi | VLBrace | VRBrace | Veof -> 0
  | LParen
  | RParen
  | LBrace
  | RBrace
  | LBrack
  | RBrack
  | Comma
  | Colon
  | Semi
  | Equal
  | Pipe
  | Dot
  | Newline -> 1
  | Operator s -> String.length s
  | Comment s -> 2 + String.length s
  | Whitespace n -> n
  | Ident s -> String.length s
  | Keyword s -> 1 + String.length s
  | String s -> 2 + String.length s
  | Number s -> String.length s
  | Error s -> String.length s
;;

let to_string = function
  | VSemi -> ""
  | VLBrace -> ""
  | VRBrace -> ""
  | Veof -> ""
  | LParen -> "("
  | RParen -> ")"
  | LBrace -> "{"
  | RBrace -> "}"
  | LBrack -> "["
  | RBrack -> "]"
  | Comma -> ","
  | Colon -> ":"
  | Semi -> ";"
  | Equal -> "="
  | Pipe -> "|"
  | Dot -> "."
  | Newline -> "\n"
  | Operator s -> s
  | Comment s -> "// " ^ s
  | Whitespace n -> String.make n ' '
  | Ident s -> s
  | Keyword s -> "~" ^ s
  | String s -> "\"" ^ s ^ "\""
  | Number s -> s
  | Error s -> s
;;

let sexp_of_t = function
  | VSemi -> Sexp.Atom "_;"
  | VLBrace -> Sexp.Atom "_{"
  | VRBrace -> Sexp.Atom "_}"
  | Veof -> Sexp.Atom "_eof"
  | LParen -> Sexp.Atom "("
  | RParen -> Sexp.Atom ")"
  | LBrace -> Sexp.Atom "{"
  | RBrace -> Sexp.Atom "}"
  | LBrack -> Sexp.Atom "["
  | RBrack -> Sexp.Atom "]"
  | Comma -> Sexp.Atom ","
  | Colon -> Sexp.Atom ":"
  | Semi -> Sexp.Atom ";"
  | Equal -> Sexp.Atom "="
  | Pipe -> Sexp.Atom "|"
  | Dot -> Sexp.Atom "."
  | Newline -> Sexp.Atom "\n"
  | Operator s -> Sexp.List [ Atom "operator"; Atom s ]
  | Comment s -> Sexp.Atom ("// " ^ s)
  | Whitespace n -> Sexp.Atom (String.make n ' ')
  | Ident s -> Sexp.Atom s
  | Keyword s -> Sexp.Atom ("~" ^ s)
  | String s -> Sexp.Atom ("\"" ^ s ^ "\"")
  | Number s -> Sexp.Atom s
  | Error s -> Sexp.List [ Atom "error"; Atom s ]
;;

type ti =
  { token : t
  ; index : int
  }
[@@deriving equal, compare]

let sexp_of_ti { token; index } = Sexp.List [ sexp_of_t token; Int.sexp_of_t index ]
let to_indexed (ts : t list) = List.mapi ts ~f:(fun index token -> { token; index })

let advance_line_col token (line_col : Line_col.t) =
  match token with
  | Newline -> { Line_col.line = line_col.line + 1; col = 0 }
  | _ -> { line_col with col = line_col.col + length token }
;;

let calculate_offsets tokens =
  let curr_offset = ref 0 in
  let offsets =
    Array.init
      (Array.length tokens + 1)
      ~f:(fun i ->
        let res = !curr_offset in
        if i <> Array.length tokens then curr_offset := !curr_offset + length tokens.(i);
        res)
  in
  offsets
;;

let calculate_line_col tokens =
  let curr_line_col = ref { Line_col.line = 0; col = 0 } in
  let line_cols =
    Array.init
      (Array.length tokens + 1)
      ~f:(fun i ->
        let res = !curr_line_col in
        if i <> Array.length tokens
        then Ref.replace curr_line_col (advance_line_col tokens.(i));
        res)
  in
  line_cols
;;

let calculate_next_non_trivia tokens =
  let len = Array.length tokens in
  let result = Array.create ~len len in
  let next_non_trivia = ref len in
  for i = len - 1 downto 0 do
    if not (is_trivia tokens.(i)) then next_non_trivia := i;
    result.(i) <- !next_non_trivia
  done;
  result
;;
