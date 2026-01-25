open Prelude

open struct
  module Line_col = Shrubbery_line_col
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

let to_string_gen ~vsemi ~vlbrace ~vrbrace ~veof = function
  | VSemi -> vsemi
  | VLBrace -> vlbrace
  | VRBrace -> vrbrace
  | Veof -> veof
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

let to_string_hide_virtual t = to_string_gen ~vsemi:"" ~vlbrace:"" ~vrbrace:"" ~veof:"" t

let to_string_show_show_virtual t =
  to_string_gen ~vsemi:"_;" ~vlbrace:"_{" ~vrbrace:"_}" ~veof:"_eof" t
;;

let to_string t = to_string_hide_virtual t
let sexp_of_t t = Sexp.Atom (to_string_show_show_virtual t)

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
    Array.init (Array.length tokens) ~f:(fun i ->
      let res = !curr_offset in
      Ref.replace curr_offset (( + ) (length tokens.(i)));
      res)
  in
  offsets
;;

let calculate_line_col tokens =
  let curr_line_col = ref { Line_col.line = 0; col = 0 } in
  let line_cols =
    Array.init (Array.length tokens) ~f:(fun i ->
      let res = !curr_line_col in
      Ref.replace curr_line_col (advance_line_col tokens.(i));
      res)
  in
  line_cols
;;
