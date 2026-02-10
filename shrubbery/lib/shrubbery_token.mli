open Prelude
module Line_col := Utility.Line_col

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
[@@deriving sexp_of, equal, compare]

val ident_val : t -> string option
val number_val : t -> string option
val string_val : t -> string option
val operator_val : t -> string option
val to_string : t -> string
val length : t -> int
val is_trivia : t -> bool

type ti =
  { token : t
  ; index : int
  }
[@@deriving sexp_of, equal, compare]

val unindex : ti -> t
val to_indexed : t list -> ti list
val advance_line_col : t -> Line_col.t -> Line_col.t
val calculate_offsets : t array -> int array
val calculate_line_col : t array -> Line_col.t array
val calculate_next_non_trivia : t array -> int array
