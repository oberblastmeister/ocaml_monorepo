module Line_col := Utility_line_col

type t [@@deriving sexp_of]

val create : string -> t

(* we can index at the end of the line *)
val pos_to_line_col : t -> int -> Line_col.t
val line_col_to_pos : t -> Line_col.t -> int
