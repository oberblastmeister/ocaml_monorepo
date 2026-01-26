module Line_col := Location_line_col

type t [@@deriving sexp_of]

val create : string -> t
val pos_to_line_col : t -> int -> Line_col.t
val line_col_to_pos : t -> Line_col.t -> int