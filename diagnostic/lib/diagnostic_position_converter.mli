type t [@@deriving sexp_of]

val create : string -> t
val pos_to_line_col : t -> int -> Diagnostic_line_col.t
val line_col_to_pos : t -> Diagnostic_line_col.t -> int