open Core
module File_span := Diagnostic_file_span
module Position_converter := Location.Position_converter

type t = File_span.t [@@deriving sexp_of]

module File : sig
  type t

  val create : string -> t
end

type files = File.t String.Map.t

val format_snippet : files -> t -> string