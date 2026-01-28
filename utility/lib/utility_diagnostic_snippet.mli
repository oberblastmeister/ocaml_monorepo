open Core
module File_span := Utility_file_span
module Position_converter := Utility_position_converter
module Pp := Utility_pp
module Doc = Pp.Doc

type t = File_span.t [@@deriving sexp_of]

module File : sig
  type t

  val create : string -> t
end

type files = File.t String.Map.t

val pp : files -> t -> Doc.t
