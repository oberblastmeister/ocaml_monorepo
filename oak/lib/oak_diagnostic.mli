open Core
module Pp := Utility.Pp
module Doc = Pp.Doc
module Snippet = Utility.Diagnostic.Snippet

module Kind : sig
  type t =
    | Warning
    | Error
    | Note
    | Help
  [@@deriving sexp_of]
end

module Part : sig
  type t =
    { kind : Kind.t
    ; message : Doc.t
    ; snippet : Snippet.t option
    }
  [@@deriving sexp_of]
end

module Code : sig
  type t = Parse_error [@@deriving sexp_of]

  val to_string : t -> string
  val description : t -> string
end

type t =
  { code : Code.t option
  ; parts : Part.t list
  }
[@@deriving sexp_of]

val pp : files:Snippet.files -> t -> Doc.t
val print : ?width:int -> ?color:bool -> files:Snippet.files -> t -> unit
