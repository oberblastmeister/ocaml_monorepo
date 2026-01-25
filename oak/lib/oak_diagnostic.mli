open Core
module Snippet = Diagnostic.Snippet

module Text : sig
  type t = Format.formatter -> unit [@@deriving sexp_of]

  val of_string : string -> t
  val to_string : width:int -> color:bool -> t -> string
end

module Severity : sig
  type t =
    | Warning
    | Error
    | Note
  [@@deriving sexp_of]

  val pp : Format.formatter -> t -> unit
  val to_string : color:bool -> t -> string
end

module Part : sig
  type t =
    { severity : Severity.t
    ; message : Text.t
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
  { code : Code.t
  ; filename : Filename.t
  ; parts : Part.t list
  }
[@@deriving sexp_of]

val format : ?width:int -> ?color:bool -> files:Snippet.files -> t -> string
val print : ?width:int -> ?color:bool -> files:Snippet.files -> t -> unit
