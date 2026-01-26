open Core
module Snippet = Diagnostic.Snippet

module Text : sig
  type t = Format.formatter -> unit [@@deriving sexp_of]

  val of_string : string -> t
end

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
  { code : Code.t option
  ; parts : Part.t list
  }
[@@deriving sexp_of]

val format : ?width:int -> ?color:bool -> files:Snippet.files -> t -> string
val print : ?width:int -> ?color:bool -> files:Snippet.files -> t -> unit
