open Core
module Span = Diagnostic_span
module Spanned = Diagnostic_spanned
module Line_col = Diagnostic_line_col
module Snippet = Diagnostic_snippet

module Severity = struct
  type t =
    | Hint
    | Warning
    | Error
    | Bug
  [@@deriving sexp_of, equal, compare]
end

module type Code = sig
  type t [@@deriving sexp]

  val severity : t -> Severity.t
  val code : t -> string
end

module Text = struct
  type t = Format.formatter -> unit
end

module Spanned_text = struct
  type t = Text.t Spanned.t
end

module type S = sig
  module Code : Code

  type t [@@deriving sexp_of]

  val create
    :  ?severity:Severity.t
    -> ?loc:Span.t
    -> ?notes:Spanned_text.t list
    -> Code.t
    -> string
    -> t

  val render : filename:string -> source:string -> t list -> string
end

type 'a t = unit [@@deriving sexp_of]

module Make (Code : Code) : S with module Code = Code = struct
  module Code = Code

  type nonrec t = Code.t t [@@deriving sexp_of]

  let create ?severity ?loc ?notes code message = ()
  let render ~filename ~source diagnostics = ""
end
