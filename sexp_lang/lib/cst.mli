open O

type t =
  | Atom of
      { span : Span.t
      ; value : string
      }
  | Keyword of
      { span : Span.t
      ; value : string
      }
  | List of
      { span : Span.t
      ; items : t list
      }
[@@deriving equal, compare, sexp]

module Token : sig
  type t =
    | LParen
    | RParen
    | LBrack
    | RBrack
    | LBrace
    | RBrace
    | Atom of string
    | Keyword of string
    | Error of string
  [@@deriving sexp, compare, equal]
end

module SpannedToken : sig
  type t =
    { token : Token.t
    ; span : Span.t
    }
  [@@deriving sexp]
end

val tokenize : string -> SpannedToken.t list
val parse_tokens : SpannedToken.t list -> t list Or_error.t
val parse_tokens_single : SpannedToken.t list -> t Or_error.t
val parse : string -> t list Or_error.t
val parse_single : string -> t Or_error.t
val span : t -> Span.t
