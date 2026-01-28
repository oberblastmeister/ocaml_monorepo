module Doc : sig
  type t [@@deriving sexp_of]

  val empty : t
  val newline : t
  val blank : int -> t
  val space : t
  val break0 : t
  val break1 : t
  val break : int -> t
  val choice : flat:t -> expanded:t -> t
  val when_flat : t -> t
  val when_expanded : t -> t
  val concat : t -> t -> t
  val indent : int -> t -> t
  val char : char -> t
  val string : string -> t
  val raw_char : char -> t
  val raw_string : string -> t
  val group : t -> t

  module Syntax : sig
    val ( ^^ ) : t -> t -> t
  end
end

module type Output = sig
  type t

  val write_string : t -> string -> unit
  val write_char : t -> char -> unit
  val write_blank : t -> int -> unit
end

module String_output : Output with type t = Buffer.t

module Make (Output : Output) : sig
  type state

  val render : ?ribbon:float -> width:int -> out:Output.t -> Doc.t -> unit
end

val render_to_string : ?buf_size:int -> ?ribbon:float -> width:int -> Doc.t -> string
