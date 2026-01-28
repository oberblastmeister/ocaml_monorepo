open Core

module Basic_color : sig
  type t =
    | Black
    | Blue
    | Cyan
    | Green
    | Magenta
    | Red
    | White
    | Yellow
  [@@deriving sexp_of]
end

module Color : sig
  type t [@@deriving sexp_of]

  val basic : ?bright:bool -> Basic_color.t -> t
  val rgb : r:int -> g:int -> b:int -> t
end

module Style : sig
  type t [@@deriving sexp_of]

  val empty : t
  val append : t -> t -> t
  val fg : Color.t -> t
  val bg : Color.t -> t
  val bold : t
  val italic : t
end

module Doc : sig
  type t [@@deriving sexp_of]

  val empty : t
  val newline : t
  val blank : int -> t
  val blank1 : t
  val space : t
  val break0 : t
  val break1 : t
  val break : int -> t
  val choice : flat:t -> expanded:t -> t
  val when_flat : t -> t
  val when_expanded : t -> t
  val append : t -> t -> t
  val indent : int -> t -> t
  val char : char -> t
  val string : string -> t
  val raw_char : char -> t
  val raw_string : string -> t
  val style : Style.t -> t -> t
  val group : t -> t
  val concat : ?sep:t -> t list -> t
  val force_expand : t

  module Syntax : sig
    val ( ^^ ) : t -> t -> t
  end
end

module type Output = sig
  type t

  val write_string : t -> string -> unit
  val write_char : t -> char -> unit
  val write_blank : t -> int -> unit
  val push_style : t -> Style.t -> unit
  val pop_style : t -> unit
end

module type Renderer = sig
  type output

  val render : ?ribbon:float -> width:int -> out:output -> Doc.t -> unit
end

module Make (Output : Output) : Renderer with type output = Output.t

val render_to_string
  :  ?buf_size:int
  -> ?ribbon:float
  -> ?color:bool
  -> width:int
  -> Doc.t
  -> string

val render_to_channel
  :  ?ribbon:float
  -> ?color:bool
  -> width:int
  -> out:out_channel
  -> Doc.t
  -> unit

val render_to_stdout : ?ribbon:float -> ?color:bool -> width:int -> Doc.t -> unit
