open Core

module type Token = sig
  type t [@@deriving sexp_of, equal, compare]

  val equal : t -> t -> bool
end

module type Chunk = sig
  type t [@@deriving sexp_of, compare, equal]
end

module type Snapshot = sig
  type t [@@deriving sexp_of]
end

module type Stream = sig
  module Token : Token
  module Chunk : Chunk
  module Snapshot : Snapshot

  type t [@@deriving sexp_of]

  val next : t -> Token.t option
  val peek : t -> Token.t option
  val snapshot : t -> Snapshot.t
  val restore : t -> Snapshot.t -> unit
end

module type Arg = sig
  module Data : sig
    type t [@@deriving sexp_of]
  end

  module Error : sig
    type t [@@deriving sexp_of]
  end

  module Stream : Stream
end

module Parse_result = struct
  type ('a, 'e) t =
    | Ok of 'a
    | Error of 'e
    | Fail
  [@@deriving sexp_of]

  let to_result_exn = function
    | Ok x -> Result.Ok x
    | Error e -> Error e
    | _ -> failwith "parsec: uncaught failure"
  ;;
end

module type S = sig
  module Arg : Arg
  open Arg
  module Token := Stream.Token

  type env [@@deriving sexp_of]
  type 'a t = env -> 'a

  module Exceptions : sig
    exception Error of Error.t * Token.t option [@@deriving sexp_of]
    exception Fail [@@deriving sexp_of]
  end

  val sep : 'a t -> by:unit t -> 'a list t
  val sep1 : 'a t -> by:unit t -> 'a list t

  val with_env
    :  Data.t
    -> Stream.t
    -> (env -> 'a)
    -> ('a, Error.t * Token.t option) Parse_result.t

  val expect_eq : Token.t -> unit t
  val expect : (Token.t -> 'a option) -> 'a t
  val fail : env -> _
  val cut : Error.t -> 'a t -> 'a t
  val commit : Error.t -> env -> (unit -> 'a) -> 'a
  val stream : env -> Stream.t
  val error : env -> Error.t -> _
  val orelse : 'a t -> 'a t -> 'a t
  val choice : 'a t list -> 'a t
  val pure : 'a -> 'a t
  val optional : 'a t -> 'a option t
  val either : 'a t -> 'b t -> ('a, 'b) Either.t t
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val many : 'a t -> 'a list t
  val some : 'a t -> 'a list t
  val guard : bool -> unit t

  module Syntax : sig
    val ( <|> ) : 'a t -> 'a t -> 'a t
    val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
    val ( <$ ) : 'a -> 'b t -> 'a t
    val ( $> ) : 'b t -> 'a -> 'a t
    val ( <* ) : 'a t -> 'b t -> 'a t
    val ( *> ) : 'a t -> 'b t -> 'b t
  end
end

module type Intf = sig
  module type Token = Token
  module type Chunk = Chunk
  module type Snapshot = Snapshot
  module type Stream = Stream
  module type Arg = Arg

  module Parse_result = Parse_result

  module Make_stream (Token : Token) : sig
    include Stream with module Token = Token and type Chunk.t = Token.t array

    val create : Chunk.t -> t
  end

  module String_stream : sig
    include Stream with module Token = Char and module Chunk = String

    val create : string -> t
  end

  module String_array_stream :
    Stream with type Token.t = string and type Chunk.t = string array

  module Make (Arg : Arg) : S with module Arg = Arg
  module Arg_parse_arg : Arg

  module Arg_parse : sig
    include S with module Arg = Arg_parse_arg

    val parse
      :  string array
      -> (env -> 'a)
      -> ('a, Error.t * string option) Parse_result.t

    val flag1 : string -> unit t
    val flag2 : string -> unit t
    val flag_any : string -> unit t
    val opt1 : string -> string t
    val opt2 : string -> string t
    val opt_any : string -> string t
    val positional : string t
  end
end
