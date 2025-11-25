open! O

type 'a t = Cst.t -> 'a

module Error : sig
  type t =
    | Single of
        { span : Span.t
        ; message : Sexp.t
        }
    | List of t list
  [@@deriving equal, compare, sexp]

  val to_error : t -> Error.t
end

val run : (unit -> 'a) -> ('a, Error.t) Result.t
val with_span : Span.t -> (unit -> 'a) -> 'a
val parse_error : Sexp.t -> 'a
val atom : (string -> 'a) -> Cst.t -> 'a
val string : Cst.t -> string
val list : Cst.t -> (Cst.t list -> 'a) -> 'a
val list_ref : Cst.t -> (Cst.t list ref -> 'a) -> 'a
val item : Cst.t list ref -> (Cst.t -> 'a) -> 'a
val next : Cst.t list ref -> Cst.t
val optional_item : Cst.t list -> (Cst.t -> 'a) -> 'a option
val rest : Cst.t list -> (Cst.t -> 'a) -> 'a list
val either : 'a t -> 'b t -> ('a, 'b) Either.t t
