type 'a t [@@deriving sexp]

val empty : 'a t
val push : 'a -> 'a t -> 'a t
val pop : 'a t -> ('a * 'a t) option
val pop_exn : 'a t -> 'a * 'a t
val get : 'a t -> int -> 'a option
val get_exn : 'a t -> int -> 'a
val iter : 'a t -> f:('a -> unit) -> unit
val to_list : 'a t -> 'a list
val of_list : 'a list -> 'a t
val length : 'a t -> int
