open Base

(** Immutable arrays backed by [Stdlib.Iarray], exposed with Core-style argument
    ordering and labeled function parameters. *)

type 'a t [@@deriving sexp]
type 'a iarray = 'a t [@@deriving sexp]

val length : 'a t -> int
val get : 'a t -> int -> 'a
val init : int -> f:(int -> 'a) -> 'a t
val append : 'a t -> 'a t -> 'a t
val concat : 'a t list -> 'a t
val sub : 'a t -> pos:int -> len:int -> 'a t

val to_list : 'a t -> 'a list
val of_list : 'a list -> 'a t
val to_array : 'a t -> 'a array
val of_array : 'a array -> 'a t

val equal : 'a t -> 'a t -> equal:('a -> 'a -> bool) -> bool
val compare : 'a t -> 'a t -> compare:('a -> 'a -> int) -> int

val iter : 'a t -> f:('a -> unit) -> unit
val iteri : 'a t -> f:(int -> 'a -> unit) -> unit
val map : 'a t -> f:('a -> 'b) -> 'b t
val mapi : 'a t -> f:(int -> 'a -> 'b) -> 'b t
val fold_left : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc) -> 'acc
val fold_left_map : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc * 'b) -> 'acc * 'b t
val fold_right : 'a t -> init:'acc -> f:('a -> 'acc -> 'acc) -> 'acc

val iter2 : 'a t -> 'b t -> f:('a -> 'b -> unit) -> unit
val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

val for_all : 'a t -> f:('a -> bool) -> bool
val exists : 'a t -> f:('a -> bool) -> bool
val for_all2 : 'a t -> 'b t -> f:('a -> 'b -> bool) -> bool
val exists2 : 'a t -> 'b t -> f:('a -> 'b -> bool) -> bool
val mem : 'a t -> 'a -> equal:('a -> 'a -> bool) -> bool
val memq : 'a t -> 'a -> bool
val find : 'a t -> f:('a -> bool) -> 'a option
val find_index : 'a t -> f:('a -> bool) -> int option
val find_map : 'a t -> f:('a -> 'b option) -> 'b option
val find_mapi : 'a t -> f:(int -> 'a -> 'b option) -> 'b option

val zip : 'a t -> 'b t -> ('a * 'b) t option
val zip_exn : 'a t -> 'b t -> ('a * 'b) t
val unzip : ('a * 'b) t -> 'a t * 'b t

val sort : 'a t -> compare:('a -> 'a -> int) -> 'a t
val stable_sort : 'a t -> compare:('a -> 'a -> int) -> 'a t
val fast_sort : 'a t -> compare:('a -> 'a -> int) -> 'a t

val to_seq : 'a t -> 'a Stdlib.Seq.t
val to_seqi : 'a t -> (int * 'a) Stdlib.Seq.t
val of_seq : 'a Stdlib.Seq.t -> 'a t
