open Base

(** Mutable dynamic arrays with Core-style naming and argument ordering.

    This wraps [Stdlib.Dynarray], exposing array-style indexed access and
    stack-style [push]/[pop]/[top] operations. *)

type 'a t
type 'a dynarray = 'a t

val create : int -> 'a t
val make : len:int -> 'a -> 'a t
val blit : src:'a t -> src_pos:int -> dst:'a t -> dst_pos:int -> len:int -> unit
val singleton : 'a -> 'a t
val copy : 'a t -> 'a t
val init : int -> f:(int -> 'a) -> 'a t
val of_array : 'a array -> 'a t
val to_array : 'a t -> 'a array
val of_list : 'a list -> 'a t
val to_list : 'a t -> 'a list
val of_seq : 'a Stdlib.Seq.t -> 'a t
val to_seq : 'a t -> 'a Stdlib.Seq.t
val length : 'a t -> int
val is_empty : 'a t -> bool
val capacity : 'a t -> int
val get : 'a t -> int -> 'a
val set : 'a t -> int -> 'a -> unit
val last : 'a t -> 'a
val push : 'a t -> 'a -> unit
val pop : 'a t -> 'a option
val pop_exn : 'a t -> 'a
val clear : 'a t -> unit
val truncate : 'a t -> int -> unit
val reset : 'a t -> unit
val until_empty : 'a t -> ('a -> unit) -> unit
val append : 'a t -> 'a t -> unit
val append_array : 'a t -> 'a array -> unit
val append_list : 'a t -> 'a list -> unit
val append_seq : 'a t -> 'a Stdlib.Seq.t -> unit
val ensure_capacity : 'a t -> int -> unit
val ensure_extra_capacity : 'a t -> int -> unit
val fit_capacity : 'a t -> unit
val set_capacity : 'a t -> int -> unit
val iter : 'a t -> f:('a -> unit) -> unit
val iteri : 'a t -> f:(int -> 'a -> unit) -> unit
val map : 'a t -> f:('a -> 'b) -> 'b t
val mapi : 'a t -> f:(int -> 'a -> 'b) -> 'b t
val fold_left : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc) -> 'acc
val fold_right : 'a t -> init:'acc -> f:('a -> 'acc -> 'acc) -> 'acc
val for_all : 'a t -> f:('a -> bool) -> bool
val exists : 'a t -> f:('a -> bool) -> bool
val mem : 'a t -> 'a -> equal:('a -> 'a -> bool) -> bool
val find : 'a t -> f:('a -> bool) -> 'a option
val findi : 'a t -> f:(int -> 'a -> bool) -> (int * 'a) option
val find_map : 'a t -> f:('a -> 'b option) -> 'b option
val find_mapi : 'a t -> f:(int -> 'a -> 'b option) -> 'b option
val filter : 'a t -> f:('a -> bool) -> 'a t
val filter_map : 'a t -> f:('a -> 'b option) -> 'b t
val equal : 'a t -> 'a t -> equal:('a -> 'a -> bool) -> bool
val compare : 'a t -> 'a t -> compare:('a -> 'a -> int) -> int
