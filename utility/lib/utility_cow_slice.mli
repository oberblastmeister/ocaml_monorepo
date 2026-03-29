module Dynarray := Utility_dynarray

(*
  This data structure is always purely functional.
  Mutability is only used as an optimization that does not affect observable behavior.
  We say a slice is full if it spans the entire array.
  We can only mutate if we have a full slice.
  Whenever we make a copy, we return a full slice.
  Pushing onto a full slice returns a full slice.
*)
type 'a t

val sexp_of_t : ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t
val t_of_sexp : (Sexplib0.Sexp.t -> 'a) -> Sexplib0.Sexp.t -> 'a t

val create : int -> 'a t
val singleton : 'a -> 'a t
val init : int -> f:(int -> 'a) -> 'a t
val copy : 'a t -> 'a t

val length : 'a t -> int
val is_empty : 'a t -> bool

val get : 'a t -> int -> 'a
val last : 'a t -> 'a
val slice : 'a t -> int -> int -> 'a t

val push : 'a t -> 'a -> 'a t
val push_full_slice_exn : 'a t -> 'a -> 'a t
val append : 'a t -> 'a t -> 'a t
val append_full_slice_exn : 'a t -> 'a t -> 'a t
val append_array : 'a t -> 'a array -> 'a t
val append_list : 'a t -> 'a list -> 'a t
val append_array_full_slice_exn : 'a t -> 'a array -> 'a t
val append_list_full_slice_exn : 'a t -> 'a list -> 'a t
val concat : 'a t list -> 'a t

val of_array : 'a array -> 'a t
val to_array : 'a t -> 'a array
val of_list : 'a list -> 'a t
val to_list : 'a t -> 'a list
val to_list_rev : 'a t -> 'a list

val iter : 'a t -> f:('a -> unit) -> unit
val iteri : 'a t -> f:(int -> 'a -> unit) -> unit
val fold : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc) -> 'acc
val foldi : 'a t -> init:'acc -> f:(int -> 'acc -> 'a -> 'acc) -> 'acc
val fold_left : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc) -> 'acc
val fold_right : 'a t -> init:'acc -> f:('a -> 'acc -> 'acc) -> 'acc

val for_all : 'a t -> f:('a -> bool) -> bool
val for_alli : 'a t -> f:(int -> 'a -> bool) -> bool
val exists : 'a t -> f:('a -> bool) -> bool
val existsi : 'a t -> f:(int -> 'a -> bool) -> bool
val mem : 'a t -> 'a -> equal:('a -> 'a -> bool) -> bool

val find : 'a t -> f:('a -> bool) -> 'a option
val find_exn : 'a t -> f:('a -> bool) -> 'a
val findi : 'a t -> f:(int -> 'a -> bool) -> (int * 'a) option
val findi_exn : 'a t -> f:(int -> 'a -> bool) -> int * 'a
val find_index : 'a t -> f:('a -> bool) -> int option
val find_map : 'a t -> f:('a -> 'b option) -> 'b option
val find_map_exn : 'a t -> f:('a -> 'b option) -> 'b
val find_mapi : 'a t -> f:(int -> 'a -> 'b option) -> 'b option
val find_mapi_exn : 'a t -> f:(int -> 'a -> 'b option) -> 'b
val find_consecutive_duplicate : 'a t -> equal:('a -> 'a -> bool) -> ('a * 'a) option

val reduce : 'a t -> f:('a -> 'a -> 'a) -> 'a option
val reduce_exn : 'a t -> f:('a -> 'a -> 'a) -> 'a

val equal : 'a t -> 'a t -> equal:('a -> 'a -> bool) -> bool
val compare : 'a t -> 'a t -> compare:('a -> 'a -> int) -> int
val is_sorted : 'a t -> compare:('a -> 'a -> int) -> bool
val is_sorted_strictly : 'a t -> compare:('a -> 'a -> int) -> bool

val map : 'a t -> f:('a -> 'b) -> 'b t
val mapi : 'a t -> f:(int -> 'a -> 'b) -> 'b t
val fold_map : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc * 'b) -> 'acc * 'b t
val fold_mapi : 'a t -> init:'acc -> f:(int -> 'acc -> 'a -> 'acc * 'b) -> 'acc * 'b t
val folding_map : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc * 'b) -> 'b t
val folding_mapi : 'a t -> init:'acc -> f:(int -> 'acc -> 'a -> 'acc * 'b) -> 'b t

val filter : 'a t -> f:('a -> bool) -> 'a t
val filteri : 'a t -> f:(int -> 'a -> bool) -> 'a t
val filter_map : 'a t -> f:('a -> 'b option) -> 'b t
val filter_mapi : 'a t -> f:(int -> 'a -> 'b option) -> 'b t
val filter_opt : 'a option t -> 'a t
val partition_tf : 'a t -> f:('a -> bool) -> 'a t * 'a t
val partitioni_tf : 'a t -> f:(int -> 'a -> bool) -> 'a t * 'a t
val rev : 'a t -> 'a t

val iter2_exn : 'a t -> 'b t -> f:('a -> 'b -> unit) -> unit
val map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
val fold2_exn : 'a t -> 'b t -> init:'acc -> f:('acc -> 'a -> 'b -> 'acc) -> 'acc
val for_all2 : 'a t -> 'b t -> f:('a -> 'b -> bool) -> bool option
val for_all2_exn : 'a t -> 'b t -> f:('a -> 'b -> bool) -> bool
val exists2_exn : 'a t -> 'b t -> f:('a -> 'b -> bool) -> bool
val zip : 'a t -> 'b t -> ('a * 'b) t option
val zip_exn : 'a t -> 'b t -> ('a * 'b) t
val unzip : ('a * 'b) t -> 'a t * 'b t

module Unchecked : sig
  (* f can only append to the array and not mutate any array cells *)
  val mutate : 'a t -> f:('a Dynarray.t -> unit) -> 'a t

  (* Asserts that the slice is full. f can only append to the array and not mutate any array cells *)
  val mutate_full_slice_exn : 'a t -> f:('a Dynarray.t -> unit) -> 'a t
end
