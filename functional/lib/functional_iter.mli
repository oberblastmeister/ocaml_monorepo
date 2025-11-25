type 'a t = f:('a -> unit) -> unit
type ('a, 'b) t2 = f:('a -> 'b -> unit) -> unit

val append : 'a t -> 'a t -> 'a t
val of_fn : (('a -> unit) -> unit) -> 'a t
val to_fn : 'a t -> ('a -> unit) -> unit
val for_ : 'a t -> ('a -> unit) -> unit
val to_list_rev : 'a t -> 'a list
val while_ : (unit -> bool) -> unit t
val while_some : (unit -> 'a option) -> 'a t
val uncurry : ('a, 'b) t2 -> ('a * 'b) t
val find_map : 'a t -> f:('a -> 'b option) -> 'b option
val find : 'a t -> f:('a -> bool) -> 'a option
val drop : int -> 'a t -> 'a t
val take : int -> 'a t -> 'a t
val take_while : 'a t -> f:('a -> bool) -> 'a t
val take_while_map : 'a t -> f:('a -> 'b option) -> 'b t
val iter : 'a t -> 'a t
val empty : 'a t
val singleton : 'a -> 'a t
val cons : 'a -> 'a t -> 'a t
val snoc : 'a t -> 'a -> 'a t
val iter : 'a t -> f:('a -> unit) -> unit
val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b
val sum : int t -> int
val filter : 'a t -> f:('a -> bool) -> 'a t
val filter_map : 'a t -> f:('a -> 'b option) -> 'b t
val concat_map : 'a t -> f:('a -> 'b t) -> 'b t
val bind : 'a t -> f:('a -> 'b t) -> 'b t
val enumerate : 'a t -> (int * 'a) t
val map : 'a t -> f:('a -> 'b) -> 'b t
val length : 'a t -> int
val unfoldr : init:'s -> f:('s -> ('a * 's) option) -> 'a t
val to_list : 'a t -> 'a list
val to_array : 'a t -> 'a array
val find_map : 'a t -> f:('a -> 'b option) -> 'b option
val find : 'a t -> f:('a -> bool) -> 'a option
val exists : 'a t -> f:('a -> bool) -> bool
val for_all : f:('a -> bool) -> 'a t -> bool
val int_range : start:int -> stop:int -> int t
val rev : 'a t -> 'a t
val repeat : 'a -> 'a t
val scan : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b t
val product : 'a t -> 'b t -> ('a * 'b) t
val ensure_ephemeral : 'a t -> 'a t
val max_elt : 'a t -> compare:('a -> 'a -> int) -> 'a option

module Infix : sig
  val ( -- ) : int -> int -> int t
end

val ( -- ) : int -> int -> int t

module Private : sig
  module MList : sig
    type 'a t

    val iter_rev : 'a t -> f:('a -> unit) -> unit
  end

  val to_mlist : 'a t -> 'a MList.t
end
