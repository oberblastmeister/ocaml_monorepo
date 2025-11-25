open Core
module Iter := Functional_iter

type ('a, 'b) t = 'a -> 'b Iter.t

val of_fn : ('a -> 'b) -> ('a, 'b) t
val compose : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
val ( @> ) : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
val map : ('a, 'b) t -> f:('b -> 'c) -> ('a, 'c) t
val iter : 'a -> ('a, 'b) t -> 'b Iter.t
