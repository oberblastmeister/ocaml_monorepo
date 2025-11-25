open Core

type ('t, 's, 'b, 'a) t = 'a -> f:('s -> 't) -> 'b
type ('s, 'a) t' = ('s, 's, 'a, 'a) t

val compose : ('t, 's, 'b, 'a) t -> ('v, 'u, 't, 's) t -> ('v, 'u, 'b, 'a) t
val ( & ) : ('t, 's, 'b, 'a) t -> ('v, 'u, 't, 's) t -> ('v, 'u, 'b, 'a) t
val filtered : f:('s -> bool) -> ('s, 's) t'
val of_field : ([> `Set_and_create ], 'r, 'f) Field.t_with_perm -> ('f, 'r) t'
