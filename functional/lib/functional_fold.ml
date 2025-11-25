open Core
module Iter = Functional_iter

type ('a, 'b) t = 'a -> 'b Iter.t

let of_field field k x = Field.get field x |> k
let of_fn f x ~f:k = f x |> k
let compose f g a ~f:k = f a ~f:(fun b -> g b ~f:k)
let ( @> ) = compose
let map fold ~f a ~f:k = fold a ~f:(fun b -> k (f b))
let postmap = map
let filtered f a k = if f a then k a else ()
let mapped f a ~f:k = k (f a)
let premap ~f fold a k = fold (f a) k
let no_ix t = premap (fun (_i, x) -> x) t
let no_ix2 t = no_ix (no_ix t)
let no_ix3 t = no_ix (no_ix (no_ix t))
let enumerate fold a = Iter.enumerate (fold a)
let ix fold (i, a) ~f:k = fold a ~f:(fun b -> k (i, b))
let ix2 t = ix (ix t)
let ix3 t = ix (ix (ix t))
let dup t = map ~f:(fun x -> x, x) t
let fold fold x ~init ~f = Iter.fold ~init ~f (fold x)
let iter x fold ~f = Iter.iter ~f (fold x)
let to_iter fold x = fold x
