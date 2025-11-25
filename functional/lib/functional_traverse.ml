open Core

type ('t, 's, 'b, 'a) t = 'a -> f:('s -> 't) -> 'b
type ('s, 'a) t' = ('s, 's, 'a, 'a) t

let of_map m x ~f = m f x

let compose
  : 'v 'u 't 's 'b 'a. ('t, 's, 'b, 'a) t -> ('v, 'u, 't, 's) t -> ('v, 'u, 'b, 'a) t
  =
  fun mf mg a ~f -> mf ~f:(fun s -> mg ~f s) a
;;

let filtered ~f:pred a ~f = if pred a then f a else a
let ( & ) = compose
let of_field field x ~f = Field.map field x ~f
