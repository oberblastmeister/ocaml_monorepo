open Core

type 'a t = 'a list [@@deriving sexp]

let empty = []
let push x xs = x :: xs

let pop = function
  | [] -> None
  | x :: xs -> Some (x, xs)
;;

let pop_exn = function
  | [] -> failwith "empty sequence"
  | x :: xs -> x, xs
;;

let get xs i = List.nth xs i
let get_exn xs i = List.nth_exn xs i
let iter xs ~f = List.iter xs ~f
let to_list xs = xs
let of_list xs = xs
let length = List.length
