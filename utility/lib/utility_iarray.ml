open Core

type 'a t = 'a iarray
type 'a iarray = 'a t

let sexp_of_t sexp_of_a t = List.sexp_of_t sexp_of_a (Stdlib.Iarray.to_list t)
let t_of_sexp a_of_sexp sexp = Stdlib.Iarray.of_list (List.t_of_sexp a_of_sexp sexp)
let sexp_of_iarray = sexp_of_t
let iarray_of_sexp = t_of_sexp

let length = Stdlib.Iarray.length
let get = Stdlib.Iarray.get
let init n ~f = Stdlib.Iarray.init n f
let append = Stdlib.Iarray.append
let concat = Stdlib.Iarray.concat
let sub t ~pos ~len = Stdlib.Iarray.sub t ~pos ~len
let to_list = Stdlib.Iarray.to_list
let of_list = Stdlib.Iarray.of_list
let to_array = Stdlib.Iarray.to_array
let of_array = Stdlib.Iarray.of_array

let equal t1 t2 ~equal = Stdlib.Iarray.equal equal t1 t2
let compare t1 t2 ~compare = Stdlib.Iarray.compare compare t1 t2

let iter t ~f = Stdlib.Iarray.iter f t
let iteri t ~f = Stdlib.Iarray.iteri f t
let map t ~f = Stdlib.Iarray.map f t
let mapi t ~f = Stdlib.Iarray.mapi f t
let fold_left t ~init ~f = Stdlib.Iarray.fold_left f init t
let fold_left_map t ~init ~f = Stdlib.Iarray.fold_left_map f init t
let fold_right t ~init ~f = Stdlib.Iarray.fold_right f t init

let iter2 t1 t2 ~f = Stdlib.Iarray.iter2 f t1 t2
let map2 t1 t2 ~f = Stdlib.Iarray.map2 f t1 t2

let for_all t ~f = Stdlib.Iarray.for_all f t
let exists t ~f = Stdlib.Iarray.exists f t
let for_all2 t1 t2 ~f = Stdlib.Iarray.for_all2 f t1 t2
let exists2 t1 t2 ~f = Stdlib.Iarray.exists2 f t1 t2
let mem t x ~equal = exists t ~f:(fun y -> equal x y)
let memq t x = Stdlib.Iarray.memq x t
let find t ~f = Stdlib.Iarray.find_opt f t
let find_index t ~f = Stdlib.Iarray.find_index f t
let find_map t ~f = Stdlib.Iarray.find_map f t
let find_mapi t ~f = Stdlib.Iarray.find_mapi f t

let invalid_argf = Printf.invalid_argf

let zip t1 t2 : ('a * 'b) t option =
  if Int.equal (length t1) (length t2)
  then Some (Stdlib.Iarray.combine t1 t2)
  else None
;;

let zip_exn t1 t2 =
  match zip t1 t2 with
  | Some t -> t
  | None ->
    invalid_argf "Iarray.zip_exn: length mismatch. %d <> %d" (length t1) (length t2) ()
;;

let unzip = Stdlib.Iarray.split

let sort t ~compare = Stdlib.Iarray.sort compare t
let stable_sort t ~compare = Stdlib.Iarray.stable_sort compare t
let fast_sort t ~compare = Stdlib.Iarray.fast_sort compare t

let to_seq = Stdlib.Iarray.to_seq
let to_seqi = Stdlib.Iarray.to_seqi
let of_seq = Stdlib.Iarray.of_seq
