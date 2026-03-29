open Core

type 'a t = 'a Stdlib.Dynarray.t
type 'a dynarray = 'a t

let create capacity =
  let a = Stdlib.Dynarray.create () in
  if capacity = 0
  then a
  else begin
    Stdlib.Dynarray.set_capacity a capacity;
    a
  end
;;

let make ~len x = Stdlib.Dynarray.make len x
let copy = Stdlib.Dynarray.copy
let init n ~f = Stdlib.Dynarray.init n f
let blit = Stdlib.Dynarray.blit
let of_array = Stdlib.Dynarray.of_array
let to_array = Stdlib.Dynarray.to_array
let of_list = Stdlib.Dynarray.of_list
let to_list = Stdlib.Dynarray.to_list
let of_seq = Stdlib.Dynarray.of_seq
let to_seq = Stdlib.Dynarray.to_seq
let length = Stdlib.Dynarray.length
let is_empty = Stdlib.Dynarray.is_empty
let capacity = Stdlib.Dynarray.capacity
let get = Stdlib.Dynarray.get
let set = Stdlib.Dynarray.set
let clear = Stdlib.Dynarray.clear
let truncate = Stdlib.Dynarray.truncate
let reset = Stdlib.Dynarray.reset
let append = Stdlib.Dynarray.append
let append_array = Stdlib.Dynarray.append_array
let append_list = Stdlib.Dynarray.append_list
let append_seq = Stdlib.Dynarray.append_seq
let ensure_capacity = Stdlib.Dynarray.ensure_capacity
let ensure_extra_capacity = Stdlib.Dynarray.ensure_extra_capacity
let fit_capacity = Stdlib.Dynarray.fit_capacity
let set_capacity = Stdlib.Dynarray.set_capacity
let iter t ~f = Stdlib.Dynarray.iter f t
let iteri t ~f = Stdlib.Dynarray.iteri f t
let map t ~f = Stdlib.Dynarray.map f t
let mapi t ~f = Stdlib.Dynarray.mapi f t
let fold_left t ~init ~f = Stdlib.Dynarray.fold_left f init t
let fold = fold_left
let fold_right t ~init ~f = Stdlib.Dynarray.fold_right f t init
let for_all t ~f = Stdlib.Dynarray.for_all f t
let exists t ~f = Stdlib.Dynarray.exists f t
let filter t ~f = Stdlib.Dynarray.filter f t
let filter_map t ~f = Stdlib.Dynarray.filter_map f t
let singleton x = make ~len:1 x
let last = Stdlib.Dynarray.get_last
let push t x = Stdlib.Dynarray.add_last t x
let pop = Stdlib.Dynarray.pop_last_opt
let pop_exn = Stdlib.Dynarray.pop_last

let until_empty t f =
  while not (is_empty t) do
    f (pop_exn t)
  done
;;

let mem t x ~equal = exists t ~f:(fun y -> equal x y)

let find t ~f =
  let rec loop i =
    if i >= length t
    then None
    else (
      let x = get t i in
      if f x then Some x else loop (i + 1))
  in
  loop 0
;;

let findi t ~f =
  let rec loop i =
    if i >= length t
    then None
    else (
      let x = get t i in
      if f i x then Some (i, x) else loop (i + 1))
  in
  loop 0
;;

let find_map t ~f =
  let rec loop i =
    if i >= length t
    then None
    else (
      match f (get t i) with
      | Some _ as result -> result
      | None -> loop (i + 1))
  in
  loop 0
;;

let find_mapi t ~f =
  let rec loop i =
    if i >= length t
    then None
    else (
      match f i (get t i) with
      | Some _ as result -> result
      | None -> loop (i + 1))
  in
  loop 0
;;

let equal t1 t2 ~equal =
  Int.equal (length t1) (length t2)
  &&
  let rec loop i =
    if i >= length t1 then true else equal (get t1 i) (get t2 i) && loop (i + 1)
  in
  loop 0
;;

let compare t1 t2 ~compare =
  let rec loop i =
    if i >= length t1 || i >= length t2
    then Int.compare (length t1) (length t2)
    else (
      let c = compare (get t1 i) (get t2 i) in
      if Int.equal c 0 then loop (i + 1) else c)
  in
  loop 0
;;

let%test_module "Dynarray" =
  (module struct
    let%test "stack operations" =
      let t = singleton 1 in
      push t 2;
      Int.equal (last t) 2
      && Option.equal Int.equal (pop t) (Some 2)
      && Int.equal (last t) 1
    ;;

    let%test "find helpers" =
      let t = of_list [ 1; 3; 4 ] in
      Option.equal Int.equal (find t ~f:(fun x -> x mod 2 = 0)) (Some 4)
      && Option.equal
           [%equal: int * int]
           (findi t ~f:(fun i x -> Int.equal (i + x) 4))
           (Some (1, 3))
      && Option.equal
           Int.equal
           (find_map t ~f:(fun x -> if x > 3 then Some (x * 2) else None))
           (Some 8)
      && Option.equal
           Int.equal
           (find_mapi t ~f:(fun i x -> if Int.equal i 2 then Some (x + 1) else None))
           (Some 5)
    ;;
  end)
;;
