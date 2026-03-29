open Core
module Dynarray = Utility_dynarray

type 'a t =
  { a : 'a Dynarray.t
  ; slice_start : int
  ; slice_stop : int
  }

let create capacity =
  let a = Dynarray.create capacity in
  { a; slice_start = 0; slice_stop = 0 }
;;

let length t = t.slice_stop - t.slice_start
let is_empty t = Int.equal (length t) 0

let is_full_slice t =
  Int.equal t.slice_start 0 && Int.equal t.slice_stop (Dynarray.length t.a)
;;

let singleton x =
  let t = create 1 in
  Dynarray.push t.a x;
  { t with slice_stop = 1 }
;;

let init len ~f =
  let a = Dynarray.init len ~f in
  { a; slice_start = 0; slice_stop = len }
;;

let copy t =
  let a = Dynarray.create (Dynarray.capacity t.a) in
  Dynarray.blit ~src:t.a ~src_pos:t.slice_start ~dst:a ~dst_pos:0 ~len:(length t);
  { a; slice_start = 0; slice_stop = length t }
;;

let update_slice_stop t =
  assert (Int.equal t.slice_start 0);
  { t with slice_stop = Dynarray.length t.a }
;;

let index_out_of_bounds_error name t i =
  failwithf
    "Cow_slice.%s: index %d is out of bounds (length = %d)"
    name
    (i - t.slice_start)
    (length t)
    ()
;;

let length_mismatch name t1 t2 =
  failwithf "Cow_slice.%s: length mismatch. %d <> %d" name (length t1) (length t2) ()
;;

let not_found name = Not_found_s (Sexp.Atom ("Cow_slice." ^ name ^ ": not found"))

let make_result ?capacity_like ?capacity () =
  match capacity with
  | Some capacity -> create capacity
  | None ->
    let t = Option.value_exn capacity_like in
    create (Dynarray.capacity t.a)
;;

module Unchecked = struct
  (* f should only append to the array and never mutate any cells *)
  let mutate t ~f =
    if is_full_slice t
    then begin
      f t.a;
      update_slice_stop t
    end
    else begin
      let t = copy t in
      f t.a;
      update_slice_stop t
    end
  ;;

  let mutate_full_slice_exn t ~f =
    if not (is_full_slice t)
    then failwith "mutate_full_slice_exn: array was mutated since last call";
    f t.a;
    update_slice_stop t
  ;;
end

let get t i =
  let i = t.slice_start + i in
  if i < t.slice_start || i >= t.slice_stop then index_out_of_bounds_error "get" t i;
  Dynarray.get t.a i
;;

let last t =
  if is_empty t
  then failwithf "Cow_slice.last: array is empty (length = %d)" (length t) ();
  Dynarray.get t.a (t.slice_stop - 1)
;;

let push t x = Unchecked.mutate t ~f:(fun a -> Dynarray.push a x)

let push_full_slice_exn t x =
  Unchecked.mutate_full_slice_exn t ~f:(fun a -> Dynarray.push a x)
;;

let append t t' =
  Unchecked.mutate t ~f:(fun a ->
    Dynarray.ensure_extra_capacity a (length t');
    for i = 0 to length t' - 1 do
      Dynarray.push a (get t' i)
    done)
;;

let append_full_slice_exn t t' =
  Unchecked.mutate_full_slice_exn t ~f:(fun a ->
    Dynarray.ensure_extra_capacity a (length t');
    for i = 0 to length t' - 1 do
      Dynarray.push a (get t' i)
    done)
;;

let append_array t a' = Unchecked.mutate t ~f:(fun a -> Dynarray.append_array a a')

let append_list_full_slice_exn t l =
  Unchecked.mutate_full_slice_exn t ~f:(fun a -> Dynarray.append_list a l)
;;

let append_list t l = Unchecked.mutate t ~f:(fun a -> Dynarray.append_list a l)

let append_array_full_slice_exn t a' =
  Unchecked.mutate_full_slice_exn t ~f:(fun a -> Dynarray.append_array a a')
;;

let slice t start stop =
  let stop = if Int.equal stop 0 then length t else stop in
  if start > stop then failwithf "Cow_slice.slice: start > stop: %d > %d" start stop ();
  let start = t.slice_start + start in
  let stop = t.slice_start + stop in
  if start > t.slice_stop then index_out_of_bounds_error "slice" t start;
  if stop > t.slice_stop then index_out_of_bounds_error "slice" t stop;
  { t with slice_start = start; slice_stop = stop }
;;

let to_list_rev t =
  let l = ref [] in
  for i = t.slice_start to t.slice_stop - 1 do
    l := Dynarray.get t.a i :: !l
  done;
  !l
;;

let to_list t = List.rev (to_list_rev t)
let to_array t = Array.init (length t) ~f:(fun i -> get t i)

let of_array a =
  let t = create (Array.length a) in
  append_array_full_slice_exn t a
;;

let of_list l =
  let t = create (List.length l) in
  append_list_full_slice_exn t l
;;

let sexp_of_t sexp_of_a t = List.sexp_of_t sexp_of_a (to_list t)
let t_of_sexp a_of_sexp sexp = of_list (List.t_of_sexp a_of_sexp sexp)

let iter t ~f =
  for i = 0 to length t - 1 do
    f (get t i)
  done
;;

let iteri t ~f =
  for i = 0 to length t - 1 do
    f i (get t i)
  done
;;

let fold_left t ~init ~f =
  let acc = ref init in
  iter t ~f:(fun x -> acc := f !acc x);
  !acc
;;

let fold_lefti t ~init ~f =
  let acc = ref init in
  iteri t ~f:(fun i x -> acc := f i !acc x);
  !acc
;;

let fold = fold_left
let foldi = fold_lefti

let fold_right t ~init ~f =
  let acc = ref init in
  for i = length t - 1 downto 0 do
    acc := f (get t i) !acc
  done;
  !acc
;;

let for_all t ~f =
  let rec loop i = i >= length t || (f (get t i) && loop (i + 1)) in
  loop 0
;;

let for_alli t ~f = fold_lefti t ~init:true ~f:(fun i acc x -> acc && f i x)

let exists t ~f =
  let rec loop i = i < length t && (f (get t i) || loop (i + 1)) in
  loop 0
;;

let existsi t ~f = fold_lefti t ~init:false ~f:(fun i acc x -> acc || f i x)
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

let find_exn t ~f =
  match find t ~f with
  | Some x -> x
  | None -> raise (not_found "find_exn")
;;

let findi t ~f =
  fold_lefti t ~init:None ~f:(fun i acc x ->
    match acc with
    | Some _ -> acc
    | None -> if f i x then Some (i, x) else None)
;;

let findi_exn t ~f =
  match findi t ~f with
  | Some x -> x
  | None -> raise (not_found "findi_exn")
;;

let find_index t ~f =
  fold_lefti t ~init:None ~f:(fun i acc x ->
    match acc with
    | Some _ -> acc
    | None -> if f x then Some i else None)
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

let find_map_exn t ~f =
  match find_map t ~f with
  | Some x -> x
  | None -> raise (not_found "find_map_exn")
;;

let find_mapi t ~f =
  fold_lefti t ~init:None ~f:(fun i acc x ->
    match acc with
    | Some _ -> acc
    | None -> f i x)
;;

let find_mapi_exn t ~f =
  match find_mapi t ~f with
  | Some x -> x
  | None -> raise (not_found "find_mapi_exn")
;;

let find_consecutive_duplicate t ~equal =
  let rec loop i =
    if i + 1 >= length t
    then None
    else (
      let x = get t i in
      let y = get t (i + 1) in
      if equal x y then Some (x, y) else loop (i + 1))
  in
  loop 0
;;

let reduce t ~f =
  if is_empty t then None else Some (fold_left (slice t 1 0) ~init:(get t 0) ~f)
;;

let reduce_exn t ~f =
  match reduce t ~f with
  | Some x -> x
  | None -> failwith "Cow_slice.reduce_exn: empty slice"
;;

let equal t1 t2 ~equal =
  Int.equal (length t1) (length t2) && for_alli t1 ~f:(fun i x -> equal x (get t2 i))
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

let is_sorted t ~compare =
  let rec loop i =
    i + 1 >= length t || (compare (get t i) (get t (i + 1)) <= 0 && loop (i + 1))
  in
  loop 0
;;

let is_sorted_strictly t ~compare =
  let rec loop i =
    i + 1 >= length t || (compare (get t i) (get t (i + 1)) < 0 && loop (i + 1))
  in
  loop 0
;;

let map t ~f =
  fold_left t ~init:(make_result ~capacity_like:t ()) ~f:(fun acc x ->
    push_full_slice_exn acc (f x))
;;

let mapi t ~f =
  fold_lefti t ~init:(make_result ~capacity_like:t ()) ~f:(fun i acc x ->
    push_full_slice_exn acc (f i x))
;;

let fold_map t ~init ~f =
  fold_left
    t
    ~init:(init, make_result ~capacity_like:t ())
    ~f:(fun (acc, out) x ->
      let acc, y = f acc x in
      acc, push_full_slice_exn out y)
;;

let fold_mapi t ~init ~f =
  fold_lefti
    t
    ~init:(init, make_result ~capacity_like:t ())
    ~f:(fun i (acc, out) x ->
      let acc, y = f i acc x in
      acc, push_full_slice_exn out y)
;;

let folding_map t ~init ~f = fold_map t ~init ~f |> snd
let folding_mapi t ~init ~f = fold_mapi t ~init ~f |> snd

let filter t ~f =
  fold_left t ~init:(make_result ~capacity_like:t ()) ~f:(fun acc x ->
    if f x then push_full_slice_exn acc x else acc)
;;

let filteri t ~f =
  fold_lefti t ~init:(make_result ~capacity_like:t ()) ~f:(fun i acc x ->
    if f i x then push_full_slice_exn acc x else acc)
;;

let filter_map t ~f =
  fold_left t ~init:(make_result ~capacity_like:t ()) ~f:(fun acc x ->
    match f x with
    | None -> acc
    | Some y -> push_full_slice_exn acc y)
;;

let filter_mapi t ~f =
  fold_lefti t ~init:(make_result ~capacity_like:t ()) ~f:(fun i acc x ->
    match f i x with
    | None -> acc
    | Some y -> push_full_slice_exn acc y)
;;

let filter_opt t = filter_map t ~f:Fn.id

let partition_tf t ~f =
  fold_left
    t
    ~init:(make_result ~capacity_like:t (), make_result ~capacity_like:t ())
    ~f:(fun (yes, no) x ->
      if f x then push_full_slice_exn yes x, no else yes, push_full_slice_exn no x)
;;

let partitioni_tf t ~f =
  fold_lefti
    t
    ~init:(make_result ~capacity_like:t (), make_result ~capacity_like:t ())
    ~f:(fun i (yes, no) x ->
      if f i x then push_full_slice_exn yes x, no else yes, push_full_slice_exn no x)
;;

let rev t =
  fold_right t ~init:(make_result ~capacity_like:t ()) ~f:(fun x acc ->
    push_full_slice_exn acc x)
;;

let concat ts =
  let total_length = List.fold ts ~init:0 ~f:(fun acc t -> acc + length t) in
  List.fold ts ~init:(create total_length) ~f:append
;;

let check_same_length_exn name t1 t2 =
  if not (Int.equal (length t1) (length t2)) then length_mismatch name t1 t2
;;

let iter2_exn t1 t2 ~f =
  check_same_length_exn "iter2_exn" t1 t2;
  for i = 0 to length t1 - 1 do
    f (get t1 i) (get t2 i)
  done
;;

let map2_exn t1 t2 ~f =
  check_same_length_exn "map2_exn" t1 t2;
  let _, out =
    fold_left
      t1
      ~init:(0, create (Int.min (Dynarray.capacity t1.a) (Dynarray.capacity t2.a)))
      ~f:(fun (i, acc) x -> i + 1, push_full_slice_exn acc (f x (get t2 i)))
  in
  out
;;

let fold2_exn t1 t2 ~init ~f =
  check_same_length_exn "fold2_exn" t1 t2;
  let acc = ref init in
  for i = 0 to length t1 - 1 do
    acc := f !acc (get t1 i) (get t2 i)
  done;
  !acc
;;

let for_all2_exn t1 t2 ~f =
  check_same_length_exn "for_all2_exn" t1 t2;
  let rec loop i = i >= length t1 || (f (get t1 i) (get t2 i) && loop (i + 1)) in
  loop 0
;;

let exists2_exn t1 t2 ~f =
  check_same_length_exn "exists2_exn" t1 t2;
  let rec loop i = i < length t1 && (f (get t1 i) (get t2 i) || loop (i + 1)) in
  loop 0
;;

let zip t1 t2 =
  if not (Int.equal (length t1) (length t2))
  then None
  else Some (map2_exn t1 t2 ~f:(fun x y -> x, y))
;;

let zip_exn t1 t2 =
  match zip t1 t2 with
  | Some t -> t
  | None -> length_mismatch "zip_exn" t1 t2
;;

let unzip t =
  fold_left
    t
    ~init:(create (Dynarray.capacity t.a), create (Dynarray.capacity t.a))
    ~f:(fun (left, right) (x, y) ->
      push_full_slice_exn left x, push_full_slice_exn right y)
;;
