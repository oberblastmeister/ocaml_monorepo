open Core

type 'a t = f:('a -> unit) -> unit
type ('a, 'b) t2 = f:('a -> 'b -> unit) -> unit

let[@inline] of_fn it = fun ~f -> it f
let[@inline] to_fn it = fun f -> it ~f
let[@inline] for_ = to_fn
let[@inline] uncurry t2 = fun ~f -> t2 ~f:(fun x y -> f (x, y))

let[@inline] append it1 it2 ~f =
  it1 ~f;
  it2 ~f
;;

let[@inline] to_list_rev t =
  let res = ref [] in
  t ~f:(fun x -> res := x :: !res);
  !res
;;

let[@inline] to_list t = to_list_rev t |> List.rev

let[@inline] while_ cond =
  fun ~f ->
  while cond () do
    f ()
  done;
  ()
;;

let[@inline] while_some cond ~f =
  let rec loop () =
    match cond () with
    | None -> ()
    | Some x ->
      f x;
      loop ()
  in
  loop ()
;;

let[@inline] empty ~f:_ = ()
let[@inline] singleton x ~f = f x

let[@inline] cons x i ~f =
  f x;
  i ~f
;;

let[@inline] snoc i x ~f =
  i ~f;
  f x
;;

let[@inline] iter i ~f = i ~f

let[@inline] fold i ~init ~f =
  let acc = ref init in
  i ~f:(fun x -> acc := f !acc x);
  !acc
;;

let[@inline] sum = fold ~init:0 ~f:( + )
let[@inline] count = fold ~init:0 ~f:(fun acc _ -> acc + 1)
let[@inline] filter i ~f:pred ~f:f' = i ~f:(fun x -> if pred x then f' x)
let[@inline] concat_map i ~f ~f:f' = i ~f:(fun x -> f x ~f:f')
let bind = concat_map

let[@inline] enumerate it ~f =
  let ix = ref 0 in
  it ~f:(fun x ->
    f (!ix, x);
    incr ix);
  ()
;;

let[@inline] map i ~f ~f:k = i ~f:(fun x -> k (f x))
let[@inline] to_list i = List.rev (fold i ~init:[] ~f:(fun xs x -> x :: xs))

let[@inline] length i =
  let len = ref 0 in
  i ~f:(fun _ -> incr len);
  !len
;;

let[@inline] filter_map i ~f ~f:k =
  i ~f:(fun x ->
    match f x with
    | None -> ()
    | Some x -> k x)
;;

let[@inline] unfoldr ~init ~f ~f:k =
  let rec loop state ~f =
    match f state with
    | None -> ()
    | Some (x, state) ->
      k x;
      loop state ~f
  in
  loop init ~f
;;

let ensure_ephemeral i =
  let used = ref false in
  fun ~f ->
    if !used
    then raise_s [%message "An ephemeral iterator was used multiple times"]
    else (
      used := true;
      i ~f)
;;

(** Mutable unrolled list to serve as intermediate storage *)
module MList = struct
  type 'a t =
    | Nil
    | Cons of 'a array * int ref * 'a t ref

  (* build and call callback on every element *)
  let of_iter_with seq ~f:k =
    let start = ref Nil in
    let chunk_size = ref 8 in
    (* fill the list. prev: tail-reference from previous node *)
    let prev, cur = ref start, ref Nil in
    seq ~f:(fun x ->
      k x;
      (* callback *)
      match !cur with
      | Nil ->
        let n = !chunk_size in
        if n < 4096 then chunk_size := 2 * !chunk_size;
        cur := Cons (Array.init n ~f:(Fn.const x), ref 1, ref Nil)
      | Cons (a, n, next) ->
        assert (!n < Array.length a);
        a.(!n) <- x;
        incr n;
        if !n = Array.length a
        then (
          !prev := !cur;
          prev := next;
          cur := Nil));
    !prev := !cur;
    !start
  ;;

  let of_iter seq = of_iter_with seq ~f:(fun _ -> ())

  let rec iter f l =
    match l with
    | Nil -> ()
    | Cons (a, n, tl) ->
      for i = 0 to !n - 1 do
        f a.(i)
      done;
      iter f !tl
  ;;

  let iteri f l =
    let rec iteri i f l =
      match l with
      | Nil -> ()
      | Cons (a, n, tl) ->
        for j = 0 to !n - 1 do
          f (i + j) a.(j)
        done;
        iteri (i + !n) f !tl
    in
    iteri 0 f l
  ;;

  let rec iter_rev l ~f =
    match l with
    | Nil -> ()
    | Cons (a, n, tl) ->
      iter_rev !tl ~f;
      for i = !n - 1 downto 0 do
        f a.(i)
      done
  ;;

  let length l =
    let rec len acc l =
      match l with
      | Nil -> acc
      | Cons (_, n, tl) -> len (acc + !n) !tl
    in
    len 0 l
  ;;

  (** Get element by index *)
  let rec get l i =
    match l with
    | Nil -> raise (Invalid_argument "MList.get")
    | Cons (a, n, _) when i < !n -> a.(i)
    | Cons (_, n, tl) -> get !tl (i - !n)
  ;;

  let to_iter l k = iter k l

  let _to_next arg l =
    let cur = ref l in
    let i = ref 0 in
    (* offset in cons *)
    let rec get_next _ =
      match !cur with
      | Nil -> None
      | Cons (_, n, tl) when !i = !n ->
        cur := !tl;
        i := 0;
        get_next arg
      | Cons (a, _, _) ->
        let x = a.(!i) in
        incr i;
        Some x
    in
    get_next
  ;;

  let to_gen l = _to_next () l

  let to_seq l =
    let rec make (l, i) () =
      match l with
      | Nil -> Seq.Nil
      | Cons (_, n, tl) when i = !n -> make (!tl, 0) ()
      | Cons (a, _, _) -> Seq.Cons (a.(i), make (l, i + 1))
    in
    make (l, 0)
  ;;
end
[@@warning "-32"]

(* TODO: make this faster *)
let to_array i = Array.of_list @@ to_list i

exception ExitFind

let[@inline] find_map seq ~f =
  let r = ref None in
  (try
     seq ~f:(fun x ->
       match f x with
       | None -> ()
       | Some _ as res ->
         r := res;
         raise_notrace ExitFind)
   with
   | ExitFind -> ());
  !r
;;

let[@inline] find i ~f = find_map i ~f:(fun x -> if f x then Some x else None)
let[@inline] exists i ~f = find i ~f |> Option.is_some

let[@inline] range ?(stride = 1) ?(start = `inclusive) ?(stop = `exclusive) start_i stop_i ~f =
  if stride = 0 then invalid_arg "Iter.range: stride must be non-zero";
  let start_i = match start with
  | `inclusive -> start_i
  | `exclusive -> start_i + 1 in
  let stop_i = match stop with
  | `inclusive -> stop_i
  | `exclusive -> stop_i - 1 in
  let i = ref start_i in
  while start_i <= !i && !i <= stop_i do
    f !i;
    i := !i + stride;
  done;
;;

(* TODO: make this faster *)
let rev i =
  let l = MList.of_iter i in
  MList.iter_rev l
;;

module Infix = struct
  let ( -- ) start stop = range start stop
end

include Infix

module Private = struct
  module MList = MList

  let to_mlist = MList.of_iter
end

let[@inline] drop n it =
  let count = ref n in
  fun ~f -> it ~f:(fun x -> if !count <= 0 then f x else decr count)
;;

exception ExitTake

let[@inline] take n seq =
  fun ~f:k ->
  let count = ref 0 in
  try
    seq ~f:(fun x ->
      if !count = n then raise_notrace ExitTake;
      incr count;
      k x)
  with
  | ExitTake -> ()
;;

exception ExitTakeWhile

let[@inline] take_while seq ~f:p =
  fun ~f:k ->
  try seq ~f:(fun x -> if p x then k x else raise_notrace ExitTakeWhile) with
  | ExitTakeWhile -> ()
;;

let[@inline] take_while_map it ~f:p =
  fun ~f:k ->
  try
    it ~f:(fun x ->
      match p x with
      | Some x -> k x
      | None -> raise_notrace ExitTakeWhile)
  with
  | ExitTakeWhile -> ()
;;

let[@inline] iter it ~f = it ~f

let[@inline] repeat x =
  fun ~f ->
  while true do
    f x
  done
;;

let scan seq ~init:acc ~f =
  fun ~f:k ->
  k acc;
  let acc = ref acc in
  seq ~f:(fun elt ->
    let acc' = f !acc elt in
    k acc';
    acc := acc')
;;

let[@inline] product outer inner ~f:k = outer ~f:(fun x -> inner ~f:(fun y -> k (x, y)))

exception ExitForall

let[@inline] for_all ~f:p seq =
  try
    seq ~f:(fun x -> if not (p x) then raise_notrace ExitForall);
    true
  with
  | ExitForall -> false
;;

let max_elt t ~compare =
  let m = ref None in
  t ~f:(fun x ->
    match !m with
    | None -> m := Some x
    | Some x' -> if compare x x' > 0 then m := Some x);
  !m
;;

let min_elt t ~compare =
  let m = ref None in
  t ~f:(fun x ->
    match !m with
    | None -> m := Some x
    | Some x' -> if compare x x' < 0 then m := Some x);
  !m
;;