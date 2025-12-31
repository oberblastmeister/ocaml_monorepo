open Prelude

module Array_union_find : sig
  type id = int [@@deriving sexp]
  type t [@@deriving sexp]

  val create : int -> t
  val find : t -> id -> id
  val size : t -> id -> int
  val union : t -> id -> id -> unit
end = struct
  type id = int [@@deriving sexp]

  type t =
    { size : int array
    ; parent : int array
    }
  [@@deriving sexp]

  let create len =
    let size = Array.create ~len 1 in
    let parent = Array.init len ~f:Fn.id in
    { size; parent }
  ;;

  let rec find t id =
    if t.parent.(id) <> id
    then begin
      let rep = find t t.parent.(id) in
      t.parent.(id) <- rep;
      rep
    end
    else id
  ;;

  let size t id =
    let id = find t id in
    t.size.(id)
  ;;

  let union t id1 id2 =
    let id1, id2 = if t.size.(id1) < t.size.(id2) then id2, id1 else id1, id2 in
    let id1, id2 = find t id1, find t id2 in
    if id1 <> id2
    then begin
      t.parent.(id2) <- id1;
      t.size.(id1) <- t.size.(id1) + t.size.(id2)
    end
  ;;
end

let parse input =
  String.split_lines input
  |> List.map ~f:(fun point ->
    let[@warning "-8"] [ x; y; z ] =
      String.split point ~on:',' |> List.map ~f:Int.of_string
    in
    x, y, z)
  |> Array.of_list
;;

let get_pairs points =
  let pairs = ref [] in
  for i = 0 to Array.length points - 1 do
    for j = i + 1 to Array.length points - 1 do
      pairs := ((i, j), (points.(i), points.(j))) :: !pairs
    done
  done;
  let pairs = Array.of_list !pairs in
  Array.sort
    ~compare:(Comparable.lift ~f:(fun (_, (p1, p2)) -> Point3.dist p1 p2) Int.compare)
    pairs;
  pairs
;;

module Uf = Array_union_find

let part1 input =
  let points = parse input in
  let pairs = get_pairs points in
  let uf = Uf.create (Array.length points) in
  Array.iter pairs |> Iter.take 1000 |> Iter.iter ~f:(fun ((i, j), _) -> Uf.union uf i j);
  List.range 0 (Array.length points)
  |> List.map ~f:(Uf.find uf)
  |> List.dedup_and_sort ~compare
  |> List.map ~f:(Uf.size uf)
  |> List.sort ~compare:(Comparable.reverse compare)
  |> Fn.flip List.take 3
  |> List.fold ~init:1 ~f:( * )
  |> Int.to_string
;;

let part2 input =
  let points = parse input in
  let pairs = get_pairs points in
  let uf = Uf.create (Array.length points) in
  Array.find_map pairs ~f:(fun ((i, j), ((x1, _, _), (y1, _, _))) ->
    Uf.union uf i j;
    if Uf.size uf i = Array.length points then Some (x1 * y1) else None)
  |> Option.value_exn
  |> Int.to_string
;;
