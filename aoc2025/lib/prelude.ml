include Core

module Matrix = struct
  type 'a t = 'a array array [@@deriving sexp, equal, compare]

  let of_string_list t = List.map ~f:String.to_array t |> Array.of_list
  let of_list_list t = List.map ~f:Array.of_list t |> Array.of_list
  let iteri t ~f = Array.iteri t ~f:(fun i a -> Array.iteri a ~f:(fun j x -> f (i, j) x))
  let iter t ~f = iteri t ~f:(fun _ x -> f x)
  let get t (i, j) = t.(i).(j)
  let set t (i, j) x = t.(i).(j) <- x
  let init (n, m) ~f = Array.init n ~f:(fun i -> Array.init m ~f:(fun j -> f (i, j)))
  let create (n, m) x = Array.init n ~f:(fun _ -> Array.create ~len:m x)
  let bounds t = Array.length t, Array.length t.(0)

  let column t i =
    let n, _m = bounds t in
    Array.init n ~f:(fun j -> t.(j).(i))
  ;;

  let transpose t =
    let n, m = bounds t in
    init (m, n) ~f:(fun (i, j) -> get t (j, i))
  ;;
end

let ( .:() ) = Matrix.get
let ( .:()<- ) = Matrix.set

module Range = struct
  type t = int * int [@@deriving sexp, equal, compare]

  let length (i, j) =
    assert (i <= j);
    j - i
  ;;

  let contains ~larger:(x1, y1) (x2, y2) =
    assert (x1 <= y1);
    assert (x2 <= y2);
    x1 <= x2 && y2 <= y1
  ;;

  let merge ranges =
    let merged = ref [] in
    List.iter ranges ~f:(fun range ->
      match List.hd !merged with
      | None -> merged := [ range ]
      | Some range' ->
        if snd range' < fst range
        then merged := range :: !merged
        else merged := (fst range', max (snd range') (snd range)) :: List.tl_exn !merged);
    !merged
  ;;

  let sort_and_merge ranges =
    List.sort ranges ~compare:(Comparable.lift ~f:fst Int.compare) |> merge
  ;;
end

let%expect_test "Range.merge" =
  print_s [%sexp (Range.merge [ 1, 2; 2, 3 ] : Range.t list)];
  [%expect {| ((1 3)) |}];
  print_s [%sexp (Range.merge [ 1, 6; 5, 10 ] : Range.t list)];
  [%expect {| ((1 10)) |}]
;;

module Point = struct
  type t = int * int [@@deriving sexp]

  let ( * ) c (x, y) = c * x, c * y
  let ( - ) (x, y) (x', y') = x - x', y - y'
  let ( + ) (x, y) (x', y') = x + x', y + y'
  let ( <. ) (x, y) (x', y') = x < x' && y < y'
  let ( <=. ) (x, y) (x', y') = x <= x' && y <= y'
  let in_bounds bounds p = (0, 0) <=. p && p <. bounds
  let around_plus (i, j) = Int.[ i + 1, j; i - 1, j; i, j + 1; i, j - 1 ]

  let around_box (i, j) =
    Int.
      [ i + 1, j
      ; i - 1, j
      ; i, j + 1
      ; i, j - 1
      ; i - 1, j - 1
      ; i - 1, j + 1
      ; i + 1, j - 1
      ; i + 1, j + 1
      ]
  ;;

  let get_bounds matrix = Array.length matrix, Array.length matrix.(0)
  let rotate (x, y) = y, -x
  let ( = ) = Tuple2.equal ~eq1:equal ~eq2:equal
  let ( <> ) x y = not (x = y)

  let iter_line p v bounds ~f =
    let pr = ref p in
    while in_bounds bounds !pr do
      f !pr;
      pr := !pr + v
    done
  ;;
end

module Point3 = struct
  type t = int * int * int [@@deriving sexp]

  let dist (x1, y1, z1) (x2, y2, z2) =
    let sq x = x * x in
    sq (x2 - x1) + sq (y2 - y1) + sq (z2 - z1)
  ;;
end

module Vector = struct end

module String = struct
  include String

  let split_on t ~on = String.Search_pattern.split_on (Search_pattern.create on) t

  let lsplit2_on_exn s ~on =
    let pat = String.Search_pattern.create on in
    let i = String.Search_pattern.index_exn pat ~in_:s in
    String.subo ~len:i s, String.subo ~pos:(i + String.length on) s
  ;;

  let strip_prefix s ~prefix =
    if String.is_prefix s ~prefix
    then Some (String.drop_prefix s (String.length prefix))
    else None
  ;;

  let strip_suffix s ~suffix =
    if String.is_suffix s ~suffix
    then Some (String.drop_suffix s (String.length suffix))
    else None
  ;;

  let strip_prefix_exn s ~prefix =
    if String.is_prefix s ~prefix
    then String.drop_prefix s (String.length prefix)
    else failwith "String.strip_prefix_exn: string did not start with prefix"
  ;;

  let strip_suffix_exn s ~suffix =
    if String.is_suffix s ~suffix
    then String.drop_suffix s (String.length suffix)
    else failwith "String.strip_suffix_exn: string did not start with suffix"
  ;;
end

module Iter = Functional.Iter
