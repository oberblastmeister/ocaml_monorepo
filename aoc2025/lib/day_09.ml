open Prelude

let iteri_pairs a ~f =
  for i = 0 to Array.length a - 1 do
    for j = i + 1 to Array.length a - 1 do
      f ((i, j), (a.(i), a.(j)))
    done
  done
;;

let parse input =
  String.split_lines input
  |> List.map ~f:(fun point ->
    let[@warning "-8"] [ x; y ] =
      String.split point ~on:',' |> List.map ~f:Int.of_string
    in
    x, y)
  |> Array.of_list
;;

module Rect = struct
  type t =
    { p1 : Point.t
    ; p2 : Point.t
    }
  [@@deriving sexp]

  let area { p1 = x1, y1; p2 = x2, y2 } = (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1)
  let create p1 p2 = { p1; p2 }

  let normalize { p1 = i1, j1; p2 = i2, j2 } =
    let p1 = min i1 i2, min j1 j2 in
    let p2 = max i1 i2, max j1 j2 in
    { p1; p2 }
  ;;
end

let part1 input =
  let points = parse input in
  iteri_pairs points
  |> Iter.map ~f:(fun (_, (p1, p2)) -> Rect.create p1 p2 |> Rect.area)
  |> Iter.max_elt ~compare
  |> Option.value_exn
  |> Int.to_string
;;

module Line = struct
  type t =
    { start : Point.t
    ; stop : Point.t
    }
  [@@deriving sexp]

  let create start stop = { start; stop }
end

module Axis = struct
  type t =
    | I
    | J
  [@@deriving sexp, equal, compare]
end

module Perp_line = struct
  type t =
    { c : int
    ; range : Range.t
    ; axis : Axis.t
    }
  [@@deriving sexp, equal, compare]

  let of_line_exn { Line.start; stop } =
    if fst start = fst stop
    then
      { c = fst start
      ; range = min (snd start) (snd stop), max (snd stop) (snd start) + 1
      ; axis = J
      }
    else if snd start = snd stop
    then
      { c = snd start
      ; range = min (fst start) (fst stop), max (fst start) (fst stop) + 1
      ; axis = I
      }
    else failwith "line was not perpendicular"
  ;;

  let create p1 p2 = Line.create p1 p2 |> of_line_exn
end

module Orientation = struct
  type t =
    | Neg
    | Pos
  [@@deriving sexp]

  let to_op = function
    | Neg -> ( <= )
    | Pos -> ( >= )
  ;;
end

module Oriented_line = struct
  type t =
    { line : Perp_line.t
    ; orientation : Orientation.t
    }
  [@@deriving sexp]

  let create p1 p2 orientation = { line = Perp_line.create p1 p2; orientation }
end

let get_rect_lines rect =
  let { Rect.p1 = i1, j1; p2 = i2, j2 } = Rect.normalize rect in
  let lines : Oriented_line.t list =
    let create = Oriented_line.create in
    [ create (i1, j1) (i1, j2) Neg
    ; create (i2, j1) (i2, j2) Pos
    ; create (i1, j1) (i2, j1) Neg
    ; create (i1, j2) (i2, j2) Pos
    ]
  in
  lines
;;

let is_line_covered (lines : Perp_line.t list) (o_line : Oriented_line.t) =
  let lines =
    List.filter lines ~f:(fun line ->
      Axis.equal line.axis o_line.line.axis
      &&
      match o_line.orientation with
      | Neg -> line.c <= o_line.line.c
      | Pos -> o_line.line.c <= line.c)
  in
  let ranges = List.map lines ~f:(fun line -> line.range) |> Range.sort_and_merge in
  List.exists ranges ~f:(fun range -> Range.contains ~larger:range o_line.line.range)
;;

let is_valid_rect lines rect =
  let o_lines = get_rect_lines rect in
  List.for_all o_lines ~f:(is_line_covered lines)
;;

let part2 input =
  let points = parse input |> Array.map ~f:(fun (i, j) -> j, i) in
  let lines =
    Iter.range 0 (Array.length points)
    |> Iter.concat_map ~f:(fun i ->
      let p = points.(i) in
      let qs =
        Iter.range 0 (Array.length points)
        |> Iter.filter ~f:(fun j -> i <> j)
        |> Iter.map ~f:(fun j -> points.(j))
        |> Iter.filter ~f:(fun q -> fst p = fst q || snd p = snd q)
        |> Iter.to_list
      in
      assert (List.length qs = 2);
      List.iter qs |> Iter.map ~f:(fun q -> Line.create p q |> Perp_line.of_line_exn))
    |> Iter.to_list
    |> List.dedup_and_sort ~compare:Perp_line.compare
  in
  let pairs = iteri_pairs points |> Iter.to_list in
  let valid =
    pairs
    |> List.map ~f:(fun (_, (p1, p2)) -> Rect.create p1 p2)
    |> List.filter ~f:(fun rect -> is_valid_rect lines rect)
  in
  valid
  |> List.map ~f:Rect.area
  |> List.max_elt ~compare
  |> Option.value_exn
  |> Int.to_string
;;
