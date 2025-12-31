open Prelude

let parse input =
  let ranges, ids = String.lsplit2_on_exn input ~on:"\n\n" in
  let ranges =
    String.split_lines ranges
    |> List.map ~f:(fun range ->
      let start, stop = String.lsplit2_exn range ~on:'-' in
      let start = Int.of_string start in
      let stop = Int.of_string stop in
      start, stop + 1)
  in
  let ids = String.split_lines ids |> List.map ~f:Int.of_string in
  ranges, ids
;;

let part1 input =
  let ranges, ids = parse input in
  ids
  |> List.count ~f:(fun id ->
    List.exists ranges ~f:(fun (start, stop) -> start <= id && id < stop))
  |> Int.to_string
;;

let part2 input =
  let ranges, _ids = parse input in
  let ranges = List.sort ~compare:(Comparable.lift ~f:fst compare) ranges in
  let merged = Range.merge ranges in
  List.map merged ~f:(fun (i, j) -> j - i)
  |> List.sum (module Int) ~f:Fn.id
  |> Int.to_string
;;
