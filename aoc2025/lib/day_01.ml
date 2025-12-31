open Core

let part1 input =
  let n = ref 50 in
  let c = ref 0 in
  String.split_lines input
  |> List.iter ~f:(fun line ->
    let x = line.[0] in
    let d = String.slice line 1 0 |> Int.of_string in
    if Char.equal x 'L' then n := !n - d else n := !n + d;
    if !n % 100 = 0 then incr c;
    ());
  sprintf "%d\n" !c
;;

let part2 input =
  let n = ref (1000_000_000 + 50) in
  let c = ref 0 in
  String.split_lines input
  |> List.iter ~f:(fun line ->
    let x = line.[0] in
    let d = String.slice line 1 0 |> Int.of_string in
    let n0 = !n in
    let n1 = if Char.equal x 'L' then n0 - d else n0 + d in
    let inc =
      if n1 > n0 then (n1 / 100) - (n0 / 100) else ((n0 + 99) / 100) - ((n1 + 99) / 100)
    in
    c := !c + inc;
    n := n1;
    ());
  sprintf "%d\n" !c
;;
