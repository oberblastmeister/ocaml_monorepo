open Core

let find_largest bank =
  let ix = ref 0 in
  let d = ref (Int.of_string (String.of_char bank.[0])) in
  for i = 1 to String.length bank - 1 do
    let d' = Int.of_string (String.of_char bank.[i]) in
    if d' > !d
    then begin
      ix := i;
      d := d'
    end
  done;
  !ix, !d
;;

let part1 input =
  String.split_lines input
  |> List.map ~f:(fun bank ->
    let i, d1 = find_largest (String.slice bank 0 (String.length bank - 1)) in
    let _, d2 = find_largest (String.slice bank (i + 1) 0) in
    (d1 * 10) + d2)
  |> List.sum (module Int) ~f:Fn.id
  |> Int.to_string
;;

let part2 input =
  String.split_lines input
  |> List.map ~f:(fun bank ->
    let k = ref 0 in
    let r = ref 0 in
    let m = ref (Int.of_float 1e11) in
    for i = 0 to 11 do
      let j, d1 =
        find_largest (String.slice bank !k (String.length bank - (12 - i - 1)))
      in
      r := !r + (!m * d1);
      m := !m / 10;
      k := !k + j + 1
    done;
    !r)
  |> List.sum (module Int) ~f:Fn.id
  |> Int.to_string
;;
