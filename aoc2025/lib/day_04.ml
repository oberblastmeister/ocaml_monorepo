open Prelude

let valid mat p =
  let c =
    Point.around_box p
    |> List.count ~f:(fun p ->
      Point.in_bounds (Point.get_bounds mat) p && Char.equal mat.:(p) '@')
  in
  c < 4
;;

let part1 input =
  let mat = String.split_lines input |> Matrix.of_string_list in
  let c = ref 0 in
  Matrix.iteri mat ~f:(fun p x ->
    if Char.equal x '@' && valid mat p
    then begin
      incr c
    end);
  Int.to_string !c
;;

let remove mat =
  let c = ref 0 in
  Matrix.iteri mat ~f:(fun p x ->
    if Char.equal x '@' && valid mat p
    then begin
      incr c;
      mat.:(p) <- 'x'
    end);
  !c
;;

let part2 input =
  let mat = String.split_lines input |> Matrix.of_string_list in
  let rec loop k =
    let c = remove mat in
    if c = 0 then k else loop (k + c)
  in
  Int.to_string (loop 0)
;;
