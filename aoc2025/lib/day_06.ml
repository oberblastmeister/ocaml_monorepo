open Prelude

let split_whitespace s =
  let s = String.strip s in
  let res = ref [] in
  let rec loop i j =
    if j >= String.length s
    then res := String.slice s i j :: !res
    else if Char.is_whitespace s.[j]
    then begin
      res := String.slice s i j :: !res;
      loop_white (j + 1)
    end
    else loop i (j + 1)
  and loop_white j =
    if j >= String.length s
    then ()
    else if Char.is_whitespace s.[j]
    then loop_white (j + 1)
    else loop j (j + 1)
  in
  loop 0 0;
  List.rev !res
;;

let%expect_test "smoke" =
  print_s [%sexp (split_whitespace "  a b  c e  f    " : string list)];
  [%expect {| (a b c e f) |}]
;;

let get_op op =
  match op with
  | '*' -> 1, ( * )
  | '+' -> 0, ( + )
  | _ -> assert false
;;

let part1 input =
  let lines = String.split_lines input |> Array.of_list in
  let mat =
    Array.slice lines 0 (Array.length lines - 1)
    |> Array.map ~f:(fun line ->
      split_whitespace line |> List.map ~f:Int.of_string |> Array.of_list)
    |> Matrix.transpose
  in
  let ops = split_whitespace lines.(Array.length lines - 1) |> Array.of_list in
  Array.mapi mat ~f:(fun i nums ->
    let init, f = get_op ops.(i).[0] in
    Array.fold nums ~init ~f)
  |> Array.sum (module Int) ~f:Fn.id
  |> Int.to_string
;;

let part2 input =
  let mat = String.split_lines input |> Matrix.of_string_list in
  let ops = mat.(Array.length mat - 1) in
  let mat = Array.slice mat 0 (Array.length mat - 1) in
  let _n, m = Matrix.bounds mat in
  let rec loop curr f acc i =
    let nums = Matrix.column mat i in
    if Array.for_all nums ~f:(Char.equal ' ')
    then begin
      loop_init (acc + curr) (i + 1)
    end
    else begin
      let num =
        Array.fold nums ~init:0 ~f:(fun acc c ->
          if Char.equal c ' ' then acc else (10 * acc) + Int.of_string (String.of_char c))
      in
      let curr = f curr num in
      if i + 1 >= m
      then begin
        acc + curr
      end
      else loop curr f acc (i + 1)
    end
  and loop_init acc i =
    let init, f = get_op ops.(i) in
    loop init f acc i
  in
  loop_init 0 0 |> Int.to_string
;;
