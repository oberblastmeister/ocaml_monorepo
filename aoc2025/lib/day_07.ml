open Prelude

let part1 input =
  let mat = String.split_lines input |> Matrix.of_string_list in
  let start_index =
    Array.findi mat.(0) ~f:(fun _ -> Char.equal 'S') |> Option.value_exn |> fst
  in
  mat.:(1, start_index) <- '|';
  let n, m = Matrix.bounds mat in
  let c = ref 0 in
  for i = 2 to n - 1 do
    for j = 0 to m - 1 do
      if Char.equal mat.:(i, j) '.' && Char.equal mat.:(i - 1, j) '|'
      then mat.:(i, j) <- '|'
      else if Char.equal mat.:(i, j) '^' && Char.equal mat.:(i - 1, j) '|'
      then begin
        incr c;
        mat.:(i, j - 1) <- '|';
        mat.:(i, j + 1) <- '|'
      end
    done
  done;
  Int.to_string !c
;;

let part2 input =
  let mat = String.split_lines input |> Matrix.of_string_list in
  let start_index =
    Array.findi mat.(0) ~f:(fun _ -> Char.equal 'S') |> Option.value_exn |> fst
  in
  let n, m = Matrix.bounds mat in
  let dp = Matrix.create (n, m) 1 in
  for i = n - 2 downto 1 do
    for j = 0 to m - 1 do
      if Char.equal mat.:(i, j) '.'
      then dp.:(i, j) <- dp.:(i + 1, j)
      else if Char.equal mat.:(i, j) '^'
      then dp.:(i, j) <- dp.:(i + 1, j - 1) + dp.:(i + 1, j + 1)
    done
  done;
  Int.to_string dp.:(1, start_index)
;;
