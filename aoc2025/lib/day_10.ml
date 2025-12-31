open Prelude

let parse_line line =
  let indicators, rest = String.lsplit2_exn line ~on:']' in
  let indicators = String.strip_prefix_exn indicators ~prefix:"[" |> String.strip in
  let buttons, requirements = String.lsplit2_exn rest ~on:'{' in
  let buttons = String.strip buttons in
  let requirements = String.strip_suffix_exn requirements ~suffix:"}" |> String.strip in
  assert (String.length indicators <= Int.floor_log2 Int.max_value);
  let indicators =
    String.to_list indicators
    |> List.mapi ~f:(fun i c ->
      if Char.equal c '#' then 1 lsl i else if Char.equal c '.' then 0 else assert false)
    |> List.fold ~init:0 ~f:( lor )
  in
  let buttons =
    String.split buttons ~on:' '
    |> List.map ~f:(fun button ->
      String.strip_prefix_exn button ~prefix:"("
      |> String.strip_suffix_exn ~suffix:")"
      |> String.split ~on:','
      |> List.map ~f:Int.of_string)
  in
  assert (List.length buttons <= 16);
  let requirements = String.split requirements ~on:',' |> List.map ~f:Int.of_string in
  indicators, buttons, requirements
;;

let machine_fewest_indicator_presses (indicators, buttons, _requirements) =
  let buttons =
    List.map buttons ~f:(fun indices ->
      List.map indices ~f:(fun i -> 1 lsl i) |> List.fold ~init:0 ~f:( lor ))
    |> Array.of_list
  in
  let res = ref Int.max_value in
  for choice = 0 to (1 lsl Array.length buttons) - 1 do
    let curr = ref 0 in
    let presses = ref 0 in
    for i = 0 to Array.length buttons - 1 do
      if choice land (1 lsl i) <> 0
      then begin
        incr presses;
        curr := !curr lxor buttons.(i)
      end
    done;
    let curr, presses = !curr, !presses in
    if curr = indicators then res := min !res presses;
    ()
  done;
  let res = !res in
  assert (res <> Int.max_value);
  res
;;

module Vector = struct
  type t = int array [@@deriving sexp, equal, compare]

  let lift1 v ~f = Array.init (Array.length v) ~f:(fun i -> f v.(i))

  let lift2 v w ~f =
    assert (Array.length v = Array.length w);
    Array.init (Array.length v) ~f:(fun i -> f v.(i) w.(i))
  ;;

  let scale c = lift1 ~f:(( * ) c)
  let sub = lift2 ~f:( - )
  let add = lift2 ~f:( + )
  let is_zero v = Array.for_all v ~f:(( = ) 0)
end

let sort_both (joltage, buttons) =
  let buttons = Array.transpose_exn buttons in
  let joltage, buttons =
    Array.zip_exn joltage buttons
    |> Array.sorted_copy ~compare:(Comparable.lift ~f:fst compare)
    |> Array.unzip
  in
  let buttons = Array.transpose_exn buttons in
  joltage, buttons
;;

let rec solve mat v =
  if Array.is_empty v
  then Some 0
  else if Array.is_empty mat
  then begin
    if Array.for_all v ~f:(( = ) 0) then Some 0 else None
  end
  else if mat.(0).(0) = 0
  then begin
    if Iter.range 0 (Array.length mat) |> Iter.for_all ~f:(fun i -> mat.(i).(0) = 0)
    then begin
      if v.(0) = 0
      then solve (Array.map mat ~f:(fun a -> Array.slice a 1 0)) (Array.slice v 1 0)
      else None
    end
    else begin
      solve (Array.append (Array.slice mat 1 0) [| mat.(0) |]) v
    end
  end
  else begin
    Iter.range ~stop:`inclusive 0 v.(0)
    |> Iter.filter_map ~f:(fun c ->
      let v' = Vector.scale c mat.(0) in
      let v'' = Vector.sub v v' in
      if Array.for_all v'' ~f:(fun x -> x >= 0)
      then begin
        let v, mat = v'', Array.slice mat 1 0 in
        (* let v, mat = sort_both (v, mat) in *)
        Option.map (solve mat v) ~f:(( + ) c)
      end
      else None)
    |> Iter.min_elt ~compare
  end
;;

let machine_fewest_joltage_presses (_indicators, buttons, joltage) =
  (* print_s [%message (buttons : int list list) (joltage : int list)]; *)
  let joltage = Array.of_list joltage in
  let buttons =
    List.map buttons ~f:(fun indices ->
      let v = Array.create ~len:(Array.length joltage) 0 in
      List.iter indices ~f:(fun i -> v.(i) <- v.(i) + 1);
      v)
    |> Array.of_list
  in
  let joltage, buttons = sort_both (joltage, buttons) in
  solve buttons joltage |> Option.value_exn ~message:"failed to solve"
;;

let part1 input =
  String.split_lines input
  |> List.map ~f:parse_line
  |> List.map ~f:machine_fewest_indicator_presses
  |> List.sum (module Int) ~f:Fn.id
  |> Int.to_string
;;

let part2 input =
  String.split_lines input
  |> List.map ~f:parse_line
  |> List.mapi ~f:(fun i m ->
    print_s [%message "solving" (i : int)];
    let res = machine_fewest_joltage_presses m in
    print_s [%message (res : int)];
    res)
  |> List.sum (module Int) ~f:Fn.id
  |> Int.to_string
;;
