open Core
open Aoc2025.Utils

let () =
  let args = Sys.get_argv () |> Array.to_list in
  let day, input_file =
    match args with
    | [ _; i ] -> Int.of_string i, None
    | [ _; i; file ] -> Int.of_string i, Some file
    | _ -> failwith "Invalid arguments"
  in
  printf "%d\n" day;
  let (module CurrentDay : Day.S) =
    Aoc2025.(
      match day with
      | 1 -> (module Day_01)
      | 2 -> (module Day_02)
      | 3 -> (module Day_03)
      | 4 -> (module Day_04)
      | 5 -> (module Day_05)
      | 6 -> (module Day_06)
      | 7 -> (module Day_07)
      | 8 -> (module Day_08)
      | 9 -> (module Day_09)
      | 10 -> (module Day_10)
      | 11 -> (module Day_11)
      | 12 -> (module Day_12)
      | _ -> failwith "Day not implemented")
  in
  let input_text =
    match input_file with
    | None ->
      let num = day |> Int.to_string in
      let num = if String.length num = 1 then "0" ^ num else num in
      In_channel.read_all @@ "input/" ^ num
    | Some input_file ->
      if String.equal input_file "-"
      then In_channel.input_all Stdio.stdin
      else In_channel.read_all input_file
  in
  let input_text = String.strip input_text in
  let answer1_text = input_text |> CurrentDay.part1 in
  let answer2_text = input_text |> CurrentDay.part2 in
  printf "\nPart 1 solution: %s\nPart 2 solution: %s\n" answer1_text answer2_text
;;
