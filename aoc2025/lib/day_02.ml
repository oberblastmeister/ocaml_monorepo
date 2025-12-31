open Core

let valid1 n =
  let s = Int.to_string n in
  if String.length s % 2 = 1
  then true
  else begin
    let s' = String.slice s 0 (String.length s / 2) in
    not (String.equal s (s' ^ s'))
  end
;;

let valid2 n =
  With_return.with_return_option (fun lab ->
    let s = Int.to_string n in
    for i = 2 to String.length s do
      if String.length s % i = 0
      then begin
        let s' = String.slice s 0 (String.length s / i) in
        if String.equal (String.concat (Array.to_list (Array.create ~len:i s'))) s
        then lab.return false
      end
    done)
  |> Option.value ~default:true
;;

let part1 input =
  let c = ref 0 in
  String.split input ~on:','
  |> List.iter ~f:(fun range ->
    let[@warning "-8"] [ start; stop ] = String.split range ~on:'-' in
    let start = Int.of_string start in
    let stop = Int.of_string stop in
    for i = start to stop do
      if not (valid1 i) then c := !c + i
    done;
    ());
  Int.to_string !c
;;

let part2 input =
  let c = ref 0 in
  String.split input ~on:','
  |> List.iter ~f:(fun range ->
    let[@warning "-8"] [ start; stop ] = String.split range ~on:'-' in
    let start = Int.of_string start in
    let stop = Int.of_string stop in
    for i = start to stop do
      if not (valid2 i) then c := !c + i
    done;
    ());
  Int.to_string !c
;;
