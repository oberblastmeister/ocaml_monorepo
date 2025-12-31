open Prelude

module Edge_kind = struct
  type t =
    | Tree
    | Back
    | Forward
    | Cross
  [@@deriving sexp, equal, compare]
end

module Edge = struct
  type t =
    { v : string
    ; w : string
    ; kind : Edge_kind.t
    }
  [@@deriving sexp]
end

let dfs_classify graph vs ~f =
  let timestamp = ref 0 in
  let enter = String.Table.create () in
  let leave = String.Table.create () in
  let rec dfs v =
    if Hashtbl.find enter v |> Option.is_none
    then begin
      incr timestamp;
      Hashtbl.add_exn enter ~key:v ~data:!timestamp;
      List.iter (Hashtbl.find_exn graph v) ~f:(fun w ->
        if Hashtbl.find enter w |> Option.is_none
        then begin
          f { Edge.v; w; kind = Tree };
          dfs w
        end
        else if Hashtbl.find leave w |> Option.is_none
        then f { Edge.v; w; kind = Back }
        else if Hashtbl.find_exn enter v < Hashtbl.find_exn enter w
        then f { Edge.v; w; kind = Forward }
        else f { Edge.v; w; kind = Cross });
      incr timestamp;
      Hashtbl.add_exn leave ~key:v ~data:!timestamp
    end
  in
  List.iter vs ~f:dfs;
  ()
;;

let parse input =
  let adj = String.Table.create () in
  String.split_lines input
  |> List.iter ~f:(fun line ->
    let v, ws = String.lsplit2_exn line ~on:':' in
    let v, ws = String.strip v, String.strip ws in
    let ws = String.split ws ~on:' ' in
    Hashtbl.add_exn adj ~key:v ~data:ws);
  Hashtbl.add_exn adj ~key:"out" ~data:[];
  adj
;;

let part1 input =
  let adj = parse input in
  let dp = String.Table.create () in
  Hashtbl.add_exn dp ~key:"out" ~data:1;
  let rec dfs v =
    match Hashtbl.find dp v with
    | Some paths -> paths
    | None ->
      let paths = List.iter (Hashtbl.find_exn adj v) |> Iter.map ~f:dfs |> Iter.sum in
      Hashtbl.add_exn dp ~key:v ~data:paths;
      paths
  in
  dfs "you" |> Int.to_string
;;

let part2 input =
  let adj = parse input in
  let dp = Array.init 4 ~f:(fun _ -> String.Table.create ()) in
  Hashtbl.add_exn dp.(0b00) ~key:"out" ~data:1;
  Iter.range 0b01 0b100 ~f:(fun i -> Hashtbl.add_exn dp.(i) ~key:"out" ~data:0);
  let rec dfs i v =
    match Hashtbl.find dp.(i) v with
    | Some paths -> paths
    | None ->
      let m0 = if String.equal v "dac" then 1 lsl 0 else 0 in
      let m1 = if String.equal v "fft" then 1 lsl 1 else 0 in
      let j = i land Int.bit_not m0 land Int.bit_not m1 in
      let paths = List.iter (Hashtbl.find_exn adj v) |> Iter.map ~f:(dfs j) |> Iter.sum in
      Hashtbl.add_exn dp.(i) ~key:v ~data:paths;
      paths
  in
  dfs 0b11 "svr" |> Int.to_string
;;
