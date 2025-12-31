open Prelude

module Region = struct
  type t =
    { length : int
    ; width : int
    ; amounts : int list
    }
  [@@deriving sexp]
end

module Piece = struct
  type t = char Matrix.t

  let num_tiles t = Matrix.iter t |> Iter.filter ~f:(Char.equal '#') |> Iter.count
end

let parse input =
  let parts = String.split_on input ~on:"\n\n" |> Array.of_list in
  let pieces =
    Array.slice parts 0 (Array.length parts - 1)
    |> Array.map ~f:(fun piece ->
      let _, piece = String.lsplit2_on_exn piece ~on:":\n" in
      String.split_lines piece |> Matrix.of_string_list)
  in
  let regions =
    Array.get parts (Array.length parts - 1)
    |> String.split_lines
    |> List.map ~f:(fun region ->
      let dims, amounts = String.lsplit2_on_exn region ~on:": " in
      let module M = Base.String.Utf8 in
      let[@warning "-8"] [ length; width ] =
        String.split dims ~on:'x' |> List.map ~f:Int.of_string
      in
      let amounts = String.split amounts ~on:' ' |> List.map ~f:Int.of_string in
      
      { Region.length; width; amounts })
  in
  pieces, regions
;;

let part1 input =
  let pieces, regions = parse input in
  let num_no_fit = ref 0 in
  let num_fit = ref 0 in
  List.iter regions ~f:(fun region ->
    let num_tiles =
      List.iter region.amounts
      |> Iter.enumerate
      |> Iter.map ~f:(fun (i, amount) -> Piece.num_tiles pieces.(i) * amount)
      |> Iter.sum
    in
    let num_pieces = List.sum (module Int) region.amounts ~f:Fn.id in
    let num_length_normal = region.length / 3 in
    let num_width_normal = region.width / 3 in
    let tiles_capacity = region.length * region.width in
    let piece_capacity_bound = num_length_normal * num_width_normal in
    if num_pieces <= piece_capacity_bound
    then incr num_fit
    else begin
      (* the input just so happens to be easy to decide *)
      assert (num_tiles > tiles_capacity);
      incr num_no_fit
    end);
  let num_fit = !num_fit in
  Int.to_string num_fit
;;

let part2 _ = ""
