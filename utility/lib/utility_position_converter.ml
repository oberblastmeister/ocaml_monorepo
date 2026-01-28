open Core
module Line_col = Utility_line_col

type t =
  { num_newlines_sum : int array (* prefix sum of number of newlines from [0, p) *)
  ; line_start : int array
    (* given a position p computes the position for the start of the line l containing p *)
  ; line_to_pos : int array (* converts from line l to position p *)
  }
[@@deriving sexp_of]

let create source =
  let len = String.length source in
  let num_newlines_sum = Array.create ~len:(len + 1) 0 in
  let line_start = Array.create ~len:(len + 1) 0 in
  let current_newlines = ref 0 in
  let current_line_start = ref 0 in
  for i = 0 to len - 1 do
    num_newlines_sum.(i) <- !current_newlines;
    line_start.(i) <- !current_line_start;
    if Char.equal source.[i] '\n'
    then (
      incr current_newlines;
      current_line_start := i + 1)
  done;
  (* this allows indexing 1 past the end of the string *)
  num_newlines_sum.(len) <- !current_newlines;
  line_start.(len) <- !current_line_start;
  let line_to_pos = Array.create ~len:(!current_newlines + 1) 0 in
  let current_line = ref 0 in
  for i = 0 to len - 1 do
    if Char.equal source.[i] '\n'
    then begin
      incr current_line;
      line_to_pos.(!current_line) <- i + 1
    end
  done;
  { num_newlines_sum; line_start; line_to_pos }
;;

let pos_to_line_col t pos =
  let line = t.num_newlines_sum.(pos) in
  let col = pos - t.line_start.(pos) in
  { Line_col.line; col }
;;

let line_col_to_pos t { Line_col.line; col } = t.line_to_pos.(line) + col

let%test_module "Utility_pos_to_line_col" =
  (module struct
    let check input pos =
      let t = create input in
      let line_col = pos_to_line_col t pos in
      print_s [%sexp (line_col : Line_col.t)];
      let pos' = line_col_to_pos t line_col in
      [%test_eq: int] pos pos'
    ;;

    let%expect_test "empty" =
      check "" 0;
      [%expect {| ((line 0) (col 0)) |}]
    ;;

    let%expect_test "simple" =
      check "abc" 0;
      [%expect {| ((line 0) (col 0)) |}];
      check "abc" 1;
      [%expect {| ((line 0) (col 1)) |}];
      check "abc" 3;
      [%expect {| ((line 0) (col 3)) |}];
      check "abc\n" 3;
      [%expect {| ((line 0) (col 3)) |}];
      check "abc\n" 4;
      [%expect {| ((line 1) (col 0)) |}]
    ;;

    let%expect_test "multiline" =
      check "a\nb" 0;
      [%expect {| ((line 0) (col 0)) |}];
      check "a\nb" 1;
      [%expect {| ((line 0) (col 1)) |}];
      check "a\nb" 2;
      [%expect {| ((line 1) (col 0)) |}];
      check "a\nb" 3;
      [%expect {| ((line 1) (col 1)) |}]
    ;;

    let%expect_test "trailing newline" =
      check "a\n" 0;
      [%expect {| ((line 0) (col 0)) |}];
      check "a\n" 1;
      [%expect {| ((line 0) (col 1)) |}];
      check "a\n" 2;
      [%expect {| ((line 1) (col 0)) |}]
    ;;

    let%test_unit "inverse" =
      let check_inverse input =
        let t = create input in
        let len = String.length input in
        for i = 0 to len do
          let line_col = pos_to_line_col t i in
          let pos' = line_col_to_pos t line_col in
          [%test_eq: int] i pos'
        done
      in
      check_inverse "";
      check_inverse "abc";
      check_inverse "a\nb";
      check_inverse "a\n"
    ;;
  end)
;;
