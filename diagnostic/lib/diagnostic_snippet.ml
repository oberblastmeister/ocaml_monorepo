open Core
module File_span = Diagnostic_file_span
module Position_converter = Location.Position_converter
module Line_col = Location.Line_col

type t = File_span.t [@@deriving sexp_of]

module File = struct
  type t =
    { source : string
    ; position_converter : Position_converter.t
    }

  let create source = { source; position_converter = Position_converter.create source }
end

type files = File.t String.Map.t

(* the padding to put before the snippet *)
let default_padding = 0

let format_snippet files snippet =
  let file = Map.find_exn files snippet.File_span.file in
  let source = file.File.source in
  let converter = file.File.position_converter in
  let start_lc = Position_converter.pos_to_line_col converter snippet.start in
  let stop_lc =
    (* this can happen when we have positions on top of virtual tokens which have length 0 *)
    (* we still want to show these positions so extend it to length 1 *)
    Position_converter.pos_to_line_col converter snippet.stop
  in
  let line_no = start_lc.Line_col.line in
  let line_display = line_no + 1 in
  let line_str = Int.to_string line_display in
  let width = Int.max 0 (String.length line_str) in
  let padding = String.make width ' ' in
  let line_start_pos =
    Position_converter.line_col_to_pos converter { Line_col.line = line_no; col = 0 }
  in
  let line_end_pos =
    match String.index_from source line_start_pos '\n' with
    | Some i -> i
    | None -> String.length source
  in
  let line_content =
    String.sub source ~pos:line_start_pos ~len:(line_end_pos - line_start_pos)
  in
  let is_multiline = start_lc.Line_col.line < stop_lc.Line_col.line in
  let underline_len =
    (* the underline can be of length 0 for some virtual tokens, so force it to be length 1 *)
    max 1
    @@
    if is_multiline
    then String.length line_content - start_lc.Line_col.col
    else snippet.stop - snippet.start
  in
  let underline_str = String.make underline_len '^' in
  let suffix = if is_multiline then "..." else "" in
  let col_display = start_lc.Line_col.col + 1 in
  sprintf
    "%s--> %s:%d:%d\n%s |\n%*d | %s\n%s | %s%s%s"
    padding
    snippet.File_span.file
    line_display
    col_display
    padding
    width
    line_display
    line_content
    padding
    (String.make start_lc.Line_col.col ' ')
    underline_str
    suffix
;;

let%test_module "format_snippet" =
  (module struct
    let setup source =
      let file = File.create source in
      let files = String.Map.of_alist_exn [ "test.ml", file ] in
      files
    ;;

    let check source start stop =
      let files = setup source in
      let snippet = { File_span.file = "test.ml"; start; stop } in
      let formatted = format_snippet files snippet in
      print_endline formatted
    ;;

    let%expect_test "simple" =
      check "let x = 1" 4 5;
      [%expect
        {|
         --> test.ml:1:5
          |
        1 | let x = 1
          |     ^
        |}]
    ;;

    let%expect_test "another" =
      check "xyz\n" 3 4;
      [%expect
        {|
         --> test.ml:1:4
          |
        1 | xyz
          |    ^...
        |}];
      check "xyz\n" 3 3;
      [%expect
        {|
         --> test.ml:1:4
          |
        1 | xyz
          |    ^
        |}];
      check "xyz" 2 3;
      [%expect
        {|
         --> test.ml:1:3
          |
        1 | xyz
          |   ^
        |}];
      check "xyz" 2 2;
      [%expect
        {|
         --> test.ml:1:3
          |
        1 | xyz
          |   ^
        |}];
      check "xyz" 3 3;
      [%expect
        {|
         --> test.ml:1:4
          |
        1 | xyz
          |    ^
        |}]
    ;;

    let%expect_test "indexing past newline" =
      check "xyz\n" 4 4;
      [%expect
        {|
         --> test.ml:2:1
          |
        2 |
          | ^
        |}]
    ;;

    let%expect_test "multiline" =
      check "let x = 1\nlet y = 2" 4 14;
      [%expect
        {|
         --> test.ml:1:5
          |
        1 | let x = 1
          |     ^^^^^...
        |}]
    ;;

    let%expect_test "large line number" =
      let prefix = String.make 10000 '\n' in
      let source = prefix ^ "let x = 1" in
      let start = 10000 + 4 in
      let stop = 10000 + 5 in
      check source start stop;
      [%expect
        {|
             --> test.ml:10001:5
              |
        10001 | let x = 1
              |     ^
        |}]
    ;;
  end)
;;
