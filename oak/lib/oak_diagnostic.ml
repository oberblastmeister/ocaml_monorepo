open Core
module Snippet = Diagnostic.Snippet

module Ansi = struct
  let red s = sprintf "\027[31m%s\027[0m" s
  let cyan s = sprintf "\027[36m%s\027[0m" s
  let bold s = sprintf "\027[1m%s\027[0m" s
end

module Text = struct
  type t = Format.formatter -> unit

  let sexp_of_t _ = Sexp.Atom "<text>"
  let of_string s : t = fun fmt -> Format.pp_print_string fmt s

  let to_string ~width (t : t) =
    let buf = Buffer.create 128 in
    let fmt = Format.formatter_of_buffer buf in
    Format.pp_set_margin fmt width;
    t fmt;
    Format.pp_print_flush fmt ();
    Buffer.contents buf
  ;;
end

module Severity = struct
  type t =
    | Warning
    | Error
    | Note
  [@@deriving sexp_of]

  let format ~color t =
    let label =
      match t with
      | Error -> "error:"
      | Warning -> "warning:"
      | Note -> "note:"
    in
    if color
    then (
      match t with
      | Error -> Ansi.red (Ansi.bold label)
      | Warning | Note -> Ansi.cyan (Ansi.bold label))
    else label
  ;;
end

module Part = struct
  type t =
    { severity : Severity.t
    ; message : Text.t
    ; snippet : Snippet.t option
    }
  [@@deriving sexp_of]
end

module Code = struct
  type t = Parse_error [@@deriving sexp_of]

  let to_string = function
    | Parse_error -> "[E0001]"
  ;;

  let description = function
    | Parse_error -> "Parse error"
  ;;
end

type t =
  { code : Code.t
  ; filename : Filename.t
  ; parts : Part.t list
  }
[@@deriving sexp_of]

let format_header ~width code filename =
  let left = sprintf "---- %s %s " (Code.description code) (Code.to_string code) in
  let right = sprintf " %s" filename in
  let dashes_needed = width - String.length left - String.length right in
  let dashes = if dashes_needed > 0 then String.make dashes_needed '-' else "" in
  sprintf "%s%s%s" left dashes right
;;

let format_part ~width ~color files part =
  let label = Severity.format ~color part.Part.severity in
  let message = Text.to_string ~width part.Part.message in
  match part.Part.snippet with
  | Some snippet ->
    let snippet_str = Snippet.format_snippet files snippet in
    sprintf "%s %s\n%s" label message snippet_str
  | None -> sprintf "%s %s" label message
;;

let format ?(width = 80) ?(color = true) ~files diagnostic =
  let header = format_header ~width diagnostic.code diagnostic.filename in
  let parts_str =
    List.map diagnostic.parts ~f:(format_part ~width ~color files)
    |> String.concat ~sep:"\n"
  in
  sprintf "%s\n%s" header parts_str
;;

let print ?(width = 80) ?(color = true) ~files diagnostic =
  print_endline (format ~width ~color ~files diagnostic)
;;

let%test_module "format" =
  (module struct
    let make_snippet ~file ~start ~stop : Snippet.t = { file; start; stop }

    let setup sources =
      List.map sources ~f:(fun (name, source) -> name, Snippet.File.create source)
      |> String.Map.of_alist_exn
    ;;

    let%expect_test "single error" =
      let source = "let x = 1 + \"hello\"" in
      let files = setup [ "test.ml", source ] in
      let diagnostic =
        { code = Parse_error
        ; filename = "test.ml"
        ; parts =
            [ { Part.severity = Error
              ; message = Text.of_string "type mismatch: expected int, got string"
              ; snippet = Some (make_snippet ~file:"test.ml" ~start:12 ~stop:19)
              }
            ]
        }
      in
      print ~width:60 ~color:false ~files diagnostic;
      [%expect
        {|
        ---- Parse error [E0001] --------------------------- test.ml
        error: type mismatch: expected int, got string
             test.ml:1:13
             |
           1 | let x = 1 + "hello"
             |             ~~~~~~~
        |}]
    ;;

    let%expect_test "error with notes" =
      let source =
        "let rec fib n =\n  if n <= 1 then n\n  else fib (n - 1) + fib (n - 2)"
      in
      let files = setup [ "test.ml", source ] in
      let diagnostic =
        { code = Parse_error
        ; filename = "test.ml"
        ; parts =
            [ { Part.severity = Error
              ; message = Text.of_string "undefined function"
              ; snippet = Some (make_snippet ~file:"test.ml" ~start:25 ~stop:28)
              }
            ; { Part.severity = Note
              ; message = Text.of_string "did you mean 'fib'?"
              ; snippet = Some (make_snippet ~file:"test.ml" ~start:8 ~stop:11)
              }
            ; { Part.severity = Note
              ; message = Text.of_string "functions must be defined before use"
              ; snippet = None
              }
            ]
        }
      in
      print ~width:70 ~color:false ~files diagnostic;
      [%expect
        {|
        ---- Parse error [E0001] ------------------------------------- test.ml
        error: undefined function
             test.ml:2:10
             |
           2 |   if n <= 1 then n
             |          ~~~
        note: did you mean 'fib'?
             test.ml:1:9
             |
           1 | let rec fib n =
             |         ~~~
        note: functions must be defined before use
        |}]
    ;;

    let%expect_test "multiline snippet" =
      let source = "let x =\n  foo\n  bar\n  baz" in
      let files = setup [ "test.ml", source ] in
      let diagnostic =
        { code = Parse_error
        ; filename = "test.ml"
        ; parts =
            [ { Part.severity = Error
              ; message = Text.of_string "unexpected indentation"
              ; snippet = Some (make_snippet ~file:"test.ml" ~start:8 ~stop:24)
              }
            ]
        }
      in
      print ~width:60 ~color:false ~files diagnostic;
      [%expect
        {|
        ---- Parse error [E0001] --------------------------- test.ml
        error: unexpected indentation
             test.ml:2:1
             |
           2 |   foo
             | ~~~~~...
        |}]
    ;;
  end)
;;
