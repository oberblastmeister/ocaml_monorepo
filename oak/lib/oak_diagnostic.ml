open Core
module Pp = Utility.Pp
module Doc = Pp.Doc
module Snippet = Diagnostic.Snippet

module Kind = struct
  type t =
    | Warning
    | Error
    | Note
    | Help
  [@@deriving sexp_of]

  let color = function
    | Error -> Pp.Term_color.Red
    | Warning -> Yellow
    | Note -> Cyan
    | Help -> Green
  ;;

  let style t = Pp.Style.append (Pp.Style.fg (Pp.Color.term (color t))) Pp.Style.bold

  let to_string t =
    match t with
    | Error -> "error"
    | Warning -> "warning"
    | Note -> "note"
    | Help -> "help"
  ;;
end

module Part = struct
  type t =
    { kind : Kind.t
    ; message : Doc.t
    ; snippet : Snippet.t option
    }
  [@@deriving sexp_of]
end

module Code = struct
  type t = Parse_error [@@deriving sexp_of]

  let to_string = function
    | Parse_error -> "E0001"
  ;;

  let description = function
    | Parse_error -> "Parse error"
  ;;
end

type t =
  { code : Code.t option
  ; parts : Part.t list
  }
[@@deriving sexp_of]

let format_part ?code ~width ~color ~files (part : Part.t) =
  let open Doc.Syntax in
  let message_doc =
    let kind = Doc.string (Kind.to_string part.kind) in
    let code =
      Option.map code ~f:(fun code -> Doc.string ("[" ^ Code.to_string code ^ "]"))
      |> Option.value ~default:Doc.empty
    in
    Doc.style (Kind.style part.kind) (kind ^^ code)
    ^^ Doc.string ": "
    ^^ part.Part.message
  in
  let message = Pp.render_to_string ~width ~color message_doc in
  match part.Part.snippet with
  | Some snippet ->
    let snippet_str = Snippet.format_snippet files snippet in
    sprintf "%s\n%s" message snippet_str
  | None -> message
;;

let format ?(width = 100) ?(color = true) ~files diagnostic =
  match diagnostic.parts with
  | [] -> ""
  | main_part :: parts ->
    let main_part_str =
      format_part ?code:diagnostic.code ~width ~color ~files main_part
    in
    let parts_str =
      List.map parts ~f:(format_part ~width ~color ~files) |> String.concat ~sep:"\n"
    in
    sprintf "%s\n%s" main_part_str parts_str
;;

let print ?width ?(color = true) ~files diagnostic =
  print_endline (format ?width ~color ~files diagnostic)
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
        { code = Some Parse_error
        ; parts =
            [ { kind = Error
              ; message = Doc.string "type mismatch: expected int, got string"
              ; snippet = Some (make_snippet ~file:"test.ml" ~start:12 ~stop:19)
              }
            ]
        }
      in
      print ~width:60 ~color:false ~files diagnostic;
      [%expect
        {|
        error[E0001]: type mismatch: expected int, got string
         --> test.ml:1:13
          |
        1 | let x = 1 + "hello"
          |             ^^^^^^^
        |}]
    ;;

    let%expect_test "error with notes" =
      let source =
        "let rec fib n =\n  if n <= 1 then n\n  else fib (n - 1) + fib (n - 2)"
      in
      let files = setup [ "test.ml", source ] in
      let diagnostic =
        { code = Some Parse_error
        ; parts =
            [ { kind = Error
              ; message = Doc.string "undefined function"
              ; snippet = Some (make_snippet ~file:"test.ml" ~start:25 ~stop:28)
              }
            ; { kind = Note
              ; message = Doc.string "did you mean 'fib'?"
              ; snippet = Some (make_snippet ~file:"test.ml" ~start:8 ~stop:11)
              }
            ; { kind = Note
              ; message = Doc.string "functions must be defined before use"
              ; snippet = None
              }
            ]
        }
      in
      print ~width:70 ~color:false ~files diagnostic;
      [%expect
        {|
        error[E0001]: undefined function
         --> test.ml:2:10
          |
        2 |   if n <= 1 then n
          |          ^^^
        note: did you mean 'fib'?
         --> test.ml:1:9
          |
        1 | let rec fib n =
          |         ^^^
        note: functions must be defined before use
        |}]
    ;;

    let%expect_test "multiline snippet" =
      let source = "let x =\n  foo\n  bar\n  baz" in
      let files = setup [ "test.ml", source ] in
      let diagnostic =
        { code = Some Parse_error
        ; parts =
            [ { kind = Error
              ; message = Doc.string "unexpected indentation"
              ; snippet = Some (make_snippet ~file:"test.ml" ~start:10 ~stop:24)
              }
            ]
        }
      in
      print ~width:60 ~color:false ~files diagnostic;
      [%expect
        {|
        error[E0001]: unexpected indentation
         --> test.ml:2:3
          |
        2 |   foo
          |   ^^^...
        |}]
    ;;

    let%expect_test "right on the end" =
      let source = "xyz\n" in
      let files = setup [ "test.ml", source ] in
      let diagnostic =
        { code = Some Parse_error
        ; parts =
            [ { kind = Error
              ; message = Doc.string "testing"
              ; snippet = Some (make_snippet ~file:"test.ml" ~start:3 ~stop:4)
              }
            ]
        }
      in
      print ~width:60 ~color:false ~files diagnostic;
      [%expect
        {|
        error[E0001]: testing
         --> test.ml:1:4
          |
        1 | xyz
          |    ^...
        |}]
    ;;

    let%expect_test "length 0 span" =
      let source = "xyz\n" in
      let files = setup [ "test.ml", source ] in
      let diagnostic =
        { code = Some Parse_error
        ; parts =
            [ { kind = Error
              ; message = Doc.string "testing"
              ; snippet = Some (make_snippet ~file:"test.ml" ~start:0 ~stop:0)
              }
            ]
        }
      in
      print ~width:60 ~color:false ~files diagnostic;
      [%expect
        {|
        error[E0001]: testing
         --> test.ml:1:1
          |
        1 | xyz
          | ^
        |}]
    ;;
  end)
;;
