open Core
module Diagnostic = Utility.Diagnostic
module Snippet = Diagnostic.Snippet
module Diag = Oak.Oak_diagnostic

let make_snippet ~file ~start ~stop : Snippet.t = { file; start; stop }

let () =
  let source =
    {|let rec fib n =
  if n <= 1 then n
  else fib (n - 1) + fib (n - 2)

let result = fib 10

let broken = 1 + "hello"|}
  in
  let file = Snippet.File.create source in
  let files = String.Map.of_alist_exn [ "example.ml", file ] in
  print_endline "=== Oak Diagnostic Demo ===\n";
  let diagnostic1 =
    { Diag.code = Some Parse_error
    ; parts =
        [ { Diag.Part.kind = Error
          ; message = Diag.Doc.string "type mismatch: expected int, got string"
          ; snippet = Some (make_snippet ~file:"example.ml" ~start:94 ~stop:101)
          }
        ; { Diag.Part.kind = Note
          ; message = Diag.Doc.string "the '+' operator expects integer operands"
          ; snippet = Some (make_snippet ~file:"example.ml" ~start:89 ~stop:90)
          }
        ; { Diag.Part.kind = Note
          ; message = Diag.Doc.string "consider using '^' for string concatenation"
          ; snippet = None
          }
        ]
    }
  in
  Diag.print ~width:80 ~files diagnostic1;
  print_endline "";
  let diagnostic2 =
    { Diag.code = Some Parse_error
    ; parts =
        [ { Diag.Part.kind = Error
          ; message = Diag.Doc.string "unexpected token"
          ; snippet = Some (make_snippet ~file:"example.ml" ~start:39 ~stop:70)
          }
        ]
    }
  in
  Diag.print ~width:180 ~files diagnostic2;
  ()
;;
