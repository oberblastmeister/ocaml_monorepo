open Core
module Diagnostic = Utility.Diagnostic
module Pp = Utility.Pp
module Snippet = Diagnostic.Snippet

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
  print_endline "=== Diagnostic Snippet Demo ===\n";
  print_endline "Source file (example.ml):";
  print_endline "----------------------------";
  print_endline source;
  print_endline "\n----------------------------\n";
  print_endline "Snippet 1: Highlighting 'fib' in the function definition";
  let snippet1 = make_snippet ~file:"example.ml" ~start:8 ~stop:11 in
  Pp.render_to_stdout (Snippet.pp files snippet1);
  print_string "\n\n";
  print_endline "Snippet 2: Highlighting the type error location";
  let snippet2 = make_snippet ~file:"example.ml" ~start:94 ~stop:101 in
  Pp.render_to_stdout (Snippet.pp files snippet2);
  print_string "\n\n";
  print_endline "Snippet 3: Multi-line highlight (the else branch)";
  let snippet3 = make_snippet ~file:"example.ml" ~start:39 ~stop:70 in
  Pp.render_to_stdout (Snippet.pp files snippet3);
  print_string "\n";
  ()
;;
