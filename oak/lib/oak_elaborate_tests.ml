open Prelude
module Snippet = Utility.Diagnostic.Snippet
module Pretty = Oak_pretty
module Common = Oak_common
module Diagnostic = Oak_diagnostic

let check s =
  let file = "<input>" in
  let source, parse_diagnostics, expr = Oak_parse.parse ~file s in
  let files = String.Map.of_alist_exn [ file, Snippet.File.create s ] in
  if not (List.is_empty parse_diagnostics)
  then Diagnostic.print_many ~files ~color:false parse_diagnostics
  else (
    match expr with
    | None -> print_string "no expression\n"
    | Some expr ->
      let rename_diagnostics, renamed = Oak_rename.rename source expr in
      if not (List.is_empty rename_diagnostics)
      then Diagnostic.print_many ~files ~color:false rename_diagnostics
      else (
        match Oak_elaborate.infer source renamed with
        | Ok (term, ty) ->
          print_s [%sexp (term : Oak_syntax.term)];
          print_string "::\n";
          Pp.render_to_stdout ~color:false (Pretty.pp_value Common.Name_list.empty ty);
          ()
        | Error diagnostic ->
          Diagnostic.print ~color:false ~files diagnostic;
          Out_channel.newline stdout))
;;

let%expect_test "smoke" =
  check
    {|
fun x -> x
    |};
  [%expect
    {|
    error: Cannot infer lambda without parameter type annotation
     --> <input>:2:1
      |
    2 | fun x -> x
      | ^^^^^^^^^^
    |}]
;;

let%expect_test "id" =
  check
    {|
fun (x : Bool) -> x
    |};
  [%expect
    {|
    (Term_abs (var ((name x) (pos 4))) (body (Term_var ((index 0)))))
    ::
    Fun (x : Bool) -> Bool
    |}]
;;

let%expect_test "modules" =
  check
    {|
mod {
  let first = #t
  let second = Bool
}
    |};
  [%expect
    {|
    (Term_let (var ((name first) (pos 8))) (rhs (Term_bool (value true)))
     (body
      (Term_let (var ((name second) (pos 18))) (rhs (Term_core_ty Bool))
       (body
        (Term_mod
         (fields
          (((name first) (e (Term_var ((index 1)))))
           ((name second) (e (Term_var ((index 0))))))))))))
    ::
    sig { let first : Bool; let second : Type }
    |}];
  check
    {|
  mod {
    let first = #t
    let second = Bool
    let third = second
  }
      |};
  [%expect
    {|
    (Term_let (var ((name first) (pos 9))) (rhs (Term_bool (value true)))
     (body
      (Term_let (var ((name second) (pos 19))) (rhs (Term_core_ty Bool))
       (body
        (Term_let (var ((name third) (pos 29)))
         (rhs (Term_sing_in (Term_var ((index 0)))))
         (body
          (Term_mod
           (fields
            (((name first) (e (Term_var ((index 2)))))
             ((name second) (e (Term_var ((index 1)))))
             ((name third) (e (Term_var ((index 0))))))))))))))
    ::
    sig { let first : Bool; let second : Type; let third : =second }
    |}]
;;

let%expect_test "signatures" =
  check
    {|
sig {
  let first : Bool
  let second : Bool 
}
    |};
  [%expect
    {|
    (Term_ty_mod
     ((ty_decls
       (((var ((name first) (pos 8))) (ty (Term_core_ty Bool)))
        ((var ((name second) (pos 18))) (ty (Term_core_ty Bool)))))))
    ::
    Kind
    |}]
;;

let%expect_test "application" =
  check
    {|
(fun (x : Bool) -> x) #t
    |};
  [%expect
    {|
    (Term_app
     (func (Term_abs (var ((name x) (pos 5))) (body (Term_var ((index 0))))))
     (arg (Term_bool (value true))))
    ::
    Bool
    |}]
;;
