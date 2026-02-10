open Prelude
module Syntax = Oak_syntax
module Snippet = Utility.Diagnostic.Snippet
module Pretty = Oak_pretty
module Common = Oak_common
module Diagnostic = Oak_diagnostic

let check ?(print_term = false) s =
  let file = "<input>" in
  let source, parse_diagnostics, expr = Oak_parse.parse ~file s in
  let files = String.Map.of_alist_exn [ file, Snippet.File.create s ] in
  if not (List.is_empty parse_diagnostics)
  then Diagnostic.print_many ~files ~color:false parse_diagnostics
  else begin
    match expr with
    | None -> print_string "no expression\n"
    | Some expr ->
      let rename_diagnostics, renamed = Oak_rename.rename source expr in
      if not (List.is_empty rename_diagnostics)
      then begin
        Diagnostic.print_many ~files ~color:false rename_diagnostics
      end
      else begin
        match Oak_elaborate.infer source renamed with
        | Ok (term, ty) ->
          if print_term then print_s [%sexp (term : Syntax.term)];
          Pp.render_to_stdout ~color:false (Pretty.pp_value Common.Name_list.empty ty);
          Out_channel.newline stdout
        | Error diagnostic ->
          Diagnostic.print ~color:false ~files diagnostic;
          Out_channel.newline stdout
      end
  end
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
    |}];
  check
    {|
 fun (x : #t) -> x
      |};
  [%expect
    {|
    error: Not a type
     --> <input>:2:11
      |
    2 |  fun (x : #t) -> x
      |           ^^
    |}];
  check
    {|
{
  let x = Bool
  let y : x = #t
  y
}
    |};
  [%expect {| Bool |}];
  check
    {|
mod {
  let x = Bool
  let y = {
    let y : x = #t
    y
  }
}
      |};
  [%expect {| sig { let x : (= Bool); let y : x.out } |}];
  check
    {|
  mod {
    let x = Type
    let y = Type
    let z = Type
    let w = Kind
  }
      |};
  [%expect
    {| sig { let x : (= Type); let y : (= Type); let z : (= Type); let w : (= Kind) } |}];
  check
    {|
      Sig
      |};
  [%expect
    {|
    error: Failed to find variable: Sig
     --> <input>:2:7
      |
    2 |       Sig
      |       ^^^
    |}];
  check
    {|
{
  let r : Bool = {
    bind x = pack #t
    #t
  }
  r
}

      |};
  check
    {|
  {
    let x = Bool
    #t : x
  }
      |};
  [%expect
    {|
    Bool
    Bool
    |}];
  check
    {|
{
  let x = Bool
  let f : Fun (= x) -> (= x) = fun x -> x
  f Bool
}
      |};
  [%expect {| (= Bool) |}];
  check
    {|
mod {
  let M1 = mod {
    let T = Bool
  }
  
  let M2 = M1
  
  let M3 : sig {
    let T : Type
  } = mod {
    let T = Bool
  }
  
  let T1 = M1.T
  
  let T2 = M2.T
  
  let T3 = M3.T
}
      |};
  [%expect
    {|
    sig {
      let M1 : sig { let T : (= Bool) }
      let M2 : (= M1)
      let M3 : sig { let T : Type }
      let T1 : (= Bool)
      let T2 : (= Bool)
      let T3 : Type
    }
    |}]
;;

let%expect_test "id" =
  check
    {|
fun (x : Bool) -> x
    |};
  [%expect {| Fun (x : Bool) -> Bool |}]
;;

let%expect_test "modules" =
  check
    {|
mod {
  let first = #t
  let second : Type = Bool
  let ty = Type
  let b : ty = Bool
}
    |};
  [%expect
    {| sig { let first : Bool; let second : Type; let ty : (= Type); let b : ty.out } |}];
  check
    {|
  mod {
    let first = #t
    let second = Bool
    let third = second
  }
      |};
  [%expect {| sig { let first : Bool; let second : (= Bool); let third : (= Bool) } |}]
;;

let%expect_test "signatures" =
  check
    {|
sig {
  let first : Bool
  let second : Bool 
}
    |};
  [%expect {| (= (sig { let first : Bool; let second : Bool })) |}]
;;

let%expect_test "application" =
  check
    {|
(fun (x : Bool) -> x) #t
    |};
  [%expect {| Bool |}]
;;
