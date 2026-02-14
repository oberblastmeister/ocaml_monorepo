open Prelude
module Syntax = Oak_syntax
module Snippet = Utility.Diagnostic.Snippet
module Pretty = Oak_pretty
module Common = Oak_common
module Diagnostic = Oak_diagnostic

let check ?(print_term = false) ?(show_singletons = false) s =
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
      end;
      begin match Oak_elaborate.infer source renamed with
      | Ok (term, ty) ->
        if print_term then print_s [%message (term : Syntax.term) (ty : Syntax.ty)];
        Pp.render_to_stdout
          ~color:false
          (Pretty.pp_value ~show_singletons Common.Name_list.empty ty);
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
  let x := Bool
  let y : x = #t
  y
}
    |};
  [%expect {| Bool |}];
  check
    {|
mod {
  let x := Bool
  let y = {
    let y : x = #t
    y
  }
}
      |};
  [%expect {| sig { let x : (= Bool); let y : x } |}];
  check
    {|
  mod {
    let x := Type
    let y := Type
    let z := Type
    let w := Kind
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

    error: Cannot infer error term
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
  [%expect {| Bool |}];
  check
    {|
  {
    let x = Bool
    (#t : x)
  }
      |};
  [%expect
    {|
    error: Types were not equal: Bool != x
    note: failed to coerce inferred type  Bool when checking against type x
     --> <input>:4:6
      |
    4 |     (#t : x)
      |      ^^
    |}];
  check
    {|
mod {
  let x := Bool
  let y := x
  let f : (= x) -> (= x) = fun x -> x
  let r := f Bool
  let b : r = #t
}
      |};
  [%expect
    {| sig { let x : (= Bool); let y : (= x); let f : (= x) -> (= x); let r : (= f ((Bool))); let b : r } |}];
  check
    {|
mod {
  let M1 = mod {
    let T := Bool
  }
  
  let M2 := M1
  
  let M3 : sig {
    let T : Type
  } = mod {
    let T := Bool
  }
  
  let T1 := M1.T
  
  let T2 := M2.T
  
  let T3 := M3.T
  
  let T4 := T3
}
      |};
  [%expect
    {|
    sig {
      let M1 : sig { let T : (= Bool) }
      let M2 : (= M1)
      let M3 : sig { let T : Type }
      let T1 : (= M1.T)
      let T2 : (= M2.T)
      let T3 : (= M3.T)
      let T4 : (= T3)
    }
    |}];
  check
    {|
mod {
  let M1 : sig {
    let M : sig {
      let M : sig {
        let T : Type
      }
    }
  } = mod {
    let M = mod {
      let M = mod {
        let T := Bool
      }
    }
  }
  
  let M2 := M1
  
  let T1 := M1.M.M.T
  
  let T2 := M2.M.M.T
  
  let T3 := (M2.M.M : sig { let T : Type }).T
}
      |};
  [%expect
    {|
    sig {
      let M1 : sig { let M : sig { let M : sig { let T : Type } } }
      let M2 : (= M1)
      let T1 : (= M1.M.M.T)
      let T2 : (= M2.M.M.T)
      let T3 : (= M2.M.M.T)
    }
    |}];
  check
    {|
mod {
  let M = mod {
    let M = mod {}
  }
  let T = M.M.T
}
    |};
  [%expect
    {|
    error: Module does not have field T
     --> <input>:6:11
      |
    6 |   let T = M.M.T
      |           ^^^^^
    |}];
  check
    {|
(Kind : Type)
      |};
  [%expect
    {|
    error: Universes were not equal: Sig != Type
    note: failed to coerce inferred type  Sig when checking against type Type
     --> <input>:2:2
      |
    2 | (Kind : Type)
      |  ^^^^
    |}];
  check
    {|
(Type : Type)
    |};
  [%expect
    {|
    error: Universes were not equal: Kind != Type
    note: failed to coerce inferred type  Kind when checking against type Type
     --> <input>:2:2
      |
    2 | (Type : Type)
      |  ^^^^
    |}];
  check
    {|
(Bool : Type)
      |};
  [%expect {| Type |}];
  check
    {|
{
  let b = #t
  let m = mod {
    let T := Bool
    let T' := (T : Type)
    let x : T = #t
  }
  m
}
    |};
  [%expect {| sig { let T : (= Bool); let T' : (= T); let x : T } |}];
  check
    {|
alias (= mod {
  let T := Bool
  let T' := (T : Type)
})
      |};
  [%expect {| (= (= mod { let T = Bool; let T' = Bool })) |}];
  check
    {|
    alias (= (Bool : Type))
    |};
  [%expect {| (= (= Bool)) |}];
  check
    {|
alias (= #t)
    |};
  [%expect {| (= (= ignore)) |}];
  check
    {|
alias (#f : (= #t))
    |};
  [%expect {| (= ignore) |}];
  check
    {|
({
  let packed_ty = pack (Bool : Type)
  bind T = packed_ty
  T
} : Type)
    |};
  [%expect
    {|
    error: Type was not ignorable: Type
    note: in bind expression
     --> <input>:2:2
      |
    2 | ({
      |  ^...
    |}];
  check
    {|
({
  let packed_ty = pack (Bool : Type)
  bind T = packed_ty
  pack T
} : (Pack Type))
      |};
  [%expect {| Pack Type |}];
  check
    {|
(mod {
  let T = Bool
  let x = #t
} : sig {
  let T : Type
  let x : T
})
    |};
  [%expect {| sig { let T : Type; let x : T } |}];
  check
    {|
(mod {
  let T = Unit
  let x = #t
} : sig {
  let T : Type
  let x : T
})
    |};
  [%expect
    {|
    error: Core types were not equal: Bool != Unit
    note: failed to coerce inferred type
      sig { let T : Type; let x : Bool }
    when checking against type
      sig { let T : Type; let x : T }
     --> <input>:2:2
      |
    2 | (mod {
      |  ^^^^^...
    |}];
  check
    {|
mod {
  let ty := (= fun (T : (= Bool)) -> T)
  let f = ((fun (T : Type) -> Bool) : ty)
  let T := f Bool
}
      |};
  [%expect {| sig { let ty : (= (= fun T -> T)); let f : ty; let T : (= f (Bool)) } |}];
  check
    {|
  mod {
    let f =
      ((fun (T : Type) -> Bool) : (= fun (T : (= Bool)) -> T))
    let T := f Int
  }
        |};
  [%expect
    {|
    error: Base types were not equal: Int != Bool
    note: failed to coerce inferred type  Type when checking against type (= Bool)
     --> <input>:5:16
      |
    5 |     let T := f Int
      |                ^^^
    |}];
  check
    {|
    mod {
      let f =
        ((fun (T : Type) -> Bool) : (= fun (T : (= Bool)) -> T))
      let T = f Int
    }
          |};
  [%expect
    {|
    error: Base types were not equal: Int != Bool
    note: failed to coerce inferred type  Type when checking against type (= Bool)
     --> <input>:5:17
      |
    5 |       let T = f Int
      |                 ^^^
    |}];
  check
    {|
    mod {
      let Un := Unit
      
      let M1 = mod {
        let T := Bool
        let T' := T
        let U := Int
        let S := Un
      }
      
      let M2 = mod {
        let T := Bool
        let T' := Bool
        let S := Unit
      }
      
      let M3 = (M1 : (= M2))
    }
    |};
  [%expect
    {|
    sig {
      let Un : (= Unit)
      let M1 : sig { let T : (= Bool); let T' : (= T); let U : (= Int); let S : (= Un) }
      let M2 : sig { let T : (= Bool); let T' : (= Bool); let S : (= Unit) }
      let M3 : (= M2)
    }
    |}];
  check
    {|
mod {
  let Pair : (a b : Type) -> Type = fun A B -> Unit
  
  let Functor := sig {
    let T : Type -> Type
    
    let map : (A B : Type) -> (A -> B) -> T A -> T B
  }
  
  let Applicative := sig {
    let T : Type -> Type
    
    let pure : (A : Type) -> T A
    let map : (A B : Type) -> (A -> B) -> T A -> T B
    let and : (A B : Type) -> T A -> T B -> T (Pair A B)
  };
  
  let Monad : Kind := sig {
    let T : Type -> Type
    let return : (A : Type) -> A -> T A
    let bind : (A B : Type) -> T A -> (A -> T B) -> T B
  }
  
  let List := sig {
    let T : Type -> Type
    let nil : (A : Type) -> T A
    let cons : (A : Type) -> A -> T A -> T A
  }
  
  let do_something = fun (A B : Type) (monad : Monad) (p : Pair (monad.T A) (monad.T B)) (x : monad.T A) (y : monad.T B) (list : List) -> {
    let first = list.cons Unit () (list.nil Unit)
    monad.bind A Unit x (fun x ->
      monad.return Unit ()
    )
  }
}
      |};
  [%expect
    {|
    sig {
      let Pair : (a : Type) -> (b : Type) -> Type
      let Functor :
        (= sig { let T : Type -> Type; let map : (A : Type) -> (B : Type) -> (A -> B) -> (T A) -> T B })
      let Applicative :
        ( =
          sig {
            let T : Type -> Type
            let pure : (A : Type) -> T A
            let map : (A : Type) -> (B : Type) -> (A -> B) -> (T A) -> T B
            let and : (A : Type) -> (B : Type) -> (T A) -> (T B) -> T (Pair A B)
          }
        )
      let Monad :
        ( =
          sig {
            let T : Type -> Type
            let return : (A : Type) -> A -> T A
            let bind : (A : Type) -> (B : Type) -> (T A) -> (A -> T B) -> T B
          }
        )
      let List :
        ( =
          sig {
            let T : Type -> Type
            let nil : (A : Type) -> T A
            let cons : (A : Type) -> A -> (T A) -> T A
          }
        )
      let do_something :
        (A : Type) ->
        (B : Type) ->
        (monad : Monad) ->
        (p : Pair (monad.T A) (monad.T B)) ->
        (x : monad.T A) ->
        (y : monad.T B) ->
        (list : List) ->
        monad.T Unit
    }
    |}];
  check
    {|
mod {
  let f : (T : (= Bool)) -> T = fun x -> #t
}
      |};
  [%expect {| sig { let f : (T : (= Bool)) -> T } |}]
;;

let%expect_test "id" =
  check
    {|
fun (x : Bool) -> x
    |};
  [%expect {| (x : Bool) -> Bool |}]
;;

let%expect_test "modules" =
  check
    {|
mod {
  let first = #t
  let second : Type = Bool
  let ty := Type
  let b : ty = Bool
}
    |};
  [%expect
    {| sig { let first : Bool; let second : Type; let ty : (= Type); let b : ty } |}];
  check
    {|
  mod {
    let first = #t
    let second := Bool
    let third := second
  }
      |};
  [%expect {| sig { let first : Bool; let second : (= Bool); let third : (= second) } |}]
;;

let%expect_test "signatures" =
  check
    {|
{
  let S := sig {
    let first : Bool
    let second : Bool 
  }
  S
}
    |};
  [%expect {| (= sig { let first : Bool; let second : Bool }) |}]
;;

let%expect_test "application" =
  check
    {|
(fun (x : Bool) -> x) #t
    |};
  [%expect {| Bool |}]
;;

let%expect_test "duplicate declarations record" =
  check
    {|
mod {
  let first = 1234
  let first = 1234
}
    |};
  [%expect {|
    error: Duplicate variable in module
     --> <input>:4:7
      |
    4 |   let first = 1234
      |       ^^^^^

    error: Cannot infer error term
     --> <input>:2:1
      |
    2 | mod {
      | ^^^^^...
    |}]
;;
