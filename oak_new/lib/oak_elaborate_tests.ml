open Prelude
open Core
module Core = Oak_core
module Typed = Oak_typed
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
  else (
    match expr with
    | None -> print_string "no expression\n"
    | Some expr ->
      let rename_diagnostics, renamed = Oak_rename.rename source expr in
      if not (List.is_empty rename_diagnostics)
      then Diagnostic.print_many ~files ~color:false rename_diagnostics
      else (
        match Oak_elaborate.infer source renamed with
        | Ok typed ->
          if print_term
          then
            print_s
              [%message
                (Typed.Expr.term typed : Core.term) (Typed.Expr.ty typed : Core.ty)];
          Pp.render_to_stdout
            ~color:false
            (Pretty.pp_ty ~show_singletons Core.Name_env.empty (Typed.Expr.ty typed));
          Out_channel.newline stdout
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
    |}];
  check
    {|
fun (x : true) -> x
    |};
  [%expect
    {|
    error: Type was not a universe: Bool
     --> <input>:2:10
      |
    2 | fun (x : true) -> x
      |          ^^^^
    |}]
;;

let%expect_test "id" =
  check
    {|
fun (x : Bool) -> x
    |};
  [%expect {| (x : Bool) -> Bool |}]
;;

let%expect_test "application" =
  check
    {|
(fun (x : Bool) -> x) true
    |};
  [%expect {| Bool |}]
;;

let%expect_test "block lets" =
  check
    {|
{
  val T : Type = Bool
  val x : T = true
  x
}
    |};
  [%expect {| Bool |}]
;;

let%expect_test "struct concrete fields" =
  check
    {|
struct {
  val T : Type = Bool
  val x : T = true
}
    |};
  [%expect {| sig { val T : Type = Bool; val x : Bool = ignore } |}]
;;

let%expect_test "struct abstract field" =
  check
    {|
struct {
  abstract val T : Type = Bool
  val x : T = true
}
    |};
  [%expect
    {|
    error: Types were not equal: Bool != T
    error: while checking the expression against the expected type
     --> <input>:4:15
      |
    4 |   val x : T = true
      |               ^^^^
    |}]
;;

let%expect_test "struct annotation subtyping" =
  check
    {|
(struct {
  val T : Type = Bool
  val x : T = true
} : sig {
  val T : Type
  val x : T
})
    |};
  [%expect {| sig { val T : Type; val x : T } |}]
;;

let%expect_test "nondependent struct punning" =
  check
    {|
{
  val x : Type = Bool
  struct (val x)
}
    |};
  [%expect {| sig { val x : Type = Bool } |}]
;;

let%expect_test "nondependent struct fields do not bind later fields" =
  check
    {|
{
  val x : Type = Bool
  struct (val x = Unit, val y = x)
}
    |};
  [%expect {| sig { val x : Type = Unit; val y : Type = Bool } |}]
;;

let%expect_test "nondependent struct checking reorders fields" =
  check
    {|
struct {
  val T : Type = Bool
  val x : sig {
    val T = Bool
    val y : T = true
  } = struct (val y = true, val T)
}
    |};
  [%expect
    {|
    sig {
      val T : Type = Bool
      val x : sig { val y : Bool = ignore; val T : Type = T } = struct (val y = ignore, val T = T)
    }
    |}]
;;

let%expect_test "field reordering" =
  check
    {|
(struct {
  val y = true
  val T : Type = Bool
  val x : T = true
} : sig {
  val x : Bool
  val T : Type
  val y : T
})
    |};
  [%expect {| sig { val x : Bool; val T : Type; val y : T } |}]
;;

let%expect_test "universes" =
  check
    {|
struct {
  val x = Type
  val y = Type
}
    |};
  [%expect {| sig { val x : Sig = Type; val y : Sig = Type } |}];
  check
    {|
struct {
  val x = Type
  val y = Type
  val z = Sig
}
    |};
  [%expect {| sig { val x : Sig = Type; val y : Sig = Type; val z : SIG = Sig } |}];
  check
    {|
struct {
  val S1 = sig {
    val x : Type = Int
    val y : Type = Int
  }
  
  val S2 = sig {
    val z : Sig = Type
    val w : Sig
    val x = sig {
      val x : Type
      val z = x
    }
  }
}
    |};
  [%expect
    {|
    sig {
      val S1 : Sig = sig { val x : Type = Int; val y : Type = Int }
      val S2 :
        SIG
      =
        sig { val z : Sig = Type; val w : Sig; val x : Sig = sig { val x : Type; val z : Type = x } }
    }
    |}];
  check
    {|
struct {
  val S1 = sig {
    val T = Int
  }
}
      |};
  [%expect {| sig { val S1 : Sig = sig { val T : Type = Int } } |}];
  check
    {|
struct {
  val S1 = sig {
    val T
  }
}
      |};
  [%expect
    {|
    error: Signature declarations require either a type annotation or a definition
     --> <input>:4:5
      |
    4 |     val T
      |     ^^^^^
    |}]
;;

let%expect_test "eta laws" =
  check
    {|
struct {
  val f : Type -> Type = fun x -> Unit
  val x : sig { val f = f } = struct { val f : Type -> Type = fun x -> f x }
}
    |};
  [%expect
    {|
    sig {
      val f : Type -> Type = fun x -> Unit
      val x : sig { val f : Type -> Type = fun x -> f x } = struct (val f = fun x -> f x)
    }
    |}]
;;

let%expect_test "eta laws 2" =
  check
    {|
struct {
  val f : Type -> Type = fun x -> Unit
  val x : sig { val f = f } = struct { val f : Type -> Type = fun x -> f x }
  val y : sig { val f = fun (x : Type) -> f x } = struct { val f = f }
  val S = sig { val T : Type; val U : Type -> Type; val x : T }
  val m : S = struct {
    val T : Type = Unit
    val U : Type -> Type = fun (x : Type) -> Unit
    val x : T = ()
  }
  val z1 : sig { val v = m } = struct { val v : S = struct { val T = m.T; val U = m.U; val x = m.x } }
  val z3 : sig { val v = m } = struct { val v = m }
  val z5 : sig { val v = m } = struct { val v : S = struct { val T = m.T; val U = fun (x : Type) -> m.U x; val x = m.x } }
}
|};
  [%expect
    {|
    sig {
      val f : Type -> Type = fun x -> Unit
      val x : sig { val f : Type -> Type = fun x -> f x } = struct (val f = fun x -> f x)
      val y : sig { val f : Type -> Type = f } = struct (val f = f)
      val S : Sig = sig { val T : Type; val U : Type -> Type; val x : T }
      val m : S = struct (val T = Unit, val U = fun x -> Unit, val x = ignore)
      val z1 :
        sig { val v : S = struct (val T = m.T, val U = m.U, val x = m.x) }
      =
        struct (val v = struct (val T = m.T, val U = m.U, val x = m.x))
      val z3 :
        sig { val v : sig { val T : Type; val U : Type -> Type; val x : T } = m }
      =
        struct (val v = m)
      val z5 :
        sig { val v : S = struct (val T = m.T, val U = fun x -> m.U x, val x = m.x) }
      =
        struct (val v = struct (val T = m.T, val U = fun x -> m.U x, val x = m.x))
    }
    |}]
;;

let%expect_test "bind" =
  check
    {|
struct{
  val r : Bool = {
    bind x = pack true
    true
  }
}
    |};
  [%expect {| sig { val r : Bool = ignore } |}];
  check
    {|
struct {
  val r : Bool = {
    bind x = true
    true
  }
}
    |};
  [%expect
    {|
    error: Expected pack type, got Bool
    error: while checking the right-hand side of the bind expression
     --> <input>:4:14
      |
    4 |     bind x = true
      |              ^^^^
    |}];
  check
    {|
struct {
  val r : Type = {
    bind x = true
    Int
  }
}
      |};
  [%expect
    {|
    error: Universes are not transparent: Type
    error: while checking the bind expression
     --> <input>:4:5
      |
    4 |     bind x = true
      |     ^^^^^^^^^^^^^
    |}];
  check
    {|
struct {
  val r : sig { val x : Bool; val T = Int } = {
    bind x = pack true
    struct (val x = true, val T = Int)
  }
  val z : sig { val x = r } = struct(val x = struct(val x = true, val T = Int))
}
    |};
  [%expect
    {|
    sig {
      val r : sig { val x : Bool; val T : Type = Int } = struct (val x = ignore, val T = Int)
      val z :
        sig { val x : sig { val x : Bool; val T : Type = Int } = r }
      =
        struct (val x = struct (val x = ignore, val T = Int))
    }
    |}];
  check
    {|
  struct {
    val r : (U : Type) -> sig { val x : Bool; val T = U } = {
      bind x = pack true
      fun (U : Type) -> struct(val x = true, val T = U)
    }
  }
      |};
  [%expect
    {|
    sig {
      val r :
        (U : Type) -> sig { val x : Bool; val T : Type = U }
      =
        fun U -> struct (val x = ignore, val T = U)
    }
    |}]
;;

let%expect_test "record patching" =
  check
    ~show_singletons:true
    {|
struct {
  val S = sig { val T : Type; val x : T }
  val S' = S where { T = Int }
}
    |};
  [%expect
    {|
    sig {
      val S : Sig = sig { val T : Type; val x : T }
      val S' : Sig = sig { val T : Type = Int; val x : T.out }
    }
    |}];
  check
    ~show_singletons:true
    {|
  struct {
    val S = sig {
      val m : sig {
        val m : sig {
          val T : Type
          val x : T
        }
        val x : m.T
      }
      val x : m.m.T
    }
    val S' = S where { m.m.T = Int }
  }
        |};
  [%expect
    {|
    sig {
      val S :
        Sig
      =
        sig { val m : sig { val m : sig { val T : Type; val x : T }; val x : m.T }; val x : m.m.T }
      val S' :
        Sig
      =
        sig {
          val m : sig { val m : sig { val T : Type = Int; val x : T.out }; val x : m.T.out }
          val x : m.m.T.out
        }
    }
    |}];
  check
    ~show_singletons:true
    {|
  struct {
    val S = sig {
      val m : sig {
        val m : sig {
          val T : Type
          val U : Type
          val V : Type
          val x : T
          val y : U
          val z : V
        }
        val x = m.T
        val y = m.U
        val z = m.V
      }
      val x = m.m.T
      val y = m.m.U
      val z = m.m.V
    }
    
    abstract val m : sig {
      val T : Type
      val U : Type
      val V : Type
      val x : T
      val y : U
      val z : V
    } = struct {
      val T = Int
      val U = Bool
      val V = Unit
      val x = 123
      val y = true
      val z = ()
    }
    
    val S' = S where { m.m = m }
  }
        |};
  [%expect
    {|
    sig {
      val S :
        Sig
      =
        sig {
          val m :
            sig {
              val m : sig { val T : Type; val U : Type; val V : Type; val x : T; val y : U; val z : V }
              val x : Type = m.T
              val y : Type = m.U
              val z : Type = m.V
            }
          val x : Type = m.m.T
          val y : Type = m.m.U
          val z : Type = m.m.V
        }
      val m : sig { val T : Type; val U : Type; val V : Type; val x : T; val y : U; val z : V }
      val S' :
        Sig
      =
        sig {
          val m :
            sig {
              val m :
                sig { val T : Type; val U : Type; val V : Type; val x : T; val y : U; val z : V }
              =
                m
              val x : Type = m.out.T
              val y : Type = m.out.U
              val z : Type = m.out.V
            }
          val x : Type = m.m.out.T
          val y : Type = m.m.out.U
          val z : Type = m.m.out.V
        }
    }
    |}]
;;

let%expect_test "subtype" =
  check
    {|
struct {
  abstract val m : sig {
    val T : Type
    val x : T
  } = struct {
    val T = Int
    val x = 123
  }
  
  val another : sig {
    val T : Type
    val x : T
  } = m
}
    |};
  [%expect
    {| sig { val m : sig { val T : Type; val x : T }; val another : sig { val T : Type; val x : T } = m } |}]
;;

let%expect_test "where commutative" =
  check
    {|
struct {
  val S1 = sig {
    val M : sig {
      val M : sig {
        val T : Type
        val x : T
      }
    }
  }
  
  val T = Int
  
  val M = struct {
    val M = struct {
      val T = Int
      val x : T = 1234
    }
  }
  
  val S2 = S1 where { M = M; M.M = M.M; M.M.T = M.M.T }
  val S3 = S1 where { M.M.T = M.M.T; M.M = M.M; M = M }
  val S4 = S1 where { M.M = M.M; M.M.T = M.M.T; M = M }
  
  val EQ1 : sig { val S = S2 } = struct(val S = S3)
  val EQ2 : sig { val S = S2 } = struct(val S = S4)
}
    |};
  [%expect
    {|
    sig {
      val S1 : Sig = sig { val M : sig { val M : sig { val T : Type; val x : T } } }
      val T : Type = Int
      val M :
        sig {
          val M :
            sig { val T : Type = Int; val x : Int = ignore }
          =
            struct (val T = Int, val x = ignore)
        }
      =
        struct (val M = struct (val T = Int, val x = ignore))
      val S2 :
        Sig
      =
        sig {
          val M :
            sig {
              val M : sig { val T : Type = M.M.T; val x : T } = struct (val T = M.M.T, val x = M.M.x)
            }
          =
            struct (val M = struct (val T = M.M.T, val x = M.M.x))
        }
      val S3 :
        Sig
      =
        sig {
          val M :
            sig {
              val M : sig { val T : Type = M.M.T; val x : T } = struct (val T = M.M.T, val x = M.M.x)
            }
          =
            struct (val M = struct (val T = M.M.T, val x = M.M.x))
        }
      val S4 :
        Sig
      =
        sig {
          val M :
            sig {
              val M : sig { val T : Type = M.M.T; val x : T } = struct (val T = M.M.T, val x = M.M.x)
            }
          =
            struct (val M = struct (val T = M.M.T, val x = M.M.x))
        }
      val EQ1 : sig { val S : Sig = S3 } = struct (val S = S3)
      val EQ2 : sig { val S : Sig = S4 } = struct (val S = S4)
    }
    |}]
;;

let%expect_test "where already transparent" =
  check
    ~show_singletons:true
    {|
struct {
  val T = Int
  
  val S1 = sig {
    val T = Int
  }
  
  val S4 = S1
  
  val S2 = S1 where { T = Int }
}
    |};
  [%expect
    {|
    sig {
      val T : Type = Int
      val S1 : Sig = sig { val T : Type = Int }
      val S4 : Sig = S1.out
      val S2 : Sig = S1.out
    }
    |}]
;;
