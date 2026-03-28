let () =
  Printexc.record_backtrace true;
  Oak_new.Oak_elaborate_tests.check
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
          |}
;;
