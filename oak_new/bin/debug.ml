let () =
  Printexc.record_backtrace true;
  Oak_new.Oak_elaborate_tests.check
    {|
struct {
  val S1 = sig {
    val M : sig {
      val T : Type
      val x : T
    }
  }
  
  val T = Int
  
  val M = struct {
    val T = Int
    val x : T = 1234
  }
  
  val S2 = S1 where { M = M }
  
  val S3 = S2 where { M.T = T }
}
    |}
