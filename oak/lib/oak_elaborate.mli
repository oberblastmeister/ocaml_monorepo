open Prelude
module Acc := Utility.Acc
module Syntax := Oak_syntax
module Var := Syntax.Var
module Purity := Syntax.Purity
module Diagnostic := Oak_diagnostic

module Effects : sig
  type t =
    { vars : (Var.t * Syntax.ty) Acc.t
    ; purity : Purity.t
    }
  [@@deriving sexp_of]
end

val infer : Syntax.expr -> Diagnostic.t list * (Effects.t * Syntax.ty) option
