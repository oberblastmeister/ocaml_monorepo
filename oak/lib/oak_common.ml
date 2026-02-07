open Prelude

open struct
  module Span = Utility.Span
end

module Core_ty = struct
  type t = Bool [@@deriving sexp_of, equal, compare]
end

module Universe = struct
  module T = struct
    type t =
      | Type
      | Kind
      | Sig
    [@@deriving sexp, equal, compare]
  end

  include T
  include Base.Comparable.Make (T)

  let minimum = Type
  let maximum = Sig

  let to_int = function
    | Type -> 0
    | Kind -> 1
    | Sig -> 2
  ;;

  let of_int_exn = function
    | 0 -> Type
    | 1 -> Kind
    | 2 -> Sig
    | _ -> failwith "invalid universe"
  ;;

  let lub u v = of_int_exn (Int.max (to_int u) (to_int v))
  let incr_exn u = of_int_exn (to_int u + 1)
  let decr_exn u = of_int_exn (to_int u - 1)

  let to_string = function
    | Type -> "Type"
    | Kind -> "Kind"
    | Sig -> "Sig"
  ;;
end
