open Prelude

open struct
  module Span = Utility.Span
end

module Core_ty = struct
  type t = Bool [@@deriving sexp_of, equal, compare]
end

module Var_info = struct
  type t =
    { name : string
    ; pos : int
    }
  [@@deriving sexp_of]

  let generated = { name = "<generated>"; pos = 0 }
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

  let pp t = Doc.string (to_string t)
end

module Level = struct
  type t = { level : int } [@@unboxed] [@@deriving sexp_of, equal, compare]

  let of_int level =
    assert (level >= 0);
    { level }
  ;;
end

module Index = struct
  type t = { index : int } [@@unboxed] [@@deriving sexp_of, equal, compare]

  let of_int index =
    assert (index >= 0);
    { index }
  ;;

  let to_level context_size { index } = Level.of_int (context_size - index - 1)
  let of_level context_size { Level.level } = of_int (context_size - level - 1)
end

module Name_list : sig
  type t [@@deriving sexp_of]

  val empty : t
  val push : string -> t -> t
  val get : t -> Level.t -> string
  val size : t -> int
end = struct
  type t =
    { names : string list
    ; shadow_num_map : int String.Map.t
    ; size : int
    }
  [@@deriving sexp_of]

  let empty = { names = []; shadow_num_map = String.Map.empty; size = 0 }

  let push name { names; shadow_num_map; size } =
    let shadow_num_map =
      Map.update shadow_num_map name ~f:(function
        | None -> 1
        | Some n -> n + 1)
    in
    let shadow_num = Map.find_exn shadow_num_map name - 1 in
    let name = if shadow_num = 0 then name else sprintf "%s/%d" name shadow_num in
    { names = name :: names; shadow_num_map; size = size + 1 }
  ;;

  let get t (level : Level.t) =
    List.drop t.names (Index.of_level t.size level).index |> List.hd_exn
  ;;

  let size t = t.size
end
