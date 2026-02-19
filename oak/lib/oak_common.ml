open Prelude

open struct
  module Span = Utility.Span
end

module Core_ty = struct
  type t =
    | Bool
    | Unit
    | Int
  [@@deriving sexp_of, equal, compare]

  let to_string = function
    | Bool -> "Bool"
    | Unit -> "Unit"
    | Int -> "Int"
  ;;

  let pp t = Doc.string (to_string t)
end

module Var_info = struct
  type t =
    { name : string
    ; pos : int
    }
  [@@deriving sexp_of]

  let generated = { name = "<generated>"; pos = 0 }
end

module Universe : sig
  type t = private int [@@deriving sexp_of, compare, equal]

  val minimum : t
  val to_int : t -> int
  val of_int : int -> t
  val incr : t -> t
  val decr : t -> t
  val decr_exn : t -> t
  val min : t -> t -> t
  val max : t -> t -> t
  val to_string : t -> string
  val type_ : t
  val kind_ : t
  val sig_ : t
  val pp : t -> Doc.t
end = struct
  include Int

  let minimum = 0
  let to_int t = t
  let of_int_exn t = t
  let incr u = u + 1
  let decr_exn u = u - 1
  let decr u = Int.max 0 (u - 1)

  let to_string = function
    | 0 -> "Type"
    | 1 -> "Kind"
    | 2 -> "Sig"
    | n -> sprintf "Sig%d" (n - 2)
  ;;

  let sexp_of_t t = Sexp.Atom (to_string t)
  let pp t = Doc.string (to_string t)
  let type_ = 0
  let kind_ = 1
  let sig_ = 2
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

  let zero = { index = 0 }

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

module Literal = struct
  type t =
    | Unit
    | Bool of bool
    | Int of int
  [@@deriving sexp_of]
end

module Icit = struct
  type t =
    | Impl
    | Expl
  [@@deriving sexp_of, equal, compare]

  let to_string = function
    | Impl -> "implicit"
    | Expl -> "explicit"
  ;;
end
