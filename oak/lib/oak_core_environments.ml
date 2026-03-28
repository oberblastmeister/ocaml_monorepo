open Prelude
module Common = Oak_common
module Name = Common.Name
module Level = Common.Level
module Index = Common.Index

module type ENV = sig
  type t [@@deriving sexp_of]
  type value [@@deriving sexp_of]

  val empty : t
  val push : value -> t -> t
  val pop : t -> (value * t) option
  val pop_exn : t -> value * t
  val get_index : t -> Index.t -> value option
  val get_level : t -> Level.t -> value option
  val get_index_exn : t -> Index.t -> value
  val get_level_exn : t -> Level.t -> value
  val iter : t -> f:(value -> unit) -> unit
  val to_list : t -> value list
  val of_list : value list -> t
  val length : t -> int
end

module Name_env : ENV with type value = Name.t = struct
  type value = Name.t [@@deriving sexp_of]

  type t =
    { names : (value * int) list
    ; amount_with_name : int String.Map.t
    ; size : int
    }
  [@@deriving sexp_of]

  let empty = { names = []; amount_with_name = String.Map.empty; size = 0 }

  let push (name : Name.t) { names; amount_with_name; size } =
    { names =
        (name, Map.find amount_with_name name.name |> Option.value ~default:0) :: names
    ; amount_with_name =
        Map.update amount_with_name name.name ~f:(function
          | None -> 1
          | Some n -> n + 1)
    ; size = size + 1
    }
  ;;

  let render_name t ((name : Name.t), name_level) =
    let amount = Map.find_exn t.amount_with_name name.name in
    let name_index = amount - name_level - 1 in
    if name_index = 0
    then name
    else { name with name = name.name ^ "@" ^ Int.to_string name_index }
  ;;

  let get_index t (index : Index.t) =
    List.nth t.names index.index |> Option.map ~f:(render_name t)
  ;;

  let get_level t (level : Level.t) = get_index t (Index.of_level t.size level)

  let get_index_exn t index =
    match get_index t index with
    | Some name -> name
    | None -> failwith "index out of bounds"
  ;;

  let get_level_exn t level =
    match get_level t level with
    | Some name -> name
    | None -> failwith "level out of bounds"
  ;;

  let pop = function
    | { names = []; _ } -> None
    | { names = ((name : Name.t), _) :: names; amount_with_name; size } ->
      let count = Map.find_exn amount_with_name name.name in
      let amount_with_name =
        if count = 1
        then Map.remove amount_with_name name.name
        else Map.set amount_with_name ~key:name.name ~data:(count - 1)
      in
      Some (name, { names; amount_with_name; size = size - 1 })
  ;;

  let pop_exn t =
    match pop t with
    | Some result -> result
    | None -> failwith "empty sequence"
  ;;

  let iter t ~f = List.iter t.names ~f:(fun (name, _) -> f name)
  let to_list t = List.map t.names ~f:fst
  let of_list names = List.fold_right names ~init:empty ~f:push
  let length t = t.size
end

module Generic_env : sig
  type 'a t [@@deriving sexp_of]

  val empty : 'a t
  val push : 'a -> 'a t -> 'a t
  val pop : 'a t -> ('a * 'a t) option
  val pop_exn : 'a t -> 'a * 'a t
  val get_index : 'a t -> Index.t -> 'a option
  val get_level : 'a t -> Level.t -> 'a option
  val get_index_exn : 'a t -> Index.t -> 'a
  val get_level_exn : 'a t -> Level.t -> 'a
  val length : 'a t -> int
  val iter : 'a t -> f:('a -> unit) -> unit
  val of_list : 'a list -> 'a t
  val to_list : 'a t -> 'a list
end = struct
  include Utility.Seq

  let get_index t (i : Index.t) = Utility.Seq.get t i.index
  let get_level t l = get_index t (Index.of_level (Utility.Seq.length t) l)
  let get_index_exn t (i : Index.t) = Utility.Seq.get_exn t i.index
  let get_level_exn t l = get_index_exn t (Index.of_level (Utility.Seq.length t) l)
end

module Erased_env = struct
  type t = int [@@deriving sexp_of]
  type value = unit [@@deriving sexp_of]

  let empty = 0
  let push t = t + 1
  let pop t = if t = 0 then None else Some (t - 1)
  let pop_exn t = if t = 0 then failwith "empty sequence" else t - 1
  let get_index t ({ Index.index } : Index.t) = index < t
  let get_level t ({ Level.level } : Level.t) = level < t
end

module Erased_env_ENV : ENV with type t = Erased_env.t and type value = unit = struct
  type t = Erased_env.t [@@deriving sexp_of]
  type value = unit [@@deriving sexp_of]

  let empty = Erased_env.empty
  let push () t = Erased_env.push t

  let pop t =
    match Erased_env.pop t with
    | Some t -> Some ((), t)
    | None -> None
  ;;

  let pop_exn t = (), Erased_env.pop_exn t
  let get_index t index = if Erased_env.get_index t index then Some () else None
  let get_level t level = if Erased_env.get_level t level then Some () else None

  let get_index_exn t index =
    match get_index t index with
    | Some value -> value
    | None -> failwith "index out of bounds"
  ;;

  let get_level_exn t level =
    match get_level t level with
    | Some value -> value
    | None -> failwith "level out of bounds"
  ;;

  let iter t ~f =
    for _ = 1 to t do
      f ()
    done
  ;;

  let to_list t = List.init t ~f:(fun _ -> ())
  let of_list xs = List.length xs
  let length t = t
end
