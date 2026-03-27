open Prelude
module Syntax = Oak_core_syntax
module Evaluate = Oak_core_evaluate
module Common = Oak_common
module Level = Common.Level
module Index = Common.Index
module Name = Common.Name

type ty_props = Syntax.ty_props
type term = Syntax.term
type term_data = Syntax.term_data
type term_data_rec = Syntax.term_data_rec
type term_data_decl = Syntax.term_data_decl
type term_data_param = Syntax.term_data_param
type term_data_body = Syntax.term_data_body
type term_data_field = Syntax.term_data_field
type term_data_constructor = Syntax.term_data_constructor
type field_loc = Syntax.field_loc
type term_field_impl = Syntax.term_field_impl
type term_field_spec = Syntax.term_field_spec
type term_ty = Syntax.term_ty
type term_ty_struct = Syntax.term_ty_struct
type value = Syntax.value
type ty = Syntax.ty
type value_data_rec = Syntax.value_data_rec
type value_data_decl = Syntax.value_data_decl
type value_data = Syntax.value_data
type ty_sing = Syntax.ty_sing
type ty_struct = Syntax.ty_struct
type head = Syntax.head
type neutral = Syntax.neutral
type spine = Syntax.spine
type term_arg = Syntax.term_arg
type value_arg = Syntax.value_arg
type frame = Syntax.frame
type value_struct = Syntax.value_struct
type value_fun = Syntax.value_fun
type ty_fun = Syntax.ty_fun
type ty_closure = Syntax.ty_closure
type value_closure = Syntax.value_closure
type value_field_impl = Syntax.value_field_impl

module type ENV = sig
  type t
  type value

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

module Make_seq_env (Value : sig
    type t
  end) : ENV with type value = Value.t and type t = Value.t Syntax.Seq.t = struct
  type value = Value.t
  type t = value Syntax.Seq.t

  let empty = Syntax.Seq.empty
  let push = Syntax.Seq.push
  let pop = Syntax.Seq.pop
  let pop_exn = Syntax.Seq.pop_exn
  let get_index = Syntax.Seq.get_index
  let get_level = Syntax.Seq.get_level
  let get_index_exn = Syntax.Seq.get_index_exn
  let get_level_exn = Syntax.Seq.get_level_exn
  let iter = Syntax.Seq.iter
  let to_list = Syntax.Seq.to_list
  let of_list = Syntax.Seq.of_list
  let length = Syntax.Seq.length
end

module Value_env : ENV with type value = value and type t = Syntax.env =
Make_seq_env (struct
    type t = value
  end)

module Ty_env : ENV with type value = ty and type t = Syntax.ty_env = Make_seq_env (struct
    type t = ty
  end)

module Name_env : ENV with type value = Name.t = struct
  type value = Name.t

  type t =
    { names : (value * int) list
    ; amount_with_name : int String.Map.t
    ; size : int
    }

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

module Erased_env = struct
  type t = int
  type value = unit

  let empty = 0
  let push t = t + 1
  let pop t = if t = 0 then None else Some (t - 1)
  let pop_exn t = if t = 0 then failwith "empty sequence" else t - 1
  let get_index t ({ Index.index } : Index.t) = index < t
  let get_level t ({ Level.level } : Level.t) = level < t
end

module Erased_env_ENV : ENV with type t = Erased_env.t and type value = unit = struct
  type t = Erased_env.t
  type value = unit

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

type value_env = Value_env.t
type ty_env = Ty_env.t
type name_env = Name_env.t
type erased_env = Erased_env.t

module Close = Evaluate.Close

module Field_loc = struct
  type t = field_loc
end

module Term = struct
  type t = term
end

module Term_ty = struct
  type t = term_ty
end

module Value = struct
  type t = value

  let quote = Evaluate.Value.quote
  let proj = Evaluate.Value.proj
  let app = Evaluate.Value.app
  let out = Evaluate.Value.out
  let decode = Evaluate.Value.decode
end

module Ty = struct
  type t = ty

  let quote = Evaluate.Ty.quote
  let proj = Evaluate.Ty.proj
  let app = Evaluate.Ty.app
  let out = Evaluate.Ty.out
end

module Struct = struct
  type t = value_struct

  let proj = Evaluate.Struct.proj
end

module Ty_struct = struct
  type t = ty_struct

  let proj = Evaluate.Ty_struct.proj
end

module Fun = struct
  type t = value_fun

  let app = Evaluate.Fun.app
end

module Ty_fun = struct
  type t = ty_fun

  let app = Evaluate.Ty_fun.app
end
