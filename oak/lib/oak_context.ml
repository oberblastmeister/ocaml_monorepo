module Core = Oak_core
module Pretty = Oak_pretty

type t =
  { ty_env : Core.ty_env
  ; name_list : Core.name_env
  }
[@@deriving sexp_of]

let empty = { ty_env = Core.Ty_env.empty; name_list = Core.Name_env.empty }

let bind (name : Core.Name.t) ty cx =
  { cx with
    ty_env = Core.Ty_env.push ty cx.ty_env
  ; name_list = Core.Name_env.push name cx.name_list
  }
;;

let size (cx : t) = Core.Ty_env.length cx.ty_env
let next_free cx = Core.Value.free_of_size (size cx)
let next_level cx = Core.Level.of_int (size cx)
let get_level_exn cx (var : Core.Level.t) = Core.Ty_env.get_level_exn cx.ty_env var
let pp_value cx value = Pretty.pp_value cx.name_list value
let pp_ty ?show_singletons cx ty = Pretty.pp_ty ?show_singletons cx.name_list ty
