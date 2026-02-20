open Prelude
open Oak_syntax

open struct
  module Spanned = Utility.Spanned
  module Common = Oak_common
  module Name_list = Common.Name_list
  module Diagnostic = Oak_diagnostic
  module Pretty = Oak_pretty
  module Source = Oak_source
  module Evaluate = Oak_evaluate
end

type t =
  { ty_env : Env.t
  ; name_list : Name_list.t
  ; source : Source.t
  ; next_meta_id : int ref
  }

let create (source : Source.t) =
  { ty_env = Env.empty; name_list = Name_list.empty; source; next_meta_id = ref 0 }
;;

let bind (var : Var_info.t) ty cx =
  { cx with
    ty_env = Env.push ty cx.ty_env
  ; name_list = Name_list.push var.name cx.name_list
  }
;;

let size (cx : t) = Env.size cx.ty_env
let next_free cx = Value.free (Level.of_int (size cx))
let next_level cx = Level.of_int (size cx)
let close_single cx body = Evaluate.close_single (next_level cx) body
let quote (cx : t) e = Evaluate.quote (Env.size cx.ty_env) e
let unfold (cx : t) e = Evaluate.unfold cx.ty_env e

(* let var_ty cx (var : Index.t) = Env.find_index_exn cx.ty_env var *)
let level_var_ty cx (var : Level.t) = Env.find_level_exn cx.ty_env var
let pp_value cx value = Pretty.pp_value cx.name_list value

let snippet cx (span : Utility.Span.t) : Utility.File_span.t =
  { file = cx.source.filename
  ; start = cx.source.token_offsets.(span.start)
  ; stop = cx.source.token_offsets.(span.stop)
  }
;;
