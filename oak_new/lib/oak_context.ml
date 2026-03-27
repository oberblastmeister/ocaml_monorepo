open Prelude
module Core = Oak_core

open struct
  module Spanned = Utility.Spanned
  module Diagnostic = Oak_diagnostic
  module Pretty = Oak_pretty
  module Source = Oak_source
end

exception Error of Diagnostic.t

type t =
  { ty_env : Core.ty_env
  ; name_list : Core.name_env
  ; source : Source.t
  ; next_meta_id : int ref
  }

let throw (_ : t) ?code parts = raise_notrace (Error (Diagnostic.create ?code parts))

let with_context (_ : t) part ~f =
  match f () with
  | x -> x
  | exception Error diagnostic ->
    let diagnostic = { diagnostic with parts = diagnostic.parts @ [ part ] } in
    raise_notrace (Error diagnostic)
;;

let create (source : Source.t) =
  { ty_env = Core.Ty_env.empty
  ; name_list = Core.Name_env.empty
  ; source
  ; next_meta_id = ref 0
  }
;;

let bind (name : Core.Name.t) ty cx =
  { cx with
    ty_env = Core.Ty_env.push ty cx.ty_env
  ; name_list = Core.Name_env.push name cx.name_list
  }
;;

let size (cx : t) = Core.Ty_env.length cx.ty_env
let next_free cx = Core.Value.free (Core.Level.of_int (size cx))
let next_level cx = Core.Level.of_int (size cx)
let level_var_ty cx (var : Core.Level.t) = Core.Ty_env.get_level_exn cx.ty_env var
let pp_value cx value = Pretty.pp_value cx.name_list value
let pp_ty ?show_singletons cx ty = Pretty.pp_ty ?show_singletons cx.name_list ty

let snippet cx (span : Utility.Span.t) : Utility.File_span.t =
  { file = cx.source.filename
  ; start = cx.source.token_offsets.(span.start)
  ; stop = cx.source.token_offsets.(span.stop)
  }
;;
