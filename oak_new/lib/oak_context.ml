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

exception Error of Diagnostic.t

type t =
  { ty_env : ty_env
  ; name_list : Name_list.t
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
  { ty_env = Seq.empty; name_list = Name_list.empty; source; next_meta_id = ref 0 }
;;

let bind (name : Name.t) ty cx =
  { cx with
    ty_env = Seq.push ty cx.ty_env
  ; name_list = Name_list.push name.name cx.name_list
  }
;;

let size (cx : t) = Seq.length cx.ty_env
let next_free cx = Value.free (Level.of_int (size cx))
let next_level cx = Level.of_int (size cx)
let close_single cx body = Evaluate.close_single (next_level cx) body
let quote (cx : t) e = Evaluate.quote (Seq.length cx.ty_env) e
let whnf_value (cx : t) e = Evaluate.whnf_value cx.ty_env e
let whnf_ty (cx : t) e = Evaluate.whnf_ty cx.ty_env e
let whnf_neutral cx e = Evaluate.whnf_neutral cx.ty_env e
let level_var_ty cx (var : Level.t) = Seq.get_level_exn cx.ty_env var
let pp_value cx value = Pretty.pp_value cx.name_list value
let pp_ty cx ty = Pretty.pp_ty cx.name_list ty

let snippet cx (span : Utility.Span.t) : Utility.File_span.t =
  { file = cx.source.filename
  ; start = cx.source.token_offsets.(span.start)
  ; stop = cx.source.token_offsets.(span.stop)
  }
;;
