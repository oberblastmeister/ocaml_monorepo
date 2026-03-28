open Prelude
module Core = Oak_core
module Diagnostic = Oak_diagnostic
module Pretty = Oak_pretty
module Source = Oak_source

exception Error of Diagnostic.t

type t =
  { source : Source.t
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

let snippet cx (span : Utility.Span.t) : Utility.File_span.t =
  { file = cx.source.filename
  ; start = cx.source.token_offsets.(span.start)
  ; stop = cx.source.token_offsets.(span.stop)
  }
;;
