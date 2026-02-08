open Core

type t =
  { start : int
  ; stop : int
  }
[@@deriving sexp, equal, compare]

let start { start; stop = _ } = start
let stop { start = _; stop } = stop
let empty = { start = 0; stop = 0 }
let create start stop = { start; stop }
let single start = { start; stop = start + 1 }
let combine a b = { start = min a.start b.start; stop = max b.stop a.stop }
