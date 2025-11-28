open Prelude

type t =
  { name : string
  ; id : int
  }

let create_source name = { name; id = -1 }
let create name id = { name; id }
let normalize i = if i < 0 then -1 else i
let equal t t' = String.equal t.name t'.name && normalize t.id = normalize t.id

let compare t t' =
  let c1 = String.compare t.name t'.name in
  if c1 = 0 then Int.compare (normalize t.id) (normalize t'.id) else c1
;;

let to_string { name; id } = if id < 0 then name else name ^ "_" ^ Int.to_string id
let sexp_of_t t = String.sexp_of_t (to_string t)

include Comparable.Make_plain (struct
    type nonrec t = t

    let compare = compare
    let sexp_of_t = sexp_of_t
  end)
