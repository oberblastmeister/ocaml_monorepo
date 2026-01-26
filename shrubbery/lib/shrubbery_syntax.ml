open Prelude

open struct
  module Token_tree = Shrubbery_token_tree
  module Token = Shrubbery_token
end

(* TODO: a group should have nonempty items here *)
type group =
  { items : item Non_empty_list.t
  ; block : token_block option
  ; alts : alt list
  }

and item =
  | Token of Token.ti
  | Delim of item_delim

and item_delim =
  { ldelim : Token.ti
  ; groups : group_sep list
  ; rdelim : Token.ti
  }

and token_block =
  { token : Token.ti
  ; block : block
  }

and block =
  { lbrace : Token.ti
  ; groups : group_sep list
  ; rbrace : Token.ti
  }

and group_sep =
  { group : group
  ; sep : Token.ti option
  }

and alt = token_block [@@deriving sexp_of, equal, compare]

module Group = struct
  type t = group

  let first_token t =
    let item = Non_empty_list.hd t.items in
    match item with
    | Token tok -> tok
    | Delim { ldelim; _ } -> ldelim
  ;;
end

module Make_sexp_of (Sexp_of_token : sig
    val sexp_of_token : Token.ti -> Sexp.t
  end) =
struct
  open Sexp_of_token

  let rec sexp_of_group { items; block; alts } =
    let group = Non_empty_list.to_list items |> List.map ~f:sexp_of_item in
    let block = Option.to_list block |> List.map ~f:sexp_of_token_block in
    let alts = List.map alts ~f:sexp_of_alt in
    Sexp.List (group @ block @ alts)

  and sexp_of_item item =
    match item with
    | Token ti -> sexp_of_token ti
    | Delim t -> sexp_of_item_delim t

  and sexp_of_group_sep { group; sep } =
    let sep = Option.to_list sep |> List.map ~f:sexp_of_token in
    Sexp.List ([ sexp_of_group group ] @ sep)

  and sexp_of_item_delim { ldelim; groups; rdelim } =
    let groups = List.map groups ~f:sexp_of_group_sep in
    Sexp.List ([ sexp_of_token ldelim ] @ groups @ [ sexp_of_token rdelim ])

  and sexp_of_token_block ({ token; block } : token_block) =
    Sexp.List [ sexp_of_token token; sexp_of_block block ]

  and sexp_of_block ({ lbrace; groups; rbrace } : block) =
    Sexp.List
      [ sexp_of_token lbrace
      ; List (List.map groups ~f:sexp_of_group_sep)
      ; sexp_of_token rbrace
      ]

  and sexp_of_alt alt = sexp_of_token_block alt
end

include Make_sexp_of (struct
    let sexp_of_token = Token.sexp_of_ti
  end)

module Minimal_sexp_of = Make_sexp_of (struct
    let sexp_of_token (t : Token.ti) = Token.sexp_of_t t.token
  end)
