open Prelude
module Token_tree = Shrubbery_token_tree
module Token = Shrubbery_token
module Span = Utility.Span

type group = item Non_empty_list.t

and item =
  | Token of Token.ti
  | Delim of item_delim

and item_delim =
  { ldelim : Token.ti
  ; groups : group_sep list
  ; rdelim : Token.ti
  }

and group_sep =
  { group : group
  ; sep : Token.ti option
  }

and root = group option

module Root = struct
  type t = root
end

module Item = struct
  type t = item

  let span = function
    | Token tok -> Span.single tok.index
    | Delim { ldelim; groups = _; rdelim } ->
      Span.combine (Span.single ldelim.index) (Span.single rdelim.index)
  ;;

  let first_token t =
    match t with
    | Token tok -> tok
    | Delim { ldelim; _ } -> ldelim
  ;;
end

module Group = struct
  type t = group

  let first_token t =
    let item = Non_empty_list.hd t in
    Item.first_token item
  ;;
end

module Make_sexp_of (Sexp_of_token : sig
    val sexp_of_token : Token.ti -> Sexp.t
  end) =
struct
  open Sexp_of_token

  let rec sexp_of_group items =
    let group = Non_empty_list.to_list items |> List.map ~f:sexp_of_item in
    Sexp.List group

  and sexp_of_root root = Option.sexp_of_t sexp_of_group root

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
  ;;
end

include Make_sexp_of (struct
    let sexp_of_token = Token.sexp_of_ti
  end)

module Minimal_sexp_of = Make_sexp_of (struct
    let sexp_of_token (t : Token.ti) = Token.sexp_of_t t.token
  end)
