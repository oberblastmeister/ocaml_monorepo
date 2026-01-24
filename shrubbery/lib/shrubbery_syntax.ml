open Prelude

open struct
  module Token_tree = Shrubbery_token_tree
  module Token = Shrubbery_token
end

type group =
  { items : item list
  ; block : token_block option
  ; alts : alt list
  }

and item =
  | Token of Token.t
  | Delim of item_delim

and item_delim =
  { ldelim : Token.t
  ; groups : group_sep list
  ; rdelim : Token.t
  }

and token_block =
  { token : Token.t
  ; block : block
  }

and block =
  { lbrace : Token.t
  ; groups : group_sep list
  ; rbrace : Token.t
  }

and group_sep =
  { group : group
  ; sep : Token.t option
  }

and alt = token_block [@@deriving sexp, equal, compare]

let rec block_to_list_ref l t =
  l := t.lbrace :: !l;
  List.iter t.groups ~f:(group_sep_to_list_ref l);
  l := t.rbrace :: !l

and group_to_list_ref l t =
  List.iter t.items ~f:(item_to_list_ref l);
  Option.iter t.block ~f:(token_block_to_list_ref l);
  List.iter t.alts ~f:(token_block_to_list_ref l);
  ()

and token_block_to_list_ref l t =
  l := t.token :: !l;
  block_to_list_ref l t.block

and group_sep_to_list_ref l t =
  group_to_list_ref l t.group;
  Option.iter t.sep ~f:(fun token -> l := token :: !l)

and item_to_list_ref l t =
  match t with
  | Token token -> l := token :: !l
  | Delim { ldelim; groups; rdelim } ->
    l := ldelim :: !l;
    List.iter groups ~f:(group_sep_to_list_ref l);
    l := rdelim :: !l
;;

let block_to_list block =
  let l = ref [] in
  block_to_list_ref l block;
  List.rev !l
;;

module Indexed = struct
  type group =
    { items : item list
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

  and alt = token_block [@@deriving sexp, equal, compare]

  let rec remove_trivia_block (t : block) : block =
    { t with groups = List.map t.groups ~f:remove_trivia_group_sep }

  and remove_trivia_group (t : group) : group =
    { items = List.filter_map t.items ~f:remove_trivia_item
    ; block = Option.map t.block ~f:remove_trivia_token_block
    ; alts = List.map t.alts ~f:remove_trivia_token_block
    }

  and remove_trivia_token_block (t : token_block) : token_block =
    { t with block = remove_trivia_block t.block }

  and remove_trivia_group_sep (t : group_sep) : group_sep =
    { t with group = remove_trivia_group t.group }

  and remove_trivia_item (t : item) : item option =
    match t with
    | Token ti when Token.is_trivia ti.token -> None
    | Token _ -> Some t
    | Delim d ->
      Some (Delim { d with groups = List.map d.groups ~f:remove_trivia_group_sep })
  ;;
end

let rec block_to_indexed' (t : block) i : int * Indexed.block =
  let lbrace = { Token.token = t.lbrace; index = i } in
  let i = i + 1 in
  let i, groups =
    List.fold_map t.groups ~init:i ~f:(fun i gs -> group_sep_to_indexed' gs i)
  in
  let rbrace = { Token.token = t.rbrace; index = i } in
  let i = i + 1 in
  i, { Indexed.lbrace; groups; rbrace }

and group_to_indexed' (t : group) i : int * Indexed.group =
  let i, items =
    List.fold_map t.items ~init:i ~f:(fun i item -> item_to_indexed' item i)
  in
  let i, block =
    match t.block with
    | None -> i, None
    | Some tb ->
      let i, tb = token_block_to_indexed' tb i in
      i, Some tb
  in
  let i, alts =
    List.fold_map t.alts ~init:i ~f:(fun i alt -> token_block_to_indexed' alt i)
  in
  i, { Indexed.items; block; alts }

and token_block_to_indexed' (t : token_block) i : int * Indexed.token_block =
  let token = { Token.token = t.token; index = i } in
  let i = i + 1 in
  let i, block = block_to_indexed' t.block i in
  i, { Indexed.token; block }

and group_sep_to_indexed' (t : group_sep) i : int * Indexed.group_sep =
  let i, group = group_to_indexed' t.group i in
  let i, sep =
    match t.sep with
    | None -> i, None
    | Some token ->
      let ti = { Token.token; index = i } in
      i + 1, Some ti
  in
  i, { Indexed.group; sep }

and item_to_indexed' (t : item) i : int * Indexed.item =
  match t with
  | Token token ->
    let ti = { Token.token; index = i } in
    i + 1, Indexed.Token ti
  | Delim { ldelim; groups; rdelim } ->
    let ldelim = { Token.token = ldelim; index = i } in
    let i = i + 1 in
    let i, groups =
      List.fold_map groups ~init:i ~f:(fun i gs -> group_sep_to_indexed' gs i)
    in
    let rdelim = { Token.token = rdelim; index = i } in
    let i = i + 1 in
    i, Indexed.Delim { ldelim; groups; rdelim }
;;

let block_to_indexed t = block_to_indexed' t 0 |> snd
let group_to_indexed t = group_to_indexed' t 0 |> snd

let rec block_of_indexed (t : Indexed.block) : block =
  { lbrace = t.lbrace.token
  ; groups = List.map t.groups ~f:group_sep_of_indexed
  ; rbrace = t.rbrace.token
  }

and group_of_indexed (t : Indexed.group) : group =
  { items = List.map t.items ~f:item_of_indexed
  ; block = Option.map t.block ~f:token_block_of_indexed
  ; alts = List.map t.alts ~f:token_block_of_indexed
  }

and token_block_of_indexed (t : Indexed.token_block) : token_block =
  { token = t.token.token; block = block_of_indexed t.block }

and group_sep_of_indexed (t : Indexed.group_sep) : group_sep =
  { group = group_of_indexed t.group; sep = Option.map t.sep ~f:(fun ti -> ti.token) }

and item_of_indexed (t : Indexed.item) : item =
  match t with
  | Token ti -> Token ti.token
  | Delim { ldelim; groups; rdelim } ->
    Delim
      { ldelim = ldelim.token
      ; groups = List.map groups ~f:group_sep_of_indexed
      ; rdelim = rdelim.token
      }
;;
