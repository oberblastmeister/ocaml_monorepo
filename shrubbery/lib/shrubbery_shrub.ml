open Prelude

open struct
  module Token_tree = Shrubbery_token_tree
  module Token = Shrubbery_token
end

type group =
  { items : item list
  ; block : colon_block option
  ; alts : alt list
  }

and item =
  | Token of Token.t
  | Tree of
      { ldelim : Token.t
      ; groups : group_sep list
      ; rdelim : Token.t
      }

and colon_block =
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

and alt =
  { pipe : Token.t
  ; block : block
  }
[@@deriving sexp, equal, compare]

let rec block_to_list_ref l t =
  l := t.lbrace :: !l;
  List.iter t.groups ~f:(group_sep_to_list_ref l);
  l := t.rbrace :: !l

and group_to_list_ref l t =
  List.iter t.items ~f:(item_to_list_ref l);
  Option.iter t.block ~f:(colon_block_to_list_ref l);
  List.iter t.alts ~f:(alt_to_list_ref l);
  ()

and colon_block_to_list_ref l t =
  l := t.token :: !l;
  block_to_list_ref l t.block

and group_sep_to_list_ref l t =
  group_to_list_ref l t.group;
  Option.iter t.sep ~f:(fun token -> l := token :: !l)

and item_to_list_ref l t =
  match t with
  | Token token -> l := token :: !l
  | Tree { ldelim; groups; rdelim } ->
    l := ldelim :: !l;
    List.iter groups ~f:(group_sep_to_list_ref l);
    l := rdelim :: !l

and alt_to_list_ref l t =
  l := t.pipe :: !l;
  block_to_list_ref l t.block
;;

let block_to_list block =
  let l = ref [] in
  block_to_list_ref l block;
  List.rev !l
;;

(* module Indexed = struct
  type group =
    { items : item list
    ; block : block option
    ; alts : alt list
    ; semi : Token.ti option
    }

  and item =
    | Token of Token.ti
    | Tree of
        { ldelim : Token.ti
        ; groups : group list
        ; rdelim : Token.ti
        }

  and block =
    { colon : Token.ti
    ; groups : group list
    }

  and alt =
    { pipe : Token.ti
    ; groups : group list
    }
  [@@deriving sexp, equal, compare]
end *)
