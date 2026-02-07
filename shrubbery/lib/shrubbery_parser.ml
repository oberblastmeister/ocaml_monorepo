open Prelude

open struct
  module Token = Shrubbery_token
  module Token_tree = Shrubbery_token_tree
  module Syntax = Shrubbery_syntax
end

module State = struct
  type t =
    { tokens : Token_tree.Indexed.t array
    ; mutable pos : int
    }

  let create tokens = { tokens; pos = 0 }
  let get t i = if i >= Array.length t.tokens then None else Some t.tokens.(i)
  let peek t = get t t.pos

  let next t =
    let res = get t t.pos in
    t.pos <- t.pos + 1;
    res
  ;;

  let debug st msg =
    let rest = Array.slice st.tokens st.pos 0 in
    print_s [%message (msg : string) (rest : Token_tree.Indexed.t array)]
  ;;

  let expect t token =
    let tok = next t in
    match tok with
    | Some (Token ti) when Token.equal ti.token token -> ti
    | _ ->
      raise_s
        [%message
          "expected token"
            ~expected:(token : Token.t)
            ~actual:(tok : Token_tree.Indexed.t option)]
  ;;
end

let rec parse_item st : Syntax.item =
  match State.next st with
  | None -> assert false
  | Some (Token ti) -> Token ti
  | Some (Delim { ldelim; tts; rdelim }) ->
    let sep =
      if Token.equal ldelim.token LBrace
      then [ Token.Semi; Token.VSemi ]
      else [ Token.Comma ]
    in
    Delim { ldelim; groups = parse_delim_inner tts sep; rdelim }

and parse_groups_until_sep_rec st sep acc : Syntax.group_sep option =
  match State.peek st with
  | None ->
    List.rev acc
    |> Non_empty_list.of_list
    |> Option.map ~f:(fun group -> ({ group; sep = None } : Syntax.group_sep))
  | Some (Token { token = Veof; _ }) ->
    List.rev acc
    |> Non_empty_list.of_list
    |> Option.map ~f:(fun group -> ({ group; sep = None } : Syntax.group_sep))
  | Some (Token ({ token; _ } as ti))
    when Option.is_some (List.find sep ~f:(Token.equal token)) ->
    let _ = State.expect st token in
    List.rev acc
    |> Non_empty_list.of_list
    |> Option.map ~f:(fun group -> ({ group; sep = Some ti } : Syntax.group_sep))
  | _ ->
    let item = parse_item st in
    parse_groups_until_sep_rec st sep (item :: acc)

and parse_groups_until_sep st sep = parse_groups_until_sep_rec st sep []

and parse_delim_inner_rec st sep acc : Syntax.group_sep list =
  match State.peek st with
  | None -> List.rev acc
  | _ ->
    parse_delim_inner_rec
      st
      sep
      (match parse_groups_until_sep st sep with
       | None -> acc
       | Some t -> t :: acc)

and parse_delim_inner tts (sep : Token.t list) : Syntax.group_sep list =
  let st = State.create (Array.of_list tts) in
  parse_delim_inner_rec st sep []
;;

let parse_tts tts : Syntax.group option =
  let st = State.create (Array.of_list tts) in
  let group = parse_groups_until_sep st [] in
  Option.map group ~f:(fun g -> g.group)
;;

let parse s =
  let open struct
    module Lexer = Shrubbery_lexer
    module Delimit = Shrubbery_delimit
    module Layout = Shrubbery_layout
  end in
  let tokens = Lexer.lex s |> Array.of_list in
  let tts, errors = Delimit.delimit tokens in
  let tts = Token_tree.Root.to_indexed tts in
  let tts_save = Layout.insert_virtual_tokens tokens tts in
  let tts = Token_tree.Root.to_indexed tts_save in
  let tts = Token_tree.Indexed.Root.remove_trivia tts in
  let group = parse_tts tts in
  tts_save, group, errors
;;
