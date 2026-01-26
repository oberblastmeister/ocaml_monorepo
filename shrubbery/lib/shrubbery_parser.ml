open Prelude

open struct
  module Token = Shrubbery_token
  module Token_tree = Shrubbery_token_tree
  module Syntax = Shrubbery_syntax
end

(* TODO: change to use Eof instead of option *)
module State : sig
  type t

  val create : Token_tree.Indexed.t array -> t
  val next : t -> Token_tree.Indexed.t option
  val next_if : t -> f:(Token.t -> bool) -> Token.ti option
  val peek : t -> Token_tree.Indexed.t option
  val expect : t -> Token.t -> Token.ti
  val expect_if : t -> f:(Token.t -> bool) -> Token.ti
  val debug : t -> string -> unit
end = struct
  type t =
    { tokens : Token_tree.Indexed.t array
    ; mutable pos : int
    }

  let create tokens = { tokens; pos = 0 }

  let next t =
    if t.pos >= Array.length t.tokens
    then None
    else begin
      let pos = t.pos in
      t.pos <- pos + 1;
      Some t.tokens.(pos)
    end
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

  let expect_if t ~f =
    let tok = next t in
    match tok with
    | Some (Token ti) when f ti.token -> ti
    | _ -> failwith "expect"
  ;;

  let peek t =
    if t.pos >= Array.length t.tokens
    then None
    else begin
      let pos = t.pos in
      Some t.tokens.(pos)
    end
  ;;

  let next_if t ~f =
    match peek t with
    | Some (Token ti) when f ti.token ->
      let _ = next t in
      Some ti
    | Some _ | None -> None
  ;;
end

let rec parse_colon_block st : Syntax.token_block =
  let token =
    State.expect_if st ~f:(function
      | Colon | Equal -> true
      | _ -> false)
  in
  let block = parse_block st in
  { token; block }

and parse_block st : Syntax.block =
  match State.next st with
  | Some (Token ({ token = VLBrace; _ } as ti)) -> parse_virtual_block st ti
  | Some (Delim { ldelim = { token = LBrace; _ } as ldelim; tts; rdelim }) ->
    let groups = parse_delimited_groups ~sep:Token.Semi tts in
    { lbrace = ldelim; groups; rbrace = rdelim }
  | tok ->
    raise_s
      [%message
        "precondition: colon should always have VLBrace or delimited LBrace after it"
          ~got:(tok : Token_tree.Indexed.t option)]

and parse_virtual_block st lbrace : Syntax.block = parse_virtual_block_rec st lbrace []

and parse_virtual_block_rec (st : State.t) lbrace (acc : Syntax.group_sep list) =
  match State.peek st with
  | Some (Token { token = VRBrace; _ }) ->
    let rbrace = State.expect st VRBrace in
    { lbrace; groups = List.rev acc; rbrace }
  | Some _ ->
    let group = parse_group st in
    let sep =
      State.next_if st ~f:(function
        | VSemi | Semi -> true
        | _ -> false)
    in
    parse_virtual_block_rec st lbrace ({ group; sep } :: acc)
  | None -> failwith "precondition: VLBrace should always have matching VRBrace"

and parse_delimited_groups ~sep tts =
  let st = State.create (Array.of_list tts) in
  parse_delimited_groups_rec ~sep st []

and parse_delimited_groups_rec ~sep st (acc : Syntax.group_sep list) =
  match State.peek st with
  | None -> List.rev acc
  | Some _ ->
    let group = parse_group st in
    let sep_tok = State.next_if st ~f:(Token.equal sep) in
    parse_delimited_groups_rec ~sep st ({ group; sep = sep_tok } :: acc)

and parse_group (st : State.t) : Syntax.group =
  let items = parse_items st in
  let block =
    match State.peek st with
    | Some (Token { token = Colon | Equal; _ }) -> Some (parse_colon_block st)
    | Some _ | None -> None
  in
  let alts = parse_alts st in
  { items; block; alts }

and parse_items st = parse_items_rec st []

and parse_items_rec st acc =
  match State.peek st with
  | Some (Token { token = Colon | Pipe | Equal | VSemi | Semi | Comma | VRBrace; _ })
  | None -> List.rev acc
  | Some _ ->
    let item = parse_item st in
    parse_items_rec st (item :: acc)

and parse_item st : Syntax.item =
  match State.next st |> Option.value_exn with
  | Token ti -> Token ti
  | Delim { ldelim; tts; rdelim } ->
    let groups = parse_delimited_groups ~sep:Comma tts in
    Delim { ldelim; groups; rdelim }

and parse_alts st = parse_alts_rec st []

and parse_alts_rec st acc =
  match State.peek st with
  | Some (Token { token = Pipe; _ }) ->
    let alt = parse_alt st in
    parse_alts_rec st (alt :: acc)
  | Some _ | None -> List.rev acc

and parse_alt st : Syntax.alt =
  let token = State.expect st Pipe in
  let block = parse_block st in
  { token; block }
;;

(* precondition: 
   the token trees must have been layed out by the layout calculator
   this means that all colons should either be followed by delimited LBrace or by VLBrace ... VRBrace where ... is are arbitrary tokens
   we must also always have VLBrace matched by a VRBrace, which is guaranteed by the layout calculator
   trivia must have been removed from the token trees
*)
let parse_tts tts : Syntax.block =
  let st = State.create (Array.of_list tts) in
  let block = parse_block st in
  let _ = State.expect st Veof in
  block
;;

let parse s =
  let open struct
    module Lexer = Shrubbery_lexer
    module Delimit = Shrubbery_delimit
    module Layout = Shrubbery_layout
  end in
  let tokens = Lexer.lex s |> Array.of_list in
  (* TODO: the errors should be shown immediately or else the index will be messed up *)
  let tts, errors = Delimit.delimit tokens in
  let tts = Token_tree.Root.to_indexed tts in
  let tts = Layout.insert_virtual_tokens tokens tts in
  let tts = Token_tree.Root.to_indexed tts in
  let tts = Token_tree.Indexed.Root.remove_trivia tts in
  let block = parse_tts tts in
  tts, block, errors
;;
