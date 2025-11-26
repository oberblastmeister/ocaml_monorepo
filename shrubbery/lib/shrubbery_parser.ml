open Prelude

open struct
  module Token = Shrubbery_token
  module Token_tree = Shrubbery_token_tree
  module Syntax = Shrubbery_syntax
end

module State : sig
  type t

  val create : Token_tree.t array -> t
  val next : t -> Token_tree.t option
  val next_if : t -> f:(Token.t -> bool) -> Token.t option
  val peek : t -> Token_tree.t option
  val expect : t -> Token.t -> Token.t
  val expect_if : t -> f:(Token.t -> bool) -> Token.t
  val debug : t -> string -> unit
end = struct
  type t =
    { tokens : Token_tree.t array
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
    print_s [%message (msg : string) (rest : Token_tree.t array)]
  ;;

  let expect t token =
    let tok = next t in
    match tok with
    | Some (Token tok) when Token.equal tok token -> tok
    | _ -> failwith "expect"
  ;;

  let expect_if t ~f =
    let tok = next t in
    match tok with
    | Some (Token tok) when f tok -> tok
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
    | Some (Token token) when f token ->
      let _ = next t in
      Some token
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
  | Some (Token VLBrace) -> parse_virtual_block st
  | Some (Tree { ldelim = LBrace as lbrace; tts; rdelim = rbrace }) ->
    let groups = parse_delimited_groups ~sep:Token.Semi tts in
    { lbrace; groups; rbrace }
  | tok ->
    raise_s
      [%message
        "precondition: colon should always have VLBrace or delimited LBrace after it"
          ~got:(tok : Token_tree.t option)]

and parse_virtual_block st : Syntax.block = parse_virtual_block_rec st []

and parse_virtual_block_rec (st : State.t) (acc : Syntax.group_sep list) =
  match State.peek st with
  | Some (Token VRBrace) ->
    let _ = State.expect st VRBrace in
    { lbrace = VLBrace; groups = List.rev acc; rbrace = VRBrace }
  | Some _ ->
    let group = parse_group st in
    let sep =
      State.next_if st ~f:(function
        | VSemi | Semi -> true
        | _ -> false)
    in
    parse_virtual_block_rec st ({ group; sep } :: acc)
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
    | Some (Token (Colon | Equal)) -> Some (parse_colon_block st)
    | Some _ | None -> None
  in
  let alts = parse_alts st in
  { items; block; alts }

and parse_items st = parse_items_rec st []

and parse_items_rec st acc =
  match State.peek st with
  | Some (Token (Colon | Pipe | Equal | VSemi | Semi | Comma | VRBrace)) | None ->
    List.rev acc
  | Some _ ->
    let item = parse_item st in
    parse_items_rec st (item :: acc)

and parse_item st =
  match State.next st |> Option.value_exn with
  | Token token -> Syntax.Token token
  | Tree { ldelim; tts; rdelim } ->
    let groups = parse_delimited_groups ~sep:Comma tts in
    Tree { ldelim; groups; rdelim }

and parse_alts st = parse_alts_rec st []

and parse_alts_rec st acc =
  match State.peek st with
  | Some (Token Pipe) ->
    let alt = parse_alt st in
    parse_alts_rec st (alt :: acc)
  | Some _ | None -> List.rev acc

and parse_alt st : Syntax.alt =
  let pipe = State.expect st Pipe in
  let block = parse_block st in
  { token = pipe; block }
;;

(* precondition: 
   the token trees must have been layed out by the layout calculator
   this means that all colons should either be followed by delimited LBrace or by VLBrace ... VRBrace where ... is are arbitrary tokens
   we must also always have VLBrace matched by a VRBrace, which is guaranteed by the layout calculator
*)
let parse_tts tts =
  let st = State.create (Array.of_list tts) in
  parse_block st
;;

let parse ?(remove_trivia = false) s =
  let open struct
    module Lexer = Shrubbery_lexer
    module Delimit = Shrubbery_delimit
    module Layout = Shrubbery_layout
  end in
  let tokens = Lexer.lex s |> Array.of_list in
  let tts, errors = Delimit.delimit tokens in
  let tts = Layout.insert_virtual_tokens tokens (Token_tree.root_to_indexed tts) in
  let tts = if remove_trivia then Token_tree.remove_trivia_root tts else tts in
  let block = parse_tts tts in
  block, errors
;;
