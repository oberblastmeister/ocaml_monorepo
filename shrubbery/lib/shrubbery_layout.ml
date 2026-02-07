open Prelude

open struct
  module Lexer = Shrubbery_lexer
  module Token = Shrubbery_token
  module Token_tree = Shrubbery_token_tree
  module Delimit = Shrubbery_delimit
end

type indentation =
  | No_layout
  | Unknown_indent
  | Known_indent of int
[@@deriving sexp, equal, compare]

type st =
  { mutable tts : Token_tree.t list
  ; mutable indent : indentation
  }

let create_state indent = { indent; tts = [] }
let finish_state st = List.rev st.tts
let add_tt st tt = st.tts <- tt :: st.tts

let offside_rule st col =
  match st.indent with
  | No_layout -> ()
  | Unknown_indent -> st.indent <- Known_indent col
  | Known_indent prev_col ->
    if col > prev_col
    then ()
    else begin
      add_tt st (Token VSemi);
      if col < prev_col then st.indent <- Known_indent col
    end
;;

(* precondition: 
   commas must not appear at the top level where they are not surrounded by delimiters.
   The Delimit module will ensure that this condition is met.
*)
let insert_virtual_tokens tokens tts =
  let line_cols = Token.calculate_line_col tokens in
  let rec loop_tt st (tt : Token_tree.Indexed.t) =
    begin match tt with
    | Token { token = Veof; _ } -> ()
    | Token token when Token.is_trivia token.token ->
      (* skip over trivia tokens *)
      add_tt st (Token token.token)
    | Token token -> begin
      let curr_lc = line_cols.(token.index) in
      offside_rule st curr_lc.col;
      add_tt st (Token token.token)
    end
    | Delim { ldelim; tts = inner_tts; rdelim } ->
      let curr_lc = line_cols.(ldelim.index) in
      offside_rule st curr_lc.col;
      let inner_tts =
        let st =
          create_state
            (if Token.equal ldelim.token LBrace then Unknown_indent else No_layout)
        in
        List.iter inner_tts ~f:(fun tt -> loop_tt st tt);
        finish_state st
      in
      let tt =
        Token_tree.Delim { ldelim = ldelim.token; tts = inner_tts; rdelim = rdelim.token }
      in
      add_tt st tt
    end
  in
  let st = create_state No_layout in
  List.iter tts ~f:(fun tt -> loop_tt st tt);
  add_tt st (Token Veof);
  let res = finish_state st in
  res
;;
