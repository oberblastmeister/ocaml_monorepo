open Prelude

open struct
  module Lexer = Shrubbery_lexer
  module Token = Shrubbery_token
  module Token_tree = Shrubbery_token_tree
  module Delimit = Shrubbery_delimit
end

type context =
  | Unknown_indent
  | Known_indent of int
[@@deriving sexp, equal, compare]

type st =
  { mutable contexts : context list
  ; mutable tts : Token_tree.t list
  }

let create_state () = { contexts = []; tts = [] }
let finish_state st = List.rev st.tts

let top_is st context =
  List.hd st.contexts
  |> Option.map ~f:(fun cx -> equal_context cx context)
  |> Option.value ~default:false
;;

let add_context st context = st.contexts <- context :: st.contexts
let start_block st = add_context st Unknown_indent

let pop_context_exn st context =
  let hd, tl = List.hd_exn st.contexts, List.tl_exn st.contexts in
  assert (equal_context context hd);
  st.contexts <- tl
;;

let add_tt st tt = st.tts <- tt :: st.tts

let pop_all st =
  while not (List.is_empty st.contexts) do
    let hd, tl = List.hd_exn st.contexts, List.tl_exn st.contexts in
    if equal_context hd Unknown_indent then add_tt st (Token VLBrace);
    add_tt st (Token VRBrace);
    st.contexts <- tl
  done
;;

let pop_offside ~add_semi st col =
  let rec loop () =
    match st.contexts with
    | [] -> ()
    | hd :: tl -> begin
      let pop_block () =
        st.contexts <- tl;
        add_tt st (Token VRBrace);
        loop ()
      in
      match hd with
      | Known_indent col' ->
        if col > col'
        then ()
        else if col = col'
        then begin
          if add_semi then add_tt st (Token VSemi)
        end
        else begin
          pop_block ()
        end
      | Unknown_indent -> begin
        add_tt st (Token VLBrace);
        match tl with
        | Unknown_indent :: _ -> assert false
        | Known_indent prev_col :: _ ->
          if col > prev_col
          then begin
            st.contexts <- Known_indent col :: tl
          end
          else begin
            pop_block ()
          end
        | [] -> st.contexts <- Known_indent col :: []
      end
    end
  in
  loop ()
;;

(* precondition: 
   commas must not appear at the top level where they are not surrounded by delimiters.
   The Delimit module will ensure that this condition is met.
*)
let insert_virtual_tokens tokens tts =
  let line_cols = Token.calculate_line_col tokens in
  let rec go_inner tts =
    let st = create_state () in
    go st tts;
    pop_all st;
    let res = finish_state st in
    res
  and go_root tts =
    let st = create_state () in
    add_tt st (Token VLBrace);
    add_context st (Known_indent 0);
    go st tts;
    pop_all st;
    let res = finish_state st in
    res
  and go (st : st) (tts : Token_tree.Indexed.t list) =
    List.iter tts ~f:(fun tt -> go_tt st tt)
  and go_tt st (tt : Token_tree.Indexed.t) =
    begin match tt with
    | Token token when Token.is_trivia token.token ->
      (* skip over trivia tokens *)
      add_tt st (Token token.token)
    | Token token -> begin
      let curr_lc = line_cols.(token.index) in
      if Token.equal token.token Comma
      then pop_all st
      else pop_offside st curr_lc.col ~add_semi:(not (Token.equal token.token Pipe));
      begin
        begin match token.token with
        | Colon | Pipe | Equal -> start_block st
        | _ -> ()
        end;
        add_tt st (Token token.token)
      end
    end
    | Delim { ldelim; tts = inner_tts; rdelim } ->
      let curr_lc = line_cols.(ldelim.index) in
      let inner_tts = go_inner inner_tts in
      let tt =
        Token_tree.Delim { ldelim = ldelim.token; tts = inner_tts; rdelim = rdelim.token }
      in
      if Token.equal ldelim.token LBrace && top_is st Unknown_indent
      then begin
        pop_context_exn st Unknown_indent
      end
      else begin
        pop_offside st curr_lc.col ~add_semi:true
      end;
      add_tt st tt
    end
  in
  go_root tts
;;
