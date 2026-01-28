open Prelude

open struct
  module Span = Utility.Span
  module Token = Shrubbery_token
  module Token_tree = Shrubbery_token_tree
  module Lexer = Shrubbery_lexer
end

let to_close = function
  | Token.LParen -> Token.RParen
  | Token.LBrace -> Token.RBrace
  | Token.LBrack -> Token.RBrack
  | _ -> failwith "not a left delimiter"
;;

let is_left_delim = function
  | Token.LParen | LBrace | LBrack -> true
  | _ -> false
;;

let is_right_delim = function
  | Token.RParen | RBrace | RBrack -> true
  | _ -> false
;;

(* these spans are byte positions *)
module Error = struct
  type t =
    | Mismatching_delimiters of
        { ldelim : Span.t
        ; rdelim : Span.t
        }
    | Expecting_delimiter of Span.t
  [@@deriving sexp_of]
end

module State : sig
  type t

  val create : Token.t array -> t

  (*
    we return ti here because we may use the token index in Error.t
  *)
  val next : t -> Token.ti
  val peek : t -> Token.ti
  val add_error : t -> Error.t -> unit
  val finish : t -> Error.t list
  val last_token : t -> Token.ti
  val get_span : t -> Token.ti -> Span.t
end = struct
  type t =
    { tokens : Token.t array
    ; mutable pos : int
    ; mutable errors : Error.t list
    ; token_positions : int array Lazy.t
    }

  let create tokens =
    { tokens
    ; pos = 0
    ; errors = []
    ; token_positions = lazy (Token.calculate_offsets tokens)
    }
  ;;

  let get t i =
    let end_i = Array.length t.tokens - 1 in
    if i >= end_i
    then begin
      assert (Token.equal t.tokens.(end_i) Token.Veof);
      { Token.token = t.tokens.(end_i); index = i }
    end
    else begin
      { Token.token = t.tokens.(i); index = i }
    end
  ;;

  let last_token t = get t (Array.length t.tokens - 1)

  let next t =
    let tok = get t t.pos in
    t.pos <- t.pos + 1;
    tok
  ;;

  let peek t = get t t.pos
  let add_error t e = t.errors <- e :: t.errors
  let finish t = t.errors
  let get_pos t i = (Lazy.force t.token_positions).(min (Array.length t.tokens - 1) i)

  let get_span t (token : Token.ti) =
    let start = get_pos t token.index in
    let stop = get_pos t (token.index + 1) in
    { Span.start; stop }
  ;;
end

(* precondition, st.tokens must not be empty *)
let rec single st : Token_tree.t =
  match State.next st with
  | { token = Veof; _ } -> assert false
  | t when is_left_delim t.Token.token ->
    let ldelim = t in
    let tts = many st false in
    let rdelim =
      match State.next st with
      | { token = Veof; _ } as tok ->
        State.add_error st (Error.Expecting_delimiter (State.get_span st tok));
        tok
      | t when Token.equal (to_close ldelim.token) t.Token.token -> t
      | rdelim ->
        State.add_error
          st
          (Error.Mismatching_delimiters
             { ldelim = State.get_span st ldelim; rdelim = State.get_span st rdelim });
        rdelim
    in
    Token_tree.Delim { ldelim = ldelim.token; tts; rdelim = rdelim.token }
  | t -> Token_tree.Token t.token

and many st is_top_level = many_rec [] st is_top_level

and many_rec acc st is_top_level =
  match State.peek st with
  | { token = Veof; _ } -> List.rev acc
  | t when is_right_delim t.Token.token ->
    if is_top_level
    then begin
      let _ = State.next st in
      many_rec (Token (Error (Token.to_string t.token)) :: acc) st is_top_level
    end
    else List.rev acc
  | { token = Comma; _ } when is_top_level ->
    let _ = State.next st in
    (* commas at the top levels are errors, because they are not surrounded by delimiters *)
    many_rec (Token (Error ",") :: acc) st is_top_level
  | _ ->
    let tt = single st in
    many_rec (tt :: acc) st is_top_level
;;

let delimit (tokens : Token.t array) : Token_tree.t list * Error.t list =
  let st = State.create tokens in
  let tts = many st true in
  let tts = tts @ [ Token Veof ] in
  let errors = State.finish st in
  tts, errors
;;

let check s =
  let tokens = Lexer.lex s |> Array.of_list in
  let tts, errors = delimit tokens in
  print_s [%sexp (tts : Token_tree.t list)];
  print_s [%sexp (errors : Error.t list)]
;;

let%expect_test "smoke" =
  check
    {|
    awef call_function(fiaefw, waef, (aewf, [aewf {awef}] awef))
    [another one, another one]
    |};
  [%expect
    {|
    ("\n" "    " awef " " call_function
     ("(" fiaefw , " " waef , " "
      ("(" aewf , " " ([ aewf " " ({ awef }) ]) " " awef ")") ")")
     "\n" "    " ([ another " " one , " " another " " one ]) "\n" "    " _eof)
    ()
    |}]
;;

let%expect_test "unclosed paren" =
  check "f(x, y";
  [%expect
    {|
    (f ("(" x , " " y _eof) _eof)
    ((Expecting_delimiter ((start 6) (stop 6))))
    |}]
;;
