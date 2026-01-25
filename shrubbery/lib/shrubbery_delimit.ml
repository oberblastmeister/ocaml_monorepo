open Prelude

open struct
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

module Error = struct
  type t =
    | Mismatching_delimiters of
        { ldelim : Token.ti
        ; rdelim : Token.ti
        }
    | Expecting_delimiter of Token.ti
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
end = struct
  type t =
    { tokens : Token.t array
    ; mutable pos : int
    ; mutable errors : Error.t list
    }

  let create tokens = { tokens; pos = 0; errors = [] }

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
      | { token = Veof; _ } ->
        let last_token = State.last_token st in
        State.add_error st (Error.Expecting_delimiter last_token);
        last_token
      | t when Token.equal (to_close ldelim.token) t.Token.token -> t
      | rdelim ->
        State.add_error st (Error.Mismatching_delimiters { ldelim; rdelim });
        rdelim
    in
    Token_tree.Delim { ldelim = ldelim.token; tts; rdelim = rdelim.token }
  | t -> Token_tree.Token t.token

and many st is_top_level = many_rec [] st is_top_level

and many_rec acc st is_top_level =
  match State.peek st with
  | { token = Veof; _ } -> List.rev (Token_tree.Token Veof :: acc)
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
  let errors = State.finish st in
  tts, errors
;;

let%expect_test "smoke" =
  let check s =
    let tokens = Lexer.lex s |> Array.of_list in
    let tts, errors = delimit tokens in
    print_s [%sexp (tts : Token_tree.t list)];
    print_s [%sexp (errors : Error.t list)]
  in
  check
    {|
    awef call_function(fiaefw, waef, (aewf, [aewf {awef}] awef))
    [another one, another one]
    |};
  [%expect
    {|
    ((Token "\n") (Token "    ") (Token awef) (Token " ") (Token call_function)
     (Delim (ldelim "(")
      (tts
       ((Token fiaefw) (Token ,) (Token " ") (Token waef) (Token ,) (Token " ")
        (Delim (ldelim "(")
         (tts
          ((Token aewf) (Token ,) (Token " ")
           (Delim (ldelim [)
            (tts
             ((Token aewf) (Token " ")
              (Delim (ldelim {) (tts ((Token awef))) (rdelim }))))
            (rdelim ]))
           (Token " ") (Token awef)))
         (rdelim ")"))))
      (rdelim ")"))
     (Token "\n") (Token "    ")
     (Delim (ldelim [)
      (tts
       ((Token another) (Token " ") (Token one) (Token ,) (Token " ")
        (Token another) (Token " ") (Token one)))
      (rdelim ]))
     (Token "\n") (Token "    ") (Token _eof))
    ()
    |}]
;;
