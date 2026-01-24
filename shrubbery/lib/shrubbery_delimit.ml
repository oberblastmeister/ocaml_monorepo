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
  [@@deriving sexp]
end

module State : sig
  type t

  val create : Token.t array -> t

  (*
    we return ti here because we may use the token index in Error.t
  *)
  val next : t -> Token.ti option
  val peek : t -> Token.ti option
  val add_error : t -> Error.t -> unit
  val finish : t -> Error.t list
  val last_token : t -> Token.ti option
end = struct
  type t =
    { tokens : Token.t array
    ; mutable pos : int
    ; mutable errors : Error.t list
    }

  let create tokens = { tokens; pos = 0; errors = [] }

  let last_token t =
    let len = Array.length t.tokens in
    if len = 0 then None else Some Token.{ token = t.tokens.(len - 1); index = len - 1 }
  ;;

  let next t =
    if t.pos >= Array.length t.tokens
    then None
    else begin
      let pos = t.pos in
      t.pos <- pos + 1;
      Some Token.{ token = t.tokens.(pos); index = pos }
    end
  ;;

  let peek t =
    if t.pos >= Array.length t.tokens
    then None
    else begin
      let pos = t.pos in
      Some Token.{ token = t.tokens.(pos); index = pos }
    end
  ;;

  let add_error t e = t.errors <- e :: t.errors
  let finish t = t.errors
end

(* precondition, st.tokens must not be empty *)
let rec single st : Token_tree.t =
  match State.next st with
  | None -> assert false
  | Some t when is_left_delim t.Token.token ->
    let ldelim = t in
    let tts = many st false in
    let rdelim =
      match State.next st with
      | None ->
        let last_token =
          Option.value_exn
            ~message:"Should have last token because we matched on Some above"
            (State.last_token st)
        in
        State.add_error st (Error.Expecting_delimiter last_token);
        last_token
      | Some t when Token.equal (to_close ldelim.token) t.Token.token -> t
      | Some rdelim ->
        State.add_error st (Error.Mismatching_delimiters { ldelim; rdelim });
        rdelim
    in
    Token_tree.Delim { ldelim = ldelim.token; tts; rdelim = rdelim.token }
  | Some t -> Token_tree.Token t.token

and many st is_top_level = many_rec [] st is_top_level

and many_rec acc st is_top_level =
  match State.peek st with
  | None -> List.rev acc
  | Some t when is_right_delim t.Token.token ->
    if is_top_level
    then begin
      let _ = State.next st in
      many_rec (Token (Error (Token.to_string t.token)) :: acc) st is_top_level
    end
    else List.rev acc
  | Some { token = Comma; _ } when is_top_level ->
    let _ = State.next st in
    (* commas at the top levels are errors, because they are not surrounded by delimiters *)
    many_rec (Token (Error ",") :: acc) st is_top_level
  | Some _ ->
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
    ((Token Newline) (Token (Whitespace 4)) (Token (Ident awef))
     (Token (Whitespace 1)) (Token (Ident call_function))
     (Delim (ldelim LParen)
      (tts
       ((Token (Ident fiaefw)) (Token Comma) (Token (Whitespace 1))
        (Token (Ident waef)) (Token Comma) (Token (Whitespace 1))
        (Delim (ldelim LParen)
         (tts
          ((Token (Ident aewf)) (Token Comma) (Token (Whitespace 1))
           (Delim (ldelim LBrack)
            (tts
             ((Token (Ident aewf)) (Token (Whitespace 1))
              (Delim (ldelim LBrace) (tts ((Token (Ident awef))))
               (rdelim RBrace))))
            (rdelim RBrack))
           (Token (Whitespace 1)) (Token (Ident awef))))
         (rdelim RParen))))
      (rdelim RParen))
     (Token Newline) (Token (Whitespace 4))
     (Delim (ldelim LBrack)
      (tts
       ((Token (Ident another)) (Token (Whitespace 1)) (Token (Ident one))
        (Token Comma) (Token (Whitespace 1)) (Token (Ident another))
        (Token (Whitespace 1)) (Token (Ident one))))
      (rdelim RBrack))
     (Token Newline) (Token (Whitespace 4)))
    ()
    |}]
;;
