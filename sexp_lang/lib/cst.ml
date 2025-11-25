open! O

type t =
  | Atom of
      { span : Span.t
      ; value : string
      }
  | Keyword of
      { span : Span.t
      ; value : string
      }
  | List of
      { span : Span.t
      ; items : t list
      }
[@@deriving equal, compare, sexp]

module Token = struct
  type t =
    | LParen
    | RParen
    | LBrack
    | RBrack
    | LBrace
    | RBrace
    | Atom of string
    | Keyword of string
    | Error of string
  [@@deriving sexp, compare, equal]
end

module SpannedToken = struct
  type t =
    { token : Token.t
    ; span : Span.t
    }
  [@@deriving sexp]
end

let take_while_from ~f ~pos s =
  let rec go pos =
    if pos >= String.length s then pos else if f s.[pos] then go (pos + 1) else pos
  in
  go pos
;;

let is_control_char c =
  Char.(
    is_whitespace c
    || equal c '('
    || equal c ')'
    || equal c '['
    || equal c ']'
    || equal c '{'
    || equal c '}')
;;

let tokenize s : SpannedToken.t list =
  let len = String.length s in
  let rec go pos =
    if pos >= len
    then []
    else (
      let c = s.[pos] in
      match c with
      | ';' ->
        let pos = take_while_from ~f:(fun c -> Char.(c <> '\n')) ~pos s in
        go pos
      | '(' ->
        ({ span = Span.single pos; token = LParen } : SpannedToken.t) :: go (pos + 1)
      | ')' -> { span = Span.single pos; token = RParen } :: go (pos + 1)
      | '[' -> { span = Span.single pos; token = LBrack } :: go (pos + 1)
      | ']' -> { span = Span.single pos; token = RBrack } :: go (pos + 1)
      | '{' -> { span = Span.single pos; token = LBrace } :: go (pos + 1)
      | '}' -> { span = Span.single pos; token = RBrace } :: go (pos + 1)
      | '#' when pos + 1 < len && Char.(s.[pos + 1] = ':') ->
        let pos = pos + 2 in
        if pos < len
        then (
          let pos' = take_while_from ~pos ~f:(fun c -> not (Char.is_whitespace c)) s in
          let tok = String.slice s pos pos' in
          if String.is_empty tok
          then
            { span = { start = pos; stop = pos' }; token = Error "empty keyword" }
            :: go pos'
          else { span = { start = pos; stop = pos' }; token = Keyword tok } :: go pos')
        else [ { span = Span.single pos; token = Error "unexpected eof" } ]
      | c when Char.is_whitespace c -> go (pos + 1)
      | _ ->
        let pos' = take_while_from ~pos ~f:(fun c -> not (is_control_char c)) s in
        { span = { start = pos; stop = pos' }; token = Atom (String.slice s pos pos') }
        :: go pos')
  in
  go 0
;;

let%expect_test _ =
  String.slice "hello" 1 3 |> print_endline;
  [%expect {| el |}]
;;

let%expect_test _ =
  tokenize "( ( as   [  ] ]        )" |> [%sexp_of: SpannedToken.t list] |> print_s;
  [%expect
    {|
    (((token LParen) (span ((start 0) (stop 1))))
     ((token LParen) (span ((start 2) (stop 3))))
     ((token (Atom as)) (span ((start 4) (stop 6))))
     ((token LBrack) (span ((start 9) (stop 10))))
     ((token RBrack) (span ((start 12) (stop 13))))
     ((token RBrack) (span ((start 14) (stop 15))))
     ((token RParen) (span ((start 23) (stop 24))))) |}]
;;

let error_preview_amount = 5

let unexpected_token t ts =
  Or_error.error_s
    [%sexp
      "unexpected token"
    , (t : Token.t)
    , (List.take ts error_preview_amount : SpannedToken.t list)]
;;

let expected_tokens expected_tokens actual_token ts =
  Or_error.error_s
    [%sexp
      "expected tokens"
    , (expected_tokens : Token.t list)
    , "got"
    , (actual_token : SpannedToken.t)
    , "rest"
    , (List.take ts error_preview_amount : SpannedToken.t list)]
;;

let unexpected_eof expected_tokens ts =
  Or_error.error_s
    [%sexp
      "unexpected end of input"
    , "wanted"
    , (expected_tokens : Token.t list)
    , "rest"
    , (List.take ts error_preview_amount : SpannedToken.t list)]
;;

let parse_tokens ts =
  let open Result.Let_syntax in
  let rec parse_many ts =
    match ts with
    | [] -> Ok ([], [])
    | ({ token; _ } : SpannedToken.t) :: _ ->
      (match token with
       | Token.RParen | RBrack | RBrace -> Ok ([], ts)
       | _ ->
         let%bind x, ts = parse_one ts in
         let%bind xs, ts = parse_many ts in
         Ok (x :: xs, ts))
  and parse_one ts =
    match ts with
    | [] -> unexpected_eof [] ts
    | ({ span; token } : SpannedToken.t) :: ts ->
      (match token with
       | LParen -> parse_list span Token.RParen ts
       | LBrack -> parse_list span RBrack ts
       | LBrace -> parse_list span RBrace ts
       | Atom value -> Ok (Atom { span; value }, ts)
       | Keyword value -> Ok (Keyword { span; value }, ts)
       | _ -> unexpected_token token ts)
  and parse_list start_span close ts =
    let%bind xs, ts = parse_many ts in
    match ts with
    | ({ span; token } : SpannedToken.t) :: ts when Token.equal token close ->
      Ok (List { span = Span.combine start_span span; items = xs }, ts)
    | t :: _ -> expected_tokens [ close ] t ts
    | [] -> unexpected_eof [ close ] ts
  in
  let%bind xs, ts = parse_many ts in
  match ts with
  | [] -> Ok xs
  | _ -> Or_error.error_s [%sexp "unexpected tokens", (ts : SpannedToken.t list)]
;;

let parse_tokens_single ts =
  let open Result.Let_syntax in
  match%bind parse_tokens ts with
  | [ x ] -> Ok x
  | _ ->
    Error
      (Error.t_of_sexp
         [%sexp
           "expected single expression"
         , (List.take ts error_preview_amount : SpannedToken.t list)])
;;

let parse s = tokenize s |> parse_tokens
let parse_single s = tokenize s |> parse_tokens_single

let span = function
  | Atom { span; _ } -> span
  | List { span; _ } -> span
  | Keyword { span; _ } -> span
;;

let%expect_test _ =
  let s =
    {|(define (fun first second)
       (let x (add x y)))|}
  in
  parse_single s |> Or_error.ok_exn |> [%sexp_of: t] |> print_s;
  [%expect
    {|
    (List (span ((start 0) (stop 52)))
     (items
      ((Atom (span ((start 1) (stop 7))) (value define))
       (List (span ((start 8) (stop 26)))
        (items
         ((Atom (span ((start 9) (stop 12))) (value fun))
          (Atom (span ((start 13) (stop 18))) (value first))
          (Atom (span ((start 19) (stop 25))) (value second)))))
       (List (span ((start 34) (stop 51)))
        (items
         ((Atom (span ((start 35) (stop 38))) (value let))
          (Atom (span ((start 39) (stop 40))) (value x))
          (List (span ((start 41) (stop 50)))
           (items
            ((Atom (span ((start 42) (stop 45))) (value add))
             (Atom (span ((start 46) (stop 47))) (value x))
             (Atom (span ((start 48) (stop 49))) (value y))))))))))) |}]
;;

let%expect_test "parse" =
  let tokens =
    tokenize
      {|
  (define (testing [x u64] [y u64]) u64)
  |}
  in
  print_s [%sexp (tokens : SpannedToken.t list)];
  [%expect
    {|
    (((token LParen) (span ((start 3) (stop 4))))
     ((token (Atom define)) (span ((start 4) (stop 10))))
     ((token LParen) (span ((start 11) (stop 12))))
     ((token (Atom testing)) (span ((start 12) (stop 19))))
     ((token LBrack) (span ((start 20) (stop 21))))
     ((token (Atom x)) (span ((start 21) (stop 22))))
     ((token (Atom u64)) (span ((start 23) (stop 26))))
     ((token RBrack) (span ((start 26) (stop 27))))
     ((token LBrack) (span ((start 28) (stop 29))))
     ((token (Atom y)) (span ((start 29) (stop 30))))
     ((token (Atom u64)) (span ((start 31) (stop 34))))
     ((token RBrack) (span ((start 34) (stop 35))))
     ((token RParen) (span ((start 35) (stop 36))))
     ((token (Atom u64)) (span ((start 37) (stop 40))))
     ((token RParen) (span ((start 40) (stop 41))))) |}]
;;
