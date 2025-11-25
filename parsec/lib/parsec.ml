open Core
include Parsec_intf

module Make_stream (Token : Token) : sig
  include Stream with module Token = Token and type Chunk.t = Token.t array

  val create : Chunk.t -> t
end = struct
  type t =
    { tokens : Token.t Array.t
    ; mutable pos : int
    }
  [@@deriving sexp_of]

  module Token = Token

  module Chunk = struct
    type t = Token.t array [@@deriving sexp_of, compare, equal]
  end

  module Snapshot = Int

  let create tokens = { tokens; pos = 0 }

  let next ({ pos; tokens } as stream) =
    if pos < Array.length tokens
    then begin
      let t = tokens.(pos) in
      stream.pos <- succ pos;
      Some t
    end
    else None
  ;;

  let peek { pos; tokens } = if pos < Array.length tokens then Some tokens.(pos) else None
  let snapshot t = t.pos
  let restore t pos = t.pos <- pos
end

module String_stream = struct
  module Token = Char
  module Chunk = String
  module Snapshot = Int

  type t =
    { s : string
    ; mutable i : int
    }
  [@@deriving sexp_of]

  let create s = { s; i = 0 }

  let next t =
    if t.i < String.length t.s
    then begin
      let res = t.s.[t.i] in
      t.i <- succ t.i;
      Some res
    end
    else None
  ;;

  let peek t =
    if t.i < String.length t.s
    then begin
      let res = t.s.[t.i] in
      Some res
    end
    else None
  ;;

  let snapshot t = t.i
  let restore t i = t.i <- i
end

module String_array_stream = Make_stream (String)

module Make (Arg : Arg) = struct
  module Arg = Arg
  open Arg
  module Token = Stream.Token

  module Exceptions = struct
    exception Error of Error.t * Token.t option [@@deriving sexp_of]
    exception Fail [@@deriving sexp_of]
  end

  open Exceptions

  type env =
    { data : Data.t
    ; mutable stream : Stream.t
    }
  [@@deriving sexp_of]

  let with_env data stream f =
    let env = { data; stream } in
    match f env with
    | exception Error (e, t) -> Parse_result.Error (e, t)
    | exception Fail -> Parse_result.Fail
    | res -> Parse_result.Ok res
  ;;

  let stream env = env.stream
  let fail _ = raise_notrace Fail
  let error env e = raise_notrace (Error (e, Stream.peek env.stream))

  type 'a t = env -> 'a

  let guard b env = if not b then fail env

  let expect_eq t env =
    match Stream.peek env.stream with
    | None -> fail env
    | Some t' when Token.equal t t' ->
      Stream.next env.stream |> ignore;
      ()
    | Some _ -> fail env
  ;;

  let expect f env =
    match Stream.peek env.stream with
    | Some t ->
      (match f t with
       | None -> fail env
       | Some x ->
         Stream.next env.stream |> ignore;
         x)
    | None -> fail env
  ;;

  let orelse p1 p2 env =
    let snap = Stream.snapshot env.stream in
    match p1 env with
    | res -> res
    | exception Fail ->
      Stream.restore env.stream snap;
      p2 env
  ;;

  let pure x _env = x

  let cut e p env =
    match p env with
    | exception Fail -> error env e
    | x -> x
  ;;

  let commit e env f = cut e (fun _env -> f ()) env
  let map p ~f env = f (p env)

  let many p env =
    let rec loop acc =
      let snap = Stream.snapshot env.stream in
      match p env with
      | exception Fail ->
        Stream.restore env.stream snap;
        List.rev acc
      | x -> loop (x :: acc)
    in
    loop []
  ;;

  let some p env =
    let x = p env in
    x :: many p env
  ;;

  let choice ps env =
    let rec loop ps env =
      match ps with
      | [] -> fail env
      | p :: ps ->
        let snap = Stream.snapshot env.stream in
        begin
          match p env with
          | res -> res
          | exception Fail ->
            Stream.restore env.stream snap;
            loop ps env
        end
    in
    loop ps env
  ;;

  module Syntax = struct
    let ( <|> ) = orelse
    let ( <$> ) f p = map p ~f

    let ( <$ ) x p env =
      p env |> ignore;
      x
    ;;

    let ( $> ) p x env =
      p env |> ignore;
      x
    ;;

    let ( <* ) p1 p2 env =
      let x = p1 env in
      p2 env |> ignore;
      x
    ;;

    let ( *> ) p1 p2 env =
      p1 env |> ignore;
      p2 env
    ;;
  end

  let sep1 p ~by env =
    let open Syntax in
    let rec loop acc env =
      ((fun env ->
         by env;
         let x = p env in
         loop (x :: acc) env)
       <|> fun _env -> List.rev acc)
        env
    in
    let x = p env in
    loop [ x ] env
  ;;

  let sep p ~by env =
    let open Syntax in
    ((fun env -> sep1 p ~by env) <|> pure []) env
  ;;

  let optional p = Syntax.(Option.some <$> p <|> pure None)
  let either p1 p2 = Syntax.(Either.first <$> p1 <|> (Either.second <$> p2))
end

module Arg_parse_arg = struct
  module Data = Unit
  module Error = Core.Error
  module Stream = String_array_stream
end

module String = struct
  include String

  let strip_prefix s ~prefix =
    if String.is_prefix s ~prefix
    then Some (String.drop_prefix s (String.length prefix))
    else None
  ;;

  let strip_suffix s ~suffix =
    if String.is_suffix s ~suffix
    then Some (String.drop_suffix s (String.length suffix))
    else None
  ;;
end

module Arg_parse = struct
  include Make (Arg_parse_arg)
  open Syntax

  let parse s f =
    let stream = String_array_stream.create s in
    with_env () stream f
  ;;

  let flag_gen ~prefix env =
    let flag, arg =
      expect
        (fun s ->
           let open Option.Let_syntax in
           let%bind flag = String.strip_prefix ~prefix s in
           return
             (match String.strip_prefix ~prefix:"=" flag with
              | Some arg -> flag, Some arg
              | None -> flag, None))
        env
    in
    flag, arg
  ;;

  let opt_prefix ~flag:s ~prefix env =
    let flag, arg = flag_gen ~prefix env in
    let arg =
      Option.value_or_thunk arg ~default:(fun () ->
        cut
          (Error.create "Expected argument for option" () sexp_of_unit)
          (expect Option.some)
          env)
    in
    guard (String.equal flag s) env;
    arg
  ;;

  let flag_prefix ~flag:s ~prefix env =
    let flag, arg = flag_gen ~prefix env in
    guard (String.equal flag s) env;
    begin
      match arg with
      | Some arg ->
        error
          env
          (Error.create_s [%message "Unexpected argument for flag" (arg : string)])
      | None -> ()
    end;
    ()
  ;;

  let flag1 flag env = flag_prefix ~flag ~prefix:"-" env
  let flag2 flag env = flag_prefix ~flag ~prefix:"--" env
  let flag_any flag env = (flag1 flag <|> flag2 flag) env
  let opt1 flag env = opt_prefix ~flag ~prefix:"-" env
  let opt2 flag env = opt_prefix ~flag ~prefix:"--" env
  let opt_any opt env = (opt1 opt <|> opt2 opt) env
  let positional = expect Option.some
end
