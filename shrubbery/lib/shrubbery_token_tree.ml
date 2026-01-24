open Prelude

open struct
  module Token = Shrubbery_token
end

type t =
  | Token of Token.t
  | Delim of
      { ldelim : Token.t
      ; tts : t list
      ; rdelim : Token.t
      }
[@@deriving sexp, equal, compare]

type root = t list [@@deriving sexp, equal, compare]

module Indexed = struct
  type t =
    | Token of Token.ti
    | Delim of
        { ldelim : Token.ti
        ; tts : t list
        ; rdelim : Token.ti
        }
  [@@deriving sexp, equal, compare]

  type root = t list [@@deriving sexp, equal, compare]

  let first_token t =
    match t with
    | Token tok -> tok
    | Delim { ldelim; _ } -> ldelim
  ;;

  let is_trivia_token = function
    | Token token when Token.is_trivia token.token -> true
    | _ -> false
  ;;
  
  let rec remove_trivia t =
    match t with
    | Token t -> if Token.is_trivia t.token then None else Some (Token t)
    | Delim { ldelim; tts; rdelim } ->
      let tts = List.concat_map tts ~f:(fun tt -> remove_trivia tt |> Option.to_list) in
      Some (Delim { ldelim; tts; rdelim })
  ;;
  
  let remove_trivia_root ts =
    List.concat_map ts ~f:(fun t -> remove_trivia t |> Option.to_list)
  ;;
end

let is_trivia_token = function
  | Token token when Token.is_trivia token -> true
  | _ -> false
;;

let rec to_indexed' tt i =
  match tt with
  | Token token -> i + 1, Indexed.Token { token; index = i }
  | Delim { ldelim; tts; rdelim } ->
    let ldelim = { Token.token = ldelim; index = i } in
    let i = i + 1 in
    let i, tts = List.fold_map tts ~init:i ~f:(fun i tt -> to_indexed' tt i) in
    let rdelim = { Token.token = rdelim; index = i } in
    let i = i + 1 in
    i, Indexed.Delim { ldelim; tts; rdelim }
;;

let to_indexed t = to_indexed' t 0 |> snd

let rec of_indexed t =
  match t with
  | Indexed.Token token -> Token token.token
  | Indexed.Delim { ldelim; tts; rdelim } ->
    Delim
      { ldelim = ldelim.token; tts = List.map tts ~f:of_indexed; rdelim = rdelim.token }
;;

let root_to_indexed tts =
  List.fold_map tts ~init:0 ~f:(fun i tt -> to_indexed' tt i) |> snd
;;

let rec remove_trivia t =
  match t with
  | Token t -> if Token.is_trivia t then None else Some (Token t)
  | Delim { ldelim; tts; rdelim } ->
    let tts = List.concat_map tts ~f:(fun tt -> remove_trivia tt |> Option.to_list) in
    Some (Delim { ldelim; tts; rdelim })
;;

let remove_trivia_root ts =
  List.concat_map ts ~f:(fun t -> remove_trivia t |> Option.to_list)
;;

let rec to_list_ref l t =
  match t with
  | Token t -> l := t :: !l
  | Delim { ldelim; tts; rdelim } ->
    l := ldelim :: !l;
    List.iter tts ~f:(fun tt -> to_list_ref l tt);
    l := rdelim :: !l
;;

let to_list t =
  let l = ref [] in
  to_list_ref l t;
  !l
;;

let root_to_list_ref l tts = List.iter tts ~f:(fun tt -> to_list_ref l tt)

let root_to_list ts =
  let l = ref [] in
  root_to_list_ref l ts;
  !l
;;
