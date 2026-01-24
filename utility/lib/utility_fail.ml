open Core

type t = Env

let env = Env

exception Fail

let fail (_env : t) = raise_notrace Fail

let unwrap env o =
  match o with
  | None -> fail env
  | Some x -> x
;;

let optional env f =
  match f env with
  | exception Fail -> None
  | x -> Some x
;;

let either env f g =
  match f env with
  | exception Fail -> First (g env)
  | x -> Second x
;;

let many_rev env f =
  let rec loop acc =
    match f env with
    | exception Fail -> acc
    | x -> loop (x :: acc)
  in
  loop []
;;

let guard env b = if not b then fail env

let many env f = many_rev env f |> List.rev

let some_rev env f =
  let x = f env in
  let xs = many_rev env f in
  x :: xs
;;

let some env f = some_rev env f |> List.rev

module List = struct
  type 'a t = 'a list ref

  let next env t =
    match !t with
    | [] -> fail env
    | x :: xs ->
      t := xs;
      x
  ;;

  let take t =
    let r = !t in
    t := [];
    r
  ;;

  let empty env t =
    match !t with
    | [] -> ()
    | _ :: _ -> fail env
  ;;

  let create env xs ~f =
    let r = ref xs in
    f env r;
    empty env r
  ;;

  let backtrack env items f =
    let saved = !items in
    match f env with
    | exception Fail ->
      items := saved;
      fail env
    | x -> x
  ;;
end

let rec one_of env fs =
  match fs with
  | [] -> fail env
  | f :: rest ->
    (match f env with
     | exception Fail -> one_of env rest
     | x -> x)
;;



let run f =
  match f Env with
  | exception Fail -> None
  | x -> Some x
;;

module Syntax = struct
  let ( <|> ) env f g =
    match f env with
    | exception Fail -> g env
    | x -> x
  ;;
end
