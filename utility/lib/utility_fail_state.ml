open Core

open struct
  module Non_empty_list = Utility_non_empty_list
end

module type Config = sig
  module State : sig
    type t
  end

  module Data : sig
    type t
  end
end

module Make (Config : Config) : sig
  module State : sig
    type t =
      { mutable state : Config.State.t
      ; data : Config.Data.t
      }

    val create : Config.State.t -> Config.Data.t -> t
  end

  type t

  val state : t -> State.t
  val get : t -> Config.State.t
  val set : t -> Config.State.t -> unit
  val data : t -> Config.Data.t
  val fail : t -> 'a
  val optional : t -> (unit -> 'a) -> 'a option
  val either : t -> (unit -> 'a) -> (unit -> 'b) -> ('b, 'a) Either.t
  val many_rev : State.t -> (t -> 'b) -> 'b list
  val many : State.t -> (t -> 'b) -> 'b list
  val some_rev : t -> (unit -> 'b) -> 'b Non_empty_list.t
  val some : t -> (unit -> 'b) -> 'b Non_empty_list.t
  val guard : t -> bool -> unit
  val one_of : t -> (unit -> 'b) list -> 'b
  val run : State.t -> (t -> 'a) -> 'a option
  val run_or_thunk : State.t -> (t -> 'a) -> (unit -> 'a) -> 'a
  val orelse : t -> (unit -> 'a) -> (unit -> 'a) -> 'a

  module Syntax : sig
    val ( <|> ) : t -> (unit -> 'a) -> (unit -> 'a) -> 'a
  end
end = struct
  module State = struct
    type t =
      { mutable state : Config.State.t
      ; data : Config.Data.t
      }

    let create state data = { state; data }
  end

  type t = State.t =
    { mutable state : Config.State.t
    ; data : Config.Data.t
    }

  exception Fail

  let state t = t
  let get t = t.state
  let set t state = t.state <- state
  let data t = t.data
  let fail (_ : t) = raise_notrace Fail

  let optional t f =
    let state = t.state in
    match f () with
    | exception Fail ->
      t.state <- state;
      None
    | x -> Some x
  ;;

  let either t f g =
    let state = t.state in
    match f () with
    | exception Fail ->
      t.state <- state;
      First (g ())
    | x -> Second x
  ;;

  let rec many_rev_acc t f acc =
    let state = t.state in
    match f () with
    | exception Fail ->
      t.state <- state;
      acc
    | x -> many_rev_acc t f (x :: acc)
  ;;

  let many_rev t f = many_rev_acc t (fun () -> f t) []
  let guard t b = if not b then fail t
  let many t f = many_rev t f |> List.rev

  let some_rev t f =
    let x = f () in
    many_rev_acc t f [ x ] |> Non_empty_list.of_list_exn
  ;;

  let some t f = some_rev t f |> Non_empty_list.rev

  let rec one_of t fs =
    match fs with
    | [] -> raise_notrace Fail
    | f :: rest ->
      let state = t.state in
      begin match f () with
      | exception Fail ->
        t.state <- state;
        one_of t rest
      | x -> x
      end
  ;;

  let orelse t f g =
    let state = t.state in
    match f () with
    | x -> x
    | exception Fail ->
      t.state <- state;
      g ()
  ;;

  let run (t : State.t) f =
    let state = t.state in
    match f t with
    | exception Fail ->
      t.state <- state;
      None
    | x -> Some x
  ;;

  let run_or_thunk t f g =
    let state = t.state in
    match f t with
    | exception Fail ->
      t.state <- state;
      g ()
    | x -> x
  ;;

  module Syntax = struct
    let ( <|> ) = orelse
  end
end
