open Core

module Size : sig
  type t = private int [@@deriving sexp_of, equal, compare]

  val of_int : int -> t
  val inf : t

  module Syntax : sig
    val ( ++ ) : t -> t -> t
    val ( <== ) : t -> int -> bool
  end
end = struct
  type t = int [@@deriving sexp_of, equal, compare]

  module Syntax = struct
    let ( ++ ) x y =
      if x = Int.max_value || y = Int.max_value then Int.max_value else x + y
    ;;

    let ( <== ) x y = x <= y
  end

  let of_int i = i
  let inf = Int.max_value
end

open Size.Syntax

module Doc = struct
  type t =
    | Empty
    (* no newlines in char or string *)
    | Char of char
    | Blank of int
    | String of string
    | Concat of
        { doc1 : t
        ; doc2 : t
        ; flat_size : Size.t
        }
    | Newline
    | Choice of
        { flat : t
        ; expanded : t
        }
    | Indent of
        { indent : int
        ; doc : t
        ; flat_size : Size.t
        }
    | Group of
        { doc : t
        ; flat_size : Size.t
        }
  [@@deriving sexp_of]

  let rec flat_size = function
    | Empty -> Size.of_int 0
    | Newline -> Size.inf
    | Char _ -> Size.of_int 1
    | Blank n -> Size.of_int n
    | String s -> Size.of_int (String.length s)
    | Concat { flat_size; _ } | Indent { flat_size; _ } | Group { flat_size; _ } ->
      flat_size
    | Choice { flat; _ } ->
      (*
        The smart constructor for Choice ensures that Choice is never nested inside another Choice
        This ensures that flat_size runs in O(1)
      *)
      flat_size flat
  ;;

  let empty = Empty
  let newline = Newline

  let blank = function
    | 0 -> empty
    | n -> Blank n
  ;;

  let space = Blank 1
  let break0 = Choice { flat = Empty; expanded = Newline }
  let break1 = Choice { flat = Blank 1; expanded = Newline }

  let break = function
    | 0 -> break0
    | 1 -> break1
    | i -> Choice { flat = Blank i; expanded = Newline }
  ;;

  let choice ~flat ~expanded =
    match flat, expanded with
    | Empty, Empty -> empty
    | Choice { flat; _ }, expanded | flat, expanded -> Choice { flat; expanded }
  ;;

  let when_flat flat = choice ~flat ~expanded:Empty
  let when_expanded expanded = choice ~flat:Empty ~expanded

  let concat doc1 doc2 =
    match doc1, doc2 with
    | Empty, _ -> doc2
    | _, Empty -> doc1
    | _ -> Concat { doc1; doc2; flat_size = flat_size doc1 ++ flat_size doc2 }
  ;;

  let indent indent doc = Indent { indent; doc; flat_size = flat_size doc }
  let char c = if Char.equal c '\n' then Newline else Char c

  let string s =
    if String.contains s '\n'
    then begin
      let lines = String.split s ~on:'\n' in
      match lines with
      | [] -> Empty
      | line :: lines ->
        List.fold lines ~init:(String line) ~f:(fun acc line ->
          concat acc (concat Newline (String line)))
    end
    else String s
  ;;

  let raw_char c = Char c
  let raw_string s = String s

  let group doc =
    match doc with
    | Empty -> Empty
    | Group _ -> doc
    | _ -> Group { doc; flat_size = flat_size doc }
  ;;

  module Syntax = struct
    let ( ^^ ) = concat
  end
end

module type Output = sig
  type t

  val write_string : t -> string -> unit
  val write_char : t -> char -> unit
  val write_blank : t -> int -> unit
end

module String_output : Output with type t = Buffer.t = struct
  type t = Buffer.t

  let write_string = Buffer.add_string
  let write_char = Buffer.add_char

  let write_blank t n =
    for i = 1 to n do
      Buffer.add_char t ' '
    done
  ;;
end

module Make (Output : Output) = struct
  type state =
    { (*
        The pretty printer will try to ensure that each line including leading whitespace has length less than the width.
      *)
      width : int
      (*
        The pretty printer will try to ensure that each line not including leading whitespace has length less than ribbon.
      *)
    ; ribbon : int
    ; out : Output.t
    ; mutable column : int
    ; mutable last_indent : int
    }

  let invariant st flatten =
    if flatten
    then begin
      assert (st.column <= st.width && st.column - st.last_indent <= st.ribbon)
    end
  ;;

  let rec render st (flatten : bool) (indent : int) (doc : Doc.t) =
    begin match doc with
    | Empty -> ()
    | Newline ->
      st.column <- indent;
      st.last_indent <- indent;
      Output.write_char st.out '\n';
      Output.write_blank st.out indent
    | String s ->
      (* TODO: use better notion of length *)
      st.column <- st.column + String.length s;
      Output.write_string st.out s
    | Char c ->
      st.column <- st.column + 1;
      Output.write_char st.out c
    | Blank n ->
      st.column <- st.column + n;
      Output.write_blank st.out n
    | Group { doc; flat_size } -> render_group st flatten indent doc flat_size
    | Choice { flat; expanded } ->
      if flatten then render st flatten indent flat else render st flatten indent expanded
    | Indent { indent = indent'; doc; flat_size = _ } ->
      render st flatten (indent + indent') doc
    | Concat { doc1; doc2; flat_size = _ } ->
      render st flatten indent doc1;
      render st flatten indent doc2
    end;
    invariant st flatten;
    ()

  and render_group st flatten indent doc flat_size =
    (* Either we are in flatten mode.
       Otherwise, only start flatten mode if subdoc fits the size requirements.
    *)
    let flatten =
      let space_left = st.width - st.column in
      let ribbon_space_used = st.column - st.last_indent in
      assert (ribbon_space_used >= 0);
      let ribbon_space_left = st.ribbon - ribbon_space_used in
      flatten || (flat_size <== space_left && flat_size <== ribbon_space_left)
    in
    render st flatten indent doc
  ;;

  let render ?ribbon:(ribbon_frac = 1.0) ~width ~out doc =
    let st =
      { width
      ; ribbon =
          max
            0
            (min
               width
               (Float.iround_towards_zero_exn (float_of_int width *. ribbon_frac)))
      ; out
      ; column = 0
      ; last_indent = 0
      }
    in
    render_group st false 0 doc (Doc.flat_size doc)
  ;;
end

module Render_to_string = Make (String_output)

let render_to_string ?(buf_size = 128) ?ribbon ~width doc =
  let buf = Buffer.create buf_size in
  Render_to_string.render ?ribbon ~width ~out:buf doc;
  Buffer.contents buf
;;
