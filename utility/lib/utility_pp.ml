(*
  A pretty printing library based off of François Pottier's pprint library.
*)
open Core

module Basic_color = struct
  type t =
    | Black
    | Blue
    | Cyan
    | Green
    | Magenta
    | Red
    | White
    | Yellow
  [@@deriving sexp_of]
end

module Color = struct
  type t =
    | Basic of
        { color : Basic_color.t
        ; bright : bool
        }
    | Rgb of
        { r : int
        ; g : int
        ; b : int
        }
  [@@deriving sexp_of]

  let basic ?(bright = false) color = Basic { color; bright }
  let rgb ~r ~g ~b = Rgb { r; g; b }
end

module Style = struct
  type t =
    { bold : bool
    ; italic : bool
    ; underline : bool
    ; fg : Color.t option
    ; bg : Color.t option
    }
  [@@deriving sexp_of]

  let empty = { bold = false; italic = false; underline = false; fg = None; bg = None }

  let append t1 t2 =
    { bold = t1.bold || t2.bold
    ; italic = t1.italic || t2.italic
    ; underline = t1.underline || t2.underline
    ; fg = Option.first_some t2.fg t1.fg
    ; bg = Option.first_some t2.bg t1.bg
    }
  ;;

  let fg color = { empty with fg = Some color }
  let bg color = { empty with bg = Some color }
  let bold = { empty with bold = true }
  let italic = { empty with italic = true }

  module Syntax = struct
    let ( ++ ) = append
  end
end

(*
  Wadler's paper contains the fits function that calculates that size of the some flat document, as it is being rendered.
  By keeping track of this value while constructing the Doc, our algorithm becomes O(n) instead of O(n^2).
  We allow for infinite values to indicate that we want to forcibly expand our document.
*)
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
    | Append of
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
      (* Nested styles override each other rather than merge with each other. We might also want to add an option to merge. *)
    | Style of
        { style : Style.t
        ; doc : t
        ; flat_size : Size.t
        }
  [@@deriving sexp_of]

  let rec flat_size = function
    | Empty -> Size.of_int 0
    | Newline -> Size.inf
    | Char _ -> Size.of_int 1
    | Blank n -> Size.of_int n
    | String s -> Size.of_int (String.length s)
    | Append { flat_size; _ }
    | Indent { flat_size; _ }
    | Group { flat_size; _ }
    | Style { flat_size; _ } -> flat_size
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

  let style style doc =
    match doc with
    | Empty -> Empty
    | Style _ ->
      (* inner styles override outer ones *)
      doc
    | _ -> Style { style; doc; flat_size = flat_size doc }
  ;;

  let choice ~flat ~expanded =
    match flat, expanded with
    | Empty, Empty -> empty
    | Choice { flat; _ }, expanded | flat, expanded -> Choice { flat; expanded }
  ;;

  let when_flat flat = choice ~flat ~expanded:Empty
  let when_expanded expanded = choice ~flat:Empty ~expanded
  let force_expand = Choice { flat = Newline; expanded = Empty }

  let append doc1 doc2 =
    match doc1, doc2 with
    | Empty, _ -> doc2
    | _, Empty -> doc1
    | _ -> Append { doc1; doc2; flat_size = flat_size doc1 ++ flat_size doc2 }
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
          append acc (append Newline (String line)))
    end
    else String s
  ;;

  let raw_char c = Char c
  let raw_string s = String s

  let group doc =
    match doc with
    | Empty -> Empty
    | Group _ -> doc
    (* Minor optimization: Groups are useless when the document is force expanded *)
    | _ when Size.equal (flat_size doc) Size.inf -> doc
    | _ -> Group { doc; flat_size = flat_size doc }
  ;;

  let concat ?(sep = Empty) docs =
    List.foldi docs ~init:Empty ~f:(fun i acc doc ->
      if i = 0 then append acc doc else append acc (append sep doc))
  ;;

  module Syntax = struct
    let ( ^^ ) = append
  end
end

module type Output = sig
  type t

  val write_string : t -> string -> unit
  val write_char : t -> char -> unit
  val write_blank : t -> int -> unit
  val push_style : t -> Style.t -> unit
  val pop_style : t -> unit
end

module type Renderer = sig
  type output

  val render : ?ribbon:float -> width:int -> out:output -> Doc.t -> unit
end

let color_code ~base (color : Color.t) =
  match color with
  | Basic { color; bright } ->
    let offset =
      match color with
      | Black -> 0
      | Red -> 1
      | Green -> 2
      | Yellow -> 3
      | Blue -> 4
      | Magenta -> 5
      | Cyan -> 6
      | White -> 7
    in
    let base = if bright then base + 60 else base in
    [ Int.to_string (base + offset) ]
  | Rgb { r; g; b } ->
    let extended = if base = 30 then 38 else 48 in
    [ Int.to_string extended; "2"; Int.to_string r; Int.to_string g; Int.to_string b ]
;;

let ansi_reset = "\x1b[0m"

let to_ansi_escape (t : Style.t) =
  let codes =
    List.concat
      [ (if t.bold then [ "1" ] else [])
      ; (if t.italic then [ "3" ] else [])
      ; (if t.underline then [ "4" ] else [])
      ; (match t.fg with
         | Some color -> color_code ~base:30 color
         | None -> [])
      ; (match t.bg with
         | Some color -> color_code ~base:40 color
         | None -> [])
      ]
  in
  match codes with
  | [] -> ""
  | _ -> "\x1b[" ^ String.concat ~sep:";" codes ^ "m"
;;

module String_output = struct
  type t =
    { buf : Buffer.t
    ; styles : Style.t Stack.t
    ; color : bool
    }

  let create buf_size color =
    { buf = Buffer.create buf_size; styles = Stack.create (); color }
  ;;

  let write_string t = Buffer.add_string t.buf
  let write_char t = Buffer.add_char t.buf

  let write_blank t n =
    for i = 1 to n do
      Buffer.add_char t.buf ' '
    done
  ;;

  let push_style t style =
    if t.color
    then begin
      Stack.push t.styles style;
      Buffer.add_string t.buf (to_ansi_escape style)
    end
  ;;

  let pop_style t =
    if t.color
    then begin
      let _ = Stack.pop_exn t.styles in
      Buffer.add_string t.buf ansi_reset;
      Stack.top t.styles
      |> Option.iter ~f:(fun style -> Buffer.add_string t.buf (to_ansi_escape style))
    end
  ;;
end

module Channel_output = struct
  type t =
    { chan : Out_channel.t
    ; styles : Style.t Stack.t
    ; color : bool
    }

  let create color chan = { chan; styles = Stack.create (); color }
  let write_string t = Out_channel.output_string t.chan
  let write_char t = Out_channel.output_char t.chan

  let write_blank t n =
    for _ = 1 to n do
      Out_channel.output_char t.chan ' '
    done
  ;;

  let push_style t style =
    if t.color
    then begin
      Stack.push t.styles style;
      Out_channel.output_string t.chan (to_ansi_escape style)
    end
  ;;

  let pop_style t =
    if t.color
    then begin
      let _ = Stack.pop_exn t.styles in
      Out_channel.output_string t.chan ansi_reset;
      Stack.top t.styles
      |> Option.iter ~f:(fun style ->
        Out_channel.output_string t.chan (to_ansi_escape style))
    end
  ;;
end

module Make (Output : Output) = struct
  type output = Output.t

  type command =
    | Chunk of
        { flatten : bool
        ; indent : int
        ; doc : Doc.t
        }
    | Pop_style

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
    ; stack : command Stack.t
    ; mutable column : int
    ; mutable last_indent : int
    }

  let invariant st flatten =
    if flatten
    then begin
      assert (st.column <= st.width && st.column - st.last_indent <= st.ribbon)
    end
  ;;

  let render st =
    (*
      Instead of using recursion we will use loops and a stack
    *)
    while not (Stack.is_empty st.stack) do
      let command = Stack.pop_exn st.stack in
      begin match command with
      | Pop_style -> Output.pop_style st.out
      | Chunk { flatten; indent; doc } ->
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
        | Style { style; doc; flat_size = _ } ->
          Output.push_style st.out style;
          Stack.push st.stack Pop_style;
          Stack.push st.stack (Chunk { flatten; indent; doc })
        | Group { doc; flat_size } ->
          (*
           Try to render the child doc in either flat mode or expanded mode. Instead of actually rendering the 
           child doc, or calculating its size, we can just use its already calculated size `flat_size` and decide in O(1).
           This is in contrast to Wadler's paper which uses the fits function that runs in O(n).
        *)
          let flatten =
            let space_left = st.width - st.column in
            let ribbon_space_used = st.column - st.last_indent in
            assert (ribbon_space_used >= 0);
            let ribbon_space_left = st.ribbon - ribbon_space_used in
            (*
            Either we are in flatten mode.
            Otherwise, only start flatten mode if the child doc fits the size requirements.
          *)
            flatten || (flat_size <== space_left && flat_size <== ribbon_space_left)
          in
          Stack.push st.stack (Chunk { flatten; indent; doc })
        | Choice { flat; expanded } ->
          Stack.push
            st.stack
            (Chunk { flatten; indent; doc = (if flatten then flat else expanded) })
        | Indent { indent = indent'; doc; flat_size = _ } ->
          Stack.push st.stack (Chunk { flatten; indent = indent + indent'; doc })
        | Append { doc1; doc2; flat_size = _ } ->
          Stack.push st.stack (Chunk { flatten; indent; doc = doc2 });
          Stack.push st.stack (Chunk { flatten; indent; doc = doc1 })
        end;
        invariant st flatten
      end
    done;
    ()
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
      ; stack =
          Stack.singleton (Chunk { flatten = false; indent = 0; doc = Doc.group doc })
      }
    in
    render st
  ;;
end

module String_renderer = Make (String_output)
module Channel_renderer = Make (Channel_output)

let render_to_string ?(buf_size = 128) ?ribbon ?(color = false) ~width doc =
  let out = String_output.create buf_size color in
  String_renderer.render ?ribbon ~width ~out doc;
  Buffer.contents out.buf
;;

let render_to_channel ?ribbon ?(color = true) ~width ~out doc =
  let out = Channel_output.create color out in
  Channel_renderer.render ?ribbon ~width ~out doc
;;

let render_to_stdout = render_to_channel ~out:stdout
