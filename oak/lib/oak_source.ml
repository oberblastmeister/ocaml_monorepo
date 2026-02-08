open Prelude

type t =
  { content : string
  ; token_offsets : int array
  ; tts : Shrubbery.Token_tree.Root.t
  ; tokens : Shrubbery.Token.t array
  ; filename : Filename.t
  }
[@@deriving sexp_of]
