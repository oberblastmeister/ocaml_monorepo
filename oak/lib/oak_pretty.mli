open Prelude
module Name_list := Oak_common.Name_list
module Syntax := Oak_syntax

val pp_value : ?show_singletons:bool -> Name_list.t -> Syntax.value -> Doc.t
