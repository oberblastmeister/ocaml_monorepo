include Core
module Non_empty_list = Utility.Non_empty_list
module Pp = Utility.Pp
module Doc = Pp.Doc
include Doc.Syntax
module Bwd = Utility.Bwd

let ( <: ) = Bwd.Infix.( <: )
