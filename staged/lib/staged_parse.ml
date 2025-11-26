open struct
  module Shrub = Shrubbery.Syntax
  module Syntax = Staged_syntax
end

let parse_expr (s : Shrub.group) =
  match s with
  | _ -> ()
;;
