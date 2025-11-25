module Fold = Functional_fold
module Iter = Functional_iter
module Traverse = Functional_traverse

module Syntax = struct
  let[@inline] ( let@ ) f x = f x
  let[@inline] ( let@: ) f x = f ~f:x
  let ( @> ) = Fold.( @> )
  let ( & ) = Functional_traverse.( & )
end
