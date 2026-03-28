open Prelude
module Core := Oak_core

val pp_value : ?show_singletons:bool -> Core.Name_env.t -> Core.value -> Doc.t
val pp_ty : ?show_singletons:bool -> Core.Name_env.t -> Core.ty -> Doc.t
