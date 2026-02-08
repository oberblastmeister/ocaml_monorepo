module Source := Oak_source

val rename : Source.t -> Oak_surface.expr -> Oak_diagnostic.t list * Oak_syntax.expr
