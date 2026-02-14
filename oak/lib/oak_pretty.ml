open Prelude

open struct
  module Name_list = Oak_common.Name_list
  module Syntax = Oak_syntax
  module Evaluate = Oak_evaluate
  module Common = Oak_common
  module Universe = Common.Universe
end

module Make (Config : sig
    val show_singletons : bool
  end) =
struct
  let next_var_of_size size = Syntax.Value.var (Common.Level.of_int size)

  let parens doc =
    Doc.group
      (Doc.char '('
       ^^ Doc.when_expanded Doc.space
       ^^ Doc.indent 2 doc
       ^^ Doc.break0
       ^^ Doc.char ')')
  ;;

  let block docs =
    Doc.group
      (Doc.char '{'
       ^^ Doc.indent
            2
            (Doc.choice ~flat:Doc.space ~expanded:Doc.newline
             ^^ Doc.concat
                  docs
                  ~sep:(Doc.choice ~flat:(Doc.string "; ") ~expanded:Doc.newline))
       ^^ Doc.choice ~flat:Doc.space ~expanded:Doc.newline
       ^^ Doc.char '}')
  ;;

  let is_spine_atom (spine : Syntax.spine) =
    match spine with
    | Empty | Snoc (_, (Elim_proj _ | Elim_out _)) -> true
    | Snoc (_, Elim_app _) -> false
  ;;

  let rec pp_value (names : Name_list.t) (value : Syntax.value) =
    match value with
    | Value_ignore -> Doc.string "ignore"
    | Value_neutral neutral -> Doc.group (pp_neutral names neutral)
    | Value_core_ty ty -> Common.Core_ty.pp ty
    | Value_universe u -> Common.Universe.pp u
    | Value_abs abs ->
      let params, names, body = collect_abs_params names [ abs.var.name ] abs in
      Doc.group
        (Doc.string "fun"
         ^^ Doc.break1
         ^^ Doc.concat params ~sep:Doc.break1
         ^^ Doc.break1
         ^^ Doc.string "->"
         ^^ Doc.indent 2 (Doc.break1 ^^ pp_value names body))
    | Syntax.Value_ty_fun { var; param_ty; body_ty } ->
      let params, names', body_ty =
        collect_ty_fun_params names [] { var; param_ty; body_ty }
      in
      Doc.group
        (Doc.concat params ~sep:(Doc.space ^^ Doc.string "->" ^^ Doc.break1)
         ^^ Doc.space
         ^^ Doc.string "->"
         ^^ Doc.break1
         ^^ pp_value names' body_ty)
    | Value_ty_sing { identity; ty = _ } ->
      Doc.group (parens (Doc.string "=" ^^ Doc.break1 ^^ pp_value names identity))
    | Value_sing_in e ->
      if Config.show_singletons
      then Doc.group (Doc.string "in" ^^ Doc.indent 2 (Doc.break1 ^^ pp_atom names e))
      else pp_atom names e
    | Value_mod { fields } ->
      let decls =
        List.map fields ~f:(fun ({ name; e } : Syntax.value_field) ->
          Doc.group
            (Doc.string "let"
             ^^ Doc.space
             ^^ Doc.string name
             ^^ Doc.space
             ^^ Doc.string "="
             ^^ Doc.indent 2 (Doc.break1 ^^ pp_value names e)))
      in
      Doc.group (Doc.string "mod" ^^ Doc.space ^^ block decls)
    | Value_ty_mod ty_mod -> pp_ty_mod names ty_mod
    | Value_ty_pack ty -> Doc.group (Doc.string "Pack" ^^ Doc.break1 ^^ pp_atom names ty)

  and pp_ty_mod names (ty_mod : Syntax.value_ty_mod_closure) =
    let _, decls =
      List.fold_map
        ty_mod.ty_decls
        ~init:(names, ty_mod.env)
        ~f:(fun (names, closure_env) decl ->
          let ty = Evaluate.eval closure_env decl.ty in
          let name = decl.var.name in
          let doc =
            Doc.group
              (Doc.string "let"
               ^^ Doc.space
               ^^ Doc.string name
               ^^ Doc.space
               ^^ Doc.string ":"
               ^^ Doc.indent 2 (Doc.break1 ^^ pp_value names ty))
          in
          ( ( Name_list.push name names
            , Syntax.Env.push (next_var_of_size (Name_list.size names)) closure_env )
          , doc ))
    in
    Doc.group (Doc.string "sig" ^^ Doc.space ^^ block decls)

  and collect_abs_params names acc_names ({ var; body } : Syntax.value_abs) =
    let arg = next_var_of_size (Name_list.size names) in
    let names = Name_list.push var.name names in
    let body = Evaluate.eval_closure1 body arg in
    match body with
    | Syntax.Value_abs abs -> collect_abs_params names (var.name :: acc_names) abs
    | _ ->
      let params = List.rev_map acc_names ~f:Doc.string in
      params, names, body

  and collect_ty_fun_params names acc_params (tf : Syntax.value_ty_fun) =
    let param_doc =
      if String.equal tf.var.name "_"
      then pp_atom names tf.param_ty
      else
        parens
          (Doc.string tf.var.name
           ^^ Doc.space
           ^^ Doc.string ":"
           ^^ Doc.break1
           ^^ pp_value names tf.param_ty)
    in
    let arg = next_var_of_size (Name_list.size names) in
    let names' = Name_list.push tf.var.name names in
    let body_ty = Evaluate.eval_closure1 tf.body_ty arg in
    match body_ty with
    | Syntax.Value_ty_fun tf' ->
      collect_ty_fun_params names' (param_doc :: acc_params) tf'
    | _ ->
      let params = List.rev (param_doc :: acc_params) in
      params, names', body_ty

  and pp_atom (names : Name_list.t) (value : Syntax.value) =
    match value with
    | Value_ignore -> pp_value names value
    | Value_neutral { head; spine } when is_spine_atom spine ->
      Doc.group (pp_neutral names { head; spine })
    | Value_core_ty _ | Value_universe _ | Value_ty_sing _ -> pp_value names value
    | _ -> parens (pp_value names value)

  and pp_var names var = Doc.string (Name_list.get names var)

  and pp_proj names ({ head; spine } : Syntax.neutral) field =
    let doc =
      if is_spine_atom spine
      then pp_neutral names { head; spine }
      else parens (pp_neutral names { head; spine })
    in
    doc ^^ Doc.break0 ^^ Doc.char '.' ^^ Doc.string field

  and pp_neutral names ({ head; spine } : Syntax.neutral) =
    match spine with
    | Snoc (spine, Elim_app arg) ->
      pp_neutral names { head; spine } ^^ Doc.break1 ^^ pp_atom names arg
    | Snoc (spine, Elim_out { identity = _ }) ->
      if Config.show_singletons
      then pp_proj names { head; spine } "out"
      else pp_neutral names { head; spine }
    | Snoc (spine, Elim_proj { field; field_index = _ }) ->
      pp_proj names { head; spine } field
    | Empty -> pp_var names head

  and pp_elim names (elim : Syntax.elim) =
    match elim with
    | Elim_app arg -> Doc.break1 ^^ pp_atom names arg
    | Elim_proj { field; field_index = _ } ->
      Doc.break0 ^^ Doc.char '.' ^^ Doc.string field
    | Elim_out { identity = _ } ->
      if Config.show_singletons
      then Doc.break0 ^^ Doc.char '.' ^^ Doc.string "out"
      else Doc.empty
  ;;
end

let pp_value ?(show_singletons = false) names value =
  let module P =
    Make (struct
      let show_singletons = show_singletons
    end)
  in
  P.pp_value names value
;;
