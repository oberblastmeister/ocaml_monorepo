open Prelude

open struct
  module Name_list = Oak_common.Name_list
  module Syntax = Oak_core_syntax
  module Evaluate = Oak_core_evaluate
end

module Make (Config : sig
    val show_singletons : bool
  end) =
struct
  let parens doc =
    Doc.group
      (Doc.char '('
       ^^ Doc.when_expanded Doc.space
       ^^ Doc.indent 2 doc
       ^^ Doc.break0
       ^^ Doc.char ')')
  ;;

  let bracks doc =
    Doc.group
      (Doc.char '['
       ^^ Doc.when_expanded Doc.space
       ^^ Doc.indent 2 doc
       ^^ Doc.break0
       ^^ Doc.char ']')
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

  let args docs =
    Doc.group
      (Doc.char '('
       ^^ Doc.indent 2 (Doc.break0 ^^ Doc.concat docs ~sep:(Doc.char ',' ^^ Doc.break1))
       ^^ Doc.break0
       ^^ Doc.char ')')
  ;;

  let is_spine_atom (spine : Syntax.spine) =
    match spine with
    | Empty | Snoc (_, (Proj _ | Out)) -> true
    | Snoc (_, App _) -> false
  ;;

  let rec pp_value (names : Name_list.t) (value : Syntax.value) =
    match value with
    | Value_ignore -> Doc.string "ignore"
    | Value_neutral neutral -> Doc.group (pp_neutral names neutral)
    | Value_fun abs ->
      let params, names, body = collect_fun_params names [] abs in
      Doc.group
        (Doc.string "fun"
         ^^ Doc.break1
         ^^ Doc.concat params ~sep:Doc.break1
         ^^ Doc.break1
         ^^ Doc.string "->"
         ^^ Doc.indent 2 (Doc.break1 ^^ pp_value names body))
    | Value_sing_in e ->
      if Config.show_singletons
      then
        Doc.group (Doc.string "in" ^^ Doc.indent 2 (Doc.break1 ^^ pp_value_atom names e))
      else pp_value names e
    | Value_struct strukt -> pp_struct names strukt
    | Value_encode_ty { ty; props = _ } -> pp_ty names ty

  and pp_ty (names : Name_list.t) (ty : Syntax.ty) =
    match ty with
    | Ty_universe props -> Syntax.Size.pp props.size
    | Ty_sing { identity; ty = _ } ->
      Doc.group (parens (Doc.string "=" ^^ Doc.break1 ^^ pp_value names identity))
    | Ty_struct strukt -> pp_ty_struct names strukt
    | Ty_fun ty_fun ->
      let params, names, body_ty = collect_ty_fun_params names [] ty_fun in
      Doc.group
        (Doc.concat params ~sep:(Doc.space ^^ Doc.string "->" ^^ Doc.break1)
         ^^ Doc.space
         ^^ Doc.string "->"
         ^^ Doc.break1
         ^^ pp_ty names body_ty)
    | Ty_core ty -> Syntax.Core_ty.pp ty
    | Ty_pack ty -> Doc.group (Doc.string "Pack" ^^ Doc.break1 ^^ pp_ty_atom names ty)
    | Ty_decode e -> Doc.group (pp_neutral names e)

  and pp_struct names ({ field_impls } : Syntax.value_struct) =
    let decls =
      List.map
        field_impls
        ~f:(fun ({ name; e; relevancy = _ } : Syntax.value_field_impl) ->
          Doc.group
            (Doc.string "val"
             ^^ Doc.space
             ^^ Doc.string name
             ^^ Doc.space
             ^^ Doc.string "="
             ^^ Doc.indent 2 (Doc.break1 ^^ pp_value names e)))
    in
    Doc.group (Doc.string "struct" ^^ Doc.space ^^ args decls)

  and pp_ty_struct names ({ env; field_specs } : Syntax.ty_struct) =
    let _, decls =
      List.fold_map
        field_specs
        ~init:(names, env)
        ~f:(fun (names, closure_env) field_spec ->
          let ty = Evaluate.eval_ty closure_env field_spec.ty in
          let name = field_spec.name.name in
          let doc =
            match ty with
            | Ty_sing { identity; ty } ->
              Doc.group
                (Doc.string "val"
                 ^^ Doc.space
                 ^^ Doc.string name
                 ^^ Doc.space
                 ^^ Doc.string ":"
                 ^^ Doc.indent 2 (Doc.break1 ^^ pp_ty names ty)
                 ^^ Doc.break1
                 ^^ Doc.string "="
                 ^^ Doc.indent 2 (Doc.break1 ^^ pp_value names identity))
            | _ ->
              Doc.group
                (Doc.string "val"
                 ^^ Doc.space
                 ^^ Doc.string name
                 ^^ Doc.space
                 ^^ Doc.string ":"
                 ^^ Doc.indent 2 (Doc.break1 ^^ pp_ty names ty))
          in
          let level = Name_list.next_level names in
          let names = Name_list.push name names in
          let closure_env = Syntax.Seq.push (Syntax.Value.free level) closure_env in
          (names, closure_env), doc)
    in
    Doc.group (Doc.string "sig" ^^ Doc.space ^^ block decls)

  and collect_fun_params
        names
        (docs : Doc.t list)
        ({ name; body; param_modifiers = _ } : Syntax.value_fun)
    =
    let level = Name_list.next_level names in
    let arg = Syntax.Value.free level in
    let names = Name_list.push name.name names in
    let docs = Doc.string name.name :: docs in
    let body = Evaluate.eval_closure1 body arg in
    match body with
    | Syntax.Value_fun abs -> collect_fun_params names docs abs
    | _ -> List.rev docs, names, body

  and collect_ty_fun_params
        names
        acc_params
        ({ name; param_ty; param_modifiers; _ } as ty_fun : Syntax.ty_fun)
    =
    let param_doc =
      if String.equal name.name "_"
      then pp_ty_non_arrow names param_ty
      else
        parens
          (Doc.string name.name
           ^^ Doc.space
           ^^ Doc.string ":"
           ^^ Doc.break1
           ^^ pp_ty names param_ty)
    in
    let arg = Syntax.Value.free (Name_list.next_level names) in
    let names = Name_list.push name.name names in
    let body_ty = Evaluate.Fun_ty.app ty_fun { e = arg; param_modifiers } in
    match body_ty with
    | Syntax.Ty_fun ty_fun -> collect_ty_fun_params names (param_doc :: acc_params) ty_fun
    | _ ->
      let params = List.rev (param_doc :: acc_params) in
      params, names, body_ty

  and pp_ty_non_arrow (names : Name_list.t) (ty : Syntax.ty) =
    match ty with
    | Ty_fun _ -> pp_ty_atom names ty
    | _ -> pp_ty names ty

  and pp_value_atom (names : Name_list.t) (value : Syntax.value) =
    match value with
    | Value_ignore -> pp_value names value
    | Value_neutral { head; spine } when is_spine_atom spine ->
      Doc.group (pp_neutral names { head; spine })
    | Value_encode_ty { ty; props = _ } -> pp_ty_atom names ty
    | _ -> parens (pp_value names value)

  and pp_ty_atom (names : Name_list.t) (ty : Syntax.ty) =
    match ty with
    | Ty_universe _ | Ty_core _ -> pp_ty names ty
    | Ty_decode e when is_spine_atom e.spine -> pp_ty names ty
    | _ -> parens (pp_ty names ty)

  and pp_var names var = Doc.string (Name_list.get names var)

  and pp_proj names ({ head; spine } : Syntax.neutral) field =
    let doc =
      if is_spine_atom spine
      then pp_neutral names { head; spine }
      else parens (pp_neutral names { head; spine })
    in
    doc ^^ Doc.break0 ^^ Doc.char '.' ^^ Doc.string field

  and pp_arg names ({ e; param_modifiers = _ } : Syntax.value_arg) = pp_value_atom names e

  and pp_neutral names ({ head; spine } : Syntax.neutral) =
    match spine with
    | Snoc (spine, App arg) ->
      pp_neutral names { head; spine } ^^ Doc.break1 ^^ pp_arg names arg
    | Snoc (spine, Out) ->
      if Config.show_singletons
      then pp_proj names { head; spine } "out"
      else pp_neutral names { head; spine }
    | Snoc (spine, Proj { name; index = _ }) -> pp_proj names { head; spine } name
    | Empty -> pp_head names head

  and pp_head names (head : Syntax.head) =
    match head with
    | Syntax.Free free -> pp_var names free
    | Syntax.Data _ | Syntax.Data_rec _ -> failwith ""
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

let pp_ty ?(show_singletons = false) names ty =
  let module P =
    Make (struct
      let show_singletons = show_singletons
    end)
  in
  P.pp_ty names ty
;;
