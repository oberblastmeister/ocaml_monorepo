open Prelude
module Core = Oak_core
module Common = Oak_common
module Cow_slice = Utility.Cow_slice

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

  let is_spine_atom (spine : Core.spine) =
    match spine with
    | Empty | Snoc (_, (Proj _ | Out)) -> true
    | Snoc (_, App _) -> false
  ;;

  let rec pp_value (names : Core.name_env) (value : Core.value) =
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

  and pp_ty (names : Core.name_env) (ty : Core.ty) =
    match ty with
    | Ty_universe props -> Common.Size.pp props.size
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
    | Ty_core ty -> Common.Core_ty.pp ty
    | Ty_pack ty -> Doc.group (Doc.string "Pack" ^^ Doc.break1 ^^ pp_ty_atom names ty)
    | Ty_decode e -> Doc.group (pp_neutral names e)

  and pp_struct names ({ field_impls } : Core.value_struct) =
    let decls =
      Cow_slice.map field_impls ~f:(fun ({ name; e } : Core.value_field_impl) ->
        Doc.group
          (Doc.string "val"
           ^^ Doc.space
           ^^ Doc.string name
           ^^ Doc.space
           ^^ Doc.string "="
           ^^ Doc.indent 2 (Doc.break1 ^^ pp_value names e)))
      |> Cow_slice.to_list
    in
    Doc.group (Doc.string "struct" ^^ Doc.space ^^ args decls)

  and pp_ty_struct names (ty : Core.ty_struct) =
    let field_spec_views = Core.Ty_struct.field_spec_views ty in
    let (~names:_, ..), decls =
      Cow_slice.fold_mapi
        field_spec_views
        ~init:
          ( ~names
          , ~running_field_impls:(Cow_slice.create (Cow_slice.length field_spec_views)) )
        ~f:(fun index (~names, ~running_field_impls) field_spec_view ->
          let field = Core.Field_loc.create field_spec_view.name.name index in
          let running_struct_value = Core.Value.create_struct running_field_impls in
          let field_spec = Core.Ty_struct.proj running_struct_value ty field in
          let ty = field_spec.ty in
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
          let level = Core.Level.of_int (Core.Name_env.length names) in
          let names = Core.Name_env.push field_spec.name names in
          let running_field_impls =
            Cow_slice.push_full_slice_exn
              running_field_impls
              (Core.Value_field_impl.create field_spec_view.name.name (Core.Value.free level))
          in
          (~names, ~running_field_impls), doc)
    in
    Doc.group (Doc.string "sig" ^^ Doc.space ^^ block (Cow_slice.to_list decls))

  and collect_fun_params
        names
        (docs : Doc.t list)
        ({ name; body = _; icit = _ } as value : Core.value_fun)
    =
    let level = Core.Level.of_int (Core.Name_env.length names) in
    let arg = Core.Value.free level in
    let names = Core.Name_env.push name names in
    let docs = Doc.string name.name :: docs in
    let body = Core.Fun.app value arg in
    match body with
    | Value_fun abs -> collect_fun_params names docs abs
    | _ -> List.rev docs, names, body

  and collect_ty_fun_params
        names
        acc_params
        ({ param = { name; ty = param_ty; modifiers }; _ } as ty_fun : Core.ty_fun)
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
    let arg = Core.Value.free (Core.Level.of_int (Core.Name_env.length names)) in
    let names = Core.Name_env.push name names in
    let body_ty =
      Core.Ty_fun.app ty_fun ({ e = arg; icit = modifiers.icit } : Core.value_arg)
    in
    match body_ty with
    | Ty_fun ty_fun -> collect_ty_fun_params names (param_doc :: acc_params) ty_fun
    | _ ->
      let params = List.rev (param_doc :: acc_params) in
      params, names, body_ty

  and pp_ty_non_arrow (names : Core.name_env) (ty : Core.ty) =
    match ty with
    | Ty_fun _ -> pp_ty_atom names ty
    | _ -> pp_ty names ty

  and pp_value_atom (names : Core.name_env) (value : Core.value) =
    match value with
    | Value_ignore -> pp_value names value
    | Value_neutral { head; spine } when is_spine_atom spine ->
      Doc.group (pp_neutral names { head; spine })
    | Value_encode_ty { ty; props = _ } -> pp_ty_atom names ty
    | _ -> parens (pp_value names value)

  and pp_ty_atom (names : Core.name_env) (ty : Core.ty) =
    match ty with
    | Ty_universe _ | Ty_core _ -> pp_ty names ty
    | Ty_decode e when is_spine_atom e.spine -> pp_ty names ty
    | _ -> parens (pp_ty names ty)

  and pp_var names var = Doc.string (Core.Name_env.get_level_exn names var).name

  and pp_proj names ({ head; spine } : Core.neutral) field =
    let doc =
      if is_spine_atom spine
      then pp_neutral names { head; spine }
      else parens (pp_neutral names { head; spine })
    in
    doc ^^ Doc.break0 ^^ Doc.char '.' ^^ Doc.string field

  and pp_arg names ({ e; icit = _ } : Core.value_arg) = pp_value_atom names e

  and pp_neutral names ({ head; spine } : Core.neutral) =
    match spine with
    | Snoc (spine, App arg) ->
      pp_neutral names { head; spine } ^^ Doc.break1 ^^ pp_arg names arg
    | Snoc (spine, Out) ->
      if Config.show_singletons
      then pp_proj names { head; spine } "out"
      else pp_neutral names { head; spine }
    | Snoc (spine, Proj { name; index = _ }) -> pp_proj names { head; spine } name
    | Empty -> pp_head names head

  and pp_head names (head : Core.head) =
    match head with
    | Free free -> pp_var names free
    | Data _ | Data_rec _ -> failwith ""
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
