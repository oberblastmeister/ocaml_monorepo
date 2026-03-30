open Prelude
module Syntax = Oak_core_syntax
module Evaluate = Oak_core_evaluate
module Cow_slice = Utility.Cow_slice

module Term_ty_struct = struct
  let of_iterated_binders_with
        context_size
        (field_specs : Syntax.term_field_spec Cow_slice.t)
    : Syntax.term_ty_struct
    =
    let ~field_specs, .. =
      let free_level = Syntax.Level.of_int context_size in
      let free_value = Syntax.Value.free free_level in
      let context_size = context_size + 1 in
      Cow_slice.foldi
        field_specs
        ~init:
          ( ~running_env:Syntax.Env.empty
          , ~field_specs:(Cow_slice.create (Cow_slice.length field_specs)) )
        ~f:(fun index (~running_env, ~field_specs) { name; ty; relevancy } ->
          let field : Syntax.field_loc = { name = name.name; index } in
          let field_spec : Syntax.term_field_spec =
            { name
            ; ty =
                Evaluate.Ty.eval running_env ty
                |> Evaluate.Ty.quote_with context_size
                |> Evaluate.Term_ty.close_single free_level
            ; relevancy
            }
          in
          ( ~running_env:(Syntax.Env.push
                            (Evaluate.Value.proj free_value field)
                            running_env)
          , ~field_specs:(Cow_slice.push_full_slice_exn field_specs field_spec) ))
    in
    { field_specs }
  ;;

  let of_iterated_binders = of_iterated_binders_with Evaluate.temporary_context_size
end

module Term_data_rec = struct
  (* Make sure each declaration binds length of field_locs number of binders *)
  let of_multiple_binders_with
        context_size
        (decls : Syntax.term_data_decl list)
        (self_ty : Syntax.term_ty_struct)
    =
    let free_level = Syntax.Level.of_int context_size in
    let free_value = Syntax.Value.free free_level in
    let context_size = context_size + 1 in
    let env =
      Cow_slice.foldi
        self_ty.field_specs
        ~init:Syntax.Env.empty
        ~f:(fun index env field_spec ->
          let field_loc = Syntax.Field_loc.create field_spec.name.name index in
          Syntax.Env.push (Evaluate.Value.proj free_value field_loc) env)
    in
    (* let term_data_rec : Syntax.term_data_rec = { decls; ty = Term_ty_struct self_ty } in *)
    let decls = List.map decls ~f:(fun decl -> Evaluate.Term_data_decl.eval env decl) in
    failwith ""
  ;;
end

module Term_ty_fun = struct
  (* each parameter can have bound variables that refer the all of the previous parameters *)
  let of_telescope
        (params : Syntax.term_param Non_empty_list.t)
        (body_ty : Syntax.term_ty)
    : Syntax.term_ty_fun
    =
    let params, param = Non_empty_list.drop_last params, Non_empty_list.last params in
    List.fold
      params
      ~init:(Syntax.Term_ty_fun.create param body_ty)
      ~f:(fun body_ty param -> Syntax.Term_ty_fun.create param (Term_ty_fun body_ty))
  ;;
end

module Term_ty = struct
  (* if params is empty, the resulting ty is body_ty *)
  let ty_fun_of_telescope (params : Syntax.term_param list) (body_ty : Syntax.term_ty) =
    match Non_empty_list.of_list params with
    | None -> body_ty
    | Some params -> Term_ty_fun (Term_ty_fun.of_telescope params body_ty)
  ;;
end
