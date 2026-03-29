open Prelude
module Syntax = Oak_core_syntax
module Evaluate = Oak_core_evaluate

module Term_ty_struct = struct
  let of_iterated_binders_with context_size (field_specs : Syntax.term_field_spec list)
    : Syntax.term_ty_struct
    =
    let _, field_specs =
      let free_level = Syntax.Level.of_int context_size in
      let free_value = Syntax.Value.free free_level in
      let context_size = context_size + 1 in
      List.fold_mapi
        field_specs
        ~init:Syntax.Env.empty
        ~f:(fun index running_env { name; ty; relevancy } ->
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
          Syntax.Env.push (Evaluate.Value.proj free_value field) running_env, field_spec)
    in
    { field_specs }
  ;;

  let of_iterated_binders = of_iterated_binders_with Evaluate.temporary_context_size
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
