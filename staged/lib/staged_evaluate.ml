open Prelude

open struct
  module Syntax = Staged_syntax
  module Stage = Syntax.Stage
  module Var = Staged_var
  module Acc = Utility.Acc
end

type st = { mutable next_id : int }

let fresh_var st name =
  let id = st.next_id in
  st.next_id <- st.next_id + 1;
  Var.create name id
;;

type env = { context : value Var.Map.t }

and closure =
  { env : env
  ; fn : Syntax.expr_fun
  }

and value =
  | Quote of Syntax.expr
  | Closure of closure

and let_binding =
  { var : Var.t
  ; expr : Syntax.expr
  }
[@@deriving sexp_of]

let env_variables env = Map.keys env.context

let value_code_exn = function
  | Quote e -> e
  | value -> raise_s [%message "value was not code" (value : value)]
;;

let value_closure_exn = function
  | Closure c -> c
  | value -> raise_s [%message "value was not closure" (value : value)]
;;

let add_var env var ty = { context = Map.set env.context ~key:var ~data:ty }
let get_var env var = Map.find_exn env.context var

let rec add_bindings bindings body =
  let body_ty = Syntax.get_ty_exn body in
  Acc.to_list_rev bindings
  |> List.fold ~init:body ~f:(fun expr binding ->
    Syntax.Expr_let
      { var = binding.var; expr = binding.expr; body = expr; ann = Some body_ty })
;;

let rec evaluate st env (expr : Syntax.expr) : let_binding Acc.t * value =
  match expr with
  | Expr_fun fn -> begin
    match fn.stage with
    | Runtime ->
      let env' =
        add_var
          env
          fn.param_var
          (Quote (Expr_var { var = fn.param_var; ann = Some fn.param_ty }))
      in
      let bindings, body_value = evaluate st env' fn.body in
      let body_expr = value_code_exn body_value |> add_bindings bindings in
      Acc.empty, Quote (Expr_fun { fn with body = body_expr })
    | Comptime -> Acc.empty, Closure { env; fn }
  end
  | Expr_app { fn; arg; ann } ->
    let fn_ty = Syntax.get_ty_exn fn |> Syntax.ty_fun_exn in
    let binds, fn_value = evaluate st env fn in
    let binds', arg_value = evaluate st env arg in
    begin match fn_ty.stage with
    | Runtime ->
      let expr =
        Syntax.Expr_app
          { fn = value_code_exn fn_value; arg = value_code_exn arg_value; ann }
      in
      Acc.(binds @ binds'), Quote expr
    | Comptime ->
      let fn_value = value_closure_exn fn_value in
      let env = fn_value.env in
      let binds'', env' =
        match Syntax.ty_stage fn_value.fn.param_ty with
        | Runtime ->
          let var = fresh_var st fn_value.fn.param_var.name in
          let env =
            add_var
              env
              fn_value.fn.param_var
              (Quote (Expr_var { var; ann = Some fn_value.fn.param_ty }))
          in
          Acc.singleton { var; expr = value_code_exn arg_value }, env
        | Comptime ->
          let env = add_var env fn_value.fn.param_var arg_value in
          Acc.empty, env
      in
      let binds''', res = evaluate st env' fn_value.fn.body in
      Acc.(binds @ binds' @ binds'' @ binds'''), res
    end
  | Expr_let ({ expr; body; _ } as expr_let) ->
    let binds, expr_value = evaluate st env expr in
    let expr_ty = Syntax.get_ty_exn expr in
    begin match Syntax.ty_stage expr_ty with
    | Runtime ->
      let var = fresh_var st expr_let.var.name in
      let env' =
        add_var env expr_let.var (Quote (Expr_var { var; ann = Some expr_ty }))
      in
      let binds', body_value = evaluate st env' body in
      ( Acc.(binds @ singleton { var; expr = value_code_exn expr_value } @ binds')
      , body_value )
    | Comptime ->
      let env = add_var env expr_let.var expr_value in
      let binds', body_value = evaluate st env body in
      Acc.(binds @ binds'), body_value
    end
  | Expr_int _ -> Acc.empty, Quote expr
  | Expr_bin { lhs; op; rhs } ->
    let binds, lhs = evaluate st env lhs in
    let binds', rhs = evaluate st env rhs in
    let res =
      Quote (Expr_bin { lhs = value_code_exn lhs; op; rhs = value_code_exn rhs })
    in
    Acc.(binds @ binds'), res
  | Expr_var { var; ann = _ } -> Acc.empty, get_var env var
;;

let evaluate expr =
  let st = { next_id = 0 } in
  let bindings, value = evaluate st { context = Var.Map.empty } expr in
  add_bindings bindings (value_code_exn value)
;;
