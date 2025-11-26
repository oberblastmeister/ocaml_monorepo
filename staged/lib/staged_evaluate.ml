open Prelude

open struct
  module Syntax = Staged_syntax
  module Stage = Syntax.Stage
end

type env = { context : value String.Map.t }

and closure =
  { env : env
  ; fn : Syntax.expr_fun
  }

and value =
  | Code of Syntax.expr
  | Closure of closure

and let_binding =
  { var : string
  ; expr : Syntax.expr
  }
[@@deriving sexp_of]

let value_code_exn = function
  | Code e -> e
  | value -> raise_s [%message "value was not code" (value : value)]
;;

let value_closure_exn = function
  | Closure c -> c
  | value -> raise_s [%message "value was not closure" (value : value)]
;;

let add_var env var ty = { context = Map.set env.context ~key:var ~data:ty }
let get_var env var = Map.find_exn env.context var

let rec add_bindings bindings body =
  match bindings with
  | { var; expr } :: bs ->
    let body = add_bindings bs body in
    Syntax.Expr_let { var; expr; body; ann = Some (Syntax.get_ty_exn body) }
  | [] -> body
;;

let rec evaluate env (expr : Syntax.expr) : let_binding list * value =
  match expr with
  | Expr_fun fn -> begin
    match fn.stage with
    | Runtime ->
      ( []
      , let env' =
          List.fold_left fn.params ~init:env ~f:(fun env param ->
            add_var
              env
              param.var
              (Code (Expr_var { var = param.var; ann = Some param.ty })))
        in
        let bindings, body_value = evaluate env' fn.body in
        let body_expr = value_code_exn body_value |> add_bindings bindings in
        Code (Expr_fun { fn with body = body_expr }) )
    | Comptime -> [], Closure { env; fn }
  end
  | Expr_app { fn; args; ann } ->
    let fn_ty = Syntax.get_ty_exn fn |> Syntax.ty_fun_exn in
    let binds, fn_value = evaluate env fn in
    let binds', arg_values = evaluate_many env args in
    begin match fn_ty.stage with
    | Runtime ->
      let expr =
        Syntax.Expr_app
          { fn = value_code_exn fn_value
          ; args = List.map ~f:value_code_exn arg_values
          ; ann
          }
      in
      binds @ binds', Code expr
    | Comptime ->
      let fn_value = value_closure_exn fn_value in
      let binds'', env' =
        List.fold_left
          (List.zip_exn arg_values fn_value.fn.params)
          ~init:([], env)
          ~f:(fun (binds, env) (arg_value, param) ->
            match Syntax.ty_stage param.ty with
            | Runtime ->
              let binds = { var = param.var; expr = value_code_exn arg_value } :: binds in
              let env =
                add_var
                  env
                  param.var
                  (Code (Expr_var { var = param.var; ann = Some param.ty }))
              in
              binds, env
            | Comptime ->
              let env = add_var env param.var arg_value in
              binds, env)
      in
      let binds''', res = evaluate env' fn_value.fn.body in
      binds @ binds' @ binds'' @ binds''', res
    end
  | Expr_let { var; expr; body; _ } ->
    let binds, expr_value = evaluate env expr in
    let expr_ty = Syntax.get_ty_exn expr in
    begin match Syntax.ty_stage expr_ty with
    | Runtime ->
      let env = add_var env var (Code (Expr_var { var; ann = Some expr_ty })) in
      let binds', body_value = evaluate env body in
      let binds'' = binds @ [ { var; expr = value_code_exn expr_value } ] @ binds' in
      binds'', body_value
    | Comptime ->
      let env = add_var env var expr_value in
      let binds', body_value = evaluate env body in
      let binds'' = binds @ binds' in
      binds'', body_value
    end
  | Expr_int _ -> [], Code expr
  | Expr_bin { lhs; op; rhs } ->
    let binds, lhs = evaluate env lhs in
    let binds', rhs = evaluate env rhs in
    let res =
      Code (Expr_bin { lhs = value_code_exn lhs; op; rhs = value_code_exn rhs })
    in
    binds @ binds', res
  | Expr_var { var; ann = _ } -> [], get_var env var

and evaluate_many env exprs =
  let rec go exprs =
    match exprs with
    | [] -> [], []
    | e :: es ->
      let binds, v = evaluate env e in
      let binds', vs = go es in
      binds @ binds', v :: vs
  in
  go exprs
;;
