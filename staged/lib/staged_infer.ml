open Prelude

open struct
  module Var = Staged_var
  module Syntax = Staged_syntax
  module Stage = Syntax.Stage
end

exception Error of Sexp.t

let throw_s s = raise (Error s)

type env = { context : Syntax.ty Var.Map.t } [@@deriving sexp_of]

let add_var env var ty = { context = Map.set env.context ~key:var ~data:ty }

let get_var_ty env var =
  match Map.find env.context var with
  | None -> throw_s [%message "Var not found" (var : Var.t)]
  | Some ty -> ty
;;

let rec infer env (expr : Syntax.expr) =
  match expr with
  | Expr_fun { param_var; param_ty; stage; body; ann = _ } ->
    let env' = add_var env param_var param_ty in
    let body = infer env' body in
    let body_ty = Syntax.expr_ty_exn body in
    let body_stage = Syntax.ty_stage body_ty in
    if Stage.compare body_stage stage > 0
    then throw_s [%message "stage is too high" (body_stage : Stage.t) (stage : Stage.t)];
    let ty = Syntax.Ty_fun { param = param_ty; stage; ret = body_ty } in
    Expr_fun { param_var; param_ty; stage; body; ann = Some ty }
  | Expr_app { fn; arg; ann = _ } ->
    let fn = infer env fn in
    let fn_ty = Syntax.expr_ty_exn fn in
    let fn_ty =
      match fn_ty with
      | Ty_fun t -> t
      | _ -> throw_s [%message "expected function type for app" (fn_ty : Syntax.ty)]
    in
    let arg = check env arg fn_ty.param in
    let ty = fn_ty.ret in
    Expr_app { fn; arg; ann = Some ty }
  | Expr_int i -> Expr_int i
  | Expr_bin { lhs; op; rhs } ->
    let lhs = check env lhs Ty_int in
    let rhs = check env rhs Ty_int in
    Expr_bin { lhs; op; rhs }
  | Expr_let { binding; body; ann = _ } ->
    let binding = { binding with expr = infer env binding.expr } in
    let env' = add_var env binding.var (Syntax.expr_ty_exn binding.expr) in
    let body = infer env' body in
    let ty = Syntax.expr_ty_exn body in
    Expr_let { binding; body; ann = Some ty }
  | Expr_let_rec _ -> failwith ""
  | Expr_var { var; ann = _ } ->
    let ty = get_var_ty env var in
    Expr_var { var; ann = Some ty }

and check env (expr : Syntax.expr) (ty : Syntax.ty) : Syntax.expr =
  let expr = infer env expr in
  let ty' = Syntax.expr_ty_exn expr in
  check_ty_eq env ty ty';
  expr

and check_ty_eq env (ty : Syntax.ty) (ty' : Syntax.ty) =
  match ty, ty' with
  | Ty_fun t1, Ty_fun t2 ->
    check_ty_eq env t1.param t2.param;
    if not (Syntax.Stage.equal t1.stage t2.stage)
    then
      throw_s
        [%message
          "unequal stages" (t1.stage : Syntax.Stage.t) (t2.stage : Syntax.Stage.t)];
    check_ty_eq env t1.ret t2.ret
  | Ty_int, Ty_int -> ()
  | _ -> throw_s [%message "types are not equal" (ty : Syntax.ty) (ty' : Syntax.ty)]
;;

let infer' expr =
  let env = { context = Var.Map.empty } in
  let expr = infer env expr in
  let ty = Syntax.expr_ty_exn expr in
  if not (Stage.equal (Syntax.ty_stage ty) Stage.Runtime)
  then throw_s [%message "Root expression should be at stage runtime"];
  expr
;;

let infer expr =
  match infer' expr with
  | exception Error s -> Result.Error (Error.t_of_sexp s)
  | res -> Result.Ok res
;;
