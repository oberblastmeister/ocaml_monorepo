open Prelude
open Oak_syntax
open Oak_evaluate

open struct
  module Spanned = Utility.Spanned
  module Common = Oak_common
  module Name_list = Common.Name_list
  module Diagnostic = Oak_diagnostic
  module Pretty = Oak_pretty
  module Context = Oak_context
  module Universe = Common.Universe
end

(* precondition: the neutral must be well typed, but it *doesn't* have to be an element of some universe. *)
let rec infer_neutral (ty_env : Env.t) (e : neutral) : value =
  Bwd.fold_left
    e.spine
    ~init:(Bwd.Empty, Env.find_exn ty_env (Index.of_level (Env.size ty_env) e.head))
    ~f:(fun (spine, ty) elim ->
      let ty =
        match elim with
        | Elim_app { arg; icit = _ } ->
          let func_ty = unfold ty |> Uvalue.ty_fun_val_exn in
          eval_closure1 func_ty.body_ty arg
        | Elim_proj { field = _; field_index } ->
          let mod_ty = unfold ty |> Uvalue.ty_mod_val_exn in
          eval_ty_mod_closure (Value_neutral { head = e.head; spine }) mod_ty field_index
        | Elim_out { identity = _ } ->
          let e_ty = unfold ty |> Uvalue.ty_sing_val_exn in
          e_ty.ty
      in
      spine <: elim, ty)
  |> snd
;;

(* precondition: value must be well typed and must be in some universe *)
let rec infer_value_universe (ty_env : Env.t) (e : value) : Universe.t =
  let panic () = raise_s [%message "value was not in a universe" (e : value)] in
  match e with
  | Value_mod _ | Value_abs _ | Value_ignore | Value_sing_in _ -> panic ()
  (* meta variables can only range over values of kind Type for now *)
  | Value_ty_meta _ | Value_core_ty _ | Value_ty_pack _ ->
    Universe.type_
    (*
      We infer kind here because we don't want subtyping issues at kind Type,
      same reason ty_mod is inferred to be kind Kind
    *)
  | Value_ty_sing _ -> Universe.kind_
  | Value_neutral neutral ->
    infer_neutral ty_env neutral |> unfold |> Uvalue.universe_val_exn
  | Value_universe u -> Universe.incr u
  | Value_ty_fun { var = _; param_ty; icit = _; body_ty } ->
    let universe1 = infer_value_universe ty_env param_ty in
    let universe2 =
      infer_value_universe
        (Env.push param_ty ty_env)
        (eval_closure1 body_ty (next_var_of_env ty_env))
    in
    Universe.max universe1 universe2
  | Value_ty_mod { env = closure_env; ty_decls } ->
    (* Modules have at least universe Kind because they include subtyping *)
    let _, _, universe =
      List.fold
        ty_decls
        ~init:(closure_env, ty_env, Universe.kind_)
        ~f:(fun (closure_env, ty_env, universe) ty_decl ->
          let ty = eval closure_env ty_decl.ty in
          let universe' = infer_value_universe ty_env ty in
          ( Env.push
              (* Important: make sure to use the size of ty_env instead of env *)
              (next_var_of_env ty_env)
              closure_env
          , Env.push ty ty_env
          , Universe.max universe universe' ))
    in
    universe
;;

let infer_literal (lit : Literal.t) : Core_ty.t =
  match lit with
  | Unit -> Unit
  | Bool _ -> Bool
  | Int _ -> Int
;;
