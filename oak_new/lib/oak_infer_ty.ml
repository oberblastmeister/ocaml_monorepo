open Prelude
open Oak_syntax

open struct
  module Common = Oak_common
  module Evaluate = Oak_evaluate
end

let rec infer_props (ty_env : ty_env) (ty : ty) =
  match ty with
  | Ty_universe props -> props
  | Ty_sing _ -> { size = Size.sig_ }
  | Ty_struct ty ->
    let size, _, _ =
      List.fold
        ty.field_specs
        ~init:(Size.sig_, ty_env, ty.env)
        ~f:(fun (size, ty_env, closure_env) field_spec ->
          let ty = Evaluate.eval_ty closure_env field_spec.ty in
          let props = infer_props ty_env ty in
          ( Size.max size props.size
          , Seq.push ty ty_env
          , Seq.push (Value.free_of_size (Seq.length ty_env)) closure_env ))
    in
    { size }
  | Ty_fun ty ->
    let arg : value_arg =
      { e = Value.free_of_size (Seq.length ty_env); param_props = ty.param_props }
    in
    let param_ty_props = infer_props ty_env ty.param_ty in
    let body_ty_props =
      infer_props (Seq.push ty.param_ty ty_env) (Evaluate.Fun_ty.app ty arg)
    in
    { size = Size.max param_ty_props.size body_ty_props.size }
  | Ty_pack _ | Ty_core _ -> { size = Size.type_ }
  | Ty_decode ty -> infer_neutral_universe ty_env ty

and infer_neutral (ty_env : ty_env) (e : neutral) : ty =
  Bwd.fold_left
    e.spine
    ~init:(Bwd.Empty, Seq.get_level_exn ty_env e.head)
    ~f:(fun (spine, ty) frame ->
      let ty =
        match frame with
        | App arg -> Evaluate.Ty.app ty_env ty arg
        | Proj field ->
          Evaluate.Ty.proj ty_env (Value_neutral { head = e.head; spine }) ty field
        | Out -> Evaluate.Ty.out ty_env ty
      in
      spine <: frame, ty)
  |> snd

and infer_neutral_universe (ty_env : ty_env) (e : neutral) : Ty_props.t =
  let ty = infer_neutral ty_env e in
  Evaluate.whnf_ty ty_env ty |> Ty.ty_universe_val_exn
;;
