open Core

open struct
  module Syntax = Oak_syntax
  module Core = Oak_core
  module Acc = Utility.Acc
  module Purity = Syntax.Purity
  module Transparency = Syntax.Transparency
  module Var = Syntax.Var
end

(*
  (= (lambda(A type): A) func(type) type)
  
    let x: |x, y, z|:
      x + y + z
      
    let z: |x, type 'x, type 'y, type z, z : int|:
      x + y + z
      
    let w: ||x, z, w||:
      x + y + z
*)
(* we can evaluate a semantic path into a syntactic path *)
module State = struct
  type t = { context : Syntax.expr Var.Map.t; next_id : int ref }

  let fresh t name =
    let id = !(t.next_id) in
    incr t.next_id;
    Var.create name id
end

type result = {
  vars : (Syntax.expr * Syntax.expr) Acc.t;
  ty : Syntax.expr;
  core_vars : (Core.ty * Core.kind) Acc.t;
  core_expr : Core.expr;
  core_ty : Core.ty;
  purity : Purity.t;
}

module Effects = struct
  type t = {
    vars : (Var.t * Syntax.expr) Acc.t;
    purity : Purity.t;
    transparency : Transparency.t;
  }

  let empty = { vars = Acc.empty; purity = Pure; transparency = Transparent }
  let opaque = { vars = Acc.empty; purity = Pure; transparency = Opaque }
  let impure = { vars = Acc.empty; purity = Impure; transparency = Opaque }

  let merge r1 r2 =
    let vars = Acc.(r1.vars @ r2.vars) in
    let purity = Purity.merge r1.purity r2.purity in
    let transparency = Transparency.merge r1.transparency r2.transparency in
    { vars; purity; transparency }

  let merge_list = List.fold ~init:empty ~f:merge
  let of_vars vars = { vars; purity = Pure; transparency = Transparent }
end

exception Exn of Error.t

let fail e = raise_notrace (Exn e)
let fail_s s = fail (Error.t_of_sexp s)

module Subst : sig
  type t [@@deriving sexp_of]

  val of_list : (Var.t * Syntax.expr) list -> t
  val apply : t -> Syntax.expr -> Syntax.expr
end = struct
  type t [@@deriving sexp_of]

  let of_list _ = failwith ""
  let apply _ _ = failwith ""
end
(* let infer_kind st (expr : Syntax.expr ) : Syntax.expr = 
  match expr with
  | _ -> ()
  | _ -> ()
;; *)

(*
  a path in weak head normal form
*)
type path_whnf =
  | Path_var of Var.t
  | Path_simple_ty of Syntax.simple_ty
  | Path_type
  | Path_sing of Syntax.expr
  | Path_ty_mod of Syntax.ty_mod
  | Path_ty_fun of Syntax.ty_fun
  | Path_app of path_whnf * Syntax.expr list
  | Path_proj of path_whnf * string
[@@deriving sexp_of]

let lookup_var st var =
  match Map.find st.State.context var with
  | None -> fail_s [%message "Variable not found" (var : Var.t)]
  | Some ty -> begin
      match ty with
      | Expr_unit | Expr_bool _ -> failwith "Was not a type"
      | _ -> ty
    end

let rec infer_elab st (expr : Syntax.expr) =
  match expr with
  | Syntax.Expr_var var -> begin
      match Map.find st.State.context var with
      | None -> fail_s [%message "Variable not found" (var : Var.t)]
      | Some ty ->
          let kind = infer_elab st ty in
          failwith ""
    end
  | Syntax.Expr_seal _ -> ()
  | Syntax.Expr_ty_sing _ -> ()
  | Syntax.Expr_app (_, _)
  | Syntax.Expr_abs (_, _, _)
  | Syntax.Expr_ty_fun (_, _)
  | Syntax.Expr_proj (_, _)
  | Syntax.Expr_mod _ | Syntax.Expr_ty_mod _
  | Syntax.Expr_let (_, _, _)
  | Syntax.Expr_type | Syntax.Expr_kind | Syntax.Expr_bool _
  | Syntax.Expr_ty_bool | Syntax.Expr_unit | Syntax.Expr_ty_unit
  | Syntax.Expr_if (_, _, _) ->
      ()

and sub_ty ty1 ty2 =
  ()

and infer_ty st (expr : Syntax.expr) : Effects.t * Syntax.expr =
  match expr with
  | Syntax.Expr_app (f, args) ->
      let f_effects, f_ty = infer_ty st f in
      let args_res = List.map args ~f:(infer_ty st) in
      let args_effects, args_ty = List.unzip args_res in
      let new_effects =
        List.concat
          [
            (if Transparency.equal f_effects.transparency Opaque then
               [ Effects.of_vars (Acc.of_list [ (State.fresh st "", f_ty) ]) ]
             else []);
            List.filter_map args_res ~f:(fun (arg_effect, arg_ty) ->
                if Transparency.equal arg_effect.transparency Opaque then
                  Some
                    (Effects.of_vars
                       (Acc.of_list [ (State.fresh st "arg", arg_ty) ]))
                else None);
          ]
      in
      let res_effects =
        Effects.merge
          (Effects.merge f_effects (Effects.merge_list args_effects))
          (Effects.merge_list new_effects)
      in
      let res_ty = infer_app f_ty args_ty in
      (res_effects, res_ty)
  | Syntax.Expr_var var -> 
    (* (Effects.empty, Syntax.Expr_ty_sing (Expr_var var)) *)
    failwith ""
  | Syntax.Expr_seal (expr, ty) -> 
    failwith ""
  | Syntax.Expr_abs (_, _, _)
  | Syntax.Expr_ty_fun _
  | Syntax.Expr_proj (_, _)
  | Syntax.Expr_mod _ | Syntax.Expr_ty_mod _
  | Syntax.Expr_let (_, _, _)
  | Syntax.Expr_type | Syntax.Expr_kind | Syntax.Expr_ty_sing _
  | Syntax.Expr_bool _ | Syntax.Expr_ty_bool | Syntax.Expr_unit
  | Syntax.Expr_ty_unit
  | Syntax.Expr_if (_, _, _) ->
      failwith ""

and infer_app (f_ty : Syntax.expr) (args_ty : Syntax.expr list) : Syntax.expr =
  let params, ret_ty =
    match f_ty with
    | Expr_ty_fun ty -> ty
    | _ -> fail_s [%message "Was not a function type" (f_ty : Syntax.expr)]
  in
  let params_with_args =
    match List.zip params args_ty with
    | Ok t -> t
    | Unequal_lengths ->
        fail_s
          [%message
            "Invalid arg length"
              (params : Syntax.param list)
              (args_ty : Syntax.expr list)]
  in
  let subst =
    List.filter_map params_with_args ~f:(fun ((name, _), arg) ->
        let open Option.Let_syntax in
        let%map name = name in
        (name, arg))
    |> Subst.of_list
  in
  Subst.apply subst ret_ty

and infer_proj (mod_ty : Syntax.expr) (field : string) : Syntax.expr =
  let ty_decls =
    match mod_ty with
    | Expr_ty_mod ty -> ty
    | _ -> fail_s [%message "Was not a module type" (mod_ty : Syntax.expr)]
  in
  (* let proj_ty =
    match List.Assoc.find ty_decls ~equal:String.equal field with
    | None ->
        fail_s
          [%message
            "Field not found"
              (mod_path : path_whnf)
              (mod_ty : Syntax.expr)
              (field : string)]
    | Some ty -> ty
  in *)
  (* map the free variables in proj_ty to have project *)
  failwith ""

(* converts a semantic path to a syntactic path *)
and eval_path st (expr : Syntax.expr) =
  let path =
    match expr with
    | Syntax.Expr_var var -> Path_var var
    | Syntax.Expr_seal _ -> fail_s [%message "not a path" (expr : Syntax.expr)]
    | Syntax.Expr_proj (mod_expr, field) ->
        (* let mod_path = eval_path st mod_expr in *)
        (* let ty = infer_ty st expr |> to_path st |> eval_path st in *)
        failwith ""
    | Syntax.Expr_app (_, _)
    | Syntax.Expr_abs (_, _, _)
    | Syntax.Expr_ty_fun (_, _)
    | Syntax.Expr_mod _ | Syntax.Expr_ty_mod _
    | Syntax.Expr_let (_, _, _)
    | Syntax.Expr_type | Syntax.Expr_kind | Syntax.Expr_ty_sing _
    | Syntax.Expr_bool _ | Syntax.Expr_ty_bool | Syntax.Expr_unit
    | Syntax.Expr_ty_unit
    | Syntax.Expr_if (_, _, _) ->
        failwith ""
  in
  path

(* and natural_ty st (path : path_whnf) =
  match path with
  | Path_var var -> lookup_var st var
  | Path_sing expr -> infer_ty st expr
  | Path_type -> Expr_kind
  | Path_simple_ty _ -> Expr_type
  | Path_ty_mod ty_mod -> failwith ""
  | Path_ty_fun (params, ret) -> failwith ""
  | Path_app (f_path, args) ->
      let f_ty = natural_ty st f_path in
      let params, ret_ty =
        match f_ty with
        | Expr_ty_fun ty -> ty
        | _ ->
            fail_s
              [%message
                "Was not a function type"
                  (f_path : path_whnf)
                  (f_ty : Syntax.expr)]
      in
      let params_with_args =
        match List.zip params args with
        | Ok t -> t
        | Unequal_lengths ->
            fail_s
              [%message
                "Invalid arg length"
                  (params : Syntax.param list)
                  (args : Syntax.expr list)]
      in
      let subst =
        List.filter_map params_with_args ~f:(fun ((name, _), arg) ->
            let open Option.Let_syntax in
            let%map name = name in
            (name, arg))
        |> Subst.of_list
      in
      Subst.apply subst ret_ty
  | Path_proj (mod_path, field) ->
      let mod_ty = natural_ty st mod_path in
      let ty_decls =
        match mod_ty with
        | Expr_ty_mod ty -> ty
        | _ ->
            fail_s
              [%message
                "Was not a module type"
                  (mod_path : path_whnf)
                  (mod_ty : Syntax.expr)]
      in
      let proj_ty =
        match List.Assoc.find ty_decls ~equal:String.equal field with
        | None ->
            fail_s
              [%message
                "Field not found"
                  (mod_path : path_whnf)
                  (mod_ty : Syntax.expr)
                  (field : string)]
        | Some ty -> ty
      in
      (* map the free variables in proj_ty to have project *)
      failwith "" *)

let check expr = ()
