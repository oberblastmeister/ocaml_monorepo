open Core

open struct
  module Syntax = Oak_syntax
  module Core = Oak_core
  module Acc = Utility.Acc
  module Purity = Syntax.Purity
  module Transparency = Syntax.Transparency
  module Var = Syntax.Var

  type path = Syntax.path
end

module State = struct
  type t = { context : Syntax.path Var.Map.t; next_id : int ref }

  let fresh t name =
    let id = !(t.next_id) in
    incr t.next_id;
    Var.create name id

  let add var ty t =
    { t with context = Map.add_exn t.context ~key:var ~data:ty }

  let add_list vars t =
    {
      t with
      context =
        List.fold vars ~init:t.context ~f:(fun context (var, ty) ->
            Map.add_exn context ~key:var ~data:ty);
    }
end

module Effects = struct
  type t = {
    vars : (Var.t * Syntax.path) Acc.t;
    purity : Purity.t;
    transparency : Transparency.t;
  }

  let empty = { vars = Acc.empty; purity = Pure; transparency = Transparent }
  let opaque = { vars = Acc.empty; purity = Pure; transparency = Opaque }
  let impure = { vars = Acc.empty; purity = Impure; transparency = Opaque }
  let of_purity purity = { vars = Acc.empty; purity; transparency = Opaque }

  let of_transparency transparency =
    { vars = Acc.empty; purity = Pure; transparency }

  let merge r1 r2 =
    let vars = Acc.(r1.vars @ r2.vars) in
    let purity = Purity.merge r1.purity r2.purity in
    let transparency = Transparency.merge r1.transparency r2.transparency in
    { vars; purity; transparency }

  let merge_list = List.fold ~init:empty ~f:merge

  let of_var var =
    { vars = Acc.singleton var; purity = Pure; transparency = Opaque }

  let ( ++ ) = merge

  let get_vars t =
    let vars = Acc.to_list t.vars in
    (vars, { t with vars = Acc.of_list vars })
end

let ( ++ ) = Effects.( ++ )

exception Exn of Error.t

let fail e = raise_notrace (Exn e)
let fail_s s = fail (Error.t_of_sexp s)

module Subst : sig
  type t [@@deriving sexp_of]

  val of_list : (Var.t * Syntax.path) list -> t
  val apply : t -> Syntax.expr -> Syntax.expr
  val find_exn : t -> Var.t -> Syntax.expr
end = struct
  type t [@@deriving sexp_of]

  let of_list _ = failwith ""
  let apply _ _ = failwith ""
  let find_exn _ = failwith ""
end

let rec infer st (e : Syntax.expr) : Effects.t * Syntax.path =
  match e with
  | Syntax.Expr_var var -> begin
      match Map.find st.State.context var with
      | None -> fail_s [%message "Variable not found" (var : Var.t)]
      | Some ty ->
          let kind = natural_kind st ty in
          begin match get_universe_exn st kind with
          | Type -> (Effects.empty, ty)
          | Kind | KIND -> (Effects.empty, Path_ty_sing { e = ty; ty = kind })
          end
    end
  | Syntax.Expr_universe u ->
      ( Effects.empty,
        Path_ty_sing
          {
            e = Path_universe u;
            ty = Path_universe (Syntax.Universe.incr_exn u);
          } )
  | Syntax.Expr_seal { e; ty = ty2 } ->
      let eff, ty1 = infer st e in
      let vars, eff = Effects.get_vars eff in
      let ty2 = expr_to_path st ty2 in
      let st = State.add_list vars st in
      subtype st ty1 ty2;
      ( Effects.of_purity eff.purity ++ Effects.of_transparency eff.transparency,
        ty2 )
  | Syntax.Expr_app { e; es } ->
      let e_eff, e_ty = infer_force_transparent st "f" e in
      let e_vars, e_eff = Effects.get_vars e_eff in
      let st = State.add_list e_vars st in
      let e_ty = Syntax.path_ty_fun_exn (zonk st e_ty) in
      let (st, es_eff), es_tys =
        List.fold_mapi es ~init:(st, Effects.empty) ~f:(fun i (st, eff_acc) e ->
            let eff, ty =
              infer_force_transparent st ("arg" ^ Int.to_string i) e
            in
            let vars, eff = Effects.get_vars eff in
            let st = State.add_list vars st in
            ((st, eff_acc ++ eff), ty))
      in
      let tys_and_params =
        match List.zip es_tys e_ty.params with
        | Ok t -> t
        | Unequal_lengths ->
            fail_s
              [%message
                "Invalid number of parameters passed"
                  ~expected:(List.length e_ty.params : int)
                  ~actual:(List.length es_tys : int)]
      in
      let subst =
        List.fold tys_and_params ~init:Var.Map.empty
          ~f:(fun subst (ty, param) ->
            subtype st ty (eval_subst_path subst param.ty);
            let v = synthesize_value_from_transparent_ty st ty in
            Map.add_exn subst ~key:param.var ~data:v)
      in
      (* TODO: also need to add opaque if this is an impure functor *)
      let res_eff = e_eff ++ Effects.of_purity e_ty.purity ++ es_eff in
      let res_ty = eval_subst_path subst e_ty.ty in
      (res_eff, res_ty)
  | Syntax.Expr_abs { params; body; purity } ->
      let st', params =
        List.fold_map ~init:st
          ~f:(fun st param ->
            let param = infer_param st param in
            (State.add param.var param.ty st, param))
          params
      in
      let body_eff, body_ty = infer st' body in
      let body_eff_vars = Acc.to_list body_eff.vars in
      if Purity.(body_eff.purity > purity) then
        fail_s
          [%message
            "Unexpected purity"
              ~expected:(purity : Purity.t)
              ~actual:(body_eff.purity : Purity.t)];
      begin match body_eff.purity with
      | Impure ->
          if not (List.is_empty body_eff_vars) then
            fail_s
              [%message
                "Cannot generate hidden existential types in impure functor"
                  ~vars:(body_eff_vars : (Var.t * Syntax.path) list)];
          ( Effects.empty,
            Syntax.Path_ty_fun { params; ty = body_ty; purity = Impure } )
      | Pure ->
          let new_body_eff_vars =
            List.map body_eff_vars ~f:(fun (var, ty) ->
                let new_params =
                  List.map params ~f:(fun param ->
                      let var = State.fresh st param.var.name in
                      ({ var; ty } : Syntax.path_param))
                in
                (* params |-> new_params *)
                let subst =
                  List.zip_exn params new_params
                  |> List.map ~f:(fun (param1, param2) ->
                      (param1.var, Syntax.Path_var param2.var))
                  |> Var.Map.of_alist_exn
                in
                let ty' =
                  Syntax.Path_ty_fun
                    {
                      params = new_params;
                      ty = subst_path subst ty;
                      purity = Pure;
                    }
                in
                (var, State.fresh st var.name, ty'))
          in
          let app_exprs =
            List.map params ~f:(fun p -> Syntax.Value_path (Path_var p.var))
          in
          let subst =
            List.map new_body_eff_vars ~f:(fun (var1, var2, _ty) ->
                (var1, Syntax.Path_app { e = Path_var var2; es = app_exprs }))
            |> Var.Map.of_alist_exn
          in
          let res_ty =
            Syntax.Path_ty_fun
              { params; ty = subst_path subst body_ty; purity = Pure }
          in
          let eff_vars =
            List.map new_body_eff_vars ~f:(fun (_var1, var2, ty) -> (var2, ty))
          in
          let res_eff =
            {
              Effects.vars = Acc.of_list eff_vars;
              purity = Pure;
              transparency = body_eff.transparency;
            }
          in
          (res_eff, res_ty)
      end
  | Syntax.Expr_let { var; rhs; body } ->
      let rhs_eff, rhs_ty = infer_force_transparent st var.name rhs in
      let vars, rhs_eff = Effects.get_vars rhs_eff in
      let body_eff, body_ty =
        let st = State.add_list vars st in
        infer st body
      in
      let value = synthesize_value_from_transparent_ty st rhs_ty in
      let subst = Var.Map.singleton var value in
      let res_ty = eval_subst_path subst body_ty in
      (rhs_eff ++ body_eff, res_ty)
  | Syntax.Expr_proj { e; field } ->
      let e_eff, e_ty = infer_force_transparent st "proj" e in
      let vars, e_eff = Effects.get_vars e_eff in
      let st = State.add_list vars st in
      let ty_mod = Syntax.path_ty_mod_exn (zonk st e_ty) in
      let decl =
        List.find ty_mod ~f:(fun decl -> String.equal decl.field field)
        |> Option.value_or_thunk ~default:(fun () ->
            fail_s
              [%message
                "Field not found"
                  ~ty:(ty_mod : Syntax.path_ty_decl list)
                  (field : string)])
      in
      (e_eff, decl.ty)
  | Syntax.Expr_mod decls ->
      begin match
        String.Map.of_list_with_key decls ~get_key:(fun decl -> decl.field)
      with
      | `Ok _ -> ()
      | `Duplicate_key field ->
          fail_s [%message "Duplicate field" (field : string)]
      end;
      let (_st', eff), ty_decls =
        List.fold_map decls ~init:(st, Effects.empty)
          ~f:(fun (st, acc_eff) decl ->
            let eff, ty = infer st decl.e in
            let vars, eff = Effects.get_vars eff in
            let ty_decl : Syntax.path_ty_decl =
              { field = decl.field; var = decl.var; ty }
            in
            let st = State.add_list vars st in
            ((State.add decl.var ty st, acc_eff ++ eff), ty_decl))
      in
      (eff, Syntax.Path_ty_mod ty_decls)
  | Syntax.Expr_ty_fun ty_fun ->
      let ty_fun = expr_ty_fun_to_path st ty_fun in
      let kind = natural_kind st (Path_ty_fun ty_fun) in
      (Effects.empty, Syntax.Path_ty_sing { e = Path_ty_fun ty_fun; ty = kind })
  | Syntax.Expr_ty_mod ty_mod ->
      let ty_mod = expr_ty_mod_to_path st ty_mod in
      let kind = natural_kind st (Path_ty_mod ty_mod) in
      (Effects.empty, Syntax.Path_ty_sing { e = Path_ty_mod ty_mod; ty = kind })
  | Syntax.Expr_ty_sing ty_sing ->
      let ty_sing = expr_ty_sing_to_path st ty_sing in
      let u = get_universe_exn st ty_sing.ty in
      ( Effects.empty,
        Syntax.Path_ty_sing { e = Path_ty_sing ty_sing; ty = Path_universe u }
      )
  | Syntax.Expr_bool _ -> (Effects.empty, Syntax.Path_core_ty Ty_bool)
  | Syntax.Expr_unit -> (Effects.empty, Syntax.Path_core_ty Ty_unit)
  | Syntax.Expr_core_ty core_ty ->
      ( Effects.empty,
        Syntax.Path_ty_sing
          { e = Path_core_ty core_ty; ty = Path_universe Type } )
  | Syntax.Expr_if { e1; e2; e3 } ->
      let eff1 = check st e1 (Path_core_ty Ty_bool) in
      let eff2, ty2 = infer st e2 in
      let eff3, ty3 = infer st e3 in
      let k2 = natural_kind st ty2 in
      let k3 = natural_kind st ty3 in
      let u2 = get_universe_exn st k2 in
      let u3 = get_universe_exn st k3 in
      if not (Syntax.Universe.equal u2 u3) then
        fail_s
          [%message
            "Different universes in if branches"
              (k2 : Syntax.path)
              (k3 : Syntax.path)];
      equivalent st ty2 ty3 k2;
      (eff1 ++ eff2 ++ eff3, ty2)

(*
the path should be well typed in the context
natural kind returns the most natural kind, which is the kind that is inferred in the singleton when inferred normally
since we are already taking a path as input, the input already has identity and thus doesn't need further identity
*)
and natural_kind st (ty : Syntax.path) : Syntax.path =
  match ty with
  | Path_var var -> Map.find_exn st.State.context var
  | Path_core_ty _ -> Path_universe Type
  | Path_universe u -> Path_universe (Syntax.Universe.incr_exn u)
  | Path_ty_sing { e = _; ty } -> natural_kind st ty
  | Path_ty_mod ty_mod ->
      let _, universe =
        List.fold ty_mod ~init:(st, Syntax.Universe.minimum)
          ~f:(fun (st, u) decl ->
            let kind = natural_kind st decl.ty in
            let v = get_universe_exn st kind in
            (State.add decl.var decl.ty st, Syntax.Universe.max u v))
      in
      Path_universe universe
  | Path_ty_fun ty_fun ->
      let st, universe =
        List.fold ty_fun.params ~init:(st, Syntax.Universe.minimum)
          ~f:(fun (st, u) param ->
            let kind = natural_kind st param.ty in
            let v = get_universe_exn st kind in
            (State.add param.var param.ty st, Syntax.Universe.max u v))
      in
      let ret_kind = natural_kind st ty_fun.ty in
      let ret_universe = get_universe_exn st ret_kind in
      let universe = Syntax.Universe.max universe ret_universe in
      Path_universe universe
  | Path_app { e; es } ->
      let ty_fun = natural_kind st e |> zonk st |> Syntax.path_ty_fun_exn in
      let subst =
        List.zip_exn ty_fun.params es
        |> List.map ~f:(fun (param, arg) -> (param.var, arg))
        |> Var.Map.of_alist_exn
      in
      eval_subst_path subst ty_fun.ty
  | Path_proj { e; field } ->
      let ty_mod = natural_kind st e |> zonk st |> Syntax.path_ty_mod_exn in
      let var_subst =
        List.map ty_mod ~f:(fun decl ->
            (decl.var, Syntax.Path_proj { e; field = decl.field }))
        |> Var.Map.of_alist_exn
      in
      let decl =
        List.find ty_mod ~f:(fun decl -> String.equal decl.field field)
        |> Option.value_exn
      in
      subst_path var_subst decl.ty

and check st (e : Syntax.expr) (ty : Syntax.path) : Effects.t =
  match (e, ty) with
  | _ ->
      let eff, ty' = infer st e in
      let vars, eff = Effects.get_vars eff in
      let st = State.add_list vars st in
      subtype st ty' ty;
      eff

and expr_ty_sing_to_path st ({ e; ty } : Syntax.expr_ty_sing) :
    Syntax.path_ty_sing =
  let e = expr_to_path st e in
  let ty = expr_to_path st ty in
  { e; ty }

and expr_ty_fun_to_path st ({ params; ty; purity } : Syntax.expr_ty_fun) :
    Syntax.path_ty_fun =
  let st, params =
    List.fold_map params ~init:st ~f:(fun st param ->
        let param = infer_param st param in
        (State.add param.var param.ty st, param))
  in
  let ty = expr_to_path st ty in
  { params; ty; purity }

and expr_ty_mod_to_path st (decls : Syntax.ty_decl list) :
    Syntax.path_ty_decl list =
  let _, decls =
    List.fold_map decls ~init:st ~f:(fun st { field; var; ty } ->
        let ty = expr_to_path st ty in
        (State.add var ty st, ({ field; var; ty } : Syntax.path_ty_decl)))
  in
  decls

and infer_transparent_exn st (expr : Syntax.expr) : Syntax.path =
  let eff, ty = infer st expr in
  assert (Transparency.equal eff.transparency Transparent);
  assert (List.is_empty (Acc.to_list eff.vars));
  ty

(* forces the inferred type to always be transparent by possibly generating abstract types *)
and infer_force_transparent st name (expr : Syntax.expr) :
    Effects.t * Syntax.path =
  let eff1, ty = infer st expr in
  let eff2, rhs_ty_transparent =
    begin match eff1.transparency with
    | Transparent -> (Effects.empty, ty)
    | Opaque ->
        let var = State.fresh st name in
        (Effects.of_var (var, ty), Syntax.Path_ty_sing { e = Path_var var; ty })
    end
  in
  (eff1 ++ eff2, rhs_ty_transparent)

and infer_param st (param : Syntax.param) : Syntax.path_param =
  let ty = expr_to_path st param.ty in
  { var = param.var; ty }

and subst_value (subst : Syntax.path Var.Map.t) (v : Syntax.value) :
    Syntax.value =
  match v with
  | Value_path p -> Value_path (subst_path subst p)
  | Value_irrelevant -> Value_irrelevant
  | Value_mod decls ->
      Value_mod
        (List.map decls ~f:(fun { field; var; e } ->
             ({ field; var; e = subst_value subst e } : Syntax.value_decl)))
  | Value_abs { params; body; purity } ->
      let params = List.map params ~f:(subst_param subst) in
      let body = subst_value subst body in
      Value_abs { params; body; purity }

and subst_path (subst : Syntax.path Var.Map.t) (p : Syntax.path) : Syntax.path =
  match p with
  | Path_var var -> Map.find subst var |> Option.value ~default:p
  | Path_core_ty _ | Path_universe _ -> p
  | Path_ty_sing { e; ty } ->
      let e = subst_path subst e in
      let ty = subst_path subst ty in
      Path_ty_sing { e; ty }
  | Path_ty_mod decls ->
      Path_ty_mod
        (List.map decls ~f:(fun decl ->
             { decl with ty = subst_path subst decl.ty }))
  | Path_ty_fun { params; ty; purity } ->
      let params = List.map params ~f:(subst_param subst) in
      let ty = subst_path subst ty in
      Path_ty_fun { params; ty; purity }
  | Path_app { e; es } ->
      let e = subst_path subst e in
      let es = List.map es ~f:(subst_value subst) in
      Path_app { e; es }
  | Path_proj { e; field } -> Path_proj { e = subst_path subst e; field }

and subst_param (subst : Syntax.path Var.Map.t)
    ({ var; ty } : Syntax.path_param) : Syntax.path_param =
  let ty = subst_path subst ty in
  { var; ty }

and eval_subst_path (subst : Syntax.value Var.Map.t) (p : Syntax.path) :
    Syntax.path =
  eval_subst_path' subst p |> Syntax.value_path_exn

(* p and subst must be well typed in some context *)
and eval_subst_path' (subst : Syntax.value Var.Map.t) (p : Syntax.path) :
    Syntax.value =
  match p with
  | Path_var var -> Map.find subst var |> Option.value ~default:(Value_path p)
  | Path_core_ty _ | Path_universe _ -> Value_path p
  | Path_ty_sing { e; ty } ->
      let e = eval_subst_path subst e in
      let ty = eval_subst_path subst ty in
      Value_path (Path_ty_sing { e; ty })
  | Path_ty_mod decls ->
      let decls =
        List.map decls ~f:(fun { field; var; ty } ->
            ({ field; var; ty = eval_subst_path subst ty }
              : Syntax.path_ty_decl))
      in
      Value_path (Path_ty_mod decls)
  | Path_ty_fun { params; ty; purity } ->
      let params =
        List.map params ~f:(fun { var; ty } : Syntax.path_param ->
            { var; ty = eval_subst_path subst ty })
      in
      let ty = eval_subst_path subst ty in
      Value_path (Path_ty_fun { params; ty; purity })
  | Path_app { e; es } ->
      let e = eval_subst_path' subst e in
      let es = List.map es ~f:(eval_subst_value subst) in
      begin match e with
      | Syntax.Value_mod _ -> failwith "invalid value for applications"
      | Syntax.Value_path e -> Value_path (Path_app { e; es })
      | Syntax.Value_irrelevant -> failwith ""
      | Syntax.Value_abs { params; body; purity } ->
          assert (Purity.equal purity Pure);
          let subst =
            List.fold (List.zip_exn params es) ~init:subst
              ~f:(fun subst (param, arg) ->
                Map.add_exn subst ~key:param.var ~data:arg)
          in
          eval_subst_value subst body
      end
  | Path_proj { e; field } ->
      let e = eval_subst_path' subst e in
      begin match e with
      | Syntax.Value_mod decls ->
          let decl =
            List.find decls ~f:(fun decl -> String.equal decl.field field)
            |> Option.value_exn ~message:"field should exist"
          in
          decl.e
      | Syntax.Value_path e -> Value_path (Path_proj { e; field })
      | Syntax.Value_irrelevant -> failwith ""
      | Syntax.Value_abs _ -> failwith "invalid value for projection"
      end

(* does not evaluate under lambdas, which is okay because they will be evaluated when they are applied *)
and eval_subst_value (subst : Syntax.value Var.Map.t) (v : Syntax.value) :
    Syntax.value =
  match v with
  | Syntax.Value_mod decls ->
      let decls =
        List.map decls ~f:(fun { field; var; e } : Syntax.value_decl ->
            let e = eval_subst_value subst e in
            { field; var; e })
      in
      Value_mod decls
  | Syntax.Value_abs _ -> v
  | Syntax.Value_irrelevant -> Value_irrelevant
  | Syntax.Value_path p -> eval_subst_path' subst p

and synthesize_value_from_transparent_ty' (ty : Syntax.path) : Syntax.value =
  match ty with
  | Path_var _ | Path_app _ | Path_core_ty _ | Path_universe _ ->
      failwith "not a transparent type"
  | Path_ty_sing { e; ty = _ } -> Value_path e
  | Path_ty_mod decls ->
      Value_mod
        (List.map decls ~f:(fun { field; var; ty } : Syntax.value_decl ->
             { field; var; e = synthesize_value_from_transparent_ty' ty }))
  | Path_ty_fun { params = _; ty = _; purity = Impure } -> Value_irrelevant
  | Path_ty_fun { params; ty; purity = Pure } ->
      Value_abs
        {
          params;
          body = synthesize_value_from_transparent_ty' ty;
          purity = Pure;
        }
  | Path_proj _ -> failwith ""

and synthesize_value_from_transparent_ty st (ty : Syntax.path) : Syntax.value =
  failwith ""

and subtype st (ty1 : Syntax.path) (ty2 : Syntax.path) = failwith ""

and equivalent st (ty1 : Syntax.path) (ty2 : Syntax.path) (k : Syntax.path) =
  failwith ""

and expr_to_path st (expr : Syntax.expr) : Syntax.path =
  let eff, ty = infer st expr in
  begin match eff.transparency with
  | Transparent -> ()
  | Opaque -> fail_s [%message "Paths cannot generate abstract types"]
  end;
  if not (List.is_empty (Acc.to_list eff.vars)) then
    failwith "Transparency was checked above";
  begin match get_universe st ty with
  | None -> fail_s [%message "Expected a path" ~actual:(expr : Syntax.expr)]
  | Some _ -> ()
  end;
  let v = synthesize_value_from_transparent_ty st ty in
  Syntax.value_path_exn v

(* and check_expr_path st (expr : Syntax.expr ) : unit = *)

and get_universe st (ty : Syntax.path) : Syntax.Universe.t option =
  match zonk st ty with
  | Path_universe u -> Some u
  | Path_ty_sing { e = _; ty = Path_universe u } -> Some u
  | _ -> None

and get_universe_exn st (ty : Syntax.path) : Syntax.Universe.t =
  match get_universe st ty with
  | None -> raise_s [%message "Was not a universe type" (ty : Syntax.path)]
  | Some u -> u

(* fully normalizes a path pushes all singletons down to root *)
and zonk st (path : Syntax.path) : Syntax.path = failwith ""

(* fully normalizes a path *)
and normalize_path (st : State.t) (path : Syntax.path) : Syntax.path =
  failwith ""

(* pushes singletons down one layer *)
and push_sing st (path : Syntax.path) : Syntax.path = failwith ""
