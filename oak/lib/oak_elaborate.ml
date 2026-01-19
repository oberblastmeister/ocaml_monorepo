open Core

open struct
  module Syntax = Oak_syntax
  module Core = Oak_core
  module Acc = Utility.Acc
  module Purity = Syntax.Purity
  module Var = Syntax.Var
  module Cvar = Syntax.Cvar
end

module State = struct
  type t = { context : Syntax.path Cvar.Map.t }

  let add var ty t = { context = Map.add_exn t.context ~key:var ~data:ty }

  let add_list vars t =
    {
      context =
        List.fold vars ~init:t.context ~f:(fun context (var, ty) ->
            Map.add_exn context ~key:var ~data:ty);
    }

  let find t var = Map.find_exn t.context var
end

module Effects = struct
  type t = { vars : (Var.t * Syntax.path) Acc.t; purity : Purity.t }

  let empty = { vars = Acc.empty; purity = Pure }
  let impure = { vars = Acc.empty; purity = Impure }
  let of_purity purity = { vars = Acc.empty; purity }

  let merge r1 r2 =
    let vars = Acc.(r1.vars @ r2.vars) in
    let purity = Purity.merge r1.purity r2.purity in
    { vars; purity }

  let merge_list = List.fold ~init:empty ~f:merge
  let of_var var = { vars = Acc.singleton var; purity = Pure }
  let ( ++ ) = merge

  let get_vars t =
    let vars = Acc.to_list t.vars in
    (vars, { t with vars = Acc.of_list vars })

  let get_cvars t =
    let vars, t = get_vars t in
    (List.map vars ~f:(fun (v, p) -> (Syntax.Cvar.Var v, p)), t)
end

let ( ++ ) = Effects.( ++ )

exception Exn of Error.t

let fail e = raise_notrace (Exn e)
let fail_s s = fail (Error.t_of_sexp s)

(*
the path should be well typed in the context
natural kind returns the most natural kind, which is the kind that is inferred in the singleton when inferred normally
since we are already taking a path as input, the input already has identity and thus doesn't need further identity
*)
let rec natural_kind st (ty : Syntax.path) : Syntax.path =
  match ty with
  | Path_core_ty _ -> Path_universe Type
  | Path_universe u -> Path_universe (Syntax.Universe.incr_exn u)
  | Path_ty_sing { e = _; ty } -> natural_kind st ty
  | Path_ty_mod { binder } ->
      Syntax.Path_ty_mod_binder.unpack binder
        ~f:(fun { var; ty_decls; ty_decls_map = _ } ->
          let _, universe =
            List.fold ty_decls ~init:(st, Syntax.Universe.minimum)
              ~f:(fun (st, u) { field; ty } ->
                let v = get_universe_exn st ty in
                ( State.add (Record_field { var; field }) ty st,
                  Syntax.Universe.max u v ))
          in
          Syntax.Path_universe universe)
  | Path_ty_fun { binder; purity = _ } ->
      Syntax.Path_ty_fun_binder.unpack binder ~f:(fun { params; ty } ->
          let st, universe =
            List.fold params ~init:(st, Syntax.Universe.minimum)
              ~f:(fun (st, u) param ->
                let v = get_universe_exn st param.ty in
                (State.add (Var param.var) param.ty st, Syntax.Universe.max u v))
          in
          let ret_universe = get_universe_exn st ty in
          let universe = Syntax.Universe.max universe ret_universe in
          Syntax.Path_universe universe)
  | Path_long_ident long_ident -> long_ident_natural_kind st long_ident

and long_ident_natural_kind st (lident : Syntax.long_ident) : Syntax.path =
  match lident with
  | Long_ident_var var -> Map.find_exn st.State.context var
  | Long_ident_app { func; args } ->
      let kind =
        long_ident_natural_kind st func |> force st |> Syntax.path_ty_fun_exn
      in
      long_ident_app_natural_kind kind args
  | Long_ident_proj { mod_e; field } ->
      let kind =
        long_ident_natural_kind st mod_e |> force st |> Syntax.path_ty_mod_exn
      in
      long_ident_proj_natural_kind mod_e kind field

and long_ident_app_natural_kind ({ binder; purity = _ } : Syntax.path_ty_fun)
    (args : Syntax.value list) : Syntax.path =
  Syntax.Path_ty_fun_binder.unpack binder ~f:(fun { params; ty } ->
      let subst =
        List.zip_exn params args
        |> List.map ~f:(fun (param, arg) -> (param.var, arg))
        |> Var.Map.of_alist_exn
      in
      Syntax.eval_subst_path subst ty)

and long_ident_proj_natural_kind (mod_e : Syntax.long_ident)
    ({ binder } : Syntax.path_ty_mod) (field : string) : Syntax.path =
  Syntax.Path_ty_mod_binder.unpack binder
    ~f:(fun { var; ty_decls = _; ty_decls_map } ->
      let record_var_subst =
        Syntax.Record_var.Map.singleton var
          (Syntax.Record_field_action.Replace mod_e)
      in
      let decl = Map.find_exn ty_decls_map field in
      Syntax.subst_record_var_path record_var_subst decl.ty)

(* exposes the head of a path by normalizing and pushing singletons down one layer *)
and force st (path : Syntax.path) : Syntax.path =
  normalize_path st path |> push_sing st

and maybe_reduce_sing (st : State.t) (lident : Syntax.long_ident) : Syntax.path
    =
  let path = Syntax.Path_long_ident lident in
  let kind = natural_kind st path in
  match kind with
  | Path_ty_sing { e; ty = _ } -> normalize_path st e
  | _ -> path

(* normalizes a path to whnf *)
and normalize_path (st : State.t) (path : Syntax.path) : Syntax.path =
  let path : Syntax.path =
    match path with
    | Path_core_ty _ | Path_universe _ | Path_ty_sing _ | Path_ty_mod _
    | Path_ty_fun _ ->
        path
    | Path_long_ident lident -> normalize_long_ident st lident
  in
  path

and normalize_long_ident st (lident : Syntax.long_ident) : Syntax.path =
  match lident with
  | Long_ident_app { func; args } ->
      let func = normalize_long_ident st func |> Syntax.path_long_ident_exn in
      maybe_reduce_sing st (Long_ident_app { func; args })
  | Long_ident_proj { mod_e; field } ->
      let mod_e = normalize_long_ident st mod_e |> Syntax.path_long_ident_exn in
      maybe_reduce_sing st (Long_ident_proj { mod_e; field })
  | Long_ident_var _ -> maybe_reduce_sing st lident

(* pushes singletons down one layer *)
and push_sing st (path : Syntax.path) : Syntax.path =
  match path with
  | Path_ty_sing { e; ty } -> begin
      match force st ty with
      | Path_universe _ -> path
      | Path_ty_sing { e = _; ty } -> push_sing st (Path_ty_sing { e; ty })
      | Path_ty_mod { binder } ->
          Syntax.Path_ty_mod_binder.unpack binder
            ~f:(fun { var; ty_decls; ty_decls_map = _ } ->
              let mod_e = Syntax.path_long_ident_exn e in
              let ty_decls =
                List.map ty_decls ~f:(fun { field; ty } : Syntax.path_ty_decl ->
                    {
                      field;
                      ty =
                        Path_ty_sing
                          {
                            e =
                              Path_long_ident (Long_ident_proj { mod_e; field });
                            ty;
                          };
                    })
              in
              let ty_decls_map =
                String.Map.of_list_with_key_exn ty_decls ~get_key:(fun decl ->
                    decl.field)
              in
              Syntax.Path_ty_mod
                {
                  binder =
                    Syntax.Path_ty_mod_binder.pack
                      { var; ty_decls; ty_decls_map };
                })
      | Path_ty_fun { binder = _; purity = Impure } | Path_core_ty _ -> ty
      | Path_ty_fun { binder; purity = Pure } ->
          Syntax.Path_ty_fun_binder.unpack binder
            ~f:(fun ({ params; ty } : Syntax.Path_ty_fun_binder.data) ->
              let func = Syntax.path_long_ident_exn e in
              let args =
                List.map params ~f:(fun param ->
                    Syntax.Value_path
                      (Path_long_ident (Long_ident_var (Var param.var))))
              in
              let e = Syntax.Path_long_ident (Long_ident_app { func; args }) in
              Syntax.Path_ty_fun
                {
                  binder =
                    Syntax.Path_ty_fun_binder.pack
                      { params; ty = Path_ty_sing { e; ty } };
                  purity = Pure;
                })
      | Path_long_ident _ -> path
    end
  | _ -> path

and kind_to_universe st (kind : Syntax.path) : Syntax.Universe.t option =
  match force st kind with
  | Path_universe u -> Some u
  | Path_ty_sing { e = _; ty = Path_universe u } -> Some u
  | _ -> None

and kind_to_universe_exn st (kind : Syntax.path) : Syntax.Universe.t =
  match kind_to_universe st kind with
  | Some u -> u
  | None -> raise_s [%message "Was not a universe type" (kind : Syntax.path)]

and get_universe_exn st (ty : Syntax.path) : Syntax.Universe.t =
  let kind = natural_kind st ty in
  kind_to_universe_exn st kind

exception Synthesize_value

let rec synthesize_value st (ty : Syntax.path) : Syntax.value =
  match force st ty with
  | Path_long_ident _ | Path_universe _ -> failwith "type was not transparent"
  | Path_core_ty _ -> Value_irrelevant
  | Path_ty_sing { e; ty = _ } -> Value_path e
  | Path_ty_mod { binder } ->
      (* TODO: this is wrong, need to substitute properly *)
      Syntax.Path_ty_mod_binder.unpack binder
        ~f:(fun { var; ty_decls; ty_decls_map = _ } ->
          let _, decls =
            List.fold_map ty_decls ~init:Syntax.Record_var.Map.empty
              ~f:(fun subst { field; ty } ->
                let ty = Syntax.subst_record_var_path subst ty in
                let e = synthesize_value st ty in
                let subst =
                  Map.set subst ~key:var
                    ~data:
                      (Syntax.Record_field_action.Replace
                         (Syntax.value_lident_exn e))
                in
                (subst, ({ field; e } : Syntax.value_decl)))
          in
          Syntax.Value_mod { decls })
  | Path_ty_fun { binder; purity } ->
      Syntax.Path_ty_fun_binder.unpack binder ~f:(fun { params; ty } ->
          match purity with
          | Impure -> Syntax.Value_irrelevant
          | Pure ->
              Value_abs
                {
                  binder =
                    Syntax.Value_abs_binder.pack
                      { params; body = synthesize_value st ty };
                  purity = Pure;
                })

let is_ty_transparent st (ty : Syntax.path) : bool = failwith ""

(*
  checks whether ty1 is a subtype of ty2.
  They need not be the same kind when calling subtype.
  For now does only simple subtyping and so records must have the same fields in the same order.
*)
let rec subtype st (ty1 : Syntax.path) (ty2 : Syntax.path) : unit =
  match (force st ty1, force st ty2) with
  | Path_universe u, Path_universe v when Syntax.Universe.equal u v -> ()
  | Path_core_ty ty1, Path_core_ty ty2 ->
      if not (Syntax.equal_core_ty ty1 ty2) then
        fail_s
          [%message
            "core types are not equal "
              (ty1 : Syntax.core_ty)
              (ty2 : Syntax.core_ty)]
  | Path_ty_sing { e = _; ty = Path_universe u }, Path_universe v ->
      if not (Syntax.Universe.equal u v) then
        fail_s
          [%message
            "Not a subtype, universes are not equal for singleton"
              (u : Syntax.Universe.t)
              (v : Syntax.Universe.t)]
  | ( Path_ty_sing { e = e1; ty = Path_universe u },
      Path_ty_sing { e = e2; ty = Path_universe v } ) ->
      if not (Syntax.Universe.equal u v) then
        fail_s
          [%message
            "" "Not a subtype, universes are not equal for singleton"
              (u : Syntax.Universe.t)
              (v : Syntax.Universe.t)];
      equivalent st e1 e2 (Path_universe u)
  | Path_ty_mod { binder = binder1 }, Path_ty_mod { binder = binder2 } ->
      Syntax.Path_ty_mod_binder.unpack binder1
        ~f:(fun { var = var1; ty_decls = decls1; ty_decls_map = _ } ->
          Syntax.Path_ty_mod_binder.unpack binder2
            ~f:(fun { var = var2; ty_decls = decls2; ty_decls_map = _ } ->
              let zipped_decls =
                match List.zip decls1 decls2 with
                | Ok t -> t
                | Unequal_lengths ->
                    fail_s
                      [%message
                        "different number of declarations"
                          (decls1 : Syntax.path_ty_decl list)
                          (decls2 : Syntax.path_ty_decl list)]
              in
              let _ =
                List.fold zipped_decls ~init:st ~f:(fun st (decl1, decl2) ->
                    subtype st decl1.ty decl2.ty;
                    let st =
                      State.add
                        (Record_field { var = var1; field = decl1.field })
                        decl1.ty st
                      |> State.add
                           (Record_field { var = var2; field = decl2.field })
                           (Path_ty_sing
                              {
                                e =
                                  Path_long_ident
                                    (Long_ident_var
                                       (Record_field
                                          { var = var1; field = decl1.field }));
                                ty = decl2.ty;
                              })
                    in
                    st)
              in
              ()))
  | ( Path_ty_fun { binder = binder1; purity = purity1 },
      Path_ty_fun { binder = binder2; purity = purity2 } ) ->
      if Purity.(purity1 > purity2) then
        fail_s
          [%message
            "purity was not a subtype" (purity1 : Purity.t) (purity2 : Purity.t)];
      Syntax.Path_ty_fun_binder.unpack binder1
        ~f:(fun { params = params1; ty = ty1 } ->
          Syntax.Path_ty_fun_binder.unpack binder2
            ~f:(fun { params = params2; ty = ty2 } ->
              let zipped_params =
                match List.zip params1 params2 with
                | Ok t -> t
                | Unequal_lengths ->
                    fail_s
                      [%message
                        "different number of parameters"
                          (params1 : Syntax.path_param list)
                          (params2 : Syntax.path_param list)]
              in
              let st =
                List.fold zipped_params ~init:st ~f:(fun st (param1, param2) ->
                    subtype st param2.ty param1.ty;
                    let st =
                      State.add (Var param2.var) param2.ty st
                      |> State.add (Var param1.var)
                           (Path_ty_sing
                              {
                                e =
                                  Path_long_ident
                                    (Long_ident_var (Var param2.var));
                                ty = param1.ty;
                              })
                    in
                    st)
              in
              subtype st ty1 ty2;
              ()))
  | _ -> fail_s [%message "Not subtype" (ty1 : Syntax.path) (ty2 : Syntax.path)]

and equivalent st (e1 : Syntax.path) (e2 : Syntax.path) (ty : Syntax.path) :
    unit =
    equivalent_value st (Value_path e1) (Value_path e2) ty

(*
precondition: e1 and e2 must both have type ty
Checks whether the types e1 and e2 are equivalent at type ty.
*)
and equivalent_value st (e1 : Syntax.value) (e2 : Syntax.value)
    (ty : Syntax.path) : unit =
  match force st ty with
  | Path_ty_sing _ | Path_core_ty _ -> ()
  | Path_long_ident _ ->
      let u = get_universe_exn st ty in
      begin match u with
      | Type -> ()
      | Kind | Sig ->
          let _k =
            structure_equivalent_lident st
              (Syntax.value_lident_exn e1)
              (Syntax.value_lident_exn e2)
          in
          ()
      end
  | Path_universe _ ->
      let _k =
        structure_equivalent st (Syntax.value_path_exn e1)
          (Syntax.value_path_exn e2)
      in
      ()
  | Path_ty_fun { binder = _; purity = Impure } -> ()
  | Path_ty_fun { binder; purity = Pure } ->
      Syntax.Path_ty_fun_binder.unpack binder ~f:(fun { params; ty } ->
          let args =
            List.map params ~f:(fun param ->
                Syntax.Value_path
                  (Path_long_ident (Long_ident_var (Var param.var))))
          in
          let st =
            State.add_list
              (List.map params ~f:(fun param ->
                   (Syntax.Cvar.Var param.var, param.ty)))
              st
          in
          let e1 = Syntax.app_value_exn e1 args in
          let e2 = Syntax.app_value_exn e2 args in
          equivalent_value st e1 e2 ty;
          ())
  | Path_ty_mod { binder } ->
      Syntax.Path_ty_mod_binder.unpack binder
        ~f:(fun { var; ty_decls; ty_decls_map = _ } ->
          let _ =
            List.fold ty_decls ~init:st ~f:(fun st { field; ty } ->
                let e1 = Syntax.proj_value_exn e1 field in
                let e2 = Syntax.proj_value_exn e2 field in
                equivalent_value st e1 e2 ty;
                State.add (Record_field { var; field }) ty st)
          in
          ())

(*
checks whether ty1 and ty2 are structurally equivalent.
Then returns the kind that they are structurally equivalent at.
*)
and structure_equivalent st (e1 : Syntax.path) (e2 : Syntax.path) : Syntax.path
    =
  match (force st e1, force st e2) with
  | Path_long_ident lident1, Path_long_ident lident2 ->
      structure_equivalent_lident st lident1 lident2
  | Path_core_ty ty1, Path_core_ty ty2 ->
      if not (Syntax.equal_core_ty ty1 ty2) then
        fail_s
          [%message
            "core types are not structurally equivalent"
              (ty1 : Syntax.core_ty)
              (ty2 : Syntax.core_ty)];
      Syntax.Path_universe Type
  | Path_universe u1, Path_universe u2 ->
      if not (Syntax.Universe.equal u1 u2) then
        fail_s
          [%message
            "universes are not structurally equivalent"
              (u1 : Syntax.Universe.t)
              (u2 : Syntax.Universe.t)];
      Syntax.Path_universe (Syntax.Universe.incr_exn u1)
  | Path_ty_sing { e = e1; ty = ty1 }, Path_ty_sing { e = e2; ty = ty2 } ->
      let kind = structure_equivalent st ty1 ty2 in
      equivalent st e1 e2 ty1;
      kind
  | ( Path_ty_fun { binder = binder1; purity = purity1 },
      Path_ty_fun { binder = binder2; purity = purity2 } ) ->
      if not (Purity.equal purity1 purity2) then
        fail_s
          [%message
            "function purities are not structurally equivalent"
              (purity1 : Purity.t)
              (purity2 : Purity.t)];
      Syntax.Path_ty_fun_binder.unpack binder1
        ~f:(fun { params = params1; ty = ty1 } ->
          Syntax.Path_ty_fun_binder.unpack binder2
            ~f:(fun { params = params2; ty = ty2 } ->
              let zipped_params =
                match List.zip params1 params2 with
                | Ok t -> t
                | Unequal_lengths ->
                    fail_s
                      [%message
                        "different number of parameters in function types"
                          (params1 : Syntax.path_param list)
                          (params2 : Syntax.path_param list)]
              in
              let st, universe =
                List.fold zipped_params ~init:(st, Syntax.Universe.minimum)
                  ~f:(fun (st, u) (param1, param2) ->
                    let kind = structure_equivalent st param1.ty param2.ty in
                    let v = kind_to_universe_exn st kind in
                    let st =
                      State.add (Var param1.var) param1.ty st
                      |> State.add (Var param2.var)
                           (Syntax.Path_ty_sing
                              {
                                e =
                                  Path_long_ident
                                    (Long_ident_var (Var param1.var));
                                ty = param1.ty;
                              })
                    in
                    (st, Syntax.Universe.max u v))
              in
              let ret_kind = structure_equivalent st ty1 ty2 in
              let ret_universe = kind_to_universe_exn st ret_kind in
              let universe = Syntax.Universe.max universe ret_universe in
              Syntax.Path_universe universe))
  | Path_ty_mod { binder = binder1 }, Path_ty_mod { binder = binder2 } ->
      Syntax.Path_ty_mod_binder.unpack binder1
        ~f:(fun { var = var1; ty_decls = decls1; ty_decls_map = _ } ->
          Syntax.Path_ty_mod_binder.unpack binder2
            ~f:(fun { var = var2; ty_decls = decls2; ty_decls_map = _ } ->
              let zipped_decls =
                match List.zip decls1 decls2 with
                | Ok t -> t
                | Unequal_lengths ->
                    fail_s
                      [%message
                        "different number of declarations in module types"
                          (decls1 : Syntax.path_ty_decl list)
                          (decls2 : Syntax.path_ty_decl list)]
              in
              let _, universe =
                List.fold zipped_decls ~init:(st, Syntax.Universe.minimum)
                  ~f:(fun (st, u) (decl1, decl2) ->
                    if not (String.equal decl1.field decl2.field) then
                      fail_s
                        [%message
                          "module type fields are not equal"
                            (decl1.field : string)
                            (decl2.field : string)];
                    let kind = structure_equivalent st decl1.ty decl2.ty in
                    let v = kind_to_universe_exn st kind in
                    let st =
                      State.add
                        (Record_field { var = var1; field = decl1.field })
                        decl1.ty st
                      |> State.add
                           (Record_field { var = var2; field = decl2.field })
                           (Syntax.Path_ty_sing
                              {
                                e =
                                  Path_long_ident
                                    (Long_ident_var
                                       (Record_field
                                          { var = var1; field = decl1.field }));
                                ty = decl1.ty;
                              })
                    in
                    (st, Syntax.Universe.max u v))
              in
              Syntax.Path_universe universe))
  | _, _ ->
      fail_s
        [%message
          "paths are not structurally equivalent"
            (e1 : Syntax.path)
            (e2 : Syntax.path)]

and structure_equivalent_lident st (lident1 : Syntax.long_ident)
    (lident2 : Syntax.long_ident) : Syntax.path =
  match (lident1, lident2) with
  | Long_ident_var var1, Long_ident_var var2 ->
      if Syntax.Cvar.equal var1 var2 then State.find st var1
      else
        fail_s
          [%message "Variables were not equal" (var1 : Cvar.t) (var2 : Cvar.t)]
  | ( Long_ident_app { func = func1; args = args1 },
      Long_ident_app { func = func2; args = args2 } ) ->
      let ty_fun =
        structure_equivalent_lident st func1 func2
        |> force st |> Syntax.path_ty_fun_exn
      in
      assert (Purity.equal ty_fun.purity Pure);
      Syntax.Path_ty_fun_binder.unpack ty_fun.binder ~f:(fun { params; ty } ->
          let subst =
            List.zip_exn (List.zip_exn args1 args2) params
            |> List.fold ~init:Var.Map.empty
                 ~f:(fun subst ((arg1, arg2), param) ->
                   equivalent_value st arg1 arg2
                     (Syntax.eval_subst_path subst param.ty);
                   Map.add_exn subst ~key:param.var ~data:arg1)
          in
          Syntax.eval_subst_path subst ty)
  | ( Long_ident_proj { mod_e = mod_e1; field = field1 },
      Long_ident_proj { mod_e = mod_e2; field = field2 } ) ->
      let kind =
        structure_equivalent_lident st mod_e1 mod_e2
        |> force st |> Syntax.path_ty_mod_exn
      in
      if not (String.equal field1 field2) then
        fail_s
          [%message "Fields were not equal" (field1 : string) (field2 : string)];
      long_ident_proj_natural_kind mod_e1 kind field1
  | _, _ ->
      fail_s
        [%message
          "lident not equal"
            (lident1 : Syntax.long_ident)
            (lident2 : Syntax.long_ident)]

let rec infer st (e : Syntax.expr) : Effects.t * Syntax.path =
  match e with
  | Syntax.Expr_var var -> begin
      match Map.find st.State.context var with
      | None -> fail_s [%message "Variable not found" (var : Cvar.t)]
      | Some ty ->
          let kind = natural_kind st ty in
          (Effects.empty, Path_ty_sing { e = ty; ty = kind })
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
      let vars, eff = Effects.get_cvars eff in
      let ty2 = expr_to_path st ty2 in
      let st = State.add_list vars st in
      subtype st ty1 ty2;
      (Effects.of_purity eff.purity, ty2)
  | Syntax.Expr_app { func; args } ->
      let e_eff, e_ty = infer_force_transparent st "f" func in
      let e_vars, e_eff = Effects.get_cvars e_eff in
      let st = State.add_list e_vars st in
      let ({ binder = e_ty_binder; purity = e_ty_purity } : Syntax.path_ty_fun)
          =
        Syntax.path_ty_fun_exn (force st e_ty)
      in
      let (st, es_eff), es_tys =
        List.fold_mapi args ~init:(st, Effects.empty)
          ~f:(fun i (st, eff_acc) e ->
            let eff, ty =
              infer_force_transparent st ("arg" ^ Int.to_string i) e
            in
            let vars, eff = Effects.get_cvars eff in
            let st = State.add_list vars st in
            ((st, eff_acc ++ eff), ty))
      in
      Syntax.Path_ty_fun_binder.unpack e_ty_binder ~f:(fun e_ty ->
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
                subtype st ty (Syntax.eval_subst_path subst param.ty);
                let v = synthesize_value st ty in
                Map.add_exn subst ~key:param.var ~data:v)
          in
          let res_eff = e_eff ++ Effects.of_purity e_ty_purity ++ es_eff in
          let res_ty = Syntax.eval_subst_path subst e_ty.ty in
          (res_eff, res_ty))
  | Syntax.Expr_abs { params; body; purity } ->
      let st', params =
        List.fold_map ~init:st
          ~f:(fun st param ->
            let param = infer_param st param in
            (State.add (Var param.var) param.ty st, param))
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
            Syntax.Path_ty_fun
              {
                binder = Syntax.Path_ty_fun_binder.pack { params; ty = body_ty };
                purity = Impure;
              } )
      | Pure ->
          let lifted_body_eff_vars =
            List.map body_eff_vars ~f:(fun (var, ty) ->
                let ty' =
                  Syntax.Path_ty_fun
                    {
                      binder = Syntax.Path_ty_fun_binder.pack { params; ty };
                      purity = Pure;
                    }
                in
                (var, Syntax.Var.make_fresh var, ty'))
          in
          let app_args = List.map params ~f:(fun p -> Syntax.path_var p.var) in
          let subst =
            List.map lifted_body_eff_vars ~f:(fun (var1, var2, _ty) ->
                ( var1,
                  Syntax.Value_path
                    (Syntax.Path_long_ident
                       (Syntax.Long_ident_app
                          { func = Long_ident_var (Var var2); args = app_args }))
                ))
            |> Var.Map.of_alist_exn
          in
          let res_ty =
            Syntax.Path_ty_fun
              {
                binder =
                  Syntax.Path_ty_fun_binder.pack
                    { params; ty = Syntax.eval_subst_path subst body_ty };
                purity = Pure;
              }
          in
          let eff_vars =
            List.map lifted_body_eff_vars ~f:(fun (_var1, var2, ty) ->
                (var2, ty))
          in
          let res_eff =
            { Effects.vars = Acc.of_list eff_vars; purity = Pure }
          in
          (res_eff, res_ty)
      end
  | Syntax.Expr_let { var; rhs; body } ->
      let rhs_eff, rhs_ty = infer_force_transparent st var.name rhs in
      let vars, rhs_eff = Effects.get_cvars rhs_eff in
      let body_eff, body_ty =
        let st = State.add_list vars st in
        infer st body
      in
      let value = synthesize_value st rhs_ty in
      let subst = Var.Map.singleton var value in
      let res_ty = Syntax.eval_subst_path subst body_ty in
      (rhs_eff ++ body_eff, res_ty)
  | Syntax.Expr_proj { mod_e; field } ->
      let e_eff, e_ty = infer_force_transparent st "proj" mod_e in
      let vars, e_eff = Effects.get_cvars e_eff in
      let st = State.add_list vars st in
      let ({ binder } : Syntax.path_ty_mod) =
        Syntax.path_ty_mod_exn (force st e_ty)
      in
      Syntax.Path_ty_mod_binder.unpack binder ~f:(fun ty_mod ->
          let decl =
            List.find ty_mod.ty_decls ~f:(fun ty_decl ->
                String.equal ty_decl.field field)
            |> Option.value_or_thunk ~default:(fun () ->
                fail_s
                  [%message
                    "Field not found"
                      ~ty:(ty_mod : Syntax.Path_ty_mod_binder.data)
                      (field : string)])
          in
          (e_eff, decl.ty))
  | Syntax.Expr_mod { var; decls } ->
      begin match
        String.Map.of_list_with_key decls ~get_key:(fun decl -> decl.field)
      with
      | `Ok _ -> ()
      | `Duplicate_key field ->
          fail_s [%message "Duplicate field" (field : string)]
      end;
      let (_st', eff), ty_decls =
        List.fold_map decls ~init:(st, Effects.empty)
          ~f:(fun (st, acc_eff) { e; field } ->
            let eff, ty = infer st e in
            let vars, eff = Effects.get_vars eff in
            let ty_decl : Syntax.path_ty_decl = { field; ty } in
            let st =
              State.add_list
                (List.map ~f:(fun (v, p) -> (Syntax.Cvar.Var v, p)) vars)
                st
            in
            ( (State.add (Record_field { var; field }) ty st, acc_eff ++ eff),
              ty_decl ))
      in
      let ty_decls_map =
        String.Map.of_list_with_key_exn ty_decls ~get_key:(fun decl ->
            decl.field)
      in
      ( eff,
        Syntax.Path_ty_mod
          {
            binder =
              Syntax.Path_ty_mod_binder.pack { var; ty_decls; ty_decls_map };
          } )
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
      let kind = natural_kind st (Path_ty_sing ty_sing) in
      ( Effects.empty,
        Syntax.Path_ty_sing { e = Path_ty_sing ty_sing; ty = kind } )
  | Syntax.Expr_bool _ -> (Effects.empty, Syntax.Path_core_ty Ty_bool)
  | Syntax.Expr_unit -> (Effects.empty, Syntax.Path_core_ty Ty_unit)
  | Syntax.Expr_core_ty core_ty ->
      ( Effects.empty,
        Syntax.Path_ty_sing
          { e = Path_core_ty core_ty; ty = Path_universe Type } )
  | Syntax.Expr_if { cond; body1; body2 } ->
      let eff1 = check st cond (Path_core_ty Ty_bool) in
      let eff2, ty2 = infer st body1 in
      let eff3, ty3 = infer st body2 in
      let k2 = natural_kind st ty2 in
      let k3 = natural_kind st ty3 in
      let u2 = kind_to_universe_exn st k2 in
      let u3 = kind_to_universe_exn st k3 in
      if not (Syntax.Universe.equal u2 u3) then
        fail_s
          [%message
            "Different universes in if branches"
              (u2 : Syntax.Universe.t)
              (u3 : Syntax.Universe.t)];
      equivalent st ty2 ty3 k2;
      (eff1 ++ eff2 ++ eff3, ty2)

and check st (e : Syntax.expr) (ty : Syntax.path) : Effects.t =
  match (e, ty) with
  | _ ->
      let eff, ty' = infer st e in
      let vars, eff = Effects.get_vars eff in
      let st =
        State.add_list
          (List.map ~f:(fun (v, p) -> (Syntax.Cvar.Var v, p)) vars)
          st
      in
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
        (State.add (Var param.var) param.ty st, param))
  in
  let ty = expr_to_path st ty in
  { binder = Syntax.Path_ty_fun_binder.pack { params; ty }; purity }

and expr_ty_mod_to_path st ({ var; ty_decls } : Syntax.ty_mod) :
    Syntax.path_ty_mod =
  let _, ty_decls =
    List.fold_map ty_decls ~init:st ~f:(fun st { field; ty } ->
        let ty = expr_to_path st ty in
        ( State.add (Record_field { var; field }) ty st,
          ({ field; ty } : Syntax.path_ty_decl) ))
  in
  let ty_decls_map =
    String.Map.of_list_with_key_exn ty_decls ~get_key:(fun decl -> decl.field)
  in
  { binder = Syntax.Path_ty_mod_binder.pack { var; ty_decls; ty_decls_map } }

(* forces the inferred type to always be transparent by possibly generating abstract types *)
and infer_force_transparent st name (expr : Syntax.expr) :
    Effects.t * Syntax.path =
  let eff1, ty = infer st expr in
  let eff2, rhs_ty_transparent =
    match is_ty_transparent st ty with
    | true -> (Effects.empty, ty)
    | false ->
        let var = Var.create name in
        ( Effects.of_var (var, ty),
          Syntax.Path_ty_sing
            { e = Path_long_ident (Long_ident_var (Var var)); ty } )
  in
  (eff1 ++ eff2, rhs_ty_transparent)

and infer_param st (param : Syntax.param) : Syntax.path_param =
  let ty = expr_to_path st param.ty in
  { var = param.var; ty }

and expr_to_path st (ty : Syntax.expr) : Syntax.path =
  let eff, kind = infer st ty in
  let vars, _eff = Effects.get_vars eff in
  if not (List.is_empty vars && is_ty_transparent st kind) then
    fail_s [%message "Paths cannot generate abstract types"];
  begin match kind_to_universe st kind with
  | None -> fail_s [%message "Expected a path" ~actual:(ty : Syntax.expr)]
  | Some _ -> ()
  end;
  let v = synthesize_value st kind in
  Syntax.value_path_exn v
