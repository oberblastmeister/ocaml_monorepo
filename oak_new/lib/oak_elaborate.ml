open Core

open struct
  module Syntax = Oak_syntax
  module Acc = Utility.Acc
  module Purity = Syntax.Purity
  module Var = Syntax.Var
  module Cvar = Syntax.Cvar
end

module State = struct
  type elem =
    { ty : Syntax.ty
    ; (* we keep a strengthened value here to avoid repeatedly computing this *)
      strengthened : Syntax.ty
    }

  type t = { context : elem Cvar.Map.t }

  let add var ty t =
    { context =
        Map.set
          t.context
          ~key:(Var var)
          ~data:{ ty; strengthened = Value_ty_sing { e = Syntax.Value.var var; ty } }
    }
  ;;

  let add_list vars t =
    { context =
        List.fold vars ~init:t.context ~f:(fun context (var, ty) ->
          Map.set
            context
            ~key:(Var var)
            ~data:{ ty; strengthened = Value_ty_sing { e = Syntax.Value.var var; ty } })
    }
  ;;

  (*
    For module variables, the ty_decls is always empty.
    We don't need it, because module variables are always projected.
  *)
  let add_mod_var var t =
    let var' = Syntax.Mod_var.create () in
    let ty =
      Syntax.Value_ty_mod
        { binder =
            Syntax.Value_ty_mod_binder.pack
              { var; ty_decls = []; ty_decls_map = String.Map.empty }
        }
    in
    let strengthened =
      Syntax.Value_ty_mod
        { binder =
            Syntax.Value_ty_mod_binder.pack
              { var = var'; ty_decls = []; ty_decls_map = String.Map.empty }
        }
    in
    { context = Map.set t.context ~key:(Mod_var var) ~data:{ ty; strengthened } }
  ;;

  let add_field var ({ field; ty } : Syntax.value_ty_decl) t =
    let add_decl_ty_mod_exn (ty_mod : Syntax.ty) field ty =
      let ty_mod = Syntax.Value.get_ty_mod_exn ty_mod in
      let binder = Syntax.Value_ty_mod_binder.unpack_advanced ty_mod.binder in
      let ty_decls_map = Map.add_exn binder.ty_decls_map ~key:field ~data:{ field; ty } in
      let binder = Syntax.Value_ty_mod_binder.pack { binder with ty_decls_map } in
      let ty_mod = Syntax.Value_ty_mod { ty_mod with binder } in
      ty_mod
    in
    let elem = Map.find_exn t.context (Mod_var var) in
    let elem =
      { ty = add_decl_ty_mod_exn elem.ty field ty
      ; strengthened =
          add_decl_ty_mod_exn
            elem.strengthened
            field
            (Value_ty_sing
               { e =
                   Value_ty
                     (Value_path (Path_proj { mod_e = Path_var (Mod_var var); field }))
               ; ty
               })
      }
    in
    { t with context = Map.set t.context ~key:(Mod_var var) ~data:elem }
  ;;

  let add_param (param : Syntax.value_param) t = add param.var param.ty t

  let add_params (params : Syntax.value_param list) t =
    List.fold params ~init:t ~f:(fun st param -> add param.var param.ty st)
  ;;

  let find_ty t var = Map.find t.context var |> Option.map ~f:(fun elem -> elem.ty)

  let find_strengthened t var =
    Map.find t.context var |> Option.map ~f:(fun elem -> elem.strengthened)
  ;;
end

module Effects = struct
  type t =
    { vars : (Var.t * Syntax.ty) Acc.t
    ; purity : Purity.t
    }

  let empty = { vars = Acc.empty; purity = Pure }
  let impure = { vars = Acc.empty; purity = Impure }
  let of_purity purity = { vars = Acc.empty; purity }

  let merge r1 r2 =
    let vars = Acc.(r1.vars @ r2.vars) in
    let purity = Purity.merge r1.purity r2.purity in
    { vars; purity }
  ;;

  let merge_list = List.fold ~init:empty ~f:merge
  let of_var var = { vars = Acc.singleton var; purity = Pure }
  let ( ++ ) = merge

  let get_vars t =
    let vars = Acc.to_list t.vars in
    vars, { t with vars = Acc.of_list vars }
  ;;

  let get_cvars t =
    let vars, t = get_vars t in
    List.map vars ~f:(fun (v, p) -> Syntax.Cvar.Var v, p), t
  ;;
end

let ( ++ ) = Effects.( ++ )

exception Exn of Error.t

let fail e = raise_notrace (Exn e)
let fail_s s = fail (Error.t_of_sexp s)

let var_natural_ty st var =
  match State.find_ty st var with
  | None -> fail_s [%message (var : Cvar.t)]
  | Some t -> t
;;

let var_strengthened_ty st var =
  match State.find_strengthened st var with
  | None -> fail_s [%message (var : Cvar.t)]
  | Some t -> t
;;

let var_natural_ty_exn st var =
  match State.find_ty st var with
  | None -> raise_s [%message (var : Cvar.t)]
  | Some t -> t
;;

let var_strengthened_ty_exn st var =
  match State.find_strengthened st var with
  | None -> raise_s [%message (var : Cvar.t)]
  | Some t -> t
;;

(*
the path should be well typed in the context
natural kind returns the most natural kind, which is the kind that is inferred in the singleton when inferred normally
since we are already taking a path as input, the input already has identity and thus doesn't need further identity
*)
let rec ty_natural_kind st (ty : Syntax.ty) : Syntax.ty =
  match ty with
  | Value_core_ty _ -> Value_univ Type
  | Value_univ u -> Value_univ (Syntax.Universe.incr_exn u)
  | Value_ty_sing { e = _; ty } -> ty_natural_kind st ty
  | Value_ty_mod { binder } ->
    Syntax.Value_ty_mod_binder.unpack
      binder
      ~f:(fun { var; ty_decls; ty_decls_map = _ } ->
        let _st, universe =
          List.fold
            ty_decls
            ~init:(State.add_mod_var var st, Syntax.Universe.minimum)
            ~f:(fun (st, u) ({ field = _; ty } as ty_decl : Syntax.value_ty_decl) ->
              let v = get_univ_exn st ty in
              State.add_field var ty_decl st, Syntax.Universe.lub u v)
        in
        Syntax.Value_univ universe)
  | Value_ty_fun { binder; purity = _ } ->
    Syntax.Value_ty_fun_binder.unpack binder ~f:(fun { params; body_ty } ->
      let st, universe =
        List.fold
          params
          ~init:(st, Syntax.Universe.minimum)
          ~f:(fun (st, u) (param : Syntax.value_param) ->
            let v = get_univ_exn st param.ty in
            State.add param.var param.ty st, Syntax.Universe.max u v)
      in
      let ret_universe = get_univ_exn st body_ty in
      let universe = Syntax.Universe.max universe ret_universe in
      Syntax.Value_univ universe)
  | Value_path path -> path_natural_kind st path

and path_natural_kind st (path : Syntax.path) : Syntax.ty =
  match path with
  | Path_var var -> var_natural_ty_exn st var
  | Path_app { func; args } ->
    let kind = path_natural_kind st func |> push_sing st |> Syntax.Value.get_ty_fun_exn in
    path_app_natural_kind kind args
  | Path_proj { mod_e; field } ->
    let kind =
      path_natural_kind st mod_e |> push_sing st |> Syntax.Value.get_ty_mod_exn
    in
    path_proj_natural_kind mod_e kind field

and path_app_natural_kind
      ({ binder; purity = _ } : Syntax.value_ty_fun)
      (args : Syntax.value list)
  : Syntax.ty
  =
  Syntax.Value_ty_fun_binder.unpack binder ~f:(fun { params; body_ty } ->
    let subst =
      List.zip_exn params args
      |> List.map ~f:(fun (param, arg) -> Cvar.Var param.var, arg)
      |> Syntax.Subst.of_alist_exn
    in
    Syntax.Ty.eval subst body_ty |> Syntax.Value.get_ty_exn)

and path_proj_natural_kind
      (mod_e : Syntax.path)
      ({ binder } : Syntax.value_ty_mod)
      (field : string)
  : Syntax.ty
  =
  let ({ var; ty_decls = _; ty_decls_map } : Syntax.Value_ty_mod_binder.data) =
    Syntax.Value_ty_mod_binder.unpack_advanced binder
  in
  let subst = Syntax.Subst.singleton (Mod_var var) (Syntax.Value_ty (Value_path mod_e)) in
  let decl = Map.find_exn ty_decls_map field in
  Syntax.Ty.eval subst decl.ty |> Syntax.Value.get_ty_exn

(* exposes the head of a ty by normalizing and pushing singletons down one layer *)
and force st (ty : Syntax.ty) : Syntax.ty = lookup_ty st ty |> push_sing st

and lookup_ty (st : State.t) (ty : Syntax.ty) : Syntax.ty =
  match ty with
  | Value_path path -> lookup_path st path
  | _ -> ty

and lookup_path (st : State.t) (path : Syntax.path) : Syntax.ty =
  let kind = path_natural_kind st path in
  match kind with
  | Value_ty_sing { e = Syntax.Value_ty (Value_path path); ty = _ } -> lookup_path st path
  | Value_ty_sing { e = Syntax.Value_ty e; ty = _ } -> e
  | _ -> Value_path path

(* pushes singletons down one layer *)
and push_sing st (ty : Syntax.ty) : Syntax.ty =
  match ty with
  | Value_ty_sing { e; ty } -> begin
    match force st ty with
    | Value_univ _ -> ty
    | Value_ty_sing { e = _; ty } -> push_sing st (Value_ty_sing { e; ty })
    | Value_ty_mod { binder } ->
      Syntax.Value_ty_mod_binder.unpack
        binder
        ~f:(fun { var; ty_decls; ty_decls_map = _ } ->
          let ty_decls =
            List.map ty_decls ~f:(fun { field; ty } : Syntax.value_ty_decl ->
              { field; ty = Value_ty_sing { e = Syntax.proj_value_exn e field; ty } })
          in
          let ty_decls_map =
            String.Map.of_list_with_key_exn ty_decls ~get_key:(fun decl -> decl.field)
          in
          Syntax.Value_ty_mod
            { binder = Syntax.Value_ty_mod_binder.pack { var; ty_decls; ty_decls_map } })
    | Value_ty_fun { binder = _; purity = Impure } | Value_core_ty _ -> ty
    | Value_ty_fun { binder; purity = Pure } ->
      Syntax.Value_ty_fun_binder.unpack
        binder
        ~f:(fun ({ params; body_ty } : Syntax.Value_ty_fun_binder.data) ->
          let args = List.map params ~f:(fun param -> Syntax.Value.var param.var) in
          let e = Syntax.app_value_exn e args in
          Syntax.Value_ty_fun
            { binder =
                Syntax.Value_ty_fun_binder.pack
                  { params; body_ty = Value_ty_sing { e; ty = body_ty } }
            ; purity = Pure
            })
    | Value_path _ -> ty
  end
  | _ -> ty

and get_univ_exn st (ty : Syntax.ty) : Syntax.Universe.t =
  let kind = force st ty |> ty_natural_kind st in
  Syntax.Value.get_ty_univ_exn kind
;;

let rec synthesize_value st (ty : Syntax.ty) : Syntax.value =
  match force st ty with
  | Value_path _ | Value_univ _ -> failwith "type was not transparent"
  | Value_core_ty _ -> Value_irrelevant
  | Value_ty_sing { e; ty = _ } -> e
  | Value_ty_mod { binder } ->
    Syntax.Value_ty_mod_binder.unpack
      binder
      ~f:(fun { var; ty_decls; ty_decls_map = _ } ->
        let st, value_mod =
          List.fold
            ty_decls
            ~init:(State.add_mod_var var st, ({ decls = [] } : Syntax.value_mod))
            ~f:(fun (st, value_mod) ({ field; ty } as ty_decl) ->
              let ty =
                Syntax.Ty.eval
                  (Syntax.Subst.singleton (Mod_var var) (Value_mod value_mod))
                  ty
                |> Syntax.Value.get_ty_exn
              in
              let e = synthesize_value st ty in
              let decl = ({ field; e } : Syntax.value_decl) in
              ( State.add_field var ty_decl st
              , { value_mod with decls = decl :: value_mod.decls } ))
        in
        ignore st;
        Syntax.Value_mod value_mod)
  | Value_ty_fun { binder; purity } ->
    Syntax.Value_ty_fun_binder.unpack binder ~f:(fun { params; body_ty } ->
      let st = State.add_params params st in
      match purity with
      | Impure -> Syntax.Value_irrelevant
      | Pure ->
        Value_abs
          { binder =
              Syntax.Value_abs_binder.pack { params; body = synthesize_value st body_ty }
          ; purity = Pure
          })
;;

let rec is_ty_transparent st (ty : Syntax.ty) : bool =
  match force st ty with
  | Value_path _ | Value_univ _ -> false
  | Value_core_ty _ -> true
  | Value_ty_sing { e = _; ty = _ } -> true
  | Value_ty_mod { binder } ->
    Syntax.Value_ty_mod_binder.unpack
      binder
      ~f:(fun { var; ty_decls; ty_decls_map = _ } ->
        let st, result =
          List.fold
            ty_decls
            ~init:(State.add_mod_var var st, true)
            ~f:(fun (st, result) ({ field = _; ty } as ty_decl) ->
              if not result
              then st, false
              else (
                let transparent = is_ty_transparent st ty in
                State.add_field var ty_decl st, transparent))
        in
        ignore st;
        result)
  | Value_ty_fun { binder; purity } ->
    (match purity with
     | Impure -> true
     | Pure ->
       Syntax.Value_ty_fun_binder.unpack binder ~f:(fun { params; body_ty } ->
         let st = State.add_params params st in
         is_ty_transparent st body_ty))
;;

(*
  checks whether ty1 is a subtype of ty2.
  They need not be the same kind when calling subtype.
  For now does only simple subtyping and so records must have the same fields in the same order.
*)
let rec subtype st (ty1 : Syntax.ty) (ty2 : Syntax.ty) : unit =
  match force st ty1, force st ty2 with
  | Value_univ u, Value_univ v when Syntax.Universe.equal u v -> ()
  | Value_core_ty ty1, Value_core_ty ty2 ->
    if not (Syntax.equal_core_ty ty1 ty2)
    then
      fail_s
        [%message
          "core types are not equal " (ty1 : Syntax.core_ty) (ty2 : Syntax.core_ty)]
  | Value_ty_sing { e = _; ty = Value_univ u }, Value_univ v ->
    if not (Syntax.Universe.equal u v)
    then
      fail_s
        [%message
          "Not a subtype, universes are not equal for singleton"
            (u : Syntax.Universe.t)
            (v : Syntax.Universe.t)]
  | ( Value_ty_sing { e = e1; ty = Value_univ u }
    , Value_ty_sing { e = e2; ty = Value_univ v } ) ->
    if not (Syntax.Universe.equal u v)
    then
      fail_s
        [%message
          ""
            "Not a subtype, universes are not equal for singleton"
            (u : Syntax.Universe.t)
            (v : Syntax.Universe.t)];
    equivalent_value st e1 e2 (Value_univ u)
  | Value_ty_mod { binder = binder1 }, Value_ty_mod { binder = binder2 } ->
    Syntax.Value_ty_mod_binder.unpack
      binder1
      ~f:(fun { var = var1; ty_decls = decls1; ty_decls_map = _ } ->
        Syntax.Value_ty_mod_binder.unpack
          binder2
          ~f:(fun { var = var2; ty_decls = decls2; ty_decls_map = _ } ->
            let zipped_decls =
              match List.zip decls1 decls2 with
              | Ok t -> t
              | Unequal_lengths ->
                fail_s
                  [%message
                    "different number of declarations"
                      (decls1 : Syntax.value_ty_decl list)
                      (decls2 : Syntax.value_ty_decl list)]
            in
            let st = State.add_mod_var var1 st |> State.add_mod_var var2 in
            let _ =
              List.fold zipped_decls ~init:st ~f:(fun st (decl1, decl2) ->
                subtype st decl1.ty decl2.ty;
                let st = State.add_field var1 decl1 st in
                let st =
                  State.add_field
                    var2
                    { field = decl2.field
                    ; ty =
                        Value_ty_sing
                          { e =
                              Value_ty
                                (Value_path
                                   (Path_proj
                                      { mod_e = Path_var (Mod_var var1)
                                      ; field = decl1.field
                                      }))
                          ; ty = decl2.ty
                          }
                    }
                    st
                in
                st)
            in
            ()))
  | ( Value_ty_fun { binder = binder1; purity = purity1 }
    , Value_ty_fun { binder = binder2; purity = purity2 } ) ->
    if Purity.(purity1 > purity2)
    then
      fail_s
        [%message "purity was not a subtype" (purity1 : Purity.t) (purity2 : Purity.t)];
    Syntax.Value_ty_fun_binder.unpack
      binder1
      ~f:(fun { params = params1; body_ty = ty1 } ->
        Syntax.Value_ty_fun_binder.unpack
          binder2
          ~f:(fun { params = params2; body_ty = ty2 } ->
            let zipped_params =
              match List.zip params1 params2 with
              | Ok t -> t
              | Unequal_lengths ->
                fail_s
                  [%message
                    "different number of parameters"
                      (params1 : Syntax.value_param list)
                      (params2 : Syntax.value_param list)]
            in
            let st =
              List.fold zipped_params ~init:st ~f:(fun st (param1, param2) ->
                subtype st param2.ty param1.ty;
                let st =
                  State.add param2.var param2.ty st
                  |> State.add
                       param1.var
                       (Value_ty_sing
                          { e = Value_ty (Value_path (Path_var (Var param2.var)))
                          ; ty = param1.ty
                          })
                in
                st)
            in
            subtype st ty1 ty2;
            ()))
  | _ -> fail_s [%message "Not subtype" (ty1 : Syntax.ty) (ty2 : Syntax.ty)]

and equivalent_ty st (e1 : Syntax.ty) (e2 : Syntax.ty) (ty : Syntax.ty) : unit =
  equivalent_value st (Value_ty e1) (Value_ty e2) ty

(*
precondition: e1 and e2 must both have type ty
Checks whether the types e1 and e2 are equivalent at type ty.
*)
and equivalent_value st (e1 : Syntax.value) (e2 : Syntax.value) (ty : Syntax.ty) : unit =
  match force st ty with
  | Value_ty_sing _ | Value_core_ty _ -> ()
  | Value_path _ ->
    let u = get_univ_exn st ty in
    (match u with
     | Type -> ()
     | Kind | Sig ->
       let _k =
         structure_equivalent st (Syntax.Value.get_ty_exn e1) (Syntax.Value.get_ty_exn e2)
       in
       ())
  | Value_univ _ ->
    let _k =
      structure_equivalent st (Syntax.Value.get_ty_exn e1) (Syntax.Value.get_ty_exn e2)
    in
    ()
  | Value_ty_fun { binder = _; purity = Impure } -> ()
  | Value_ty_fun { binder; purity = Pure } ->
    Syntax.Value_ty_fun_binder.unpack binder ~f:(fun { params; body_ty } ->
      let args = List.map params ~f:(fun param -> Syntax.Value.var param.var) in
      let st = State.add_params params st in
      let e1 = Syntax.app_value_exn e1 args in
      let e2 = Syntax.app_value_exn e2 args in
      equivalent_value st e1 e2 body_ty;
      ())
  | Value_ty_mod { binder } ->
    Syntax.Value_ty_mod_binder.unpack
      binder
      ~f:(fun { var; ty_decls; ty_decls_map = _ } ->
        let st = State.add_mod_var var st in
        let _ =
          List.fold ty_decls ~init:st ~f:(fun st ({ field; ty } as decl) ->
            let e1 = Syntax.proj_value_exn e1 field in
            let e2 = Syntax.proj_value_exn e2 field in
            equivalent_value st e1 e2 ty;
            State.add_field var decl st)
        in
        ())

(*
checks whether ty1 and ty2 are structurally equivalent.
Then returns the natural kind that they are structurally equivalent at.
*)
and structure_equivalent st (e1 : Syntax.ty) (e2 : Syntax.ty) : Syntax.ty =
  match force st e1, force st e2 with
  | Value_path path1, Value_path path2 -> structure_equivalent_path st path1 path2
  | Value_core_ty ty1, Value_core_ty ty2 ->
    if not (Syntax.equal_core_ty ty1 ty2)
    then
      fail_s
        [%message
          "core types are not structurally equivalent"
            (ty1 : Syntax.core_ty)
            (ty2 : Syntax.core_ty)];
    Syntax.Value_univ Type
  | Value_univ u1, Value_univ u2 ->
    if not (Syntax.Universe.equal u1 u2)
    then
      fail_s
        [%message
          "universes are not structurally equivalent"
            (u1 : Syntax.Universe.t)
            (u2 : Syntax.Universe.t)];
    Syntax.Value_univ (Syntax.Universe.incr_exn u1)
  | Value_ty_sing { e = e1; ty = ty1 }, Value_ty_sing { e = e2; ty = ty2 } ->
    let kind = structure_equivalent st ty1 ty2 in
    equivalent_value st e1 e2 ty1;
    kind
  | ( Value_ty_fun { binder = binder1; purity = purity1 }
    , Value_ty_fun { binder = binder2; purity = purity2 } ) ->
    if not (Purity.equal purity1 purity2)
    then
      fail_s
        [%message
          "function purities are not structurally equivalent"
            (purity1 : Purity.t)
            (purity2 : Purity.t)];
    Syntax.Value_ty_fun_binder.unpack
      binder1
      ~f:(fun { params = params1; body_ty = ty1 } ->
        Syntax.Value_ty_fun_binder.unpack
          binder2
          ~f:(fun { params = params2; body_ty = ty2 } ->
            let zipped_params =
              match List.zip params1 params2 with
              | Ok t -> t
              | Unequal_lengths ->
                fail_s
                  [%message
                    "different number of parameters in function types"
                      (params1 : Syntax.value_param list)
                      (params2 : Syntax.value_param list)]
            in
            let st, universe =
              List.fold
                zipped_params
                ~init:(st, Syntax.Universe.minimum)
                ~f:(fun (st, univ1) (param1, param2) ->
                  let kind = structure_equivalent st param1.ty param2.ty in
                  let univ2 = Syntax.Value.get_ty_univ_exn kind in
                  let st =
                    State.add param1.var param1.ty st
                    |> State.add
                         param2.var
                         (Syntax.Value_ty_sing
                            { e = Syntax.Value.var param1.var; ty = param1.ty })
                  in
                  st, Syntax.Universe.max univ1 univ2)
            in
            let ret_kind = structure_equivalent st ty1 ty2 in
            let ret_univ = Syntax.Value.get_ty_univ_exn ret_kind in
            let univ = Syntax.Universe.max universe ret_univ in
            Syntax.Value_univ univ))
  | Value_ty_mod { binder = binder1 }, Value_ty_mod { binder = binder2 } ->
    Syntax.Value_ty_mod_binder.unpack
      binder1
      ~f:(fun { var = var1; ty_decls = decls1; ty_decls_map = _ } ->
        Syntax.Value_ty_mod_binder.unpack
          binder2
          ~f:(fun { var = var2; ty_decls = decls2; ty_decls_map = _ } ->
            let zipped_decls =
              match List.zip decls1 decls2 with
              | Ok t -> t
              | Unequal_lengths ->
                fail_s
                  [%message
                    "different number of declarations in module types"
                      (decls1 : Syntax.value_ty_decl list)
                      (decls2 : Syntax.value_ty_decl list)]
            in
            let st = State.add_mod_var var1 st in
            let st = State.add_mod_var var2 st in
            let _, universe =
              List.fold
                zipped_decls
                ~init:(st, Syntax.Universe.minimum)
                ~f:(fun (st, u) (decl1, decl2) ->
                  if not (String.equal decl1.field decl2.field)
                  then
                    fail_s
                      [%message
                        "module type fields are not equal"
                          (decl1.field : string)
                          (decl2.field : string)];
                  let kind = structure_equivalent st decl1.ty decl2.ty in
                  let v = Syntax.Value.get_ty_univ_exn kind in
                  let st = State.add_field var1 decl1 st in
                  let st =
                    State.add_field
                      var2
                      { field = decl1.field
                      ; ty =
                          Value_ty_sing
                            { e =
                                Value_ty
                                  (Value_path
                                     (Path_proj
                                        { mod_e = Path_var (Mod_var var1)
                                        ; field = decl1.field
                                        }))
                            ; ty = decl2.ty
                            }
                      }
                      st
                  in
                  st, Syntax.Universe.max u v)
            in
            Syntax.Value_univ universe))
  | _, _ ->
    fail_s
      [%message "types are not structurally equivalent" (e1 : Syntax.ty) (e2 : Syntax.ty)]

and structure_equivalent_path st (path1 : Syntax.path) (path2 : Syntax.path) : Syntax.ty =
  match path1, path2 with
  | Path_var var1, Path_var var2 ->
    if not (Syntax.Cvar.equal var1 var2)
    then fail_s [%message "Variables were not equal" (var1 : Cvar.t) (var2 : Cvar.t)];
    var_natural_ty_exn st var1
  | Path_app { func = func1; args = args1 }, Path_app { func = func2; args = args2 } ->
    let ty_fun =
      structure_equivalent_path st func1 func2 |> force st |> Syntax.Value.get_ty_fun_exn
    in
    assert (Purity.equal ty_fun.purity Pure);
    (* essentially path_app_natural_kind, but we don't use it here because we also have to check for equivalence of argument values *)
    Syntax.Value_ty_fun_binder.unpack
      ty_fun.binder
      ~f:(fun ({ params; body_ty } : Syntax.Value_ty_fun_binder.data) ->
        let st, subst =
          List.zip_exn (List.zip_exn args1 args2) params
          |> List.fold
               ~init:(st, Syntax.Subst.empty)
               ~f:(fun (st, subst) ((arg1, arg2), param) ->
                 equivalent_value
                   st
                   arg1
                   arg2
                   (Syntax.Ty.eval subst param.ty |> Syntax.Value.get_ty_exn);
                 let st = State.add_param param st in
                 let subst = Syntax.Subst.add_exn subst (Var param.var) arg1 in
                 st, subst)
        in
        ignore st;
        Syntax.Ty.eval subst body_ty |> Syntax.Value.get_ty_exn)
  | ( Path_proj { mod_e = mod_e1; field = field1 }
    , Path_proj { mod_e = mod_e2; field = field2 } ) ->
    let kind =
      structure_equivalent_path st mod_e1 mod_e2
      |> force st
      |> Syntax.Value.get_ty_mod_exn
    in
    if not (String.equal field1 field2)
    then fail_s [%message "Fields were not equal" (field1 : string) (field2 : string)];
    path_proj_natural_kind mod_e1 kind field1
  | _, _ ->
    fail_s
      [%message
        "paths not structurally equivalent" (path1 : Syntax.path) (path2 : Syntax.path)]
;;

let rec infer st (e : Syntax.expr) : Effects.t * Syntax.ty =
  match e with
  | Syntax.Expr_var var -> Effects.empty, var_strengthened_ty st var
  | Syntax.Expr_universe u ->
    ( Effects.empty
    , Value_ty_sing
        { e = Value_ty (Value_univ u); ty = Value_univ (Syntax.Universe.incr_exn u) } )
  | Syntax.Expr_seal { e; ty = ty2 } ->
    let eff, ty1 = infer st e in
    let vars, eff = Effects.get_vars eff in
    let ty2 = expr_to_ty st ty2 in
    let st = State.add_list vars st in
    subtype st ty1 ty2;
    Effects.of_purity eff.purity, ty2
  | Syntax.Expr_app { func; args } ->
    let func_ty =
      infer_force_transparent st func |> force st |> Syntax.Value.get_ty_fun_exn
    in
    let arg_tys = List.map args ~f:(fun e -> infer_force_transparent st e) in
    Syntax.Value_ty_fun_binder.unpack func_ty.binder ~f:(fun { params; body_ty } ->
      let tys_and_params =
        match List.zip arg_tys params with
        | Ok t -> t
        | Unequal_lengths ->
          fail_s
            [%message
              "Invalid number of parameters passed"
                ~expected:(List.length params : int)
                ~actual:(List.length arg_tys : int)]
      in
      let subst =
        List.fold tys_and_params ~init:Syntax.Subst.empty ~f:(fun subst (ty, param) ->
          subtype st ty (Syntax.Ty.eval subst param.ty |> Syntax.Value.get_ty_exn);
          let value = synthesize_value st ty in
          Syntax.Subst.add_exn subst (Var param.var) value)
      in
      let res_eff = Effects.of_purity func_ty.purity in
      let res_ty = Syntax.Ty.eval subst body_ty |> Syntax.Value.get_ty_exn in
      res_eff, res_ty)
  | Syntax.Expr_abs { params; body; purity } ->
    let st', params =
      List.fold_map
        ~init:st
        ~f:(fun st param ->
          let param = infer_param st param in
          State.add param.var param.ty st, param)
        params
    in
    let body_eff, body_ty = infer st' body in
    let body_eff_vars, body_eff = Effects.get_vars body_eff in
    if Purity.(body_eff.purity > purity)
    then
      fail_s
        [%message
          "Unexpected purity"
            ~expected:(purity : Purity.t)
            ~actual:(body_eff.purity : Purity.t)];
    begin match body_eff.purity with
    | Impure ->
      if not (List.is_empty body_eff_vars)
      then
        fail_s
          [%message
            "Cannot generate hidden existential types in impure functor"
              ~vars:(body_eff_vars : (Var.t * Syntax.ty) list)];
      ( Effects.empty
      , Syntax.Value_ty_fun
          { binder = Syntax.Value_ty_fun_binder.pack { params; body_ty }
          ; purity = Impure
          } )
    | Pure ->
      let lifted_body_eff_vars =
        List.map body_eff_vars ~f:(fun (var, ty) ->
          let ty' =
            Syntax.Value_ty_fun
              { binder = Syntax.Value_ty_fun_binder.pack { params; body_ty = ty }
              ; purity = Pure
              }
          in
          var, Syntax.Var.make_fresh var, ty')
      in
      let app_args = List.map params ~f:(fun p -> Syntax.Value.var p.var) in
      let subst =
        List.map lifted_body_eff_vars ~f:(fun (var1, var2, _ty) ->
          ( Cvar.Var var1
          , Syntax.Value_ty
              (Syntax.Value_path
                 (Syntax.Path_app { func = Path_var (Var var2); args = app_args })) ))
        |> Syntax.Subst.of_alist_exn
      in
      let res_ty =
        Syntax.Value_ty_fun
          { binder =
              Syntax.Value_ty_fun_binder.pack
                { params
                ; body_ty = Syntax.Ty.eval subst body_ty |> Syntax.Value.get_ty_exn
                }
          ; purity = Pure
          }
      in
      let eff_vars =
        List.map lifted_body_eff_vars ~f:(fun (_var1, var2, ty) -> var2, ty)
      in
      let res_eff = { Effects.vars = Acc.of_list eff_vars; purity = Pure } in
      res_eff, res_ty
    end
  | Syntax.Expr_let { var; rhs; body } ->
    let rhs_eff, rhs_ty = infer st rhs in
    let vars, rhs_eff = Effects.get_vars rhs_eff in
    let st = State.add_list vars st in
    let body_eff, body_ty = infer st body in
    if is_ty_transparent st rhs_ty
    then begin
      let value = synthesize_value st rhs_ty in
      let subst = Syntax.Subst.singleton (Var var) value in
      let res_ty = Syntax.Ty.eval subst body_ty |> Syntax.Value.get_ty_exn in
      rhs_eff ++ body_eff, res_ty
    end
    else rhs_eff ++ Effects.of_var (var, rhs_ty) ++ body_eff, body_ty
  | Syntax.Expr_proj { mod_e; field } ->
    let e_ty = infer_force_transparent st mod_e in
    let ({ binder } : Syntax.value_ty_mod) =
      Syntax.Value.get_ty_mod_exn (force st e_ty)
    in
    Syntax.Value_ty_mod_binder.unpack binder ~f:(fun ty_mod ->
      let decl =
        List.find ty_mod.ty_decls ~f:(fun ty_decl -> String.equal ty_decl.field field)
        |> Option.value_or_thunk ~default:(fun () ->
          fail_s
            [%message
              "Field not found"
                ~ty:(ty_mod : Syntax.Value_ty_mod_binder.data)
                (field : string)])
      in
      Effects.empty, decl.ty)
  | Syntax.Expr_mod { var; decls } ->
    begin match String.Map.of_list_with_key decls ~get_key:(fun decl -> decl.field) with
    | `Ok _ -> ()
    | `Duplicate_key field -> fail_s [%message "Duplicate field" (field : string)]
    end;
    let (st, eff), ty_decls =
      List.fold_map
        decls
        ~init:(State.add_mod_var var st, Effects.empty)
        ~f:(fun (st, acc_eff) { e; field } ->
          let eff, ty = infer st e in
          let vars, eff = Effects.get_vars eff in
          let ty_decl : Syntax.value_ty_decl = { field; ty } in
          let st = State.add_list vars st in
          let st = State.add_field var ty_decl st in
          (st, acc_eff ++ eff), ty_decl)
    in
    ignore st;
    let ty_decls_map =
      String.Map.of_list_with_key_exn ty_decls ~get_key:(fun decl -> decl.field)
    in
    ( eff
    , Syntax.Value_ty_mod
        { binder = Syntax.Value_ty_mod_binder.pack { var; ty_decls; ty_decls_map } } )
  | Syntax.Expr_ty_fun { params; body_ty; purity } ->
    let st, params =
      List.fold_map params ~init:st ~f:(fun st param ->
        let param = infer_param st param in
        State.add param.var param.ty st, param)
    in
    let body_ty = expr_to_ty st body_ty in
    let ty_fun : Syntax.value_ty_fun =
      { binder = Syntax.Value_ty_fun_binder.pack { params; body_ty }; purity }
    in
    let kind = ty_natural_kind st (Value_ty_fun ty_fun) in
    Effects.empty, Syntax.Value_ty_sing { e = Value_ty (Value_ty_fun ty_fun); ty = kind }
  | Syntax.Expr_ty_mod { var; ty_decls } ->
    let st, ty_decls =
      List.fold_map ty_decls ~init:(State.add_mod_var var st) ~f:(fun st { field; ty } ->
        let ty = expr_to_ty st ty in
        let ty_decl = ({ field; ty } : Syntax.value_ty_decl) in
        let st = State.add_field var ty_decl st in
        st, ty_decl)
    in
    let ty_decls_map =
      String.Map.of_list_with_key_exn ty_decls ~get_key:(fun decl -> decl.field)
    in
    let ty_mod : Syntax.value_ty_mod =
      { binder = Syntax.Value_ty_mod_binder.pack { var; ty_decls; ty_decls_map } }
    in
    let kind = ty_natural_kind st (Value_ty_mod ty_mod) in
    Effects.empty, Syntax.Value_ty_sing { e = Value_ty (Value_ty_mod ty_mod); ty = kind }
  | Syntax.Expr_ty_sing { e; ty } ->
    let e_ty = infer_force_transparent st e in
    let ty = expr_to_ty st ty in
    let ty_sing : Syntax.value_ty_sing = { e = synthesize_value st e_ty; ty } in
    let kind = ty_natural_kind st (Value_ty_sing ty_sing) in
    ( Effects.empty
    , Syntax.Value_ty_sing { e = Value_ty (Value_ty_sing ty_sing); ty = kind } )
  | Syntax.Expr_bool _ -> Effects.empty, Syntax.Value_core_ty Ty_bool
  | Syntax.Expr_unit -> Effects.empty, Syntax.Value_core_ty Ty_unit
  | Syntax.Expr_core_ty core_ty ->
    ( Effects.empty
    , Syntax.Value_ty_sing { e = Value_ty (Value_core_ty core_ty); ty = Value_univ Type }
    )
  | Syntax.Expr_if { cond; body1; body2 } ->
    let eff1, cond_ty = infer st cond in
    subtype st cond_ty (Value_core_ty Ty_bool);
    let eff2, ty2 = infer st body1 in
    let eff3, ty3 = infer st body2 in
    let k2 = ty_natural_kind st (force st ty2) in
    let k3 = ty_natural_kind st (force st ty3) in
    let u2 = Syntax.Value.get_ty_univ_exn k2 in
    let u3 = Syntax.Value.get_ty_univ_exn k3 in
    if not (Syntax.Universe.equal u2 u3)
    then
      fail_s
        [%message
          "Different universes in if branches"
            (u2 : Syntax.Universe.t)
            (u3 : Syntax.Universe.t)];
    equivalent_ty st ty2 ty3 k2;
    eff1 ++ eff2 ++ eff3, ty2

(* TODO: do some bidirectional stuff here *)
and check st (e : Syntax.expr) (ty : Syntax.ty) : Effects.t =
  match e, ty with
  | _ ->
    let eff, ty' = infer st e in
    let vars, eff = Effects.get_vars eff in
    let st = State.add_list vars st in
    subtype st ty' ty;
    eff

and infer_param st (param : Syntax.expr_param) : Syntax.value_param =
  let ty = expr_to_ty st param.ty in
  { var = param.var; ty }

and infer_force_transparent st (expr : Syntax.expr) : Syntax.ty =
  let eff, ty = infer st expr in
  let vars, _eff = Effects.get_vars eff in
  if not (List.is_empty vars && is_ty_transparent st ty)
  then fail_s [%message "Expected a transparent type"];
  ty

and kind_to_universe st (kind : Syntax.ty) : Syntax.Universe.t option =
  match force st kind with
  | Value_univ u -> Some u
  | Value_ty_sing { e = _; ty = Value_univ u } -> Some u
  | _ -> None

and expr_to_ty st (ty : Syntax.expr) : Syntax.ty =
  let kind = infer_force_transparent st ty in
  begin match kind_to_universe st kind with
  | None -> fail_s [%message "Expected a path" ~actual:(ty : Syntax.expr)]
  | Some _ -> ()
  end;
  let v = synthesize_value st kind in
  Syntax.Value.get_ty_exn v
;;
