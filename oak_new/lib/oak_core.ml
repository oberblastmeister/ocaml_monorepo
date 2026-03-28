open Prelude
module Syntax = Oak_core_syntax
module Evaluate = Oak_core_evaluate
module Core_utils = Oak_core_utils
module Common = Oak_common
module Icit = Common.Icit
module Level = Common.Level
module Index = Common.Index
module Name = Common.Name
module Relevancy = Common.Relevancy
module Environments = Oak_core_environments

type ty_props = Syntax.ty_props [@@deriving sexp_of]
type term = Syntax.term [@@deriving sexp_of]
type term_data = Syntax.term_data [@@deriving sexp_of]
type term_data_rec = Syntax.term_data_rec [@@deriving sexp_of]
type term_data_decl = Syntax.term_data_decl [@@deriving sexp_of]
type term_data_param = Syntax.term_data_param [@@deriving sexp_of]
type term_data_body = Syntax.term_data_body [@@deriving sexp_of]
type term_data_field = Syntax.term_data_field [@@deriving sexp_of]
type term_data_constructor = Syntax.term_data_constructor [@@deriving sexp_of]
type field_loc = Syntax.field_loc [@@deriving sexp_of]
type term_field_impl = Syntax.term_field_impl [@@deriving sexp_of]
type term_field_spec = Syntax.term_field_spec [@@deriving sexp_of]
type term_ty = Syntax.term_ty [@@deriving sexp_of]
type term_ty_struct = Syntax.term_ty_struct [@@deriving sexp_of]
type value = Syntax.value [@@deriving sexp_of]
type ty = Syntax.ty [@@deriving sexp_of]
type value_data_rec = Syntax.value_data_rec [@@deriving sexp_of]
type value_data_decl = Syntax.value_data_decl [@@deriving sexp_of]
type value_data = Syntax.value_data [@@deriving sexp_of]
type ty_sing = Syntax.ty_sing [@@deriving sexp_of]
type ty_struct = Syntax.ty_struct [@@deriving sexp_of]
type head = Syntax.head [@@deriving sexp_of]
type neutral = Syntax.neutral [@@deriving sexp_of]
type spine = Syntax.spine [@@deriving sexp_of]
type term_arg = Syntax.term_arg [@@deriving sexp_of]
type value_arg = Syntax.value_arg [@@deriving sexp_of]
type frame = Syntax.frame [@@deriving sexp_of]
type value_struct = Syntax.value_struct [@@deriving sexp_of]
type value_fun = Syntax.value_fun [@@deriving sexp_of]
type ty_fun = Syntax.ty_fun [@@deriving sexp_of]
type ty_closure = Syntax.ty_closure [@@deriving sexp_of]
type value_closure = Syntax.value_closure [@@deriving sexp_of]
type value_field_impl = Syntax.value_field_impl [@@deriving sexp_of]
type value_field_spec = Syntax.value_field_spec [@@deriving sexp_of]

module type ENV = Environments.ENV

module Make_seq_env (Value : sig
    type t [@@deriving sexp_of]
  end) : ENV with type value = Value.t and type t = Value.t Syntax.Env.t = struct
  type value = Value.t [@@deriving sexp_of]
  type t = value Syntax.Env.t [@@deriving sexp_of]

  let empty = Syntax.Env.empty
  let push = Syntax.Env.push
  let pop = Syntax.Env.pop
  let pop_exn = Syntax.Env.pop_exn
  let get_index = Syntax.Env.get_index
  let get_level = Syntax.Env.get_level
  let get_index_exn = Syntax.Env.get_index_exn
  let get_level_exn = Syntax.Env.get_level_exn
  let iter = Syntax.Env.iter
  let to_list = Syntax.Env.to_list
  let of_list = Syntax.Env.of_list
  let length = Syntax.Env.length
end

module Value_env : ENV with type value = value and type t = Syntax.env =
Make_seq_env (struct
    type t = value [@@deriving sexp_of]
  end)

module Ty_env : ENV with type value = ty and type t = Syntax.ty_env = Make_seq_env (struct
    type t = ty [@@deriving sexp_of]
  end)

module Name_env = Environments.Name_env
module Erased_env = Environments.Erased_env
module Erased_env_ENV = Environments.Erased_env_ENV

type value_env = Value_env.t [@@deriving sexp_of]
type ty_env = Ty_env.t [@@deriving sexp_of]
type name_env = Name_env.t [@@deriving sexp_of]
type erased_env = Erased_env.t [@@deriving sexp_of]

module Close = Evaluate.Close

module Ty_props = struct
  type t = ty_props [@@deriving sexp_of]
end

module Field_loc = struct
  type t = field_loc [@@deriving sexp_of]

  let create = Syntax.Field_loc.create
end

module Term = struct
  type t = term [@@deriving sexp_of]

  let of_level = Syntax.Term.of_level
  let close = Evaluate.Term.close
  let close_single = Evaluate.Term.close_single
  let eval = Evaluate.Term.eval
end

module Term_ty = struct
  type t = term_ty [@@deriving sexp_of]

  let close = Evaluate.Term_ty.close
  let close_single = Evaluate.Term_ty.close_single
  let eval = Evaluate.Term_ty.eval
end

module Value_field_impl = struct
  type t = value_field_impl [@@deriving sexp_of]

  let create = Syntax.Value_field_impl.create
end

module Value = struct
  type t = value [@@deriving sexp_of]

  let create_struct = Syntax.Value.create_struct
  let whnf = Evaluate.Value.whnf
  let free = Syntax.Value.free
  let free_of_size = Syntax.Value.free_of_size
  let neutral_val_exn = Syntax.Value.neutral_val_exn
  let quote = Evaluate.Value.quote
  let proj = Evaluate.Value.proj
  let app = Evaluate.Value.app
  let out = Evaluate.Value.out
  let decode = Evaluate.Value.decode
end

module Ty = struct
  type t = ty [@@deriving sexp_of]

  let infer_props = Evaluate.Ty.infer_props
  let whnf = Evaluate.Ty.whnf
  let ty_fun_val_exn = Syntax.Ty.ty_fun_val_exn
  let ty_struct_val_exn = Syntax.Ty.ty_struct_val_exn
  let ty_sing_val_exn = Syntax.Ty.ty_sing_val_exn
  let ty_universe_val_exn = Syntax.Ty.ty_universe_val_exn
  let quote = Evaluate.Ty.quote
  let proj = Evaluate.Ty.proj
  let app = Evaluate.Ty.app
  let out = Evaluate.Ty.out
end

module Neutral = struct
  type t = neutral [@@deriving sexp_of]

  let infer_ty = Evaluate.Neutral.infer_ty
  let infer_universe = Evaluate.Neutral.infer_universe
  let whnf = Evaluate.Neutral.whnf
end

module Head = struct
  type t = head [@@deriving sexp_of]

  let infer_ty = Evaluate.Head.infer_ty
end

module Term_ty_struct = struct
  type t = term_ty_struct [@@deriving sexp_of]

  let of_iterated_binders = Core_utils.struct_ty_of_iterated_binders
end

module Struct = struct
  type t = value_struct [@@deriving sexp_of]

  let proj = Evaluate.Struct.proj
end

module Ty_struct = struct
  type t = ty_struct [@@deriving sexp_of]

  let of_iterated_binders field_specs : t =
    { env = Syntax.Env.empty
    ; field_specs = (Term_ty_struct.of_iterated_binders field_specs).field_specs
    }
  ;;

  let field_locations = Syntax.Ty_struct.field_locations
  let proj = Evaluate.Ty_struct.proj
end

module Fun = struct
  type t = value_fun [@@deriving sexp_of]

  let app = Evaluate.Fun.app
end

module Ty_fun = struct
  type t = ty_fun [@@deriving sexp_of]

  let app = Evaluate.Ty_fun.app
end
