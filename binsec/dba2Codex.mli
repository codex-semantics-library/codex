(**************************************************************************)
(*  This file is part of the Codex semantics library.                     *)
(*                                                                        *)
(*  Copyright (C) 2013-2024                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file LICENSE).                      *)
(*                                                                        *)
(**************************************************************************)

module VarMap : Map.S with type key = string

type jump_target =
  | Jump_Inner of Dba.id (** Some instruction in a block *)
  | Jump_Outer of Virtual_address.t (** Some instruction outside of the block. *)
  | Jump_Dynamic              (* TODO *)

(*
module Constraints : Codex.Constraints_constraints_sig.Constraints
  with module Condition = Codex.Constraints.Condition.ConditionCudd
module Propag_domain : Codex.Constraint_domains_sig.Domain_S
  with module Constraints = Constraints
*)


module Ctypes = Types.Ctypes

module type Address_sig = sig
  type t
  val create : int -> t

  module Set : Stdlib.Set.S with type elt = t
  module Htbl : Stdlib.Hashtbl.S with type key = t
end

module type RegionS = sig
  module Virtual_address : Address_sig
  
  val written_data_addrs : Virtual_address.Set.t ref
  val read_data_addrs : Virtual_address.Set.t ref
  val set_untyped_load : bool -> unit
  val set_check_store_intvl : bool -> unit
  val set_param_typing : (Ctypes.typ * int) Virtual_address.Htbl.t -> unit
end



module type StateS = sig
  module Domain : Codex.With_focusing.S_with_types
  (*  with module Context = Weak_shape_domain.BR.Scalar.Context
    and module Type = Weak_shape_domain.Type *)
  module EvalPred : Codex.Type_domain.EvalPred.Sig
    with module Domain := Domain

  type t = {
    ctx: Domain.Context.t;
    vars: Domain.binary VarMap.t;
    memory: Domain.memory;
    instruction_count: int;
    is_bottom: bool;
    never_went_to_user_code : bool;
  }


  val initial : (Loader_elf.Img.t, 'a, 'b, 'c) Loader.t_pack -> Domain.Context.t ->
                t
  val initial_concrete : (Loader_elf.Img.t, 'a, 'b, 'c) Loader.t_pack
    (*-> (Loader_elf.Img.t, 'a, 'b) Loader.t_pack*) -> Domain.Context.t -> t
  val reset : (Loader_elf.Img.t, 'a, 'b, 'c) Loader.t_pack -> Domain.Context.t ->
              t
  val get : size:int -> t -> string -> Domain.binary
  val set : t -> string -> Domain.binary -> t
  val assume : Domain.boolean -> t -> t
  (* val assume_pred : size:int -> Codex.Types.Ctypes.Pred.t
    -> Weak_shape_domain.BR.binary -> t -> t *)
  val bottom : Domain.Context.t -> t
  val dump_state : Format.formatter -> t -> unit
  val dump_state_diff : Format.formatter -> t -> t -> Virtual_address.t -> (string, string) Hashtbl.t -> unit
  val join : t -> t -> t
  val is_included : t -> t -> bool

  (** Serialize a state's variables and memory into a tuple. [to_tuple ctx
      state] returns the result of serialization, along with an inversion
      function, to turn a tuple back into a state. This function takes the
      original state and a tuple as arguments, and update the state's [vars]
      and [memory] fields with the tuple's contents. *)
  val serialize: t -> t -> 'a Domain.Context.in_acc -> (t, 'a) Domain.Context.result
end


module Create():sig
  module Numeric_simple : Codex.Domain_sig.Base
  module Numeric : Codex.Domain_sig.Base
  module Domain : Codex.With_focusing.S_with_types
  module Region : RegionS with module Virtual_address := Virtual_address

  module EvalPred : Codex.Type_domain.EvalPred.Sig
    with module Domain := Domain

  module Make (Reg : Arch_settings.Registers with module Domain = Domain) : sig

    module State : StateS
      with module Domain = Domain

    val expr : Dba.Expr.t -> State.t -> (Domain.binary * State.t) option
    val instr : State.t -> Dba.Instr.t -> (jump_target * State.t) list

  end
end

