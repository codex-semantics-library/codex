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

module Addr_tbl : Hashtbl.S with type key = Virtual_address.t

(* Analysis hooks, builtin functions etc. *)
module Make(State : Dba2Codex.StateS)(Record_cfg : Record_cfg.S):sig

  type skip_type = NotWhenInterpreting | Always
  
  type hook =
    | SkipTo of skip_type * Virtual_address.t (** skip to address *)
    | Hook of (Record_cfg.t -> State.t ->
               Record_cfg.t * (Virtual_address.t * State.t) list)
    (** Manually change the transfer function of an instruction. *)
    | ChangeState of (State.t -> State.t)
    (** Modifies the state before executing the corresponding instruction. *)
    | Unroll of int
    (** Unroll every loop having this address as its head. Argument: number
        of iterations. *)
    | EndPath
    (** End this trace *)
    | Return of Types.Ctypes.typ option
    (** End this trace and check the return type if given *)
    | EntryCall of string * Types.Ctypes.typ
    (** Used during interprocedural analysis to enter the entry function, but should be replaced the first time it is encountered *)

  (** Finds a hook, or raise Not_found.  *)
  val find_hook: Virtual_address.t -> (hook * string)

  (** Says whether the analysis should be "merge over all paths" only. Initially false. *)
  val exploration_only : bool ref

  val kernel_exit_point : Virtual_address.t

  (** Add a hook to say that we should stop at this address. *)
  val add_stop: Virtual_address.t -> unit

  (** Add a hook to a function address to say that the function should return and just 
      return a value of some type. *)
  val add_return_unknown: Virtual_address.t -> Types.Ctypes.typ ->  unit

  (** Add a hook to a function address to say that the address does nothing, 
      instead it directly jumps to dest. *)
  val add_skip: Virtual_address.t -> dest:Virtual_address.t -> unit

  val add_entrycall: name:string -> Virtual_address.t -> Types.Ctypes.typ -> unit

  val add_function_hook : name:string -> Virtual_address.t -> Types.Ctypes.typ -> unit

  val add_return: Virtual_address.t -> Types.Ctypes.typ option -> unit

end
