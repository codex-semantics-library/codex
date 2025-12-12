(**************************************************************************)
(*  This file is part of the Codex semantics library.                     *)
(*                                                                        *)
(*  Copyright (C) 2013-2025                                               *)
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

open Frama_c_kernel
open Units
module VarMap : module type of Codex.Extstdlib.Map.Make(Cil_datatype.Varinfo);;
module StringMap : Map.S with type key = string

module type CallingContext = sig
  include Datatype_sig.S
  val get: unit -> t
end

(* module Make(CallingContext:CallingContext)(Domain:Codex.Domains.Domain_sig.Base):sig *)
module Make(CallingContext:CallingContext)(Domain:Codex.Domains.Memory_domains.With_focusing.S_with_types):sig

  (* Indicates that a pure function is being analyzed *)
  val exploring_pure_function : bool ref

  (* Representation of the value for each calling context. *)
  val exp_to_value: Cil_types.kinstr * Cil_types.exp -> string Hashtbl.Make(CallingContext).t

  (* Is the expression dead, i.e. the analysis never executed it? *)
  val exp_has_value: Cil_types.kinstr * Cil_types.exp -> bool

  (** Iterate on all registered alarms. *)
  val iter_on_alarms: (Alarms.t -> Cil_types.location -> Codex.Lattices.Quadrivalent.t -> unit) -> unit

  (** Delete all the registered alarms, use only if you know what you are doing*)
  val reset_alarms: unit -> unit

  (** Delete all the registered assertions, use only if you know what you are doing*)
  val reset_assertions: unit -> unit

  (** Iterate f on all registered assertions. f receives the location
     of the assertion, and strings indicating the status and method to
     solve the assertion. *)
  val iter_on_assertions: (Cil_types.location -> string -> string -> unit) -> unit

  (* The context in which an instruction is called, i.e. abstraction of the control flow. *)
  type context = {
    (* We follow a recursive iteration strategy. This is the number of loops in which we are iterating;
       0 means that we are not in a loop (and thus the results that we compute are definitive). *)
    loop_nesting_level: int;
    (* Current statement (or global if called outside of a statement). *)
    kinstr : Cil_types.kinstr;
    (* Analysis context, representing the current basic block in the current calling context. *)
    ctx: Domain.Context.t;
  }
  ;;

  (* A state is either bottom, or a tuple with a memory, a mapping
     from variable names to their addresses, and a mapping from
     strings to their addresses.*)
  type state = { mem: Domain.memory;
                 var_addresses : Domain.binary VarMap.t;
                 string_addresses: Domain.binary StringMap.t;
                 context:context;
               }

  (* Enter a Cil block (and allocate the local variables in it). *)
  val block_entry:  state -> Cil_types.block -> state

  (* Leave a Cil block (and deallocate the local variables in it). *)
  val block_close:  state -> Cil_types.block -> state

  (* Given a function, a list of arguments (with their size), and a
     state, provide a return value (with its size) and an output state (if not bottom). *)
  type funcall = Kernel_function.t -> (In_bits.t * Domain.binary) list -> state -> (In_bits.t * Domain.binary) option * state option

  (* val function_to_ctype: Kernel_function.t -> Types.TypedC.typ *)

  (* Analyze a function interproceduraly using only a function type (signature)
     this is used by function pointer and for analyzing specified function interproceduraly *)
  val analyze_summary: Types.TypedC.typ -> (In_bits.t * Domain.binary) list -> state -> (In_bits.t * Domain.binary) option * state


  (* Allocate and assign the formal arguments of a function, given a list of real
     arguments (with their size). *)
  val init_function_args: state -> Kernel_function.t -> (In_bits.t * Domain.binary) list -> state

  (* Deallocate the formal arguments of a function. *)
  val free_function_args: state -> Kernel_function.t -> state


  (* Note: state option = None means that the state is known to concretize into the empty set.  *)

  (* Analyzis of an instruction, with an argument to tell how function calls should be handled. *)
  val instruction: funcall:funcall -> Cil_types.stmt -> Cil_types.instr -> state -> state option

  (** Result of the analysis of a return instruction:
      - [Return_fails] means that the execution always trigger an undefined behaviour (e.g. return 1/0).
      - [Return_result{state;return_value=None}] is for [return; ] statements;
      - [Return_result{state;return_value=(size,value)}] is for [return value; ] statements, where size=8*sizeof(value)
        (i.e. the size of value in bits). *)
  type return_result =
    | Return_fails
    | Return_result of {return_state:state;return_value:(In_bits.t * Domain.binary) option}

  (** Transitions can be a regular instruction or a return instruction:
      - return instructions return a [return_result];
      - normal instruction return (State None) when the result is Bottom, or State(Some _) otherwise. *)
  type ret_transition =
    | State of state option
    | Return of return_result (* (state * (int * Domain.binary) option) option *)

  (* Analysis of an interpreted automata transition.
     Returns a state, but also the size and binary value of the return statement
     (if the transition is a return statement). *)
  val transition: funcall:funcall ->
    Interpreted_automata.vertex Interpreted_automata.transition -> state -> ret_transition

  (* Analyzis of an expression used in a boolean context (e.g. if condition). *)
  val cond_node: Cil_types.exp -> state -> (Domain.boolean * state) option

  (* Analyzis of an expression used in a value context. *)
  val expression: Cil_types.exp -> state -> (Domain.binary * state) option

  (** [initial_state_ret kf root] receives a kernel function and the
      root calling context, and returns the initial state, the
      arguments and the expected return type. *)
  val initial_state_ret: Kernel_function.t -> CallingContext.t ->
    state option * (In_bits.t * Domain.binary) list * Types.TypedC.typ

  (* Pretty-print the current state. *)
  val pretty_state: Format.formatter -> state -> unit
  val pretty_state_option: Format.formatter -> state option -> unit

end
