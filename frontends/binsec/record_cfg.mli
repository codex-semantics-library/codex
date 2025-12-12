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


(** A module to record a set of traces. Note: imperative interface for now. *)
module type S = sig
  val dump_html_fun: (unit -> unit) option ref
  val wto: Cfg_analysis.Cfg.V.t Fixpoint.Wto.component list ref
  type t

  val nb_forks: t -> int
  val init: Loader.Img.t -> Virtual_address.t -> t
  val reuse: t -> t
    (** Reopens the file handles and empty the work list that keeps track of
        forks. The trace recorded is unchanged. *)

  type context_change =
  | NoChange (** We remained in the same function. *)
  | EnteredFunction of string (** We entered a new function. *)
  | LeftFunction of string (** We left the current function, possibly more. *)

  exception Visited_vertex

  (** [next trace addr] adds the code address [addr] to the current
     trace [trace]. It returns a value of type [context_change] to express how
     the call stack is affected (according to our heuristic). If [addr] has
     been visited before, [next trace addr] raises the exception
     [Visited_vertex], unless [exploration_only] is set. *)
  val next: t -> exploration_only:bool -> record_cfg:bool -> Virtual_address.t -> unit
  val set_position : t -> keep_forks:bool -> Cfg_analysis.Cfg.V.t -> unit
    (** Set the current position. Overwrites the work list that keeps track of
        forks. This should only be used before starting a depth-first analysis
        at a given address, never during analysis.  *)

  (** May raise [Not_found] is [next] was never called. *)
  val current_position : t -> Cfg_analysis.V.t

  val instruction_graph : t -> Cfg_analysis.Cfg.t

  val call_stack_after_jump : t -> Cfg_analysis.Cfg.V.t -> Virtual_address.t ->
    Cfg_analysis.V.call_stack * context_change
    (** Given the current state and a destination address, return the call
        stack after a jump to that destination. The call stack reconstruction
        assumes that the symbols in the code represent the actual functions of
        every instruction. *)

  val start_fork: t -> unit
  val next_fork: t -> unit
  val end_fork: t -> unit
  val close: t -> ret_addr:Virtual_address.t -> Virtual_address.t
             -> graph_filename:string
             -> html_filename:string option
             -> (string, string) Hashtbl.t
             -> Loader.Img.t
             -> Cfg_analysis.Cfg.t
    (** [close tr start] computes the CFG (i.e. the graph of basic blocks) from
        the instruction graph, starting at address [start] using the record
        trace [tr]. The CFG is written to a Graphviz file, as well as an UML
        sequence diagram. *)

  val graph_changed : t -> bool
  val set_graph_changed : t -> bool -> unit

  (** Back edges are a relative notion. This tells whether the edge is a back
     edge relatively to the depth-first search memorized by [trace]. *)
  val back_edge : trace:t -> Cfg_analysis.Cfg.V.t -> Cfg_analysis.Cfg.V.t -> bool

  val visited_instructions : t -> Virtual_address.Set.t
end

module Make (State : Dba2Codex.StateS) : S
