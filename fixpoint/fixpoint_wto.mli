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

(** Forward fixpoint computation of an abstract domain over a WTO
    (which is not memory-efficient). *)

(** Signature for the analysis, which is a standard abstract
    interpretation ({{: https://doi.org/10.1145/512950.512973}Cousot and Cousot, POPL'77}) *)
module type AbstractDomain = sig

  (** An abstract domain is a lattice representing abstract states
      [state]. *)
  type state

  (** The syntax for transiton between program points.  *)
  type transition

  (** An identifier for the loop id. Can be unit if unused; it can be
      used as a widening ID in the symbolic expression domain
      ({{: https://codex.top/papers/2023-popl-ssa-translation-is-an-abstract-interpretation.html}Lemerre, POPL'23}; {{: https://codex.top/papers/2024-pldi-compiling-with-abstract-interpretation.html}Lesbre & Lemerre, PLDI'24}). *)
  type loop_id


  (** Create an independant copy of the state, not sharing any mutable data *)
  val copy: state -> state

  (** Standard join operation, over-approximating union. *)
  val join: state -> state -> state

  (** [fixpoint_step ~loop_id previous next] detects whether fixpoint
      computation is over (then it returns true and the final abstract
      state), or it tries to extrapolates (widen) from the evolution
      from [previous] to [next] (returning false and an widened
      state). *)
  val fixpoint_step: loop_id:loop_id -> state -> state -> (state * bool)

  (** Standard transfer function. The transition returns None if the state is bottom *)
  val transfer: transition -> state -> state option

  (** Pretty printer used for debugging. *)
  val pp: Format.formatter -> state -> unit

  (** Enter loop, providing a new identifier for the loop. *)
  val enter_loop: state -> state * loop_id

  (** Leave loop; useful e.g. for induction variable analysis, that
      introduces variable counting the number of loop iterations. *)
  val leave_loop: state -> state

end


(** Signature for the directed graph on which we do the fixpoint computation. *)
module type Graph = sig
  type transition
  module ControlLocation:PatriciaTree.KEY
  val preds_with_transition: ControlLocation.t -> (ControlLocation.t * transition) list
end


(** Given a graph, an abstract domain, a WTO computation on this
    graph, and an initial mapping from locations to pre-state, compute
    the final mapping from all reachable locations to their
    pre-state. Note that it is better if the input node is not part of
    the WTO. *)
module Make(G : Graph)(D : AbstractDomain with type transition = G.transition):sig
  module CLMap : module type of PatriciaTree.MakeMap(G.ControlLocation)
  val fixpoint_partition: D.state CLMap.t -> G.ControlLocation.t Wto.component list -> D.state CLMap.t
end
