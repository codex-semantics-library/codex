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


(** Produce graphs that are suitable and efficient for fixpoint
    computation, notably:

    - There is a distinction between the first node (that may be part
      of a loop) and input node (that holds the entry state of the
      program). This simplifes fixpoint computation algorithms,
      e.g. each processed node has a predecessor.

    - The produced data structure is computationally and memory
      efficient (everything is obtained by following pointers, no hash
      or map is used to traverse the graph).

    - Graph nodes have unique consecutively labelled integers as
    ids. This means that abstract states can be stored in arrays, or
    that Patricia trees would be well balanced in those.

*)


(** Signature for the input graph. We need the vertices to be hashed
    for traversal. In the algorithm, we also need the first node and a
    function returning the successor edge and nodes.  *)
module type Graph =
sig
  module Vertex : Hashtbl.HashedType
  type transition

  (** [transition_to_string] is used only for debugging purposes; it can return "". *)
  val transition_to_string: transition -> string

  (** A skip transition is one that does not change the state.  *)
  val skip : transition
end

module Make(G : Graph):sig

  (** Control locations are also vertices in the graph. We provide
      [to_int] that uniquely identifies each node, as they will be
      used as keys in [PatriciaTree]. *)
  module ControlLocation:sig
    include PatriciaTree.KEY
    type t 
    val to_int: t -> int
    val equal: t -> t -> bool
    val hash: t -> int
    val pretty: Format.formatter -> t -> unit
  end

  
  include Fixpoint_wto.Graph
    with module ControlLocation := ControlLocation
     and type transition = G.transition


  (** The list of successors from each node. *)
  val succs: ControlLocation.t -> ControlLocation.t list

  (** [make init succs] traverses the exiting graph to creates a new
      graph suitable for wto iteration. It returns a triple
      [input_node,first_node,count] where
      - [first_node] corresponds to [init] in the original graph;
      - [input_node] is a (pseudo-)node before [first_node], which should not be part of the fixpoint computation,
        as it is used to hold the initial value for the fixpoint graph
      - [count] is the number of nodes in the graph. *)
  val make:G.Vertex.t -> (G.Vertex.t -> (transition * G.Vertex.t) list) -> ControlLocation.t * ControlLocation.t * int

end

