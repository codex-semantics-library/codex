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

(** This defines a generic hierarchical graph component, that can
    display a hierarchical graph, and adapts to its
    transformations. *)
module type S = sig
  type node
  type cluster

  (* TODO: should be more abstract.  *)
  type edge_info = {
    back: bool
  }

  module Node:sig
    val id: node -> int
    val label: node -> string
    val succs: node -> (edge_info * node) list
    val parent_cluster: node -> cluster
    val classes: node -> string      
  end
  module Cluster:sig
    val id: cluster -> int
    val succs: cluster -> cluster list
    val label: cluster -> string
    val parent_cluster: cluster -> cluster option
  end
end

module Make(G:S):sig
  
  type internal_message
  
  type outgoing_message =
    (* TODO: node clicked? also cluster clicked, this kind of things? *)
    | Internal_out of internal_message

  type incoming_message =
    | Redraw of {show_wto:bool}      (* Change the graph definition and redraw. *)
    | Internal_in of internal_message

  type initial_data =
    G.node * G.cluster * (int,G.node) Hashtbl.t * (int,G.cluster) Hashtbl.t * Interface.Graph.Wto.component list

  include Component.S with type incoming_message := incoming_message
                       and type outgoing_message := outgoing_message
                       and type initial_data := initial_data


end
