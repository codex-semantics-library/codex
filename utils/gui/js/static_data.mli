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

(** The information in marshalled is designed to be terse and easy
    produce.

    The static data builds all the indexes so that it is easy to
    navigate inside this information, and make it available to all the
    components. *)

module Int64Map = Interface.Int64Map

type interval_node = [`interval_node]
type single_node = [`single_node]

(* It is both the result and the tag for the node kind. *)
type 'a result =
  | Result: string -> interval_node result
  | No_result: interval_node result
  | Single: {severity:int} -> single_node result


  

(* The trace_node type is parameterized with a phantom type telling if
   it is an interval node or a single node. *)
type 'a trace_node = {
  id: int;                       (** Unique identifier for the trace node. *)
  loc: Syntax_tree.Location_identifier.any option;
  category: string;
  content:string  ;
  result: 'a result;
  children: any_trace_node array;    (** List of children in order. *)
  mutable expanded: bool;        (** True if expanded, false if collapsed. *)
}
and any_trace_node = Any_trace_node: 'a trace_node -> any_trace_node [@@unboxed]

type trace_data = {
  trace_root_node : interval_node trace_node;
  categories: Prefix_tree.t  
}

module Trace_node_map:Map.S with type key = any_trace_node                                   
module Trace_node_hash:Hashtbl.S with type key = any_trace_node                                   


(** Map from locations to all the trace nodes that refer to this location. *)
module Loc_hash:Hashtbl.S with type key = Syntax_tree.Location_identifier.any
val loc_hash: any_trace_node Loc_hash.t


module Graph:sig

  open Interface.Graph

  type node = {
    id: int;
    label: node_label;
    mutable succs: (edge_info * node) list;
    mutable parent_cluster: cluster;
  }
  and
    cluster = {
    id: int;
    name: string;
    mutable children_clusters: cluster list;
    mutable parent_cluster: cluster option;
    call_stack: int64 list
  }

end

type source_graph = {
  entry_node: Graph.node;      (* Entry node for the graph. *)
  root_cluster: Graph.cluster;
  id_to_node: (int, Graph.node) Hashtbl.t; (* Hashtbl from ids to their node. *)
  id_to_cluster: (int, Graph.cluster) Hashtbl.t (* Hashtbl from ids to their cluster. *)
}


(* Like [!Interface.disassembly], but with pre-compilation of the trie.  *)
type disassembly = {
  decompiled : Interface.disassembly_line Int64Map.t;  
  symbols_trie: Prefix_tree.t;  
}

type source = {
  text: string
}


type static_data = {
  marshalled: Interface.marshalled;
  trace_data: trace_data;
  source_graph: source_graph option;
  disassembly:disassembly option;
  source: source option
}

val static_data : static_data
