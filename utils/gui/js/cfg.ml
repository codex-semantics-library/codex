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

(* TODO: We should split this in a sequence of layers.

   - The bottom most is graphviz handling. We send dot source, we
   receive events about nodes (this corresponds to the module
   graphviz).

   - The next layer is a notion of hierarchical graph. We give
   information about nodes, edges and clusters, those get displayed.
   (this corresponds to the module hierarchical_graph).

   - Next, we should have a notion of CFG, with components like wto,
   domination graph, etc. that can be displayed. (this corresponds to
   graph_utils/display_graph).

   - Finally, we have the notion of a CFG for binary code, with number
   as addresses (this module).

   Note that there are not that many differences between the third and
   fourth layer for now.

*)


module Printable_Cfg = struct
  open Interface.Graph
  open Static_data.Graph
  let pp_int64 fmt i = let open Format in fprintf fmt "0x%08Lx" i
  type nonrec node = node
  type nonrec cluster = cluster
  type nonrec edge_info = edge_info = { back: bool }
  module Node = struct
    let id (node:node) = node.id
    let succs node = node.succs
    let parent_cluster (node:node) = node.parent_cluster
    let label node = Format.asprintf "<<TABLE BORDER=\"1\" CELLBORDER=\"0\" CELLSPACING=\"0\"><TR><TD>%a</TD></TR><HR/><TR><TD>%a</TD></TR></TABLE>>"
        pp_int64 node.label.head
        pp_int64 node.label.tail
    let classes node = Format.asprintf "hover_%a" pp_int64 node.label.head
  end

  module Cluster = struct
    let id cluster = cluster.id
    let succs cluster = cluster.children_clusters
    let label cluster = cluster.name
    let parent_cluster cluster = cluster.parent_cluster
  end
end


module Hierarchical_graph = Hierarchical_graph.Make(Printable_Cfg)

type model = {
  (* collapsed_demo: bool; *)
  hierarchical_graph: Hierarchical_graph.model;
}


type internal_message =
  | Hierarchical_graph_internal of Hierarchical_graph.internal_message
  | Set_selected_line of int64

type outgoing_message =
  | Internal_out of internal_message

type incoming_message =
  | Internal_in of internal_message

let lift_hierarchical_graph_message : Hierarchical_graph.outgoing_message -> outgoing_message =
  function (Hierarchical_graph.Internal_out msg) -> Internal_out(Hierarchical_graph_internal msg)

let select_line (addr:int64) : incoming_message =
  Internal_in (Set_selected_line addr)


(* NB: This code is part of a dfs to update the range map.
   Should be in static data

*)
    (* ~pre_node:(fun node node_id_str -> *)
    (*     match ranges_map with *)
    (*     | Some map -> (match Hashtbl.find_opt map (node.label.head, node.label.tail) with *)
    (*         | Some existing_node_ids -> Hashtbl.replace map (node.label.head, node.label.tail) (StringSet.add node_id_str existing_node_ids) *)
    (*         | None -> Hashtbl.add map (node.label.head, node.label.tail) (StringSet.singleton node_id_str) *)
    (*       ) *)
    (*     | None -> (); *)
    (*   ) *)

type initial_data = Interface.cfg * Static_data.source_graph * (int64 * int64, Set.Make(String).t) Hashtbl.t

let init ((cfg:Interface.cfg), source_graph, ranges_map) =
  let hierarchical_graph,cmd =
    Hierarchical_graph.init
      (source_graph.Static_data.entry_node,
       source_graph.root_cluster,
       source_graph.id_to_node,
       source_graph.id_to_cluster,
       cfg.wto)
  in
  let model = {hierarchical_graph} in
  model, Vdom.Cmd.map lift_hierarchical_graph_message cmd



(* TODO: Generic graph module + ingoing/outgoing for hover/click on node,cluster and edges.
   + CFG module that uses these functions


*)

(* TODO: We should define a fixed graph div here that we would reuse;
   and imperatively update the state in the update function. *)
(* XXX: The show-wto button is not used anymore. *)
(* let view model =
  div [ Widget.button
          ~onclick:(fun _ -> Internal_out Toggle_wto)
          (if model.show_wto then "Hide WTO" else "Show WTO")
      ; div ~a:[class_ "border border-blue-500"]
          [Vdom.map lift_hierarchical_graph_message @@
           Hierarchical_graph.view model.hierarchical_graph]
      ] *)

let view model =
  Vdom.map lift_hierarchical_graph_message @@
  Hierarchical_graph.view model.hierarchical_graph

(* let render_graph model = *)
(*   let hierarchical_graph,cmd = *)
(*     Hierarchical_graph.(update model.hierarchical_graph @@ Redraw{show_wto=model.show_wto}) in *)
(*   { model with hierarchical_graph},cmd *)

let update model (Internal_in message) =
  let model,cmd =
    match message with
    | Hierarchical_graph_internal msg ->
      let hierarchical_graph,cmd =
        Hierarchical_graph.(update model.hierarchical_graph (Internal_in msg)) in
      {hierarchical_graph},cmd
    | Set_selected_line addr ->
        Vdom.return model
    in 
    model, Vdom.Cmd.map lift_hierarchical_graph_message cmd



let get_menu model = [Transient_menu.{title = "Control-flow Graph"; entries = [| |]}]
