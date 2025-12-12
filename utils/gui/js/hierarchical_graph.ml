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

open Command.Writer_monad

module type S = sig
  type node
  type cluster

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

module Color = struct

  let alarm_background = "#eeb2b6"              (* a light red. *)


  (* hsl is hue, saturation, lightness where
     - hue is the angle on the color wheel, from 0 to 360;
     - saturation is between 0 and 1 (0 = gray, 1 = full color)
     - l is lightness (0 = black, 1 = white, 0.5 = normal) *)
  let hue_to_rgb p q t =
    let t = if t < 0.0 then t +. 1.0 else if t > 1.0 then t -. 1.0 else t in
    if t < 1.0 /. 6.0 then p +. (q -. p) *. 6.0 *. t
    else if t < 1.0 /. 2.0 then q
    else if t < 2.0 /. 3.0 then p +. (q -. p) *. (2.0 /. 3.0 -. t) *. 6.0
    else p

  let hsl_to_rgb h s l =
    let h = (h /. 360.0) in    (* normalize hue to [0,1] *)
    let r, g, b =
      if s = 0.0 then
        (l, l, l)
      else
        let q = if l < 0.5 then l *. (1.0 +. s)
          else l +. s -. l *. s in
        let p = 2.0 *. l -. q in
        ( hue_to_rgb p q (h +. 1.0 /. 3.0),
          hue_to_rgb p q h,
          hue_to_rgb p q (h -. 1.0 /. 3.0) )
    in
    ( int_of_float (r *. 255.0 +. 0.5),
      int_of_float (g *. 255.0 +. 0.5),
      int_of_float (b *. 255.0 +. 0.5) )

  let hsl_to_rgb h s l =
    let (r,g,b) = hsl_to_rgb h s l in
    let r = min 255 r in
    let g = min 255 g in
    let b = min 255 b in
    Printf.sprintf "#%02x%02x%02x" r g b
  ;;

  (* Different levels of blue. *)
  let base_cluster_color depth max_depth =
    let h = 205.0 in (* In the blue *)
    let s = 0.5 in   (* Between grey and blue, not too vivid *)
    (* between [0.8..0.99]: never too dark nor completely white. *)
    let l = 0.8 +. (float depth /. float max_depth) *. 0.19 in
    hsl_to_rgb h s l

  let highlighted_color depth max_depth =
    (* Different levels of yellow-orange. *)
    let h = 42.0 in
    let s = 0.9 in
    let l = 0.9 -. (float depth /. float max_depth) *. 0.4 in
    let r = hsl_to_rgb h s l in
    (* Js_browser.Console.log Js_browser.console (Obj.magic @@ Printf.sprintf "highlighted_color %d/%d %s" depth max_depth r); *)
    assert(r <> "#000000");
    r

end



module IntMap = Map.Make(Int)

let graph_id = ref 0
(* Note: I had issues with scanf on graph_%d_cluster_%d, so I removed one _. *)
let node_id_format : _ format6 = "graph_%dnode_%d"
let cluster_id_format : _ format6 = "graph_%dcluster_%d"
let collapsed_cluster_id_format = cluster_id_format

module Dot_Graph(G:S) = struct

  let unique_graph_id = incr graph_id; !graph_id

  let fprintf = Format.fprintf

  type node_or_cluster =
    | Node of G.node
    | Cluster of G.cluster

  module Node_or_cluster = struct
    type t = node_or_cluster
    (* Injective function on node or clusters. We hash using it. *)
    let inj = function
      | Node x ->    (0,G.Node.id x)
      | Cluster x -> (1,G.Cluster.id x)
    let hash x = Hashtbl.hash (inj x)
    let equal a b = (inj a) = (inj b)

    let rec depth = function
      | Node n -> 1 + depth (Cluster (G.Node.parent_cluster n))
      | Cluster c -> begin match G.Cluster.parent_cluster c with
          | None -> 0
          | Some c -> 1 + depth (Cluster c)
        end

    (* Trivial least common ancestor algorithm: compute the path of
       elements to the root, trim so that they have the same length, and
       iterate in sync until we find the common element. There must be
       one because all elements have a root common ancestor. *)
    let least_common_ancestor elt1 elt2 =
      let rec make_list len rev elt =
        let rev = elt::rev in
        let len = len + 1 in
        match elt with
        | Node n -> make_list len rev (Cluster (G.Node.parent_cluster n))
        | Cluster c -> begin
            match G.Cluster.parent_cluster c with
            | None -> len, List.rev rev (* Root. *)
            | Some parent -> make_list len rev (Cluster parent)
          end in
      let len1,l1 = make_list 0 [] elt1 in
      let len2,l2 = make_list 0 [] elt2 in
      let len = min len1 len2 in
      let l1 = l1 |> List.to_seq |> Seq.drop (len1 - len) |> List.of_seq in
      let l2 = l2 |> List.to_seq |> Seq.drop (len2 - len) |> List.of_seq in
      let rec loop l1 l2 = match l1,l2 with
        | [], _ | _, [] -> assert false (* We should end at root at worst. *)
        | hd1::_, hd2::_ when equal hd1 hd2 -> hd1
        | _::tl1, _::tl2 -> loop tl1 tl2
      in loop l1 l2

  end

  module Node_or_cluster_hash = Hashtbl.Make(Node_or_cluster)

  module Cluster_hash = Hashtbl.Make(struct
      type t = G.cluster
      let hash x = Hashtbl.hash (G.Cluster.id x)
      let equal a b = (G.Cluster.id a) = (G.Cluster.id b)
    end)

  (* let parent = function *)
  (*   | Node n -> Some(Cluster (G.Node.parent_cluster n)) *)
  (*   | Cluster c ->  *)
  (*     G.Cluster.parent_cluster c |> Option.map (fun cl -> Cluster cl) *)

  let show_node_id n =
    Format.sprintf node_id_format unique_graph_id @@ G.Node.id n

  let show_collapsed_cluster_id c =
    Format.sprintf cluster_id_format unique_graph_id @@ G.Cluster.id c

  let show_cluster_id c =
    Format.sprintf cluster_id_format unique_graph_id @@ G.Cluster.id c


  let show_node_or_cluster_id = function
    | Node n -> show_node_id n
    | Cluster c -> show_collapsed_cluster_id c

  (* If one of your parent is collapsed, you are not displayed. This
     function returns the first node or cluster for which all is
     parents are not collapsed. Note that nodes themselves are always
     displayed, so we always find a level at which the node is
     displayed. *)
  let first_displayed_node_or_cluster node is_collapsed =
    let rec aux cur cluster =
      let cur =
        if is_collapsed cluster
        then Cluster cluster
        else cur
      in
      match G.Cluster.parent_cluster cluster with
      | None -> cur             (* No parent: root. *)
      | Some parent -> aux cur parent
    in aux (Node node) (G.Node.parent_cluster node)

  let first_displayed_node_or_cluster node is_collapsed =
    let res = first_displayed_node_or_cluster node is_collapsed in
    (* (match res with *)
    (*  | Node r -> Js_browser.Console.log Js_browser.console (Obj.magic @@ Printf.sprintf "first_displayed_node_or_cluster %d node %d" (G.Node.id node) (G.Node.id r)) *)
    (*  | Cluster r -> Js_browser.Console.log Js_browser.console (Obj.magic @@ Printf.sprintf "first_displayed_node_or_cluster %d cluster %d" (G.Node.id node) (G.Cluster.id r))); *)
    res;;



  (* Print an arrow from a node to a node. *)
  let pp_arrow fmt n1 n2 edge_info =
    let back = edge_info.G.back in
    let self_loop = Node_or_cluster.equal n1 n2 in
    let default() =
      fprintf fmt "\"%s\" -> \"%s\"" (show_node_or_cluster_id n1) (show_node_or_cluster_id n2);
      if back then
        fprintf fmt " [%scolor=red constraint=false]"
          (* For self back loops, make them look like they go back up. *)
          (if self_loop then "dir=back " else "");
      fprintf fmt ";@\n"
    in
    (* We do not print self loops from collapsed clusters to
       themselves, except if it is an identified self-loop. *)
    match back, self_loop, n1 with
    | true, _, _ -> default()             (* Print identified back loops. *)
    | false, true, Cluster _ -> ()
    | false, _ , _ -> default()

  (* Print a collapsed cluster. "eq_node"
     TODO: have a language for table labels? *)
  let pp_collapsed_cluster fmt cluster color =
    fprintf fmt "\"%s\"[id=\"%s\"; fillcolor=\"%s\"; label=<<TABLE BORDER=\"1\" CELLBORDER=\"0\" CELLSPACING=\"0\"><TR><TD>%s</TD></TR><HR/><TR><TD>[...]</TD></TR></TABLE>>];@\n"
      (show_collapsed_cluster_id cluster)
      (show_collapsed_cluster_id cluster)
      color
      (G.Cluster.label cluster)
  ;;

  let show_node_color node =
    if (List.length (G.Node.succs node)) = 0
    then Color.alarm_background
    else "white"

  (** Print information about a single node.  TODO: Get rid of wtomap
      here.  Also: we should probably be printing wto components as
      clusters.  *)
  let pp_node wtomap fmt node =
    let _color = match wtomap with
      | Some map -> (match IntMap.find_opt (G.Node.id node) map with
          | Some depth -> (match depth mod 5 with
              | 0 -> "#ffbe0b"
              | 1 -> "#fb5607"
              | 2 -> "#ff006e"
              | 3 -> "#8338ec"
              | 4 -> "#3a86ff"
              | _ -> "#06d6a0")
          | None -> "black")
      | None -> "white"
    in
    (* Add cursor-pointer tailwind property to tell that we can click *)
    fprintf fmt ("\"%s\"  [id=\"%s\"; class=\"%s cursor-pointer\"; style=filled; fillcolor=\"%s\"; label=%s];@\n")
      (show_node_id node)
      (show_node_id node)
      (G.Node.classes node)
      (show_node_color node)
      (G.Node.label node)

  (* Iterate on every node, calling on_edge and on_node. *)
  let dfs entry on_edge on_node  =
    let hash = Hashtbl.create 100 in
    let rec do_node n =
      if not @@ Hashtbl.mem hash (G.Node.id n) then begin
        Hashtbl.replace hash (G.Node.id n) ();
        on_node n;
        n |> G.Node.succs |> List.iter @@ fun (ei,m) ->
        on_edge n ei m;
        do_node m
      end
    in do_node entry


  type cluster_info = {
    children: node_or_cluster list;
  }

  (* Intermediate structure that we compute before printing:
     - list of nodes;
     - list of edges as mapping from source to target to info
     - for each non-collapsed cluster nodes: the list of its children.
       Note that collapsed clusters are not in the map.
     - for each cluster: its height in the tree, i.e. maximal distance
       from the root. *)
  type printed_graph = {
    nodes: unit Node_or_cluster_hash.t;
    edges: G.edge_info Node_or_cluster_hash.t Node_or_cluster_hash.t;
    cluster_infoh: cluster_info Cluster_hash.t;
    depthh: int Cluster_hash.t;
    max_depth: int;
  }

  let compute_printed_graph entry is_collapsed =
    let nodes = Node_or_cluster_hash.create 100 in
    let edges = Node_or_cluster_hash.create 100 in
    let cluster_infoh = Cluster_hash.create 100 in
    (* We don't want to process a cluster twice, and add it twice to the list of children.
       To avoid that, we record when a cluster has been processed.
       Note that we don't need to to this for nodes, because
       we know that they will be processed only once, at they are the leaves.

       We also attach to each cluster its depth relative to the root. *)
    let done_clusters = Cluster_hash.create 100 in

    let max_depth = ref 0 in

    (* Complete the children list, using done_clusters to avoid
       filling elements twice. *)
    let complete_children node_or_cluster =
      let add_child child parent =
        let cl_info = match Cluster_hash.find cluster_infoh parent with
          | exception Not_found -> {children=[]}
          | l -> l in
        let children = child::cl_info.children in
        Cluster_hash.replace cluster_infoh parent {children}
      in
      let rec process_cluster cluster  =
        (* Js_browser.Console.log Js_browser.console (Obj.magic @@ Printf.sprintf "process_cluster height %d" height); *)
        match Cluster_hash.find_opt done_clusters cluster with
        | Some depth -> depth
        | None -> begin
            match G.Cluster.parent_cluster cluster with
            | None ->
              Cluster_hash.replace done_clusters cluster 0; 0
            | Some parent ->
              let depth = process_cluster parent in
              Cluster_hash.replace done_clusters cluster (depth + 1);
              add_child (Cluster cluster) parent;
              depth + 1
          end
      in
      let depth = match node_or_cluster with
        | Node n ->
          let parent = G.Node.parent_cluster n in
          add_child node_or_cluster parent;
          1 + process_cluster parent
        | Cluster c -> process_cluster c in
      max_depth := max !max_depth depth;
    in

    let on_node n =
      let n' = first_displayed_node_or_cluster n is_collapsed in
      Node_or_cluster_hash.replace nodes n' ();
      complete_children n'
    in
    let on_edge n edge_info m =
      let n' = first_displayed_node_or_cluster n is_collapsed in
      let m' = first_displayed_node_or_cluster m is_collapsed in
      let hashtbl2 = match Node_or_cluster_hash.find edges n' with
        | exception Not_found ->
          let h = Node_or_cluster_hash.create 5 in
          Node_or_cluster_hash.replace edges n' h;
          h
        | x -> x in
      Node_or_cluster_hash.replace hashtbl2 m' edge_info in
    dfs entry on_edge on_node;
    { nodes; edges; cluster_infoh; depthh = done_clusters; max_depth = !max_depth }

  let print_printed_graph wtomap root_cluster fmt {nodes;edges;cluster_infoh;depthh;max_depth} =
    let open Format in

    fprintf fmt "digraph G {@\n";
    (* fillcolor=white, still=filled,solid is for collapsed cluster nodes.
       shape=none margin=0 is for every node.*)
    fprintf fmt "node[fillcolor=\"white\" style=\"filled,solid\" shape=none margin=0];@\n";

    (* Print every edge. *)
    edges |> Node_or_cluster_hash.iter (fun n h ->
        h |> Node_or_cluster_hash.iter (fun m ei  ->
            pp_arrow fmt n m ei));

    (* Print every cluster hierarchically. *)
    let rec do_cluster = function
      | Node n -> pp_node wtomap fmt n
      | Cluster cluster ->
        match Cluster_hash.find cluster_infoh cluster  with
        | exception Not_found -> begin
            let depth = Cluster_hash.find depthh cluster in
            let color = Color.base_cluster_color depth max_depth in
            (* No children = collapsed cluster. *)
            pp_collapsed_cluster fmt cluster color
          end
        | {children} -> begin
            let depth = Cluster_hash.find depthh cluster in
            (* let fillcolor = (\* "#aaaaaa" in *\) Color.base_cluster_color depth max_depth in *)
            let fillcolor = (* "#aaaaaa" in *) Color.base_cluster_color depth max_depth in
            (* Js_browser.Console.log Js_browser.console (Obj.magic "fillcolor"); *)
            (* Js_browser.Console.log Js_browser.console (Obj.magic fillcolor); *)
            fprintf fmt "@[<v 2>subgraph \"cluster_%d\" {@,\
                         id=\"%s\" style=\"filled,solid\" color=\"black\"; fillcolor=\"%s\"; label=\"%s\";@]"
              (G.Cluster.id cluster) (show_cluster_id cluster)  fillcolor (G.Cluster.label cluster);
            children |> List.iter (fun child -> do_cluster child);
            fprintf fmt "}@]@\n";
          end;
    in
    do_cluster (Cluster root_cluster);
    fprintf fmt "}@."
  ;;

  (* What the algorithm does is:
     1. Do a DFS;
     2. Create a summary graph whose nodes are collopased clusters and
     original nodes, and edges between them, and the tree that says,
     for each cluster, what nodes/clusters it contains;
     3. Print the nodes using the tree;
     4. Print all the edges. *)
  let pp_dot_graph fmt graph root_cluster ~wtomap ?(pre_node=(fun _ _ -> ())) (is_collapsed: G.cluster -> bool) =
    let summary = compute_printed_graph graph is_collapsed in
    print_printed_graph wtomap root_cluster fmt summary;
    summary.max_depth

end

module Make(G:S) = struct

  module Dot_Cfg = Dot_Graph(G)
  type model = {
    (* Static information about the graph. *)
    entry_node: G.node;
    root_cluster: G.cluster;
    id_to_node: (int,G.node) Hashtbl.t;
    id_to_cluster: (int,G.cluster) Hashtbl.t;

    (* Information about the collapsed graph.  The source of truth is
       collapsed_id. From that, we derive the graphviz model and the
       maximum depth of the collapsed graph. *)
    collapsed_ids: int list;
    max_depth: int; (* Depth of the collapsed cluster tree. *)
    graphviz: Graphviz.model;

    (* The current element pointed to in the tree. We highlight all
       the elements from it to the root (and change the graphviz model
       accordingly. *)
    hovered_element: Dot_Cfg.node_or_cluster option;

    (* Information about the WTO. TODO: This should be a cluster just
       like the others. *)
    show_wto: bool;
    wto_depth_map: int IntMap.t;
  }

  type internal_message =
    | Nop
    | Collapse_cluster of G.cluster
    | Expand_cluster of G.cluster
    | Mouseon of Dot_Cfg.node_or_cluster option

  type outgoing_message =
    (* Maybe: node_clicked. *)
    | Internal_out of internal_message

  type incoming_message =
    | Redraw of { show_wto : bool; }
    | Internal_in of internal_message

  type node_kind = Node | Cluster | Collapsed_cluster | Nothing
  let lift_msg model : Graphviz.outgoing_message -> outgoing_message = function
    | Graphviz.Event {elt;kind;event_type} -> begin
        let id = Js_browser.Element.id elt in
        (* Js_browser.Console.log Js_browser.console (Obj.magic id); *)
        (* Js_browser.Console.log Js_browser.console (Obj.magic @@ match kind with Cluster -> "cluster" | Edge -> "edge" | Node -> "node" | Nothing -> "nothing"); *)
        (* Js_browser.Console.log Js_browser.console (Obj.magic @@ match event_type with Mouseenter -> "mouseenter" | Mouseleave -> "mouseleave" | Click -> "click" | Mousemove -> "mousemove"); *)


        let node_kind,id = match kind with
          | Cluster -> Cluster, snd @@ Scanf.sscanf id cluster_id_format (fun x y -> (x,y))
          | Node ->
            (try Node,Scanf.sscanf id node_id_format (fun _ x -> x) with
               Scanf.Scan_failure _ -> begin
                 Collapsed_cluster,Scanf.sscanf id collapsed_cluster_id_format (fun _ x -> x)
               end)
          | Edge -> Nothing, 0
          | Nothing -> Nothing, 0
        in
        match node_kind,event_type with
        (* Click on a cluster. *)
        | Cluster,Click -> Internal_out (Collapse_cluster (Hashtbl.find model.id_to_cluster id))
        (* Click on a collapsed cluster. *)
        | Collapsed_cluster,Click -> Internal_out (Expand_cluster (Hashtbl.find model.id_to_cluster id))
        (* | Node, Mouseenter -> Internal_out (Highlight_node (Hashtbl.find model.id_to_node id)) *)
        (* | Node, Mouseleave -> Internal_out (Unhighlight_node (Hashtbl.find model.id_to_node id)) *)
        (* | Cluster, Mouseenter -> Internal_out (Highlight_cluster (Hashtbl.find model.id_to_cluster id)) *)
        (* | Cluster, Mouseleave -> Internal_out (Unhighlight_cluster (Hashtbl.find model.id_to_cluster id)) *)
        | Node, Mousemove -> Internal_out (Mouseon (Some (Node (Hashtbl.find model.id_to_node id))))
        | Cluster, Mousemove -> Internal_out (Mouseon (Some (Cluster (Hashtbl.find model.id_to_cluster id))))
        | Nothing, (Mouseleave | Mousemove) -> Internal_out (Mouseon None)
        | _, Mouseenter -> (Internal_out Nop)
        | _, Mouseleave -> (Internal_out Nop)
        | _ -> (Internal_out Nop)
      end
  ;;




  module StringSet = Set.Make(String)

  let pp_dot_graph fmt model =
    Dot_Cfg.pp_dot_graph fmt
      model.entry_node
      model.root_cluster
      (fun cl -> List.mem (G.Cluster.id cl) model.collapsed_ids)
      ~wtomap:(if model.show_wto then Some model.wto_depth_map else None)



  let to_dot model =
    let x = ref (Obj.magic 0) in
    let f fmt model =
      let res = pp_dot_graph fmt model in
      x := res
    in
    !x, Format.asprintf "%a" f model


  type initial_data =
    G.node * G.cluster * (int,G.node) Hashtbl.t * (int,G.cluster) Hashtbl.t * Interface.Graph.Wto.component list

  let init (entry_node,root_cluster,id_to_node,id_to_cluster,wto) =
    let map = ref IntMap.empty in
    let rec visit component depth =
      match component with
      | Interface.Graph.Wto.Node id -> map := IntMap.add id depth !map;
      | Interface.Graph.Wto.SCC (head, list) -> (map := IntMap.add head (depth + 1) !map; List.iter (fun c -> visit c (depth + 1)) list)
    in
    List.iter (fun c -> visit c 0) wto;
    let fake =
      { entry_node;
        root_cluster;
        id_to_node;
        id_to_cluster;
        hovered_element = None;
        show_wto = false;
        wto_depth_map = !map;
        max_depth = -1;
        collapsed_ids = [];
        graphviz = Obj.magic 0 } in
    let max_depth,dot = to_dot fake in
    let graphviz, cmd = Graphviz.init dot in
    let model = { fake with max_depth; graphviz } in
    model,
    (let _ = assert (cmd = Vdom.Cmd.Batch []) in Vdom.Cmd.Batch [])
    (* Vdom.Cmd.map (lift_msg model) cmd *)

  (* These are for operations that need a redraw, e.g. when adding nodes *)
  let redraw model =
    let max_depth,dot = to_dot model in
    let+ graphviz = Graphviz.(update model.graphviz @@ Redraw{dot}) in
    { model with max_depth; graphviz }

  (* Highlight or unhighlight an element in the tree. *)
  let change_elt f graphviz max_depth elt =
    let open Dot_Cfg in
    match elt with
    | Node n ->
      let id = Dot_Cfg.show_node_id n in
      let color = f elt max_depth max_depth in
      let msg = Graphviz.Set_background_color{id;color} in
      Graphviz.(update graphviz msg)
    | Cluster c ->
      let id = Dot_Cfg.show_cluster_id c in
      let depth = Node_or_cluster.depth (Cluster c) in
      let color = f elt depth max_depth in
      let msg = Graphviz.Set_background_color{id;color} in
      Graphviz.(update graphviz msg)

  (* Highlight or unhighlight elements from elt to ancestor (excluded).
     If ancestor is None, change everything. *)
  let change_elts f max_depth ancestor graphviz elt =
    let open Dot_Cfg in
    let rec loop graphviz elt : _ * _ Vdom.Cmd.t =
      match elt,ancestor with
      (* We reached the ancestor: stop changing. *)
      | _, Some ancestor when Node_or_cluster.equal elt ancestor -> Vdom.return graphviz
      | _ -> begin
          let* graphviz = change_elt f graphviz max_depth elt in
          match elt with
          | Node n ->  loop graphviz (Cluster (G.Node.parent_cluster n))
          | Cluster c -> begin
              match G.Cluster.parent_cluster c with
              | None -> Vdom.return graphviz     (* No parent: stop. *)
              | Some parent -> loop graphviz (Cluster parent)
            end
        end
    in loop graphviz elt
  ;;

  let highlight_elts = change_elts (fun _elt -> Color.highlighted_color)
  let unhighlight_elts = change_elts
      (function Node n -> fun _ _ -> Dot_Cfg.show_node_color n
              | Cluster _ -> Color.base_cluster_color)

  let rehighlight_hierarchy model elt' =
    let elt = model.hovered_element in
    let graphviz = model.graphviz in
    let* graphviz = match elt,elt' with
      | None, None -> Vdom.return graphviz
      | None, Some elt' ->
        (* Highlight every parent up to the root.  *)
        highlight_elts model.max_depth None graphviz elt'
      | Some elt, None ->
        (* Unhighlight every parent up to the root.  *)
        unhighlight_elts model.max_depth None graphviz elt
      | Some elt, Some elt' -> begin
          let lca = Dot_Cfg.Node_or_cluster.least_common_ancestor elt elt' in
          let* graphviz = unhighlight_elts model.max_depth (Some lca) graphviz elt in
          let+ graphviz = highlight_elts model.max_depth (Some lca) graphviz elt' in
          graphviz
        end in
    return { model with hovered_element = elt'; graphviz}

  let update model msg =
    let model,cmd = match msg with
      (* No redraw for simple highlighting, we just change the color directly. *)
      (* Note: instead of listening to mouseleave/mouseenter events
         (which does not work because we miss some), we instead get
         samples where the position of the mouse currently is; and we
         act upon it by dehighlighting the previous position, and
         highlighting the new. This is much more robust. *)
      | Internal_in Nop -> return model
      | Internal_in(Mouseon x) -> rehighlight_hierarchy model x
      | Internal_in(Collapse_cluster cl) ->
        let id = G.Cluster.id cl in
        redraw {model with collapsed_ids = id::model.collapsed_ids}
      | Internal_in(Expand_cluster cl) ->
        let id = G.Cluster.id cl in
        redraw {model with collapsed_ids = List.filter ((!=) id) model.collapsed_ids}
      | Redraw _ -> redraw model
    in model, Vdom.Cmd.map (lift_msg model) cmd




  (* Some of our nodes are not real nodes, but collapsed clusters. *)
  let view model =
    Vdom.map (fun msg -> lift_msg model msg) @@ Graphviz.view model.graphviz


end
