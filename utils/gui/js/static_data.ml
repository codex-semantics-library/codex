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

open Interface
open Binarytrace



(* This module holds static data used within the webapp *)


module Int64Map = Interface.Int64Map

type interval_node = [`interval_node]
type single_node = [`single_node]

(* It is both the result and the tag for the node kind. *)
type 'a result =
  | Result: string -> interval_node result
  | No_result: interval_node result
  | Single: {severity:int} -> single_node result

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


module Trace_node = struct
  type t = any_trace_node
  let compare (Any_trace_node a) (Any_trace_node b) = Int.compare a.id b.id
  let hash (Any_trace_node x) = x.id
  let equal (Any_trace_node a) (Any_trace_node b) = a.id = b.id
end
(* module Trace_node_set = Set.Make(Trace_node) *)
module Trace_node_map = Map.Make(Trace_node)    
module Trace_node_hash = Hashtbl.Make(Trace_node)

module Loc_hash = Hashtbl.Make(struct
    open Syntax_tree.Location_identifier
    type t = any
    let equal (Any a) (Any b) = equal a b
    let hash (Any x) =  hash x
  end)
let loc_hash = Loc_hash.create 17


module Process_Trace = struct

  (* used only for parsing *)
  type semi_trace_node = {
    semi_id: int;
    loc: Syntax_tree.Location_identifier.any option;
    category: string;
    content:string;
    mutable children: any_trace_node list
  }

  let parse_trace_lines (lines:Binarytrace.trace_line list) = 
    let semi_root_node = {
      semi_id=(-1);
      loc=None;
      category="";
      content="";
      children=[]
    } in
    let path = Stack.create () in
    let counter = ref (-1) in
    Stack.push semi_root_node path;
    List.iter (fun line ->
        match line with 
        | Node {loc;category;content} -> (
            incr counter;
            let semi_node = {
              semi_id = (!counter);
              loc = loc |> Option.map (fun loc -> Syntax_tree.Location_identifier.Any loc);
              category;
              content;
              children = []
            } in
            Stack.push semi_node path;
          )
        | Result result -> (
            (* end the current (top of stack) semi node*)
            let semi_node = Stack.pop path in
            let children_array = Array.of_list (List.rev semi_node.children) in
            let node = {
              id = semi_node.semi_id;
              content = semi_node.content;
              category = semi_node.category;
              loc = semi_node.loc;
              children = children_array;
              result= (match result with None -> No_result | Some x -> Result x);
              expanded=false
            } in
            (*update the map of absolute locations. *)
            (match node.loc with
            | None -> ()
            | Some loc ->
              Loc_hash.add loc_hash loc (Any_trace_node node));

            (* add to parent semi_trace_node *)
            let parent = Stack.top path in
            parent.children <- (Any_trace_node node) :: parent.children;
          )
        | Single {severity; category;content} -> begin
            incr counter;
            let node = {
              id = (!counter);
              loc = None;
              category; content;
              children = [||];
              result = Single {severity};
              expanded = false;
            } in
            let parent = Stack.top path in
            parent.children <- (Any_trace_node node)::parent.children
          end
      ) lines;

    (*now close semi_root_node*)
    let children_array = Array.of_list @@ List.rev semi_root_node.children in
    let root_node = {
      id = semi_root_node.semi_id;
      category = semi_root_node.category;
      content = semi_root_node.content;
      loc = semi_root_node.loc;
      children = children_array;
      expanded = false;
      result = No_result
    } in
    root_node
end

module StringSet = Set.Make(String)

type trace_data = {
  trace_root_node: interval_node trace_node;
  categories: Prefix_tree.t
}


module Graph = struct

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

(* Build the index for CFG. Actually redo a graph with links to the
   cluster,  and create the clusters too. *)
module Process_cfg = struct

  open Graph
  
  (* Note: this algorithm is complicated because it makes assumptions on the CFG, such as:
     
     - There is a root cluster that is on top of every instruction;
     - The entry node has for parent this root cluster;
     - The nodes are processed in order so that when we analyze a call stack, we already know the parent call-stack.
     - The entry node in at the start of a function, so we can retrieve the function symbol using it.

     Not unreasonable but may break.  Probably we could do something
     simpler; maybe the clustering should probably be provided
     directly by the analyzer (would make sense to display the wto). *)
  let find_or_create_cluster id_to_cluster (node:Interface.Graph.light_cfg_node)  (node':Graph.node) (cluster_map: (int64 list, Graph.cluster) Hashtbl.t) find_symbol id =
    let call_stack = node.label.call_stack in
    match Hashtbl.find_opt cluster_map call_stack with
    | Some cluster -> 
      node'.parent_cluster<-cluster; 
      cluster
    | None ->(
        incr id;
        let new_cluster = {
          id = (!id);
          name = find_symbol node.label.head ;
          children_clusters = [];
          parent_cluster = None;(* Filled later *)
          call_stack=call_stack;
        } in
        Hashtbl.add cluster_map call_stack new_cluster;
        Hashtbl.replace id_to_cluster (!id) new_cluster;

        (*Add the new cluster as a child if it should have a parent:*)
        (match new_cluster.call_stack with
         | [] -> () (* new_cluster is the root_cluster*)
         | elt::parent_cs -> (
             (* Find the parent *)
             match Hashtbl.find_opt cluster_map parent_cs with
             | Some parent_cluster ->
               new_cluster.parent_cluster <- Some parent_cluster;
               parent_cluster.children_clusters <- new_cluster :: parent_cluster.children_clusters; 
             | None -> failwith "No parent foundf"
           ));
        node'.parent_cluster <- new_cluster;
        new_cluster

      )

  let rec last = function
    | [] -> raise @@ Invalid_argument "Static_data.last"
    | [x] -> x
    | _ :: xs -> last xs


  let find_symbol_in_decompiled (decompiled: disassembly_line Int64Map.t) (addr:int64) =
    (* Js_browser.Console.log Js_browser.console (Obj.magic "In find symbol_in_decompiled"); *)
    match Int64Map.find_opt addr decompiled  with
    | Some disassembly_line -> 
      (* Js_browser.Console.log Js_browser.console (Obj.magic "Found symbol"); *)
      let res = let symbs = disassembly_line.symbols_names in
        if (symbs <> []) then last symbs else "no symbol" in
      (* Js_browser.Console.log Js_browser.console (Obj.magic res); *)
      res
    | None -> "_end" 
  let find_symbol decompiled addr = (* from interval2symbol*)
    (* Js_browser.Console.log Js_browser.console (Obj.magic "In find symbol"); *)
    (* Js_browser.Console.log Js_browser.console (Obj.magic (Int64.to_string addr));     *)
    if addr = Int64.zero then "ZERO"
    else if addr = 0xefacefacL || addr = 0xcafecaf1L || addr = 0xcafecaf2L
    then "_start"
    else find_symbol_in_decompiled decompiled addr 


  
  module NodeHash = Hashtbl.Make(struct
      open Interface.Graph
      type t = light_cfg_node
      let hash (x:light_cfg_node) = x.id
      let equal
          (a:light_cfg_node)
          (b:light_cfg_node) = a.id = b.id
    end)
  
  let dfs entry cluster_map decompiled  =
    let hash = NodeHash.create 100 in
    let cluster_id = ref 0 in
    let id_to_node = Hashtbl.create 10 in
    let id_to_cluster = Hashtbl.create 10 in     
    let rec do_node (node:Interface.Graph.light_cfg_node) =
      match NodeHash.find hash node with
      | exception Not_found -> begin
          (* We check that the id is indeed unique.  *)
          assert(not @@ Hashtbl.mem id_to_node node.id);
          let node':Graph.node =
            { label = node.label; id = node.id;
              succs = [];
              parent_cluster = Obj.magic 0 }
          in
          NodeHash.replace hash node node';
          Hashtbl.replace id_to_node node.id node';
          let _ =
            find_or_create_cluster id_to_cluster node node' cluster_map (find_symbol decompiled)
              cluster_id
          in
          node'.succs <- (node.succs |> List.map @@ fun (ei,m) -> (ei,do_node m));
          node'
        end
      | node' -> node'
    in
    let entry_node = do_node entry in
    entry_node, id_to_node, id_to_cluster

  (* Build indexes with the CFG (starting in entry) and
     disassembly. *)
  let doit entry decompiled =
    let cluster_map = Hashtbl.create 100 in
    let entry,id_to_node,id_to_cluster = dfs entry cluster_map decompiled in
    
    let root_cluster = (match Hashtbl.find_opt cluster_map entry.label.call_stack with
        | Some root_cluster -> root_cluster
        | None -> failwith "No parent found")
    in
    entry,root_cluster,id_to_node,id_to_cluster

end


(* Build tries for prefix completion. *)

type disassembly = {
  decompiled : Interface.disassembly_line Int64Map.t;  
  symbols_trie: Prefix_tree.t;  
}

module Process_completions = struct

  let process_symbol_table (symbol_table: (string * int64) array) =
    symbol_table
    |> Array.to_seq
    |> Seq.map fst
    |> Seq.fold_left Prefix_tree.add Prefix_tree.empty_set
      

  

end


type source_graph = {
  entry_node: Graph.node;
  root_cluster: Graph.cluster;
  id_to_node: (int,Graph.node) Hashtbl.t;
  id_to_cluster: (int,Graph.cluster) Hashtbl.t;  
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



let static_data =
  let marshalled:Interface.marshalled =
    let data_array = Js_of_ocaml.Js.Unsafe.js_expr "dataArray" in
    Marshal.from_string (Js_of_ocaml.Typed_array.String.of_uint8Array data_array) 0
    (* TODO: We could assign data_array to 0 to retrieve the memory. *)
  in
  let trace_data = 
    let trace_root_node = Process_Trace.parse_trace_lines marshalled.tracelog in
    let categories =
      let f acc (trace_line:Binarytrace.trace_line)  =
        match trace_line with
        | Result _ -> acc
        | Node{category} | Single{category}-> StringSet.add category acc
      in
      let categories = marshalled.tracelog |> List.fold_left f StringSet.empty in
      StringSet.fold (fun a b -> Prefix_tree.add b a) categories Prefix_tree.empty_set in
      {trace_root_node; categories}
  in

  let source:source option = marshalled.source |> Option.map (fun ({text}:Interface.source) -> {text}) in
  
  let disassembly:disassembly option = marshalled.disassembly |> Option.map (fun disassembly ->
      let symbols_trie = Process_completions.process_symbol_table disassembly.symbol_table in
      {decompiled = disassembly.decompiled; symbols_trie}
    ) in
  (* TODO: the "ranges_map": a pointer from each address to a range of
     addresses, and to a list of basic block ids, for
     highlighting on hover. *)
  let source_graph = match marshalled.disassembly, marshalled.cfg with
    | None, _ | _, None -> None
    | Some disassembly, Some cfg ->
      let entry_node,root_cluster,id_to_node,id_to_cluster =
        Process_cfg.doit cfg.cfg_entry_node disassembly.decompiled in  
      Some { entry_node; root_cluster;id_to_node; id_to_cluster }
  in {marshalled; trace_data; source_graph; disassembly; source}
