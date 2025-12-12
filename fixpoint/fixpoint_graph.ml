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


module Log = Tracelog.Make(struct let category = "Fixpoint.MakeGraph" end);;


type 'transition node = {
  mutable id:int;
  mutable preds_with_transition: ('transition node * 'transition) list;
  (* We do not need the list successors for the fixpoint iteration,
     but it needed to traverse the graph from the initial node, and
     thus e.g. for printing, and for Wto computation. So, we keep
     it. *)
  mutable succs: 'transition node list;
}


module type Graph = sig
  module Vertex:Hashtbl.HashedType
  type transition
  val transition_to_string: transition -> string (* For debugging purposes. *)
  val skip: transition
end


module Make(G:Graph) = struct

  type transition = G.transition
  module ControlLocation = struct
    type t = transition node
    let to_int x = x.id
    let equal a b = a == b
    let hash = to_int
    let pretty fmt x = Format.fprintf fmt "%d" x.id
  end
  let preds_with_transition x = x.preds_with_transition
  let succs x = x.succs
  
  module H = Hashtbl.Make(G.Vertex)


  (* Generate a digraph representation that can be outputed by xdot.
     TODO: A tracelog button to display digraphs.  *)
  let dump_graph fmt init =
    let module H' = Hashtbl.Make(struct
        type t = transition node
        let hash x = x.id
        let equal = (==)
      end)
    in
    let hashtbl = H'.create 30 in
    let rec loop cur =
      match H'.find hashtbl cur with
      | exception Not_found -> begin
          H'.replace hashtbl cur ();
          cur.preds_with_transition |> List.iter (fun (pred,trans) ->
              Format.fprintf fmt "%d -> %d [label=\"%s\"] " pred.id cur.id (G.transition_to_string trans)
            );
          cur.succs |> List.iter loop
        end
      | () -> ()
    in
    Format.fprintf fmt "digraph {@[<v>";
    loop init;
    Format.fprintf fmt "@]}"
  ;;
  
  let make init succs =
    (* Mapping from original node to the new nodes. *)
    let hashtbl = H.create 17 in
    (* To avoid conversions, parent is a list with 0 or 1 element. *)
    let rec loop parent cur count =
      match H.find hashtbl cur with
      | exception Not_found -> 
        begin
          (* We label in post-order, which should be close to the
             iteration order. This shoud allow better locality if we
             store the value in an array. *)
          let node = { preds_with_transition=parent;succs=[];id = -1} in
          H.replace hashtbl cur node;
          (* Fold on successors.  *)
          let f (acc,count) (transition,child) =
            let childnode,count = loop [(node,transition)] child count in
            childnode::acc,count
          in
          let children,count = List.fold_left f ([],count) (succs cur) in
          node.succs <- children;
          node.id <- count;
          Log.debug (fun p -> p "Creating node %d" count);
          node,count+1
        end
      | curnode -> begin
          (* Already visited. Just add the parent to the list of predecessors. *)
          let parent = match parent with
            | [parent] -> parent
            | _ -> assert false   (* impossible. *)
          in
          curnode.preds_with_transition <- parent::curnode.preds_with_transition;
          curnode,count
        end
    in

    let first_node, total_nodes = loop [] init 0 in
    let input_node = {
      preds_with_transition = [];
      succs = [first_node] ;
      id = total_nodes;
    } in
    first_node.preds_with_transition <- (input_node, G.skip)::first_node.preds_with_transition;
    Log.debug(fun p -> p "Made graph: %a" dump_graph input_node); 
    input_node,first_node, total_nodes + 1
  ;;
    
end

