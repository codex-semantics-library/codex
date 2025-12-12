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

module V = struct
  type t = Dba.id
  type label = t

  let label x = x
  let create x = x
  let equal = (=)
  let hash = Hashtbl.hash
  let compare = Stdlib.compare

  let pretty fmt x = Format.pp_print_int fmt x
end

module E = struct
  type vertex = V.t
  type t = vertex * vertex
  type label = unit

  let create x () y = (x,y)
  let label _ = ()

  let src (x,_) = x
  let dst (_,y) = y

  let compare (x1,x2) (y1,y2) =
    let c = V.compare x1 y1 in
    if c <> 0 then c
    else V.compare x2 y2

  let equal x y =
    compare x y = 0

  let hash (x,y) =
    Hashtbl.hash (V.hash x, V.hash y)

  let pretty fmt (v1,v2) =
    Format.fprintf fmt "(%a->%a)" V.pretty v1 V.pretty v2
end

module G_0 = Graph.Persistent.Digraph.ConcreteBidirectional(V)

module G_1 = struct
  module V = V
  module E = E
  include (G_0 : module type of G_0
    with module V := V and module E := E)
end

module G = struct
    include G_1
    (*
    let vertex_name v =
      Format.asprintf "%i" v
    let graph_attributes _ = []
    let edge_attributes _ = []
    let default_edge_attributes _ = []
    let vertex_attributes _ = []
    let default_vertex_attributes _ = []
    let get_subgraph _ = None
    *)
end

(* Graph printing. Useful for debugging. *)
(*
module Output = Graph.Graphviz.Dot(G)
*)

module Dhunk_regex = Fixpoint.Regex.Make(G.E)
module Reduce = Fixpoint.Reduce.Make(G)(Dhunk_regex)
module Wto_alg = Fixpoint.Wto.Make(G.V)

open Dba2Codex

let addr_of_target {Dba.base;Dba.id} =
  assert (id = 0); base

let graph_of_dhunk dhunk : G.t * (Dba.id * Virtual_address.t option) list =
  let open Dba.Instr in
  let next = function
    | Assign (_,_,nxt) -> [Jump_Inner nxt]
    | SJump (Dba.JInner nxt,_) -> [Jump_Inner nxt]
    | SJump (Dba.JOuter target,_) -> [Jump_Outer (addr_of_target target)]
    | DJump _ -> [Jump_Dynamic]
    | If (_,Dba.JInner ift_nxt,iff_nxt) ->
        [Jump_Inner ift_nxt; Jump_Inner iff_nxt]
    | If (_,Dba.JOuter ift_addr,iff_nxt) ->
        [Jump_Outer (addr_of_target ift_addr); Jump_Inner iff_nxt]
    | Stop _ -> raise @@ Invalid_argument "regex_of_dhunk: stop encountered"
    | Assert (_,nxt) -> [Jump_Inner nxt]
    | Assume (_,nxt) -> [Jump_Inner nxt]
    | Nondet (_,nxt) -> [Jump_Inner nxt]
    | Undef (_,nxt) -> [Jump_Inner nxt]
  in
  let add_exit_or_edge id (acc_gr, acc_exits) = function
    | Jump_Inner nxt -> G.add_edge acc_gr id nxt, acc_exits
    | Jump_Outer addr -> acc_gr, (id, Some addr) :: acc_exits
    | Jump_Dynamic -> acc_gr, (id, None) :: acc_exits
  in
  let rec aux acc_graph acc_exits id =
    if id >= Dhunk.length dhunk then acc_graph, acc_exits
    else
      let i = match Dhunk.inst dhunk id with Some i -> i
        | _ -> raise @@ Failure "graph_of_dhunk: Dhunk.get" in
      let acc_graph', acc_exits' = 
        List.fold_left (add_exit_or_edge id) (acc_graph, acc_exits) @@
        next i
      in
      aux acc_graph' acc_exits' (id + 1)
  in
  aux G.(add_vertex empty 0) [] 0

let exit_regexes dhunk : (Dba.id * Dhunk_regex.t * Virtual_address.t option) list =
  let graph, exits = graph_of_dhunk dhunk in
  let wto = Wto_alg.partition ~pref:(fun _ _ -> 0) ~init:0 ~succs:(G.succ graph) in
  let regexes = Reduce.compute_exprs graph wto in
  List.map (fun (id,addr) -> (id, Hashtbl.find regexes id, addr)) exits

