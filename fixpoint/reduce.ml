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

(** Interface for directed graphs with labeled vertices and edges.  The
 * reduction algorithm reduces graphs into a regex over the alphabet of edge
 * labels. The input graph must be without self-loops, i.e. edges from one node
 * to itself.
 * NOTE: This algorithm's complexity and correctness has only been established
 * if there is at most one edge between two nodes (as should be the case in a
 * CFG)! *)
module type GRAPHI = sig
  type t

  module V : sig
    type t
      (** Vertex labels *)

    val equal : t -> t -> bool
    val hash : t -> int
    val pretty : Format.formatter -> t -> unit
  end

  module E : sig
    type t
    val src : t -> V.t
    val dst : t -> V.t
  end

  (** Fold over the incoming edges of a node *)
  val fold_pred_e : (E.t -> 'a -> 'a) -> t -> V.t -> 'a -> 'a
end

module Make (G : GRAPHI) (R : Regex.S with type letter = G.E.t) = struct

  type state = (G.V.t, R.t) Hashtbl.t

  module Wto_utils = Wto_utils.Make(G.V)
  open Wto

  module NodeMap = Hashtbl.Make(G.V)

  (* To the w.t.o., we add a map from every node to the set of its heads, to
   * look up efficiently whether a node is in a given component. The set of
   * heads is represented as a list, because in practice we expect to be very
   * small so lists will probably be more efficient. *)
  type decorated_wto =
    { partition : G.V.t partition; heads : (G.V.t list) NodeMap.t }

  let decorate wto =
    let table = NodeMap.create 17 in
    let rec loop heads = function
      | [] -> ()
      | Node n :: rest -> NodeMap.add table n heads; loop heads rest
      | Component (h,body) :: rest ->
          let heads' = h :: heads in
          NodeMap.add table h heads';
          loop heads' body;
          loop heads rest
    in
    loop [] wto;
    { partition = wto; heads = table }

  let in_component {heads;_} head node =
    try List.mem head @@ NodeMap.find heads node
    with Not_found -> raise @@ Invalid_argument "in_component"

  (** [udpate state loops is_head add_epsilon graph predicate node] computes
      the new path expression leading to [node] in [graph] and stores it in
      [state] (replacing whatever value was there before). If [add_epsilon] is
      true, then [R.epsilon] is joined to the result. To that end, it needs the
      loop map [loops], and [is_head] that tells whether [node] is a head. The
      path expression is computed by considering the edges to all predecessors
      of [node] matching [predicate]. If [node] has no such predecessors, the
      expression will be [R.empty], unless [add_epsilon] is true (useful if
      [node] is the entry node).
   *)
  let update state loops is_head add_espilon graph predicate node =
    (* Codex_log.feedback "update %a" G.V.pretty node; *)
    let initial = if add_espilon then R.epsilon else R.empty in
    let new_state = G.fold_pred_e
        (fun e acc ->
           (* Note: faults on nodes that are not reachable. *)
           (* Codex_log.feedback "pred is %a" G.V.pretty (G.E.src e); *)
        if predicate (G.E.src e)
        then R.join acc @@ R.append (Hashtbl.find state (G.E.src e)) e
        else acc
      )
      graph node initial
    in
    Hashtbl.replace state node new_state;
    if is_head then
      try
        let loop = Hashtbl.find loops node in
        if not (R.equal loop R.epsilon) then
          Hashtbl.replace state node @@
            R.append_star (Hashtbl.find state node) loop
      with Not_found -> assert false

  (** [stabilize state loops graph add_epsilon head body] propagates inside the
      component ([head] [body]) the paths that enter the component, but not
      through its head. Once [stabilize] terminates, the state of every node in
      the component is guaranteed to express all possible paths from the entry
      node to that node, provided that the arguments verify the following
      pre-conditions:
        - all predecessors of the component have their states matching the
          possible paths to them;
        - [loops] contains a correct self-loop for [head].
    *)
  let rec stabilize state loops graph predicate add_espilon head body =
    Hashtbl.replace state head R.empty;
    let rec aux = function
      | [] -> ()
      | Node n :: rest ->
          update state loops false false graph predicate n;
          aux rest
      | Component (h, l) :: rest ->
          stabilize state loops graph predicate add_espilon h l;
          aux rest
    in
    aux body;
    (*Format.printf "value in %a before update: %a\n" G.V.pretty head R.pretty (Hashtbl.find state head);*)
    update state loops true add_espilon graph predicate head;
    (*Format.printf "value in %a after update: %a\n" G.V.pretty head R.pretty (Hashtbl.find state head);*)
    Wto_utils.iter
      (fun n is_head -> update state loops is_head false graph predicate n)
      body

  (** [propagate state loops graph predicate wto] takes the first node in the
      w.t.o. as the entry point and propagates and stabilizes path expressions.
      [w] should be a w.t.o. of [g]. [state] and [loops] should be empty.
      [propagate] guarantees, if [predicate] is [fun _ -> true] that the state
      of each node expresses all paths from the first node of [wto] to that
      node. [predicate] is passed to [update]. *)
  let rec propagate loops graph predicate wto =
    let state = Hashtbl.create 17 in
    (* Loop function *)
    let rec aux = function
    | [] -> ()
    | Node n :: rest ->
        update state loops false false graph predicate n;
        aux rest
    | Component (h, l) :: rest ->
        compute_self_loop loops graph wto h l;
        stabilize state loops graph predicate false h l;
        aux rest
    in
    (match wto.partition with
    | [] -> raise (Invalid_argument "propagate")
    | Node n :: rest ->
        (* [n] is the entry point, set its state to "empty regex" *)
        Hashtbl.replace state n R.epsilon;
        aux rest
    | Component (h, l) :: rest ->
        compute_self_loop loops graph wto h l;
        stabilize state loops graph predicate true h l;
        aux rest
    );
    state

  (** Computes the self-loop of a component and stores it. *)
  and compute_self_loop loops graph wto head body =
    (* Propagate on the nodes in the component *)
    let sub_state = propagate loops graph
      (in_component wto head) {wto with partition = Node head :: body} in
    (* Updating the head inside the subgraph gives us all paths from [h] to
     * [h] in that subgraph. Tell [update] that [h] is not a head so that it
     * does not try to access the self-loop (since it is not computed yet). *)
    update sub_state loops false false graph (in_component wto head) head;
    let loop = Hashtbl.find sub_state head in
    if not (R.equal loop R.empty) then
      Hashtbl.replace loops head loop

  let compute_exprs graph wto =
    let loops = Hashtbl.create 17 in
    let wto = decorate wto in
    propagate loops graph (fun _ -> true) wto

end
