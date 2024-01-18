(**************************************************************************)
(*  This file is part of the Codex semantics library.                     *)
(*                                                                        *)
(*  Copyright (C) 2013-2024                                               *)
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

module type AbstractDomain = sig

  (* An abstract domain is a lattice... *)
  type t
  val join: t -> t -> t
  val is_included: t -> t -> bool
  val bottom: t

  module ControlLocation:sig
    include Datatype_sig.S
    val to_int: t -> int                  (* Unique integer identifier. *)
    val preds: t -> t list                (* Predecessors to the block. *)
  end

  (* And a transfer function. Given a control location (cl) and an
     abstract state, returns the successor control locations and
     abstract states. *)
  val transfer: ControlLocation.t -> t -> (ControlLocation.t * t) list

  (* Pretty printer used for debugging. *)
  val pp: Format.formatter -> t -> unit

end


(* Helper for hash functions. *)
let sdbm x y = y + (x lsl 16) + (x lsl 6) - x;;

(* This module defines functions for fixpoint computation over Bourdoncle's weak topological ordering.
     The recursive and iterative strategies are defined by Bourdoncle (we only have a small trick
     in the iterative strategy, to avoid recomputations at the nodes at the beginning of a component
     if things are already propagated.) *)
module WTOFixpoint(L:AbstractDomain) = struct


  module CLMap = Okasakimap.Make(L.ControlLocation)

  (* We store the values on the edges between cls. *)
  module CLPair = Datatype_sig.Prod2(L.ControlLocation)(L.ControlLocation);;

  (* Note: maps are faster than hashtbls here. Okasakimaps and regular maps are almost equally fast, with a small advantage to Okasaki maps. *)
  (* module CLPairMap = Map.Make(CLPair);; *)
  module CLPairMap = Okasakimap.Make(struct
      include CLPair
      let to_int (a,b) =
        assert(L.ControlLocation.to_int a < 1 lsl 32);
        assert(L.ControlLocation.to_int b < 1 lsl 32);
        ((L.ControlLocation.to_int a) lsl 32 ) lor (L.ControlLocation.to_int b)
    end);;

  module CLPairHash = Hashtbl.Make(CLPair);;

  let hashtbl:L.t CLPairHash.t = CLPairHash.create 1000;;

  let reduce = function
    | [] -> L.bottom
    | (a::b) -> List.fold_left L.join a b;;

  (* Get the prestate. *)
  let pre init cl (map,_) =
    (* Printf.printf "Pre cl %d\n" cl.Cil_types.sid; *)
    let preds = L.ControlLocation.preds cl in
    let tentative =
      reduce  @@ List.map(fun pred ->
          try CLPairMap.find (pred,cl) map(* CLPairHash.find hashtbl (pred,cl) *)
          with Not_found -> L.bottom ) preds
    in
    (* Printf.printf "Preds size is %d\n" @@ List.length preds; *)
    (* Printf.printf "Init is %b\n" @@ CLMap.mem cl init; *)
    try L.join tentative @@ CLMap.find cl init
    with Not_found -> tentative
  ;;

  let update prestate node (edgemap,clmap) =
    let clmap = CLMap.add node prestate clmap in 
    (* Self.feedback "Update cl %d %a %a\n" node.Cil_types.sid Cil_datatype.Cl.pretty node L.pp prestate; *)
    (* let l = pre init node acc in *)
    let result = L.transfer node prestate in
    let edgemap = List.fold_left (fun edgemap (cl,state) ->
        (* CLPairHash.replace hashtbl (node,cl) state; acc *)
        CLPairMap.add (node,cl) state edgemap
      ) edgemap result
    in (edgemap,clmap)
  ;;

  (* A counter to count the maximum number of iterations. *)
  let max_iterations = ref 0;;

  module RecursiveStrategy = struct

    let rec fixpoint_component count init acc = function
      | Wto.Node n ->
        let prestate = pre init n acc in
        update prestate n acc
      | Wto.Component(head,part) as c ->
        let head_prestate = pre init head acc in
        (* Note: if we don't pass acc here and start from the old acc for every loop,
           it works but is much slower. Check why and see the algorithm in the paper. *)
        let rec loop acc count head_prestate =
          (* Printf.printf "loop head %d %d\n" head.Cil_types.sid count; *)
          let acc = update head_prestate head acc in
          let acc = fixpoint_partition init acc part in
          let new_head_prestate = pre init head acc in
          if L.is_included new_head_prestate head_prestate
          then (max_iterations := max !max_iterations count; acc)
          else loop acc (count + 1) new_head_prestate
        in
        loop acc 1 head_prestate
    and fixpoint_partition init acc list =
      List.fold_left (fixpoint_component 0 init) acc list

  end

  module IterativeStrategy = struct

    let fixpoint_component count init acc c =


      (* We know that we will have to redo one iteration, so we just update. *)
      let rec loop_update_component c acc =
        (* Printf.printf "Loop update component\n"; *)
        match c with
        | Wto.Node n -> 
          let prestate = pre init n acc in
          update prestate n acc
        | Wto.Component(head,part) ->
          let prestate = pre init head acc in
          let acc = update prestate head acc in
          loop_update_partition part acc
      and loop_update_partition part acc = match part with
        | [] -> acc
        | hd::tl ->
          let acc = loop_update_component hd acc
          in loop_update_partition tl acc
      in

      (* We don't know if we have to redo one iteration: we check and do not update 
         as long as we can.  We return true while the fixpoint is reached. *)
      let rec loop_check_component c ((edgemap,clmap) as acc) =
        (* Printf.printf "Loop check component\n"; *)
        match c with
        | Wto.Node _ -> acc,true
        | Wto.Component(head,part) ->
          let old_prestate = CLMap.find head clmap in
          let new_prestate = pre init head acc in
          (* Self.feedback "inclusion test %a %a %a" Cil_datatype.Cl.pretty head *)
          (*   L.pp new_prestate L.pp old_prestate *)
          (* ; *)

          if L.is_included new_prestate old_prestate
          then loop_check_partition part acc
          else begin
            (*   Self.feedback "not included test %a %a %a" Cil_datatype.Cl.pretty head *)
            (*   L.pp new_prestate L.pp old_prestate *)
            (* ; *)
            let clmap = CLMap.add head new_prestate clmap in
            let acc = update new_prestate head (edgemap,clmap) in
            loop_update_partition part acc, false
          end
      and loop_check_partition part acc = match part with
        | [] -> acc,true
        | hd::tl ->
          let acc,cond = loop_check_component hd acc in
          if cond then loop_check_partition tl acc
          else loop_update_partition part acc, false
      in

      (* Top level iteration. *)
      match c with
      | Wto.Node n ->
        let prestate = pre init n acc in
        update prestate n acc
      | Wto.Component(head,part) as c ->
        (* We need at least two iterations, the second one to check. *)
        let acc = loop_update_component c acc in
        let rec loop numiter acc =
          (* Self.feedback "Doing the iteration %d %a\n" numiter Cil_datatype.Cl.pretty head; *)
          let acc, fp_reached = loop_check_component c acc in
          if fp_reached then acc
          else begin
            (* We only count the iterations that updated the analysis,
               not the ones that do only checks. So we update numiter
               here (fp_reached false => we did some updates) *)
            let numiter = numiter + 1 in
            max_iterations := max numiter !max_iterations; 
            loop numiter acc
          end
        in loop 1 acc
    ;;



    let fixpoint_partition init acc list =
      List.fold_left (fixpoint_component 0 init) acc list
    ;;
  end

  let iteration_strategy = `iterative

  let fixpoint_partition = match iteration_strategy with
    | `iterative -> IterativeStrategy.fixpoint_partition
    | `recursive -> RecursiveStrategy.fixpoint_partition
  ;;

  let fixpoint_partition kf init c =
    let (edgemap,clmap) = fixpoint_partition init (CLPairMap.empty,CLMap.empty) c in
    clmap

end
