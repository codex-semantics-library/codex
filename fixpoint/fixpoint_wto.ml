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

module Log = Tracelog.Make(struct let category = "Fixpoint.Fixpoint_Wto" end);;

module type AbstractDomain = sig
  type state
  type transition
  type loop_id
  val copy: state -> state
  val join: state -> state -> state
  val fixpoint_step: loop_id:loop_id -> state -> state -> (state * bool)
  val transfer: transition -> state -> state option
  val pp: Format.formatter -> state -> unit
  val enter_loop: state -> (state * loop_id)
  val leave_loop: state -> state
end


module type Graph = sig
  type transition
  module ControlLocation:PatriciaTree.KEY
  val preds_with_transition: ControlLocation.t -> (ControlLocation.t * transition) list
end

module Make(G:Graph)(D:AbstractDomain with type transition = G.transition) = struct

  module CLMap = PatriciaTree.MakeMap(G.ControlLocation);;

  (* Compute the prestate for a node. *)
  let pre node map =
    let preds = G.preds_with_transition node in
    let prestates = preds |> List.filter_map(fun (pred,transition) ->
        match CLMap.find pred map with
        | exception Not_found -> None
        | state -> D.transfer transition state) in
    match prestates with
    | [] -> None
    | a::b ->
      let state = List.fold_left D.join a b in
      Some state

  let pp_opt fmt x = match x with
    | None -> Format.fprintf fmt "None"
    | Some v -> Format.fprintf fmt "Some %a" D.pp v

  let pp_map fmt map =
    Format.fprintf fmt "{ ";
    map |> CLMap.iter (fun key _value -> Format.fprintf fmt "%d " (G.ControlLocation.to_int key));
    Format.fprintf fmt "} "
  ;;

  (* If node is a single node, update it in the map (remove the binding if the state is bottom. *)
  let update node value map =
    Log.debug (fun p -> p "Updating %d with %a %a" (G.ControlLocation.to_int node) pp_opt value pp_map map);
    match value with
    | None -> CLMap.remove node map
    | Some value -> CLMap.add node value map
  ;;

  let fixpoint_step ~loop_id previous next = match previous,next with
    | Some previous, Some next ->
      let state,bool = D.fixpoint_step ~loop_id previous next in
      Log.debug (fun p ->
          p "Fixpoint step@\n previous:@\n %a@\nnext:@\n %a@\nres:@\n%a"
            D.pp previous D.pp next D.pp state);
      Some state, bool
    | None, (Some _) -> next, false
    | None, None -> None, true
    | Some _, None -> assert false (* Should not happen if transfer
                                      functions are monotonous. *)

  let copy = Option.map D.copy

  (* Note: this assumes an initial map which is not changed after,
     i.e. we want the entry point to have no predecessor. TODO: check
     it. *)
  let rec fixpoint_component acc = function
    | Wto.Node n -> update n (pre n acc) acc
    | Wto.Component(head,part) as c ->
      (* We start over from save_acc everytime, otherwise we have
         spurious values when joining that come from the end of
         previous iterations. This strategy is described in [Lemerre,
         POPL 2023]. *)
      let save_acc = acc in
      let head_prestate = pre head acc in
      match head_prestate with
      | None -> acc             (* Nothing to propagate on this component. *)
      | Some head_prestate -> begin
        let head_prestate, loop_id = D.enter_loop head_prestate in
        let rec loop head_prestate =
          let head_prestate_backup = copy head_prestate in
          let acc = update head head_prestate acc in
          let acc = fixpoint_partition acc part in
          let new_head_prestate = pre head acc in
          let newstate, fp_reached = fixpoint_step ~loop_id head_prestate_backup new_head_prestate in
          Log.debug (fun p -> p "Fixpoint reached: %b" fp_reached);
          if fp_reached then
            (* We found an invariant on head. Perform one last,
               possibly descending, iteration, to continue and to
               register alarms in the loop. Note that this also
               performs localized widening
               [amato_scozzari2013localizing_widening_narrowing]. *)
            let newstate = Option.map (fun st -> D.leave_loop st) newstate in
            let acc = update head newstate save_acc in
            fixpoint_partition acc part
          else loop newstate
        in
        loop (Some head_prestate)
      end

  and fixpoint_partition acc list =
    List.fold_left fixpoint_component acc list

end
