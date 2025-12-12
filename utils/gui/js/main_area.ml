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

open Vdom
open Vdom_ext;;

module Pane = struct
  type t = 
    | Trace_pane
    | Source_pane
    | Disassembly_pane
    | CFG_pane
    (* | Values_pane *)
    (* | SSA_pane *)
  [@@deriving eq,ord]

  let name = function
    | Trace_pane        -> "Trace"
    | Source_pane       -> "Source"
    | Disassembly_pane  -> "Disassembly"
    | CFG_pane          -> "Control-flow graph"
    (* | Values_pane       -> "Values" *)
    (* | SSA_pane          -> "SSA" *)
  
  (* let manual model =     function *)
  (* | Trace_pane        -> ["Click line content to select"; "Navigate with arrows once selected"; "Ctrl+click to goto disassembly line";"Toggle collapse: Enter"; "Hello"; "World"] *)
  (* | Disassembly_pane -> ["p: Previous line"; "n: Next line"; "Click to select and highlight on the CFG"] *)
  (* | CFG_pane -> ["Click node to select and highlight in disassembly"] *)

end

module PaneSet = Set.Make(Pane)


(* TODO: Better distinguish between the panes that are not displayed,
   but could, and those that are not active because the model does not
   exists. *)
type model = {
  current_pane: Pane.t;
  hovered_pane: Pane.t option;
  active_panes: PaneSet.t;      (* The panes that are displayed. *)

  (* Invariant: there is no disassembly pane if there are no model. *)
  disassembly: Disassembly.model option;
  cfg:  Cfg.model option;
  source: Sourceview.model option;
  trace: Trace.model;
}




type internal_message =
  | Set_hovered_pane of Pane.t option (* None = nothing selected. *)
  | Set_current_pane of Pane.t
  | Cfg_out of Cfg.outgoing_message
  | Source_out of Sourceview.outgoing_message
  | Disassembly_out of Disassembly.outgoing_message
  | Trace_out of Trace.outgoing_message
  | Close of Pane.t

type incoming_message =
  | Internal_in of internal_message

type outgoing_message =
  | Internal_out of internal_message
  | Display_modal: 'a Modal.t * ('a option ->  internal_message) -> outgoing_message      

let lift_source_messages = function
  | Sourceview.Display_modal (m, f) ->
      Display_modal (m, fun res ->
        Source_out (Sourceview.Internal_out (f res)))
  | msg ->
      Internal_out (Source_out msg)
let lift_cfg_messages = fun msg -> Internal_out (Cfg_out msg)
let lift_disasm_messages = function
  | Disassembly.Display_modal(m,f) ->
    Display_modal(m, fun res -> Disassembly_out (Disassembly.Internal_out (f res)))
  | msg -> Internal_out (Disassembly_out msg)
let lift_trace_messages = function
  | Trace.Display_modal(m,f) ->
    Display_modal(m, fun res -> Trace_out (Trace.Internal_out (f res)))
  | msg -> Internal_out (Trace_out msg)

type initial_data = unit

(* Note: you cannot send message during init. *)
let init() = begin
  let open Static_data in
  let marshalled = static_data.marshalled in
  (* TODO: THe ranges_map should be part of the static data, and computed at the beginning. *)
  let ranges_map = Hashtbl.create 100  in

  let trace,trace_cmd = Trace.init marshalled in
  let trace_cmd = Vdom.Cmd.map lift_trace_messages trace_cmd in


  let cfg,cfg_cmd = match marshalled.cfg, static_data.source_graph with
    | None, None -> None, Vdom.Cmd.batch []
  (* There is an invariant that both are simultanously none or some. *)
    | None, Some _ | Some _, None -> assert false
    | Some cfg, Some source_graph ->
      let cfg,cfg_cmd = Cfg.init (cfg,source_graph, ranges_map) in
      let cfg_cmd = Vdom.Cmd.map lift_cfg_messages cfg_cmd in
      Some cfg,cfg_cmd
  in

  let disassembly,disasm_cmd = match static_data.disassembly with
    | None -> None, Vdom.Cmd.batch []
    | Some disassembly ->
      let disassembly = Option.get static_data.disassembly in
      let disassembly, disasm_cmd = Disassembly.init (disassembly,ranges_map) in
      let disasm_cmd = Vdom.Cmd.map lift_disasm_messages disasm_cmd in
      Some disassembly,disasm_cmd
  in

  let source,src_cmd = match static_data.source with
    | None -> None, Vdom.Cmd.batch []
    | Some source ->
      let source = Option.get static_data.source in
      let source, src_cmd = Sourceview.init source.text in
      let src_cmd = Vdom.Cmd.map lift_source_messages src_cmd in
      Some source,src_cmd
  in


  let active_panes =
    PaneSet.singleton Trace_pane
    |> (fun x -> if Option.is_some disassembly then PaneSet.add Disassembly_pane x else x)
    |> (fun x -> if Option.is_some cfg then PaneSet.add CFG_pane x else x)
    |> (fun x -> if Option.is_some source then PaneSet.add Source_pane x else x)       
  in
  let model =
    { disassembly; cfg; trace; source; 
      current_pane = Trace_pane;
      hovered_pane = (* None; *) Some Trace_pane;
      active_panes } in
  model,
  Command.batch [trace_cmd;cfg_cmd;disasm_cmd;src_cmd]
end


let view_pane state width idx id =
  (* We alternate colors for panes, but use a darker background
     for the active pane. *)
  let color =
    match state.current_pane, state.hovered_pane with
    | p, _ when p = id -> " bg-gray-300 outline-2 z-10"
    | _, Some p when p = id -> " bg-gray-200"
    | _ -> begin
        if idx mod 2 = 0
        then " bg-gray-50"
        else " bg-gray-100" 
      end in
  let title = Pane.name id in
  let content = match id with
    | Pane.Source_pane -> Vdom.map lift_source_messages @@
      Vdom.memo ~key:"Source" Sourceview.view (Option.get state.source)
    | Pane.Disassembly_pane -> Vdom.map lift_disasm_messages @@
      Vdom.memo ~key:"Disassembly" Disassembly.view (Option.get state.disassembly)
    | Pane.CFG_pane -> Vdom.map lift_cfg_messages @@
      Vdom.memo ~key:"Cfg" Cfg.view (Option.get state.cfg)
    | Pane.Trace_pane -> Vdom.map lift_trace_messages @@ Vdom.memo ~key:"Trace" Trace.view state.trace
  in
  div ~a:[class_ ( (* width ^ " " ^  *)color ^ " p-2 flex-grow flex-shrink flex flex-col");
          onmousedown (fun _ -> Internal_out (Set_current_pane id));
          onmouseenter (fun _ -> Internal_out (Set_hovered_pane (Some id)))]
    (* [ div ~a:[class_ "h-full"] *)
        [ h2 ~a:[class_ "text-lg font-semibold pb-4"] [text title]
        ; content ]
  

let view state = 
  (* Main Content *)
  div ~a:[class_ "min-w-0 min-h-0 flex-grow flex";
          onmouseleave (fun _ -> Internal_out (Set_hovered_pane None))]
    (let width = match PaneSet.cardinal state.active_panes with
        | 0 | 1 -> "w-full"
        | 2 -> "w-1/2"
        | 3 -> "w-1/3"
        | 4 -> "w-1/4"
        | 5 -> "w-1/5"
        | _ -> assert false in
     PaneSet.elements state.active_panes |> List.mapi  (fun idx id ->
         (* Note: we set min-width to 0 so that the width of the pane
            is set by the flex, not by the content, and thus that it
            shrinks. *)
         div ~a:[class_ "min-w-0 flex-1 flex-grow flex-shrink flex flex-col"]
           [view_pane state width idx id]))


let update_hovered_pane model p =
  Vdom.return { model with hovered_pane = p }


(* Note: Instead, I could have three functions: update_cfg,
   update_disasm, and update_trace.

   No need for incoming message. *)

let update_cfg model msg = 
    (* If called, we should have a cfg model. *)
    let cfg,cmd = Cfg.update (Option.get model.cfg) msg in
    {model with cfg = Some cfg},
    Vdom.Cmd.map lift_cfg_messages cmd

let update_disassembly model msg =
    (* If called, we should have a disassembly model. *)
    let disassembly = Option.get model.disassembly in
    let disassembly,cmd = Disassembly.(update disassembly msg) in
    {model with disassembly = Some disassembly}, Vdom.Cmd.map lift_disasm_messages cmd


let update_source model msg =
    (* If called, we should have a source model. *)
    let source = Option.get model.source in
    let source,cmd = Sourceview.(update source msg) in
    {model with source = Some source}, Vdom.Cmd.map lift_source_messages cmd


let update_trace model msg =
    let trace,cmd = Trace.(update model.trace msg) in
    {model with trace}, Vdom.Cmd.map lift_trace_messages cmd

    
(* We structure the update as a mapping from out msg to in msg. *)
let update model (Internal_in msg) =
  match msg with
  | Cfg_out (Cfg.Internal_out im) -> update_cfg model (Cfg.Internal_in im)
  | Disassembly_out (Disassembly.Internal_out im) ->
    update_disassembly model (Disassembly.Internal_in im)
  | Disassembly_out (Disassembly.Trace_update_filter x) ->
    update_trace model (Trace.Update_filter x)
  | Disassembly_out(Display_modal _) -> assert false (* Transformed elsewhere, move here? *)
  | Trace_out (Trace.Internal_out im) ->
    update_trace model (Trace.Internal_in im)
  | Trace_out (Trace.Disassembly_goto msg) ->
    update_disassembly model (Disassembly.Direct_goto msg)
  | Trace_out (Display_modal _) -> assert false (* Transformed elsewhere, move here? *)
  | Set_hovered_pane p -> update_hovered_pane model p
  | Set_current_pane p ->
    Vdom.return {model with current_pane = p }
  | Source_out (Sourceview.Internal_out im) ->
    update_source model (Sourceview.Internal_in im)
  | Trace_out (Trace.Sync_selection addr) ->
    let model, cmd1 = update_disassembly model (Disassembly.select_line addr) in
    let model, cmd2 = update_cfg model (Cfg.select_line addr) in
    model, Vdom.Cmd.batch [cmd1; cmd2]
  | Source_out (Display_modal (_, _)) -> assert false
  | Close pane -> Vdom.return {model with active_panes = PaneSet.remove pane model.active_panes}

    
    


(* TODO: s for source. Maybe: a v prefix, for view (instead of pane) *)
let main_menu =
  [ (Pane.Trace_pane, "t");
     (Pane.Disassembly_pane, "d");
     (Pane.CFG_pane, "c") ]


let pane_killer model =
  let open Transient_menu in
  {
    highlight= false;
    keys= [Char "k"];
    desc= "kill current pane";
    action= Internal_out (Close model.current_pane);
  }

let main_menu model =
  let open Transient_menu in
  {
    title= "Panes";
    entries=
      main_menu
      |> List.map (fun (pane, key) ->
             {
               highlight= model.current_pane = pane;
               keys= [Char key];
               desc= Pane.name pane;
               action= Internal_out (Set_current_pane pane);
             })
      |> List.cons (pane_killer model)
      |> Array.of_list;
  }

let get_menu model =
  let stack =
    let main_menu = main_menu model in
    match model.current_pane with
    | panel -> begin
        let stack = match panel with
          | (Trace_pane) ->
            Transient_menu.map_stack lift_trace_messages @@ Trace.get_menu model.trace
          | (Disassembly_pane) ->
            Transient_menu.map_stack lift_disasm_messages @@ Disassembly.get_menu @@ Option.get model.disassembly
          | (CFG_pane) ->
            Transient_menu.map_stack lift_cfg_messages @@ Cfg.get_menu @@ Option.get model.cfg
          | (Source_pane) ->
            Transient_menu.map_stack lift_source_messages @@ Sourceview.get_menu @@ Option.get model.source
        in main_menu::stack
      end
  in
  stack
