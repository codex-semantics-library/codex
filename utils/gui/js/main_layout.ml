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

(** This displays the main components of the apps.

    The layout is as follow:
    - We have a banner on top (with possibly some information)
    - We have a footer with with the keyboard menus (list of key
    bindings) and the minibuffer;

    - In the middle, we have a set of panes with useful
    information. (Possibly we could have two rows of such panes,
    which could make sense on large screens).

    Not all panes will be displayed at the same time, but the idea is
    to select the ones that make sense (possibly some will be
    automatically shown when requested, e.g. the trace may display the
    disassembly, clicking on values may bring up the value pane, etc.).

    The keybindings depend on the active pane. *)
open Vdom;;
open Vdom_ext;;

(* We use an extensible type to handle the circular type definition. *)
type internal_message = ..

type outgoing_message =
  | Internal_out of internal_message

type incoming_message =
  | Internal_in of internal_message


module My_Display_Modal = Modal.Display_modal(struct type t = internal_message end)

type internal_message +=
  | Main_area of Main_area.internal_message
  | In_display_modal of My_Display_Modal.internal_message
  | Request_modal: 'a Modal.t * ('a option ->  internal_message) -> internal_message

type model =
  { main_area: Main_area.model;
    modal: My_Display_Modal.model
  }


type initial_data = Main_area.initial_data

(* Displaying a modal loses focus, so we restore it manually.
   This is a bit hackish, but OK. *)
let main_layout_str, main_layout_id =
  let module M = Unique_prefix.Make(struct let name = "main_layout" end)() in
  M.prefix, M.id ""

let restore_focus() =
  let open Js_browser in
  Document.get_element_by_id document main_layout_str
  |> Option.get
  |> Element.focus

let lift_main_area = function
  | Main_area.Internal_out x -> Internal_out (Main_area x)
  | Main_area.Display_modal(m,f) ->
    Internal_out (Request_modal(m, fun res -> Main_area(f res)))

let lift_modal = function
  | Modal.Internal_out msg -> Internal_out (In_display_modal msg)
  | Modal.Result msg ->
    restore_focus();
    Internal_out msg


let init id =
  let main_area,cmds = Main_area.init id in
  let cmds1 = Vdom.Cmd.map lift_main_area cmds in
  let modal, cmds = My_Display_Modal.init() in
  let cmds2 = Vdom.Cmd.map lift_modal cmds in
  let cmds = Command.concat cmds1 cmds2 in
  {main_area;modal}, cmds

let update model = function
  | Internal_in (Request_modal(m,f)) ->
    let modal,cmds = My_Display_Modal.update model.modal (Display_modal (m,f)) in
    let cmds = Vdom.Cmd.map lift_modal cmds in
    { model with modal}, cmds

  | Internal_in (In_display_modal m) ->
    let modal,cmds = My_Display_Modal.update model.modal (My_Display_Modal.Internal_in m) in
    let cmds = Vdom.Cmd.map lift_modal cmds in
    { model with modal}, cmds


  | Internal_in (Main_area layout_message) ->
    let main_area, cmds = Main_area.(update model.main_area (Internal_in layout_message)) in
    {model with main_area}, Vdom.Cmd.map lift_main_area cmds
  |_ -> assert false

(* Note: a suitable stragety for keyboard input is also to have a
   hidden textarea helement that always has the focus; and we react to
   its input. The benefit is that it allows input composition, copy
   and paste, and mobile support. We create a text area, and on
   update, we store the input and clear the textarea, so as to receive
   only new characters. *)


(* We give the focus either to the main thread, or to the modal. *)
let focus = [Vdom.Attribute("tabindex","0"); Vdom.Attribute("autofocus","true")]


let view_main_area {main_area} is_modal =
  let menu_stack = Main_area.get_menu main_area in
  let menu_stack = Transient_menu.map_stack lift_main_area menu_stack in
  (* let autofocus = if is_modal then [] else [Vdom.autofocus] in *)
  div ~a:(focus@[
           main_layout_id;
           class_ "h-screen flex flex-col";
           Transient_menu.handle_key_events ~prevent_default:() menu_stack;
          (* Transient_menu.onkeydown ~prevent_default:() (fun ke -> Messages.Keyboard ke); *)
          (* on "keydown" ~prevent_default:() custom_decoder *)
          ])
    [
      (* Fixed Header *)
      (* header ~a:[class_ "z-100 fixed top-0 left-0 w-full bg-gray-800 text-white p-4"] *)
      header ~a:[class_ "flex-none bg-gray-800 text-white p-4"]
        [ h1 ~a:[class_ "text-xl"] [text "Codex analysis results"] ];

      main ~a:[class_ "overflow-hidden flex flex-1"]
        [ Vdom.map lift_main_area @@ Main_area.view main_area ];

      (* footer ~a:[class_ "z-100 text-sm fixed bottom-0 left-0 w-full"] *)
      footer ~a:[class_ "flex-none text-sm"]
        (* Keyboard menus. *)
        [Transient_menu.view menu_stack]

    ]


let view model =
  let is_modal = My_Display_Modal.is_modal model.modal in
  let main_area = view_main_area model is_modal in
  let modal_view = Vdom.map lift_modal @@ My_Display_Modal.view model.modal in
  Vdom.fragment [modal_view;main_area]
