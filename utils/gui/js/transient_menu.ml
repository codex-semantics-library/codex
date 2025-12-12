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

type key =
  | Char of string
  | ArrowDown
  | ArrowUp
  | ArrowLeft
  | ArrowRight
  | Enter
  | Escape


let keyboard_string_of_key = function
  | Char s -> s
  | ArrowUp -> "ArrowUp"
  | ArrowDown -> "ArrowDown"
  | ArrowRight -> "ArrowRight"
  | ArrowLeft -> "ArrowLeft"
  | Enter -> "Enter"
  | Escape -> "Escape"
    

let printed_string_of_key = function
  | Char s -> s
  | ArrowUp -> "↑"
  | ArrowDown -> "↓"
  | ArrowRight -> "→"
  | ArrowLeft -> "←"
  | Enter -> "⏎ "
  | Escape -> "Esc"    

let printed_string_of_key_list l =
  String.concat "," @@ List.map printed_string_of_key l


(** A menu entry is a triple (key list, description, action):
    - The key list is the list of keys that can trigger the action;
    - The description is the description of what the action does;
    - Action is an update function that does the action. *)

(* TODO:
   - make it a record;
   - a notion of being active; for instance if we hare already viewing the disassembly component. 

*)

type 'a entry =
  { keys: (key list);
    desc: string;
    action: 'a;
    highlight: bool;
  }

(** A menu block is a title and an array of entries. *)
type 'a block = { title: string;
                  entries: 'a entry array }

(** A menu is a stack of blocks, displayed from left to right. *)
type 'a stack = 'a block list


let map_block f menu =
  { title = menu.title;
    entries = menu.entries |> Array.map (fun entry -> {entry with action=f entry.action})}

let map_stack f s = List.map (map_block f) s

(**************** Keyboard menu display. ****************)


open Vdom
open Vdom_ext

(* We manually compute an HTML table to display the list of commands.

   - Each entry of the menu occupies two cells, one right-aligned for
   the keychord, and one left-aligned for the description. We add a
   pr-1 to the right of chords for some separation, and pr-2 between
   consecutive columns.

   - The table is divided in groups of column, corresponding to each
   block. On top of each of them we have the title for the block.
   Note that the display is stable because blocks are organised as a
   stack, and we are only adding new columns to the stack.

   - The problem with HTML tables is that we have to adjust padding
   and borders by doing it on every cell. So for cells at the left of
   a block, we add a left padding "pl-2". At the right, we add a right
   padding "pr-2" and a border "border-r".
   
   MAYBE: this could be done using a flexbox whose elements would be
   arrays. This would allow command menus on several lines if one is
   not enough. *)


(* Global constant, setting the height of the command menu. To
   increase if there are many commands. *)
let num_rows = 2

(* Number of logical columns needed to display a block.  The
   "physical" column is twice that, because we need to columns for
   each entry (one for the keychord, one for the description). *)
let block_columns block =
  max 1 @@ (Array.length block.entries + (num_rows - 1)) / num_rows


(* let colspan x = Vdom.Property("colspan", Int x) *)
let colspan x = Vdom.attr "colspan" (string_of_int x)

let border_style = " border-dotted border-r border-gray-300 pr-2"

let view_head stack =
  thead
    [ tr
        (stack |> List.map (fun menu ->
             let columns = block_columns menu in
             th ~a:[class_ ("pl-2 text-left underline" ^ border_style); colspan (2*columns)]
               [text menu.title])) ]

(* Returns the pair of cells displayed at coordinate (row i, column j)
   of the current menu. The number of columns for the menu is columns. *)
let view_entry i j columns menu =
  (* let idx = i * columns + j in *)
  let idx = j * num_rows + i in
  let border_r = if j = (columns - 1) then border_style else "" in
  let pl = if j = 0 then " pl-2" else ""  in
  (* We separate blocks by a right border, that we have to put on the last element of each row. *)
  if idx < Array.length menu.entries
  then
    let {highlight;keys;desc;action} = menu.entries.(idx) in
    (* We can't change the font weight as this change the size. We
       cannot set the background because of the gap between the two
       cells. Maybe we could if we highlight the padding too. *)
    let highlight text = if highlight then
        span ~a:[class_"text-yellow-300"] [text]
      else text
    in

    let link_style = " cursor-pointer hover:underline" in
    let link_action = onclick (fun _ -> action) in

    let fst = 
      (td ~a:[class_ @@ "text-right text-gray-300 pr-1 " ^ pl ^ link_style; link_action]
         [highlight @@ text (printed_string_of_key_list keys)]) in
    let snd =
      (td ~a:[class_ @@ "text-left pr-2" ^ link_style ^ border_r; link_action]
         [highlight @@ text desc]) in
    (fst,snd)
  else                          (* Empty cell. *)
    let fst = (td ~a:[class_ pl] [text ""]) in
    let snd = (td ~a:[class_  border_r] [text ""]) in
    (fst,snd)

let view_row i stack =
  (* let entries_per_row = stack |> List.map block_columns |> List.fold_left (+) 0 in *)
  let l = ref [] in
  stack |> List.iter (fun menu ->
      let columns = block_columns menu in
      for j = 0 to columns - 1 do
        let (fst,snd) = view_entry i j columns menu in
        l:= snd::fst::!l
      done);
  tr ~a:[] @@ List.rev !l

let view_keyboard_menu stack =
  div  ~a:[class_ "w-full bg-gray-800 text-white text-sm pt-1 pb-1"]
    [ table ~a:[class_ "table-fixed border-collapse"]
        [ view_head stack
        ; tbody
          [ view_row 0 stack
          ; view_row 1 stack  ]
        ]
    ]

let view stack = view_keyboard_menu stack

let handle_key_events (type a) ?prevent_default ?stop_propagation stack =

  let key_event_match_entry ke entry =
    List.exists (fun entrykey ->
        String.equal ke.key (keyboard_string_of_key entrykey)) entry.keys
  in


  (* Linear pass should be sufficiently fast. *)
  onkeydown ?prevent_default ?stop_propagation (fun ke ->
      (* Js_browser.Console.log Js_browser.console @@ Obj.magic @@ *)
      (* Format.asprintf "ke.key: %s" ke.key; *)

      let exception Found of a entry in
      try
        (* We prioritize deep menu entries, hence the rev. *)
        (List.rev stack) |> List.iter (fun block ->
            match block.entries |> Array.find_opt (key_event_match_entry ke) with
            | None -> ()
            | Some e -> raise @@ Found e);
        Js_browser.Console.log Js_browser.console @@ Obj.magic @@
        Format.asprintf "Ignoring unknown key %s" ke.key;
        assert false            (* TODO: ignore with None. *)
      with Found e -> e.action)
