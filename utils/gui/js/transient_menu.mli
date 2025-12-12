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

(** {1 Menu} *)

type key =
    Char of string
  | ArrowDown
  | ArrowUp
  | ArrowLeft
  | ArrowRight
  | Enter
  | Escape


(** A menu entry maps a set of keys to some action.  The description
    is a short description that tells what the key does. If highlight
    is true, the menu entry is highlighted. *)
type 'a entry = {
  keys : key list;
  desc : string;
  action : 'a;
  highlight : bool;
}


(** A menu block is a title and an array of entries. *)
type 'a block = { title : string; entries : 'a entry array; }

(** A menu is a stack of blocks, where the top element is displayed on
    the right. *)
type 'a stack = 'a block list


val map_block: ('a -> 'b) -> 'a block -> 'b block
val map_stack: ('a -> 'b) -> 'a stack -> 'b stack

val view : 'a stack -> 'a Vdom.vdom

(** {1 Helpers to receive keyboard events. } *)


(* Note: I can hide all of those, except for the last. *)
val handle_key_events: 
  ?prevent_default:unit ->
  ?stop_propagation:unit ->
  'a stack ->  
  'a Vdom.attribute
