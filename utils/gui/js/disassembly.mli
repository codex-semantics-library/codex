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

type initial_data = Static_data.disassembly * (int64 * int64, Set.Make(String).t) Hashtbl.t

type internal_message

type incoming_message =
  | Internal_in of internal_message
  | Direct_goto of int64

type outgoing_message =
  | Internal_out of internal_message
  | Display_modal: 'a Modal.t * ('a option ->  internal_message) -> outgoing_message      
  (* The name should not be that. We should say that an element is
     clicked, so that others can act on this.  *)
  | Trace_update_filter of (int64 * Syntax_tree.expr_path * Syntax_tree.expr) option

include Component.S
  with type initial_data := initial_data
   and type incoming_message := incoming_message
   and type outgoing_message := outgoing_message

val get_menu: model -> outgoing_message Transient_menu.stack
val select_line : int64 -> incoming_message
