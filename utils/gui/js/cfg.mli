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

type internal_message

type outgoing_message =
  | Internal_out of internal_message

type incoming_message =
  | Internal_in of internal_message


type initial_data =
  Interface.cfg *
  Static_data.source_graph *
  (int64 * int64, Set.Make(String).t) Hashtbl.t


include Component.S with type incoming_message := incoming_message
                     and type outgoing_message := outgoing_message
                     and type initial_data := initial_data

val get_menu: model -> outgoing_message Transient_menu.stack
val select_line : int64 -> incoming_message
