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

(** Information about the tooltip. It should be structured as a tree,
    where the root is the top-level expression. The root itself is not
    hoverable, only its content is. *)
module type Tooltip_information = sig
  type t
  val root: t
  val get: t -> string option      (* Information about the current node, if any. *)

  (** [find_child x i] returns an information about the [i]th child of [x]. *)
  val find_child: t -> int -> t option

end

type initial_data = (module Tooltip_information) * Syntax_tree.expr
type internal_message
type incoming_message =
  | Incoming_internal of internal_message
type outgoing_message =
  | ClickInstr of Syntax_tree.expr_path * Syntax_tree.expr
  | HoverInstr of Syntax_tree.expr_path option
  | Outgoing_internal of internal_message

include Component.S with type incoming_message := incoming_message
                     and type outgoing_message := outgoing_message
                     and type initial_data := initial_data
