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

(**  This component displays a graphviz data, with smooth transitions
     when the graph changes. The events that it returns are low-level;
     see [Hierarchical_graph] for a higher-level encapsulation. *)


type initial_data = string
(** The dot text representing the graph to be displayed. *)

type elt_kind = Cluster | Node | Edge | Nothing

type event_type = Mouseenter | Mouseleave | Mousemove | Click

type outgoing_message =
  | Event of { kind:elt_kind; elt:Js_browser.Element.t;event_type:event_type}

type incoming_message =
  (** Change the background color of a node; no redraw necessary. *)
  | Set_background_color of {id:string; color:string }

  (** Change the graph definition and redraw. Necessary when the shape changes. *)
  | Redraw of {dot:string}

include Component.S with type incoming_message := incoming_message
                     and type outgoing_message := outgoing_message
                     and type initial_data := initial_data
