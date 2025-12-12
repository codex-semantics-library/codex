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

(** Line_viewer: helper module that helps display a list of lines,
    with a "current" line being highlighted.

   The module handles things like height computation, recenter when
   the current line becomes invisible, etc. *)

module Make(Row: sig

    (** A model expressing if a row is displayed. Just use unit if every line is visible. *)
    type visibility

    (** An identifier for the lines being displayed. *)
    type t
    
    (** Outgoing message from the Row.*)
    type message

    (** Should the row be displayed on the viewport. *)
    val is_visible : visibility -> t -> bool

    (** How to go from one visible line to the next visble one, or
        None if we cannot (e.g., last line). *)
    val next_visible : visibility -> t -> t option

    (** How to go from one (visible or invisible) line to the
        previously visible one, or None if we cannot (e.g., first
        line). *)
    val previous_visible : visibility -> t -> t option

    (** Comparison between row models. Used to know if one row is the current. *)
    val equal : t -> t -> bool

    (** Comparison between row models. Used to compute scroll effects. *)
    val compare : t -> t -> int

    (** Display of the text of the line, and a string containing the
        background color. Note that the height is fixed. *)
    val view : visibility -> t -> cur:t -> string option * message Vdom.vdom

    (** [preferred_visible_span cur = Some(a,b)] means that when we
        need to scroll to make the current line visible, we should try
        to make all the lines in (a,b) (where a <= cur <= b) visible
        (and we will center on that span).
        
        If [None] (or the constraint cannot be satisfied), we only
        center on the current line. *)
    val preferred_visible_span: t -> (t * t) option
    
    (** The fixed height of the line, in pixels. *)
    val row_height : int
  end):sig

  (** The line viewer has a standard Component.S interface *)
  
  type internal_message
  type incoming_message = Internal_in of internal_message
  type outgoing_message = 
  | Internal_out of internal_message
  | Custom_out of Row.message

  include Component.S
    (* Initial visibility, and first visible row.  *)            
    with type initial_data = Row.visibility * Row.t 
     and type incoming_message := incoming_message
     and type outgoing_message := outgoing_message

  (** Tell the line viewer that the visibility of some lines has
     changed. (possibly this creates a scroll, move the current line
     if no longer visible, we may want to animate the fact that some
     lines appear and disappear, etc. *)
  val update_visibility: model -> Row.visibility -> model
  
  

  (** Update the model to move the current line to the given one. If
      the line is already visible, we will highlight it; otherwise, we
      scroll so that the line appears at the center of the screen. *)
  val go_to_line: model -> Row.t -> model

  (** Returns the current line. *)
  val current: model -> Row.t
  
end
