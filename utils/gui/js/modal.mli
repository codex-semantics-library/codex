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

(* A modal is just a temporary component that returns a result.

   TODO: The code that allows displaying a modal should be here too.

*)

type ('result,'internal) outgoing = 
    | Internal_out of 'internal
    | Result of 'result


module type S =
  sig
    type result
    type internal_message
    type incoming_message = Internal_in of internal_message
    type outgoing_message = (result option,internal_message) outgoing (* Return None if no result, i.e. the modal was closed. *)
    include Component.S with type incoming_message := incoming_message
                         and type outgoing_message := outgoing_message
                         and type initial_data = unit

    val title: string
  end
type 'a t = (module S with type result = 'a)

(** This is the code used to display a modal component.

    The model of this component can be asked to display a modal
    element; on exit, this modal component will send an exit message
    (we need to give the function from the modal result to the exit
    message to do so.)

    It should be instantiated once, and its view placed where the
    modal should appear. *)
module Display_modal(Exit_message:sig type t end):sig

  type initial_data = unit
  type internal_message
  type incoming_message =
    | Internal_in of internal_message
    | Display_modal: 'a t * ('a option -> Exit_message.t) -> incoming_message
    (** Ask the module to display a modal, that returns this result on completion. *)

  type outgoing_message = (Exit_message.t, internal_message) outgoing
      
  include Component.S with type initial_data := initial_data
                       and type incoming_message := incoming_message
                       and type outgoing_message := outgoing_message


  (* Returns true if a modal is currently displayed. Useful e.g. for
     autofocus, *)
  val is_modal: model -> bool

end
