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

module type S = sig

  (** The state of the application. *)
  type model

  (* Typically, there will be a hidden [internal_message] type used to
     pass some message from outgoing to incoming, without knowing what
     they are outside of the component. *)

  type incoming_message
  (** Messages that the component can handle. Note that sometimes,
      instead of incoming messages, we may have different update
      functions; but this prevents doing the action directly as the
      result of a user action. *)

  type outgoing_message
  (** Messages that the component can trigger. *)

  type initial_data
  (** Constant data used to initialize the model .*)

  val init: initial_data -> model * outgoing_message Vdom.Cmd.t
  (** Initialize the model, and possibly return an message to do. *)

  val update: model -> incoming_message -> (model * outgoing_message Vdom.Cmd.t)
  (** Apply a known message to the model, and possibly return a new message. *)

  val view: model -> outgoing_message Vdom.vdom
  (** When giving a function that lift outgoing_message to global
      messages, draw the model. *)

end

(** A [Self_contained] component only has messages to itself. *)
module type Self_contained = sig
  type internal_message
  type incoming_message = Internal_in of internal_message
  type outgoing_message = Internal_out of internal_message
  include S with type incoming_message := incoming_message
             and type outgoing_message := outgoing_message

end


(* TODO: with the function get-menu.

   Note that the menu adapts with the model (i.e. the current state).
   E.g. for toggle keys, it can say what action it will do. *)

(* TODO: specialised version.
   - Modal version: something with no interaction, except that it returns some datatype.
   - Something for the keyboard interaction.

*)
