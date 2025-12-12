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

(* Vdom_ext: Extension to the elements needed to write a view. *)


type 'msg elt = ?key:string -> ?a:'msg Vdom.attribute list -> 'msg Vdom.vdom list -> 'msg Vdom.vdom

val colspan: string -> 'a Vdom.attribute
val placeholder: string -> 'a Vdom.attribute

val body    : 'msg elt
val br      : 'msg elt    
val button  : 'msg elt
val details : 'msg elt    
val div     : 'msg elt
val footer  : 'msg elt
val h1      : 'msg elt
val h2      : 'msg elt
val header  : 'msg elt
val input   : 'msg elt    
val label   : 'msg elt
val li      : 'msg elt
val main    : 'msg elt
val p       : 'msg elt
val pre     : 'msg elt
val span    : 'msg elt
val summary : 'msg elt    
val table   : 'msg elt
val tbody   : 'msg elt    
val td      : 'msg elt
val thead   : 'msg elt
val th      : 'msg elt
val tr      : 'msg elt
val ul      : 'msg elt

(** We redefine [key_event] to have a [key] field instead of [which]
    (which is deprecated), and update [onkeydown] to handle those. *)
type key_event =
  {key:string; ctrl_key:bool;shift_key:bool;alt_key:bool}


(** Like [Vdom.onkeydown], but handles the redefined [key_event]. *)
val onkeydown :
  ?prevent_default:unit ->
  ?stop_propagation:unit ->
  (key_event -> 'a) -> 'a Vdom.attribute


(** Like [Vdom.onkeydown], but allows to not do anything (None), and
    choose whether to stop default behaviour and stop propagation
    depending on the key. *)
val onkeydown_with_options :
  (key_event -> 'a Vdom.msg_options) -> 'a Vdom.attribute


(** When a CSS transition stops. *)
val ontransitionend :
  ?prevent_default:unit ->
  ?stop_propagation:unit ->
  (unit -> 'a) -> 'a Vdom.attribute

val ontransitioncancel :
  ?prevent_default:unit ->
  ?stop_propagation:unit ->
  (unit -> 'a) -> 'a Vdom.attribute
