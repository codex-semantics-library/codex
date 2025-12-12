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

type 'msg Vdom.Cmd.t +=
  | After_redraw of 'msg
  (** Send a message after the next redraw. *)
  | Setup_resize_observer of string * (Js_browser.Rect.t -> 'msg option)
  (** Setup_resize_observer(id,f) will put a resize observer on the
      element of id [id].  When this element is resized, we use f to
      possibly send a message containing the new bounding box. *)

(* The following elements are used to setup the command handler. *)
val after_redraw_handler : Vdom_blit.Cmd.handler
val resize_observer_handler : Vdom_blit.Cmd.handler

(* Helpers with commands; not sure they are really useful.  *)

val empty: 'msg Vdom.Cmd.t
val batch : 'msg Vdom.Cmd.t list -> 'msg Vdom.Cmd.t
val concat : 'msg Vdom.Cmd.t -> 'msg Vdom.Cmd.t -> 'msg Vdom.Cmd.t
module Writer_monad :
sig
  type ('a,'b) m = 'a * 'b Vdom.Cmd.t
  val (let+): ('a,'s) m -> ('a -> 'b) -> ('b,'s) m
  val (let*): ('a,'s) m -> ('a -> ('b,'s) m) -> ('b,'s) m
  val return : ?c:'s Vdom.Cmd.t list -> 'a -> 'a * 's Vdom.Cmd.t      
end
