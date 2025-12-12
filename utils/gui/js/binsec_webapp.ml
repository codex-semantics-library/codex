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

(** Binsec_webapp: Entry point to the interface. *)

module U = Vdom_blit
module Js = Js_of_ocaml.Js

let init = Main_layout.init()
let view = Main_layout.view
let update model = function
  | Main_layout.(Internal_out msg) ->
    Main_layout.update model (Internal_in msg)

let app = Vdom.app ~init ~view ~update ()

(* Handle the after_redraw command. *)
let env =
  Vdom_blit.merge
    [ Vdom_blit.cmd Command.after_redraw_handler;
      Vdom_blit.cmd Command.resize_observer_handler ]

let vdom_blit_app = Vdom_blit.run ~env app;;

open Js_browser;;

let run () =
  vdom_blit_app
  |> Vdom_blit.dom    (* get its root DOM container *)
  |> Element.append_child  (Document.body document)

let () = Window.set_onload window run
