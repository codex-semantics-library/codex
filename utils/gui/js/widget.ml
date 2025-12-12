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

(* Basic widget/component library based on tailwindcss.  *)

open Vdom_ext;;
open Vdom

let buttonclassstr = "text-white bg-blue-700 hover:bg-blue-800 font-medium rounded-lg text-sm px-5 py-2.5 me-2 mb-2 focus:outline-none"
let buttonclassstr_red = "text-white bg-red-700 hover:bg-red-800 font-medium rounded-lg text-sm px-5 py-2.5 me-2 mb-2 focus:outline-none"


let red_button ~onclick:f text =
  let onclick = Vdom.onclick (fun evt -> f evt) in
  button ~a:[
        type_ "button";
        class_ buttonclassstr_red;
        onclick
      ] [Vdom.text text]


let button ~onclick:f text =
  let onclick = Vdom.onclick (fun evt -> f evt) in
  button ~a:[
        type_ "button";
        class_ buttonclassstr;
        onclick
      ] [Vdom.text text]
