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

let unique = Hashtbl.create 17

module Make(N:sig val name: string end)() = struct


  let prefix =
    let count =
      try Hashtbl.find unique N.name
      with Not_found -> 1
    in
    Hashtbl.replace unique N.name (count + 1);
    (* Common case: instantiated once. *)
    if count = 1 then
      N.name ^ "_"
    else
      N.name ^ (string_of_int count) ^ "_"

    let id str = Vdom.attr "id" (prefix ^ str)
    let int_id num = id (string_of_int num)
  
end
