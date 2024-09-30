(**************************************************************************)
(*  This file is part of the Codex semantics library.                     *)
(*                                                                        *)
(*  Copyright (C) 2013-2024                                               *)
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

open Frama_c_kernel
include Cil_state_builder.Varinfo_hashtbl
  (Kernel_function)
  (struct
    let name = "Ref_Addr_Non_Modular.Varinfo_Enclosing_Function"
    let dependencies = [Ast.self]
    let size = 17
   end)

open Cil_types;;

let fill_it() =
  Globals.Functions.iter(fun kf ->
    let register vi = replace vi kf in
    try
      let fundec = Kernel_function.get_definition kf in
      List.iter register fundec.sformals;
      List.iter register fundec.slocals
    with Kernel_function.No_Definition -> ()
  )
    
let get vi =
  assert (not vi.vglob);
  try find vi
  with Not_found -> fill_it(); find vi
;;

let pretty fmt vi =
  if vi.vglob then
    Format.fprintf fmt "%s" vi.vname
  else
    let kf = get vi in
    Format.fprintf fmt "%s@@%a" vi.vname Kernel_function.pretty kf
;;


