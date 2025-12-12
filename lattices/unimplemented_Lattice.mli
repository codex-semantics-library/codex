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

open Lattice_sig

(* Generation of modules that fail (with information to find out which
   function failed, using the provided id) when a function is called. *)
module Enum_Lattice(UnimplementedId:sig val loc:string end):ENUM_LATTICE

module Bitvector_Lattice(UnimplementedId:sig
    type t
    val loc:string
  end):BITVECTOR_LATTICE with type t = UnimplementedId.t


module Integer_Lattice(UnimplementedId:sig
    type t
    val loc:string
  end):INTEGER_LATTICE with type t = UnimplementedId.t
