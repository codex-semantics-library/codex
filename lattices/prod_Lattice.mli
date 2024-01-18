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

(* Product with intersection semantics: elements are in the
   intersections of the concretizations. *)

open Lattice_sig

module Prod2(L1:Join_Semi_Lattice)(L2:Join_Semi_Lattice)
  :Join_Semi_Lattice with type t = L1.t * L2.t

module Prod2_With_Bottom(L1:Join_Semi_Lattice_With_Bottom)(L2:Join_Semi_Lattice_With_Bottom)
  :Join_Semi_Lattice_With_Bottom with type t = L1.t * L2.t

module Prod2_With_Inter_Bottom
    (L1:Join_Semi_Lattice_With_Inter_Bottom)
    (L2:Join_Semi_Lattice_With_Inter_Bottom)
  :Join_Semi_Lattice_With_Inter_Bottom with type t = L1.t * L2.t
