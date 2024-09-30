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

module Make
    (C: Constraints.Constraints_sig.Constraints)
    (B: Single_value_abstraction.Sig.Numeric_Basis)
    (_: Constraints.Relations.GROUP_ACTION
         with type binary = B.binary
          and type integer = B.integer
          and type boolean = B.boolean
          and type ('a, 'b) relation = ('a, 'b) C.Relation.t) :
  Constraint_domains_sig.Domain_S
    with module Query.Boolean_Lattice = B.Boolean_Lattice
     and module Query.Integer_Lattice = B.Integer_Lattice
     and module Constraints = C
