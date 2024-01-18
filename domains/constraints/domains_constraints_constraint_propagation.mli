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


(* Implementation options, on which constraint_domain2 depends. *)
val option_push_and_pop: bool
val option_push_and_pop_on_restart: bool

module Make
    (Constraints:Constraints.Constraints_sig.Constraints
     (* For now; maybe the condition should come with its map instead. *)                   
     with type Condition.t = Cudd.Man.d Cudd.Bdd.t
     (* with type Condition.t = Constraints_condition.ConditionDom.t *)
    )
    (B:Single_value_abstraction.Sig.Binary_Integer_Basis):sig
  include Constraint_domains_sig.Domain_S
    with module Constraints = Constraints
     and module Query.Boolean_Lattice = B.Boolean_Lattice
     and module Query.Integer_Lattice = B.Integer_Lattice
     and module Query.Binary_Lattice = B.Binary_Lattice
end
