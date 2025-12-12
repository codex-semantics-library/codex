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

(** Simple non-relational abstraction, maps terms to the numeric abstraction
    provided in [SVA]. This is for the case where there are no relations
    between the terms (besides equality).  *)
module Make
    (Terms: Terms.Sig.TERMS with type ('a, 'b) Relation.t = ('a, 'b) Terms.Relations.Equality.t)
    (SVA: Single_value_abstraction.Sig.NUMERIC_ENUM) :
  Term_based_sig.Domain_S
    with module Query.Boolean_Lattice = SVA.Boolean_Lattice
     and module Query.Integer_Lattice = SVA.Integer_Lattice
     and module Terms = Terms

(** Make the reduced product of a non-relational domain and a labeled union-find.
    This should then be passed to one of the functions in {!Union_find}, as these
    are the ones that create relation *)
module MakeUF
    (T: Terms.Sig.TERMS)
    (SVA: Single_value_abstraction.Sig.NUMERIC_ENUM)
    (_: Terms.Relations.GROUP_ACTION
         with type bitvector = SVA.bitvector
          and type integer = SVA.integer
          and type boolean = SVA.boolean
          and type enum = SVA.enum
          and type ('a, 'b) relation = ('a, 'b) T.Relation.t) :
  Term_based_sig.DOMAIN_WITH_UNION
    with module Query.Boolean_Lattice = SVA.Boolean_Lattice
     and module Query.Integer_Lattice = SVA.Integer_Lattice
     and module Terms = T
