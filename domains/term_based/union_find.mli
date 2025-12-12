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

(** These functors are placed above a union-find aware non-relational domain
    (i.e. {!Nonrelational.MakeUF}).
    They create union between terms when certain operations are performed
    (for instance {!Term_based_sig.Domain_S.Binary_Forward.biadd})
    with one constant argument. *)


(** Builder for the simple "constant difference relation" [y = x + c].
    This relation is defined at {!Terms.Relations.Additive}. *)
module MakeAdditive
    (C: Term_based_sig.DOMAIN_WITH_UNION
        with type ('a, 'b) Terms.Relation.t = ('a, 'b) Terms.Relations.Additive.t)
  : Term_based_sig.Domain_S
     with type t = C.t
      and module Terms = C.Terms
      and module Query = C.Query

(** Builder for a linear two variable equality relation [f1*x + f2*y + 0],
    also called TVPE.
    This relation is defined at {!Terms.Relations.LinearTwoVarEquality}. *)
module MakeLinearTwoVarEquality
    (C: Term_based_sig.DOMAIN_WITH_UNION
        with type ('a, 'b) Terms.Relation.t = ('a, 'b) Terms.Relations.LinearTwoVarEquality.t)
  : Term_based_sig.Domain_S
     with type t = C.t
      and module Terms = C.Terms
      and module Query = C.Query
