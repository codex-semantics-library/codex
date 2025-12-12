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

(** The lattice for four-valued boolean logic. It represents the
   powerset of [{true,false}]. See {!Lattices.Quadrivalent}.

   The transfer functions are implemented naturally following the
   Galois connection between this lattice and the [{true,false}]
   powerset. For instance, {m A \land^{\#} B = \alpha(\{\, a \land b \mid a \in \gamma(A),\ b \in \gamma(B) \,\})}  
   The operations on {!True}, {!False} and {!Top} correspond to Kleene's strong
   three-valued logic.

   Those on {!True}, {!False} and {!Bottom} correspond to Kleene's weak
   three-valued logic (= Bochvar three-valued logic).

   However, note that this four-valued logic is different Belnap's
   four-valued logic. It should be seen as a combination of Kleene's
   strong and weak ternary logics.

   Note: For the lattice implementor, it may be sometimes difficult to
   know whether the answer to a predicate should be Top or
   Bottom. Implementation should be guided by the Gallois connection
   to the collecting semantics. For instance, when implementing
   abstract equality {m ==^\#}: {m A ==^{\#} B = \gamma( \{ a \in \alpha(A), b \in \alpha(B): a == b \})}

   Thus {m ==^\#} returns {!Bottom} if either A or B concretize only into the
    empty set. *)

open Sva_sig;;

type boolean = Sva_sig.Quadrivalent.boolean = Bottom | True | False | Top
include BOOLEAN with type boolean := boolean

(** The result of [of_bools may_be_false may_be_false]:

     - May be false (i.e. {!False} or {!Top}) if [may_be_false] is true,
     and must not be otherwise (i.e. {!True} or {!Bottom})

     - May be true (i.e. {!True} of {!Top}) if [may_be_true] is true,
     and must not be otherwise (i.e. {!False} or {!Bottom}) *)
val of_bools: may_be_false:bool -> may_be_true:bool -> boolean

(** It attempts to refine the abstract value [older] with the information
    provided by [newer]. *)
val refine: older:boolean -> newer:boolean -> boolean option
