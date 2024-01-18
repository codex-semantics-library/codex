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

(* Map of terms to their associated values. Abstract values are couple
   (term,abstraction), where the abstraction is provided by D. Only
   terms are manipulated; the associated value is computed on demand
   using "eval". *)

open Domain_sig;;


(* Note: does no longer need to be a full domain; we do not need to
   build a term, just to evaluate it. Or should we keep this
   possibility? *)
module Make_Memory_Binary_Boolean(D:Domain_sig.Base):
sig
  (* include Domain_sig.Base with type binary = Term.t *)
  (*                                 and type boolean = Term.t *)
  (*                                 and type memory = Term.t *)
  (*                                 and type tuple = Term.t *)
  (*                                 and module Ref_Addr = D.Ref_Addr *)
  (*                                 and module Query.Boolean = Lattices.Quadrivalent *)
  (*                                 and module Query.Integer = D.Query.Integer *)
  (*                                 and module Query.Binary = D.Query.Binary *)

  (* (\* Note: the backward propagation on terms does nothing, because it *)
  (*    should be done on the underlying domain D. *\) *)
  (* include With_Memory_Empty with type memory := memory *)
  (* include With_Binary_Empty with type binary := binary *)
  (*                            and module Context := Context *)
  (* include With_Boolean_Empty with type boolean := boolean *)


  type root_context
  (* module Context:sig *)
  (*   type t *)
  (* end *)
  include With_Queries with type memory = Term.t
                        and type binary = Term.t
                        and type integer = Term.t
                        and type boolean = Term.t
                        and module Query.Boolean_Lattice = Lattices.Quadrivalent
  val root_context: unit -> root_context
  val root_context_upcast: root_context -> Context.t
      
  
  (* MAYBE:  evaluating_lattice could provide a bottom without requiring a bottom.
     This would be done by not evaluating syntactically bottom terms. *)
  val eval: Context.t -> Term.t -> unit

  (* Evaluate a term, and retrieve the associated value. Note that
     unlike queries, this return a dimension identifier, and not a
     lattice. *)
  val eval_boolean: Context.t -> Term.t -> D.boolean
  val eval_binary: Context.t -> Term.t -> D.binary
  val eval_memory: Context.t -> Term.t -> D.memory

  (* Retrieve the context of the underlying domain. *)
  val domain_context: Context.t -> D.Context.t

  (* val retrieve_boolean_term: Fresh_id.t -> Context.t -> boolean -> Term.t;; *)
  (* val retrieve_reachable_memory_term: Fresh_id.t -> Context.t -> memory -> Term.t;;   *)
  
  (* Note: the partitionning part is actually optional; we could
     define a functor without it.  But it is just as convenient to
     give a useless split_boolean in the argument.  *)
  (* include With_Partitionning with type boolean := boolean and type 'a decision = 'a D.decision *)
end
