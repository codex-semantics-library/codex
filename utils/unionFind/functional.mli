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

(** Functional Union Find structures.
    These are immutable and can grow or shrink (add/remove equalities).
    Represented with PatriciaTree (binary tree maps) and eager compression.

    However, their performance isn't as good as imperative union-find's:
    - Imperative UF uses union-by-size and path compression, so the amortized
      complexity of its operations is O(inv_ackermann(n)) (also known as O(1)
      for any n that can be physically represented).
    - Functional UF performs eager compression, and maintains a set of back
      pointers (from representative to class elements), so its complexities are:
      - [find] is O(log n) (single map lookup)
      - [union] is O(c + log(n)) where c is the max of cardinals of the class
        being united. *)

(** Functional union find
    - with generic types (parameterised by 'a)
    - with relations [('a, 'b) Relation.t] on edges between elements and representatives
    - with values attached to each representative
    This interface can easily be reused to remove any of the above features if needed.

    This is functional and immutable *)
module GenericRelationalValued
    (Elt : Parameters.GENERIC_ELT)
    (Relation : Parameters.GENERIC_GROUP)
    (Value : Parameters.GENERIC_VALUE with type ('a,'b) relation = ('a,'b) Relation.t) :
sig
  module EltSet : PatriciaTree.HETEROGENEOUS_SET with type 'a elt = 'a Elt.t
  (** Set of ['a Elt.t] *)

  type t
  (** Type of the union-find structure *)

  val pretty : Format.formatter -> t -> unit
  (** Prints equivalence classes *)

  val pretty_debug : Format.formatter -> t -> unit
  (** Prints whole data structure *)

  val equal : t -> t -> bool
  val subseteq : t -> t -> bool
  val empty : t

  (** {2 Existential wrappers for the return type of find operations} *)

  type 'a elt_through_relation =
    EltThroughRel : 'b Elt.t * ('a, 'b) Relation.t -> 'a elt_through_relation
  type 'a value_through_relation =
    ValueThroughRelation : 'b Value.t option * ('a, 'b) Relation.t -> 'a value_through_relation
  type 'a all_through_relation =
    AllThroughRelation : 'b Elt.t * 'b Value.t option * EltSet.t * ('a, 'b) Relation.t -> 'a all_through_relation

  (** {2 Find operations} *)

  val find_representative : t -> 'a Elt.t -> 'a elt_through_relation
  (** Returns the representative of the given element, along with the relation
      between the given element and its representative. O(1) complexity *)

  val find_class : t -> 'a Elt.t -> EltSet.t
  (** Returns the class of the given element, O(1) complexity *)

  val find_value : t -> 'a Elt.t -> 'a value_through_relation
  (** Returns the value of the given element's representative ([None] for top), along with the relation
      between the given element and its representative. O(1) complexity *)

  val find : t -> 'a Elt.t -> 'a all_through_relation
  (** Returns the given element's representative, associated value ([None] for top),
      along with the relation between the given element and its representative. O(1) complexity *)

  val check_related : t -> 'a Elt.t -> 'b Elt.t -> ('a, 'b) Relation.t option
  (** [check_related uf a b] returns the relation between [a] and [b]
      if they are in the same class. *)

  val add_value : t -> 'a Elt.t -> 'a Value.t -> t
  (** [add_value uf a v] is [uf] with the value [v] added to [a]
      (Or more precisely, the value is added to the representative of [a],
      via the relation between [a] and its representative).
      Intersects with previous value via {!Value.meet} if one is present *)

  (** {2 Union and join} *)

  val union : t -> 'a Elt.t -> 'b Elt.t -> ('a, 'b) Relation.t -> (t, ('a, 'b) Relation.t) result
  (** [union uf a b r] adds the [r a b] relation in [uf].
      Returns [Ok uf'] with the new value if successful, and [Error rel]
      if [a] and [b] are already related by [rel] and [r != rel]. *)

  val join : t -> t -> t
  (** [join a b] joins the two union find-structures:
      - The new classes are the intersection of the old classes
      - The new values are the {!Value.join} of the old values *)

  (**/**)

  (** {2 Functions for tests} *)

  val join_test : t -> t -> t
  (** Super slow join, only used for testing *)

  val check_invariants : t -> (unit, string) result
  (** Checks all invariants hold on the data structure
      Returns an error message detailing the broken one if they don't *)

  (**/**)
end
