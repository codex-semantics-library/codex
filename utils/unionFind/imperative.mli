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

(** Imperative Union Find structures.
    These are mutable and can only grow (i.e. add new equalities), using
    union by size and lazy path compression. *)

(** {2 Nodes}
    Building nodes out of terms, relations and values*)

module MakeSimpleNode
    (Term : Parameters.SIMPLE_GENERIC_TERM)
    (Relation : Parameters.SIMPLE_GENERIC_GROUP) :
sig
    include Parameters.SIMPLE_UF_NODE
       with module Relation = Relation

    val payload : 'a Node.t -> 'a Term.t
    (** Inspect the payload of a node *)

    val make_node : 'a Term.t -> 'a Node.t
    (** Create a new node, in its own class.
        At most one node should be created per term! *)
end

(** Same as [MakeSimpleNode], but also remembers all built node in a [PatriciaTree]
    so we can check if terms already have an associated node *)
module MakeSimpleNumberedNode
    (Term : PatriciaTree.HeterogeneousKey)
    (Relation : Parameters.SIMPLE_GENERIC_GROUP) :
sig
    include module type of MakeSimpleNode(Term)(Relation)

    val get_node : 'a Term.t -> 'a Node.t option
    (** Checks if a node has already been constructed for the given term *)

    val get_or_make_node : 'a Term.t -> 'a Node.t
    (** Returns the node associated with the given term if it exists, else builds it *)
end

module MakeNode
    (Term : Parameters.SIMPLE_GENERIC_TERM)
    (Relation : Parameters.SIMPLE_GENERIC_GROUP)
    (Value : Parameters.SIMPLE_GENERIC_VALUE with type ('a,'b) relation = ('a,'b) Relation.t) :
sig
    include Parameters.UF_NODE
       with module Relation = Relation
        and module Value = Value

    val payload : 'a Node.t -> 'a Term.t
    (** Inspect the payload of a node *)

    val make_node : 'a Term.t -> 'a Value.t -> 'a Node.t
    (** Create a new node with given value, in its own class.
        At most one node should be created per term! *)
end

module MakeNumberedNode
    (Term : PatriciaTree.HeterogeneousKey)
    (Relation : Parameters.SIMPLE_GENERIC_GROUP)
    (Value : Parameters.SIMPLE_GENERIC_VALUE with type ('a,'b) relation = ('a,'b) Relation.t) :
sig
    include module type of MakeNode(Term)(Relation)(Value)

    val get_node : 'a Term.t -> 'a Node.t option
    (** Checks if a node has already been constructed for the given term *)

    val get_or_make_node : 'a Term.t -> 'a Value.t -> 'a Node.t
    (** Returns the node associated with the given term if it exists, else builds it
        The value is only used when creating nodes *)
end

(** {2 Union find structures} *)

(** Imperative union find
    - with generic types (parameterised by 'a)
    - with relations [('a, 'b) Relation.t] on edges between terms and representatives
    - with values attached to each representative
    This interface can easily be reused to remove any of the above features if needed.

    Performs union by size, and path compression in find.

    This is fully imperative and mutable.
    - Create at most one node per ['a Term.t]
    - Do NOT perform [union] between related nodes *)
module GenericRelationalValued(Node: Parameters.UF_NODE) : sig
  open Node

  (** {3 Existential wrappers for the return types of find} *)

  type 'a node_through_relation =
    NodeThoughRelation : 'b Node.t * ('a, 'b) Relation.t -> 'a node_through_relation
  type 'a value_through_relation =
    ValueThroughRelation : 'b Value.t * ('a, 'b) Relation.t -> 'a value_through_relation
  type 'a node_and_value_through_relation =
    NodeValueThroughRelation : 'b Node.t * 'b Value.t * ('a, 'b) Relation.t
        -> 'a node_and_value_through_relation

  (** {3 Find operations} *)

  val find_representative : 'a Node.t -> 'a node_through_relation
  (** Find the representative of a node, and the associated relation *)

  val find_value : 'a Node.t -> 'a value_through_relation
  (** Find the value of a node, and the associated relation *)

  val find : 'a Node.t -> 'a node_and_value_through_relation
  (** Find the value and representative of a node, and the associated relation *)

  (** {3 Other misc operations} *)

  val check_related : 'a Node.t -> 'b Node.t -> ('a, 'b) Relation.t option
  (** [check_related a b] returns the relation between [a] and [b]
      if they are in the same class. *)

  val add_value : 'a Node.t -> 'a Value.t -> unit
  (** [add_value a v] adds with the value [v] added to [a]
      (Or more precisely, the value is added to the representative of [a],
      via the relation between [a] and its representative).
      Intersects with previous value via [Value.meet] if one is present *)

  (** {3 Union operation} *)

  val union : 'a Node.t -> 'b Node.t -> ('a, 'b) Relation.t -> unit
  (** [union m n r] adds the r(m,n) relation to the union find, refining the value
      of the chosen parent class using that of the chosen child via [Value.value_intersection].
      @raise AssertionFailure if both nodes are in the same class *)
end

(** Same as [GenericRelationValued], but without the values *)
module GenericRelational (Node: Parameters.SIMPLE_UF_NODE) :
sig
  open Node

  type 'a node_through_relation =
    | NodeThoughRelation : 'b Node.t * ('a, 'b) Relation.t -> 'a node_through_relation

  val find_representative : 'a Node.t -> 'a node_through_relation
  (** Find the representative of a node, and the associated relation *)

  val check_related : 'a Node.t -> 'b Node.t -> ('a, 'b) Relation.t option
  (** [check_related a b] returns the relation between [a] and [b]
      if they are in the same class. *)

  val union : 'a Node.t -> 'b Node.t -> ('a, 'b) Relation.t -> unit
  (** [union m n r] adds the r(m,n) relation to the union find
      @raise AssertionFailure if both nodes are in the same class *)
end
