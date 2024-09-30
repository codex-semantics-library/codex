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
    Building nodes out of elements, relations and values*)

module MakeSimpleNode
    (Elt : Parameters.SIMPLE_GENERIC_ELT)
    (Relation : Parameters.GENERIC_GROUP) :
sig
    include Parameters.SIMPLE_UF_NODE
       with module Relation = Relation

    val payload : 'a t -> 'a Elt.t
    (** Inspect the payload of a node *)

    val make_node : 'a Elt.t -> 'a t
    (** Create a new node, in its own class.
        At most one node should be created per unique element! *)
end

(** Same as [MakeSimpleNode], but also remembers all built node in a [PatriciaTree]
    so we can check if elements already have an associated node *)
module MakeSimpleNumberedNode
    (Elt : PatriciaTree.HETEROGENEOUS_KEY)
    (Relation : Parameters.GENERIC_GROUP) :
sig
    include module type of MakeSimpleNode(Elt)(Relation)

    val get_node : 'a Elt.t -> 'a t option
    (** Checks if a node has already been constructed for the given element *)

    val get_or_make_node : 'a Elt.t -> 'a t
    (** Returns the node associated with the given element if it exists, else builds it *)
end

module MakeNode
    (Elt : Parameters.SIMPLE_GENERIC_ELT)
    (Relation : Parameters.GENERIC_GROUP)
    (Value : Parameters.SIMPLE_GENERIC_VALUE with type ('a,'b) relation = ('a,'b) Relation.t) :
sig
    include Parameters.UF_NODE
       with module Relation = Relation
        and module Value = Value

    val payload : 'a t -> 'a Elt.t
    (** Inspect the payload of a node *)

    val make_node : 'a Elt.t -> 'a Value.t -> 'a t
    (** Create a new node with given value, in its own class.
        At most one node should be created per element! *)
end

module MakeNumberedNode
    (Elt : PatriciaTree.HETEROGENEOUS_KEY)
    (Relation : Parameters.GENERIC_GROUP)
    (Value : Parameters.SIMPLE_GENERIC_VALUE with type ('a,'b) relation = ('a,'b) Relation.t) :
sig
    include module type of MakeNode(Elt)(Relation)(Value)

    val get_node : 'a Elt.t -> 'a t option
    (** Checks if a node has already been constructed for the given element *)

    val get_or_make_node : 'a Elt.t -> 'a Value.t -> 'a t
    (** Returns the node associated with the given element if it exists, else builds it
        The value is only used when creating nodes *)
end

(** {2 Union find structures} *)

(** Imperative union find
    - with generic types (parameterised by 'a)
    - with relations [('a, 'b) Relation.t] on edges between elements and representatives
    - with values attached to each representative
    This interface can easily be reused to remove any of the above features if needed.

    Performs union by size, and path compression in find.

    This is fully imperative and mutable.
    - Create at most one node per ['a Elt.t]
    - Do NOT perform [union] between related nodes *)
module GenericRelationalValued(Node: Parameters.UF_NODE) :
  Signatures.IMPERATIVE_GENERIC_RELATIONAL_VALUED
    with type 'a t = 'a Node.t
     and type ('a, 'b) relation = ('a, 'b) Node.Relation.t
     and type 'a value = 'a Node.Value.t

(** Same as {!GenericRelationalValued}, but without the values *)
module GenericRelational (Node: Parameters.SIMPLE_UF_NODE) :
  Signatures.IMPERATIVE_GENERIC_RELATIONAL
    with type 'a t = 'a Node.t
     and type ('a, 'b) relation = ('a, 'b) Node.Relation.t
