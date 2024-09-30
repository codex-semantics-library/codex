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

(** Common module types for {!Imperative} and {!Functional} parameters. Defines
    the types of the functor parameters.

    Some vocabulary conventions:
    - An {b elt} or {b element} is the type of elements of the union-find. Here it is
      a generic type {{!GENERIC_ELT.t}['a GENERIC_ELT.t]} (often a GADT/a type where ['a] is phantom)
    - A {b node} is an element plus its parent pointers.
    - A {b relation} is a type {{!GENERIC_GROUP.t}[('a,'b) GENERIC_GROUP.t]} modeling mathematical relation between
      elements. The set of relations has a monoid or group structure.
      Classical union-find only has a single relation:
      [type ('a, 'b) t = Equiv : ('a,'a) t])
    - A {b value} is a type {{!GENERIC_VALUE.t}['a GENERIC_VALUE.t]} of values
      associated with each class (or more precisely, to the representative element
      of each class).
      Values and relations interact via a group action {!GENERIC_VALUE.apply}. *)

(** {2 Elements} *)

(** A type for generic elements stored in our {!Imperative} union-find structures. *)
module type SIMPLE_GENERIC_ELT = sig
  type 'a t
  (** The type of elements (nodes) in the union-find structure *)

  val polyeq : 'a t -> 'b t -> ('a, 'b) PatriciaTree.cmp
  (** polymorphic equality on elements *)
end

(** A type for generic elements stored in our {!Functional} union-find structures.
    This should be a super-type of {!PatriciaTree.HETEROGENEOUS_KEY}. *)
module type GENERIC_ELT = sig
  include SIMPLE_GENERIC_ELT

  val to_int : 'a t -> int
  (** Returns a unique integer identifier associated with the given element (eg
      hash-consed tag). See {!PatriciaTree.HETEROGENEOUS_KEY.to_int} *)

  val pretty : Format.formatter -> 'a t -> unit
  (** Pretty printer *)
end

(** {2 Relations}
    Relations between elements. Classical union find has a single equivalence
    relation, which can be mimicked with [type ('a, 'b) t = Equiv : ('a,'a) t])
    However, we can have more complex relation (any group structure) fairly easily *)

(** A (possibly non-commutative) {{: https://en.wikipedia.org/wiki/Monoid}monoid} structure, used to represent relations.

    {b Assumes} that any module [G] implementing this should verify the axioms of a monoid:
    - {b identity is neutral for compose}.
      For all x, [G.compose x G.identity = G.compose G.identity x = x]
    - {b compose is associative}.
      For all x y z, [G.compose x (G.compose y z) = G.compose (G.compose x y) z]

    where [=] is [G.equal] *)
module type GENERIC_MONOID = sig
  type ('a, 'b) t
  (** The type of elements of the monoid.
      Since these are used to represent relation between our generic union-find
      elements {!GENERIC_ELT.t}, they have two type parameters, so an
      [('a, 'b) t] represents a relation between ['a GENERIC_ELT.t] and
      ['b GENERIC_ELT.t] *)

  val equal : ('a, 'b) t -> ('a, 'b) t -> bool
  (** Equality of relations *)

  val pretty : Format.formatter -> ('a, 'b) t -> unit
  (** Pretty printer for relations *)

  val identity : ('a, 'a) t
  (** The identity relation *)

  val compose : ('b, 'c) t -> ('a, 'b) t -> ('a, 'c) t
  (** Monoid composition, written using the functional convention
      [compose f g] is {m f \circ g}.
      Should be associative, and compatible with {!identity}:
      - For all x, [G.compose x G.identity = G.compose G.identity x = x]
      - For all x y z, [G.compose x (G.compose y z) = G.compose (G.compose x y) z] *)
end

(** A (possibly non-commutative) {{: https://en.wikipedia.org/wiki/Group_(mathematics)}group}
    structure, used to represent relations.

    {b Assumes} that any module [G] implementing this should verify the axioms of a group:
    - {b identity is neutral for compose}.
      For all x, [G.compose x G.identity = G.compose G.identity x = x]
    - {b compose is associative}.
      For all x y z, [G.compose x (G.compose y z) = G.compose (G.compose x y) z]
    - {b inversion}.
      For all x, [G.compose x (G.inverse x) = G.compose (G.inverse x) x = G.identity]

    where [=] is [G.equal] *)
module type GENERIC_GROUP = sig
  include GENERIC_MONOID

  val inverse : ('a, 'b) t -> ('b, 'a) t
  (** Group inversion, should verify for all x:
      [G.compose x (G.inverse x) = G.compose (G.inverse x) x = G.identity] *)
end

(** {2 Values}
    Values associated with each class.
    Values and relation interact via a group action
    {{!SIMPLE_GENERIC_VALUE.apply}[apply]} *)

(** The values associated with each equivalence class in the imperative union-find *)
module type SIMPLE_GENERIC_VALUE = sig
  type ('a, 'b) relation
  (** The type of relations, should match {!GENERIC_MONOID.t}. *)

  type 'a t
  (** The generic type of our values.
      An ['a t] value is associated to each class of our union find whose
      representative has type {{!GENERIC_ELT.t}['a GENERIC_ELT.t]}. *)

  val apply : 'a t -> ('a, 'b) relation -> 'b t
  (** [apply v r] is the value obtained by applying relation [r] to value [v]
      [apply] should be a group action from
      {{!GENERIC_GROUP}[R : GENERIC_GROUP with type ('a,'b) t = ('a,'b) relation]}
      on the value ['a t]. Meaning it should verify the following:
      - [apply v R.identity = v]
      - [apply (apply v r2) r1 = apply v (R.compose r2 r1)] *)

  val meet : 'a t -> 'a t -> 'a t
  (** Intersection of values *)
end

(** The values associated with each equivalence class in the functional union-find *)
module type GENERIC_VALUE = sig
  include SIMPLE_GENERIC_VALUE

  val equal : 'a t -> 'a t -> bool
  (** Equality on values. *)

  val pretty : Format.formatter -> 'a t -> unit
  (** Only required for [Functional.XX.pretty] *)

  val join : 'a t -> 'a t -> 'a t
  (** Union of values, only required for [Functional.XX.join] *)

  val is_top : 'a t -> bool
  (** Only required for {!Functional}, top values can be removed
      from the data structure which allows smaller structures and faster access.
      This function only need to be an over-approximation:
      it can return [false] on top, but returning [true] on non-top leads to
      precision-loss. *)
end

(** {2 Imperative nodes}
    Node representative for imperative union-find.
    Nodes contain both the elements in the union find and the parent pointer.
    Nodes can be built on top of an element type (see {!Imperative.MakeNode} and
    its variants). However, it is sometimes better to define the type of elements
    that contains the pointer (avoids having to keep track of two types: the uf-node
    and the element, as going back and forth can be costly). *)

(** Simple union-find node, without values *)
module type SIMPLE_UF_NODE = sig
  include SIMPLE_GENERIC_ELT
  module Relation : GENERIC_GROUP

  type 'a parent = Node : 'b t * ('a, 'b) Relation.t -> 'a parent | Root
  (** The type of parents. A term either points to:
      - a representative via a relation
      - a root if it is a representative *)

  val get_parent : 'a t -> 'a parent
  val set_parent : 'a t -> 'a parent -> unit
end

(** union-find node with values *)
module type UF_NODE = sig
  include SIMPLE_UF_NODE
  module Value : SIMPLE_GENERIC_VALUE with type ('a,'b) relation = ('a,'b) Relation.t

  type 'a root = { mutable value : 'a Value.t; mutable size : int }
  (** The type of root nodes, attached to each representative *)

  type 'a parent = Node : 'b t * ('a, 'b) Relation.t -> 'a parent | Root of 'a root
  (** The type of parents. A term either points to:
      - a representative via a relation
      - a root if it is a representative *)

  val get_parent : 'a t -> 'a parent
  val set_parent : 'a t -> 'a parent -> unit
end
