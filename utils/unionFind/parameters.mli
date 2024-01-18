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

(** Common module types for [Imperative] and [Functional] parameters. Defines
    the types of the functor parameters.

    Some vocabulary conventions:
    - A {b term} is the type of elements of the union-find. Here it is
      a generic type ['a t] (often a GADT/a type where ['a] is phantom)
    - A {b node} is a term plus its parent pointers.
    - A {b relation} is a type [('a,'b) t] modeling mathematical relation between
      terms. The set of relations has a group structure.
      Classical union-find only has a single relation:
      [type ('a, 'b) t = Equiv : ('a,'a) t])
    - A {b value} is a type ['a t] of values associated with each class (or more
      precisely, to the representative of each class).
      Values and relations interact via a group action [Value.apply]. *)

(** {2 Terms}
    Terms are the elements in our union-find structures. *)

(** A type for generic terms stored in our imperative union-find structures. *)
module type SIMPLE_GENERIC_TERM = sig
  type 'a t
  val polyeq : 'a t -> 'b t -> ('a, 'b) PatriciaTree.cmp
end

(** A type for generic terms stored in our functional union-find structures.
    This should be a subtype [PatriciaTree.HeterogeneousKey]. *)
module type GENERIC_TERM = sig
  include SIMPLE_GENERIC_TERM

  val to_int : 'a t -> int
  val pretty : Format.formatter -> 'a t -> unit
end

(** {2 Relations}
    Relations between terms. Classical union find has a single equivalence
    relation, which can be mimicked with [type ('a, 'b) t = Equiv : ('a,'a) t])
    However, we can have more complex relation (any group structure) fairly easily *)

(** A (possibly non-commutative) group structure, used to represent relations.

    @assumes that any module [G] implementing this should verify the axioms of a group:
    - for all x, [G.compose x G.identity = G.compose G.identity x = x]
    - for all x y z, [G.compose x (G.compose y z) = G.compose (G.compose x y) z]
    - for all x, [G.compose x (G.inverse x) = G.compose (G.inverse x) x = G.identity]
    (where [=] is [G.equal])

    Note: This isn't the "simple group" mathematical concept, this is a simple type
    representing a "group". *)
module type SIMPLE_GENERIC_GROUP = sig
  type ('a, 'b) t

  val compose : ('b, 'c) t -> ('a, 'b) t -> ('a, 'c) t
  val inverse : ('a, 'b) t -> ('b, 'a) t
  val identity : ('a, 'a) t

end

(** Same as [SIMPLE_GENERIC_GROUP], with some extra functions used in the
    functional union-find. *)
module type GENERIC_GROUP = sig
  include SIMPLE_GENERIC_GROUP

  val equal : ('a, 'b) t -> ('a, 'b) t -> bool
  val pretty : Format.formatter -> ('a, 'b) t -> unit
end

(** {2 Values}
    Values associated with each relation class between terms,
    values and relation interact via a group action *)

(** The values associated with each equivalence class in the imperative union-find *)
module type SIMPLE_GENERIC_VALUE = sig
  type ('a, 'b) relation
  type 'a t

  val apply : 'a t -> ('a, 'b) relation -> 'b t
  (** [apply v r] is the value obtained by applying relation [r] to value [v]
      [apply] should be a group action from [R : GENERIC_GROUP with type ('a,'b) t = ('a,'b) relation]
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

  val pretty : Format.formatter -> 'a t -> unit
  (** Only required for [Functional.XX.pretty] *)

  val join : 'a t -> 'a t -> 'a t
  (** Union of values, only required for [Functional.XX.join] *)

  val is_top : 'a t -> bool
  (** Only required for [Functional], top values can be removed
      from the data structure which allows smaller structures and faster access.
      This function only need to be an over-approximation:
      it can return [false] on top, but returning [true] on non-top leads to
      precision-loss. *)
end

(** {2 Imperative nodes}
    Node representative for imperative union-find.
    This can be used if terms have builtins pointer to their parents *)

(** Simple union-find node, without values *)
module type SIMPLE_UF_NODE = sig
  module Node : SIMPLE_GENERIC_TERM
  module Relation : SIMPLE_GENERIC_GROUP

  type root = { mutable size : int }
  (** The type of root nodes, attached to each representative *)

  type 'a parent = Node : 'b Node.t * ('a, 'b) Relation.t -> 'a parent | Root of root
  (** The type of parents. A term either points to:
      - a representative via a relation
      - a root if it is a representative *)

  val get_parent : 'a Node.t -> 'a parent
  val set_parent : 'a Node.t -> 'a parent -> unit
end

(** union-find node with values *)
module type UF_NODE = sig
  include SIMPLE_UF_NODE
  module Value : SIMPLE_GENERIC_VALUE with type ('a,'b) relation = ('a,'b) Relation.t

  type 'a root = { mutable value : 'a Value.t; mutable size : int }
  (** The type of root nodes, attached to each representative *)

  type 'a parent = Node : 'b Node.t * ('a, 'b) Relation.t -> 'a parent | Root of 'a root
  (** The type of parents. A term either points to:
      - a representative via a relation
      - a root if it is a representative *)

  val get_parent : 'a Node.t -> 'a parent
  val set_parent : 'a Node.t -> 'a parent -> unit
end
