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

open Transfer_functions.Term

module type Constraints = sig

  module Condition:Condition_map.Condition

  (** Superterms of {!t}, i.e. list/set of terms that contain {!t} as subterm *)
  type superterms

  (** Relation between terms, used for the union-find *)
  module Relation : Union_Find.Parameters.GENERIC_GROUP

  module Id:sig
    type 'a t
    val to_int: 'a t -> int
  end

  (** Loop nesting where the variable was defined. When feasible, we
     automatically compute it to be at the highest possible scope. *)
  type level = int

  (** A unique identifier for the widening point, needed to detect
      when the fixpoint is reached. *)
  type widening_id = int

  type 'a complete_term =
    | Tuple_get: int * tuple_ -> 'res complete_term
    (** Phi arguments are represented using tuples (and [Tuple_get]
        represents an element in this tuple). This encodes the fact
        that the different variables are updated in sync. Using these
        tuples and path predicates to represent conditions, we have a
        sound and complete translation to SSA or SMT without the need
        for a control flow graph. *)
    | Empty: 'res complete_term (** No value. Possibly should be only used as phi arguments. *)
    | Unknown: level -> 'res complete_term (** A fresh value. TODO: Add an identifier for its scope.  *)
    | Mu_formal:{level:level; actual: ('res t * Transfer_functions.Term.boolean t) } -> 'res complete_term

    | Inductive_var: {widening_id:widening_id; level:level; mutable definition: 'res t} -> 'res complete_term
    | T0: { tag: (ar0,'res) term } -> 'res complete_term                                       (** Term of arity 0.*)
    | T1: { tag: ('a ar1,'res) term; a: 'a t; level: level} -> 'res complete_term              (** Term of arity 1.*)
    | T2: { tag: (('a,'b) ar2,'res) term; a: 'a t; b:'b t; level:level } -> 'res complete_term (** Term of arity 2.*)

  and 'a t =
    | Bool: {
        mutable superterms: superterms;
        id: boolean Id.t;
        mutable parent: boolean parent;
        term: boolean complete_term;
        bdd:Condition.t
      } -> boolean t
    | Integer: {
        mutable superterms: superterms;
        id: integer Id.t;
        mutable parent: integer parent;
        term: integer complete_term;
      } -> integer t
    | Binary: {
        mutable superterms: superterms;
        id: binary Id.t;
        mutable parent: binary parent;
        term: binary complete_term;
        size: int
      } -> binary t

  and 'a parent =
    | Node : 'b t * ('a, 'b) Relation.t -> 'a parent
    | Root

  and any = Any: 'res t -> any [@@unboxed]
  and tuple = any Immutable_array.t
  and tuple_ = private
    | Nondet of {id: tuple Id.t; level: level;
                 a:tuple; conda_bool: boolean t;
                 b:tuple; condb_bool: boolean t}
    | Mu of { id: tuple Id.t; level: level; init:tuple;var:tuple;body:tuple;body_cond: boolean t}
    | Inductive_vars of { id : tuple Id.t; widening_id:widening_id; level:int; mutable def: tuple; }

  (** Polymorphic term equality *)
  val polyeq : 'a t -> 'b t -> ('a, 'b) PatriciaTree.cmp

  module Any:sig
    type t = any
    val hash: t -> int
    val equal: t -> t -> bool
    val compare: t -> t -> int
    val get_id_int: t -> int
  end

  module Tuple:sig
    type t = tuple_
    val hash: t -> int
    val equal: t -> t -> bool
    val compare: t -> t -> int
  end


  module Superterms : sig
    val iter_on_superterms: 'a t -> (any -> unit) -> unit
  end

  val compare: 'a t -> 'b t -> int
  val hash: 'a t -> int
  val equal: 'a t -> 'b t -> bool

  (** Print the constraint/tuple. The dependencies should have already
      been printed. *)
  val pretty: Format.formatter -> 'a t -> unit


  (** Returns the nesting level of a constraint. Constants are -1,
      topmost is 0. *)
  val level: 'a t -> int

  val size_of: binary t -> int

  module Build:sig

    module Boolean:sig
      val empty: boolean t
      val bunion: boolean t -> boolean t -> boolean t
      val unknown: level:int -> boolean t
      include Transfer_functions.Boolean_Forward
        with module Arity := Transfer_functions.Forward_Arity
         and type boolean = boolean t
    end

    module Integer:sig
      val empty: integer t
      val unknown: level:int -> integer t
      include Transfer_functions.Integer_Forward
        with module Arity := Transfer_functions.Forward_Arity
         and type boolean = boolean t
         and type integer = integer t
    end

    module Binary:sig
      val empty: size:int -> binary t
      val bunion: size:int -> Transfer_functions.Condition.t -> binary t -> binary t -> binary t
      val unknown: size:int -> level:int -> binary t
      include Transfer_functions.Binary_Forward
        with module Arity := Transfer_functions.Forward_Arity
         and type boolean = boolean t
         and type binary = binary t
    end


    module Mu_Formal:sig
      val intro: level:int -> actual:'a t ->  actual_cond: Transfer_functions.Term.boolean t -> 'a Transfer_functions.Term.typ -> 'a t
    end

    module Tuple:sig
      val get_binary: size:int -> int -> tuple ->  Transfer_functions.Term.binary t
      val get_integer:int -> tuple ->  Transfer_functions.Term.integer t
      val get_boolean:int -> tuple ->  Transfer_functions.Term.boolean t
      val nondet: level:int -> conda_bool:Transfer_functions.Term.boolean t ->  a:tuple ->
        condb_bool:Transfer_functions.Term.boolean t -> b:tuple -> tuple
      val inductive_vars: widening_id:int -> level:int -> def:tuple(* cond_def:Transfer_functions.Term.boolean t -> def:tuple ->  *)-> tuple
      val mu: level:int -> init:tuple -> var:tuple -> body:tuple -> body_cond:Transfer_functions.Term.boolean t -> tuple
    end
  end

  module Utils: sig
    val get_term: 'a t -> 'a complete_term
  end

  (** Union find structure: some terms points to their parents through the set
      of relations, allowing for accurate representation of relations in the group
      and storing fewer independent terms. *)
  module UnionFind : Union_Find.Signatures.IMPERATIVE_GENERIC_RELATIONAL
        with type 'a t = 'a t
        and type ('a, 'b) relation = ('a,'b) Relation.t
end
