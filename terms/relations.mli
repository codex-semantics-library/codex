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

open Union_Find.Parameters
open Operator.Function_symbol
open Units

(** {1 Group Action} *)

(** Computes the action of a group of relation on some numeric values *)
module type GROUP_ACTION = sig
  type bitvector
  type integer
  type boolean
  type enum
  type ('a, 'b) relation

  (** One-to-one correspodance between value types and term types *)
  type (_, _) mapping =
    | BitvectorMapping: (bitvector,  Operator.Function_symbol.bitvector) mapping
    | IntegerMapping: (integer, Operator.Function_symbol.integer) mapping
    | BooleanMapping: (boolean, Operator.Function_symbol.boolean) mapping
    | EnumMapping: (enum, Operator.Function_symbol.enum) mapping

  (** Existential wrapper for mappings *)
  type 'term wrapper = Wrap: ('value * ('value, 'term) mapping) -> 'term wrapper

  (** [apply_relation vp rel proof] is the value [vc] obtained by applying
      relation [rel] to value [vp]. This is a simple forward transfer function.

      The {!proof} terms are there to ensure correspondance between the types
      of values ({!B.bitvector}, {!B.boolean}, {!B.integer}, {!B.enum}) and
      of terms ({!TC.bitvector}, {!TC.boolean}, {!TC.integer}, {!TC.enum}) *)
    val apply_relation:
      'value_parent ->
      ('term_child, 'term_parent) relation ->
      ('value_parent, 'term_parent) mapping ->
      'term_child wrapper

    (** [refine_relation ~size vp vc rel proof_p proof_c] refines the value [vp]
        into, using knowledge that applying [rel] to [vp] must be in [vc].
        This is a simple backward transfer function. [None] is returned when
        no new information can be deduced.

        {b Note:} this returns the newly learned value. It should be
        intersected with [vp] for best precision.

        The {!proof} terms are there to ensure correspondance between the types
        of values ({!B.bitvector}, {!B.boolean}, {!B.integer}, {!B.enum}) and
        of terms ({!TC.bitvector}, {!TC.boolean}, {!TC.integer}, {!TC.enum}) *)
    val refine_relation:
      'value_parent ->
      'value_child ->
      ('term_child, 'term_parent) relation ->
      ('value_parent, 'term_parent) mapping ->
      ('value_child, 'term_child) mapping ->
      'value_parent option
end

(** {1 Some implementations} *)

(** The simplest relation: strict equality [y = x] between variables *)
module Equality: sig
  type (_, _) t = Equal: ('a, 'a) t
  (** Simplest possible relation: equality (singleton group) *)

  include Union_Find.Parameters.GENERIC_GROUP with type ('a, 'b) t := ('a, 'b) t
  (** Group operation (compose, inverse, identity, equal) and printer *)

  module Action(B: sig type bitvector type integer type boolean type enum end):
    GROUP_ACTION
    with type bitvector = B.bitvector
      and type integer = B.integer
      and type boolean = B.boolean
      and type enum = B.enum
      and type ('a, 'b) relation = ('a, 'b) t
end

(** Simple additive relation: [y = delta*x + b] where [delta] is +/- 1 (indicated
    by a boolean with true -> positive)
    - On {!integer} is is the usual unbounded addition
    - On {!bitvector} it is modulo addition
    - On {!boolean} is is simply negation *)
module Additive: sig
  type delta = PlusOne | MinusOne

  type (_, _) t = private
    | Identity: ('a, 'a) t
    | Add_Modulo: { factor: delta; size: In_bits.t; amount: Z.t } -> (bitvector, bitvector) t
        (** Invariant: is not identity
            either [factor] is [MinusOne] or [amount] is non-zero (modulo 2^size)
            [amount] is in [-2^(size-1) .. 2^(size-1)-1] *)
    | Add_Unbounded: delta * Z.t -> (integer, integer) t
        (** Invariant: is not identity
            either the [delta] is [MinusOne] (negation) or the value is non-zero *)
    | Bool_Not: (boolean, boolean) t
  (** see {!Additive} for description of this relation

      The type is made private to ensure invariant are respected,
      use the constructors below to build terms *)

  val additive_identity: ('a, 'a) t
  val additive_bitvector: size:In_bits.t -> delta -> Z.t -> (bitvector, bitvector) t
  val additive_integer: delta -> Z.t -> (integer, integer) t
  val boolean_not: (boolean, boolean) t

  include GENERIC_GROUP with type ('a, 'b) t := ('a, 'b) t

  module Action(B: Single_value_abstraction.Sig.NUMERIC_ENUM):
    GROUP_ACTION
    with type bitvector = B.bitvector
      and type integer = B.integer
      and type boolean = B.boolean
      and type enum = B.enum
      and type ('a, 'b) relation = ('a, 'b) t
end

module XOR_Rotate: sig
  type (_, _) t = private
    | XR_Identity: ('a, 'a) t
    | XR_BNot: (boolean, boolean) t
      (** Boolean not *)
    | XR_XOR_rotate: { rotate: int; xor: Z.t; size:In_bits.t } -> (bitvector, bitvector) t
      (** On bounded integers, represents a rotation then a xor
          Invariant: [xor] fits on [size] bits, [0 <= rotate < size] *)
  (** Bitwise focused relation: xor and rotate. *)

  val xr_identity: ('a, 'a) t
  val xr_bnot: (boolean, boolean) t
  val xr_xor_rotate: rotate:int -> xor:Z.t -> size:In_bits.t -> (bitvector, bitvector) t

  include GENERIC_GROUP with type ('a, 'b) t := ('a, 'b) t
end

module LinearTwoVarEquality : sig
  type (_, _) t = private
    | Identity: ('a, 'a) t
    | Linear_Equality: { size: In_bits.t; f1: Z.t; f2: Z.t; offset: Z.t } -> (bitvector, bitvector) t
    (** [Linear_Equality{f1; f2; offset}] represents the relation [f1*x + f2*y = offset].
        For instance [{f1=1; f2=-2; offset=8}] represents [x - 2y = 8] or [x = 2y+8]
        Both [f1] and [f2] should be non-zero.
        The upside of storing it this way instead of [x = q*y + r] is that it avoids
        having to use rational numbers.

        Invariants:
        - [f1 > 0]
        - [gcd f1 f2 offset = 1]
        - We never have [f1 = -f2] and [offset = 0]. That case is represented by
          {!Identity}. *)

  val make: size:In_bits.t -> f1:Z.t -> f2:Z.t -> Z.t -> (bitvector, bitvector) t
  (** Smart constructor, normalizes terms to respect invariants *)

  (** Destructors for easy access *)

  val f1: ('a, 'b) t -> Z.t
  val f2: ('a, 'b) t -> Z.t
  val offset: ('a, 'b) t -> Z.t

  include GENERIC_GROUP with type ('a, 'b) t := ('a, 'b) t

  module Action(B: Single_value_abstraction.Sig.NUMERIC_ENUM):
    GROUP_ACTION
    with type bitvector = B.bitvector
      and type integer = B.integer
      and type boolean = B.boolean
      and type enum = B.enum
      and type ('a, 'b) relation = ('a, 'b) t
end
