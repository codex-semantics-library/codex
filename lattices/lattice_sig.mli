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

(** Signature for lattices, semi-lattices, and type-specific lattices. *)

(** {1 Common lattice signatures}                                             *)
(******************************************************************************)

(** {2 Minimal signature}                                           *)
(********************************************************************)
open Units

(* Extends datatype: elements now have a lattice structure. *)

(** A JOIN_SEMI_LATTICE is the minimal structure needed for abstract interpretation.
    It represents a mathematical {{: https://en.wikipedia.org/wiki/Semilattice}join semi-lattice}:
    i.e. a set ({{!JOIN_SEMI_LATTICE.t}[type t]}) with an inclusion order
    ({{!JOIN_SEMI_LATTICE.includes}[includes : t -> t -> bool]}) that orders those values.
    The key operation is {{!JOIN_SEMI_LATTICE.join}[join : t -> t -> t]}, which merges information
    from two elements to form their least upper bound (an over-approximation of both).

    In program analysis, {{!JOIN_SEMI_LATTICE.join}[join]} is used at control-flow merge points,
    e.g., after an [if] or [while], to unify information from different paths.

    The {{!JOIN_SEMI_LATTICE.widen}[widen]} operator ensures convergence when computing fixpoints in loops, by accelerating growth of abstract states.

    This minimal signature doesn't require meet or top operations. *)
module type JOIN_SEMI_LATTICE = sig
  include Datatype_sig.S (** @closed *)

  (** Computes the least upper bound of two elements. *)
  val join: t -> t -> t

  (** In a fixpoint iteration of a function {m \mathcal{F}}, [previous] is the
     previous element of the fixpoint iteration sequence. The other
     element is the newly computed (tentative) element,
     i.e. {m \mathcal{F}}([previous]). If the new element is included in [previous], [true] is returned,
     together with [new] (the smaller element): the post-fixpoint of {m \mathcal{F}}
     has been found (further calls to {m \mathcal{F}} can decrease the sequence). Else, the returned element should be used as the next element of
     the fixpoint iteration sequence; the operator guarantees its
     convergence. For lattices of finite height, the widening part can just perform
     an over-approximation of the join; however note that it is not
     required that the sequence is monotonic. *)
  val includes_or_widen: previous:t -> t -> (bool * t)

  (** [includes a b] holds if {m b \sqsubseteq a}, i.e., [b] is at least as precise as [a]. *)
  val includes: t -> t -> bool

  (** Widening operator used in fixpoint iteration to enforce convergence. *)
  val widen: previous:t -> t -> t
end

(** {2 Optional, standard, operations on lattices}                  *)
(********************************************************************)

(** Equips lattices with the ability to compute intersections: {{!WITH_INTER.inter}[inter : t -> t -> t]},
    also called meet, the greatest lower bound. *)
module type WITH_INTER = sig
  type t
  val inter: t -> t -> t
end

(** Equips lattices with a {{!WITH_BOTTOM.bottom}[bottom: t]}
    and test if the lattice element is a Bottom.

    @canonical Lattices.Sig.WITH_BOTTOM *)
module type WITH_BOTTOM = sig
  type t
  val bottom: unit -> t                 (* One of the bottoms. *)
  val is_bottom: t -> bool      (* Can be imprecise; e.g. always returning false is correct. *)
end

(** Equips lattices with a {{!WITH_TOP.top}[top: t]}. *)
module type WITH_TOP = sig
  type t
  val top: unit -> t                 (* One of the tops. *)
end

(** A join-semilattice with bottom. *)
module type JOIN_SEMI_LATTICE_WITH_BOTTOM = sig
  include JOIN_SEMI_LATTICE
  include WITH_BOTTOM with type t := t
end

(** A join-semilattice with bottom and intersection. *)
module type JOIN_SEMI_LATTICE_WITH_INTER_BOTTOM = sig
  include JOIN_SEMI_LATTICE
  include WITH_INTER with type t := t
  include WITH_BOTTOM with type t := t
end

(** {2 Lattice with all operations}                                 *)
(********************************************************************)

(** A mathematical {{: https://en.wikipedia.org/wiki/Lattice_(order)}lattice}: join, meet, bottom, and top all available. *)
module type LATTICE = sig
  include JOIN_SEMI_LATTICE
  include WITH_INTER with type t := t
  include WITH_BOTTOM with type t := t
  include WITH_TOP with type t := t
end

(** {1 Type specific lattices}                                                *)
(******************************************************************************)
(** The lattices represent abstract set of common types (boolean, integers, bitvectors). *)

(** A boolean lattice: a {!LATTICE} with elements representing
    truth values. Provides {{!BOOLEAN_LATTICE.singleton}[singleton]} to build from a concrete [bool], and a conversion
    to the standard 4-element {{!Quadrivalent}quadrivalent lattice}. *)
module type BOOLEAN_LATTICE = sig
  include LATTICE
  val singleton: bool -> t
  val to_quadrivalent: t -> Boolean_standard.Quadrivalent.t
end


(** a {!LATTICE} for integers. *)
module type INTEGER_LATTICE = sig
  include LATTICE (* Many Integer_Lattices do not have a top, as they cannot represent
    an infinite set. *)
  val singleton: Z.t -> t
  val is_singleton: t -> Z.t option

  (** Fold on all elements of the concretization that are between inf and
      sup. *)
  val fold_crop:
    t -> inf:Z.t -> sup:Z.t ->
    (Z.t -> 'a -> 'a) ->
    'a -> 'a
end

(** This helps communicating information between lattices by converting to a
    common representation. *)
module type BITVECTOR_STANDARD_CONVERSIONS = sig
  type t
  (* Export information. *)
  val to_unsigned_interval: size:In_bits.t -> t -> Bitvector_standard.Unsigned_Interval.t
  val to_signed_interval: size:In_bits.t -> t -> Bitvector_standard.Signed_Interval.t
  val to_congruence: size:In_bits.t -> t -> Bitvector_standard.Congruence.t
  val to_known_bits: size:In_bits.t -> t -> Bitvector_standard.Known_Bits.t
  val to_bvset: size:In_bits.t -> t -> Bitvector_standard.BVSet.t

  (* Import information. *)
  val inter_unsigned_interval: size:In_bits.t -> t -> Bitvector_standard.Unsigned_Interval.t -> t
  val inter_signed_interval: size:In_bits.t -> t -> Bitvector_standard.Signed_Interval.t -> t
  val inter_congruence: size:In_bits.t -> t -> Bitvector_standard.Congruence.t -> t
  val inter_known_bits: size:In_bits.t -> t -> Bitvector_standard.Known_Bits.t -> t
  val inter_bvset: size:In_bits.t -> t -> Bitvector_standard.BVSet.t -> t
end

(** Bitvector Lattice: a {!LATTICE} representing fixed sized bitvectors.
    The size isn't stored in the lattice, instead lattice operations take it as
    an extra argument. *)
module type BITVECTOR_LATTICE_NO_CONVERSION = sig
  include Datatype_sig.S
  val bottom: size:In_bits.t -> t
  val is_bottom: size:In_bits.t -> t -> bool
  val top: size:In_bits.t -> t
  val inter: size:In_bits.t -> t -> t -> t
  val join: size:In_bits.t -> t -> t -> t
  val pretty: size:In_bits.t -> Format.formatter -> t -> unit
  val widen: size:In_bits.t -> previous:t -> t -> t
  val includes: size:In_bits.t -> t -> t -> bool
  val includes_or_widen: size:In_bits.t -> previous:t -> t -> (bool * t)
  val singleton: size:In_bits.t -> Z.t -> t
end

module type BITVECTOR_QUERIES = sig
  type t
  val is_singleton: size:In_bits.t -> t -> Z.t option

  (** True if the binary cannot be concretized into any value.  *)
  val is_empty: size:In_bits.t -> t -> bool

  (** Fold on all integers contained in either the signed or unsigned
     representation of a binary. *)
  val fold_crop_signed: size:In_bits.t -> t -> inf:Z.t -> sup:Z.t -> 'a -> (Z.t -> 'a -> 'a) -> 'a
  val fold_crop_unsigned: size:In_bits.t -> t -> inf:Z.t -> sup:Z.t -> 'a -> (Z.t -> 'a -> 'a) -> 'a
end


(** A full bitvector lattice, combining core operations and queries.
    Also allows conversion to standard domains like known-bits
    or intervals. *)
module type BITVECTOR_LATTICE = sig
  include BITVECTOR_LATTICE_NO_CONVERSION
  include BITVECTOR_QUERIES with type t := t
  val to_known_bits: size:In_bits.t -> t -> Bitvector_standard.Known_Bits.t
  val to_unsigned_interval: size:In_bits.t -> t -> Bitvector_standard.Unsigned_Interval.t
  val to_signed_interval: size:In_bits.t -> t -> Bitvector_standard.Signed_Interval.t
  (* include Bitvector_Standard_Conversions *)
end

(** Enum Lattice: i.e. a {!LATTICE} representing a set of enum values.
    Like {!BITVECTOR_LATTICE}, they don't store the size in them. Thus, some
    functions take the size has argument. *)
module type ENUM_LATTICE = sig
  include Datatype_sig.S

  (** Top requires the size as it is a union of all the cases from 0 to size-1. *)
  val top: size:int -> t
  val singleton: int -> t
  val is_singleton: t -> int option

  (** Fold on all the set indices in the bitfield, in ascending order.  *)
  val fold_on_cases : t -> 'a -> (int -> 'a -> 'a) -> 'a
  include WITH_BOTTOM with type t:= t
  include JOIN_SEMI_LATTICE with type t:= t
  include WITH_INTER with type t:= t
end
