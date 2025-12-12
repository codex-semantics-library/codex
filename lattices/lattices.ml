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

(** Our primary way of exchanging and information about the program is
    using lattices. Lattices should be the abstraction of a set of
    something (its concretization).

    Note that many operations operate over several lattices (notably,
    transfer functions) are defined in the {!Single_value_abstraction}
    module. Lattices operation defined here should be concerned only
    with a single lattice.

    TODO: This is probably what we should be exporting for later
    display. 

*)

module Sig = Lattice_sig

module Unimplemented = Unimplemented_Lattice

(**The quadrivalent lattice for booleans, with four elements:
    Bottom, True, False, and Top.*)
module Quadrivalent = Quadrivalent_Lattice           
module Unit = Unit_Lattice

(**Product lattice is a lattice that pairs two (or more) component
    lattices*)
module Prod = Prod_Lattice

(** A bitvector lattice based on “known bits”: tracks which bits are
    definitely 0 or definitely 1, leaving others unknown. *)
module Known_Bits:Sig.BITVECTOR_LATTICE with type t = Bitvector_standard.Known_Bits.t = Bitvector_standard.Known_Bits

(** A lattice of finite sets of bitvectors. Best for small domains where
    explicit enumeration is feasible. *)
module BVSet = Bitvector_standard.BVSet

(** The congruence lattice: abstracts integers by modular constraints
    of the form x ≡ a (mod n). Captures properties like even/odd or
    divisibility. *)
module Congruence = Bitvector_standard.Congruence

(** Signed interval lattice: represents ranges of integers with signed
    semantics (e.g. [-10, 42])*)
module Signed_Interval = Bitvector_standard.Signed_Interval


module Unsigned_Interval = Bitvector_standard.Unsigned_Interval                           

module Integer = struct
  module Known_Bits = Integer_standard.Known_Bits
end

module Bitfield = Bitfield_Lattice
module Bitvector_Of_Integer = Bitvector_Of_Integer
