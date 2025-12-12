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

(* Cast a lattice about integers to a lattice about bitvectors.

   Given [size], the concretization of the bitvector lattice elmt l is
   the last [size] bits of the bitvector representation of all the
   elements in the integer concretization of the lattice.

   Notice that the representation is not unique: for size 8, [-1;-1]
   and [255;255] are different abstract elements representing the same
   concrete element.

   This makes it fine for doing queries, but the lack of unique
   element has many issues; first, we don't have a lattice (see [Gange
   et al.,2015]: Interval Analysis and Machine Arithmetic on the
   wrapped intervals). For instance, joining [-1;-1] and [3;3] with
   size 8 could result in [-1;3] or in [3;255], that are both
   incompatible. The combined signed/unsigned bitvector representation
   does not have these problems. *)


open Lattice_sig;;
module Make(I:INTEGER_LATTICE):BITVECTOR_LATTICE with type t = I.t
