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

(** A {i Single-value abstraction} consists of {!Operator} over {!Lattices}.

    There are two kinds of transfer functions:
    - Forward transfer functions take lattice as arguments and returns a lattice as result;
    - Backward transfer functions take lattice as arguments, a lattice as the known result, and returns
      a new version of the arguments that match the result and other argument values.

    The transfer functions are multi-sorted, and we have an implicit order on the types:
    - Boolean values are standalone;
    - Enum values depend on boolean;
    - Integer values depend on boolean;
    - Bitvector values depend on boolean (it could also depend on integer).
    - (Block and Memory are currently not used). *)

(** The signature of all single-value abstractions. Start here. *)
module Sig = Sva_sig

(** A basic abstraction, featuring only transfer function over abstract booleans. *)

module Quadrivalent = Sva_quadrivalent

(** Our main bitvector abstraction: interval and congruence
    abstraction, for both the signed and unsigned interpretation of
    bitvectors. *)
module Ival = Sva_ival

(** The most precise abstraction for finite enumerations: a bitfield,
    with a bit for each possible value, compactly representing a set
    of values.  *)
module Bitfield = Sva_bitfield

(** A bitvector abstraction representing information known about the
    known and unknown bits.  *)
module Known_bits = Sva_known_bits

(** Code when you need new transfer functions that fail when they are called.
    Start here when you implement a new single-value abstraction. *)
module Dummy = Sva_dummy

(** Automatically log the calls to a basis in the trace. *)
module Log = Sva_log

(** Automatically computes stats about the number of calls. *)
module Stats = Sva_stats

