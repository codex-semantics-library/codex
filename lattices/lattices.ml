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

module Sig = Lattice_sig
module Quadrivalent = Quadrivalent_Lattice           
module Unit = Unit_Lattice
module Assert_False = Assert_False_Lattice  

module Prod = Prod_Lattice
module Known_Bits:Sig.Bitvector_Lattice with type t = Bitvector_standard.Known_Bits.t = Bitvector_standard.Known_Bits
module BVSet = Bitvector_standard.BVSet
module Congruence = Bitvector_standard.Congruence
module Signed_Interval = Bitvector_standard.Signed_Interval
module Unsigned_Interval = Bitvector_standard.Unsigned_Interval                           

module Integer = struct
  module Known_Bits = Integer_standard.Known_Bits
end

