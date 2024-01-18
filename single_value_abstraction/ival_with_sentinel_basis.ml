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

module Sentinel = Sentinel_basis

(** Reduction between sentinels and {Ival}s. *)
module Reduce = struct
  type t1 = Sentinel.binary
  type t2 = Ival_basis.binary

  let reduce ~size (sntl,ival) =
    let ival_zero = Ival_basis.Binary_Forward.biconst ~size Z.zero in
    (* Reduce {ival} using {sentinel} *)
    let ival = if Sentinel.is_zero sntl then ival_zero else ival in
    (* Reduce {sentinel} using {ival} *)
    let sntl = match Ival_basis.Binary_Forward.beq ~size ival ival_zero with
      | Lattices.Quadrivalent.True -> Sentinel.zero
      | Lattices.Quadrivalent.False -> Sentinel.nonzero
      | _ -> sntl
    in
    sntl,ival
end

module Ival_with_sentinel = Reduced_prod_basis.Make
  (Sentinel)
  (Ival_basis)
  (Reduce)

include Ival_with_sentinel
