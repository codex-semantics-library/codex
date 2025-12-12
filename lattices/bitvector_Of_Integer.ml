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

open Lattice_sig;;
module In_bits = Units.In_bits

module Make(I:INTEGER_LATTICE):BITVECTOR_LATTICE with type t = I.t = struct

  include Unimplemented_Lattice.Bitvector_Lattice(struct
      type t = I.t
      let loc = __LOC__
    end)
  
  type t = I.t
  let equal = I.equal
  let compare = I.compare
  let hash = I.hash
  let bottom ~size = I.bottom()
  let is_bottom ~size = I.is_bottom
  let is_empty = is_bottom
  let includes ~size = assert false
  let includes_or_widen ~size = assert false
  let top ~size = I.top()

  (* Note: maybe we want to use the size to display bounds, to simplify things here. *)
  let pretty ~size = I.pretty
  
  let is_singleton ~(size:In_bits.t) x =
    I.is_singleton x |> Option.map (fun z -> Z.signed_extract z 0 (size:>int))


  (* Notes on the implementation of inter and join, if we ever need them.

     For intersection, we have to be careful: e.g. [-3;-2] inter [240-255] is not bottom, but [-3;2].

     For join, we should split along the three intervals:
     -2^size-1..-1, 0..2^size-1, 2^size-1..2^size, then perform piecewise join.

     The problem is that we can end up with several pieces, and there
     are two ways of stiching that back into a single interval (so we
     don't have a lattice structure). However, in many cases it would
     work.

     An easy solution is to just join on intervals, but note that this
     can be quite imprecise.  However, it probably suffices in many cases.

     The best is probably to use bitvector_Of_Integer only to perform
     queries out of a relational integer abstract domain, and use the
     signed/unsigned interval reduced product when dealing with
     bitvectors. This doesnot require implementing these functions. *)
end
