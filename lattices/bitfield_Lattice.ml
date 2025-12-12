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

module Log = Tracelog.Make(struct let category = "Bitfield_Lattice" end);;

(* Warn if we create too large bitfields.
   Probably no real performance issue below this value.  *)
let bitfield_perf_warn_size = 512;;
let bitfield_perf_warn_done = ref false;;
let bitfield_perf_warn ~size = 
  if(!bitfield_perf_warn_done && size > bitfield_perf_warn_size)
  then begin
    Log.warning(fun p -> p "Data structure may not suit large bit values");
    bitfield_perf_warn_done := true
  end;;

module BitfieldDT = (struct
  type t = Z.t
  let pretty = Z.pp_print
  let equal = Z.equal
  let compare = Z.compare
  let hash = Z.hash
end)

module Bitfield = struct
  
  (* type t = Z.t *)
  include BitfieldDT

  let bottom () = Z.zero
  let is_bottom = Z.equal Z.zero

  let top ~size =
    assert (0 <= size);
    bitfield_perf_warn ~size;
    Z.pred (Z.shift_left Z.one size)

  let singleton idx =
    bitfield_perf_warn ~size:idx;
    Z.shift_left Z.one idx

  let join = Z.logor

  let is_included a b = Z.equal a (Z.logand a b) 

  let includes a b = is_included b a

  let widen ~previous = join previous

  let includes_or_widen ~previous a =
    if includes previous a then (true,a) else (false,join previous a)

  let inter = Z.logand

end

include Bitfield

(* https://graphics.stanford.edu/~seander/bithacks.html#DetermineIfPowerOf2 *)
let is_singleton x =
  if Z.equal Z.zero x then None
  else
  if Z.equal Z.zero (Z.(land) x (Z.pred x))
  then Some (Z.log2 x)
  else None
;;

(* Fold on indices that are set, by chunks of 64 bits, in ascending order. *)
let fold_on_cases x acc f =
  assert(Z.sign x >= 0);
  let rec fold_on_word pos x acc =
    if Z.equal x Z.zero
    then acc
    else
      let n = Z.trailing_zeros x in
      let acc = f (pos + n) acc in
      let x = Z.(land) x (Z.lognot (Z.shift_left Z.one n)) in
      fold_on_word pos x acc
  in
  let rec loop pos x acc = 
    if Z.equal x Z.zero then acc
    else
      let word = Z.extract x 0 64 in 
      let acc = fold_on_word pos word acc in
      let x = Z.shift_right x 64 in
      loop (pos + 64) x acc
  in loop 0 x acc
