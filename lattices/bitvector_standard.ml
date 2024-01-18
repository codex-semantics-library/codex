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


(**************** Standard abstractions for bitvectors. ****************)

(* Standard abstractions that are useful for exchanging information between domains.  *)

            
module Interval = struct
(* Min, max pair. Note that on bitvectors there is a maximum, so there
   is no need for infinity to represent top.  *)

  include Integer_standard.ZPair;;

  let join ~size:_ (min1,max1) (min2,max2) = (Z.min min1 min2, Z.max max1 max2);;
  let inter ~size:_ (min1,max1) (min2,max2) = (Z.max min1 min2, Z.min max1 max2);;  
    
  (* Any element where max is smaller than min is bottom. *)
  let bottom ~size:_ = Z.one, Z.zero

  let includes ~size (min1,max1) (min2,max2) = Z.leq min1 min2 && Z.geq max1 max2;;

  let pretty ~size fmt (a,b) =
    Format.fprintf fmt "[%s..%s]" (Z.to_string a) (Z.to_string b)
end

module Signed_Interval = struct
  include Interval
end

module Unsigned_Interval = struct
  include Interval
  let top ~size = (Z.zero, Z.pred @@ Z.(lsl) Z.one size)
end
             
module Known_Bits = struct

  (* First bitvector = 0 where the bits must be 0, 1 otherwise.
     Second bitvector = 1 where the bits must be 1, 0 otherwise.

     This is the same representation than for integers, with the
     additional restriction that bits at position higher than size are
     zero (so we must be careful and use extract for every operation
     that can change the sign or higher bits).  *)
  include Integer_standard.ZPair

  let chop ~size x = Z.extract x 0 size

  let bottom ~size = (Z.zero, chop ~size Z.minus_one);;
  let is_bottom ~size x = Integer_standard.Known_Bits.is_bottom x

  let top ~size = (chop ~size Z.minus_one, Z.zero);;
  let pretty ~size = Integer_standard.Known_Bits.pretty 
  
  let singleton ~size x =
    let x = chop ~size x in
    (x,x)
  ;;

  (* Intersection: keep the bits known by one side. *)  
  let inter0 = Z.(land)
  let inter1 = Z.(lor)
  let inter ~size:_ (x0,x1) (y0,y1) = (inter0 x0 y0, inter1 x1 y1);;

  let join0 = Z.(lor)
  let join1 = Z.(land)
  let join ~size (x0,x1) (y0,y1) = (join0 x0 y0, join1 x1 y1)

  let equal (x0,x1) (y0,y1) = Z.equal x0 y0 && Z.equal x1 y1;;
  let is_included ~size x y = equal y (join ~size x y);;
  let includes ~size x y = is_included ~size y x

  (* A widening operator from a paper by Antoine Miné. *)
  let widen ~size ~previous:(prev0,prev1) (new0,new1) =
    let res0 =
      let t = join0 prev0 new0 in
      if Z.equal prev0 t then prev0
      else Z.extract Z.minus_one 0 size
    in
    let res1 =
      let t = join1 prev1 new1 in
      if Z.equal prev1 t then prev1
      else Z.zero
    in    
    (res0,res1)
  ;;

  let includes_or_widen ~size ~previous _ = assert false
  
end
  

module Congruence = struct

  (* Represents a set q * Z + r, where q is the first element, and r
     the second.  - If q is negative, this represents bottom.  - If q
     is 0, this represents a singleton.  - If q is >0, then 0 <= r <
     q.

     Note: as this representation is independent from the sign, most
     operations are the same than for integers.
  *)
  include Integer_standard.ZPair

  let bottom ~size = Integer_standard.Congruence.bottom
  let top ~size = Integer_standard.Congruence.top
  let singleton ~size x = Z.zero, Z.extract x 0 size
end


(* Bitvectors are finite, thus we can explicitely enumerate bitvectors. *)
module BVSet = struct
  module ZSet = Set.Make(Z)

  let equal = ZSet.equal
  let compare = ZSet.compare
  let hash _ = assert false
  let pretty ~size fmt x =
    Format.fprintf fmt "@[<hv>";
    x |> ZSet.iter (fun x -> Format.fprintf fmt "%s@ " (Z.to_string x));
    Format.fprintf fmt "@]"
  ;;
              
  type t = ZSet.t
  let bottom ~size = ZSet.empty
  let top ~size = assert false
  let join ~size = ZSet.union
  let inter ~size = ZSet.inter
  let singleton ~size x = ZSet.singleton x
end 
