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


(**************** Standard abstractions for integers. ****************)

(* All these abstractions rely on pairs of Z. *)
module ZPair = struct
  type t = Z.t * Z.t
  let compare (a1,b1) (a2,b2) =
    let res = Z.compare a1 a2 in
    if res == 0 then Z.compare b1 b2
    else res
  ;;

  let equal (a1,b1) (a2,b2) =
    Z.equal a1 a2 && Z.equal b1 b2

  let sdbm x y = y + (x lsl 16) + (x lsl 6) - x;;
  
  let hash (a,b) = sdbm (Z.hash a) (Z.hash b);;

end
            
module Interval = struct
  (* Min, max pair. Note that we can only represent finite sets.  *)
  include ZPair;;

  let join  (min1,max1) (min2,max2) = (Z.min min1 min2, Z.max max1 max2);;
  let inter  (min1,max1) (min2,max2) = (Z.max min1 min2, Z.min max1 max2);;  
    
  (* Any element where max is smaller than min is bottom, and this is
     the canonical one. *)
  let bottom  = Z.one, Z.zero

  let includes  (min1,max1) (min2,max2) = Z.leq min1 min2 && Z.geq max1 max2;;

  let pretty  fmt (a,b) =
    Format.fprintf fmt "[%s..%s]" (Z.to_string a) (Z.to_string b)
end

module Known_Bits = struct
(* First bitvector = 0 where the bits must be 0, 1 otherwise.
   Second bitvector = 1 where the bits must be 1, 0 otherwise. *)
  include ZPair

  let bottom = (Z.zero, Z.minus_one);;
  let is_bottom (x0,x1) =
    let open Z in
    not @@ Z.equal Z.zero @@ ~!x0 land x1
  ;;

  let top = (Z.minus_one, Z.zero);;
  
  type bitvalue = One | Zero | Unknown

  (* Note: 0 is the least significant bit. *)
  let testbit (x0,x1) index = 
    if Z.testbit x0 index
    then if Z.testbit x1 index (* First bit is 1. *)
         then One
         else Unknown
    else Zero
  ;;

  let pretty fmt (x0,x1) =
    if is_bottom (x0,x1) then Format.fprintf fmt "<bottom>"
    else
      let size = Z.numbits x0 in
      for i = (size - 1) downto 0 do
        match testbit (x0,x1) i with
        | Zero ->    Format.fprintf fmt "0"
        | One ->     Format.fprintf fmt "1"
        | Unknown -> Format.fprintf fmt "?"
      done
  ;;

  let read s =
    let n = (String.length s) - 1 in
    let rec loop n bit (x0,x1) =
      if n < 0 then (x0,x1)
      else
        match s.[n] with
        | '0' -> loop (n-1) (Z.(lsl) bit 1) (x0,x1)
        | '1' -> loop (n-1) (Z.(lsl) bit 1) (Z.(lor) x0 bit, Z.(lor) x1 bit)
        | '?' -> loop (n-1) (Z.(lsl) bit 1) (Z.(lor) x0 bit, x1)
        | _ -> failwith "Wrong input"
    in loop n Z.one (Z.zero,Z.zero)
  ;;


  
  let singleton x = (x,x);;

  (* Intersection: keep the bits known by one side. *)  
  let inter0 = Z.(land)
  let inter1 = Z.(lor)
  let inter  (x0,x1) (y0,y1) = (inter0 x0 y0, inter1 x1 y1);;

  let join0 = Z.(lor)
  let join1 = Z.(land)
  let join (x0,x1) (y0,y1) = (join0 x0 y0, join1 x1 y1)

  let equal (x0,x1) (y0,y1) = Z.equal x0 y0 && Z.equal x1 y1;;
  let is_included x y = equal y (join x y);;
  let includes x y = is_included y x

  (* A widening operator from a paper by Antoine Miné. *)
  let widen ~size ~previous:(prev0,prev1) (new0,new1) =
    let res0 =
      let t = join0 prev0 new0 in
      if Z.equal prev0 t then prev0
      else Z.minus_one
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

(* Represents a set q * Z + r, where q is the first element, and r the second. 
   - If q is negative, this represents bottom.
   - If q is 0, this represents a singleton.
   - If q is >0, then 0 <= r < q. *)
  (* Note: we could have another abstraction representing truncated
     division. *)
  include ZPair

  let bottom  = (Z.minus_one, Z.minus_one)
  let top  = Z.one, Z.zero   (* 1 * Z + 0 = R *)
  let singleton x = Z.zero, x
end


module Integer_Set = struct
  module ZSet = Set.Make(Z)

  let equal = ZSet.equal
  let compare = ZSet.compare
  let hash _ = assert false
  let pretty fmt x =
    Format.fprintf fmt "@[<hv>";
    x |> ZSet.iter (fun x -> Format.fprintf fmt "%s@ " (Z.to_string x));
    Format.fprintf fmt "@]"
  ;;
              
  type t = ZSet.t
  let bottom = ZSet.empty
  (* let top = assert false *)
  let join = ZSet.union
  let inter = ZSet.inter
  let singleton = ZSet.singleton
end 
