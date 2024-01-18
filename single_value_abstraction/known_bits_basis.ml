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

include Quadrivalent_basis;;
module Quadrivalent = Lattices.Quadrivalent;;
module Collecting = Lattices.BVSet;;
module Concrete = Transfer_functions.Concrete.Bitvector_Interp

module ZSet = Set.Make(Z)


(* Several representations for known bits can be considered:

   must0_must1: a pair (x0,x1) with x0 = 0 when the bit must be 0, 1 otherwise
                                    x1 = 1 when the bit must be 1, 0 otherwise
                thus: (0,0) = 0 (1,1) = 1 (0,1) = bottom (1,0) = top

   value_known: a pair (v,k) with k = 1 if the bit is known, 0 otherwise
                                  v = value of the bit if known.

   value_unknown: a pair (v,k) with k = 1 if the bit is unknown, 0 otherwise
                                  v = value of the bit if known.


   Except for xor and addition, the first representation allows much
   simpler transfer functions,  and is thus the one that we use.
   (Moreover, it is difficult to represent bottom with the second one)*)

(* Conversion between the above representations. *)
let _must0_must1_to_value_known (x0,x1) = 
  let open Z in
  (x0, ~! x0 lor x1)            (* or xor, if we know there is no bottom  *)
;;

let _value_known_to_must0_must1 (xv,xk) =
  let open Z in
  (xv lor ~! xk ,xv land xk)
;;

let name = "Known_bits";;

module Must0_Must1 = struct

  module Binary_Lattice = Lattices.Known_Bits
  module BL = Binary_Lattice

  (* Conversions; should go inside Lattice. *)
  let binary_to_known_bits ~size x = x
  let binary_to_ival ~signed ~size _ = assert false
  let binary_is_singleton ~size x = assert false
  let binary_is_empty ~size = assert false
  let binary_fold_crop ~size _ = assert false
  type binary = Binary_Lattice.t

  let is_bottom (x0,x1) =
    let open Z in
    not @@ Z.equal Z.zero @@ ~!x0 land x1
  ;;

  let chop ~size x = Z.extract x 0 size;;
  
  let is_singleton (x0,x1) = Z.equal x0 x1;;
  let is_power_of_two x = Z.equal Z.zero (Z.(land) x (Z.pred x)) (* alternative: Z.popcount x == 1 *)
  let inter0 = Z.(land)
  let inter1 = Z.(lor)
  let inter (x0,x1) (y0,y1) = (inter0 x0 y0, inter1 x1 y1)

    
  let join0 = Z.(lor)
  let join1 = Z.(land)
  let join (x0,x1) (y0,y1) = (join0 x0 y0, join1 x1 y1)

  
  (* If not bottom, the unknown bits are those that differ between x0 and x1 *)
  let unknown (x0,x1) = Z.(lxor) x0 x1;;
  
  (* True if x and y are known to be empty, false otherwise.  This
     means that at one index, one of the bit is known to be different.  *)
  let disjoint (x0,x1) (y0,y1) =
    let open Z in
    (not @@ Z.equal zero (~! x0 land y1))
  || (not @@ Z.equal zero (~! y0 land x1))
  ;;
  
  (* x1 has 0 whenever the bit may be 0, x0 has 1 whenever it may be 1. 
     So x1 has more zeroes than x1.  *)
  let min_max_unsigned ~size (x0,x1) = (x1,x0)

  let testbit = Lattices.Integer.Known_Bits.testbit;;
  
  let min_max_signed ~size (x0,x1) =
    match testbit (x0,x1) (size - 1) with
    | Zero -> (x1,x0)
    | One -> (Z.signed_extract x1 0 size, Z.signed_extract x0 0 size)                    (* If the number is negative, the more one the higher, like when it is positive. *)
    | Unknown ->
       (* Flip the bit before extraction, to ensure that min is negative and max positive. *)       
       let flip = (Z.shift_left Z.one @@ size - 1) in
       (Z.signed_extract (Z.(lxor) x1 flip) 0 size,
        Z.extract (Z.(lxor) x0 flip) 0 size)
  ;;

  
  
  let concretize (x0,x1) =
    let open Z in
    if is_bottom (x0,x1)
    then  (* (\* Collecting.bottom *\) failwith (Format.asprintf "bottom for (%s,%s)" (Z.format "%b" x0) (Z.format "%b" x1)) *)
      ZSet.empty
    else
      (* let max = x0 lor x1 in *)
      let numbits = Stdlib.max (Z.numbits x0) (Z.numbits x1) in
      (* Format.printf "numbits = %d@." numbits; *)
      let max = (Z.(lsl) Z.one numbits) in
      let rec loop bit acc =
        (* Format.printf "loop@."; *)
        if Z.geq bit max then acc
        else if (Z.equal zero @@ x0 land bit)
        then loop (bit lsl 1) acc
        else if (Z.equal bit @@ x1 land bit)
        then loop (bit lsl 1) (ZSet.map ((lor) bit) acc)
        else loop (bit lsl 1) @@ ZSet.union acc (ZSet.map ((lor) bit) acc)
      in loop Z.one (ZSet.singleton Z.zero)
  ;;


  (* Best abstraction for this set. *)
  let abstract ~size set =
    match ZSet.elements set with
    | [] -> BL.bottom ~size
    | a::b -> List.fold_left (fun acc x -> join acc (BL.singleton ~size x)) (BL.singleton ~size a) b
  ;;


  module Binary_Forward = struct

    let biconst = BL.singleton
    let bofbool ~size = function
      | Quadrivalent.Bottom -> Binary_Lattice.bottom ~size
      | Quadrivalent.True -> biconst ~size Z.one
      | Quadrivalent.False -> biconst ~size Z.zero
      | Quadrivalent.Top -> join (biconst ~size Z.zero) (biconst ~size Z.one)
    
    (* And: keep 0 when one is known, 1 when both are known. *)
    let band0 = Z.(land)
    let band1 = Z.(land)
    let band ~size (x0,x1) (y0,y1) = (band0 x0 y0, band1 x1 y1)  

    (* Or: keep 1 when one is known, 0 when both are known. *)
    let bor0 = Z.(lor)
    let bor1 = Z.(lor)
    let bor ~size (x0,x1) (y0,y1) = (bor0 x0 y0, bor1 x1 y1)  

    (* Not: known 1 bits are known as 0, known 0 bits are known as 1. *)
    (* We use the size to avoid keeping negative numbers. *)
    let bnot ~size (x0,x1) = (chop ~size (Z.(~!) x1), chop ~size (Z.(~!) x0))

    let bconcat ~size1 ~size2 (x0,x1) (y0,y1) =
      Concrete.bconcat ~size1 ~size2 x0 y0,
      Concrete.bconcat ~size1 ~size2 x1 y1
    ;;

    let bextract ~size ~index ~oldsize (x0,x1) =
      Concrete.bextract ~size ~index ~oldsize x0,
      Concrete.bextract ~size ~index ~oldsize x1
    ;;

    (* 6 operations. *)
    let bxor ~size (x0,x1) (y0,y1) =
      let open Z in
      let xorx0y0 = x0 lxor y0 in
      let xorx1y1 = x1 lxor y1 in
      let unknown = x0 lxor x1 in
      (* xorx0y0 lor xorx1y1 works for all but the double unknown case.
       So we add an "unkown" check by loring with (x0 xor x1, i.e. when xbit is unknown) *)
      (xorx0y0 lor xorx1y1 lor unknown, xorx0y0 land xorx1y1)  
    ;;

    (* Fast and relatively precise, without requiring a 
       large number of iterations. *)
    let biadd ~size ~nsw ~nuw ~nusw (x0,x1) (y0,y1) =
      (* Ideal case: no carry at all, if for all indices at least one
       component is known to be 0. *)
      let carry = Z.(land) x0 y0 in   
      if(Z.equal Z.zero carry)
      then bxor ~size (x0,x1) (y0,y1)
      else
        (* Fallback case: when initially there are two zeroes, the
         carries will not propagate across this position. So we know
         the value of the bit above. This is useful when both values
         are known to hold a lot of zeroes. *)
        let never_carry = Z.(lor) x0 y0 in
        let (res0,res1) = bxor ~size (x0,x1) (y0,y1) in
        let shifted_carry = (Z.shift_left never_carry 1)  in
        let nshifted_carry = Z.(~!) shifted_carry in
        (Z.(lor) res0 shifted_carry, Z.(land) res1 nshifted_carry)
    ;;

    (* More precise. *)
    let _biadd ~size (x0,x1) (y0,y1) =
      (* let nocarry = Z.extract (Z.shift_left (Z.(land) x0 y0) 1) 0 size in *)
      (* Nocarry avoids infinitely propagating carries. 
       I think it ensures termination. *)
      let nocarry = Z.(land) x0 y0 in
      let rec loop a b =
        let half_sum = bxor ~size a b in
        let (carry0,carry1) = band ~size a b in
        let carry0 = Z.(land) carry0 nocarry in
        (* If a carry is possible. *)
        if Z.equal carry0 Z.zero 
        then half_sum
        else let carry0 = Z.shift_left carry0 1 in
             let carry1 = Z.(land) carry1 nocarry in
             let carry1 = Z.shift_left carry1 1 in           
             loop half_sum (carry0,carry1)
      in let (res0,res1) = (loop (x0,x1) (y0,y1)) in
         Z.extract res0 0 size, Z.extract res1 0 size
    ;;    

    (* This algorithm is symmetric to biadd's. *)
    let bisub ~size ~nsw ~nuw ~nusw (x0,x1) (y0,y1) =
      (* Borrow happens when x may be 0 and y may be 1. *)
      let res = bxor ~size (x0,x1) (y0,y1) in
      let no_borrow = Z.(land) (Z.(~!) x1) y0 in
      if(Z.equal Z.zero no_borrow)
      then res
      else
        (* Fallback: no borrow when x_i is known to be 1 and y_i known to be 0. 
         In this case, we use the half_sum computed by xor.
         Otherwise, we set the bit to unknown. *)
        let never_borrow = Z.(land) x1 (Z.(~!) y0)  in
        let shifted_borrow = (Z.shift_left never_borrow 1)  in
        let nshifted_borrow = Z.extract (Z.(~!) shifted_borrow) 0 size in
        let (res0,res1) = res in
        (Z.(lor) res0 nshifted_borrow, Z.(land) res1 shifted_borrow)
    ;;

    let bimul_singleton ~size (x0,x1) k =
      if(is_power_of_two k)
      then (Z.extract (Z.( * ) x0 k) 0 size,
            Z.extract (Z.( * ) x1 k) 0 size)
      else BL.top ~size
    ;;
    
    let bimul ~size ~nsw ~nuw x y =
      match is_singleton x, is_singleton y with
      | true, true -> biconst ~size Z.(fst x * fst y)
      | true, false -> bimul_singleton ~size y (fst x)
      | false, true -> bimul_singleton ~size x (fst y)
      | false, false -> BL.top ~size  (* Ival will tell us what we need. *)

    let biudiv ~size (x0,x1) ((y0,y1) as y) =
      if(is_singleton y && is_power_of_two y0)
      then
        if Z.equal Z.zero y0
        then BL.bottom ~size
        else
          (Concrete.biudiv ~size x0 y0,
           Concrete.biudiv ~size x1 y0)
      else BL.top ~size


    (* TODO: we can do better than this:
     *  - All the bottom bits are known according to number of trailing zeroes
     *  - All the top bits are known according to the number of leading zeroes *)
    let biumod ~size (x0,x1) ((y0,y1) as y) =
      if(is_singleton y && is_power_of_two y0)
      then
        if Z.equal Z.zero y0
        then BL.bottom ~size
        else
          (Concrete.biumod ~size x0 y0,
           Concrete.biumod ~size x1 y0)
      else BL.top ~size

    (* If y is a multiple of 2^k, the last k bits of x and result are
       the same.
       Also, if y is smaller than some value, then the result must be
       smaller than this value. *)
    let biumod ~size (x0,x1) (y0,y1) =
      if Z.equal Z.zero y0
      then BL.bottom ~size
      else 
        let k = Z.trailing_zeros y0 (* Number of known zeros. *) in
        (* Put to top all the bits above k. *)
        let mask0 = Z.sub (Z.shift_left Z.one k) Z.one in
        let mask1 = Z.(~!) mask0 in
        let k2 = Z.log2up y0 in (* Maximum value for y0. *)
        let nask = Z.sub (Z.shift_left Z.one k2) Z.one in
        (Z.(land) nask @@ Z.(lor) x0 mask1,
         Z.(land) nask @@ Z.(land) x1 mask0)
    ;;

    (* Works well, despite the fact that the behaviour of positive and
       negative numbers is quite different. *)
    let bismod_alt ~size (x0,x1) ((y0,y1) as y) =
      if(is_singleton y && is_power_of_two y0)
      then
        if Z.equal Z.zero y0
        then BL.bottom ~size
        else (Concrete.bismod ~size x0 y0,
              Concrete.bismod ~size x1 y0)
      else BL.top ~size
    ;;

    (* Like biumod for the low bits, but more complex for the high ones *)
    let bismod ~size ((x0,x1) as x) ((y0,y1) as y) =
      match testbit x (size - 1), testbit y (size - 1) with
      | Zero, Zero -> biumod ~size x y
      | _ -> begin
          if Z.equal Z.zero y0
          then BL.bottom ~size
          else 
            let k = Z.trailing_zeros y0 (* Number of known zeros. *) in
            (* Put to top all the bits above k. *)
            let mask0 = Z.sub (Z.shift_left Z.one k) Z.one in
            let mask1 = Z.extract (Z.(~!) mask0) 0 size in
            let low0 = Z.(lor) x0 mask1 in
            let low1 = Z.(land) x1 mask0 in
            (low0,low1)
        end
    ;;

    let bismod ~size x y = inter (bismod ~size x y) (bismod_alt ~size x y);;

    let bisdiv ~size (x0,x1) ((y0,y1) as y) =
      if(is_singleton y && is_power_of_two y0)
      then
        if Z.equal Z.zero y0
        then BL.bottom ~size
        else
          match testbit (x0,x1) (size - 1) with
          | Zero -> (Concrete.bisdiv ~size x0 y0,
                    Concrete.bisdiv ~size x1 y0)
          (* Signed division of negative numbers is different from a
           shift. *)
          | One -> BL.top ~size
          | Unknown -> BL.top ~size
      else BL.top ~size
    ;;
    
    let buext ~size ~oldsize (x0,x1) = (x0,x1);;

    let bsext ~size ~oldsize ((x0,x1) as x) =
      match testbit x (oldsize - 1) with
      | Zero -> x
      | One -> (Z.extract (Z.signed_extract x0 0 oldsize) 0 size,
               Z.extract (Z.signed_extract x1 0 oldsize) 0 size)
      | Unknown -> (Z.extract (Z.signed_extract x0 0 oldsize) 0 size, x1)
    ;;

    let bshl_singleton ~size (x0,x1) y =
      (Z.extract (Z.shift_left x0 y) 0 size,
       Z.extract (Z.shift_left x1 y) 0 size)
    ;;


    let blshr_singleton ~size (x0,x1) y =
      (Z.extract (Z.shift_right x0 y) 0 size,
       Z.extract (Z.shift_right x1 y) 0 size)
    ;;

    let bashr_singleton ~size (x0,x1) y =
      (Z.extract (Z.shift_right (Z.signed_extract x0 0 size) y) 0 size,
       Z.extract (Z.shift_right (Z.signed_extract x1 0 size) y) 0 size)
    ;;

    (* Note: if we have an interval, then we can put a minimum of
       zeros.  In general: we could be more precise by using the
       interval concretisation.  *)
    
    (* We implement a very precise version; actually, if x or y are imprecise, 
       this may not be worth it. *)
    (* Note: we follow SMTLIB's semantics, which returns 0 when shifts are too large. *)
    let do_shift f ~size x ((y0,y1) as y) =
      (* Check that size is a power of two. *)
      assert(size land (size - 1) == 0);
      assert(size <= 64);          (* We don't want to do this on huge values. *)

      (* Mask the last bits. *)
      let nzsizem1 = Z.lognot @@ Z.of_int (size - 1) in
      let must_be_larger_than_size = (not @@ Z.equal Z.zero @@ Z.(land) y1 nzsizem1) in
      if must_be_larger_than_size
      then BL.singleton ~size Z.zero
      else
        let may_be_larger_than_size = (not @@ Z.equal Z.zero @@ Z.(land) y0 nzsizem1) in

        let zset = concretize (band ~size y (BL.singleton ~size (Z.of_int @@ size - 1))) in
        let list = ZSet.fold (fun k acc -> List.cons (f ~size x @@ Z.to_int k) acc) zset [] in
        let res = 
          match list with
          | [] -> assert false         (* Should not happen. *)
          | a::b -> List.fold_left join a b
        in
        if may_be_larger_than_size
        then join res (BL.singleton ~size Z.zero)
        else res
    ;;

    (* TODO: Handle nsw and nuw flags. *)
    let bshl ~size ~nsw ~nuw = do_shift bshl_singleton ~size;;
    let blshr = do_shift blshr_singleton;;

    let bashr  ~size x ((y0,y1) as y) =
      (* Check that size is a power of two. *)
      assert(size land (size - 1) == 0);
      assert(size <= 64);          (* We don't want to do this on huge values. *)

      (* Mask the last bits. *)
      let nzsizem1 = Z.lognot @@ Z.of_int (size - 1) in
      let must_be_larger_than_size = (not @@ Z.equal Z.zero @@ Z.(land) y1 nzsizem1) in
      let overflow_value =
        match testbit x (size - 1) with
        | Zero -> BL.singleton ~size Z.zero
        | One -> BL.singleton ~size Z.minus_one
        | Unknown -> BL.top ~size
      in
      if must_be_larger_than_size
      then overflow_value
      else
        let may_be_larger_than_size = (not @@ Z.equal Z.zero @@ Z.(land) y0 nzsizem1) in

        let zset = concretize (band ~size y (BL.singleton ~size (Z.of_int @@ size - 1))) in
        let list = ZSet.fold (fun k acc -> List.cons (bashr_singleton ~size x @@ Z.to_int k) acc) zset [] in
        let res = 
          match list with
          | [] -> assert false         (* Should not happen. *)
          | a::b -> List.fold_left join a b
        in
        if may_be_larger_than_size
        then join res overflow_value
        else res
    ;;



    let beq ~size (x0,x1) (y0,y1) =
      let open Z in
      (* Bits for which both x0 and x1 are different. Outside of
       (bottom,top) that we do not consider, should happen only if
       one bit is known to be 0 and the other known to be 1. *)
      if(not @@ Z.equal Z.zero @@ (x0 lxor y0) land (x1 lxor y1))
      then Quadrivalent.False
      else if(Z.equal x0 x1) && (Z.equal x0 y0) && (Z.equal x1 y1)
      then Quadrivalent.True
      else Quadrivalent.Top
    (* This domain will be used with others that can check the singleton case,
     so we do not bother. *)

    (* must be true if no unknown bit and equal. Not the best domain for that. *)
    (* must be false if a single bit is known to be different.  *)

    (* Note that this probably duplicates what the interval will be doing. *)
    let comp min_max_extract ~size x y =
      let (minx,maxx) = min_max_extract ~size x in
      let (miny,maxy) = min_max_extract ~size y in
      if (Z.leq maxx miny) then Quadrivalent.True
      else if (Z.lt maxy minx) then Quadrivalent.False
      else Quadrivalent.Top

    let bisle = comp min_max_signed;;
    let biule = comp min_max_unsigned;;
  end

  module Binary_Backward = struct

    let inter_refines (old0,old1) (new0,new1) =
      let res0 = inter0 old0 new0 in
      let res1 = inter1 old1 new1 in
      if(Z.equal old0 res0 && Z.equal old1 new1)
      then None
      else Some(res0,res1)
    ;;

    let bofbool ~size arg (x0,x1) =
      match arg,testbit (x0,x1) 0 with
      | Quadrivalent.Bottom, _ -> None
      | _ when is_bottom (x0,x1) -> Some Quadrivalent.Bottom
      | _, Unknown -> None
      | Quadrivalent.Top, One -> Some Quadrivalent.True
      | Quadrivalent.Top, Zero -> Some Quadrivalent.False
      | Quadrivalent.True, One -> None
      | Quadrivalent.False, Zero -> None        
      | Quadrivalent.True, Zero -> Some Quadrivalent.Bottom
      | Quadrivalent.False, One -> Some Quadrivalent.Bottom                                     
    ;;
    
    
    let band ~size (x0,x1) (y0,y1) (res0,res1) =
      let open Z in
      let x1' = res1 in         (* x and y are one whenever res is 1 *)
      let y1' = res1 in
      let x0' = (res0 lor ~! y1) in (* x is 0 when res is 0 and y is 1. *)
      let y0' = (res0 lor ~! x1) in
      (inter_refines (x0,x1) (x0',x1'),
       inter_refines (y0,y1) (y0',y1'))
    ;;

    let bor ~size (x0,x1) (y0,y1) (res0,res1) =
      let open Z in
      let x0' = res0 in         (* x and y are 0 whenever res is 0 *)
      let y0' = res0 in
      let x1' = (res1 land ~! y0) in (* x is 1 when res is 1 and y is 0. *)
      let y1' = (res1 land ~! x0) in
      (inter_refines (x0,x1) (x0',x1'),
       inter_refines (y0,y1) (y0',y1'))
    ;;

    let bxor ~size x y r =
      (* TODO: There are some redundant computations between the two. *)
      let x' = Binary_Forward.bxor ~size y r in
      let y' = Binary_Forward.bxor ~size x r in
      inter_refines x x', inter_refines y y'
    ;;
      
    let bnot ~size (x0,x1) (r0,r1) =
      inter_refines (x0,x1) @@ Binary_Forward.bnot ~size (r0,r1)
    ;;

    let bconcat ~size1 ~size2 x y r =
      let oldsize = size1 + size2 in
      (inter_refines x @@ Binary_Forward.bextract ~oldsize ~index:size2 ~size:size1 r,
      inter_refines y @@ Binary_Forward.bextract ~oldsize ~index:0 ~size:size2 r)

    let bextract ~size ~index ~oldsize x r =
      (* Add top bits before and after if needed. *)
      let r =
        if index == 0 then r
        else Binary_Forward.bconcat ~size1:size ~size2:index r (BL.top ~size:index)
      in
      let r =
        if index + size == oldsize
        then r
        else
          let missing_size = (oldsize - (index + size)) in
          Binary_Forward.bconcat ~size1:missing_size ~size2:(index + size) (BL.top ~size:missing_size) r
      in
      inter_refines x r
    ;;

    let buext ~size ~oldsize x r =
      let (r0,r1) = r in
      (* If something in the upper bits of r is known to be 1,
         the result is bottom.  Maybe not worth to check. *)
      let upper1 = Z.extract r1 oldsize (size - oldsize) in
      if not @@ Z.equal Z.zero upper1
      then inter_refines x @@ BL.bottom ~size:oldsize
      else inter_refines x (Binary_Forward.bextract ~size:oldsize ~oldsize:size ~index:0 r)
    ;;

    (* Returns a pair (contains_known_one,contains_known_zero). *)
    let test_high_bits ~size r k =
      (* Printf.printf "Test high bits: size = %d k = %d%!" size k; *)
      let (r0,r1) = r in
      let upper0 = Z.extract r0 (size-k) k in
      let upper1 = Z.extract r1 (size-k) k in
      let high_bit_is_one = (* One bit is known to be 1. *)
        (not @@ Z.equal Z.zero upper1)
      in
      let high_bit_is_zero = (* One bit is known to be 0. *)
        (not @@ Z.equal (Z.extract Z.minus_one 0 k) upper0)
      in
      (high_bit_is_one, high_bit_is_zero)
    ;;

    (* We are in a situation where r (or size bigsize) was derived
       from some x (of size smallsize), such that the high bits in r
       is a copy from the most significant bit of x. We update the
       corresponding bit of r so that it can be used to refine x.  *)
    let update_according_to_high_bits ~bigsize ~smallsize ((r0,r1) as r) =
      match test_high_bits ~size:bigsize r (1 + bigsize - smallsize) with
        | true,true -> BL.bottom ~size:bigsize
        | true,false ->
           let clear_bit = Z.(lognot) @@ Z.shift_left Z.one smallsize in
           let r = (Z.(land) r0 clear_bit, Z.(land) r1 clear_bit) in
           r
        | false,true ->
           let set_bit = Z.shift_left Z.one smallsize in
           let r = (Z.(lor) r0 set_bit, Z.(lor) r1 set_bit) in
           r          
        | false,false -> r


    let bsext ~size ~oldsize x r =
      let r = update_according_to_high_bits ~bigsize:size ~smallsize:oldsize r in
      inter_refines x (Binary_Forward.bextract ~size:oldsize ~oldsize:size ~index:0 r)
    

    let shift_right_introduce_top ~size x k =
      if k == 0 then x
      else Binary_Forward.bconcat 
          ~size1:k (BL.top ~size:k) 
          ~size2:(size-k) (Binary_Forward.bextract ~index:k ~size:(size-k) ~oldsize:size x)
    ;;

    let shift_left_introduce_top ~size x k =
      if k == 0 then x
      else Binary_Forward.bconcat 
        ~size1:(size-k) (Binary_Forward.bextract ~index:0 ~size:(size-k) ~oldsize:size x)
        ~size2:k (BL.top ~size:k) 
    ;;


    let do_shift f ~size x y r =
      assert(is_power_of_two @@ Z.of_int size);
      
      (* If zero belongs to the result, it may be that y is just very large;
           in this case x and y can have any value, and we don't learn anything. 
           TODO: also check nuw and nsw. *)
      let sizem1 = (Z.of_int @@ size - 1) in
      let result_may_be_zero = not @@  (disjoint r @@ BL.singleton ~size Z.zero) in
      let y_may_be_larger_than_size =
        let (y0,_) = y in
        not @@ Z.equal Z.zero @@ Z.(land) y0 @@ Z.(~!) sizem1
      in
      if result_may_be_zero && y_may_be_larger_than_size 
      then None,None
      else begin
          let yset = concretize (Binary_Forward.band ~size y (BL.singleton ~size sizem1)) in
          let (newx,newy) = ZSet.fold (fun y (newx,newy) ->
              (* Format.printf "y is %s@." @@ Z.to_string y; *)
              let oldx = f ~size r (Z.to_int y)  in
              if disjoint x oldx then
                (newx,newy)
              else (join newx @@ inter x oldx,
                    join newy @@ BL.singleton ~size y)) yset (BL.bottom ~size, BL.bottom ~size)
          in
          (inter_refines x newx,
           inter_refines y newy)
        end          
    ;;

    let bshl ~size ~nsw ~nuw = do_shift shift_right_introduce_top ~size;;
    let blshr = do_shift shift_left_introduce_top;;
    
    let bashr ~size x y r =
      assert(is_power_of_two @@ Z.of_int size);

      (* TODO: independently from everything, we can do like in bsext
       *    and try to find the top bit of x from the top bits of r. *)
      
      (* If zero belongs to the result (of -1 if x can be negative),
         it may be that y is just very large; in this case x and y can
         have any value, and we don't learn anything.  TODO: also
         check nuw and nsw. *)
      let sizem1 = (Z.of_int @@ size - 1) in

      let result_ok_large_size =
        match testbit x (size - 1) with
        | Zero -> not (disjoint r @@ BL.singleton ~size Z.zero)
        | One -> not (disjoint r @@ BL.singleton ~size Z.minus_one)
        | Unknown -> not (disjoint r @@ BL.singleton ~size Z.zero)
                    || not (disjoint r @@ BL.singleton ~size Z.minus_one)
      in
      (* Format.printf "HELLOS %a size %d@." pretty y size; 
       * Format.printf "OK large size %b@." result_ok_large_size; *)
      let y_may_be_larger_than_size =
        let (y0,_) = y in
        not @@ Z.equal Z.zero @@ Z.(land) y0 @@ Z.(~!) sizem1
      in
      (* Format.printf "OK large size %b@." result_ok_large_size; *)
      if result_ok_large_size && y_may_be_larger_than_size 
      then None,None
      else begin
          let yset = concretize (Binary_Forward.band ~size y (BL.singleton ~size sizem1)) in
          let (newx,newy) = ZSet.fold (fun y (newx,newy) ->

               (* Lookup the high bits of r to better estimate the high bit of x. *)
               let r =
                 if Z.equal y Z.zero
                 then r
                 else update_according_to_high_bits ~bigsize:size ~smallsize:(size - Z.to_int y) r 
               in
               
               let oldx = shift_left_introduce_top ~size r (Z.to_int y)  in
               if disjoint x oldx then
                 (newx,newy)
               else (join newx @@ inter x oldx,
                     join newy @@ BL.singleton ~size y)) yset (BL.bottom ~size, BL.bottom ~size)
          in
          (inter_refines x newx,
           inter_refines y newy)
        end          
    ;;

    let beq ~size x y r = match r with
      | Quadrivalent.Bottom | Quadrivalent.Top -> None,None
      | Quadrivalent.True -> let xy = inter x y in
                            inter_refines x xy,
                            inter_refines y xy
      | Quadrivalent.False ->
         (* We can learn only one bit, if it is the only one unknown
            for x, that everything is known in y, and all the other
            bits are the same in x and y. 
            Probably never triggers. *)
         if(is_bottom x || is_bottom y)
         then (None,None)
         else
           let unknownx = unknown x in
           let unknowny = unknown y in

           (* Learn 1 bit of y from x. *)
           let learn (x0,x1) (y0,y1) =
             if (Z.equal x0 y0) then (y1,y1)
             else (y0,y0)
           in
           if Z.equal unknownx Z.zero && is_power_of_two unknowny then
             None, inter_refines y @@ learn x y 
           else if Z.equal unknowny Z.zero && is_power_of_two unknownx 
           then (inter_refines x @@ learn y x),None
           else (None,None)
    ;;

    (* Assume that (x0,x1) <=u maxu. *)
    let assume_unsigned_max ~size maxu (x0,x1) =
      (* Format.printf "assume unsigned max %s %a@." (Z.to_string maxu) pretty (x0,x1); *)
      (* Naive algorithm: iterate from the most significant bit.
       If max is 0: set to 0 and continue.
       If max is 1: if x is known as 1, continue; if unknown, or known 0, stop
       (we don't known about the next bits). 

       Clever algorithm : find the limit of all the bits that will be copied
       from maxu, then do the copy. *)
      try       
        let first_uncopied_bit =
            Z.log2 @@ Z.(land) maxu @@ Z.(~!) @@ Z.(land) x0 x1
        in
        let copy_bits_up_to = first_uncopied_bit + 1 in
        (* Format.printf "copy_bits_up_to %d@." copy_bits_up_to; *)
        let mask = Z.sub (Z.shift_left Z.one copy_bits_up_to) Z.one in
        let nmask = Z.(~!) mask in
        let maxu_copied_bits = Z.(land) nmask maxu in
        let res0 = Z.(lor) maxu_copied_bits (Z.(land) mask x0) in
        let res1 = Z.(lor) maxu_copied_bits (Z.(land) mask x1) in      
        (res0,res1)
      with Invalid_argument _ ->
        if Z.equal Z.zero maxu then (Z.zero,Z.zero)
        else (x0,x1)            (* Do something here? *)
    ;;

    (* Assume that minu <=u (x0,x1). *)
    let assume_unsigned_min ~size minu (x0,x1) =
      (* Naive algorithm: iterate from the most significant bit.
       If min is 1: set to 1 and continue.
       If min is 0: if x is known as 0, continue; if unknown, or known 1, stop
       (we don't known about the next bits). 

       Clever algorithm : find the limit of all the bits that will be copied
       from maxu, then do the copy. *)
        let first_uncopied_bit =
          try 
            Z.log2 @@ Z.(land) (Z.(~!) minu) @@ Z.(lor) x0 x1
          with Invalid_argument _ -> -1
        in
        let copy_bits_up_to = first_uncopied_bit + 1 in
        (* Format.printf "copy_bits_up_to %d@." copy_bits_up_to; *)
        let mask = Z.sub (Z.shift_left Z.one copy_bits_up_to) Z.one in
        let nmask = Z.(~!) mask in
        let maxu_copied_bits = Z.(land) nmask minu in
        let res0 = Z.(lor) maxu_copied_bits (Z.(land) mask x0) in
        let res1 = Z.(lor) maxu_copied_bits (Z.(land) mask x1) in      
        (res0,res1)
    ;;

    
    (* Here: by comparing the min and max, we can try to learn some
       bits.  Do we learn something more than intervals? Yet, because
       we maintain the knowledge of the low bits.  *)
    let biule ~size x y r =
      match r with
      | Quadrivalent.Top | Quadrivalent.Bottom -> None,None
      | Quadrivalent.True -> 
         let (_,maxy) = min_max_unsigned ~size y in
         let (minx,_) = min_max_unsigned ~size x in         
         let newx = assume_unsigned_max ~size maxy x in
         let newy = assume_unsigned_min ~size minx y in
         inter_refines x newx, inter_refines y newy
      | Quadrivalent.False ->                 (* ~(x <= y) <=> x > y <=> x >= y + 1  *)
         let (miny,_) = min_max_unsigned ~size y in
         let (_,maxx) = min_max_unsigned ~size x in
         let newx = assume_unsigned_min ~size (Z.add miny Z.one) x in
         let newy = assume_unsigned_max ~size (Z.sub maxx Z.one) y in
         inter_refines x newx,  inter_refines y newy
    ;;

    (* For signed values, 1 is also higher than 0s, except for the first bit. *)
    let assume_signed_max ~size maxu (x0,x1) =
      if(Z.lt maxu Z.zero) then
        let mask = Z.shift_left Z.one (size - 1) in      
           let (x0,x1) = (Z.(lor) x0 mask, Z.(lor) x1 mask) in
           assume_unsigned_max ~size maxu (x0,x1)
      else 
        (* If the first bit is 0: try to learn.
           Otherwise: we cannot learn anything. *)
        match testbit (x0,x1) (size - 1) with
        | Zero -> assume_unsigned_max ~size maxu (x0,x1)
        | One | Unknown -> (x0,x1)
    ;;

    (* For signed values, 1 is also higher than 0s, except for the first bit. *)
    let assume_signed_min ~size minu (x0,x1) =
      (* let mask = Z.shift_left Z.one (size - 1) in       *)
      if(Z.geq minu Z.zero) then
        assume_unsigned_min ~size minu (x0,x1)
      else
        (* If the first bit is 1: try to learn.
           Otherwise: we cannot learn anything. *)
        match testbit (x0,x1) (size - 1) with
        (* The extract here is needed, otherwise we produce negative numbers. *)
        | One -> assume_unsigned_min ~size (Z.extract minu 0 size) (x0,x1)
        | Zero | Unknown -> (x0,x1)
    ;;
    
    let bisle ~size x y r =
      match r with
      | Quadrivalent.Top | Quadrivalent.Bottom -> None,None
      | Quadrivalent.True ->     (* we known that x <= y, so x <= max(y) *)
         let (_,maxy) = min_max_signed ~size y in
         let (minx,_) = min_max_signed ~size x in
         (* Format.printf "maxy is %s@." (Z.to_string maxy);          *)
         let newx = assume_signed_max ~size maxy x in
         let newy = assume_signed_min ~size minx y in
         (* Format.printf "y is %a newy is %a inter is %a" pretty y pretty newy pretty (inter y newy); *)
         inter_refines x newx, inter_refines y newy
      | Quadrivalent.False ->
         let (miny,_) = min_max_signed ~size y in
         let (_,maxx) = min_max_signed ~size x in
         let newx = assume_signed_min ~size (Z.add miny Z.one) x in
         let newy = assume_signed_max ~size (Z.sub maxx Z.one) y in
         (* Format.printf "old y %a newy %a maxx %s@." pretty y pretty newy (Z.to_string maxx); *)
         inter_refines x newx,  inter_refines y newy

    (* x + y = r <=> x = r - y <=> y = r - x  *)
    let biadd ~size ~nsw ~nuw ~nusw x y r =
      inter_refines x (Binary_Forward.bisub ~size ~nsw ~nuw ~nusw r y),
      inter_refines y (Binary_Forward.bisub ~size ~nsw ~nuw ~nusw r x)
    ;;

    (* x - y = r <=> x = r + y <=> y = x - r *)    
    let bisub ~size ~nsw ~nuw ~nusw x y r =
      inter_refines x (Binary_Forward.biadd ~size ~nsw ~nuw ~nusw r y),
      inter_refines y (Binary_Forward.bisub ~size ~nsw ~nuw ~nusw x r)
    ;;

    let bitimes ~size ~nsw ~nuw x ((k0,k1) as k) r =
      (* k must be a singleton. *)
      assert(Z.equal k0 k1);
      if Z.equal Z.zero k0 then None
      else if nuw then
        inter_refines x @@ Binary_Forward.biudiv ~size r k
      else if nsw then
        inter_refines x @@ Binary_Forward.bisdiv ~size r k
      else None
    ;;
    
    let bimul ~size ~nsw ~nuw x y r =
      if is_singleton y then (bitimes ~size ~nsw ~nuw x y r), None
      else if is_singleton x then None, (bitimes ~size ~nsw ~nuw y x r)
      else None,None
    ;;

    (* Only handle the cases where the division is a shift. *)
    let biudiv ~size x y r =
      let (y0,_) = y in      
      if is_singleton y && (not @@ Z.equal Z.zero y0) && is_power_of_two y0 then
        let k = Z.log2 y0 in
        Format.printf "k is %d %a@." k (Binary_Lattice.pretty ~size) (shift_left_introduce_top ~size r k);
        inter_refines x (shift_left_introduce_top ~size r k), None
      else None, None
    ;;

    (* Negative values behave badly, so we rule them out. *)
    let bisdiv ~size x y r =
      if testbit x (size - 1) = Zero
         && testbit y (size - 1) = Zero
      then biudiv ~size x y r
      else None,None
    ;;
    
    (* if r = x mod (2^k * z)
       then the k lastbits of r are the k last bits of x.
       TODO: This should be done on the forward operation too. *)
    let biumod ~size x (y0,y1) (r0,r1) =
      (* Find the number of trailing zeroes in y. *)
      if Z.equal Z.zero y0      (* y must be zero. *)
      then None,None
      else
        let k = Z.trailing_zeros y0 (* Number of known zeros. *) in
        (* Put to top all the bits above k. *)
        let mask0 = Z.sub (Z.shift_left Z.one k) Z.one in
        let mask1 = Z.(~!) mask0 in
        inter_refines x (Z.(lor) r0 mask1, Z.(land) r1 mask0), None
    ;;        

    (* This also works for modulo. *)
    let bismod = biumod;;
    
  end

end

include Must0_Must1


module Value_Known = struct

  (* This representation is often less iteresting, but here is an
     implementation of some operations. *)

  (* Intersection: we know more about both. Note: could also raise bottom. *)
  let inter (xv,xk) (yv,yk) =
    let open Z in
    (* There is an inconsistency if known values differ.  *)
    let inconsistent = (xk land yk) land (xv lxor yv) in
    if (not (Z.equal inconsistent Z.zero)) then failwith "Bottom";
    let value = (xk lor yv) land (yk lor xv) in
    (value, xk lor yk)
  ;;

  let bnot (xv,xk) = (Z.(~!) xv, xk)
  
  let band (xv,xk) (yv,yk) =
    let open Z in
    (xv land yv, (xk land yk) lor ((Z.(~!) xv) land xk) lor ((Z.(~!) yv) land yk))
  ;;

  let bor (xv,xk) (yv,yk) =
    let open Z in
    (xv lor yv, xk land yk lor (xv land xk) lor (yv land yk))
  ;;

  (* Result of xor is result of the known variable, but only value
     that are both known are known. *)
  let bxor (xv,xk) (yv,yk) =
    (Z.(lxor) xv yv, Z.(land) xk yk)
  
end


module Value_Unknown = struct

  (* Inersection: we know more about both. Note: could also raise bottom. *)
  let inter (xv,xu) (yv,yu) =
    let open Z in
    (* There is an inconsistency if known values differ.  *)
    let inconsistent = ~! (xu lor yu) land (xv lxor yv) in
    if (not (Z.equal inconsistent Z.zero)) then failwith "Bottom";
    let value = (xu lor xv) land (yu lor yv) in
    (value, xu land yu)
  ;;

  let bnot (xv,xu) = (Z.(~!) xv, xu)
  let band (xv,xu) (yv,yu) =
    let open Z in
    (xv land yv, (xu lor yu) land (xv lor xu) land (yv lor yu))
  ;;
  let bor (xv,xu) (yv,yu) = 
    let open Z in
    (xv lor yv, (xu lor yu) land (~! xv lor xu) land (~! yv lor yu))
  ;;

  (* Result of xor is result of the known variable, but only value
     that are both known are known. *)
  let bxor (xv,xu) (yv,yu) =
    (Z.(lxor) xv yv, Z.(lor) xu yu)

  (* We transform this addition algorithm:  into a n addition wih uncertainty.

     let rec add a b =
     let     half_sum = a lxor b in
     let carry = (a land b) in
     if carry == 0 then half_sum
     else add half_sum (carry lsl 1)
  *)

  let rec badd (av,au) (bv,bu) =
    let open Z in
    let half_sum = bxor (av,au) (bv,bu) in
    let (carryv,carryu) = band (av,au) (bv,bu) in
    (* if carryv == 0 && carryu == 0 *) (* Might drop the carryu part. We could also do a or to avoid making two tests  *)
    if (Z.equal zero @@ carryv lor carryu)  (* all bits are known to be 0. *)
    then half_sum
    else badd half_sum (carryv,carryu)
        
end;;

