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

(* The interpreter takes value in {true,false,empty}; {Z + empty}; the
   set of bitvectors + empty. Empty is not an error, just a special
   value representing the absence of value.

   But, considering that whenever an argument is empty, the result is
   empty, we can build an interpreter by focusing on the non-empty
   values, and just return an exception for the (infrequent) empty cases. *)
exception Empty

module Types = struct
  type boolean = bool
  type integer = Z.t
  type bitvector = Z.t
  (* Should be positive, and only the |size| least significant bits
     can be 1.  (Note: even though the signed representation should
     unbox more with Zarith). *)
end


module Boolean_Interp:Transfer_functions_sig.Boolean_Forward
  with module Arity := Transfer_functions_sig.Forward_Arity
   and type boolean = bool
= struct
  include Types
  let false_ = false
  let true_ = true
  let (||) = (||)
  let (&&) = (&&)
  let not =  not
end

module Bitvector_Interp:Transfer_functions_sig.Bitvector_Forward
  with module Arity := Transfer_functions_sig.Forward_Arity
   and type boolean = bool
   and type bitvector = Z.t
= struct
  include Types
  let bofbool ~size:_ = function
    | true -> Z.one
    | false -> Z.zero

  let biconst ~size x = Z.extract x 0 size;;

  let bsext ~size ~oldsize x = Z.extract (Z.signed_extract x 0 oldsize) 0 size;;
  let buext ~size:_ ~oldsize:_ x = x;;

  let bxor ~size:_ = Z.(lxor)
  let band ~size:_ = Z.(land)
  let bor ~size:_ = Z.(lor)

  let biule ~size:_ = Z.leq
  let beq ~size:_ = Z.equal
  let bisle ~size a b =
    let a = Z.signed_extract a 0 size in
    let b = Z.signed_extract b 0 size in
    Z.leq a b
  ;;
                      
  

  let bshl ~size ~nsw ~nuw x y =
    let yi = Z.to_int y in
    let res = (Z.shift_left x yi) in
    if nuw && Z.(geq) res (Z.shift_left Z.one size)
    then raise Empty
    else if nsw then
      (* The shifted out bits must be the same than the msb. *)
      let shifted_out_and_msb = Z.signed_extract res (size - 1) (yi + 1) in
      if Z.equal shifted_out_and_msb Z.zero || Z.equal shifted_out_and_msb Z.minus_one
      then ()
      else raise Empty
    else ();
    Z.extract res 0 size               
  ;;

  let blshr ~size:_ x y = Z.shift_right x (Z.to_int y);;

  let bashr ~size x y =
    Z.extract (Z.shift_right (Z.signed_extract x 0 size) (Z.to_int y)) 0 size;;
  ;;

  
  let bconcat ~size1:_ ~size2 a b =
    Z.(lor) (Z.shift_left a size2) b
  ;;

  let bextract ~size ~index ~oldsize:_ x =
    Z.extract x index size
  ;;

  let signed_overflow ~size x =
    Z.(geq) x (Z.shift_left Z.one (size - 1))
    ||   Z.(lt) x @@ Z.neg (Z.shift_left Z.one (size - 1))
  ;;
  
  let unsigned_overflow ~size x =
    Z.(geq) x (Z.shift_left Z.one size)
  ;;
  

  let biadd ~size ~nsw ~nuw ~nusw a b =
    let res = Z.(+) a b in
    if nuw && unsigned_overflow ~size res then raise Empty
    else if nusw &&
            let b = Z.signed_extract b 0 size in
            let unsigned_res = Z.(+) a b in
            unsigned_overflow ~size unsigned_res then raise Empty
    else if nsw &&
            let a = Z.signed_extract a 0 size in
            let b = Z.signed_extract b 0 size in
            let signed_res = Z.(+) a b in
            signed_overflow ~size signed_res
    then raise Empty
    else Z.extract res 0 size
  ;;

  let bisub ~size ~nsw ~nuw ~nusw a b =
    let res = Z.(-) a b in
    if nuw && unsigned_overflow ~size res then raise Empty
    else if nusw && 
            let b = Z.signed_extract b 0 size in
            let unsigned_res = Z.(-) a b in
            unsigned_overflow ~size unsigned_res then raise Empty
    else if nsw &&
            let a = Z.signed_extract a 0 size in
            let b = Z.signed_extract b 0 size in
            let signed_res = Z.(-) a b in
            signed_overflow ~size signed_res
    then raise Empty
    else Z.extract res 0 size
  ;;

  let bimul ~size ~nsw ~nuw a b =
    let ab = Z.( * ) a b in 
    if nuw && unsigned_overflow ~size ab then raise Empty
    else if nsw &&
            let sa = Z.signed_extract a 0 size in
            let sb = Z.signed_extract b 0 size in
            let sab = Z.( * ) sa sb in
            signed_overflow ~size sab
    then raise Empty
    else Z.extract ab 0 size;;
  ;;

  let biudiv ~size:_ a b =
    if Z.equal b Z.zero then raise Empty
    else Z.div a b
  let biumod ~size:_ a b =
    if Z.equal b Z.zero then raise Empty
    else Z.rem a b
  let bisdiv ~size a b =
    if Z.equal b Z.zero then raise Empty
    else Z.extract (Z.div (Z.signed_extract a 0 size) (Z.signed_extract b 0 size)) 0 size
  let bismod ~size a b =
    if Z.equal b Z.zero then raise Empty
    else Z.extract (Z.rem (Z.signed_extract a 0 size) (Z.signed_extract b 0 size)) 0 size   
end

