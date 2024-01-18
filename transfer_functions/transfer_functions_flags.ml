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


(**************** Flags ****************)
(* TODO: use this structure, which allows to easily change the flags,
   as they impact only the places where the flags are used or
   produced. Define it in transfer_functions. *)


(* Note: 
   - nsw means no signed wrap: the result does not overflow in a signed representation.
   - nuw means no unsigned wrap: the result does not overflow in an unsigned representation. 
   - npw means no pointer wrap: the first argument is unsigned, the second is signed, 
     and the computation must not overflow.
 *)

(* Note: this structure has a lattice structure. We can perform intersection on the flags to
   be more precise; try to resolve them.

 *)
module type Flags = sig
  type t
  type unpacked
  val pack: unpacked -> t
  val unpack: t -> unpacked
end


module Imul_Flags = struct
  type t = int
  type unpacked = {
      nsw:bool;
      nuw:bool;
    }
  ;;
  let pack u = 
      ((if u.nsw then 1 else 0) lsl 0)
  lor ((if u.nuw then 1 else 0) lsl 1)

  let to_bool x = if x == 0 then false else true;;
  
  let unpack x =
    { nsw = to_bool (x land (0 lsl 0));
      nuw = to_bool (x land (0 lsl 1));
    }
end

