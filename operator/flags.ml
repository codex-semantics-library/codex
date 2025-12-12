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


(* We pack the flags as a single integer, not only for performance,
   but also so that it is easy to add or remove flags. *)

(* Note: 
   - nsw means no signed wrap: the result does not overflow in a signed representation.
   - nuw means no unsigned wrap: the result does not overflow in an unsigned representation. 
   - nusw means no unsigned/signed wrap: the first argument is unsigned, the second is signed, 
     and the computation must not overflow.
 *)

module Bimul:sig
  type t = private int
  type unpacked = {nsw:bool;nuw:bool}
  val pack: nsw:bool -> nuw:bool -> t
  val unpack: t -> unpacked
end =
struct
  type t = int
  type unpacked = {nsw:bool;nuw:bool}
  let pack ~nsw ~nuw = (if nuw then 2 else 0) + (if nsw then 1 else 0)
  let is_nsw f = (f land 1) = 1
  let is_nuw f = (f land 2) = 2
  let unpack f = {nsw=is_nsw f;nuw=is_nuw f}
end

module Bshl:module type of Bimul = Bimul


module Biadd:sig
  type t = private int
  type unpacked = {nsw:bool;nuw:bool;nusw:bool}
  val no_overflow: t
  val pack: nsw:bool -> nuw:bool -> nusw:bool -> t
  val unpack: t -> unpacked
  val is_nsw: t -> bool
  val is_nuw: t -> bool
end =
struct
  type t = int
  type unpacked = {nsw:bool;nuw:bool;nusw:bool}
  let pack ~nsw ~nuw ~nusw =
    (if nusw then 4 else 0)
    lor (if nuw then 2 else 0)
    lor (if nsw then 1 else 0)
  let no_overflow = pack ~nsw:false ~nuw:false ~nusw:false
  let is_nsw f = (f land 1) = 1
  let is_nuw f = (f land 2) = 2
  let is_nusw f = (f land 4) = 4
  let unpack f = {nsw=is_nsw f;nuw=is_nuw f;nusw=is_nusw f}
end

module Bisub = Biadd
