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

(** Adds checks for overflows around bitvector operation. Assumes no overflow
    occurs and optionaly raise alarms if overflows are possible. *)

(** This functor adds overflow checks to bitvector operations.
    I.E. for a {{!Operator.Binary_Forward.biadd}[biadd]} with [~size=32]
    it first performs the addition on [33] bits, then checks if the result fits
    in [32] bits (Checking signedness according to the various flags).
    Similarly, it adds guards to
    {{!Operator.Binary_Forward.bisub}[bisub]},
    {{!Operator.Binary_Forward.bimul}[bimul]}, and
    {{!Operator.Binary_Forward.bisdiv}[bisdiv]} (dividing [min_int] by [-1] underflows).
    There are no overflow guards on shifts as of yet.

    If not, then it {{!Sig.BASE.assume}[assume]} that it does fit,
    raising {!Sig.Bottom} if the overflow is guaranteed.

    It can also optionaly raise an alarm when a possible overflow is detected.

    Specifically:
    - {{!Operator.Binary_Forward.biadd}[Binary_Forward.biadd ~size ~nsw:true]}
      and {{!Operator.Binary_Forward.bisub}[Binary_Forward.bisub ~size ~nsw:true]}
      now check then {{!Sig.BASE.assume}[assume]} that their results (interpreted as signed)
      is in range {m [-2^{size-1} .. 2^{size-1}-1]} (using signed comparison
      {{!Operator.Binary_Forward.bisle}[Binary_Forward.bisle]})
    - {{!Operator.Binary_Forward.biadd}[Binary_Forward.biadd ~size ~nuw:true]}
      and {{!Operator.Binary_Forward.bisub}[Binary_Forward.bisub ~size ~nuw:true]}
      now check then {{!Sig.BASE.assume}[assume]} that their results (interpreted as unsigned)
      is smaller than {m [2^{size}-1]} (using unsigned comparison
      {{!Operator.Binary_Forward.biule}[Binary_Forward.biule]})
    - {{!Operator.Binary_Forward.biadd}[Binary_Forward.biadd ~size ~nusw:true]}
      and {{!Operator.Binary_Forward.bisub}[Binary_Forward.bisub ~size ~nusw:true]}
      now check then {{!Sig.BASE.assume}[assume]} that their results
      (where left argument is unsigned, right argument is signed)
      is in range {m [0 .. 2^{size}-1]} (fits unsigned result).
      This flag is assumed to be mutually exclusive with the two previous ones.*)
module Make(Sub : Sig.BASE_WITH_INTEGER) :
  Sig.BASE_WITH_INTEGER
    with type boolean = Sub.boolean
     and type binary = Sub.binary
     and type integer = Sub.integer
     and type enum = Sub.enum
