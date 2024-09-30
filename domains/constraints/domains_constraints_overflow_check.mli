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

(** For the most part, this returns a domain equal to [Sub], it simply modifies
    the signed {{!Constraint_domains_sig.Domain_S.Binary_Forward}[Binary_Forward]}
    operations of [Sub] to incorporate overflow checks. It should be placed in the
    functor pile between {!Constraint_domain2} (which builds the terms) and
    {!Domains_constraints_nonrelational}.

    Specificaly:
    - {{!Constraint_domains_sig.Domain_S.Binary_Forward.biadd}[Binary_Forward.biadd ~size ~nsw:true]}
      and {{!Constraint_domains_sig.Domain_S.Binary_Forward.bisub}[Binary_Forward.bisub ~size ~nsw:true]}
      now {{!Constraint_domains_sig.Domain_S.assume}[assume]} that their results (interpreted as signed)
      is in range {m [-2^{size-1} .. 2^{size-1}-1]} (using signed comparison
      {{!Constraint_domains_sig.Domain_S.Binary_Forward.bisle}[Binary_Forward.bisle]})
    - {{!Constraint_domains_sig.Domain_S.Binary_Forward.biadd}[Binary_Forward.biadd ~size ~nuw:true]}
      and {{!Constraint_domains_sig.Domain_S.Binary_Forward.bisub}[Binary_Forward.bisub ~size ~nuw:true]}
      now {{!Constraint_domains_sig.Domain_S.assume}[assume]} that their results (interpreted as unsigned)
      is smaller than {m [2^{size}-1]} (using unsigned comparison
      {{!Constraint_domains_sig.Domain_S.Binary_Forward.biule}[Binary_Forward.biule]})
    - {{!Constraint_domains_sig.Domain_S.Binary_Forward.biadd}[Binary_Forward.biadd ~size ~nusw:true]}
      and {{!Constraint_domains_sig.Domain_S.Binary_Forward.bisub}[Binary_Forward.bisub ~size ~nusw:true]}
      now {{!Constraint_domains_sig.Domain_S.assume}[assume]} that their results
      (where left argument is unsigned, right argument is signed)
      is in range {m [0 .. 2^{size}-1]} (fits unsigned result).
      This flag is assumed to be mutually exclusive with the two previous ones. *)
module Make(Sub : Constraint_domains_sig.Domain_S) :
  Constraint_domains_sig.Domain_S
   with type t = Sub.t
    and module Constraints = Sub.Constraints
    and module Query = Sub.Query
