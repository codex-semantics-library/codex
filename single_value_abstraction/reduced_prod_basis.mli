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

module type Reduce = sig
  type t1
  type t2
  val reduce : size:int -> t1 * t2 -> t1 * t2
end

(** Reduced product domain. The folding operation [binary_fold_crop] is
   forwarded as-is to the second domain, so the domain most precise for folding
   should be put second.  *)
module type Reduced_prod_functor = functor
  (B1 : Basis_sig.Numeric_Basis)
  (B2 : Basis_sig.Numeric_Basis)
  (R : Reduce with
    type t1 = B1.binary
    and type t2 = B2.binary)
  -> Basis_sig.Numeric_Basis

(** A domain that abstracts a bitvector, trying to track whether it is equal to
   zero or not. *)
module Make : Reduced_prod_functor
