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

(* Glue to Frama-C Ival to comply to the Codex interface (notably the
   fact that predicates return elements of the quadrivalent basis. *)

open Basis_sig;;

include Integer_Basis with type boolean = Quadrivalent_basis.boolean
                       and type integer = Framac_ival.Ival.t
include Binary_Basis with type boolean := boolean
                      and module Boolean_Lattice := Boolean_Lattice
                      and module Boolean_Forward := Boolean_Forward
                      and module Boolean_Backward := Boolean_Backward

(* Returns the element if we concretize to the singleton set. *)
val integer_is_singleton: integer -> Z.t option

(* Fold on all elements of the concretization that are between inf and
   sup. *)
val integer_fold_crop:
  integer -> inf:Z.t -> sup:Z.t ->
  (Z.t -> 'a -> 'a) ->
  'a -> 'a
