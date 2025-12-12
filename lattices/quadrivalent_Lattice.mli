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

(** Standard abstraction for booleans; powerset of [{true;false}] *)
type boolean = Boolean_standard.Quadrivalent.t =
  | Bottom                      (* Neither in Nuel Belnaps' four-value logic  *)
  | True
  | False
  | Top                         (* Both in Nuel Belnap's four-value logic. *)

include Lattice_sig.BOOLEAN_LATTICE with type t = boolean
val boolean_bottom: boolean

(** The result of [of_bools may_be_false may_be_false]:
    - May be false (i.e. {!False} of {!Top}) if [may_be_false] is true,
      and must not be otherwise (i.e. {!True} or {!Bottom})
    - May be true (i.e. {!True} of {!Top}) if [may_be_true] is true,
      and must not be otherwise (i.e. {!False} or {!Bottom}) *)
val of_bools: may_be_false:bool -> may_be_true:bool -> boolean

(** returns [(may_be_false, may_be_true)], as defined in {!of_bools}. *)
val to_bools: boolean -> (bool * bool)
val truth_value: boolean -> boolean
