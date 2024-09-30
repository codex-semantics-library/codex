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

(** This module signature describes two rather distinct components:
    - a description of the architecture registers, along with the initial abstract values that they should contain
    - other settings for the analysis such as a hook table.

    All these signatures are parameterized by an abstract domain.
*)

module type Registers = sig
  module Domain : Codex.Domain_sig.Base
  val registers : (string * int) list
  val initial_value : Domain.Context.t -> string * int -> Domain.binary
end

module type S = functor
  (Domain : Codex.With_focusing.S_with_types) -> sig

  module Registers : Registers with module Domain = Domain
end
