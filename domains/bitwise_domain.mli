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


module type Subdomain = Domain_sig.Base

(** A domain that abstracts a bitvector, and remembers what bitwise
   operations have been applied to it. Can be a very efficient way to
   simplify sequences of bitwise operations.

    The idea is that each bit x_i is represented as being either 0, 1,
   or y_j, i.e. the bit j from another word y.  *)
module Make (Sub : Subdomain) :
  Domain_sig.Base
  with module Context = Sub.Context
  and type boolean = Sub.boolean
