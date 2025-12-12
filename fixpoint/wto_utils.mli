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

open Wto

module type NODE = sig
  type t
  val equal: t -> t -> bool
  val hash: t -> int
  val pretty: Format.formatter -> t -> unit
end

module Make(N : NODE) : sig
  (** [map f wto] applies [f] to every element of [wto] while conserving the
      w.t.o. structure. The second argument of [f] is [true] if the argument of
      [f] is a head. *)
  val map : (N.t -> bool -> 'a) -> N.t partition -> 'a partition

  (** [iter f wto] applies [f] to every element of [wto]. The second argument
      of [f] is true if the argument of [f] is a head. *)
  val iter : (N.t -> bool -> unit) -> N.t partition -> unit

  (** [head_of wto h n] returns whether [n] is inside the component of head
      [h]. [h] must be a head. Returns true also if [n = h].
      Complexity: linear in the number of nodes. *)
  val is_head_of : N.t partition -> N.t -> N.t -> bool

  (** Maximum number of heads among all nodes, i.e. maximum number of nested
      components. *)
  val depth : N.t partition -> int
end
