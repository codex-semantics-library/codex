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

(** A graph of (address, call_stack) pair that can be translated to regular
    expressions. It can be used to store a graph of instructions or a graph of
    basic blocks. *)

module V : sig
  type call_stack = Virtual_address.t list
  type t = Virtual_address.t * call_stack
  include Graph.Sig.VERTEX with type t := t

  val pretty : Format.formatter -> t -> unit
  val pp_stack : Format.formatter -> call_stack -> unit

  include Sigs.Collection with type t := t
end

module E : sig
  include Graph.Sig.EDGE
    with type vertex = V.t
    and type t = V.t * V.t
    and type label = unit
  val equal : t -> t -> bool
  val pretty : Format.formatter -> t -> unit
end

module Cfg : sig
  include Graph.Sig.I with module V = V and module E = E
end

module CfgRegex : Fixpoint.Regex.S
  with type letter = Cfg.E.t
  and type t = Cfg.E.t Fixpoint.Regex.tagged_regex
module Reduce : module type of Fixpoint.Reduce.Make(Cfg)(CfgRegex)
module Wto : module type of Fixpoint.Wto.Make(Cfg.V)
