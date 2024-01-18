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

module V : sig
  include Graph.Sig.VERTEX with type t = Dba.id

  val hash : t -> int
  val pretty : Format.formatter -> t -> unit
end

module E : sig
  include Graph.Sig.EDGE
    with type vertex = V.t
    and type t = V.t * V.t
    and type label = unit
  val pretty : Format.formatter -> t -> unit
end

module G : sig
  include Graph.Sig.P with module V = V and module E = E
end

module Dhunk_regex : Fixpoint.Regex.S
  with type letter = G.E.t
  and type t = G.E.t Fixpoint.Regex.tagged_regex
module Reduce : module type of Fixpoint.Reduce.Make(G)(Dhunk_regex)
module Wto_alg : module type of Fixpoint.Wto.Make(G.V)

(** For each outer jump instruction in a dhunk, return the path expression of
    that DBA instruction in the dhunk, and optionnally the target address, if it
    statically known. *)
val exit_regexes : Dhunk.t -> (Dba.id * Dhunk_regex.t * Virtual_address.t option) list
