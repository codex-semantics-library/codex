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

(** This implements a regular expression suitable for forward dataflow
   analysis (concat has been replaced by append), where the regular
   expression is represented using a term graph. *)


module type LETTER = sig
  type t
  val equal : t -> t -> bool
  val pretty : Format.formatter -> t -> unit
  val hash : t -> int
end

(** Generic signature for a regex interface. *)
module type S = sig
  type letter

  type t

  val empty : t
  val epsilon : t

  val join : t -> t -> t
  val append : t -> letter -> t
  val append_star : t -> t -> t
  val equal : t -> t -> bool
  val hash : t -> int
  val pretty : Format.formatter -> t -> unit

  (** Print a regex in reverse. Useful sometimes when a regex is too big to be
   * fully printed, and you only want to see the end, you can start printing it
   * in reverse and interrupt the program. *)
  val pretty_reverse : Format.formatter -> t -> unit
end

(** Tagged regular expressions.  The tag is used for cons-hashing and share as
    many data structures as possibles. Tags should not be used outside of the
    implementation. *)
type 'l tagged_regex = private
  | Empty
  | Epsilon
  | Join of int * 'l tagged_regex * 'l tagged_regex
  | Append of int * 'l tagged_regex * 'l
  | AppendStar of int * 'l tagged_regex * 'l tagged_regex

module Make (L : LETTER) : S
    with type letter = L.t
    and type t = L.t tagged_regex
