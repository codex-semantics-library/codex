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

(* A mutable dynamic array. Accept any positive index, and will grow as required. 
   Trying to get an element that was not previously set, returns Not_found. *)

type 'a t

val empty: unit -> 'a t        (* Maybe: initial capacity? *)
val get: 'a t -> int -> 'a
val set: 'a t -> int -> 'a -> unit
val length: 'a t -> int         (* The length is the max of the indices used by set. *)
val append: 'a t -> 'a -> unit  (* Appends according to the length. *)
