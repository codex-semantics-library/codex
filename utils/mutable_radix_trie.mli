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

(* A mutable radix trie. A radix trie is much like a trie, except that
   when a node has only one child, it is merged with its parents. This
   allows to use less memory.

   The key is given as a parameter to the module; "strings" are
   represented as lists of keys, where the head element is near the
   root of the trie and last elements near the leaves.

   Note that this trie structure allows lookup and replacement of
   interior nodes (i.e. we can attach values to both "abc" and
   "abcde"). *)

module Make(Key:sig
    type t
    val equal : t -> t -> bool
    val hash : t -> int
    val pretty : Format.formatter -> t -> unit
  end):
sig
  type 'a t 
  val create : unit -> 'a t
  val find : 'a t -> Key.t list -> 'a
  val replace : 'a t -> Key.t list -> 'a -> unit

  val fold: 'a t -> ('a -> 'b -> 'b) -> 'b -> 'b

  val map: 'a t -> ('a -> 'b) -> 'b t

  (* TODO: Remove suffixes when they all lead to the same value. *)
  (* val compress: ('a -> 'a -> bool) -> 'a t -> 'a t *)
  
  
  val pp_trie : (Format.formatter -> 'a -> unit) ->
    Format.formatter -> 'a t -> unit
end
    
