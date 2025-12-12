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

(** Abstract type representing a prefix trie.  For most operations, it
    can be viewed as as set of strings. *)
type t


(** [add trie word] returns a new trie that contains all the strings
    from [trie] plus the string [word].  *)
val add : t -> string -> t


(** Represent the empty set of words.  *)
val empty_set : t

(** Represents a set with a single word. *)
val singleton : string -> t

(** [dump trie] prints the contents of [trie] to standard output, for
    debugging. *)
val dump : t -> unit

(** [to_list trie] returns the list of all strings stored in [trie] *)
val to_list : t -> string list


(** [suggestions trie prefix ~max] provides completion suggestions.  
   Given a [prefix], it returns:
     - the part of the prefix that can be auto-completed (common to all matches)
     - a list of up to [max] strings that start with [prefix].  
   Example: if [trie] contains ["cat"; "car"; "dog"; "door"]
     suggestions trie "ca" ~max:3 returns ("ca", ["cat"; "car"]). *)
val suggestions: t -> string -> max:int -> string * string list
