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

(* A generic immutable dynamic array datatype; it is similar to a
   linked list, except that it has O(1) access to any element of the
   sequence. Construction of a list by using append in a "fold"
   construct has each "append" in amortized O(1). However, multiple
   calls to append(v,x) with the same v and different x is O(length
   v) *)

type 'a t;;

(* Creates a fresh empty sequence. *)
val empty : unit -> 'a t
val singleton: 'a -> 'a t

(* Ammortized O(1). Element is appended at the end (towards greater indices). *)
val append : 'a t -> 'a -> 'a t
val length : 'a t -> int

(* Get an element of the sequence. O(1). Index start from 0. *)
val get : 'a t -> int -> 'a

val for_all: ('a ->  bool) -> 'a t -> bool
val for_all2: ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
val fold_lefti : ('a -> int -> 'b -> 'a) -> 'a -> 'b t -> 'a
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a  
val fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b t -> 'c t -> 'a
val fold_left3 : ('a -> 'b -> 'c -> 'd -> 'a) -> 'a -> 'b t -> 'c t -> 'd t -> 'a  
val iteri : (int -> 'a -> unit) -> 'a t -> unit
val iter : ('a -> unit) -> 'a t -> unit
val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
val mapi_filter: (int -> 'a -> 'b option) -> 'a t -> 'b t
val map : ('a -> 'b) -> 'a t -> 'b t
val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
val to_array: 'a t -> 'a array
val to_list: 'a t -> 'a list
val from_array: 'a array -> 'a t
val from_list: 'a list -> 'a t

val split: ('a * 'b) t -> 'a t * 'b t
val combine: 'a t -> 'b t -> ('a * 'b) t
(* Like from array, but does not perform a copy, so the array must
   not be modified afterwards. *)
val unsafe_from_array: 'a array -> 'a t

val init: int -> (int -> 'a) -> 'a t

  
type sformat = (unit,Format.formatter,unit) Stdlib.format
type 'a formatter = Format.formatter -> 'a -> unit
  
val pp_dynarray: ?pre:sformat -> ?sep:sformat -> ?suf:sformat -> 'a formatter -> 'a t formatter
val pp_dynarray2: ?pre:sformat -> ?sep:sformat -> ?suf:sformat -> (int * 'a) formatter -> 'a t formatter
