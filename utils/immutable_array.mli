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


(* Immutable array: array are created at creation time, and never
   modified after this. Actually uses Array. *)

type 'a t

val length: 'a t -> int
val get: 'a t -> int -> 'a
val init: int -> (int -> 'a) -> 'a t
val empty: 'a t

type sformat = (unit,Format.formatter,unit) Stdlib.format
type 'a formatter = Format.formatter -> 'a -> unit

val pp_array: ?pre:sformat -> ?sep:sformat -> ?suf:sformat -> 'a formatter -> 'a t formatter

val iter : ('a -> unit) -> 'a t -> unit
val iter2 : ('a -> 'b -> unit) -> 'a t -> 'b t -> unit
val iter3 : ('a -> 'b -> 'c -> unit) -> 'a t -> 'b t -> 'c t -> unit  
val iteri : (int -> 'a -> unit) -> 'a t -> unit

val map2: ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

val fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b t -> 'c t -> 'a
val fold_left3 : ('a -> 'b -> 'c -> 'd -> 'a) -> 'a -> 'b t -> 'c t -> 'd t -> 'a  
val map : ('a -> 'b) -> 'a t -> 'b t
val to_list: 'a t -> 'a list

(* When using this function, you must guarantee that you won't use the
   array in the future. Else, it just means that we forbide writes to
   the array, not that they are constant. *)
(* val cast_from_array: 'a array -> 'a t *)

val of_list: 'a list -> 'a t
