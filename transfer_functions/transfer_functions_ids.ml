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

(* Unique Id used by transfer functions.  *)

module MakeId():sig
  type t = private int
  val fresh: level:int -> t
  val get_level: t -> int
end = struct
  type t = int
  let count = ref 0
  let fresh ~level =
    assert(level < 16);
    assert(!count < 1 lsl (31 - 4)); (* No overflow *)
    incr count;
    ((!count) lsl 4) lor level;;
  let get_level x = x land 0xf;
end


(* Sets lack relational information that we need, so instead of sets
   we use decision trees: A union B becomes "if(cond) A else B", where
   cond is a condition variable.

   A /choice/ of an element in the set then corresponds to a valuation
   of all condition variables.

   Using named choices and condition variables allows to relate
   different choices in different sets, which cannot be done using
   normal sets.  *)
module Condition:sig
  type t = private int
  val fresh: level:int -> t
  val get_level: t -> int
end = MakeId()

module Choice:sig
  type t = private int
  val fresh: level:int -> t
  val get_level: t -> int
end = MakeId()

(* Identifier for malloc sites. *)
module Malloc_id:sig
  type t = private int * string
  val fresh: string -> t
  val hash: t -> int
  val to_string: t -> string
  val to_int: t -> int
  val compare: t -> t -> int
  val equal: t -> t -> bool
end
= struct
  type t = int * string
  let count = ref 0
  let fresh string = incr count; !count,string
  let to_int (x,_) = x
  let hash = to_int
  let to_string (_i,s) = s (* ^ "#" ^ (string_of_int i) *)
  let compare a b = (to_int a) - (to_int b)
  let equal a b =  (to_int a) == (to_int b)      
end
