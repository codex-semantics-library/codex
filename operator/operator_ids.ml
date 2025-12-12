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



(** Unique identifier for [malloc] sites, which eventually includes all
    allocations in a C program. We also give string as a convenient
    name for these allocations. *)
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
  let compare a b = Int.compare (to_int a) (to_int b)
  let equal a b =  (to_int a) == (to_int b)
end

(** Generative functor to create unique ids.  *)
module [@inline always] MakeId():sig
  type t = private int
  val fresh: unit -> t
end = struct
  type t = int
  let count = ref 0
  let fresh () =
    incr count;
    !count
end


(** We want a choose operation on sets (which normally selects an
    arbitrary elements in a set) but we want to tell whether two
    distinct choose(S) operations selected the same element or not.

    For this we parameterized the choice function by a choice
    identifier: Two calls with the same identifier on the same set
    return the same value. This makes the choice function
    deterministic (but parameterized by an unknown value).

    We also represent sets by decision trees: A union B is really
    "if(cond) A else B", where cond is a fresh condition variable
    (that we never use). Thus, a choice can viewed as a valuation of
    different condition variables. We can thus express that
    choise_c(S) is equal to one element in the set (we just don't know
    which one). *)
module Condition:sig
  type t = private int
  val fresh: unit -> t
end = MakeId()

module Choice:sig
  type t = private int
  val fresh: unit -> t
end = MakeId()
