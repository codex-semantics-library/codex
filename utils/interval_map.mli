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

(** A rangemap partitions an interval in 0..size into sub-intervals,
   and associates a value to this interval (or, more precisely, to
   each element of this interval). *)

type 'a t

type key = int

(** Create a new interval map with a given size. *)
val create: size:int -> 'a -> 'a t

(** Iterate on every interval in [[key,key+size-1]] in a map.  *)
val iter_between: size:int -> key -> 'a t -> (size:int -> key -> 'a -> unit) -> unit

(** Fold on every interval in [[key,key+size-1]] in a map.  *)
val fold_between: size:int -> key -> 'a t -> 'b -> (size:int -> key -> 'a -> 'b -> 'b) -> 'b

(** Adds a mapping to the interval [[key,key+size-1]] in the map. *)
val store: size:int -> key -> 'a t -> 'a -> 'a t

(** Total size of the interval map. *)
val get_size: 'a t -> int

(** Calls [f ~size key va vb acc] when the value for the interval
   [[key,key+size-1]] is different in both maps (and is respectively [va]
   and [vb]). The different intervals are called in increasing order. *)
val fold_on_diff: 'a t -> 'a t -> 'b ->
  (size:int -> key -> 'a -> 'a -> 'b -> 'b) ->
  'b

(** Like {!fold_on_diff}, but with three maps. Less optimized too. *)
val fold_on_diff3: 'a t -> 'a t -> 'a t -> 'b ->
  (size:int -> key -> 'a -> 'a -> 'a -> 'b -> 'b) ->
  'b

(** Replace the value of every interval in [[key,key+size-1]]. Preserve
   physical equality when feasible. *)
val subst_between: key -> size:int -> 'a t -> (size:int -> key -> 'a -> 'a ) -> 'a t

(** This associates value to an interval, but splitting (or
   concatenating) interval affects the value they contain. *)
module With_Extract(Value:sig
  type t
  (* MAYBE: get_size, if needed. *)
  (* val extract: t -> idx:int -> size:int -> t *)
  (* concat? *)
  end):sig
  type t
  val create: size:int -> Value.t -> t
  val get_size: t -> int

  val iter_between: size:int -> key -> t ->
    extract:(Value.t -> idx:int -> size:int -> oldsize:int -> 'extracted) ->
    (size:int -> key -> 'extracted -> unit) -> unit
  val fold_between: size:int -> key -> t ->
    extract:(Value.t -> idx:int -> size:int -> oldsize:int -> 'extracted) ->
    'b -> (size:int -> key -> 'extracted -> 'b -> 'b) -> 'b
  val store: size:int -> key -> t -> Value.t -> t

  (** Guarantees that we call the function only on values that are not [equal]. *)
  val fold_on_diff: t -> t -> 'a ->
    extracta:(Value.t -> idx:int -> size:int -> oldsize:int -> 'extracted) ->
    extractb:(Value.t -> idx:int -> size:int -> oldsize:int -> 'extracted) ->
    (size:int -> key -> 'extracted -> 'extracted -> 'a -> 'a) ->
    'a

  val fold_on_diff3: t -> t -> t -> 'a ->
    extract:(Value.t -> idx:int -> size:int -> oldsize:int -> 'extracted) ->
    (size:int -> key -> 'extracted -> 'extracted -> 'extracted -> 'a -> 'a) ->
    'a

  (** Call subst on every interval in [[key,key+size-1]]. Preserve physical
     equality when feasible. *)
  val subst_between: key -> size:int -> t ->
    extract:(Value.t -> idx:int -> size:int -> oldsize:int -> Value.t) ->
    (size:int -> key -> Value.t -> Value.t ) -> t
end
