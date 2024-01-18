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

(* Implementation of Chris Okasaki's Fast mergeable interval maps. *)

(* All functions try to preserve physical equality whenever possible. *)
module type S = sig
  type 'a t
  type key
  val empty: 'a t
  val singleton: key -> 'a -> 'a t
  val find: key -> 'a t -> 'a
  val find_opt: key -> 'a t -> 'a option
  val remove: key -> 'a t -> 'a t
  (* The function tells what to do when a value was already
     present. *)
  val insert : (old:'a -> value:'a -> 'a) -> key -> 'a -> 'a t -> 'a t
  val add : key -> 'a -> 'a t -> 'a t

  val cardinal: 'a t -> int

  (* These iterate in ascending order ONLY IF all ids are positive. *)
  val map: ('a -> 'b) -> 'a t -> 'b t
  val mapi: (int -> 'a -> 'b) -> 'a t -> 'b t

  (* Returns the binding for which the integer is the smallest
     (if all ids are positive). *)
  val min_elt: 'a t -> (int * 'a)
  val max_elt: 'a t -> (int * 'a)

  (* Fast equality test between maps. *)
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int

  (* As we do not store the keys in the map, we can only access its id. *)
  (* Unlike the standard Map.union, this assumes [f _ a a = a]. *)
  val unioni : (int -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val interi : (int -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t

  (* Like interi, but when the keys differ, we can also answer None to remove the key.  *)
  val interi_filter : (int -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t

  val union : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val inter : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t

  (* These iterate in ascending order ONLY IF all ids are positive. *)
  val iter : (int -> 'a -> unit) -> 'a t -> unit
  val fold : (int -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val to_list : 'a t -> (int * 'a) list

  val is_singleton : 'a t -> 'a option

  (* union_inter funion finter a b c is equivalent to union funion ta @@ inter finter tb tc. *)
  (* val union_inter:
   *   funion:('a -> 'a -> 'a) ->
   *   finter:('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t -> 'a t *)

end

include S with type key = int

(* Note: This requires keys to be uniquely identified by an integer,
   i.e. (Key.to_int a) == (Key.to_int b) means (Key.equal a b) (to_int
   is injective). *)
module Make(Key:sig
  type t
  val to_int: t -> int
end):S with type key = Key.t

(* This version also stores the key just so that we can pretty-print the map.
   It takes much more memory space, so this should be used only when debugging. *)
module MakePretty(Key:sig
  type t
  val to_int: t -> int
  val pp: Format.formatter -> t -> unit
end):sig
  include S with type key = Key.t
  val pp: (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

(* This version is intended to be a replacement for Map.Make. *)
module MakeWithKey(Key:sig
  type t
  val to_int: t -> int
  val pp: Format.formatter -> t -> unit
end):sig
  include S with type key = Key.t
  val iter: (Key.t -> 'a -> unit) -> 'a t -> unit
  val fold: (Key.t -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val to_list : 'a t -> (Key.t * 'a) list
end
