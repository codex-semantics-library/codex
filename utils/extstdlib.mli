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

(* Helper functions added to the standard library.  *)


module Array :
sig
  include module type of Array
  val for_all : ('a -> bool) -> 'a array -> bool
  val reduce : ('a -> 'a -> 'a) -> 'a array -> 'a
  val map_reduce : ('a -> 'b) -> ('b -> 'b -> 'b) -> 'a array -> 'b
  val filter : ('a -> bool) -> 'a array -> 'a array
  val equivalent : ('a -> 'a -> bool) -> 'a array -> bool
end

module List :
sig
  include module type of List
  val reduce : ('a -> 'a -> 'a) -> 'a list -> 'a

  (* Compatibility with old OCaml versions *)
  val equal : ('a -> 'a -> bool) -> 'a list -> 'a list -> bool
end

module Option:sig
  val the: 'a option -> 'a
  val map: ('a -> 'b) -> 'a option -> 'b option
end

module Stream :
sig
  include module type of Stream with type 'a t = 'a Stream.t
  val map : ('a -> 'b) -> 'a Stream.t -> 'b Stream.t
  val fold : ('a -> 'b -> 'a) -> 'a -> 'b Stream.t -> 'a
  val reduce : ('a -> 'a -> 'a) -> 'a Stream.t -> 'a
end

module Map:sig

  module type S = sig
    include Map.S
    val is_singleton: 'a t -> (key * 'a) option
    val is_inter_empty: 'a t -> 'b t -> bool
    val mk_pretty: (Format.formatter -> key -> unit) -> (Format.formatter -> 'a -> unit) ->
      Format.formatter -> 'a t -> unit
    val for_all2: (key -> 'a option -> 'a option -> bool) -> 'a t -> 'a t -> bool
    val fold2:'a t -> 'b t -> 'c -> (key -> 'a option -> 'b option -> 'c -> 'c) -> 'c
    val fold_on_diff2:'a t -> 'a t -> 'c -> (key -> 'a option -> 'a option -> 'c -> 'c) -> 'c
    val fold3:'a t -> 'b t -> 'c t -> 'd -> (key -> 'a option -> 'b option -> 'c option -> 'd -> 'd) -> 'd
    val fold_on_diff3:'a t -> 'a t -> 'a t -> 'd -> (key -> 'a option -> 'a option -> 'a option -> 'd -> 'd) -> 'd      
  end
  
  module Make(Ord:Map.OrderedType):S with type key = Ord.t
end

module Set:sig

  module type S = sig
    include Set.S
    val reduce: (elt -> elt -> elt) -> t ->  elt
    val map_reduce: (elt -> 'a) -> ('a -> 'a -> 'a) -> t -> 'a
    val is_singleton: t -> bool
    val mk_pretty: (Format.formatter -> elt -> unit) -> Format.formatter -> t -> unit      
  end
  
  module Make(Ord:Set.OrderedType):S with type elt = Ord.t
end
