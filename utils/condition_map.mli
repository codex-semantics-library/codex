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

(** A map from conditions to lattice elements, which is incrementally
    refined according to the result of some operations. *)

(** Raised when using a condition map with a condition for which the
    value was never refined. *)
exception Never_refined

(** The type of lattice elements. *)
module type L = sig
  (* TODO: Maybe those should go in a separate "LTF", L transfer functions. *)
  (* val pretty: Format.formatter -> t -> unit *)
  (* val join: t -> t -> t *)
  (* val bottom: t *)
  (* val inter: t -> t -> t *)
  type t
  val equal: t -> t -> bool
  val compare: t -> t -> int
  val hash: t -> int
end

(** The type of conditions. This can be viewed as formulas, or as sets
    of valuations satisfying the formula. *)
(*   MAYBE: The formula terminology is probably simpler, so we shoud change it.. *)
module type CONDITION = sig
  type t
  val pretty: Format.formatter -> t -> unit
  val all: t                      (* true *)
  val equal: t -> t -> bool       (* set equality. *)
  val empty: t                    (* false *)
  val is_empty: t -> bool         (* is_false *)
  val inter: t -> t -> t          (* && *)
  val (&&~): t -> t -> t
  val union: t -> t -> t          (* || *)
  val (||~): t -> t -> t
  val disjoint: t -> t -> bool    (* a && b = false *)
  val is_included: t -> t -> bool (* a => b *)
  val complement: t -> t          (* !a *)
  val var: unit -> t              (* Fresh variable. *)
  val hash: t -> int
end

(* A partial map from condition to lattice elements. Partial means
   that the information about some lattice elements is not known. We
   can distinguish this value from "top" for optimization, and
   checking that we never try to use "find" on an unknown value. *)
module type LConditionMap = sig
  module L:L
  module Cond:CONDITION
  type t
  val pretty: (Format.formatter -> L.t -> unit) -> Format.formatter -> t -> unit
  (* TODO: Possibly labeled arguments should be replaced by having
     some module L as an argument to find and refine. *)
  val find: join:(L.t -> L.t -> L.t) -> bottom:L.t -> t -> Cond.t -> L.t
  val refine: inter:(L.t -> L.t -> L.t) -> t -> cond:Cond.t -> ?notcond:Cond.t -> L.t -> t
  val create_partial: t
end

module type LConditionMapFold = sig
  include LConditionMap

  (* Fold on every partition matching [cond]. *)
  (* : Limit folding: if too big, merge things. *)
  val fold_with_cond: t -> Cond.t -> 'a -> (L.t -> Cond.t -> 'a -> 'a) -> 'a
end



(** One implementation of {!LConditionMap}, where partitions are
    hierarchically split in two, based on the order in which they were
    inserted. *)
module ConditionMapTree(Cond:CONDITION):sig
  type 'a t
  module Make(L:L):LConditionMap with module L = L and module Cond = Cond and type t = L.t t
end;;

(** Another implementation of {!LConditionMap}: maintains a flat partition of the possible different
   values, with the BDD that lead to it. *)
module ConditionMapPartition(Cond:CONDITION):sig
  type 'a t
  module Make(L:L):LConditionMapFold with module L = L and module Cond = Cond and type t = L.t t
end;;

(* Combining condition maps with different lattice elements, possibly
   of different types; which improve the value of existing ones. *)
module type TRANSFER_FUNCTIONS = sig
  module Cond:CONDITION

  (* A condition map with key Cond.t and value L.t *)
  type 'a t

  (* Transfer functions. *)
  val ar0: (module LConditionMap with type t = 'a t and type L.t = 'a and type Cond.t = Cond.t) -> interres:('a -> 'a -> 'a) ->
    Cond.t -> 'a -> 'a t -> 'a t

  val ar1:
    (module LConditionMap with type t = 'a t and type L.t = 'a and type Cond.t = Cond.t) -> joina:('a -> 'a -> 'a) -> bottoma:'a ->
    (module LConditionMap with type t = 'res t and type L.t = 'res and type Cond.t = Cond.t) -> interres:('res -> 'res -> 'res) ->
    Cond.t -> ('a -> 'res) -> 'a t -> 'res t -> 'res t

  val ar2:
    (module LConditionMap with type t = 'a t and type L.t = 'a and type Cond.t = Cond.t) -> joina:('a -> 'a -> 'a) -> bottoma:'a ->
    (module LConditionMap with type t = 'b t and type L.t = 'b and type Cond.t = Cond.t) -> joinb:('b -> 'b -> 'b) -> bottomb:'b ->
    (module LConditionMap with type t = 'res t and type L.t = 'res and type Cond.t = Cond.t) ->  interres:('res -> 'res -> 'res) ->
    Cond.t -> ('a -> 'b -> 'res) -> 'a t -> 'b t -> 'res t -> 'res t

  val ar1_bwd:
    (module LConditionMap with type t = 'a t and type L.t = 'a and type Cond.t = Cond.t) -> joina:('a -> 'a -> 'a) -> bottoma:'a -> intera:('a -> 'a -> 'a) ->
    (module LConditionMap with type t = 'res t and type L.t = 'res and type Cond.t = Cond.t) -> joinres:('res -> 'res -> 'res) -> bottomres:'res ->
    Cond.t -> ('a -> 'res -> 'a option) ->
    'a t -> 'res t -> (Cond.t * 'a t)

  val ar2_bwd:
    (module LConditionMap with type t = 'a t and type L.t = 'a and type Cond.t = Cond.t) -> joina:('a -> 'a -> 'a) -> bottoma:'a -> intera:('a -> 'a -> 'a) ->
    (module LConditionMap with type t = 'b t and type L.t = 'b and type Cond.t = Cond.t) -> joinb:('b -> 'b -> 'b) -> bottomb:'b -> interb:('b -> 'b -> 'b) ->
    (module LConditionMap with type t = 'res t and type L.t = 'res and type Cond.t = Cond.t) -> joinres:('res -> 'res -> 'res) ->  bottomres:'res ->
    Cond.t -> ('a -> 'b -> 'res -> 'a option * 'b option) ->
    'a t -> 'b t -> 'res t -> (Cond.t * 'a t) * (Cond.t * 'b t)

  val nondet_disjoint:
    (module LConditionMap with type t = 'res t and type L.t = 'res and type Cond.t = Cond.t) ->
    conda:Cond.t -> notconda:Cond.t -> cma:'res t ->
    condb:Cond.t -> notcondb:Cond.t -> cmb:'res t ->
    join:('res -> 'res -> 'res) -> bottom:'res -> inter:('res -> 'res -> 'res) ->
    old:'res t  -> 'res t

  val nondet_non_disjoint:
    (module LConditionMap with type t = 'res t and type L.t = 'res and type Cond.t = Cond.t) ->
    conda:Cond.t -> cma:'res t ->
    condb:Cond.t -> cmb:'res t ->
    condaorb:Cond.t -> notcondaorb:Cond.t -> (* a || b; !(a || b) *)
    join:('res -> 'res -> 'res) -> bottom:'res -> inter:('res -> 'res -> 'res) ->
    old:'res t  -> 'res t

end

(* A generic and simple way to implement transfer functions. *)
module MakePathInsensitive(Cond:CONDITION)
    (M:sig type 'a t end):
  TRANSFER_FUNCTIONS with module Cond = Cond
                       and type 'a t = 'a M.t


(* module MakePathSensitive(Cond:Condition)
 *     (M:sig type 'a t end):sig
 *   module Cond:Condition
 *
 *   (\* A condition map with key Cond.t and value L.t *\)
 *   type 'a t
 *
 *   (\* Transfer functions. *\)
 *   val ar0: (module LConditionMap with type t = 'a t and type L.t = 'a and type Cond.t = Cond.t) ->
 *     Cond.t -> 'a -> 'a t -> 'a t
 *
 *   val ar1:
 *     (module LConditionMapFold with type t = 'a t and type L.t = 'a and type Cond.t = Cond.t) ->
 *     (module LConditionMap with type t = 'res t and type L.t = 'res and type Cond.t = Cond.t) ->
 *     Cond.t -> ('a -> 'res) -> 'a t -> 'res t -> 'res t
 *
 *   val ar2:
 *     (module LConditionMapFold with type t = 'a t and type L.t = 'a and type Cond.t = Cond.t) ->
 *     (module LConditionMapFold with type t = 'b t and type L.t = 'b and type Cond.t = Cond.t) ->
 *     (module LConditionMap with type t = 'res t and type L.t = 'res and type Cond.t = Cond.t) ->
 *     Cond.t -> ('a -> 'b -> 'res) -> 'a t -> 'b t -> 'res t -> 'res t
 *
 *
 *   val ar1_bwd:
 *     (module LConditionMapFold with type t = 'a t and type L.t = 'a and type Cond.t = Cond.t) ->
 *     (module LConditionMapFold with type t = 'res t and type L.t = 'res and type Cond.t = Cond.t) ->
 *     Cond.t -> ('a -> 'res -> 'a option) ->
 *     'a t -> 'res t -> (Cond.t * 'a t)
 *
 *   val ar2_bwd:
 *     (module LConditionMapFold with type t = 'a t and type L.t = 'a and type Cond.t = Cond.t) ->
 *     (module LConditionMapFold with type t = 'b t and type L.t = 'b and type Cond.t = Cond.t) ->
 *     (module LConditionMapFold with type t = 'res t and type L.t = 'res and type Cond.t = Cond.t) ->
 *     Cond.t -> ('a -> 'b -> 'res -> 'a option * 'b option) ->
 *     'a t -> 'b t -> 'res t -> (Cond.t * 'a t) * (Cond.t * 'b t)
 *
 *
 *   val nondet_disjoint:
 *     (module LConditionMapFold with type t = 'res t and type L.t = 'res and type Cond.t = Cond.t) ->
 *     conda:Cond.t -> notconda:Cond.t -> cma:'res t ->
 *     condb:Cond.t -> notcondb:Cond.t -> cmb:'res t ->
 *     old:'res t  -> 'res t
 *
 *   val nondet_non_disjoint:
 *     (module LConditionMapFold with type t = 'res t and type L.t = 'res and type Cond.t = Cond.t) ->
 *     conda:Cond.t -> cma:'res t ->
 *     condb:Cond.t -> cmb:'res t ->
 *     condaorb:Cond.t -> notcondaorb:Cond.t -> (\* a || b; !(a || b) *\)
 *     old:'res t  -> 'res t
 *
 * end with module Cond =  Cond
 *      and type 'a t = 'a M.t *)
