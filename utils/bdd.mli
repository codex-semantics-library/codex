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


(* Note: this implementation of BDD does not require to know in
   advance the number of variables: new variables can be created and
   added to bdds without limitation.  *)
module Make(Var:sig
  type t
  val equal: t -> t -> bool
  val compare: t -> t -> int   (* Variable order in the bdd and mtbdd: smallest = closer to root. *)
  val hash: t -> int
  val pretty: Format.formatter -> t -> unit
end):sig

  type tag = private int

  module BDD:sig


    (* Standard BDD constructors. [mk] corresponds to If. *)
    type bdd = private Zero | One | If of tag * Var.t * bdd * bdd
    type t = bdd
    val zero: bdd
    val one: bdd
    val mk: Var.t -> bdd -> bdd -> bdd
    val var: Var.t -> bdd

    (* Comparison of BDDs. Note that equal can be replaced with ==. *)
    val equal: bdd -> bdd -> bool
    val hash: bdd -> int
    val compare: bdd -> bdd -> int
    val pretty: Format.formatter -> bdd -> unit


    (* Boolean operations on BDDs. The caches are kept between
       invocations of the methods; they can be cleared when
       desired. Several invocations allow to keep the caches
       separated. *)

    (* MAYBE: we could use weak key caches instead: keep the caches
       only when the bdd (which can be part of a larger bdd) exists. *)
    module WithCache(Param:sig val cache_default_size:int end):sig
      (* val clear: unit -> unit *)
      val (!~): bdd -> bdd
      val (&&~): bdd -> bdd -> bdd
      val (||~): bdd -> bdd -> bdd
      val (==>~): bdd -> bdd -> bdd
    end

    (* Default invocation with small default initial caches. *)
    val (!~): bdd -> bdd
    val (&&~): bdd -> bdd -> bdd
    val (||~): bdd -> bdd -> bdd
    val (==>~): bdd -> bdd -> bdd

  end

  (* Multiple terminal bdds, that can be viewed as a map from a
     boolean valuation of all variables to a Terminal. *)
  type 'a mtbdd = private Terminal of tag * 'a | If of tag * Var.t * 'a mtbdd * 'a mtbdd

  module type Terminal = sig
    type t
    val equal: t -> t -> bool
    val hash: t -> int
    val pretty: Format.formatter -> t -> unit
  end

  module type MTBDD = sig
    module Terminal:Terminal
    type t = Terminal.t mtbdd
    val equal: t -> t -> bool
    val hash: t -> int
    val pretty: Format.formatter -> t -> unit

    val terminal: Terminal.t -> t
    val mk: Var.t -> t -> t -> t
  end

  val map1:
    (module MTBDD with type Terminal.t = 'a) ->
    (module MTBDD with type Terminal.t = 'res) ->    
    ('a -> 'res) ->
    'a mtbdd -> 'res mtbdd
      
  val map2:
    (module MTBDD with type Terminal.t = 'a) ->
    (module MTBDD with type Terminal.t = 'b) ->
    (module MTBDD with type Terminal.t = 'res) ->    
    ('a -> 'b -> 'res) -> 
    'a mtbdd -> 'b mtbdd -> 'res mtbdd

  val map3:
    (module MTBDD with type Terminal.t = 'a) ->
    (module MTBDD with type Terminal.t = 'b) ->
    (module MTBDD with type Terminal.t = 'c) ->    
    (module MTBDD with type Terminal.t = 'res) ->    
    ('a -> 'b -> 'c -> 'res) -> 
    'a mtbdd -> 'b mtbdd -> 'c mtbdd -> 'res mtbdd

  
  (* Multiple terminal bdds, that can be viewed as a map from a
     boolean valuation of all variables to a Terminal. *)
  module MTBDD_Make(Terminal:Terminal):
  sig
    (* Base constructors. *)
    type t = Terminal.t mtbdd    
    val terminal: Terminal.t -> t
    val mk: Var.t -> t -> t -> t

    (* Comparison of BDDs. Note that equal can be replaced with ==. *)
    val equal: t -> t -> bool
    val hash: t -> int
    val compare: t -> t -> int
    val pretty: Format.formatter -> t -> unit

    (* Lift functions on terminals to their mtbdd counterparts. For
       multiple calls, it is best to do a partial application using the
       [f] argument (to share the cache). *)
    val map1: (Terminal.t -> Terminal.t) -> t -> t
    val map2: (Terminal.t -> Terminal.t -> Terminal.t) -> t -> t -> t

    (* Returns all the valuations of an [mtbdd] that match predicate
       [pred].  If multiple calls, it is best to do a partial
       application of the [pred] argument (to share the caches). *)
    val all: (Terminal.t -> bool) -> t -> BDD.t

    (* TODO: map3, map4. *)

    (* These functions work on sets of terminals. Several
       implementations are possible: e.g. with the interval basis, we
       could join the basis on the fly. We could use patricia tries
       (to have fast unions). We could have a special Terminal value
       which represents "error/not present", and would not be
       accounted in the map (or would indicate an error); etc. *)
    module With_Set(TerminalSet:sig
      type t
      val empty: t
      val singleton: Terminal.t -> t
      val union: t -> t -> t
      val add: Terminal.t -> t -> t
      val pretty: Format.formatter -> t -> unit
    end):sig

      (* Given a mtbdd (function from valuations), return a new one where the valuations in [bdd]
         have been changed to be mapped to the [Terminal.t]. *)
      val add: t -> BDD.bdd -> Terminal.t -> t

      (* Given a mtbdd (function from valuations), return a new one
         where the valuations in [bdd] have been updated according to
         f. Note that [add] is more efficient than [update (fun _ ->
         constant)] (it needs to traverse less of the original bdd). *)
      val update: t -> BDD.bdd -> (Terminal.t -> Terminal.t) -> t
      
      (* Given a mtbdd (function from valuations to terminals) and a
         bdd (i.e. a set of valuations), find the set of terminals
         corresponding to the set of valuations of the bdd. *)
      val find:t -> BDD.bdd -> TerminalSet.t
    end

  end


end
