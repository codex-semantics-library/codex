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

open Units

(** Signature for abstract domains whose "dimensions" or "variables"
    are variables of the constraints language. *)
module type Domain_S = sig
    module Terms: Terms.Sig.TERMS

    val name: string

    (** Type for elements of the abstract domain. The interface is
        persistent but the implementation can be imperative. *)
    type t

    (** Represent binary, integer and boolean dimensions/variables in the abstract domain. *)
    type binary = Operator.Function_symbol.binary Terms.t
    type integer = Operator.Function_symbol.integer Terms.t
    type boolean = Operator.Function_symbol.boolean Terms.t
    type enum = Operator.Function_symbol.enum Terms.t

    val equal : t -> t -> bool
    val top : t

    (** Projection to a non-relational basis. *)
    module Integer_Query:sig
      include Sig.Integer_Query with type abstract_state := t and type integer := integer
    end
    module Query:sig
      module Boolean_Lattice:Single_value_abstraction.Sig.BOOLEAN_LATTICE with type t = Lattices.Quadrivalent.t
      module Integer_Lattice:Single_value_abstraction.Sig.INTEGER_LATTICE
      module Binary_Lattice:Single_value_abstraction.Sig.BITVECTOR_LATTICE
      module Enum_Lattice:Single_value_abstraction.Sig.ENUM_LATTICE
      val boolean: t -> boolean ->  Boolean_Lattice.t
      val integer: t -> integer ->  Integer_Lattice.t
      val binary: size:In_bits.t -> t -> binary ->  Binary_Lattice.t
      val enum: t -> enum ->  Enum_Lattice.t
    end

    (* Note: it would be simpler if we splitted into several arrays of types. *)
    (* TODO: Replace tuples of any by a type "any_tuple". *)
    val nondet:
      doma:t -> tupa:Terms.any Immutable_array.t ->
      domb:t -> tupb:Terms.any Immutable_array.t ->
      tupres: Terms.any Immutable_array.t -> t

    (** [widened_fixpoint_step ~previous ~previous_tup ~next ~next_tup bool ~res_tup] where:
        - [previous] is the previous domain state
          and [previous_tup] the tuple of phi arguments;
        - [next] is the next domain state obtained by joining
          execution of the function body and initial state and
          [next_tup] the phi arguments;
        - [bool] is false if we know that the fixpoint is not reached yet,
          and true otherwise;
        - [res_tup] is the terms corresponding to the phi function;

        returns a triple [(context,bool)] where:
        - [context] is the new domain state;
        - [bool] is true if the fixpoint is reached,
          and false if not reached or we don't know. *)
    val widened_fixpoint_step:
      previous:t -> previous_tup:Terms.any Immutable_array.t ->
      next:t -> next_tup:Terms.any Immutable_array.t ->
      bool -> res_tup: Terms.any Immutable_array.t -> t * bool



    val integer_empty: t -> integer -> t
    val boolean_empty: t -> boolean -> t
    val binary_empty: size:In_bits.t -> t -> binary -> t
    val enum_empty: t -> enum -> t

    val integer_unknown: t -> integer -> t
    val boolean_unknown: t -> boolean -> t
    val binary_unknown: size:In_bits.t -> t -> binary -> t
    val enum_unknown: enumsize:int -> t -> enum -> t

    (** Returns [None] if the result is bottom. *)
    val assume: t -> boolean -> t option

    module Domain_Arity:sig
      type 'r ar0 = t -> 'r -> t
      type ('a,'r) ar1 = t -> 'a -> 'r -> t
      type ('a,'b,'r) ar2 = t -> 'a -> 'b -> 'r -> t
      type ('a,'b,'c,'r) ar3 = t -> 'a -> 'b -> 'c -> 'r -> t
      type ('a,'r) variadic = t -> 'a list -> t
    end

    module Boolean_Forward:Operator.BOOLEAN_FORWARD
    with module Arity := Domain_Arity and type boolean := boolean

    module Integer_Forward:Operator.INTEGER_FORWARD
      with module Arity := Domain_Arity
       and type boolean := boolean
       and type integer := integer

    module Binary_Forward:Operator.BINARY_FORWARD
      with module Arity := Domain_Arity
       and type boolean := boolean
       and type binary := binary

    module Enum_Forward:Operator.ENUM_FORWARD
      with module Arity := Domain_Arity
       and type boolean := boolean
       and type enum := enum


    val boolean_pretty: t -> Format.formatter -> boolean -> unit
    val integer_pretty: t -> Format.formatter -> integer -> unit
    val binary_pretty: size:In_bits.t -> t -> Format.formatter -> binary -> unit
    val enum_pretty: t -> Format.formatter -> enum -> unit

    val pretty: Format.formatter -> t -> unit

    val fixpoint_open: unit -> unit

    (** Tells if fixpoint is reached, and prepare to write the new
       values. The returned pair (bool,function) allows to decide
       whether we should end the fixpoint computation; the function
       represents a continuation, and the boolean tells whether the
       fixpoint is reached (if yes, we can close, but we don't have
       to). *)
    val fixpoint_step:
      lvl:int ->
      iteration:int ->
      t -> actuals:Terms.any Immutable_array.t ->
      t -> args:Terms.any Immutable_array.t ->
      t -> finals:Terms.any Immutable_array.t ->
      bool * (close:bool -> Terms.any Immutable_array.t -> t)

  end

(** A superset of {!Domain_S} that support adding relations (as labeled union-find
    unions, Lesbre et al PLDI'25) between variables (Constraints). *)
module type DOMAIN_WITH_UNION = sig
  include Domain_S
  val union:
    t -> 'a Terms.t -> 'b Terms.t ->
    ('a, 'b) Terms.Relation.t ->
    t
end
