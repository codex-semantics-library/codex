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


(* Signature for abstract domains whose "dimensions" or "variables"
   are variables of the constraints language. *)
module type Domain_S = sig
    module Constraints:Constraints.Constraints_sig.Constraints

    val name: string
    
    (* Type for elements of the abstract domain. The interface is
       persistent but the implementation can be imperative. *)
    type t

    (* Represent binary, integer and boolean dimensions in the abstract
       domain. *)
    type binary = Transfer_functions.Term.binary Constraints.t      
    type integer = Transfer_functions.Term.integer Constraints.t
    type boolean = Transfer_functions.Term.boolean Constraints.t
  
    val equal : t -> t -> bool
    val top : t

    (* Projection to a non-relational basis. *)
    module Query:sig
      module Boolean_Lattice:Single_value_abstraction.Sig.Boolean_Lattice with type t = Lattices.Quadrivalent.t
      module Integer_Lattice:Single_value_abstraction.Sig.Integer_Lattice
      module Binary_Lattice:Single_value_abstraction.Sig.Binary_Lattice
      include Single_value_abstraction.Sig.Boolean_Conversions with type boolean := Boolean_Lattice.t
      include Single_value_abstraction.Sig.Integer_Conversions with type integer := Integer_Lattice.t
      include Single_value_abstraction.Sig.Binary_Conversions with type binary := Binary_Lattice.t
      val boolean: t -> boolean ->  Boolean_Lattice.t
      val integer: t -> integer ->  Integer_Lattice.t
      val binary: size:int -> t -> binary ->  Binary_Lattice.t
    end
    
    (* Note: it would be simpler if we splitted into several arrays of types. *)
    (* TODO: Replace tuples of any by a type "any_tuple". *)
    val nondet:
      doma:t -> tupa:Constraints.any Immutable_array.t ->
      domb:t -> tupb:Constraints.any Immutable_array.t ->
      tupres: Constraints.any Immutable_array.t -> t
    
    val integer_empty: t -> integer -> t                   
    val boolean_empty: t -> boolean -> t
    val binary_empty: size:int -> t -> binary -> t

    val integer_unknown: t -> integer -> t                   
    val boolean_unknown: t -> boolean -> t
    val binary_unknown: size:int -> t -> binary -> t

    (* Returns None if the result is bottom. *)
    val assume: t -> boolean -> t option
  
    module Domain_Arity:sig
      type 'r ar0 = t -> 'r -> t
      type ('a,'r) ar1 = t -> 'a -> 'r -> t
      type ('a,'b,'r) ar2 = t -> 'a -> 'b -> 'r -> t
      type ('a,'b,'c,'r) ar3 = t -> 'a -> 'b -> 'c -> 'r -> t
      type ('a,'r) variadic = t -> 'a list -> t
    end
  
    module Boolean_Forward:Transfer_functions.Boolean_Forward
    with module Arity := Domain_Arity and type boolean := boolean
  
    module Integer_Forward:Transfer_functions.Integer_Forward
      with module Arity := Domain_Arity
       and type boolean := boolean
       and type integer := integer

    module Binary_Forward:Transfer_functions.Binary_Forward
      with module Arity := Domain_Arity
       and type boolean := boolean
       and type binary := binary

    
    val boolean_pretty: t -> Format.formatter -> boolean -> unit
    val integer_pretty: t -> Format.formatter -> integer -> unit
    val binary_pretty: size:int -> t -> Format.formatter -> binary -> unit      

    val pretty: Format.formatter -> t -> unit

    val fixpoint_open: unit -> unit
    
    (* Tells if fixpoint is reached, and prepare to write the new
       values. The returned pair (bool,function) allows to decide
       whether we should end the fixpoint computation; the function
       represents a continuation, and the boolean tells whether the
       fixpoint is reached (if yes, we can close, but we don't have
       to). *)
    val fixpoint_step:
      lvl:int -> 
      t -> actuals:Constraints.any Immutable_array.t ->
      t -> args:Constraints.any Immutable_array.t ->
      t -> finals:Constraints.any Immutable_array.t ->
      bool * (close:bool -> Constraints.any Immutable_array.t -> t)

  end
