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

(** These functors allows automatic logging of transfer functions. You
    define how to handle functions of different arities, and how to
    print values of different types, and then you can automatically
    log transfer functions of a given signature (the functor names
    correspond to this signature).

    See {!Domains.Domain_log} and {!Single_value_abstraction.Log} for examples
    of instantiations.  *)

open Operator_sig
open Units

module type BOOLEAN_CONVERSION = sig
  module Arity : ARITY
  type boolean
  type 'a pp
  val bool_printer : boolean pp
  val ar0: 'r pp -> (Format.formatter -> unit) -> 'r Arity.ar0 -> 'r Arity.ar0
  val ar1: 'a pp -> 'r pp -> (Format.formatter -> unit) ->
    ('a,'r) Arity.ar1 -> ('a,'r) Arity.ar1
  val ar2: 'a pp -> 'b pp -> 'r pp -> (Format.formatter -> unit) ->
    ('a,'b,'r) Arity.ar2 -> ('a,'b,'r) Arity.ar2
end

module type INTEGER_CONVERSION = sig
  include BOOLEAN_CONVERSION
  type integer
  val integer_printer : integer pp
end

module type ENUM_CONVERSION = sig
  include BOOLEAN_CONVERSION
  type enum
  val enum_printer : enum pp
end

module type BITVECTOR_CONVERSION = sig
  include BOOLEAN_CONVERSION
  type bitvector
  val bv_printer : size:In_bits.t -> bitvector pp
end

module type MEMORY_CONVERSION = sig
  include BOOLEAN_CONVERSION
  val ar3: 'a pp -> 'b pp -> 'c pp -> 'r pp -> (Format.formatter -> unit) ->
    ('a,'b,'c,'r) Arity.ar3 -> ('a,'b,'c,'r) Arity.ar3
  
  type block
  type memory
  type address
  type value
  val block_printer: block pp    
  val address_printer: address pp
  val memory_printer: memory pp
  val value_printer: size:In_bits.t -> value pp
  val prod_printer: 'a pp -> 'b pp -> ('a * 'b) pp
end


module Log_Boolean_Backward
    (C : BOOLEAN_CONVERSION)
    (F : BOOLEAN_BACKWARD
     with module Arity := C.Arity
      and type boolean := C.boolean) :
  BOOLEAN_BACKWARD with module Arity := C.Arity and type boolean := C.boolean

module Log_Integer_Backward
    (C : INTEGER_CONVERSION)
    (F : INTEGER_BACKWARD
     with module Arity := C.Arity
      and type boolean := C.boolean
      and type integer := C.integer) :
  INTEGER_BACKWARD
  with module Arity := C.Arity
   and type boolean := C.boolean
   and type integer := C.integer

module Log_Bitvector_Forward
    (C : BITVECTOR_CONVERSION)
    (F : BITVECTOR_FORWARD
     with module Arity := C.Arity
      and type boolean := C.boolean
      and type bitvector := C.bitvector) :
  BITVECTOR_FORWARD
  with module Arity := C.Arity
   and type boolean := C.boolean
   and type bitvector := C.bitvector

module Log_Bitvector_Forward_With_Bimul_add
    (C : BITVECTOR_CONVERSION)
    (F : BITVECTOR_FORWARD_WITH_BIMUL_ADD
     with module Arity := C.Arity
      and type boolean := C.boolean
      and type bitvector := C.bitvector) :
  BITVECTOR_FORWARD_WITH_BIMUL_ADD
  with module Arity := C.Arity
   and type boolean := C.boolean
   and type bitvector := C.bitvector

module Log_Binary_Forward
    (C : BITVECTOR_CONVERSION)
    (F : BINARY_FORWARD
     with module Arity := C.Arity
      and type boolean := C.boolean
      and type binary := C.bitvector) :
  BINARY_FORWARD
  with module Arity := C.Arity
   and type boolean := C.boolean
   and type binary := C.bitvector

module Log_Enum_Forward
    (C : ENUM_CONVERSION)
    (F : ENUM_FORWARD
     with module Arity := C.Arity
      and type boolean := C.boolean
      and type enum := C.enum) :
  ENUM_FORWARD
  with module Arity := C.Arity
   and type boolean := C.boolean
   and type enum := C.enum

module Log_Memory_Forward
    (C : MEMORY_CONVERSION)
    (F : MEMORY_FORWARD
     with module Arity := C.Arity
      and type boolean := C.boolean
      and type memory := C.memory
      and type block := C.block
      and type address := C.address
      and type value := C.value) :
  MEMORY_FORWARD
  with module Arity := C.Arity
   and type boolean := C.boolean
   and type memory := C.memory
   and type block := C.block
   and type value := C.value
   and type address := C.address
