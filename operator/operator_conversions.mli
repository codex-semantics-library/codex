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

(** Functors to change arities of transfer functions signatures (i.e. replace [ar0] with a new [ar0]).
    "Conversions"; i.e. passing the same transfer function (currently:
    with same types for dimension identifiers) with minimal changes. *)

open Operator_sig

module type Conversion = sig
  module From_Arity:ARITY
  module To_Arity:ARITY
  val ar0: 'r From_Arity.ar0 -> 'r To_Arity.ar0
  val ar1: ('a,'r) From_Arity.ar1 -> ('a,'r) To_Arity.ar1
  val ar2: ('a,'b,'r) From_Arity.ar2 -> ('a,'b,'r) To_Arity.ar2
  val ar3: ('a,'b,'c,'r) From_Arity.ar3 -> ('a,'b,'c,'r) To_Arity.ar3
end

module Convert_Boolean_Forward
  (C:Conversion)
  (F:BOOLEAN_FORWARD with module Arity := C.From_Arity):
  BOOLEAN_FORWARD with module Arity := C.To_Arity
                   and type boolean = F.boolean


module Convert_Integer_Forward
  (C:Conversion)
  (F:INTEGER_FORWARD with module Arity := C.From_Arity):
  INTEGER_FORWARD with module Arity := C.To_Arity
                   and type boolean = F.boolean
                   and type integer = F.integer


module Convert_Bitvector_Forward
  (C:Conversion)
  (F:BITVECTOR_FORWARD with module Arity := C.From_Arity):
  BITVECTOR_FORWARD with module Arity := C.To_Arity
                  and type boolean = F.boolean
                  and type bitvector = F.bitvector


module Convert_Binary_Forward
  (C:Conversion)
  (F:BINARY_FORWARD with module Arity := C.From_Arity):
  BINARY_FORWARD with module Arity := C.To_Arity
                  and type boolean = F.boolean
                  and type binary = F.binary

module Convert_Block_Forward
  (C:Conversion)
  (F:BLOCK_FORWARD with module Arity := C.From_Arity):
  BLOCK_FORWARD with module Arity := C.To_Arity
                  and type boolean = F.boolean
                  and type value = F.value
                  and type block = F.block
                  and type offset = F.offset

module Convert_Enum_Forward
  (C:Conversion)
  (F:ENUM_FORWARD with module Arity := C.From_Arity):
  ENUM_FORWARD with module Arity := C.To_Arity
                  and type boolean = F.boolean
                  and type enum = F.enum

module Convert_Memory_Forward
  (C:Conversion)
  (F:MEMORY_FORWARD with module Arity := C.From_Arity):
  MEMORY_FORWARD with module Arity := C.To_Arity
                  and type boolean = F.boolean
                  and type address = F.address
                  and type memory = F.memory
                  and type block = F.block
                  and type value  = F.value
