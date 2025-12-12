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

(****************************************************************)
(** Standard abstraction for booleans. *)
module Quadrivalent = Lattices.Quadrivalent
module Bitfield = Lattices.Bitfield

(****************************************************************)
(** {1 Forward transfer functions} *)

module type WITH_BOOLEAN_FORWARD = sig
  type boolean = Quadrivalent.t
  module Boolean_Forward:Operator.BOOLEAN_FORWARD
    with module Arity := Operator.Forward_Arity and type boolean := boolean
end

module type WITH_INTEGER_FORWARD = sig
  type boolean = Quadrivalent.t
  type integer
  module Integer_Forward:Operator.INTEGER_FORWARD
    with module Arity := Operator.Forward_Arity
     and type boolean := boolean
     and type integer := integer
end


module type WITH_BITVECTOR_FORWARD = sig
  type boolean = Quadrivalent.t
  type bitvector
  module Bitvector_Forward:Operator.BITVECTOR_FORWARD_WITH_BIMUL_ADD
    with module Arity := Operator.Forward_Arity
     and type boolean := boolean
     and type bitvector := bitvector
end

module type WITH_ENUM_FORWARD = sig
  type boolean = Quadrivalent.t
  type enum
  module Enum_Forward:Operator.ENUM_FORWARD
    with module Arity := Operator.Forward_Arity
     and type boolean := boolean
     and type enum := enum
end

module type WITH_BLOCK_FORWARD = sig
  type boolean = Quadrivalent.t
  type bitvector
  type block
  module Block_Forward:Operator.BLOCK_FORWARD
    with module Arity := Operator.Forward_Arity
     and type boolean := boolean
     and type value := bitvector
     and type block := block
     and type offset := bitvector
end

module type WITH_MEMORY_FORWARD = sig
  type boolean = Quadrivalent.t
  type bitvector
  type memory
  type block
  module Memory_Forward:Operator.MEMORY_FORWARD
    with module Arity := Operator.Forward_Arity
     and type boolean := boolean
     and type address := bitvector
     and type memory := memory
     and type block := block
     and type value := bitvector
end

module type WITH_MEMORY_BITVECTOR_BOOLEAN_FORWARD = sig
  include WITH_MEMORY_FORWARD
  include WITH_BLOCK_FORWARD with type boolean := boolean
                              and type bitvector := bitvector
                              and type block := block
  include WITH_BITVECTOR_FORWARD with type boolean := boolean
                                  and type bitvector := bitvector
  include WITH_BOOLEAN_FORWARD with type boolean := boolean
end

module type WITH_MEMORY_BITVECTOR_INTEGER_BOOLEAN_FORWARD = sig
  include WITH_MEMORY_FORWARD
  include WITH_BLOCK_FORWARD with type boolean := boolean
                              and type bitvector := bitvector
                              and type block := block
  include WITH_BITVECTOR_FORWARD with type boolean := boolean
                                  and type bitvector := bitvector
  include WITH_INTEGER_FORWARD with type boolean := boolean
  include WITH_BOOLEAN_FORWARD with type boolean := boolean
end

module type WITH_FORWARD = sig
  include WITH_MEMORY_BITVECTOR_INTEGER_BOOLEAN_FORWARD
end


(****************************************************************)
(** {1 Backward transfer functions} *)

module type WITH_BOOLEAN_BACKWARD = sig
  type boolean = Quadrivalent.t
  module Boolean_Backward:Operator.BOOLEAN_BACKWARD
    with module Arity := Operator.Backward_Arity and type boolean := boolean;;
end

module type WITH_INTEGER_BACKWARD = sig
  type boolean = Quadrivalent.t
  type integer
  module Integer_Backward:Operator.INTEGER_BACKWARD
    with module Arity := Operator.Backward_Arity
     and type boolean := boolean
     and type integer := integer
end

module type WITH_ENUM_BACKWARD = sig
  type boolean = Quadrivalent.t
  type enum
  module Enum_Backward:Operator.ENUM_BACKWARD
    with module Arity := Operator.Backward_Arity
     and type boolean := boolean
     and type enum := enum
end

module type WITH_BITVECTOR_BACKWARD = sig
  type boolean = Quadrivalent.t
  type bitvector
  module Bitvector_Backward:sig
    include Operator.BITVECTOR_BACKWARD
      with module Arity := Operator.Backward_Arity
       and type boolean := boolean
       and type bitvector := bitvector

    (** Combined operation for multiplication and addition [x -> prod*x + offset].
        This operation does not overflows if only the intermediate term [prod*x] overflows. *)
    val bimul_add: size:Units.In_bits.t -> prod:Z.t -> offset:Z.t -> (bitvector,bitvector) Operator.Backward_Arity.ar1
  end
end

module type WITH_BLOCK_BACKWARD = sig
  type boolean = Quadrivalent.t
  type bitvector
  type block
  module Block_Backward:Operator.BLOCK_BACKWARD
    with module Arity := Operator.Backward_Arity
     and type boolean := boolean
     and type value := bitvector
     and type block := block
     and type offset := bitvector
end

module type WITH_MEMORY_BACKWARD = sig
  type boolean = Quadrivalent.t
  type bitvector
  type memory
  type block
  module Memory_Backward:Operator.MEMORY_BACKWARD
    with module Arity := Operator.Backward_Arity
     and type boolean := boolean
     and type address := bitvector
     and type memory := memory
     and type block := block
     and type value := bitvector
end

module type WITH_MEMORY_BITVECTOR_BOOLEAN_BACKWARD = sig
  include WITH_MEMORY_BACKWARD
  include WITH_BLOCK_BACKWARD with type boolean := boolean
                               and type bitvector := bitvector
                               and type block := block
  include WITH_BITVECTOR_BACKWARD with type boolean := boolean
                                   and type bitvector := bitvector
  include WITH_BOOLEAN_BACKWARD with type boolean := boolean
end

(****************************************************************)
(** {1 Both backward and forward} *)

module type WITH_BOOLEAN_FORWARD_BACKWARD = sig
  type boolean = Quadrivalent.t
  include WITH_BOOLEAN_FORWARD with type boolean := boolean
  include WITH_BOOLEAN_BACKWARD with type boolean := boolean
end

module type WITH_INTEGER_FORWARD_BACKWARD = sig
  type boolean = Quadrivalent.t
  type integer
  include WITH_INTEGER_FORWARD
    with type boolean := boolean
     and type integer := integer
  include WITH_INTEGER_BACKWARD
    with type boolean := boolean
     and type integer := integer
end

module type WITH_ENUM_FORWARD_BACKWARD = sig
  type boolean = Quadrivalent.t
  type enum
  include WITH_ENUM_FORWARD
    with type boolean := boolean
     and type enum := enum
  include WITH_ENUM_BACKWARD
    with type boolean := boolean
     and type enum := enum
end

module type WITH_BITVECTOR_FORWARD_BACKWARD = sig
  type boolean = Quadrivalent.t
  type bitvector
  include WITH_BITVECTOR_FORWARD
    with type boolean := boolean
     and type bitvector := bitvector
  include WITH_BITVECTOR_BACKWARD
    with type boolean := boolean
     and type bitvector := bitvector
end

module type WITH_BLOCK_FORWARD_BACKWARD = sig
  type boolean = Quadrivalent.t
  type bitvector
  type block
  include WITH_BLOCK_FORWARD
    with type boolean := boolean
     and type bitvector := bitvector
     and type block := block
  include WITH_BLOCK_BACKWARD
    with type boolean := boolean
     and type bitvector := bitvector
     and type block := block
end

module type WITH_MEMORY_FORWARD_BACKWARD = sig
  type boolean = Quadrivalent.t
  type bitvector
  type memory
  type block
  include WITH_MEMORY_FORWARD
    with type boolean := boolean
     and type bitvector := bitvector
     and type memory := memory
     and type block := block
  include WITH_MEMORY_BACKWARD
    with type boolean := boolean
     and type bitvector := bitvector
     and type memory := memory
     and type block := block
end

module type WITH_BITVECTOR_BOOLEAN_FORWARD_BACKWARD = sig
  include WITH_BITVECTOR_FORWARD_BACKWARD
  include WITH_BOOLEAN_FORWARD_BACKWARD with type boolean := boolean
end


module type WITH_MEMORY_BITVECTOR_BOOLEAN_FORWARD_BACKWARD = sig
  include WITH_MEMORY_FORWARD_BACKWARD;;
  include WITH_BLOCK_FORWARD_BACKWARD with type boolean := boolean
                                       and type bitvector := bitvector
                                       and type block := block
  include WITH_BITVECTOR_BOOLEAN_FORWARD_BACKWARD with type boolean := boolean
                                                   and type bitvector := bitvector
end


module type WITH_FORWARD_BACKWARD = sig
  include WITH_MEMORY_BITVECTOR_BOOLEAN_FORWARD_BACKWARD
end

(****************************************************************)
(** {1 Base versions} *)

module type BOOLEAN_LATTICE = Lattices.Sig.BOOLEAN_LATTICE

module type INTEGER_LATTICE = Lattices.Sig.INTEGER_LATTICE

module type BITVECTOR_LATTICE = Lattices.Sig.BITVECTOR_LATTICE

module type ENUM_LATTICE = Lattices.Sig.ENUM_LATTICE

module type MEMORY_LATTICE = sig
  include Lattices.Sig.JOIN_SEMI_LATTICE_WITH_BOTTOM
end

module type BLOCK_LATTICE = sig
  include Lattices.Sig.JOIN_SEMI_LATTICE_WITH_BOTTOM
end

module type BOOLEAN = sig
  val name: string
  include WITH_BOOLEAN_FORWARD_BACKWARD
  module Boolean_Lattice:BOOLEAN_LATTICE with type t = boolean
end

module type INTEGER = sig
  val name: string
  include WITH_INTEGER_FORWARD_BACKWARD
  include WITH_BOOLEAN_FORWARD_BACKWARD with type boolean := boolean
  module Boolean_Lattice:BOOLEAN_LATTICE with type t = boolean
  module Integer_Lattice:INTEGER_LATTICE with type t = integer
end

module type ENUM = sig
  val name: string
  include WITH_ENUM_FORWARD_BACKWARD
  include WITH_BOOLEAN_FORWARD_BACKWARD with type boolean := boolean
  module Boolean_Lattice:BOOLEAN_LATTICE with type t = boolean
  module Enum_Lattice:ENUM_LATTICE with type t = enum
end

module type BITVECTOR = sig
  val name: string
  include WITH_BITVECTOR_FORWARD_BACKWARD
  include WITH_BOOLEAN_FORWARD_BACKWARD with type boolean := boolean
  module Boolean_Lattice:BOOLEAN_LATTICE with type t = boolean
  module Bitvector_Lattice:BITVECTOR_LATTICE with type t = bitvector
end

module type BITVECTOR_ENUM = sig
  include BITVECTOR
  include ENUM with type boolean := boolean
                and module Boolean_Lattice := Boolean_Lattice
                and module Boolean_Forward := Boolean_Forward
                and module Boolean_Backward := Boolean_Backward
end

module type NUMERIC = sig
  include BITVECTOR
  include INTEGER with type boolean := boolean
                   and module Boolean_Lattice := Boolean_Lattice
                   and module Boolean_Forward := Boolean_Forward
                   and module Boolean_Backward := Boolean_Backward
end

module type NUMERIC_ENUM = sig
  include BITVECTOR
  include INTEGER with type boolean := boolean
                   and module Boolean_Lattice := Boolean_Lattice
                   and module Boolean_Forward := Boolean_Forward
                   and module Boolean_Backward := Boolean_Backward

  include ENUM with type boolean := boolean
                and module Boolean_Lattice := Boolean_Lattice
                and module Boolean_Forward := Boolean_Forward
                and module Boolean_Backward := Boolean_Backward
end


(** Most complete version, with all datatypes. Used as a parameter for
    functors. *)
module type ALL = sig
  val name: string
  include WITH_BOOLEAN_FORWARD_BACKWARD
  include WITH_INTEGER_FORWARD_BACKWARD with type boolean := boolean
  include WITH_BITVECTOR_FORWARD_BACKWARD with type boolean := boolean
  include WITH_BLOCK_FORWARD_BACKWARD with type boolean := boolean
                                       and type bitvector := bitvector
  include WITH_MEMORY_FORWARD_BACKWARD with type bitvector := bitvector
                                        and type boolean := boolean
                                        and type block := block
  include WITH_ENUM_FORWARD_BACKWARD with type boolean := boolean
  module Boolean_Lattice:BOOLEAN_LATTICE with type t = boolean
  module Integer_Lattice:INTEGER_LATTICE with type t = integer
  module Bitvector_Lattice:BITVECTOR_LATTICE with type t = bitvector
  module Memory_Lattice:MEMORY_LATTICE with type t = memory
  module Enum_Lattice:ENUM_LATTICE with type t = enum
  module Block_Lattice:BLOCK_LATTICE with type t = block
end
