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

(** Signature for the parts of memory domain.

   The general idea is this. A memory maps adresses to
   values. Memories are built hierarchically: a part of a memory may
   be represented by another memory (e.g. whole memory can be
   parametrized by how memory is represented in each region, arrays
   can be parametrized by how memory is represented inside single
   cells, etc.). Ultimately, memories contain scalar /values/.

   The fact that the value at the bottom of the hierarchy in the
   memory may contain parts of the adresses used at the top of the
   hierarchy requires a particular instantiation of functors. In
   general, a functor use the adresses of its parameter to build its
   own adresses. The adress of the top-level functor can be used as
   the value for the whole chain of functors, and is passed as an
   argument to build the memories.

   So memory "domains" contains two parts: a part used to build
   adresses and the final value, which is passed as an argument to the
   second part, to build memories. *)



(** Used to distinguish cases for weak updates, strong updates, or
   completely invalid destinations. *)
type 'a precision =
  | Empty
  | Singleton of 'a
  | Imprecise


(** An Abstract Abstract Datatype ({!AADT}) is just a data structure that
    can contains symbolic values constrainted by a base domain, named
    Scalar. *)
module type AADT = sig
  module Scalar:Sig.BASE
end


(** For {!AADT}s that redefine the notion of boolean. *)
module type WITH_BOOLEAN_REDEFINITION = sig
  include Sig.With_Boolean_Forward
  include Sig.With_Boolean with type boolean := boolean
  val assume: Context.t -> boolean -> Context.t option
end

module type AADT_WITH_BOOLEAN = sig
  include AADT
  include (WITH_BOOLEAN_REDEFINITION with module Context = Scalar.Context)
end


(** A scalar value stored inside a memory region. Should be a subset of {!Sig.MINIMAL} + {!Sig.WITH_BINARY}. *)
(* TODO: Merge with Fixed_size_value. Clarify Value in the sense
    (result of the evaluation of expressions) from "elements in an array". *)
module type VALUE = sig
  include AADT_WITH_BOOLEAN
  type binary
  (** Generally different from {!Scalar.binary}! *)

  val binary_pretty: size:In_bits.t -> Context.t -> Format.formatter -> binary -> unit
  val binary_empty: size:In_bits.t -> Context.t -> binary
  val binary_unknown: size:In_bits.t -> Context.t -> binary

  (** Returns an unknown value with a given type. *)
  val binary_unknown_typed : size:In_bits.t -> Context.t -> Types.TypedC.typ -> binary

  (** Type check a binary value *)
  val check_type : size:In_bits.t -> Context.t -> Types.TypedC.typ -> binary -> binary Types.Type_check_tree.t

  (** Serialize functions also return their de-serializer. *)
  val serialize: widens:bool -> size:In_bits.t -> Context.t -> binary -> Context.t -> binary ->
    'a Context.in_acc -> (binary,'a) Context.result

  (** Here we consider that all values are sets with a given cardinality, and most operations operate
     on values of cardinality 1, except union and choose which change the cardinality.

     Or, we could have a separate value_set type, wich functions
     empty, singleton, union and choose (this would prevent some
     errors and avoid the need to store cardinals for singleton
     values), between value sets and values. This requires to add set
     operations for most types, but it seems feasible.

     But there are many questions then: e.g. should division by zero
     return a set with one element, or assume that the divisor is
     non-zero and ignore divisions by zero? Probably the latter is
     fine, but then we need to throw an exception, like for memory,
     when an operation does not return any value,etc. Maybe we should
     differentiate sets with 0 and 1 elements from sets with more than
     1 element, etc.

     So for now, we do not differenciate values and sets using types. *)
  val union: Operator.Condition.t -> Context.t -> 'a Context.in_tuple -> 'a Context.out_tuple

  (** [addresses_in_binary ~size ctxa a ctxb b] should return a list of addresses corresponding to
      on both sides in values a and b of size [size] *)
  val addresses_in_binary : size:In_bits.t -> Context.t -> binary -> Context.t -> binary -> (binary * binary) list


  module Binary_Forward:sig
    val bextract: size:In_bits.t -> index:In_bits.t -> oldsize:In_bits.t ->
      Context.t -> binary -> binary
    val bconcat: size1:In_bits.t -> size2:In_bits.t -> Context.t -> binary -> binary -> binary
    val bchoose: size:In_bits.t -> Operator.Choice.t -> Context.t -> binary -> binary
  end

end

(** Abstraction of an offset in a contiguous memory region.  *)
module type WITH_OFFSET = sig
  module Scalar:Sig.BASE
  type boolean
  type offset
  module Offset:sig
    val pretty: Format.formatter -> offset -> unit
    val equal: offset -> offset -> bool
  end

  (* Should be offset_pretty, offset_empty, offset_serialize, etc.  *)
  val offset_pretty: Scalar.Context.t -> Format.formatter -> offset -> unit
  val offset_empty: Scalar.Context.t -> offset
  val serialize_offset: widens:bool -> Scalar.Context.t -> offset -> Scalar.Context.t -> offset ->
    'a Scalar.Context.in_acc -> (offset,'a) Scalar.Context.result
  ;;

  (** {1 Transfer functions} *)

  (** Beginning of a region (offset 0) of size [max] (in bytes, if known and/or useful). *)
  val offset_zero: max:int option -> Scalar.Context.t -> offset

  (** [offset_shift offset:k o] adds a suffix [.f], i.e. returns [o.f] when the field [f] has relative offset [k]. *)
  val offset_shift: offset:int -> max:int option -> Scalar.Context.t -> offset -> offset

  (** [offset_index size:k o i] adds a suffix [[i]], i.e. returns [o[i]] when o was pointing to an array of elements of size [k]. *)
  val offset_index: int -> Scalar.Context.t -> offset -> Scalar.binary -> offset

  (** [offset_sub o1 o2] when [o1] is a suffix of [o2] returns the suffix part. E.g. [offset_sub o1.k.a[3] o1]
     returns [.k.a[3]]. *)
  val offset_sub: Scalar.Context.t -> offset -> offset -> Scalar.binary

  (* TODO: offset_modulo. When o1 is of the form [o1.a[i].b[j].f] and
     elements of size a have the requested size [k], this would return
     .b[j].f. *)

  (** Offset comparison. *)
  val offset_le: Scalar.Context.t -> offset -> offset -> boolean
  val offset_eq: Scalar.Context.t -> offset -> offset -> boolean

  (** Check that all array indices operation are within bound. *)
  val offset_within_bounds: size:In_bits.t -> Scalar.Context.t -> offset -> boolean

  val offset_choose: Operator.Choice.t -> Scalar.Context.t -> offset -> offset


  (** Lift/unlift a {!boolean} to a {!Scalar.boolean}. Probably this should not
      be part of the domain signature. *)
  (* TODO: Given that boolean and Scalar.boolean are the same type,
     this should be removed. *)
  val boolean2scalar_bool : Scalar.Context.t -> boolean -> Scalar.boolean
  val scalar_bool2boolean : Scalar.Context.t -> Scalar.boolean -> boolean

end

(** An {!AADT} of offsets. *)
  (* TODO: We should drop the size argument, it is always ptr_size. *)
module type OFFSET = sig
    include AADT                (* MAYBE: Should this be AADT_WITH_BOOLEAN? *)
    include WITH_OFFSET with module Scalar := Scalar
                       and type boolean = Scalar.boolean (* May change in the future. *)
end


(** Abstraction of an address, pointing to a region in a C-like memory model.
    The type adress is named binary for historical reasons, and also to
    help sharing with bitvectors in whole. *)
module type WITH_ADDRESS = sig
  include AADT
  type boolean
  type binary

  module Binary:Datatype_sig.S with type t = binary

  val binary_pretty: size:In_bits.t -> Scalar.Context.t -> Format.formatter -> binary -> unit
  val binary_empty: size:In_bits.t -> Scalar.Context.t -> binary

  val satisfiable: Scalar.Context.t -> boolean -> Smtbackend.Smtlib_sig.sat

  (** Serialize functions also return their de-serializer. *)
  val serialize: widens:bool -> size:In_bits.t ->
    Scalar.Context.t -> binary -> Scalar.Context.t-> binary ->
    'a Scalar.Context.in_acc -> (binary,'a) Scalar.Context.result

  (* TODO: rename these transfer_functions to address_index, address_sub, etc. *)
  (* TODO: These should not require a size operand. The size of
     address is contant.  *)

  (* Note: we do not require a "top" representation for addresses. *)
  val bchoose: size:In_bits.t -> Operator.Choice.t -> Scalar.Context.t -> binary -> binary

  (** [bindex ~size k ctx address expr] is [address + k * expr] *)
  val bindex: size:In_bits.t -> int -> Scalar.Context.t -> binary -> Scalar.binary -> binary

  (** pointer substraction. *)
  val bisub: size:In_bits.t -> Scalar.Context.t -> binary -> binary -> Scalar.binary

  (** [bshift ~size ~offset ~max ctx address] is [address + offset]. In addition, further shifts/index are bounded by max. *)
  val bshift: size:In_bits.t -> offset:int -> max:int option -> Scalar.Context.t -> binary -> binary

  (** Pointer comparison. *)
  val ble: size:In_bits.t -> Scalar.Context.t -> binary -> binary -> boolean
  val beq: size:In_bits.t -> Scalar.Context.t -> binary -> binary -> boolean

  val binary_unknown : size:In_bits.t -> Scalar.Context.t -> binary
  val binary_unknown_typed: size:In_bits.t -> Scalar.Context.t -> Types.TypedC.typ -> binary

  (** Type check a {!binary} value *)
  val check_type : size:In_bits.t -> Scalar.Context.t -> Types.TypedC.typ -> binary -> binary Types.Type_check_tree.t
  (* This should have a better name. I think it is returning the
     bitvector representation of the pointer (containing the numerical
     information about the pointer), like alignment. We should
     probably have a separate domain to keep this information. *)
  val binary2scalar_binary : size:In_bits.t -> Scalar.Context.t -> binary -> Scalar.binary

  (** Returns the type of a function, or None if unknown.  *)
  val type_of : size:In_bits.t -> Scalar.Context.t -> binary -> Types.TypedC.typ option

  (** Check that all array indices operation are within bound. *)
  val within_bounds: size:In_bits.t -> Scalar.Context.t -> binary -> boolean

  module Query:sig
    module Binary_Lattice:Sig.Binary_Lattice
    val binary: size:In_bits.t -> Scalar.Context.t -> binary -> Binary_Lattice.t
  end
end


(** Representation of pointers inside some memory regions. *)
    module type ADDRESS = sig
    include AADT_WITH_BOOLEAN
    include WITH_ADDRESS with module Scalar := Scalar and type boolean := boolean
end

(*****************************************************************************)

module type WITH_MEMORY_QUERIES = sig
  module Context : Sig.Context
  type memory
  type address
  type offset

  (** [should_focus ~size ctx mem addr] asks the domain whether it is useful to
     "focus" (or "unfold", i.e. try to represent precisely the memory region
     pointed to by [addr], as long as aliasing info ensures that it is sound) a
     loaded value. [size] should be the size of [addr]. If the answer is yes,
     then the returned value should contain three things about the region to
     focus: its base, the offset of the end and the offset of [addr] in it (in
     bits). *)
  val should_focus : size:In_bits.t -> Context.t -> memory -> address ->
    (address * offset) option

  (** [may_alias ~ptr_size ~size1 ~size2 ctx addr1 addr2] should return whether
      the region starting at [addr1] of size [size1] bytes and the region
      starting at [addr2] of size [size2] bytes may have a non-empty
      intersection. This function is used by focusing abstractions to discard a
      focused region when writing in a possibly aliased address. [ptr_size] is
      the size in bits of both [addr1] and [addr2]. *)
  val may_alias : ptr_size:In_bits.t -> Context.t -> size1:offset -> size2:offset -> address -> address -> bool

  (** [is_weak ~size ctx addr] asks the domain whether the address [addr] points
      to a weak type which indicates that the address will not alias with any
      other *)
  (* TODO: No need for this, it should just be used to refine may_alias. *)
  val is_weak : size:In_bits.t -> Context.t -> address -> bool
end

(*****************************************************************************)


(** A fixed-size value is the type of values returned by C expressions
    or held in machine code registers, i.e. it is a bitvector
    containing an integer, pointer, floating point value, or
    contenation or extraction of other fixed-size values.

    As it can both be an address or a bitvector, it contains the union
    of both interfaces {!WITH_ADDRESS} and {!Sig.With_Binary}. *)
module type WITH_FIXED_SIZE_VALUE = sig
  include WITH_ADDRESS
  include Sig.With_Binary
    with type boolean := boolean
     and type binary := binary
     and module Context := Scalar.Context

  type enum
  include Sig.With_Enum
    with type boolean := boolean
     and type enum := enum
     and module Context := Scalar.Context

  val union: Operator.Condition.t -> Scalar.Context.t -> 'a Scalar.Context.in_tuple -> 'a Scalar.Context.out_tuple
  val global_symbol : Scalar.Context.t -> string -> In_bits.t * binary
  val add_global_symbol : size:In_bits.t -> Scalar.Context.t -> string -> binary -> unit

  (** Check for given typ being a function type and a list of pairs of (size * argument) that all the arguments are correct for the function
     and returns a boolean (true if all arguments are correct) and the appropriate return value with its size (size * ret) *)
  val analyze_summary : Scalar.Context.t -> Types.TypedC.typ -> (In_bits.t * binary) list -> bool * (In_bits.t * binary) option

  val addresses_in_binary : size:In_bits.t -> Scalar.Context.t -> binary -> Scalar.Context.t -> binary -> (binary * binary) list

  module Query:sig
    module Binary_Lattice:Sig.Binary_Lattice
    val binary: size:In_bits.t -> Scalar.Context.t -> binary -> Binary_Lattice.t

    module Enum_Lattice:Sig.Enum_Lattice
    val enum: Scalar.Context.t -> enum -> Enum_Lattice.t
  end
end


(** An abstract domain handling fixed-size values. *)
module type FIXED_SIZE_VALUE_DOMAIN = sig
  module Scalar:Sig.BASE
  include WITH_FIXED_SIZE_VALUE with module Scalar := Scalar
  include WITH_BOOLEAN_REDEFINITION with module Context = Scalar.Context and type boolean := boolean
end


(****************************************************************)
(** {1 Base module types describing operations on terms of block types} *)

module type BLOCK = sig
  include AADT_WITH_BOOLEAN
  module Offset:OFFSET with module Scalar = Scalar
  module Value:VALUE with module Scalar = Scalar
  type offset = Offset.offset

  type block
  val pretty: Context.t -> Format.formatter -> block -> unit

  include Operator.BLOCK_FORWARD
    with module Arity := Sig.Context_Arity_Forward(Context)
    and type boolean := boolean
    and type value := Value.binary
    and type block := block
    and type offset := offset

  val addresses_in_block : Context.t -> block -> Context.t -> block -> (Value.binary * Value.binary) list

  val serialize: widens:bool -> Context.t -> block -> Context.t -> block -> 'a Context.in_acc ->
    (block,'a) Context.result


  (** Create an initial region of size int (in bytes).
      Only meaning full for memories that are a single contiguous region *)
  val initial: Context.t -> In_bytes.t -> block

  val unknown: level:int -> Context.t -> block

  (** An empty region, with no bound offset. *)
  val block_empty: Context.t -> block

  (** Returns an unknown block value with a given type. *)
  val block_unknown_typed: Context.t -> Types.TypedC.typ -> block

  (** Type check a block value*)
  val check_type: Context.t -> Types.TypedC.typ -> block -> block Types.Type_check_tree.t
end


(** Memory map adresses to values. These types may differ (for
   instance, an array functor can takes integer as adresses, but can
   contain any value). *)
module type MEMORY = sig
  include AADT_WITH_BOOLEAN
  module Address:ADDRESS with module Scalar = Scalar
  module Block:BLOCK with module Scalar = Scalar
  type address = Address.binary
  open Scalar
  type memory
  val pretty: Context.t -> Format.formatter -> memory -> unit

  val load: size:In_bits.t -> Context.t -> memory -> address -> Block.Value.binary
  val store: size:In_bits.t -> Context.t -> memory -> address -> Block.Value.binary -> memory

  val load_block: Context.t -> memory -> address -> Block.block
  val store_block: Context.t -> memory -> address -> Block.block -> memory

  val serialize: widens:bool -> Context.t -> memory -> Context.t -> memory -> 'a Context.in_acc ->
    (memory,'a) Context.result

  (** Allocates a separated block of memory, identified by id.
      The values initially contained is empty. *)
  (* Not: if memory is a single region, this should return the address of the beginning in that region,
     i.e. the offset 0. *)
  val malloc: id:Operator.Malloc_id.t -> malloc_size:In_bytes.t ->
    Context.t -> memory -> address * memory

  (** Free a memory region. *)
  val free: Context.t -> memory -> address -> memory

  (** An initial, unknown region.  *)
  val unknown: level:int -> Context.t -> memory

  (** An empty region, with no bound address. *)
  val memory_empty: Context.t -> memory

  val addresses_in_memory : Context.t -> memory -> Context.t -> memory -> (Block.Value.binary * Block.Value.binary) list

  include WITH_MEMORY_QUERIES
    with module Context := Context
     and type memory := memory
     and type address := address
     and type offset := Block.Offset.offset
end


(** Those are the canonical signatures of memory domains; however many
   domains provide a more specialized signature (with additional
   Context and type equalities, and sometimes also ask for more
   specialized signature (but that wil improve) *)

(** The idea is to match a {!BLOCK} with the representation of an
    {!OFFSET} within that {!BLOCK}. But we delay the creation of the
    Block using a {{!OFFSET_AND_MAKE_BLOCK.Make_Block}[Make_Block]} functor, as the {!OFFSET} can be used to
    create the {!VALUE} that the {!BLOCK} contains. *)
module type OFFSET_AND_MAKE_BLOCK = sig
  module Scalar:Sig.BASE
  module Offset:OFFSET with module Scalar = Scalar
  module Make_Block(Value:VALUE with module Scalar = Scalar):BLOCK
    with module Scalar = Scalar
     and module Value = Value
     and module Offset = Offset
     and type boolean = Value.boolean (* Changing the boolean for memories is currently not useful,
                                              so we assume it does not change for now. *)
end

(** The idea is to match a {!MEMORY} with the representation of an
    {!ADDRESS} referencing cells in that {!MEMORY}. But we delay the
    creation of the {!MEMORY} using a {{!ADDRESS_AND_MAKE_MEMORY.Make_Memory}[Make_Memory]} functor, as the
    {!ADDRESS} can be used to create the {!VALUE} and {!BLOCK} that
    {!MEMORY} cells contain. *)
module type ADDRESS_AND_MAKE_MEMORY = sig
  module Scalar:Sig.BASE
  module Offset:OFFSET with module Scalar = Scalar
  module Address:ADDRESS with module Scalar = Scalar
  module Make_Memory(Block:BLOCK with module Scalar = Scalar and module Offset = Offset):MEMORY
    with module Scalar = Scalar
     and module Address := Address
     and module Block := Block
end

(** Like {!ADDRESS_AND_MAKE_MEMORY}, but we can do bitvector
    operations on addresses. This module is not fully instantiated as
    module {!MEMORY} still a functor waiting for parameter of module
    type {!BLOCK} *)
(* TODO: Rename to Value_and_Memory. Not sure if it is useful. *)
module type WHOLE_MEMORY_DOMAIN = sig
  module Scalar:Sig.BASE
  module Offset: OFFSET with module Scalar = Scalar
  module Address:FIXED_SIZE_VALUE_DOMAIN with module Scalar = Scalar
  module Make_Memory
      (Block:BLOCK with module Scalar = Scalar and module Offset = Offset)
    :MEMORY
      with module Scalar = Scalar
       and module Address := Address
       and module Block := Block (* Changing the boolean for memories is currently not useful,
                                              so we assume it does not change for now. *)
end

(** This is a fully instantiated version of {!WHOLE_MEMORY_DOMAIN}. It is used to break the recursion of
    the stack of recursive domains (containing uninitiated functors) in modules [Codex_register] and
    [Dba2Codex] *)
module type COMPLETE_DOMAIN = sig
  module Scalar: Sig.BASE
  module Offset : OFFSET with module Scalar = Scalar
  module Value: FIXED_SIZE_VALUE_DOMAIN with module Scalar = Scalar
  module Block: BLOCK with module Scalar = Scalar
                       and module Offset = Offset
                       and module Value = Value
  module Memory:MEMORY
    with module Scalar = Scalar
     and module Address := Value
     and module Block := Block
end



(****************************************************************)
(** {1 Base module types describing operations on one or several types of terms including memory ones} *)

(* TODO: This should probably be moved to transfer functions. *)
module type WITH_BLOCK_FORWARD = sig
  type boolean
  type value
  type block
  type offset
  module Context:Sig.Context
  module Block_Forward:Operator.BLOCK_FORWARD
    with module Arity := Sig.Context_Arity_Forward(Context)
    and type boolean := boolean
    and type value := value
    and type block := block
    and type offset := offset
end

(* TODO: this should probably be used in the Block AADT above. *)
module type WITH_BLOCK = sig
  module Context:Sig.Context
  type boolean
  type value
  type block
  type offset

  val block_empty: Context.t -> block
  val block_unknown: Context.t -> block
  val block_pretty: Context.t -> Format.formatter -> block -> unit
  val serialize_block: widens:bool -> Context.t -> block -> Context.t -> block -> 'a Context.in_acc -> (block,'a) Context.result
  val addresses_in_block: Context.t -> block -> Context.t -> block -> (value * value) list

  include WITH_BLOCK_FORWARD with module Context := Context
                              and type boolean := boolean
                              and type value := value
                              and type block := block
                              and type offset := offset

  val offset_pretty: Context.t -> Format.formatter -> offset -> unit

end

(* TODO: This should probably be moved to transfer functions. *)
module type WITH_MEMORY_FORWARD = sig
  type boolean
  type address
  type memory
  type block
  type offset
  type value
  module Context:Sig.Context
  module Memory_Forward:Operator.MEMORY_FORWARD
    with module Arity := Sig.Context_Arity_Forward(Context)
    and type boolean := boolean
    and type address := address
    and type memory := memory
    and type block := block
    and type value := value
end

(* TODO: This should probably be merged with WITH_OFFSET. *)
module type WITH_BLOCK_OFFSET = sig
  module Context:Sig.Context
  type boolean
  type offset

  val offset_pretty: Context.t -> Format.formatter -> offset -> unit

  (** Transfer functions *)

  (** Beginning of a region (offset 0) of size max (in bytes, if known and/or useful). *)
  val offset_zero: max:int option -> Context.t -> offset

  (** address := address + k *)
  val offset_shift: offset:int -> max:int option -> Context.t -> offset -> offset

  (** Offset comparison. *)
  val offset_le: Context.t -> offset -> offset -> boolean
  val offset_eq: Context.t -> offset -> offset -> boolean

end

module type WITH_MEMORY = sig
  module Context:Sig.Context
  type boolean
  type address
  type memory
  type offset
  type block
  type value

  val serialize_memory: widens:bool -> Context.t -> memory -> Context.t -> memory -> 'a Context.in_acc -> (memory,'a) Context.result
  val memory_pretty: Context.t -> Format.formatter -> memory -> unit

  (* val memory_empty: Context.t -> memory *)

  val addresses_in_memory : Context.t -> memory -> Context.t -> memory -> (address * address) list

  include WITH_MEMORY_FORWARD with module Context := Context
                               and type memory := memory
                               and type address := address
                               and type boolean := boolean
                               and type block := block
                               and type offset := offset
                               and type value := value
end


module type WITH_QUERIES = sig

  include Sig.WITH_QUERIES

  type memory
  type address
  type offset

  module Query:sig
    module Binary_Lattice:Sig.Binary_Lattice
    module Enum_Lattice:Sig.Enum_Lattice
    val enum: Context.t -> enum -> Enum_Lattice.t
    val binary: size:In_bits.t -> Context.t ->  binary -> Binary_Lattice.t

    (** Reachable means that the set of memory states is not empty.
     - If we return {{!Sig.Quadrivalent.Top}[Top] (i.e. [{True,False}])}, then the memory may be reachable;
     - If we return {{!Sig.Quadrivalent.True}[True]}, then the memory is reachable;
     - If we return {{!Sig.Quadrivalent.False}[False]} or {{!Sig.Quadrivalent.Bottom}[Bottom]}, the memory is not reachable. *)
    val reachable: Context.t -> memory -> Sig.Quadrivalent.t

  end

  include WITH_MEMORY_QUERIES
    with module Context := Context
     and type memory := memory
     and type address := address
     and type offset := offset
end


(* TODO: This may no longer be needed, if we organise all our domains
   as a base domain + some AADTs, that are used directly by the
   variable-base domains. *)
(* TODO: if we keep this, we may to change the name to avoid conflicts with Sig.BASE
*)
module type Base = sig
  include Sig.BASE

  type block
  type offset
  type memory

  include WITH_QUERIES with module Context := Context
                             and type binary := binary
                             and type memory := memory
                             and type offset := offset
                             and type address := binary
                             and type enum := enum

  (* TODO : remove this module inclusion *)
  include WITH_BLOCK_OFFSET with module Context := Context
                        and type offset := offset
                        and type boolean := boolean

  include WITH_BLOCK with module Context := Context
                        and type block := block
                        and type offset := offset
                        and type boolean := boolean
                        and type value := binary

  include WITH_MEMORY with module Context := Context
                        and type memory := memory
                        and type address := binary
                        and type boolean := boolean
                        and type block := block
                        and type offset := offset
                        and type value := binary

  (** Check if the memory is reachable. sat means reachable. *)
  val reachable: Context.t -> memory -> Smtbackend.Smtlib_sig.sat

  (** Retrieves the value of a global symbol *)
  val global_symbol : Context.t -> string -> In_bits.t * binary

  (** Updates the value of a global symbol *)
  val add_global_symbol : size:In_bits.t -> Context.t -> string -> binary -> unit

  (** Type check a binary value*)
  val check_type : size:In_bits.t -> Context.t -> Types.TypedC.typ -> binary -> binary Types.Type_check_tree.t

  (** Retrieves the type of value (this only works if it is a pointer and should
      only be used when looking at function pointers) *)
  val type_of : size:In_bits.t -> Context.t -> binary -> Types.TypedC.typ option

  (** Checks that a value has a given type *)
  val analyze_summary : Context.t -> Types.TypedC.typ -> (In_bits.t * binary) list -> bool * (In_bits.t * binary) option
end
