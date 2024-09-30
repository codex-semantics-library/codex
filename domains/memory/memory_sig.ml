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

(* Signature for the parts of memory domain.

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



(* Used to distinguish cases for weak updates, strong updates, or
   completely invalid destinations. *)
type 'a precision =
  | Empty
  | Singleton of 'a
  | Imprecise
;;

(* Raised e.g. when storing to an invalid location. *)
exception Memory_Empty;;

(** A scalar value stored inside a memory region. Should be a subset of Domain.Minimal + With_Binary. *)
module type Value = sig
  include Domain_sig.Minimal
  type binary                   (* Generally different from Scalar.binary! *)
  val binary_pretty: size:int -> Context.t -> Format.formatter -> binary -> unit
  val binary_empty: size:int -> Context.t -> binary

  (** Returns an unknown value with a given type. *)
  val binary_unknown_typed : size:int -> Context.t -> Types.Ctypes.typ -> binary

  (** Returns true if we can prove that x can be given type t. *)
  val has_type : size:int -> Context.t -> Types.Ctypes.typ -> binary -> bool

  (* Serialize functions also return their de-serializer. *)
  val serialize: size:int -> Context.t -> binary -> Context.t -> binary ->
    'a Context.in_acc -> (binary,'a) Context.result
  ;;

  (* Here we consider that all values are sets with a given cardinality, and most operations operate
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
  val union: Transfer_functions.Condition.t -> Context.t -> 'a Context.in_tuple -> 'a Context.out_tuple

  (** [contained_addresses ~size ctx value] should return whether
      the parts of the region value [value] of size [size] that contain addresses
      The returned value should be a list giving of pairs, with an address and an offset
      signaling where in the region, the pointer is *)
  val contained_addresses : size:int -> Context.t -> binary -> (int * binary) list


  module Binary_Forward:sig
    val bextract: size:int -> index:int -> oldsize:int ->
      Context.t -> binary -> binary
    val bconcat: size1:int -> size2:int -> Context.t -> binary -> binary -> binary
    val bchoose: size:int -> Transfer_functions.Choice.t -> Context.t -> binary -> binary
  end

end

(** Abstraction of an offset in a contiguous memory region.  *)
module type With_Offset = sig
  module Context:Domain_sig.Context
  module Scalar:Domain_sig.Base with module Context = Context (* For now. *)
  type boolean
  type offset
  module Offset:sig
    val pretty: Format.formatter -> offset -> unit
    val equal: offset -> offset -> bool
  end

  (* Should be offset_pretty, offset_empty, offset_serialize, etc.  *)
  val offset_pretty: size:int -> Context.t -> Format.formatter -> offset -> unit
  val offset_empty: size:int -> Context.t -> offset
  val serialize_offset: size:int -> Context.t -> offset -> Context.t -> offset ->
    'a Context.in_acc -> (offset,'a) Context.result
  ;;

  (** Transfer functions **)
  (* Beginning of a region (offset 0) of size max (in bytes, if known and/or useful). *)
  val offset_zero: size:int -> max:int option -> Context.t -> offset

  (* address := address + k *)
  val offset_shift: size:int -> offset:int -> max:int option -> Context.t -> offset -> offset

  (* address := address + k * expr *)
  val offset_index: size:int -> int -> Context.t -> offset -> Scalar.binary -> offset

  (* offset substraction. *)
  val offset_sub: size:int -> Context.t -> offset -> offset -> Scalar.binary

  (* Offset comparison. *)
  val offset_le: size:int -> Context.t -> offset -> offset -> boolean
  val offset_eq: size:int -> Context.t -> offset -> offset -> boolean

  (* Check that all array indices operation are within bound. *)
  val offset_within_bounds: size:int -> Context.t -> offset -> boolean

  val offset_choose: size:int -> Transfer_functions.Choice.t -> Context.t -> offset -> offset


  (* Lift/unlift a boolan to a scalar boolan. Probably this should not
     be part of the domain signature. *)
  val boolean2scalar_bool : Context.t -> boolean -> Scalar.boolean
  val scalar_bool2boolean : Context.t -> Scalar.boolean -> boolean

end

(** A true domain dealing with offsets.

    For now we assume that they do not have internal state (i.e. their
    context, in_tup, out_tuple is the same of the Scalar on which they
    build), but we will change that in.

    TODO: We should drop the size argument, it is always ptr_size. *)
module type Offset = sig
  include Domain_sig.Minimal
  include With_Offset with module Context := Context
                       and type boolean := boolean
end


(** Abstraction of an address, pointing to a region in a C-like memory model.
    The type adress is named binary for historical reasons, and also to
    help sharing with bitvectors in whole. *)
module type With_Address = sig
  module Context:Domain_sig.Context
  module Scalar:Domain_sig.Base with module Context = Context (* At least for now. *)
  type boolean
  type binary

  module Binary:Datatype_sig.S with type t = binary

  val binary_pretty: size:int -> Context.t -> Format.formatter -> binary -> unit
  val binary_empty: size:int -> Context.t -> binary

  val satisfiable: Context.t -> boolean -> Smtbackend.Smtlib_sig.sat

  (* Serialize functions also return their de-serializer. *)
  val serialize: size:int ->
    Context.t -> binary -> Context.t-> binary ->
    'a Context.in_acc -> (binary,'a) Context.result

  (* TODO: rename these transfer_functions to address_index, address_sub, etc. *)

  (* Note: we do not require a "top" representation for addresses. *)
  val bchoose: size:int -> Transfer_functions.Choice.t -> Context.t -> binary -> binary

  (* address := address + k * expr *)
  val bindex: size:int -> int -> Context.t -> binary -> Scalar.binary -> binary

  (* pointer substraction. *)
  val bisub: size:int -> Context.t -> binary -> binary -> Scalar.binary

  (* address := address + k *)
  val bshift: size:int -> offset:int -> max:int option -> Context.t -> binary -> binary

  (* Pointer comparison. *)
  val ble: size:int -> Context.t -> binary -> binary -> boolean
  val beq: size:int -> Context.t -> binary -> binary -> boolean

  val binary_unknown : size:int -> Context.t -> binary
  val binary_unknown_typed: size:int -> Context.t -> Types.Ctypes.typ -> binary
  val has_type : size:int -> Context.t -> Types.Ctypes.typ -> binary -> bool

  (* This should have a better name. I think it is returning the
     bitvector representation of the pointer (containing the numerical
     information about the pointer), like alignment. We should
     probably have a separate domain to keep this information. *)
  val binary2scalar_binary : size:int -> Context.t -> binary -> Scalar.binary
  val assume_type : size:int -> Context.t -> binary -> Types.Ctypes.typ -> unit

  (** Returns the type of a function, or None if unknown.  *)
  val type_of : size:int -> Context.t -> binary -> Types.Ctypes.typ option


  (* Check that all array indices operation are within bound. *)
  val within_bounds: size:int -> Context.t -> binary -> boolean

  module Query:sig
    module Binary_Lattice:Domain_sig.Binary_Lattice
    val binary: size:int -> Context.t -> binary -> Binary_Lattice.t
    include Single_value_abstraction.Sig.Binary_Conversions with type binary := Binary_Lattice.t
  end
end


(* Representation of pointers inside some memory regions. *)
module type Address(* _domain *) = sig
  include Domain_sig.Minimal
  include With_Address with module Context := Context
                        and type boolean := boolean
end


(*****************************************************************************)

module type With_Block_Forward = sig
  type boolean
  type value
  type block
  type offset
  module Context:Domain_sig.Context
  module Block_Forward:Transfer_functions.Block_Forward
    with module Arity := Domain_sig.Context_Arity_Forward(Context)
    and type boolean := boolean
    and type value := value
    and type block := block
    and type offset := offset
end

module type With_Block = sig
  module Context:Domain_sig.Context
  type boolean
  type value
  type block
  type offset

  val shared_addresses : Context.t -> block -> Context.t -> block -> (value * value) list

  include With_Block_Forward with module Context := Context
                              and type boolean := boolean
                              and type value := value
                              and type block := block
                              and type offset := offset
end


module type With_Memory_Forward = sig
  type boolean
  type address
  type memory
  type block
  type value
  module Context:Domain_sig.Context
  module Memory_Forward:Transfer_functions.Memory_Forward
    with module Arity := Domain_sig.Context_Arity_Forward(Context)
    and type boolean := boolean
    and type address := address
    and type memory := memory
    and type block := block
    and type value := value
end


module type Memory_Lattice = Single_value_abstraction.Sig.Memory_Lattice


module type With_Memory_Queries = sig
  module Context : Domain_sig.Context
  type memory
  type address
  type offset

  (** [should_focus ~size ctx mem addr] asks the domain whether it is useful to
     "focus" (or "unfold", i.e. try to represent precisely the memory region
     pointed to by [addr], as long as aliasing info ensures that it is sound) a
     loaded value. [size] should be the size of [addr]. If the answer is yes,
     then the returned value should contain three things about the region to
     focus: its base, its size (in bits) and the offset of [addr] in it (in
     bits). *)
  val should_focus : size:int -> Context.t -> memory -> address ->
    (address * int * int) option

  (** [may_alias ~ptr_size ~size1 ~size2 ctx addr1 addr2] should return whether
      the region starting at [addr1] of size [size1] bytes and the region
      starting at [addr2] of size [size2] bytes may have a non-empty
      intersection. This function is used by focusing abstractions to discard a
      focused region when writing in a possibly aliased address. [ptr_size] is
      the size in bits of both [addr1] and [addr2]. *)
  val may_alias : ptr_size:int -> Context.t -> size1:int -> size2:int -> address -> address -> bool

  (** [is_weak ~size ctx addr] asks the domain whether the address [addr] points
      to a weak type which indicates that the address will not alias with any
      other *)
  val is_weak : size:int -> Context.t -> address -> bool
end


module type With_Memory = sig
  module Context:Domain_sig.Context
  type boolean
  type address
  type memory
  type block
  type value

  val serialize_memory: Context.t -> memory -> Context.t -> memory -> 'a Context.in_acc -> (memory,'a) Context.result
  val memory_pretty: Context.t -> Format.formatter -> memory -> unit

  (* val memory_empty: Context.t -> memory *)

  val shared_addresses : Context.t -> memory -> Context.t -> memory -> (address * address) list

  include With_Memory_Forward with module Context := Context
                               and type memory := memory
                               and type address := address
                               and type boolean := boolean
                               and type block := block
                               and type value := value
end

(*****************************************************************************)


(** A fixed-size value is the type of values returned by C expressions
    or held in machine code registers, i.e. it is a bitvector
    containing an integer, pointer, floating point value, or
    contenation or extraction of other fixed-size values.

    As it can both be an address or a bitvector, it contains the union
    of both interfaces. *)
module type With_Fixed_Size_Value = sig
  include With_Address
  include Domain_sig.With_Binary
    with type boolean := boolean
     and type binary := binary
     and module Context := Context
  val union: Transfer_functions.Condition.t -> Context.t -> 'a Context.in_tuple -> 'a Context.out_tuple
  val global_symbol : Context.t -> string -> int * binary
  val add_global_symbol : size:int -> Context.t -> string -> binary -> unit

  (* Check for given typ being a function type and a list of pairs of (size * argument) that all the arguments are correct for the function
     and returns a boolean (true if all arguments are correct) and the appropriate return value with its size (size * ret) *)
  val analyze_summary : Context.t -> Types.Ctypes.typ -> (int * binary) list -> bool * (int * binary) option

  val contained_addresses : size:int -> Context.t -> binary -> (int * binary) list
end


(** An abstract domain handling fixed-size values. *)
module type Fixed_size_value_domain = sig
  include Domain_sig.Minimal
  include With_Fixed_Size_Value with module Context := Context
                                 and type boolean := boolean
end

(* Previous name, used for compatiblity reasons. *)
(* module type Operable_Value_Whole = Fixed_size_value_domain *)


(****************************************************************)
(* Base module types describing operations on terms of block types. *)

module type Block = sig
  module Offset:Offset
  module Value:Value
  include Domain_sig.Minimal with module Context = Value.Context (* For now. *)
  type offset = Offset.offset


  type block
  val pretty: Context.t -> Format.formatter -> block -> unit

  include Transfer_functions.Block_Forward
    with module Arity := Domain_sig.Context_Arity_Forward(Context)
    and type boolean := boolean
    and type value := Value.binary
    and type block := block
    and type offset := offset

  val shared_addresses : Context.t -> block -> Context.t -> block -> (Value.binary * Value.binary) list

  val serialize: Context.t -> block -> Context.t -> block -> 'a Context.in_acc ->
    (block,'a) Context.result
  

  (** Create an initial region of size int (in bytes). 
      Only meaning full for memories that are a single contiguous region *)
  val initial: Context.t -> int -> block

  val unknown: level:int -> Context.t -> block

  (** An empty region, with no bound offset. *)
  val block_empty: Context.t -> block

end


(** Memory map adresses to values. These types may differ (for
   instance, an array functor can takes integer as adresses, but can
   contain any value). *)
module type Memory = sig
  module Address:Address    
  module Block:Block
  include Domain_sig.Minimal with module Context = Block.Context (* For now. *)
  type address = Address.binary

  type memory
  val pretty: Context.t -> Format.formatter -> memory -> unit

  val load: size:int -> Context.t -> memory -> address -> Block.Value.binary
  val typed_load: size:int -> Context.t -> memory -> address -> Types.Ctypes.typ -> Block.Value.binary
  val store: size:int -> Context.t -> memory -> address -> Block.Value.binary -> memory
  val typed_store: size:int -> Context.t -> memory -> address -> Types.Ctypes.typ -> Block.Value.binary -> memory

  val load_block: size:int -> Context.t -> memory -> address -> Block.block
  val store_block: size:int -> Context.t -> memory -> address -> Block.Value.binary -> Block.block

  val serialize: Context.t -> memory -> Context.t -> memory -> 'a Context.in_acc ->
    (memory,'a) Context.result

  (** Allocates a separated block of memory, identified by id. 
      The values initially contained is empty. *)
  (* Not: if memory is a single region, this should return the address of the beginning in that region,
     i.e. the offset 0. *)
  val malloc: id:Transfer_functions.Malloc_id.t -> malloc_size:int ->
    Context.t -> memory -> address * memory

  (** Free a memory region. *)
  val free: Context.t -> memory -> address -> memory

  (** An initial, unknown region.  *)
  val unknown: level:int -> Context.t -> memory

  (** An empty region, with no bound address. *)
  val memory_empty: Context.t -> memory

  val shared_addresses : Context.t -> memory -> Context.t -> memory -> (Block.Value.binary * Block.Value.binary) list

  include With_Memory_Queries
    with module Context := Context
     and type memory := memory
     and type address := address
     and type offset := Block.Offset.offset
end

module type Value_to_offset = sig
  module Value:Value
  module Offset:Offset

  val ctx: Value.Context.t -> Offset.Context.t * (Offset.Context.t -> Value.Context.t)
end

(** Value is above Address on the domain stack, so we
    decompose a Value stack in an Offset stack and a context:
    [Lift.ctx value_ctx] = (adderess_ctx,f) means that f
    offset_ctx = value_ctx. *)
module type Value_to_address = sig
  module Value:Value
  module Address:Address

  val ctx: Value.Context.t -> Address.Context.t * (Address.Context.t -> Value.Context.t)
end

(** Address is above Scalar on the domain stack, so we
    decompose a Address stack in a Scalar stack and a context:
    [Lift.ctx address_ctx = (scalar_ctx,f)] means that [f
    scalar_ctx = address_ctx_ctx]. *)
module type Address_to_Scalar = sig
  module Address:Address
  module Scalar:Domain_sig.Base

  val ctx: Address.Context.t -> Scalar.Context.t * (Scalar.Context.t -> Scalar.Context.t)
end

(** Offset is above Scalar on the domain stack, so we
    decompose a Offset stack in a Scalar stack and a context:
    [Lift.ctx address_ctx = (scalar_ctx,f)] means that [f
    scalar_ctx = address_ctx_ctx]. *)
module type Offset_to_Scalar = sig
  module Offset:Offset
  module Scalar:Domain_sig.Base

  val ctx: Offset.Context.t -> Scalar.Context.t * (Scalar.Context.t -> Scalar.Context.t)

  (** Allows serialization of a Scalar.binary in a Offset domain. *)
  val serialize_binary: size:int -> Offset.Context.t -> Scalar.binary -> Offset.Context.t -> Scalar.binary -> 'a Offset.Context.in_acc -> (Scalar.binary,'a) Offset.Context.result


  (* val union: Transfer_functions.Condition.t -> Context.t -> 'a Context.in_tuple -> 'a Context.out_tuple        *)
end




(** Those are the canonical signatures of memory domains; however many
   domains provide a more specialized signature (with additional
   Context and type equalities, and sometimes also ask for more
   specialized signature (but that wil improve) *)


(* Should be named region: A memory whose addresses are offsets.
   Because the functions are different from regular domains, the
   transfer functions should have a different name. *)
module type Offset_Memory_domain = sig
  module Offset:Offset
  module Memory
      (Value:Value)
      (_:Value_to_offset with module Value := Value and module Offset := Offset):Block
    with module Value = Value
     and module Offset = Offset
     and type boolean = Value.boolean (* Changing the boolean for memories is currently not useful,
                                              so we assume it does not change for now. *)
end


(** A memory domain is an Address and a Memory, built on top of a Scalar domain.
   The memory is parameterized by the Value put in that memory. *)
module type Memory_domain = sig
  module Address:Address
  module Memory
      (Block:Block)
      (Lift:Value_to_address with module Value := Block.Value and module Address := Address)
    :Memory
    with module Address = Address
     and module Block = Block
     and type boolean = Block.Value.boolean (* Changing the boolean for memories is currently not useful,
                                              so we assume it does not change for now. *)
end

(** Like a [Memory_domain], but we can do bitvector operations on addresses.  *)
module type Whole_Memory_domain = sig
  module Address:Fixed_size_value_domain
  module Memory
      (Block:Block)
      (Lift:Value_to_address with module Value := Block.Value and module Address := Address)
    :Memory
    with module Address = Address
     and module Block = Block
     and type boolean = Block.Value.boolean (* Changing the boolean for memories is currently not useful,
                                              so we assume it does not change for now. *)
end

(** A domain where the Memory functor has been instantiated. *)
module type Complete_domain = sig
  module Address : Fixed_size_value_domain
  module Block : Block with module Value = Address
  module Memory : Memory
    with module Address.Context = Address.Context
     and module Block.Value.Context = Address.Context
     and type Block.Value.binary = Address.binary
     and type Address.binary = Address.binary
     and type boolean = Address.boolean
     and type address = Address.binary
end



(****************************************************************)
(* Base module types describing operations on one or several types of terms including memory ones. *)

module type With_Queries = sig

  include Domain_sig.With_Queries

  type memory
  type address
  type offset

  module Query:sig
    module Binary_Lattice:Domain_sig.Binary_Lattice

    include Single_value_abstraction.Sig.Binary_Conversions with type binary := Binary_Lattice.t
    (* TODO: This supersedes "truth_value" *)

    val binary: size:int -> Context.t ->  binary -> Binary_Lattice.t

    (* Reachable means that the set of memory states is not empty.
     - If we return {True,False}, then the memory may be reachable;
     - If we return {True}, then the memory is reachable;
     - If we return {False} or {}, the memory is not reachable. *)
    val reachable: Context.t -> memory -> Domain_sig.Quadrivalent.t
  end

  include With_Memory_Queries
    with module Context := Context
     and type memory := memory
     and type address := address
     and type offset := offset
end

module type Base = sig
  include Domain_sig.Base

  type block
  type memory

  include With_Queries with module Context := Context
                             and type binary := binary
                             and type memory := memory
                             and type offset := binary
                             and type address := binary

  include With_Block with module Context := Context
                        and type block := block
                        and type offset := binary
                        and type boolean := boolean
                        and type value := binary

  include With_Memory with module Context := Context
                        and type memory := memory
                        and type address := binary
                        and type boolean := boolean
                        and type block := block
                        and type value := binary

  (* Builtin functions. *)
  include Transfer_functions.Builtin.With_Builtin with type binary := binary
                                                   and type boolean := boolean
                                                   and type memory := memory
                                                   and module Context := Context

  val block_empty: Context.t -> block
  val block_unknown: size:binary -> Context.t -> block
  val block_pretty: Context.t -> Format.formatter -> block -> unit
  val serialize_block: Context.t -> block -> Context.t -> block -> 'a Context.in_acc -> (block,'a) Context.result
  
  val reachable: Context.t -> memory -> Smtbackend.Smtlib_sig.sat
  (** Check if the memory is reachable. sat means reachable. *)

  val global_symbol : Context.t -> string -> int * binary
 (** Retrieves the value of a global symbol *)

  val add_global_symbol : size:int -> Context.t -> string -> binary -> unit
  (** Updates the value of a global symbol *)

  val has_type : size:int -> Context.t -> Types.Ctypes.typ -> binary -> bool
  (** Checks that a value has a given type *)

  val type_of : size:int -> Context.t -> binary -> Types.Ctypes.typ option
  (** Retrieves the type of value (this only works if it is a pointer and should
      only be used when looking at function pointers) *)

  val analyze_summary : Context.t -> Types.Ctypes.typ -> (int * binary) list -> bool * (int * binary) option
  (** Checks that a value has a given type *)

  val contained_addresses : size:int -> Context.t -> binary -> (int * binary) list

end


(****************************************************************)
(* Context conversion procedures: pass through the values by just
   changing the context. *)

module Convert_Memory_Forward
  (C:Domain_sig.Convert_Contexts)
  (D:With_Memory_Forward with module Context = C.To) =
struct
  module C = Domain_sig.Make_Convert(C)
  module F = struct include D;; include D.Memory_Forward end
  include Transfer_functions.Conversions.Convert_Memory_Forward(C)(F)
end


(* This will help to the transition in a top-down manner, starting
   from the translation and top-level domain to the lower-level
   domain.

   The idea is to support both interfaces, and use conversion to
   simplify the support for both. I can have a signature for both
   domains, and an "AddMonadic" functor to support both domains. *)

module Convert_to_monadic(D:Base) = struct

  include Domain_sig.Convert_to_monadic(D:Base)

  module Types = struct
    include Types
    type memory = D.memory
    type block = D.block
    type offset = D.binary
    type value = D.binary
    type address = D.binary
  end

  module Block_Forward = Transfer_functions.Conversions.Convert_Block_Forward(Conversion)(struct include Types include D.Block_Forward end)
  module Memory_Forward = Transfer_functions.Conversions.Convert_Memory_Forward(Conversion)(struct include Types include D.Memory_Forward end)
end
