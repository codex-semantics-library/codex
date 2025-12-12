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

(** This defines the syntax for the operators usable in the
    internal languages of Codex, expressed as signatures as in the
    {{: http://okmij.org/ftp/tagless-final/}Tagless final} approach.

     The signatures are grouped by type of values manipulated
     (boolean, integer, bitvector, binary, memory, enum). We define
     two set of functions: the forward are the normal operations, and
     the backward exclude the functions of arity 0 (for which a
     backward operation is meaningless). *)
open Operator_ids
open Units

type access_type =
| Read  (** Loading from memory. *)
| Write  (** Storing into memory. *)

type arith_type =
| Plus  (** Addition operation *)
| Minus  (** Substraction operation *)

(** Arity of function symbols. ['r] represents the result type and ['a], ['b], ['c] the arguments. *)
module type ARITY = sig
  type ('r) ar0
  type ('a,'r) ar1
  type ('a,'b,'r) ar2
  type ('a,'b,'c,'r) ar3
end

(** Standard arities for forward transfer functions: given the arguments, return the results.
    These match the arities of the concrete functions they represent (but with concrete types
    substituted for their abstract counterparts). *)
module Forward_Arity = struct
  type 'r ar0 = 'r
  type ('a,'r) ar1 = 'a -> 'r
  type ('a,'b,'r) ar2 = 'a -> 'b -> 'r
  type ('a,'b,'c,'r) ar3 = 'a -> 'b -> 'c -> 'r
end

(** Standard arities for backward transfer functions (used to refined the arguments from information
    on the result values). These take the result value ['r] as argument and return a new-improved
    value for each argument. They return [None] when no improvement is possible for that argument.

    We generally don't include backward function for symbols of arity 0. *)
module Backward_Arity = struct
  type 'r ar0 = 'r -> unit
  type ('a,'r) ar1 = 'a -> 'r -> ('a option)
  type ('a,'b,'r) ar2 = 'a -> 'b -> 'r -> ('a option * 'b option)
  type ('a,'b,'c,'r) ar3 = 'a -> 'b -> 'c -> 'r -> ('a option * 'b option * 'c option)
end


(** Note: in the following, we distinguish between backward and forward
   because there is no need to implement backward transfer functions
   for symbols with arity 0. *)


(****************************************************************)
(** {1 Boolean transfer functions}

    Transfer functions for boolean values:
    {{!Boolean_Backward.not}[not]},
    and ({{!Boolean_Backward.(&&)}[&&]}),
    or ({{!Boolean_Backward.(||)}[||]}),
    as well as contants {{!Boolean_Forward.true_}[true_]} and
    {{!Boolean_Forward.false_}[false_]}. *)

module type BOOLEAN_BACKWARD = sig
  type boolean
  module Arity:ARITY
  val not: (boolean,boolean) Arity.ar1
  val (&&): (boolean,boolean,boolean) Arity.ar2
  val (||): (boolean,boolean,boolean) Arity.ar2
end

module type BOOLEAN_FORWARD = sig
  include BOOLEAN_BACKWARD
  val true_: boolean Arity.ar0
  val false_: boolean Arity.ar0
end

(****************************************************************)
(** {1 Integer transfer functions}

    Transfer functions for unbounded integers:
    - addition ({{!Integer_Backward.iadd}[iadd]}); subtraction ({{!Integer_Backward.isub}[isub]});
    - multiplication ({{!Integer_Backward.imul}[imul]}, in general, {{!Integer_Backward.itimes}[itimes]}
      when multiplying by a constant);
    - division ({{!Integer_Backward.idiv}[idiv]}), remainder ({{!Integer_Backward.iadd}[imod]});
    - comparisons ({{!Integer_Backward.ieq}[ieq]} for [==], {{!Integer_Backward.ile}[ile]} for [<=]);
    - shifts (left {{!Integer_Backward.ishl}[ishl]} and right {{!Integer_Backward.ishr}[ishr]})
    - bitwise operations ({{!Integer_Backward.ior}[ior]}, {{!Integer_Backward.iand}[iand]}, {{!Integer_Backward.ixor  }[ixor]}).

    For the bitwise operation, we
    assume an infinite two-complement representation: i.e. [-1] is
    represented by an infinite sequence of [1], and [0] by an infinite
    sequence of [0]. *)


module type INTEGER_BACKWARD = sig
  type integer
  type boolean

  module Arity:ARITY

  (* Minimum versions. *)
  val itimes: Z.t -> (integer,integer) Arity.ar1
  (** Multiply an integer by a constant *)

  val iadd: (integer,integer,integer) Arity.ar2
  val imul: (integer,integer,integer) Arity.ar2
  val idiv: (integer,integer,integer) Arity.ar2
  (** This is truncated (C99-like) integer division *)
  (* Maybe: rename to itdiv/itmod? *)

  val imod: (integer,integer,integer) Arity.ar2
  val ishl: (integer,integer,integer) Arity.ar2
  val ishr: (integer,integer,integer) Arity.ar2
  val iand: (integer,integer,integer) Arity.ar2
  (** Bitwise and, where negative integers are seen as prefixed by infinite ones *)

  val ior: (integer,integer,integer) Arity.ar2
  (** Bitwise or, where negative integers are seen as prefixed by infinite ones *)

  val ixor: (integer,integer,integer) Arity.ar2
  val isub: (integer,integer,integer) Arity.ar2
  val ieq: (integer,integer,boolean) Arity.ar2
  val ile: (integer,integer,boolean) Arity.ar2
end

module type INTEGER_FORWARD_MIN = sig
  include INTEGER_BACKWARD
  val iconst: Z.t -> integer Arity.ar0
  (** Integer constant *)
end

module type INTEGER_FORWARD = sig
  include INTEGER_FORWARD_MIN

  (** These can be defined from the others, but it may be more
     efficient to implement them directly (no need to build temporary
     values...). They are also often convenient to use directly. *)
  val zero: integer Arity.ar0
  val one: integer Arity.ar0
end

(****************************************************************)
(** {1:bitvector Bitvector transfer functions}

    Purely numerical operations on fixed-size bitvectors. Includes
    bitwise operations and arithmetic, but not pointer arithmetic.

    Note: the [size] argument is generally the size of both arguments
    and the result. *)

module type BITVECTOR_BACKWARD = sig
  type bitvector
  type boolean
  module Arity: ARITY
  (* nsw means no signed overflow, nuw means no unwrapped overflow. No
     (signed) overflow means that when taking the signed integer value
     of the variable, then performing the operation remains in the
     signed range (and respectively for unsigned overflow). Transfer
     functions can take advantage of this fact to improve their
     precision. *)

  (** Bitvector Integer ADDition.
      Operaters on bitvectors of size [size]. The flags represent behavior on overflow:

      - [nuw]: no unsigned wrap: the operation is partial, i.e. fails
      if the sum of the two operands (interpreted as unsigned numbers)
      is not in the {m 0} to {m 2^{size}-1} interval.

      - [nsw]: no signed wrap: the operation is partial, i.e. fails if
      the sum of the two operands (interpreted as signed numbers) is
      not in the {m -2^{size-1}} to{m 2^{size-1} - 1} interval.

      - [nusw]: no unsigned plus signed wrap: the addition of the
      first operand (interpreted as an unsigned number) and the second
      one (interpreted as a signed number) fails if its value is not
      in the {m 0} to {m 2^{size}-1} interval. This is useful when
      doing pointer arithmetic (address (unsigned) + offset (signed))

      Note that the simultaneous combination of different flags may be
      unimplemented (as it never happens in practice). *)
  val biadd: size:In_bits.t -> flags:Flags.Biadd.t -> (bitvector,bitvector,bitvector) Arity.ar2

  (** Bitvector Integer SUBtraction.
      See {!biadd} for the flag meanings *)
  val bisub: size:In_bits.t -> flags:Flags.Bisub.t -> (bitvector,bitvector,bitvector) Arity.ar2

  (** Bitvector Integer MULtiplication.
      See {!biadd} for the flag meanings *)
  val bimul: size:In_bits.t -> flags:Flags.Bimul.t -> (bitvector,bitvector,bitvector) Arity.ar2

  (** Bitvector SHift Left,
      If second argument is larger than size, bshl returns 0. *)
  val bshl: size:In_bits.t  -> flags:Flags.Bshl.t -> (bitvector,bitvector,bitvector) Arity.ar2

  (** Arithmetic shift right: fill with the most significant bit. *)
  val bashr: size:In_bits.t -> (bitvector,bitvector,bitvector) Arity.ar2

  (** Logical shift right: fill with 0. *)
  val blshr: size:In_bits.t -> (bitvector,bitvector,bitvector) Arity.ar2

  (** Bitvector equality *)
  val beq:   size:In_bits.t -> (bitvector,bitvector,boolean) Arity.ar2

  (** Signed comparison between two bitvectors (using less-or-equal [<=]).
      Bitvectors are interpreted as integers in {m [-2^{size-1}..-2^{size-1}-1]} using two's complement. *)
  val bisle: size:In_bits.t -> (bitvector,bitvector,boolean) Arity.ar2

  (** Unsigned comparison between two bitvectors (using less-or-equal [<=]).
      Bitvectors are interpreted as positive integers in {m [0..-2^{size}-1]}. *)
  val biule: size:In_bits.t -> (bitvector,bitvector,boolean) Arity.ar2

  (** Bitvector concatenation: the new bitvector's size is the sum of the sizes of the arguments.
      The first argument becomes the most significant bits. *)
  val bconcat: size1:In_bits.t -> size2:In_bits.t -> (bitvector,bitvector,bitvector) Arity.ar2

  (** Extract a [size] of a bitvector of total size [oldsize], starting at [index].
      Should satisfy [index + size <= oldsize]. *)
  val bextract: size:In_bits.t -> index:In_bits.t -> oldsize:In_bits.t -> (bitvector,bitvector) Arity.ar1

  (** Bitvector bitwise and *)
  val band: size:In_bits.t -> (bitvector,bitvector,bitvector) Arity.ar2

  (** Bitvector bitwise or *)
  val bor: size:In_bits.t -> (bitvector,bitvector,bitvector) Arity.ar2

  (** Bitvector bitwise xor *)
  val bxor: size:In_bits.t -> (bitvector,bitvector,bitvector) Arity.ar2

  (** Unsingned-extend (pad left with zero) the argument bitvector until it reaches the specified [size]. *)
  val buext: size:In_bits.t -> oldsize:In_bits.t -> (bitvector,bitvector) Arity.ar1

  (** Sign-extend (pad left with topbit) the argument bitvector until it reaches the specified [size]. *)
  val bsext: size:In_bits.t -> oldsize:In_bits.t -> (bitvector,bitvector) Arity.ar1

  (** Bitvector signed division (truncate towards 0) *)
  val bisdiv: size:In_bits.t -> (bitvector,bitvector,bitvector) Arity.ar2

  (** Bitvector signed modulo: should satisfy
      [a = b*(bisdiv a b) + bismod a b], just like C-modulo.
      This means it can be negative for negative divisions. *)
  val bismod: size:In_bits.t -> (bitvector,bitvector,bitvector) Arity.ar2

  (** Bitvector unsigned division (corresponds to euclidian division) *)
  val biudiv: size:In_bits.t -> (bitvector,bitvector,bitvector) Arity.ar2

  (** Bitvector unsigned modulo (corresponds to euclidian remainder) *)
  val biumod: size:In_bits.t -> (bitvector,bitvector,bitvector) Arity.ar2

    (** Turn a boolean into a bitvector of the given [size]:
    [false] is [0] and [true] is [1]. *)
  val bofbool: size:In_bits.t -> (boolean,bitvector) Arity.ar1

end

module type BITVECTOR_FORWARD = sig
  include BITVECTOR_BACKWARD
  val biconst: size:In_bits.t -> Z.t -> bitvector Arity.ar0
  (** Bitvector constant with the given size and value. *)
end

module type BITVECTOR_FORWARD_WITH_BIMUL_ADD = sig
  include BITVECTOR_FORWARD
  
  (** Combined operation for multiplication and addition [x -> prod*x + offset].
      This operation does not overflow if only the intermediate term [prod*x] overflows. *)
  val bimul_add: size:In_bits.t -> prod:Z.t -> offset:Z.t -> (bitvector,bitvector) Arity.ar1
end

(****************************************************************)
(** {1 Binary transfer functions}

    Binary is the name of values handled by C or machine-level
    programs, i.e. either numeric {{!bitvector}bitvectors} or
    pointers. *)

module type BINARY_BACKWARD = sig

  type binary
  include BITVECTOR_BACKWARD with type bitvector := binary

  (* TODO: We should drop the size argument here. Those functions
     apply only to addresses, and we know the size of addresses.  *)
  (* TODO: Should be a predicate on memory, and take both a binary and memory. *)
  (** Returns true if the access to the pointer in memory is valid. *)
  val valid: size:In_bits.t -> access_type -> (binary,boolean) Arity.ar1

  (** [valid_ptr_arith(ptr,off)] where [ptr] is a pointer (unsigned)
      and [off] a signed integer offset returns [true] if [ptr + off]
      (or [ptr - off], depending on the [arith_type]) is in-bound. *)
  (* MAYBE: Could be replaced by a new \valid access types, and overflow assertions on addition. *)
  val valid_ptr_arith: size:In_bits.t -> arith_type -> (binary,binary,boolean) Arity.ar2

  (* [max], if not None, limits further pointer arithmetics: one cannot go beyond max. *)
  (* MAYBE: separate this between bshift and a new operator [bnarrow]. *)

  (** [bshift ptr ~offset] returns [ptr + offset].
      If [max] is not None, limits further pointer arithmetics: one cannot go beyond max.
      Note: offset and max are in bytes, not in bits. *)
  val bshift: size:In_bits.t -> offset:int -> max:int option -> (binary,binary) Arity.ar1

  (** [bindex] takes an integer [k] and two binary values [ptr] and [off].
      It returns [ptr + k*off]. *)
  val bindex: size:In_bits.t -> int -> (binary,binary,binary) Arity.ar2 (* b + k * exp. *)

  (** If s is a set, and c is a "choice" (i.e. valuation of conditions),
     choose an element of set according to choice. *)
  (*   MAYBE: A more proper solution could be to have types for
     collections, i.e. arrays and sets, but this would require
     duplicating the work on the single elements to collections.  *)
  val bchoose: size:In_bits.t -> Choice.t -> (binary,binary) Arity.ar1
end

module type BINARY_FORWARD = sig
  include BINARY_BACKWARD
  val biconst: size:In_bits.t -> Z.t -> binary Arity.ar0
  (** Binary constant with given size and value*)

  val buninit: size:In_bits.t -> binary Arity.ar0
  (** Uninitialized binary value*)
end

(****************************************************************)
(* Offset transfer functions.  *)

module type OFFSET_BACKWARD = sig
  type boolean
  type offset
  module Arity: ARITY

  (* <= *)
  val le: size:int -> (offset,offset,boolean) Arity.ar2
  (* = *)
  val eq: size:int -> (offset,offset,boolean) Arity.ar2

  (* offset + k *)
  val shift: size:int -> offset:int -> (offset,offset) Arity.ar1
end

module type OFFSET_FORWARD = sig
  include OFFSET_BACKWARD
end 

(****************************************************************)
(* Block transfer functions.  *)

module type BLOCK_BACKWARD = sig
  type boolean
  type offset
  type value
  type block
  module Arity: ARITY

  (** Size of a block in bytes *)
  val sizeof: (block,offset) Arity.ar1

  (** Concatenates two blocks *)
  val concat: (block,block,block) Arity.ar2

  (** Loads (extracts) a value of a fixed size at a given index from a block *)
  val load: size:In_bits.t -> (block,offset,value) Arity.ar2

  (** Stores (writes) a fixed size value of a given index in a block *)
  val store: size:In_bits.t -> (block,offset,value,block) Arity.ar3

  (** Converts a fixed size value to a block *)
  val binary_to_block: size:In_bits.t -> (value,block) Arity.ar1
end

module type BLOCK_FORWARD = sig
  include BLOCK_BACKWARD
end

(****************************************************************)
(** {1 Enum transfer functions} *)
(** Transfer function for enum values. Enums are types with a fixed (small) number of possible cases. *)

module type ENUM_BACKWARD = sig

  type boolean
  type enum
  module Arity: ARITY

  (** Boolean operation to check if the enumeration value is in case [case] *)
  val caseof: case:int -> (enum,boolean) Arity.ar1
end

module type ENUM_FORWARD = sig
  include ENUM_BACKWARD
  val enum_const: case:int -> enum Arity.ar0
  (** Constant enum value *)
end

(****************************************************************)
(** {1 Memory transfer functions} *)

module type MEMORY_BACKWARD = sig
  type block
  type memory
  type address
  type value
  type boolean
  module Arity: ARITY
  (* TODO: All sizes should probably be in bytes. *)
  val load: size:In_bits.t -> (memory, address, value * memory) Arity.ar2
  val store: size:In_bits.t -> (memory,address,value,memory) Arity.ar3
  val load_block: (memory, address, block * memory) Arity.ar2
  val store_block: (memory,address,block,memory) Arity.ar3
  (* Fixed-size memcpy(store,from_,to_). *)
  val memcpy: size:In_bits.t -> (memory,address,address,memory) Arity.ar3
  val malloc: id:Malloc_id.t -> malloc_size:In_bytes.t ->
    (memory,address * memory) Arity.ar1
  val free: (memory,address,memory) Arity.ar2
  val unknown: level:int -> memory Arity.ar0
end

module type MEMORY_FORWARD = sig
  include MEMORY_BACKWARD
end
