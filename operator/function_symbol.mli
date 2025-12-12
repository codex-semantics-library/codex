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

(** Function symbol: Reusable tags that allow a building a term,
    i.e. a tagged representation of an expressions that composes
    transfer functions. *)
open Units

(** {1 Some aliases for legibility} *)
type case = int
(** The case number of an enum *)

(** {1 Phantom type parameters} *)

(** These types are never instanciated as values.  They are used as
    phantom parameters in GADTs to distinguish terms and function
    symboles based on the concrete value they represent. *)

(** A boolean value, e.g. [true] or [false] *)
type boolean = private TypeBoolean

(** An unbounded integer value, often represented by [Z.t] in the concrete *)
type integer = private TypeInteger

(** An enum value: can take a fixed number of values depending on the enum type
    being considered. *)
type enum = private TypeEnum

(** A bitvector of size [size] We generally do not store the size;
    instead, most operation have a [~size] parameter specifying the
    number of bits of the underlying operation. *)
type bitvector = private TypeBitvector
type binary = bitvector

type memory = private TypeMemory

(** {1 Arities} *)

(** A term of arity 0 is just a constant *)
type ar0 = private Ar0

(** A term of arity 1 is a unary operation (ex: boolean not).
    The ['a] parameter represents the type of the argument. *)
type 'a ar1 = private Ar1

(** A term of arity 2 is a binary operation (ex: addition)
    The ['a] parameter represents the type of the arguments. *)
type ('a, 'b) ar2 = private Ar2

(** {1 Types} *)

(** A GADT regrouping all our phantom type parameters *)
type 'a typ =
  | Boolean : boolean typ
  | Integer : integer typ
  | Enum : enum typ
  | Binary : In_bits.t -> binary typ
  | Memory : memory typ

type any_type = Any_type : 'a typ -> any_type [@@unboxed]
(** Existential wrapper for {!typ}. *)

(** {1 Function symbols} *)

(** The type of function symbols takes two parameters:

    - ['arg] is the arity, i.e. the type of arguments needed to build
    this term.  The arity should be either {!ar0}, {{!ar1}['a ar1]} or
    {{!ar2}[('a, 'b) ar2]}.

    - ['ret] it the return type of the function symbol, i.e. the type
    of the term whose head is this function symbol. *)

type ('arg, 'ret) function_symbol =
  | True : (ar0, boolean) function_symbol
  | False : (ar0, boolean) function_symbol
  | And : ((boolean, boolean) ar2, boolean) function_symbol
  | Or : ((boolean, boolean) ar2, boolean) function_symbol
  | Not : (boolean ar1, boolean) function_symbol
  | BoolUnion: ((boolean,boolean) ar2,boolean) function_symbol
  | Biconst : In_bits.t * Z.t -> (ar0, binary) function_symbol
      (** Bitvector constant with the given size and value. *)
  | Biadd : {size:In_bits.t;flags:Flags.Biadd.t} -> ((binary, binary) ar2, binary) function_symbol
      (** Addition on bitvectors of size [size].
          See {{!Operator.Sig.Bitvector_Backward.biadd}[biadd]} for the flag meanings. *)
  | Bisub: {size:In_bits.t;flags:Flags.Bisub.t} -> ((binary,binary) ar2,binary) function_symbol        
      (** Bitvector subtraction
          See {{!Operator.Sig.Bitvector_Backward.biadd}[biadd]} for the flag meanings. *)
  | Bimul : {size:In_bits.t;flags:Flags.Bimul.t} -> ((binary, binary) ar2, binary) function_symbol
      (** Bitvector multiplication,
          See {{!Operator.Sig.Bitvector_Backward.biadd}[biadd]} for the flag meanings. *)
  | Biudiv : In_bits.t -> ((binary, binary) ar2, binary) function_symbol
      (** Bitvector unsigned division. *)
  | Bisdiv : In_bits.t -> ((binary, binary) ar2, binary) function_symbol
      (** Bitvector signed division.
          This is truncated (C99-like) integer division. *)
  | Biumod : In_bits.t -> ((binary, binary) ar2, binary) function_symbol
      (** Bitvector unsigned modulo. *)
  | Bismod : In_bits.t -> ((binary, binary) ar2, binary) function_symbol
      (** Bitvector signed modulo. *)
  | Bshl : {size:In_bits.t;flags:Flags.Bshl.t} -> ((binary, binary) ar2, binary) function_symbol
      (** Bitvector left-shift *)
  | Bashr : In_bits.t -> ((binary, binary) ar2, binary) function_symbol
      (** Bitvector arithmetic right shift (pad with top-bit) *)
  | Blshr : In_bits.t -> ((binary, binary) ar2, binary) function_symbol
      (** Bitvector logical right shift (pad with zeros) *)
  | Band : In_bits.t -> ((binary, binary) ar2, binary) function_symbol
      (** Bitvector bitwise and *)
  | Bor : In_bits.t -> ((binary, binary) ar2, binary) function_symbol
      (** Bitvector bitwise or *)
  | Bxor : In_bits.t -> ((binary, binary) ar2, binary) function_symbol
      (** Bitvector bitwise xor *)
  | Beq : In_bits.t -> ((binary, binary) ar2, boolean) function_symbol
      (** Bitvector equality *)
  | Bisle : In_bits.t -> ((binary, binary) ar2, boolean) function_symbol
      (** Signed comparison between two bitvectors (using less-or-equal [<=]).
          Bitvectors are interpreted as integers in {m [-2^{size-1}..-2^{size-1}-1]} using two's complement. *)
  | Biule : In_bits.t -> ((binary, binary) ar2, boolean) function_symbol
      (** Unsigned comparison between two bitvectors (using less-or-equal [<=]).
          Bitvectors are interpreted as positive integers in {m [0..-2^{size}-1]}. *)
  | Bconcat : In_bits.t * In_bits.t -> ((binary, binary) ar2, binary) function_symbol
      (** Bitvector concatenation: the new bitvector's size is the sum of the sizes of the arguments.
          The first argument becomes the most significant bits. *)
  | Bextract : { size:In_bits.t; index:In_bits.t; oldsize:In_bits.t; } -> (binary ar1, binary) function_symbol
      (** Extract a [size] of a bitvector of total size [oldsize], starting at [index].
          Should satisfy [index + size <= oldsize]. *)
  | Bsext : In_bits.t -> (binary ar1, binary) function_symbol
      (** Sign-extend (pad left with topbit) the argument bitvector until it reaches the specified [size]. *)
  | Buext : In_bits.t -> (binary ar1, binary) function_symbol
      (** Unsngned-extend (pad left with zero) the argument bitvector until it reaches the specified [size]. *)
  | Bofbool: In_bits.t -> (boolean ar1,binary) function_symbol
      (** Turn a boolean into a bitvector: [false] is [0] and [true] is [1]. *)
  | Bunion: Operator_ids.Condition.t * In_bits.t -> ((binary,binary) ar2,binary) function_symbol
  | Bchoose: Operator_ids.Choice.t * In_bits.t -> (binary ar1,binary) function_symbol
  | Iconst : Z.t -> (ar0, integer) function_symbol
      (** Integer constant *)
  | Iadd : ((integer, integer) ar2, integer) function_symbol
  | Isub : ((integer, integer) ar2, integer) function_symbol
  | Imul : ((integer, integer) ar2, integer) function_symbol
  | Idiv : ((integer, integer) ar2, integer) function_symbol
      (** This is truncated (C99-like) integer division *)
  | Imod : ((integer, integer) ar2, integer) function_symbol
  | Ishl : ((integer, integer) ar2, integer) function_symbol
  | Ishr : ((integer, integer) ar2, integer) function_symbol
  | Ior : ((integer, integer) ar2, integer) function_symbol
      (** Bitwise or, where negative integers are seen as prefixed by infinite ones *)
  | Ixor : ((integer, integer) ar2, integer) function_symbol
  | Iand : ((integer, integer) ar2, integer) function_symbol
      (** Bitwise and, where negative integers are seen as prefixed by infinite ones *)
  | Ieq : ((integer, integer) ar2, boolean) function_symbol
  | Ile : ((integer, integer) ar2, boolean) function_symbol
  | Itimes : Z.t -> (integer ar1, integer) function_symbol
      (** Multiply an integer by a constant *)
  | EnumConst: case -> (ar0, enum) function_symbol
      (** An enum case with the given value *)
  | CaseOf : case -> (enum ar1, boolean) function_symbol
      (** [true] if the argument matches the given case, [false] otherwise. *)

(** Builder for these terms. Note that we do not perform any hash-consing here. *)
module Build:sig

  module Arity:sig
    type nonrec 'r ar0 = (ar0,'r) function_symbol
    type nonrec ('a,'r) ar1 = ('a ar1,'r) function_symbol
    type nonrec ('a,'b,'r) ar2 = (('a,'b) ar2,'r) function_symbol
    type nonrec ('a,'b,'c,'r) ar3 = unit
  end

  module Binary:sig
    include Operator_sig.BINARY_FORWARD
      with module Arity := Arity
       and type boolean := boolean
       and type binary := binary
    val bunion: size:In_bits.t -> Operator_ids.Condition.t -> ((binary,binary) ar2, binary) function_symbol
  end

end
val type_of : ('a, 'b) function_symbol -> 'b typ
val hash : ('a, 'b) function_symbol -> int
val equal : ('a, 'b) function_symbol -> ('c, 'd) function_symbol -> bool


(** Helper to build pretty-printers. We can pretty print a term if we
    have access to the tag (representing the operation) and we know
    how to pretty-print the arguments. *)
module type PRETTY_ARG = sig
  type 'a t
  (** The term to print. *)

  type 'a pack
  (** Packing of the arguments *)

  val pretty : Format.formatter -> 'a t -> unit
  (** Term pretty printer *)

  val extract1 : 'a ar1 pack -> 'a t
  (** Extract the argument when there is one argument in a term.  *)

  val extract2 : ('a, 'b) ar2 pack -> 'a t * 'b t
  (** Extract the arguments when there are two arguments in a term. *)
end


module type PRETTY_RESULT = sig
  type 'a t
  type 'a pack

  (** Pretty print a term that is destructured, i.e. given as a
      separate function symbol and a "pack" or arguments. *)
  val pretty_destruct :
    Format.formatter -> ('arg, 'a) function_symbol -> 'arg pack -> unit
end

module Pretty(M : PRETTY_ARG):PRETTY_RESULT
  with type 'a t = 'a M.t and type 'a pack = 'a M.pack
