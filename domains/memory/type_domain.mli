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

module Ctypes = Types.Ctypes

module type S = sig
  module BR : Memory_sig.Fixed_size_value_domain

  type t = Ctypes.typ
  (** [PtrT {ptyp;idx;fe;ofs;ppred}] abstracts the type [ptyp.o* with ppred],
       with [o = i * sizeof(ptyp) + ofs], where [i] belongs to the
       concretizations of both [idx] and [fe].
  *)

  (** Size in bits of the array index argument to {ParamT}. *)
  val array_index_size : int

  val index_zero : BR.Scalar.Context.t -> BR.binary
  val index_minus_one : BR.Scalar.Context.t -> BR.binary

  (** Size in bits of the struct offset argument to {ParamT}. *)
  val offset_size : int

  val eq : only_descr:bool -> BR.Scalar.Context.t -> t -> t -> bool

  (** A more imprecise equality between {!t}s that does not require a
     context. The difference is in the comparison between the array indices
     (of type {!BR.binary}). The only guarantee on this function is that if
     [t] and [u] are not equal, then [imprecise_eq t u] is [false]. *)
  val imprecise_eq : t -> t -> bool

  exception Global_symbol_not_found

  val fresh_symbol : unit -> string 

  (** Retrieve the abstract numeric value corresponding to a global symbolic
     constant (along with its size in bits). May throw {!Global_symbol_not_found}. *)
  val symbol : BR.Scalar.Context.t -> string -> int * BR.binary * t

  val simple_symbol : BR.Scalar.Context.t -> string -> int * BR.binary * t

  val add_symbol : BR.Scalar.Context.t -> string -> int -> BR.binary -> t -> unit

  val is_symbol : BR.Scalar.Context.t -> string -> bool

  val type_of_binary : BR.Scalar.Context.t -> BR.binary -> t option

  (** Utility functions to manipulate predicates over abstract values of the
     numeric domain. These functions are not really about types but it was
     convenient to put them here. *)

  val cond_of_pred_subdomain : size:int -> BR.Scalar.Context.t -> Ctypes.Pred.t
    -> BR.binary -> BR.boolean
  val check_invariant_subdomain : size:int -> BR.Scalar.Context.t -> Ctypes.Pred.t
    -> BR.binary -> bool

  (** Required by the domain signature, but don't expect this order to make
     any sense. WARNING: this function is NOT the lattice comparison
     function! *)
  val compare : t -> t -> int
  val pp : BR.Scalar.Context.t -> Format.formatter -> t -> unit

  exception Type_error of string

  val serialize : BR.Scalar.Context.t -> t -> BR.Scalar.Context.t -> t -> 'a BR.Scalar.Context.in_acc
    -> (t option, 'a) BR.Scalar.Context.result
  val join : BR.Scalar.Context.t -> t -> t -> t option

  val serialize_with_bottom_left : BR.Scalar.Context.t -> BR.Scalar.Context.t -> t -> 'a BR.Scalar.Context.in_acc
    -> (t, 'a) BR.Scalar.Context.result
  val serialize_with_bottom_right : BR.Scalar.Context.t -> t -> BR.Scalar.Context.t -> 'a BR.Scalar.Context.in_acc
    -> (t, 'a) BR.Scalar.Context.result

  (* Represent a type that is either a pointer or a non-pointer value,
     represented only by its predicate. *)
  type dereferenced_value = Ctypes.typ

  (** We represent the result of dereferencing a type as a sequence of
      word-sized types for ease of type checking upon store.
      This does not endanger soundness nor precision since we do not support
      predicates on product types yet.
      The return value is a list of (size in bytes, type) pairs,
      obtained by dereferencing the argument type which must be of [size] bytes.
      May throw a {!exception:Type_error}. *)
  val deref : size:int -> BR.Scalar.Context.t -> t -> (int * dereferenced_value) list

  (** Simpler version of above. *)
  val deref_to_ctype : size:int -> BR.Scalar.Context.t -> t -> Ctypes.typ

  (** Temporary. *)
  val dereferenced_value_of_typ: size:int ->
    BR.Scalar.Context.t -> Ctypes.typ list -> (int * dereferenced_value) list
  
  (** Lattice comparison function. *)
  val subtype : BR.Scalar.Context.t -> t -> t -> bool

  (** Non-standard but needed operator. See notes. Returns true iff one of
      the concrete types in the concretization of the first operand is a
      subtype of one of the concrete types in the concretization of the first
      operand. *)
  val may_subtype : BR.Scalar.Context.t -> t -> t -> bool

  (** [add_offset ctx t ~idx ~fe ~old_ofs ~pred ~incr] returns the result of
      adding the abstract offset [incr] to a pointer to [t], with an index
      described by [idx] and [fe] and an offset in the pointed object
      [old_ofs]. *)
  val add_offset : BR.Scalar.Context.t -> BR.Binary.t -> t -> t

  (** Determines whether the offset associated with a pointer is in the bounds
      of the pointed type. Assumes that the argument has been produced using
      only applications of {!add_offset} starting from a zero-offset pointer,
      and not crafted by other means.
      As a consequence, this operation only makes sense on arrays, since only
      on array pointers can {!add_offset} return an out-of-bounds pointer.
      On pointers to non-array types, it returns true. *)
  val in_bounds : BR.Scalar.Context.t -> t -> bool

  (** Returns [true] iff the concretization of the type is empty, i.e. if the
      concretization of the index is the empty set. *)
  val is_empty : BR.Scalar.Context.t -> t -> bool
end

module Make : functor (BR : Memory_sig.Fixed_size_value_domain)
    (TS : Ctypes.Type_settings)
-> S with module BR = BR

module EvalPred : sig

  module type Arg = sig
    include Domain_sig.Base
    val symbol : Context.t -> string -> binary
  end

  module type Sig = sig
    module Domain : Domain_sig.Base
    val check_invariant : size:int -> Domain.Context.t -> Ctypes.Pred.t
      -> Domain.binary -> bool
    val use_invariant : size:int -> Domain.Context.t -> Ctypes.Pred.t
      -> Domain.binary -> Domain.Context.t option
  end

  module Make : functor (A : Arg) ->
    Sig with module Domain := A

end
