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

(** Some transfer functions are partial: e.g. division by zero,
    loading at invalid addresses, arithmetic operation that overflow
    when overflowing is not permitted. When this situation happens in
    the analyzer, we raise an alarm.

    TODO: we should add more information to better report what is the
    alarm. For instance, for overflows, we should tell what kind of
    operation created an overflow (add, sub, mul, div by -1, etc.),
    what kind of overflow (signed,unsigned, or unsigned + signed),
    etc.

    TODO: We should raise the corresponding alarm as an exception in
    the concrete implementation too, to better document what is
    happening.

    TODO: We should factorize and document each alarm. *)

(** [Signed] means that both operations were signed, the result was
    signed and did not fit in [size] bits.

    [Unsigned] means that both operations were unsigned, the result
    was unsigned and did not fit in [size] bits.

    [Unsigned_signed] means that the first operation was unsigned, the
    second signed, the result was unsigned and did not fit in [size]
    bits. *)
type integer_overflow_type = Signed | Unsigned | Unsigned_signed

(** Operation that triggered the overflow warning. *)
type integer_overflow_operation = Biadd | Bisub | Bimul | Bisdiv | Bishl

(** This is a list of all alarms that can be raised by Codex *)
type _ t =
  | Integer_overflow : {
      overflow_type : integer_overflow_type;
      size : Units.In_bits.t;
      operation : integer_overflow_operation;
    }
      -> [ `Any ] t
      (** An integer operation on bitvector overflows its type.  *)
  | Integer_underflow : {
      overflow_type : integer_overflow_type;
      size : Units.In_bits.t;
      operation : integer_overflow_operation;
    }
      -> [ `Any ] t
      (** An integer operation on bitvector underflows its type.  *)
  (* TODO: Clarify the following ones. *)
  | Free_on_null_address : [ `Any ] t
  | Extract_offset : [ `Any ] t
  | Heap_typing : [ `Any ] t
  | Weak_type : [ `Any ] t
  | Weak_array_type : [ `Any ] t
  | Array_offset_access : [ `Any ] t
  | Load_param_nonptr : [ `Any ] t
  | Store_param_nonptr : [ `Any ] t
  | Load_param_nonptr_old : [ `Any ] t
  | Incompatible_function_pointer : [ `Any ] t
  | Incompatible_return_type : [ `Any ] t
  | Impure_store : [ `Any ] t
  | Manual_stub : [ `Any ] t
  | Ptr_arith : [ `Any ] t
  | Invalid_load_access : [ `Any ] t
  | Invalid_store_access : [ `Any ] t
  | Possibly_false_assertion : [ `Any ] t
  | Unresolved_dynamic_jump : [ `Any ] t
  | Weak_type_use : [ `Typing ] t
  | Typing_store : [ `Typing ] t

(* TODO: Display an informative message instead, with a short name,
   and an explanation of what happens and why it is wrong. *)
let show (type a) : a t -> string = function
  | Free_on_null_address -> "free-on-null-address"
  | Extract_offset -> "extract_offset"
  | Weak_type_use -> "weak-type-use"
  | Typing_store -> "typing_store"
  | Weak_type -> "weak-type "
  | Array_offset_access -> "array_offset_access"
  | Weak_array_type -> "weak_array_type"
  | Load_param_nonptr_old -> "load_param_nonptr_old"
  | Load_param_nonptr -> "load_param_nonptr"
  | Store_param_nonptr -> "store_param_nonptr"
  | Integer_overflow { overflow_type = Signed } -> "signed integer overflow"
  | Integer_overflow { overflow_type = Unsigned } -> "unsigned integer overflow"
  | Integer_overflow { overflow_type = Unsigned_signed } ->
      "unsigned-signed integer overflow"
  | Integer_underflow { overflow_type = Signed } -> "signed integer underflow"
  | Integer_underflow { overflow_type = Unsigned } ->
      "unsigned integer underflow"
  | Integer_underflow { overflow_type = Unsigned_signed } ->
      "unsigned-signed integer underflow"
  | Incompatible_return_type -> "incompatible-return-type"
  | Incompatible_function_pointer -> "incompatible_function_pointer"
  | Impure_store -> "impure_store"
  | Heap_typing -> "heap_typing"
  | Manual_stub -> "manual_stub"
  | Ptr_arith -> "ptr_arith"
  | Invalid_load_access -> "-alarm- invalid_load_access"
  | Invalid_store_access -> "-alarm- invalid_store_access"
  | Possibly_false_assertion -> "possibly_false_assertion"
  | Unresolved_dynamic_jump -> "unresolved_dynamic_jump"
