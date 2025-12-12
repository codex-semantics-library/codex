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

(** Floating-point operations. *)

(** Rounding modes defined in the C99 standard. *)
type c_rounding_mode =
    FE_ToNearest | FE_Upward | FE_Downward | FE_TowardZero

val string_of_c_rounding_mode : c_rounding_mode -> string

(* replace "noalloc" with [@@noalloc] for OCaml version >= 4.03.0 *)
[@@@ warning "-3"]

external set_round_downward : unit -> unit = "fc_ival_set_round_downward" "noalloc"
external set_round_upward : unit -> unit = "fc_ival_set_round_upward" "noalloc"
external set_round_nearest_even : unit -> unit = 
    "fc_ival_set_round_nearest_even" "noalloc"
external set_round_toward_zero : unit -> unit = "fc_ival_set_round_toward_zero" "noalloc"
external get_rounding_mode: unit -> c_rounding_mode = "fc_ival_get_rounding_mode" "noalloc"
external set_rounding_mode: c_rounding_mode -> unit = "fc_ival_set_rounding_mode" "noalloc"

[@@@ warning "+3"]

external round_to_single_precision_float: float -> float = "fc_ival_round_to_float"

val max_single_precision_float: float
val most_negative_single_precision_float: float

val min_denormal: float
val neg_min_denormal: float
val min_single_precision_denormal: float
val neg_min_single_precision_denormal: float

external sys_single_precision_of_string: string -> float = 
    "fc_ival_single_precision_of_string"


(** If [s] is parsed as [(n, l, u)], then [n] is the nearest approximation of
    [s] with the desired precision. Moreover, [l] and [u] are the
    most precise float such that [l <= s <= u], again with this precision.

    Consistent with [logic_real] definition in Cil_types. *)
type parsed_float = {
  f_nearest : float ;
  f_lower : float ;
  f_upper : float ;
}

(** [parse s] parses [s] and returns the parsed float and its kind (single,
    double or long double precision) according to its suffix, if any. Strings
    with no suffix are parsed as double. *)
(* val parse: string -> Cil_types.fkind * parsed_float *)

val pretty_normal : use_hex : bool -> Format.formatter -> float -> unit
val pretty : Format.formatter -> float -> unit


type sign = Neg | Pos

exception Float_Non_representable_as_Int64 of sign

val truncate_to_integer: float -> Z.t
(** Raises [Float_Non_representable_as_Int64] if the float value cannot
    be represented as an Int64 or as an unsigned Int64. *)


(** binary representation of -DBL_MAX and DBL_MAX as 64 bits signed integers *)
val bits_of_max_double : Z.t
val bits_of_most_negative_double : Z.t

(** binary representation of -FLT_MAX and FLT_MAX as 32 bits signed integers *)
val bits_of_max_float : Z.t
val bits_of_most_negative_float : Z.t

(** Rounds to nearest integer, away from zero (like round() in C). *)
external fround: float -> float = "fc_ival_c_round"

(** Rounds to integer, toward zero (like trunc() in C). *)
external trunc: float -> float = "fc_ival_c_trunc"

(** Single-precision (32-bit) floating-point wrappers *)

external expf: float -> float = "fc_ival_c_expf"
external logf: float -> float = "fc_ival_c_logf"
external log10f: float -> float = "fc_ival_c_log10f"
external powf: float -> float -> float = "fc_ival_c_powf"
external sqrtf: float -> float = "fc_ival_c_sqrtf"
external fmodf: float -> float -> float = "fc_ival_c_fmodf"
external cosf: float -> float = "fc_ival_c_cosf"
external sinf: float -> float = "fc_ival_c_sinf"
external atan2f: float -> float -> float = "fc_ival_c_atan2f"


(** Auxiliary functions similar to the ones in the C math library *)

val isnan : float -> bool
val isfinite : float -> bool

val nextafter : float -> float -> float
val nextafterf : float -> float -> float

(*
Local Variables:
compile-command: "make -C ../../.. byte"
End:
*)
