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

(** Compute various statistical indicators for a collection of values *)

type exhaustive = |
type compact = |

type ('a, 'kind) t
(** Stats for a collection of type ['a], either [int] or [float].

    If ['kind] is {!exhaustive}, then this contains all values, and can compute
    the {!median}, {!q1} and {!q3} (computing these requires sorting, but it is
    only done once, however, adding new values will require resorting).

    Otherwise, when ['kind] is {!compact}, this is a {b constant sized} aggregate.
    It does not store all the values, only the minimal required to compute some
    stats (min, max, sum, sum of squares). It can't compute the median or
    quartiles. *)

(** {1 Constuctors} *)


val make_float: (int, 'kind) t -> (float, 'kind) t
(** Convert an int collection to a float collection *)

val make_compact: ('a, exhaustive) t -> ('a, compact) t
(** Convert an exhaustive collection into a compact representation *)

(** {2 Compact values} *)
(** While empty constructors are provided, accessing stats of empty will raise
    {!CollectionTooShort}. List and array constructors are linear in the size
    of the given list/array. *)

val compact_int_empty: (int, compact) t
val compact_float_empty: (float, compact) t

val compact_int_singleton: int -> (int, compact) t
val compact_float_singleton: float -> (float, compact) t

val compact_of_int_list: int list -> (int, compact) t
val compact_of_float_list: float list -> (float, compact) t

val compact_of_int_array: int array -> (int, compact) t
val compact_of_float_array: float array -> (float, compact) t

(** {2 Exhaustive values} *)

val exhaustive_int_empty: (int, exhaustive) t
val exhaustive_float_empty: (float, exhaustive) t

val exhaustive_int_singleton: int -> (int, exhaustive) t
val exhaustive_float_singleton: float -> (float, exhaustive) t

val exhaustive_of_int_list: int list -> (int, exhaustive) t
val exhaustive_of_float_list: float list -> (float, exhaustive) t

val exhaustive_of_int_array: int array -> (int, exhaustive) t
val exhaustive_of_float_array: float array -> (float, exhaustive) t

(** {2 Adding values} *)

val add_value: ('a, 'kind) t -> 'a -> ('a, 'kind) t
(** Add a new value to the collection.
    Constant time operation. *)

val concat: ('a, 'kind) t -> ('a, 'kind) t -> ('a, 'kind) t
(** Concatenate both collections.
    Constant time operation. *)

val add_list: ('a, 'kind) t -> 'a list -> ('a, 'kind) t
(** Add all values in the list, equivalent to [List.fold_left add_value],
    linear in the size of the list for compact values, [O(n log n)] (in size of list+collection) for exhaustive *)

val add_array: ('a, 'kind) t -> 'a array -> ('a, 'kind) t
(** Add all values in the array, equivalent to [Array.fold_left add_value],
    linear in the size of the array for compact values, [O(n log n)] (in size of array+collection) for exhaustive *)

(** {1 Accessors} *)
(** All accessors are constant time operations,
    except {!median}, {!q1} and {!q3}, which need to sort the collection.
    Sorting is only done once and then saved, so getting the {!q1} after
    computing the {!median} is constant time. *)

val size: ('a, 'kind) t -> int
(** The size of the collection, i.e. the number of elements *)

exception CollectionTooShort
(** Exception raised when attempting to access the stats ({!sum}, {!min}, {!average}, ...)
    of an empty collection (whose {!size} is [0]), or attempting to access {!q1} or {!q3}
    of a collection whose {!size} is smaller than [4]. *)

val sum: ('a, 'kind) t -> 'a
(** Sum of all items in the collection: {m \sum_i x_i} *)

val sum_squares: ('a, 'kind) t -> 'a
(** The sum of the squares of the collection:  {m \sum_i x_i^2}.
    May raise [Z.Overflow]. *)

val min: ('a, 'kind) t -> 'a
(** The minimal element *)

val max: ('a, 'kind) t -> 'a
(** The maximal element *)

val range: ('a, 'kind) t -> 'a
(** The range, i.e. [max - min]. *)

val average: ('a, 'kind) t -> float
(** The average/mean value: i.e. [sum collection / size collection]. *)

val variance: ('a, 'kind) t -> float
(** The {{: https://en.wikipedia.org/wiki/Variance}variance}: i.e. sum of the squares of the difference with the average
    {m \sum_i (x_i - \mu)^2} *)

val standard_deviation: ('a, 'kind) t -> float
(** The square root of the {!variance}. *)

val median: ('a, exhaustive) t -> float
(** The median, or 2nd quartile *)

val q1: ('a, exhaustive) t -> float
(** The first quartile, requires size >= 4 *)

val q3: ('a, exhaustive) t -> float
(** The third quartile, requires size >= 4 *)

(** {1 Export values} *)
(** Export the list/array of values, sorted in increasing order.
    If unsorted, these will sort the collection ([O(n log n)]), else they will
    copy it [O(n)]. *)

val to_list: ('a, exhaustive) t -> 'a list
val to_array: ('a, exhaustive) t -> 'a array

(** {1 Pretty printers} *)
(** Both of these take an extra [unit] parameter to mark the end of the optional
    arguments. *)

val pp_percent:
  ?justify:bool ->
  ?precision:int ->
  unit ->
  Format.formatter -> int * int -> unit
(** [pp_percent () fmt (num, denom)] prints the ratio [num / denom] as a percentage,
    including a final ["%"] symbol. Rounds fractions, so ["20.99%"] is printed
    as ["21.0%"] when [precision] is [1].

    @param justify (default: [false]), when true, add spaces left of the number
      so that they all take the same space (print [" 20.0%"] instead of ["20.0%"])
    @param precision (default: [1]) number of digits to print.
        [0 -> "20%" | 1 -> "20.0%" | 2 -> "20.00%"], etc... *)

val unit_prefixes : string list
(** Standard SI unit prefix list: [""; "k"; "M"; "G"; "T"; "P"; "E"; "Z"; "Y"; "R"; "Q"]. *)

val pp_with_unit:
  ?justify:bool ->
  ?unit_prefixes:string list ->
  ?separator:string ->
  ?base:int ->
  unit ->
  Format.formatter -> int -> unit
(** [pp_with_unit () fmt nb] prints the number nb with at most three digits using the
    specified unit prefixes. For example:
    - [pp_unit fmt 123]       -> ["123"]
    - [pp_unit fmt 12345]     -> ["12.3k"]
    - [pp_unit fmt 123456789] -> ["123M"]

    @param justify (default: [false]), left-pad so all numbers have the same widths
    @param unit_prefixes (default: {!unit_prefixes})
        the prefix letters, increment each [base] step
    @param separator printed between number and unit, default is empty string
    @param base (default: [1000]), the scale between unit increments, [1000] or [1024]  *)

(** {1 Multi-session loggers} *)

(** Save stats between mutliple codex runs.
    Each logger saves a mapping [string -> stat] between various runs.

    THE LOGGER MUST BE INSTANCIATED BEFORE THE STARTUP HOOK IS EXECUTED, an
    failwith will be triggered if that is not the case. *)
module StatLogger(S : sig
  val id: string (** Unique logger id, must be the same between all runs *)

  type stat
  (** The type of stats being saved. For example:
      - A stat aggregate {{!t}[int t]} or {{!t}[float t]} (in which case {!combine} is just {{!concat}[fun _ -> concat]})
      - A record of such stat aggregates ({!combine} is then the field-wise {!concat})
      - A list of values ({!combine} is then [List.concat]) *)

  val combine : string -> stat -> stat -> stat
end)() : sig
  val add : string -> S.stat -> unit
  (** [add key stat] Add a stat with the given key.
      If a stat is already present for this key, they are merged with
      {{!S.combine}[S.combine key old_stat stat]} *)

  val get : string -> S.stat option
  (** Get the stat value for the key, [None] if the key is undefined. *)

  val to_list : unit -> (string * S.stat) list
  (** Return a list of all defined keys and their associated value *)
end
