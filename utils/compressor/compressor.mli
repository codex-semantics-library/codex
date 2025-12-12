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


(** This module offers some small utilities to compress and decompress data by
    writing it to a single byte sequence. One key advantage of doing it this way
    is to pack multiple discriminating booleans in a single byte, another is to
    store multiple int32 or int64 values without the boxing cost.

    The {!compress} type accumulates data and then renders it to bytes when done.
    The {!decompress} type does the reverse, extracting data from bytes.
    {b Data MUST be extracted in the SAME order that it was inserted}:

    {@ocaml[
      # open Compressor;;
      # let compress = make 12;;
      val compress : compress = <abstr>

      # write_int8 compress 'a';
        write_bool compress true;
        write_int32 compress 42l;
        write_bytes compress (Bytes.of_string "hello");
        write_bool compress false;
        write_bool compress true;;
      - : unit = ()

      # let bytes = to_bytes compress;;
      val bytes : bytes = Bytes.of_string "a*\000\000\000hello\005"

      # let decompress = of_bytes bytes;;
      val decompress : decompress = <abstr>

      # read_int8 decompress;;
      - : char = 'a'
      # read_bool decompress;;
      - : bool = true
      # read_int32 decompress;;
      - : int32 = 42l
      # read_bytes decompress 5 |> String.of_bytes;;
      - : string = "hello"
      # read_bool decompress;;
      - : bool = false
      # read_bool decompress;;
      - : bool = true
    ]} *)

(** {1 Compression}                                                           *)
(******************************************************************************)

(** {2 Types}                                                        *)
(*********************************************************************)

type compress

exception Uncompressable
(** Exception raised when trying to compress a [Z.t] that does not fit [int64]. *)

val make : int -> compress
(** [make n] creates a compress object with an [n] bytes internal buffer.
    The buffer is resized as needed, but picking a large enough [n] avoids unnecessary copies *)

(** {2 Core operations}                                              *)
(*********************************************************************)
(** These write the data exactly as is to the byte sequence.         *)

val write_bytes : compress -> bytes -> unit
val write_int8 : compress -> char -> unit
val write_int32 : compress -> int32 -> unit
val write_int64 : compress -> int64 -> unit
val write_bool : compress -> bool -> unit

(** {2 Compound operations}                                          *)
(*********************************************************************)
(** These use a combination of writing booleans and data in an attempt to be
    small. For instance, {!write_int} will use {!write_int32} if the value is
    small enough, else {!write_int64}:

    {@ocaml[
      # let compress = make 8 in
        write_z ~signed:true compress (Z.of_int 3);
        to_bytes compress;;
      - : bytes = Bytes.of_string "\003\000\000\000\001"
      # (* Bigger numbers lead to longer sequences *)
        let compress = make 8 in
        write_z ~signed:true compress (Z.of_int 5_000_000_000);
        to_bytes compress;;
      - : bytes = Bytes.of_string "\000\242\005*\001\000\000\000\000"
    ]} *)

val write_int : compress -> int -> unit
val write_z : signed:bool -> compress -> Z.t -> unit
(** [write_z ~signed z] attempts to push [z] as a 32 or 64 bit value.
    @raises {!Uncompressable} if the value is too large. *)

val write_option : (compress -> 'a -> unit) -> compress -> 'a option -> unit
val write_either :
  (compress -> 'a -> unit) ->
  (compress -> 'b -> unit) ->
  compress -> ('a, 'b) Either.t -> unit

val to_bytes : compress -> bytes

(** {1 Decompression}                                                         *)
(******************************************************************************)
(** Decompression must be performed in the same order as compression. There
    is no way to check that the bytes being decompressed were originally of the
    given type.

    Arbitrary decompression will not create invalid values, (since all types
    have no invalid values) but may fail with
    [Invalid_argument "Index out of bounds"] (if decompressing more bytes then
    were compressed). *)

type decompress

val of_bytes : bytes -> decompress

val read_bytes : decompress -> int -> bytes
val read_int8 : decompress -> char
val read_int32 : decompress -> int32
val read_int64 : decompress -> int64
val read_bool : decompress -> bool

val read_int : decompress -> int
(** May raise [Z.Overflow] if incorrectly called, as an arbitrary value
    may not fit 31 or 63 bits. *)

val read_z : signed:bool -> decompress -> Z.t
val read_option : (decompress -> 'a) -> decompress -> 'a option
val read_either :
  (decompress -> 'a) ->
  (decompress -> 'b) ->
  decompress -> ('a, 'b) Either.t
