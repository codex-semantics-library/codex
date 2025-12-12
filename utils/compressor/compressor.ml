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

exception Uncompressable

(** The storage format is
    {v
      +------------------------------------+------------+
      | Aligned data (int32, 64, bytes...) | extra bits |
      +------------------------------------+------------+
      0                                               size-1
    v}
    - Aligned data is in order of insertion
    - Extra bits is in the reverse order of insertion, grouped by bytes.
      So the last byte contains [b0, b1, ... b7] where [b0] is the first pushed
      boolean, [b1] the second and so on.
      [b0] is the least significant bit, [b1] is shifted by 1, [b2] by 2, etc...
      The byte before last is [b8, ... b15] and so on.

      If the total number of booleans isn't a multiple of eight, the extra bits
      is padded with 0 on the left.

    This essentially allows a double stack, growing from the front for aligned
    data, and from the rear for booleans, until the middle point, whose position
    is unknown. *)

(** {1 Compress}                                                              *)
(******************************************************************************)

(** Implementation as a byte list - easy, but lots of allocations *)
(* module ByteList = struct
  type compress = {
    mutable aligned_data: bytes list;
    mutable extra_bits: bool list;
  }

  let make _ = { aligned_data = []; extra_bits = [] }
  let write_bytes x bytes = x.aligned_data <- bytes :: x.aligned_data
  let write_int8 x char =
    let bytes = Bytes.create 1 in
    Bytes.set bytes 0 char;
    write_bytes x bytes

  let write_int32 x int32 =
    let bytes = Bytes.create 4 in
    Bytes.set_int32_ne bytes 0 int32;
    write_bytes x bytes

  let write_int64 x int64 =
    let bytes = Bytes.create 8 in
    Bytes.set_int64_ne bytes 0 int64;
    write_bytes x bytes

  let write_bool x b = x.extra_bits <- b :: x.extra_bits

  let byte_of_bits x =
    let (_, offset, value, char_list) = List.fold_left (fun (pos, offset, value, char_list) b ->
      let value = if b then value lor (1 lsl offset) else value in
      if offset = 7
      then (pos+1, 0,        0,     char_of_int value :: char_list)
      else (pos,   offset+1, value, char_list))
    (0, 0, 0, []) (List.rev x) in
    (if offset != 0 then char_of_int value :: char_list else char_list)
    |> List.to_seq
    |> Bytes.of_seq

  let to_bytes { aligned_data; extra_bits } =
    let extra_bits = byte_of_bits extra_bits in
    Bytes.concat Bytes.empty (List.rev_append aligned_data [extra_bits])
end *)

(** Vector/Dynarrat-like byte structure: aligned data is resized as stuff is inserted *)
type compress = {
  mutable pos: int;
  mutable aligned_data: bytes;
  mutable bit_pos: int;
  mutable extra_bits: int list;
  (* This is really a list of char, but since bitwise operation aren't available on char,
     and a char takes the same space as an int, we only convert to char at the last minute. *)
}

let make n = { pos=0; aligned_data = Bytes.create (n+1); bit_pos = 8; extra_bits = [] }

let resize x n =
  let len = Bytes.length x.aligned_data in
  if len < n then
    x.aligned_data <- Bytes.extend x.aligned_data 0 (max (n-len) len)

let write_bytes x bytes =
  let len = Bytes.length bytes in
  let new_pos = x.pos + len in
  resize x new_pos;
  Bytes.blit bytes 0 x.aligned_data x.pos len;
  x.pos <- new_pos

let write_int8 x char =
  let new_pos = x.pos + 1 in
  resize x new_pos;
  Bytes.set x.aligned_data x.pos char;
  x.pos <- new_pos

let write_int32 x i32 =
  let new_pos = x.pos + 4 in
  resize x new_pos;
  Bytes.set_int32_ne x.aligned_data x.pos i32;
  x.pos <- new_pos

let write_int64 x i64 =
  let new_pos = x.pos + 8 in
  resize x new_pos;
  Bytes.set_int64_ne x.aligned_data x.pos i64;
  x.pos <- new_pos

let write_bool x b =
  if x.bit_pos = 8 then (
    x.bit_pos <- 1;
    x.extra_bits <- Bool.to_int b :: x.extra_bits
  )
  else match x.extra_bits with
  | [] -> failwith "Invalid compress"
  | i::is ->
      x.extra_bits <- (i lor (Bool.to_int b) lsl x.bit_pos) :: is;
      x.bit_pos <- x.bit_pos + 1

let to_bytes x =
  let len_extra = List.length x.extra_bits in
  let remaining_size = Bytes.length x.aligned_data - x.pos in
  if remaining_size != len_extra then
    x.aligned_data <- Bytes.extend x.aligned_data 0 (len_extra - remaining_size);
  List.iteri (fun i c -> Bytes.set x.aligned_data (x.pos + i) (char_of_int c)) x.extra_bits;
  x.aligned_data

(** {2 Compound operations}                                         *)
(********************************************************************)

let write_option write_value x opt = match opt with
  | None -> write_bool x false
  | Some value -> write_bool x true;
                  write_value x value

let write_either write_left write_right compress = function
  | Either.Left l -> write_bool compress false;
                     write_left compress l
  | Either.Right r -> write_bool compress true;
                      write_right compress r

let write_z ~signed compress z =
  let fits_32 = if signed then Z.fits_int32 z else Z.fits_int32_unsigned z in
  write_bool compress fits_32;
  if fits_32 then
    write_int32 compress (if signed then Z.to_int32 z else Z.to_int32_unsigned z)
  else try
    write_int64 compress (if signed then Z.to_int64 z else Z.to_int64_unsigned z)
  with Z.Overflow -> raise Uncompressable

let fits_31 int = -0x40_00_00_00 <= int && int <= 0x3f_ff_ff_ff

let mark_i32 i32 =
  if Sys.big_endian
  then Int32.logor i32 Int32.min_int
  else Int32.shift_left i32 1 |> Int32.add 1l

(* Return true if the mark is 0, false if it isn't *)
let unmark_i32 i32 =
  if Sys.big_endian
  then (
    (* get top bit to check mark, then shift left then right to recover top bit sign *)
    Int32.logand i32 Int32.max_int |> Int32.equal 0l,
    let i32 = Int32.shift_left i32 1 in Int32.shift_right i32 1
  )
  else (
    Int32.logand i32 1l |> Int32.equal 0l,
    Int32.shift_right i32 1)

let unmark_i64 i64 =
  if Sys.big_endian
  then let i64 = Int64.shift_left i64 1 in Int64.shift_right i64 1
  else Int64.shift_right i64 1

let mark_i64 i64 =
  if Sys.big_endian
  then i64
  else Int64.shift_left i64 1

let write_int compress int =
  (* If fits 31 bits, ensure lowest bit is 1 *)
  if fits_31 int
  then Int32.of_int int |> mark_i32 |> write_int32 compress
  else Int64.of_int int |> mark_i64 |> write_int64 compress

(** {1 Decompress}                                                            *)
(******************************************************************************)

type decompress = {
  bytes: bytes;
  mutable aligned_pos: int;
  mutable extra_bits_pos: int;
}

let of_bytes bytes = { bytes; aligned_pos=0; extra_bits_pos=0 }
let read_bytes x n =
  let pos = x.aligned_pos in
  x.aligned_pos <- pos + n;
  Bytes.sub x.bytes pos n

let read_int8 x =
  let pos = x.aligned_pos in
  x.aligned_pos <- pos + 1;
  Bytes.get x.bytes pos

let read_int32 x =
  let pos = x.aligned_pos in
  x.aligned_pos <- pos + 4;
  Bytes.get_int32_ne x.bytes pos

let read_int64 x =
  let pos = x.aligned_pos in
  x.aligned_pos <- pos + 8;
  Bytes.get_int64_ne x.bytes pos

let read_bool x =
  let pos = x.extra_bits_pos in
  x.extra_bits_pos <- x.extra_bits_pos + 1;
  let char = Bytes.get x.bytes (Bytes.length x.bytes - 1 - pos / 8) in
  int_of_char char land (1 lsl (pos mod 8)) <> 0

(** {2 Compound operations}                                         *)
(********************************************************************)

let read_option read_value x = if read_bool x then Some (read_value x) else None

let read_either read_left read_right x =
  if read_bool x
  then Either.Right (read_right x)
  else Either.Left (read_left x)

let read_z ~signed x =
  if read_bool x
  then read_int32 x |> (if signed then Z.of_int32 else Z.of_int32_unsigned)
  else read_int64 x |> (if signed then Z.of_int64 else Z.of_int64_unsigned)

let read_int x =
  let i32 = read_int32 x in
  let is_64, i32 = unmark_i32 i32 in
  if is_64 then
    (* undo read, then read int64 *)
    let () = x.aligned_pos <- x.aligned_pos - 4 in
    read_int64 x |> unmark_i64 |> Int64.to_int
  else Int32.to_int i32
