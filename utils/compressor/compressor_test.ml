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

type value =
  | Bool of bool
  | Int32 of int32
  | Int64 of int64
  | Char of char
  | Int of int
  | Z of (bool * Z.t)
  | Opt of value option
  | Either of (value, value) Either.t

let rec pp_value fmt = function
  | Bool b -> Format.pp_print_bool fmt b
  | Int32 i -> Format.fprintf fmt "I32(%ld)" i
  | Int64 i -> Format.fprintf fmt "I64(%Ld)" i
  | Char c -> Format.fprintf fmt "'%c'" c
  | Int i -> Format.pp_print_int fmt i
  | Z (b, z) -> Format.fprintf fmt "Z(%b, %a)" b Z.pp_print z
  | Opt None -> Format.fprintf fmt "None"
  | Opt (Some v) -> Format.fprintf fmt "Some(%a)" pp_value v
  | Either (Left v) -> Format.fprintf fmt "Left(%a)" pp_value v
  | Either (Right v) -> Format.fprintf fmt "Right(%a)" pp_value v

open Compressor

let rec write_value compress = function
  | Bool b -> write_bool compress b
  | Int32 i -> write_int32 compress i
  | Int64 i -> write_int64 compress i
  | Char c -> write_int8 compress c
  | Int i -> write_int compress i
  | Z (signed, z) -> write_z ~signed compress z
  | Opt value -> write_option write_value compress value
  | Either value -> write_either write_value write_value compress value

let rec read_value decomp = function
  | Bool _ -> Bool (read_bool decomp)
  | Int32 _ -> Int32 (read_int32 decomp)
  | Int64 _ -> Int64 (read_int64 decomp)
  | Char _ -> Char (read_int8 decomp)
  | Int _ -> Int (read_int decomp)
  | Z (signed, _) -> Z (signed, read_z ~signed decomp)
  | Opt None -> Opt (read_option (fun decomp -> read_value decomp (Bool false)) decomp)
  | Opt (Some v) -> Opt (read_option (fun decomp -> read_value decomp v) decomp)
  | Either (Left v)
  | Either (Right v) ->
      let read_value decomp = read_value decomp v in
      Either (read_either read_value read_value decomp)

let gen_value =
  let open QCheck.Gen in
  (* Delay recursion so `Opt` can refer back to `value` *)
  fix (fun self () ->
    frequency
      [ 4, map (fun b -> Bool b) bool;
        1, map (fun i -> Int32 i) int32;
        1, map (fun i -> Int64 i) int64;
        1, map (fun c -> Char c) char;
        1, map (fun i -> Int i) int;
        1, map2 (fun b z -> Z (b, if b then Z.of_int64 z else Z.of_int64_unsigned z)) bool int64;
        1,
          map (fun opt -> Opt opt)
            (option (self ()));
        1, map2 (fun b v -> if b then Either (Left v) else Either (Right v)) bool (self ());
      ]) ()

let rec shrink v =
  let open QCheck.Iter in
  match v with
  | Bool _ -> empty
  | Int32 i -> map (fun i' -> Int32 i') (QCheck.Shrink.int32 i)
  | Int64 i -> map (fun i' -> Int64 i') (QCheck.Shrink.int64 i)
  | Int i -> map (fun i' -> Int i') (QCheck.Shrink.int i)
  | Char c -> map (fun c' -> Char c') (QCheck.Shrink.char c)
  | Z (b, z) ->
      let i = if b then Z.to_int64 z else Z.to_int64_unsigned z in
      map (fun i -> Z(b, if b then Z.of_int64 i else Z.of_int64_unsigned i)) (QCheck.Shrink.int64 i)
  | Opt None -> empty
  | Opt (Some v') ->
      (* Shrink inside Some v, or shrink to None *)
      of_list [Opt None] <+> (shrink v' |> map (fun v2 -> Opt (Some v2)))
  | Either (Left v) -> shrink v |> map (fun v -> Either (Left v))
  | Either (Right v) -> shrink v |> map (fun v -> Either (Right v))


let arb_value = QCheck.make
  ~print:(fun v -> Format.asprintf "%a" pp_value v)
  ~shrink
  gen_value

let arb_list = QCheck.list arb_value

let check_compress_decompress list =
  let compress = make 10 in
  List.iter (write_value compress) list;
  let bytes = to_bytes compress in
  (* Format.eprintf "Bytes: %d %a@." (Bytes.length bytes) Format.pp_print_bytes bytes; *)
  let decompress = of_bytes bytes in
  List.fold_left (fun eq value -> read_value decompress value = value && eq) true list

let _ = assert (check_compress_decompress [
  Int 0;
  Opt None;
  Bool true;
  Opt (Some (Int 0));
  Bool true;
  Z(false, Z.zero);
  Int 0;
  Int 0
  ])

let test_compress = QCheck.Test.make ~count:1000 ~name:"Compressor compress/decompress" arb_list
  check_compress_decompress

let%test_unit _ = QCheck.Test.check_exn test_compress
