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

include Array

type 'a t = 'a array

let empty = [| |]

type sformat = (unit,Format.formatter,unit) Stdlib.format
type 'a formatter = Format.formatter -> 'a -> unit

let iter3 f a b c =
  let alen = Array.length a in
  assert(Array.length b == alen);
  assert(Array.length c == alen);
  for i = 0 to alen - 1 do
    f (Array.unsafe_get a i) (Array.unsafe_get b i) (Array.unsafe_get c i);
  done
;;


let pp_iter
    ?(pre=format_of_string "@[")
    ?(sep=format_of_string "")
    ?(suf=format_of_string "@]")
    iter pp fmt v =
  let need_sep = ref false in
  Format.fprintf fmt pre;
  iter (fun v ->
          if !need_sep then Format.fprintf fmt sep else need_sep := true;
          pp fmt v;
       ) v;
  Format.fprintf fmt suf;
;;


let pp_array ?pre ?sep ?suf func =
  let iter f ar =
    for i = 0 to (Array.length ar) - 1 do f (unsafe_get ar i) done
  in
  pp_iter ?pre ?sep ?suf iter func

let fold_left2 f init a1 a2 =
  let l1 = Array.length a1 and l2 = Array.length a2 in
  assert (l1 == l2);
  let rec loop i acc =
    if i == l1 then acc
    else let acc = f acc (unsafe_get a1 i) (unsafe_get a2 i)
         in loop (i+1) acc
  in loop 0 init
;;

let fold_left3 f init a1 a2 a3 =
  let l1 = Array.length a1
  and l2 = Array.length a2
  and l3 = Array.length a3 in
  assert (l1 == l2); assert(l1 == l3);
  let rec loop i acc =
    if i == l1 then acc
    else let acc = f acc (unsafe_get a1 i) (unsafe_get a2 i) (unsafe_get a3 i)
         in loop (i+1) acc
  in loop 0 init
;;


let _cast_from_array x = x
