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

type 'a t = { mutable array: 'a option array;
              mutable length: int (* Last known set index. *)
            }
;;

let empty() = { length = 0; array = [| None |]};;

let set ar i value =
  let prev_capacity = Array.length ar.array in
  (* We double the capacity unless i is very large.
     This still allows amortized complexity. *)
  if i >= prev_capacity then begin
    let new_capacity = max (i + 1) @@ 2 * prev_capacity in
    let new_array = Array.make new_capacity None in
    Array.blit ar.array 0 new_array 0 prev_capacity;
    ar.array <- new_array
  end;
  (* ar.length is the max of the values set so far. *)
  if i >= ar.length then
    ar.length <- i + 1;
  ar.array.(i) <- Some value
;;

let get ar i =
  try
    match Array.get ar.array i with
    | None -> raise Not_found
    | Some v -> v
  with Invalid_argument _ -> raise Not_found     (* Array out of bounds. *)
;;

let length ar = ar.length

let append ar value = set ar ar.length value
