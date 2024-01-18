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

(* A generic immutable dynamic array datatype. The idea is to use an
   array to store the value (to allow O(1) access to elements); the
   array has a capacity larger than the length of the vector, such
   that appending just need to insert an element using that extra
   capacity. Of course, when appending several times to the same
   prefix, or when the capacity is exceeded, the array needs to be
   reallocated. *)

(* Note: instead of using an option type, we just need a special value
   that is different from any other 'a. This could avoid boxing in
   performance critical cases. Or we could just see if we are a
   sub-sequence of an existing array; the option type is probably
   un-necessary (at least could be replaced with a bitmap) *)

(* Note: another possible implementation of this structure is using
   VList: cheaper append but lookup with higher cost. *)
type 'a t = { length: int;
              mutable array: 'a option array }
;;

(* Note: as we double capacity when it is exceeded, we must not start
   with an empty array. *)
let empty () = { length = 0; array = [| None |]}
;;

let append vec elt =
  let {length;array} = vec in
  let capacity = Array.length array in
  assert (capacity != 0);
  let array =
    (* Capacity exceeded: allocate a new array. *)
    if length == capacity
    then let new_array = Array.make (2*capacity) None in
         Array.blit array 0 new_array 0 capacity;
         new_array
    (* Spot already filled: duplicate the array. *)
    else if (Array.unsafe_get array length) == None
    then let new_array = Array.make capacity None in
         Array.blit array 0 new_array 0 length;
         new_array
    else array
  in
  Array.unsafe_set array length (Some elt);
  {length=length+1;array}
;;

let (+++) = append

let singleton x = empty() +++ x;;
  
let length {length} = length;;

let unsafe_get array idx =
  match Array.unsafe_get array idx with
  | Some a -> a
  | None -> assert false        (* Broken invariant. *)
;;

let get {length;array} idx =
  assert (idx < length);
  unsafe_get array idx
;;

let fold_lefti f init {length;array} =
  let rec loop i acc =
    if i == length then acc
    else let acc = f acc i (unsafe_get array i)
         in loop (i+1) acc
  in loop 0 init
;;

let fold_left f = fold_lefti (fun acc _ x -> f acc x);;

let fold_right f init {length;array} =
  let rec loop i acc =
    if i < 0 then acc
    else let acc = f acc (unsafe_get array i)
         in loop (i-1) acc
  in loop (length-1) init
;;


let fold_left2 f init {length=l1;array=a1} {length=l2;array=a2} =
  assert (l1 == l2);
  let rec loop i acc =
    if i == l1 then acc
    else let acc = f acc (unsafe_get a1 i) (unsafe_get a2 i)
         in loop (i+1) acc
  in loop 0 init
;;

let fold_left3 f init {length=l1;array=a1} {length=l2;array=a2} {length=l3;array=a3} =
  assert (l1 == l2); assert(l1 == l3);
  let rec loop i acc =
    if i == l1 then acc
    else let acc = f acc (unsafe_get a1 i) (unsafe_get a2 i) (unsafe_get a3 i)
         in loop (i+1) acc
  in loop 0 init
;;


let map f {length;array=a} =
  let array = Array.make (Array.length a) None in
  for i = 0 to (length-1) do
    let value = f (unsafe_get a i) in
    Array.unsafe_set array i (Some value)
  done;
  { length; array}
;;

let mapi_filter f {length;array=a} =
  let new_ = ref (empty()) in
  for i = 0 to (length-1) do
    let value = f i (unsafe_get a i) in
    match value with
    | None -> ()
    | Some x -> new_ := append !new_ x
  done;
  !new_
;;

(* Generalizes map_filter: each element can be mapped to 0,1,or several elements; in order. *)
(* let map_flatten *)

let mapi f {length;array=a} =
  let array = Array.make (Array.length a) None in
  for i = 0 to (length-1) do
    let value = f i (unsafe_get a i) in
    Array.unsafe_set array i (Some value)
  done;
  { length; array}
;;


let iteri f {length;array} =
  for i = 0 to (length-1) do
    f i (unsafe_get array i)
  done
;;

let iter f {length;array} =
  for i = 0 to (length-1) do
    f (unsafe_get array i)
  done
;;





let map2 f {length=l1;array=a1} {length=l2;array=a2} =
  assert (l1 == l2);
  let array = Array.make (Array.length a1) None in
  for i = 0 to (l1-1) do
    let value = f (unsafe_get a1 i) (unsafe_get a2 i) in
    Array.unsafe_set array i (Some value)
  done;
  { length = l1; array}
;;

exception Forall;;
let for_all p {length;array} =
  try
    for i = 0 to length - 1 do
      if not (p (unsafe_get array i)) then raise Forall
    done;
    true
  with Forall -> false
;;    

let for_all2 p {length=l1;array=a1} {length=l2;array=a2} =
  assert (l1 == l2);
  try
    for i = 0 to (l1-1) do
      if not (p (unsafe_get a1 i) (unsafe_get a2 i)) then raise Forall
    done;
    true
  with Forall -> false
;;

let from_array ar =
  if Array.length ar == 0 then empty()
  else {length = Array.length ar; array = Array.map (fun x -> Some x) ar};;
let unsafe_from_array ar =
  (* assert (Array.length ar != 0); *)
  from_array ar      (* for now *)


let from_list l = from_array @@ Array.of_list l

let to_array ({length;array} as v) =
  Array.init length (fun i -> get v i);;

let to_list v = fold_right (fun b a -> a::b) [] v

let init length f =
  let ar = Array.init length (fun i -> Some (f i)) in
  {length; array = ar}
;;

let split a =
  let ar1 = Array.make a.length (Obj.magic 0) in
  let ar2 = Array.make a.length (Obj.magic 0) in
  for i = 0 to a.length - 1 do
    match a.array.(i) with
    | Some(a1,a2) -> ar1.(i) <- Some a1; ar2.(i) <- Some a2;
    | None -> assert false
  done;
  {array=ar1;length = a.length},  {array=ar2;length = a.length}
;;

let combine a1 a2 =
  assert (a1.length = a2.length);
  let array = Array.init a1.length (fun i ->
      let a1 = a1.array.(i) in
      let a2 = a2.array.(i) in
      match a1,a2 with
      | Some(a1), Some a2 -> Some(a1,a2)
      | _ -> assert false
    ) in
  {array;length = a1.length}
;;


type sformat = (unit,Format.formatter,unit) Stdlib.format
type 'a formatter = Format.formatter -> 'a -> unit

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


let pp_dynarray2 ?pre ?sep ?suf func =
  let iter f {length;array}=
    for i = 0 to (length-1) do f (i,(unsafe_get array i)) done
  in
  pp_iter ?pre ?sep ?suf iter func

let pp_dynarray ?pre ?sep ?suf func =
  let iter f {length;array}=
    for i = 0 to (length-1) do f (unsafe_get array i) done
  in
  pp_iter ?pre ?sep ?suf iter func

(* Unit tests. *)
(* let a0 = create();; *)
(* let a1 = append a0 1;; *)
(* let a2 = append a1 2;; *)
(* let a3 = append a2 3;; *)
(* let a4 = append a3 4;; *)

(* let b1 = append a0 11;; *)
(* let b2 = append a1 12;; *)
(* let b3 = append a2 13;; *)
(* let b4 = append a3 14;; *)

(* a4;; *)
(* b4;; *)
