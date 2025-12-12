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

(* Most of this code is adapated from the OCaml Standard library's implementation
   of Hashtbl.

   Copyright 1996 Institut National de Recherche en Informatique et en Automatique.
   distributed under the terms of the GNU Lesser General Public License
   version 2.1, with the  special exception on linking described in the license *)

(* Hash tables *)


module type HETEROGENEOUS_HASHED_TYPE = sig
  type 'a t
  val polyeq: 'a t -> 'b t -> ('a, 'b) PatriciaTree.cmp
  val hash: 'a t -> int
end

module type HETEROGENEOUS_SEEDED_HASHED_TYPE = sig
  type 'a t
  val polyeq: 'a t -> 'b t -> ('a, 'b) PatriciaTree.cmp
  val hash: int -> 'a t -> int
end

module type S = sig
  type 'a t
  type 'key key
  type ('key, 'a) value

  type 'b key_value = KeyValue : 'a key * ('a, 'b) value -> 'b key_value

  val create : ?random:bool -> int -> 'a t
  val clear : 'a t -> unit
  val reset : 'a t -> unit
  val copy : 'a t -> 'a t
  val add : 'a t -> 'key key -> ('key, 'a) value -> unit
  val remove : 'a t -> 'b key -> unit
  val find : 'a t -> 'key key -> ('key, 'a) value
  val find_opt : 'a t -> 'key key -> ('key, 'a) value option
  val find_all : 'a t -> 'key key -> ('key, 'a) value list
  val replace : 'a t -> 'key key -> ('key, 'a) value -> unit
  val mem : 'a t -> 'key key -> bool
  val add_seq : 'a t -> 'a key_value Seq.t -> unit
  val replace_seq : 'a t -> 'a key_value Seq.t -> unit
  val of_seq : 'a key_value Seq.t -> 'a t

  type 'a polyiter = { f : 'key. 'key key -> ('key, 'a) value -> unit; } [@@unboxed]
  val iter : 'a polyiter -> 'a t -> unit

  type ('a, 'b) polyfiltermap = { f : 'key. 'key key -> ('key, 'a) value -> ('key, 'b) value option; } [@@unboxed]
  val filter_map_inplace : ('a, 'a) polyfiltermap -> 'a t -> unit

  type ('a, 'acc) polyfold = { f : 'key. 'key key -> ('key, 'a) value -> 'acc -> 'acc; } [@@unboxed]
  val fold : ('a, 'acc) polyfold -> 'a t -> 'acc -> 'acc

  val length : 'a t -> int
  val stats : 'a t -> Hashtbl.statistics
  val to_seq : 'a t -> unit -> 'a key_value Seq.node
end

let is_randomized = Hashtbl.is_randomized

let prng = lazy (Random.State.make_self_init())

 (* Functions which appear before the functorial interface must either be
    independent of the hash function or take it as a parameter (see #2202 and
    code below the functor definitions. *)

 (* Creating a fresh, empty table *)

 let rec power_2_above x n =
   if x >= n then x
   else if x * 2 > Sys.max_array_length then x
   else power_2_above (x * 2) n



module MakeSeeded(Key: HETEROGENEOUS_SEEDED_HASHED_TYPE)(Value: PatriciaTree.HETEROGENEOUS_VALUE) = struct
  type 'value t = {
    mutable size: int; (* number of entries *)
    mutable data: 'value bucketlist array;  (* the buckets *)
    seed: int;                        (* for randomization *)
    mutable initial_size: int;                (* initial array size *)
  }
  and 'value bucketlist =
    | Empty
    | Cons : { mutable key: 'key Key.t;
               mutable data: ('key, 'value) Value.t;
               mutable next: 'value bucketlist } -> 'value bucketlist

  type 'a key = 'a Key.t
  type ('a, 'b) value = ('a, 'b) Value.t

  let create ?(random ) initial_size =
    let s = power_2_above 16 initial_size in
    let random =
      match random with
      | Some bool -> bool
      | None -> is_randomized () in
    let seed = if random then Random.State.bits (Lazy.force prng) else 0 in
    { initial_size = s; size = 0; seed = seed; data = Array.make s Empty }

  let clear h =
    if h.size > 0 then begin
      h.size <- 0;
      Array.fill h.data 0 (Array.length h.data) Empty
    end

  let reset h =
    let len = Array.length h.data in
    if len = abs h.initial_size then
      clear h
    else begin
      h.size <- 0;
      h.data <- Array.make (abs h.initial_size) Empty
    end

  let copy_bucketlist = function
    | Empty -> Empty
    | Cons {key; data; next} ->
        let rec loop prec = function
          | Empty -> ()
          | Cons {key; data; next} ->
              let r = Cons {key; data; next} in
              begin match prec with
              | Empty -> assert false
              | Cons prec ->  prec.next <- r
              end;
              loop r next
        in
        let r = Cons {key; data; next} in
        loop r next;
        r

  let copy h = { h with data = Array.map copy_bucketlist h.data }

  let key_index h key = (Key.hash h.seed key) land (Array.length h.data - 1)

  let ongoing_traversal h = h.initial_size < 0
  let flip_ongoing_traversal h = h.initial_size <- - h.initial_size

  type index_fun = { f : 'a. 'a key -> int } [@@unboxed]

  let insert_all_buckets indexfun inplace odata ndata =
    let nsize = Array.length ndata in
    let ndata_tail = Array.make nsize Empty in
    let rec insert_bucket = function
      | Empty -> ()
      | Cons {key; data; next} as cell ->
          let cell =
            if inplace then cell
            else Cons {key; data; next = Empty}
          in
          let nidx = indexfun.f key in
          begin match ndata_tail.(nidx) with
          | Empty -> ndata.(nidx) <- cell;
          | Cons tail -> tail.next <- cell;
          end;
          ndata_tail.(nidx) <- cell;
          insert_bucket next
    in
    for i = 0 to Array.length odata - 1 do
      insert_bucket odata.(i)
    done;
    if inplace then
      for i = 0 to nsize - 1 do
        match ndata_tail.(i) with
        | Empty -> ()
        | Cons tail -> tail.next <- Empty
      done

  let resize indexfun h =
    let odata = h.data in
    let osize = Array.length odata in
    let nsize = osize * 2 in
    if nsize < Sys.max_array_length then begin
      let ndata = Array.make nsize Empty in
      let inplace = not (ongoing_traversal h) in
      h.data <- ndata;          (* so that indexfun sees the new bucket count *)
      insert_all_buckets (indexfun h) inplace odata ndata
    end

  let add h key data =
    let i = key_index h key in
    let bucket = Cons{key; data; next=h.data.(i)} in
    h.data.(i) <- bucket;
    h.size <- h.size + 1;
    if h.size > Array.length h.data lsl 1
    then resize (fun t -> { f=fun a -> key_index t a }) h

  let rec remove_bucket: type a. 'b t -> int -> a key -> 'b bucketlist -> 'b bucketlist -> unit =
    fun h i key prec next -> match next with
    | Empty -> ()
    | (Cons {key=k; next; _}) as c -> match Key.polyeq key k with
        | Diff -> remove_bucket h i key c next
        | Eq ->
          h.size <- h.size - 1;
          match prec with
          | Empty -> h.data.(i) <- next
          | Cons c -> c.next <- next

  let remove h key =
    let i = key_index h key in
    remove_bucket h i key Empty h.data.(i)

  let rec find_rec : type a. a key -> 'b bucketlist -> (a, 'b) value = fun key bucketlist ->
    match bucketlist with
    | Empty -> raise Not_found
    | Cons{key=k; data; next} -> match Key.polyeq key k with
      | Eq -> data
      | Diff -> find_rec key next

  let find h key = find_rec key h.data.(key_index h key)

  let find_opt h key = match find h key with
    | value -> Some value
    | exception Not_found -> None

  let find_all : type a. 'b t -> a key -> (a, 'b) value list = fun h key ->
    let rec find_in_bucket : 'b bucketlist -> (a, 'b) value list = function
       | Empty -> []
       | Cons{key=k; data=d; next} ->
           match Key.polyeq k key with
           | Eq -> d :: find_in_bucket next
           | Diff -> find_in_bucket next
    in find_in_bucket h.data.(key_index h key)

  let rec replace_bucket: type a. a key -> (a, 'b) value -> 'b bucketlist -> bool =
    fun key data bucketlist -> match bucketlist with
    | Empty -> true
    | Cons ({key=k; next; _} as slot) ->
        match Key.polyeq k key with
        | Eq -> slot.key <- key;
                slot.data <- data;
                false
        | Diff -> replace_bucket key data next

  let replace h key data =
    let i = key_index h key in
    let l = h.data.(i) in
    if replace_bucket key data l then begin
      h.data.(i) <- Cons{key; data; next=l};
      h.size <- h.size + 1;
      if h.size > Array.length h.data lsl 1
      then resize (fun t -> { f=fun a -> key_index t a }) h
    end

  let mem: type a. 'b t -> a key -> bool = fun h key ->
    let rec mem_in_bucket = function
    | Empty -> false
    | Cons{key=k; next; _} ->
        match Key.polyeq k key with
        | Eq -> true
        | Diff -> mem_in_bucket next
    in mem_in_bucket h.data.(key_index h key)
    type 'b key_value = KeyValue: 'a key * ('a, 'b) value -> 'b key_value

  let add_seq tbl i = Seq.iter (fun (KeyValue(k,v)) -> add tbl k v) i

  let replace_seq tbl i = Seq.iter (fun (KeyValue(k,v)) -> replace tbl k v) i
  let of_seq i =
    let tbl = create 16 in
    replace_seq tbl i;
    tbl

  type 'b polyiter = { f: 'a. 'a key -> ('a, 'b) value -> unit } [@@unboxed]
  let iter f h =
    let rec do_bucket = function
      | Empty -> ()
      | Cons{key; data; next} ->
          f.f key data;
          do_bucket next
    in
    let old_trav = ongoing_traversal h in
    if not old_trav then flip_ongoing_traversal h;
    try
      let d = h.data in
      for i = 0 to Array.length d - 1 do
        do_bucket d.(i)
      done;
      if not old_trav then flip_ongoing_traversal h;
    with exn when not old_trav ->
      flip_ongoing_traversal h;
      raise exn

  type ('b, 'c) polyfiltermap = { f: 'a. 'a key -> ('a, 'b) value -> ('a, 'c) value option } [@@unboxed]
  let rec filter_map_inplace_bucket f h i prec = function
    | Empty -> begin match prec with
        | Empty -> h.data.(i) <- Empty
        | Cons c -> c.next <- Empty
        end
    | (Cons ({key; data; next} as c)) as slot -> begin match f.f key data with
        | None ->
            h.size <- h.size - 1;
            filter_map_inplace_bucket f h i prec next
        | Some data ->
            begin match prec with
            | Empty -> h.data.(i) <- slot
            | Cons c -> c.next <- slot
            end;
            c.data <- data;
            filter_map_inplace_bucket f h i slot next
        end

  let filter_map_inplace f h =
    let d = h.data in
    let old_trav = ongoing_traversal h in
    if not old_trav then flip_ongoing_traversal h;
    try
      for i = 0 to Array.length d - 1 do
        filter_map_inplace_bucket f h i Empty h.data.(i)
      done;
      if not old_trav then flip_ongoing_traversal h
    with exn when not old_trav ->
      flip_ongoing_traversal h;
      raise exn

  type ('b, 'acc) polyfold = { f: 'a. 'a key -> ('a, 'b) value -> 'acc -> 'acc } [@@unboxed]
  let fold f h init =
    let rec do_bucket b accu =
      match b with
        Empty ->
          accu
      | Cons{key; data; next} ->
          do_bucket next (f.f key data accu) in
    let old_trav = ongoing_traversal h in
    if not old_trav then flip_ongoing_traversal h;
    try
      let d = h.data in
      let accu = ref init in
      for i = 0 to Array.length d - 1 do
        accu := do_bucket d.(i) !accu
      done;
      if not old_trav then flip_ongoing_traversal h;
      !accu
    with exn when not old_trav ->
      flip_ongoing_traversal h;
      raise exn

  let length h = h.size

  let rec bucket_length accu = function
    | Empty -> accu
    | Cons{next} -> bucket_length (accu + 1) next
  let stats h =
    let mbl =
      Array.fold_left (fun m b -> Int.max m (bucket_length 0 b)) 0 h.data in
    let histo = Array.make (mbl + 1) 0 in
    Array.iter
      (fun b ->
        let l = bucket_length 0 b in
        histo.(l) <- histo.(l) + 1)
      h.data;
    { Hashtbl.num_bindings = h.size;
      num_buckets = Array.length h.data;
      max_bucket_length = mbl;
      bucket_histogram = histo }

  let to_seq tbl =
    (* capture current array, so that even if the table is resized we
        keep iterating on the same array *)
    let tbl_data = tbl.data in
    (* state: index * next bucket to traverse *)
    let rec aux i buck () = match buck with
      | Empty ->
          if i = Array.length tbl_data
          then Seq.Nil
          else aux(i+1) tbl_data.(i) ()
      | Cons {key; data; next} ->
          Seq.Cons (KeyValue(key, data), aux i next)
    in
    aux 0 Empty
end

module Make
    (Key: HETEROGENEOUS_HASHED_TYPE)
    (Value: PatriciaTree.HETEROGENEOUS_VALUE)
= MakeSeeded(struct
    include Key
    let hash (_seed: int) x = Key.hash x
  end)(Value)
