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

(* ocamlfind ocamlc okasakimaptop.mli okasakimaptop.ml -package zarith -linkpkg *)

(* The integer associated with a key *)
type intkey = int

(* A mask is an integer with a single bit set (i.e. a power of 2). *)
type mask = int


module type Node = sig
  type 'key key
  type ('key, 'map) value
  type 'map t

  val empty : 'map t
  val leaf : 'key key -> ('key, 'map) value -> 'map t
  val branch :
    prefix:intkey ->
    branching_bit:mask -> tree0:'map t -> tree1:'map t -> 'map t

  type 'map view = private
    | Empty : 'map view
    | Branch : { prefix : intkey; branching_bit : mask;
                 tree0 : 'map t; tree1 : 'map t; } -> 'map view
    | Leaf : { key : 'key key; value : ('key, 'map) value; } -> 'map view

  val is_empty: 'map t -> bool
  val view: 'a t -> 'a view
end

module type NodeWithId = sig
  include Node
  val get_id: 'a t -> int
end

module type BaseMap_S = sig
  include Node

  type 'map key_value_pair =
      KeyValue : 'a key * ('a, 'map) value -> 'map key_value_pair
  val min_binding : 'a t -> 'a key_value_pair
  val max_binding : 'a t -> 'a key_value_pair
  val singleton : 'a key -> ('a, 'b) value -> 'b t
  val cardinal : 'a t -> int
  val is_singleton : 'a t -> 'a key_value_pair option

  val find : 'map t -> 'key key -> ('key, 'map) value
  (** @raises [Not_found] if key is absent from map *)

  val find_opt : 'map t -> 'key key -> ('key, 'map) value option
  (** Same as [find], but returns [None] for [Not_found] *)

  val mem : 'map t -> 'key key -> bool
  val remove : 'map t -> 'key key -> 'map t
  val pop_minimum: 'map t -> ('map key_value_pair * 'map t) option
  val pop_maximum: 'map t -> ('map key_value_pair * 'map t) option

  val insert: 'map t -> 'a key -> (('a,'map) value option -> ('a,'map) value) -> 'map t
  val update: 'map t -> 'a key -> (('a,'map) value option -> ('a,'map) value option) -> 'map t
  val add : 'map t -> 'key key -> ('key, 'map) value -> 'map t

  val split : 'key key -> 'map t -> 'map t * ('key, 'map) value option * 'map t

  type 'map polyiter = { f : 'a. 'a key -> ('a, 'map) value -> unit; } [@@unboxed]
  val iter : 'map t -> 'map polyiter -> unit
  type ('acc,'map) polyfold = { f: 'a. 'a key -> ('a,'map) value -> 'acc -> 'acc } [@@unboxed]
  val fold : 'map t -> 'acc -> ('acc,'map) polyfold -> 'acc

  type 'map polypredicate = { f: 'a. 'a key -> ('a,'map) value -> bool; } [@@unboxed]
  val filter : 'map t -> 'map polypredicate -> 'map t
  val for_all : 'map t -> 'map polypredicate -> bool

  type ('map1,'map2) polymap =
    { f : 'a. 'a key -> ('a, 'map1) value -> ('a, 'map2) value; } [@@unboxed]
  val map : 'map t -> ('map,'map) polymap -> 'map t
  val map_no_share : 'map1 t -> ('map1,'map2) polymap -> 'map2 t

  type ('map1,'map2) polyfilter_map =
    { f : 'a. 'a key -> ('a, 'map1) value -> ('a, 'map2) value option; } [@@unboxed]
  val filter_map : 'map t -> ('map,'map) polyfilter_map -> 'map t
  val filter_map_no_share : 'map1 t -> ('map1,'map2) polyfilter_map -> 'map2 t

  type 'map polypretty = { f: 'a. Format.formatter -> 'a key -> ('a, 'map) value -> unit } [@@unboxed]
  val pretty :
    ?pp_sep:(Format.formatter -> unit -> unit) -> 'map polypretty ->
    Format.formatter -> 'map t -> unit

  type ('map1,'map2) polysame_domain_for_all2 =
    { f : 'a. 'a key -> ('a, 'map1) value -> ('a, 'map2) value -> bool; } [@@unboxed]

  val reflexive_same_domain_for_all2 :
    'map t -> 'map t -> ('map,'map) polysame_domain_for_all2 -> bool
  val nonreflexive_same_domain_for_all2:
    'map1 t -> 'map2 t -> ('map1,'map2) polysame_domain_for_all2 -> bool
  val reflexive_subset_domain_for_all2 :
    'map t -> 'map t -> ('map,'map) polysame_domain_for_all2 -> bool
  type ('map1, 'map2, 'map3) polyunion = {
    f : 'a. 'a key -> ('a, 'map1) value -> ('a, 'map2) value -> ('a, 'map3) value; } [@@unboxed]
  val idempotent_union : 'a t -> 'a t -> ('a, 'a, 'a) polyunion -> 'a t

  type ('map1, 'map2, 'map3) polyinter = {
    f : 'a. 'a key -> ('a, 'map1) value -> ('a, 'map2) value -> ('a, 'map3) value;
  } [@@unboxed]
  val idempotent_inter : 'a t -> 'a t -> ('a, 'a, 'a) polyinter -> 'a t
  val nonidempotent_inter_no_share : 'a t -> 'b t -> ('a, 'b, 'c) polyinter -> 'c t

  type ('map1, 'map2, 'map3) polyinterfilter = { f : 'a. 'a key -> ('a, 'map1) value -> ('a, 'map2) value -> ('a, 'map3) value option; } [@@unboxed]
  val idempotent_inter_filter : 'a t -> 'a t -> ('a, 'a, 'a) polyinterfilter -> 'a t

  type ('map1, 'map2, 'map3) polymerge = {
    f : 'a. 'a key -> ('a, 'map1) value option -> ('a, 'map2) value option -> ('a, 'map3) value  option; } [@@unboxed]
  val slow_merge : 'map1 t -> 'map2 t -> ('map1, 'map2, 'map3) polymerge -> 'map3 t
end

module type Map_S = sig
  type key
  type 'a t

  (** Underlying basemap, for cross map/set operations *)
  module BaseMap : BaseMap_S
   with type 'a t = 'a t
    and type _ key = key
    and type (_,'a) value = 'a

  val empty: 'a t
  val is_empty: 'a t -> bool
  val min_binding: 'a t -> (key * 'a)
  val max_binding: 'a t -> (key * 'a)
  val singleton: key -> 'a -> 'a t
  val cardinal: 'a t -> int
  val is_singleton: 'a t -> (key * 'a) option
  val find: 'a t -> key -> 'a
  val find_opt: 'a t -> key -> 'a option
  val mem: 'a t -> key -> bool
  val remove: 'a t -> key -> 'a t
  val pop_minimum: 'a t -> (key * 'a * 'a t) option
  val pop_maximum: 'a t -> (key * 'a * 'a t) option
  val insert: 'a t -> key -> ('a option -> 'a) -> 'a t
  val update: 'a t -> key -> ('a option -> 'a option) -> 'a t
  val add: 'a t -> key -> 'a -> 'a t
  val split: key -> 'a t -> 'a t * 'a option * 'a t
  val iter: 'a t -> (key -> 'a -> unit) -> unit
  val fold: 'a t -> 'acc -> (key -> 'a -> 'acc -> 'acc) -> 'acc
  val filter : 'a t -> (key -> 'a -> bool) -> 'a t
  (* val for_all : 'a t -> (key -> 'a -> bool) -> bool *)
  val map : 'a t -> (key -> 'a -> 'a) -> 'a t
  val map_no_share : 'a t -> (key -> 'a -> 'b) -> 'b t
  val filter_map : 'a t -> (key -> 'a -> 'a option) -> 'a t
  val filter_map_no_share : 'a t -> (key -> 'a -> 'b option) -> 'b t
  val reflexive_same_domain_for_all2: 'a t -> 'a t -> (key -> 'a -> 'a -> bool) -> bool
  val nonreflexive_same_domain_for_all2: 'a t -> 'b t -> (key -> 'a -> 'b -> bool) -> bool
  val reflexive_subset_domain_for_all2: 'a t -> 'a t -> (key -> 'a -> 'a -> bool) -> bool
  val idempotent_union : 'a t -> 'a t -> (key -> 'a -> 'a -> 'a) -> 'a t
  val idempotent_inter : 'a t -> 'a t -> (key -> 'a -> 'a -> 'a) -> 'a t
  val nonidempotent_inter_no_share : 'a t -> 'b t -> (key -> 'a -> 'b -> 'c) -> 'c t
  val idempotent_inter_filter : 'a t -> 'a t -> (key -> 'a -> 'a -> 'a option) -> 'a t
  val slow_merge: 'a t -> 'b t -> (key -> 'a option -> 'b option -> 'c option) -> 'c t
  module WithForeign(Map2:BaseMap_S with type 'a key = key):sig
    type ('b,'c) polyfilter_map_foreign = { f: 'a. key -> ('a,'b) Map2.value -> 'c option } [@@unboxed]
    val filter_map_no_share: 'b Map2.t -> ('b, 'c) polyfilter_map_foreign -> 'c t
    type ('value,'map2) polyinter_foreign =
      { f: 'a. 'a Map2.key -> 'value -> ('a, 'map2) Map2.value -> 'value  } [@@unboxed]
    val nonidempotent_inter: 'a t -> 'b Map2.t -> ('a, 'b) polyinter_foreign -> 'a t
    type ('map1,'map2) polyinsert_multiple = { f: 'a. key -> 'map1 option -> ('a,'map2) Map2.value -> 'map1 option } [@@unboxed]
    val insert_multiple: 'a t -> 'b Map2.t -> ('a,'b) polyinsert_multiple -> 'a t
    type ('map1,'map2) polyremove_multiple = { f: 'a. key -> 'map1 -> ('a,'map2) Map2.value -> 'map1 option } [@@unboxed]
    val remove_multiple: 'a t -> 'b Map2.t -> ('a,'b) polyremove_multiple -> 'a t
  end

  val pretty :
    ?pp_sep:(Format.formatter -> unit -> unit) ->
    (Format.formatter -> key -> 'a -> unit) ->
    Format.formatter -> 'a t -> unit
  (** [pp_sep] defaults to [Format.pp_print_cut] *)
end

module type Set_S = sig
  (** Underlying basemap, for cross map/set operations *)
  module BaseMap : BaseMap_S
    with type (_,_) value = unit

  type t = unit BaseMap.t
  type key = unit BaseMap.key

  val empty: t
  val is_empty: t -> bool
  val mem: key -> t -> bool
  val add: key -> t -> t
  val singleton: key -> t
  val cardinal: t -> int
  val is_singleton: t -> key option
  val remove: key -> t -> t
  val union: t -> t -> t
  val inter: t -> t -> t
  val disjoint: t -> t -> bool
  val split: key -> t -> t * bool * t
  val iter: (key -> unit) -> t -> unit
  val fold: (key -> 'b -> 'b) -> t -> 'b -> 'b
  val filter: (key -> bool) -> t -> t
  val for_all: (key -> bool) -> t -> bool
  val min_elt: t -> key
  val max_elt: t -> key

  val pop_minimum: t -> (key * t) option
  val pop_maximum: t -> (key * t) option

  val pretty :
    ?pp_sep:(Format.formatter -> unit -> unit) ->
    (Format.formatter -> key -> unit) ->
    Format.formatter -> t -> unit

  val equal : t -> t -> bool
  val subset : t -> t -> bool
end

module type HeterogeneousMap_S = sig
  include BaseMap_S

  module WithForeign(Map2:BaseMap_S with type 'a key = 'a key):sig

    type ('map1,'map2) polyinter_foreign = { f: 'a. 'a key -> ('a,'map1) value -> ('a,'map2) Map2.value -> ('a,'map1) value } [@@unboxed]
    val nonidempotent_inter: 'a t -> 'b Map2.t -> ('a,'b) polyinter_foreign -> 'a t

    type ('map2,'map1) polyfilter_map_foreign =
      { f : 'a. 'a key -> ('a, 'map2) Map2.value -> ('a, 'map1) value option; } [@@unboxed]
    val filter_map_no_share : 'map2 Map2.t -> ('map2,'map1) polyfilter_map_foreign -> 'map1 t

    type ('map1,'map2) polyinsert_multiple = { f: 'a. 'a key -> ('a,'map1) value option -> ('a,'map2) Map2.value -> ('a,'map1) value option } [@@unboxed]
    val insert_multiple: 'a t -> 'b Map2.t -> ('a,'b) polyinsert_multiple -> 'a t

    type ('map1,'map2) polyremove_multiple = { f: 'a. 'a key -> ('a,'map1) value -> ('a,'map2) Map2.value -> ('a,'map1) value option } [@@unboxed]
    val remove_multiple: 'a t -> 'b Map2.t -> ('a,'b) polyremove_multiple -> 'a t
  end
end

module type HeterogeneousSet_S = sig
  (** Underlying basemap, for cross map/set operations *)
  module BaseMap : BaseMap_S
    with type (_,_) value = unit

  type t = unit BaseMap.t
  type 'a key = 'a BaseMap.key

  type any_key = Any : 'a key -> any_key

  val empty: t
  val is_empty: t -> bool
  val mem: 'a key -> t -> bool
  val add: 'a key -> t -> t
  val singleton: 'a key -> t
  val cardinal: t -> int
  val is_singleton: t -> any_key option
  val remove: 'a key -> t -> t
  val union: t -> t -> t
  val inter: t -> t -> t
  val disjoint: t -> t -> bool

  val equal : t -> t -> bool
  val subset : t -> t -> bool

  val split: 'a key -> t -> t * bool * t

  val iter: ('a key -> unit) -> t -> unit
  type polypredicate = { f: 'a. 'a key -> bool; } [@@unboxed]
  val filter: polypredicate -> t -> t
  val for_all: polypredicate -> t -> bool

  type 'acc polyfold = { f: 'a. 'a key -> 'acc -> 'acc } [@@unboxed]
  val fold: 'acc polyfold -> t -> 'acc -> 'acc

  val min_elt: t -> any_key
  val max_elt: t -> any_key
  val pop_minimum: t -> (any_key * t) option
  val pop_maximum: t -> (any_key * t) option
  type polypretty = { f: 'a. Format.formatter -> 'a key -> unit; } [@@unboxed]
  val pretty :
    ?pp_sep:(Format.formatter -> unit -> unit) -> polypretty -> Format.formatter -> t -> unit
  (** [pp_sep] defaults to [Format.pp_print_cut] *)
end


(* Optimized computation, but does not work for values too high. *)
let _highest_bit v =
  (* compute highest bit.
     First, set all bits with weight less than
     the highest set bit *)
  let v1 = v lsr 1 in
  let v2 = v lsr 2 in
  let v = v lor v1 in
  let v = v lor v2 in
  let v1 = v lsr 3 in
  let v2 = v lsr 6 in
  let v = v lor v1 in
  let v = v lor v2 in
  let v1 = v lsr 9 in
  let v2 = v lsr 18 in
  let v = v lor v1 in
  let v = v lor v2 in
  (* then get highest bit *)
  (succ v) lsr 1
;;

let lowest_bit x =
  x land (-x)

let rec highest_bit x =
  let m = lowest_bit x in
  if x = m then
    m
  else
    highest_bit (x - m)
;;

let highest_bit x =
  1 lsl (Z.log2 @@ Z.of_int x)

(* Note: in the original version, okasaki give the masks as arguments
   to optimize the computation of highest_bit. *)
let branching_bit a b = highest_bit (a lxor b);;

let mask i m =
  i land (lnot (2*m-1));;



type (_,_) cmp = Eq: ('a,'a) cmp | Diff: ('a,'b) cmp;;

module type HeterogeneousKey = sig
  type 'key t                   (* A key to a value of type value.  *)
  val to_int: ('key) t -> int
  (* Polymorphic comparison. We don't use == in this variant (even
     with Obj.magic), as it could be used to escape type safety (==
     may return true for different types (e.g. (Obj.magic 0) ==
     (Obj.magic false))   *)
  val polyeq: 'a t -> 'b t -> ('a,'b) cmp
end

module type Value = sig
  (** The type that we store when the key is a 'key HeterogeneousKey.t and the map is a 'map Node.t *)
  type ('key,'map) t
end

(* Simple node, with no hash consing. *)
module [@inline] SimpleNode(Key:sig type 'a t end)(Value:Value) = struct
  type 'a key = 'a Key.t
  type ('key,'map) value = ('key,'map) Value.t

  type 'map view =
    | Empty: 'map view
    | Branch: {prefix:intkey;branching_bit:mask;tree0:'map t;tree1:'map t} -> 'map view
    | Leaf: {key:'key key; value:('key,'map) value} -> 'map view
  and 'map t = 'map view
  let view x = x

  let empty = Empty
  let is_empty x = x == Empty
  let leaf key value  = Leaf {key;value}
  let branch ~prefix ~branching_bit ~tree0 ~tree1 =
    match tree0,tree1 with
    | Empty, x -> x
    | x, Empty -> x
    | _ -> Branch{prefix;branching_bit;tree0;tree1}

end

module WeakNode(Key:sig type 'a t end)(Value:Value)(* :Node *) = struct

  type 'a key = 'a Key.t
  type ('key,'map) value = ('key,'map) Value.t

  type 'map view =
    | Empty: 'map view
    | Branch: {prefix:intkey;branching_bit:mask;tree0:'map t;tree1:'map t} -> 'map view
    | Leaf: {key:'key key; value:('key,'map) value} -> 'map view
  and 'a t =
    | TEmpty: 'map t
    | TBranch: {prefix:intkey;branching_bit:mask;tree0:'map t;tree1:'map t} -> 'map t
    (* Additional hidden case: leaf, which is an Ephemeron.K1, whose
       tag is 251, so it can be discriminated against the other
       cases. This avoids an indirection. *)

  let empty = TEmpty
  let is_empty x = x == TEmpty
  let leaf key value = Obj.magic (Ephemeron.K1.make key value)
  let branch ~prefix ~branching_bit ~tree0 ~tree1 =
    match tree0,tree1 with
    | TEmpty, x -> x
    | x, TEmpty -> x
    | _ -> TBranch{prefix;branching_bit;tree0;tree1}

  let view (type k) (type map) (t:map t) =
    let obj = Obj.repr t in
    if Obj.is_block obj && Obj.tag obj != 0 then
      (* Ephemeron.K1.get_(key|value) are no longer available in 5.0,
         so we do that instead. *)
      let ephe:Obj.Ephemeron.t = Obj.magic obj in
      let key:k key option = Obj.magic @@ Obj.Ephemeron.get_key ephe 0 in
      let data:(k,map) Value.t option = Obj.magic @@ Obj.Ephemeron.get_data ephe in
      match key,data with
      | Some key, Some value -> Leaf{key;value}
      | _ -> Empty
    else match t with
    | TEmpty -> Empty
    | TBranch{prefix;branching_bit;tree0;tree1} -> Branch{prefix;branching_bit;tree0;tree1}

end


(** Add a unique id to nodes, e.g. so that they can be used as keys in maps or sets.  *)
module NodeWithId(Key:sig type 'a t end)(Value:Value):NodeWithId
  with type 'key key = 'key Key.t
   and type ('key,'map) value = ('key,'map) Value.t
= struct

  type 'a key = 'a Key.t
  type ('key,'map) value = ('key,'map) Value.t

  type 'map view =
    | Empty: 'map view
    | Branch: {prefix:intkey;branching_bit:mask;tree0:'map t;tree1:'map t} -> 'map view
    | Leaf: {key:'key key; value:('key,'map) value} -> 'map view
  and 'map t =
    | NEmpty: 'map t
    | NBranch: {prefix:intkey;branching_bit:mask;tree0:'map t;tree1:'map t;id:int} -> 'map t
    | NLeaf: {key:'key key;value:('key,'map) value;id:int} -> 'map t

  let view = function
    | NEmpty -> Empty
    | NBranch{prefix;branching_bit;tree0;tree1} -> Branch{prefix;branching_bit;tree0;tree1}
    | NLeaf{key;value} -> Leaf{key;value}

  let get_id = function
    | NEmpty -> 0
    | NBranch{id} -> id
    | NLeaf{id} -> id

  let count = ref 0;;

  let empty = NEmpty
  let is_empty x = x == NEmpty
  let leaf key value = incr count; NLeaf {key;value;id=(!count)}
  let branch ~prefix ~branching_bit ~tree0 ~tree1 =
    match tree0,tree1 with
    | NEmpty, x -> x
    | x, NEmpty -> x
    | _ -> NBranch{prefix;branching_bit;tree0;tree1;id=(!count)}

end



(** Node for sets, i.e. when there is no associated values.  *)
module SetNode(Key:sig type 'a t end):Node
  with type 'key key = 'key Key.t
   and type ('key,'map) value = unit
= struct

  type 'a key = 'a Key.t
  type ('key,'map) value = unit

  type 'map view =
    | Empty: 'map view
    | Branch: {prefix:intkey;branching_bit:mask;tree0:'map t;tree1:'map t} -> 'map view
    | Leaf: {key:'key key; value:('key,'map) value} -> 'map view
  and 'map t =
    | NEmpty: 'map t
    | NBranch: {prefix:intkey;branching_bit:mask;tree0:'map t;tree1:'map t} -> 'map t
    | NLeaf: {key:'key key} -> 'map t


  let view = function
    | NEmpty -> Empty
    | NBranch{prefix;branching_bit;tree0;tree1} -> Branch{prefix;branching_bit;tree0;tree1}
    | NLeaf{key} -> Leaf{key;value=()}

  let empty = NEmpty
  let is_empty x = x == NEmpty
  let leaf key value  = NLeaf {key}
  let branch ~prefix ~branching_bit ~tree0 ~tree1 =
    match tree0,tree1 with
    | NEmpty, x -> x
    | x, NEmpty -> x
    | _ -> NBranch{prefix;branching_bit;tree0;tree1}

end

module WeakSetNode(Key:sig type 'a t end)(* :Node *) = struct

  type 'a key = 'a Key.t
  type ('key,'map) value = unit

  type 'map view =
    | Empty: 'map view
    | Branch: {prefix:intkey;branching_bit:mask;tree0:'map t;tree1:'map t} -> 'map view
    | Leaf: {key:'key key; value:('key,'map) value} -> 'map view
  and 'a t =
    | TEmpty: 'map t
    | TBranch: {prefix:intkey;branching_bit:mask;tree0:'map t;tree1:'map t} -> 'map t
    (* Additional hidden case: leaf, which is a Weak array, whose tag
       is 251, so it can be discriminated against the other
       cases. This avoids an indirection. *)

  let empty = TEmpty
  let is_empty x = x == TEmpty
  let leaf key () = Obj.magic (let a = Weak.create 1 in Weak.set a 0 (Some key))
  let branch ~prefix ~branching_bit ~tree0 ~tree1 =
    match tree0,tree1 with
    | TEmpty, x -> x
    | x, TEmpty -> x
    | _ -> TBranch{prefix;branching_bit;tree0;tree1}

  let view t =
    let obj = Obj.repr t in
    if Obj.is_block obj && Obj.tag obj != 0 then
      let weak = Obj.magic obj in
      let key = Weak.get weak 0 in
      match key with
      | Some key -> Leaf{key;value=()}
      | _ -> Empty
    else  match t with          (* Identity in memory. *)
    | TEmpty -> Empty
    | TBranch{prefix;branching_bit;tree0;tree1} -> Branch{prefix;branching_bit;tree0;tree1}

end

module MakeCustom
    (Key:HeterogeneousKey)
    (Value:Value)
    (Node:Node with type 'a key = 'a Key.t and type ('key,'map) value = ('key,'map) Value.t)(* : *)
  (* HeterogeneousMap_S with type 'a key = 'a Key.t *)
  (*                     and type ('key,'map) value = ('key,'map) Value.t *)
  (*                     and type 'a t = 'a Node.t *)
= struct

  (* We provide two versions: with or without hash-consing. Hash-consing
     allows faster implementations for the fold_on_diff* operations.
     Benchmarks seems to indicate that hashconsing and the more complex
     fold_on_diff are not very useful in practice (perhaps they would on
     huge structures?) *)

  (* With hash-consing of interior nodes: slower node construction, but
     faster comparison with fold_on_diff. *)

  (* module Node = TNoHashCons;; *)
  include Node

  type 'map key_value_pair = KeyValue: 'a Key.t * ('a,'map) value -> 'map key_value_pair
  let rec min_binding (type a) x = match Node.view x with
    | Empty -> raise Not_found
    | Leaf{key;value} -> KeyValue(key,value)
    | Branch{tree0} -> min_binding tree0
  let rec max_binding (type a) x = match Node.view x with
    | Empty -> raise Not_found
    | Leaf{key;value} -> KeyValue(key,value)
    | Branch{tree1} -> max_binding tree1


  (* Merge trees whose prefix disagree. *)
  let join pa treea pb treeb =
    (* Printf.printf "join %d %d\n" pa pb; *)
    let m = branching_bit pa pb in
    let p = mask pa (* for instance *) m in
    if (pa land m) = 0 then
      branch ~prefix:p ~branching_bit:m ~tree0:treea ~tree1:treeb
    else
      branch ~prefix:p ~branching_bit:m ~tree0:treeb ~tree1:treea
  ;;


  (* [match_prefix k p m] returns [true] if and only if the key [k] has prefix [p] up to bit [m]. *)
  let match_prefix k p m =
    mask k m = p
  ;;

  let singleton = leaf

  let rec cardinal m =
    match Node.view m with
    | Empty -> 0
    | Leaf _ -> 1
    | Branch{tree0; tree1; _ } -> cardinal tree0 + cardinal tree1

  let is_singleton m =
    match Node.view m with
    | Leaf{key;value} -> Some (KeyValue(key,value))
    | _ -> None

  let rec findint: type a map. a Key.t -> int -> map t -> (a,map) value =
    fun witness searched m -> match Node.view m with
      | Leaf{key;value} -> begin
          match Key.polyeq key witness with
          | Eq -> value
          | Diff -> raise Not_found
        end
      | Branch{prefix;branching_bit;tree0;tree1} ->
        (* Optional if not (match_prefix searched prefix branching_bit) then raise Not_found
           else *) if (branching_bit land searched == 0)
        then findint witness searched tree0
        else findint witness searched tree1
      | Empty -> raise Not_found
  let find m searched = findint searched (Key.to_int searched) m

  let rec split: type a map. a key -> int -> map t -> map t * ((a,map) value) option * map t =
    fun split_key split_key_int m -> match Node.view m with
      | Leaf{key;value} -> begin
          match Key.polyeq key split_key with
          | Eq -> Node.empty, Some value, Node.empty
          | Diff ->
            if Key.to_int key < split_key_int then
              m, None, Node.empty else Node.empty, None, m
        end
      | Branch{prefix;branching_bit;tree0;tree1} ->
          if not (match_prefix split_key_int prefix branching_bit) then
            if prefix < split_key_int
            then m, None, Node.empty
            else Node.empty, None, m
          else if (branching_bit land split_key_int == 0) then
            let left, found, right = split split_key split_key_int tree0 in
            left, found, Node.branch ~prefix ~branching_bit ~tree0:right ~tree1
          else
            let left, found, right = split split_key split_key_int tree1 in
            Node.branch ~prefix ~branching_bit ~tree0 ~tree1:left, found, right
      | Empty -> Node.empty, None, Node.empty

  let split k m = split k (Key.to_int k) m

  let find_opt m searched = match find m searched with
    | x -> Some x
    | exception Not_found -> None

  let mem m searched =
    match findint searched (Key.to_int searched) m with
    | exception Not_found -> false
    | _ -> true

  type ('map1,'map2) polymap = { f: 'a. 'a Key.t -> ('a,'map1) Value.t -> ('a,'map2) Value.t } [@@unboxed]
  let rec map m (f:('map1,'map1) polymap) = match Node.view m with
    | Empty -> empty
    | Leaf{key;value} ->
      let newval = (f.f key value) in
      if newval == value
      then m
      else leaf key newval
    | Branch{prefix;branching_bit;tree0;tree1} ->
      let newtree0 = map tree0 f in
      let newtree1 = map tree1 f in
      if tree0 == newtree0 && tree1 == newtree1 then m
      else branch ~prefix ~branching_bit ~tree0:newtree0 ~tree1:newtree1
  ;;


  (* MAYBE: A map (and map_filter) homogeneous, that try to preserve physical equality. *)
  let rec map_no_share m (f:('map1,'map2) polymap) = match Node.view m with
    | Empty -> empty
    | Leaf{key;value} -> leaf key (f.f key value)
    | Branch{prefix;branching_bit;tree0;tree1} ->
        let tree0 = map_no_share tree0 f in
        let tree1 = map_no_share tree1 f in
        branch ~prefix ~branching_bit ~tree0 ~tree1

  type ('map1,'map2) polyfilter_map = { f: 'a. 'a Key.t -> ('a,'map1) Value.t -> ('a,'map2) Value.t option } [@@unboxed]
  let rec filter_map m (f:('map1,'map1) polyfilter_map) = match Node.view m with
    | Empty -> empty
    | Leaf{key;value} ->
      (match f.f key value with
       | None -> empty
       | Some newval -> if newval == value then m else leaf key newval)
    | Branch{prefix;branching_bit;tree0;tree1} ->
      let newtree0 = filter_map tree0 f in
      let newtree1 = filter_map tree1 f in
      if tree0 == newtree0 && tree1 == newtree1 then m
      else branch ~prefix ~branching_bit ~tree0:newtree0 ~tree1:newtree1

  let rec filter_map_no_share m (f:('b,'c) polyfilter_map) = match Node.view m with
    | Empty -> empty
    | Leaf{key;value} -> (match (f.f key value) with Some v -> leaf key v | None -> empty)
    | Branch{prefix;branching_bit;tree0;tree1} ->
        let tree0 = filter_map_no_share tree0 f in
        let tree1 = filter_map_no_share tree1 f in
        branch ~prefix ~branching_bit ~tree0 ~tree1

  type 'map polypretty = { f: 'a. Format.formatter -> 'a Key.t -> ('a, 'map) Value.t -> unit } [@@unboxed]
  let rec pretty ?(pp_sep=Format.pp_print_cut) (f : 'map polypretty) fmt m =
    match Node.view m with
    | Empty -> ()
    | Leaf{key;value} -> (f.f fmt key value)
    | Branch{tree0; tree1; _} ->
        pretty f ~pp_sep fmt tree0;
        pp_sep fmt ();
        pretty f ~pp_sep fmt tree1


  let rec removeint to_remove m = match Node.view m with
    | Leaf{key;value} when (Key.to_int key) == to_remove -> empty
    | (Empty | Leaf _) -> m
    | Branch{prefix;branching_bit;tree0;tree1} ->
      if (branching_bit land to_remove) == 0
      then begin
        let tree0' = removeint to_remove tree0 in
        if tree0' == empty then tree1
        else if tree0' == tree0 then m
        else branch ~prefix ~branching_bit ~tree0:tree0' ~tree1
      end
      else begin
        let tree1' = removeint to_remove tree1 in
        if tree1' == empty then tree0
        else if tree1' == tree1 then m
        else branch ~prefix ~branching_bit ~tree0 ~tree1:tree1'
      end
  ;;
  let remove m to_remove = removeint (Key.to_int to_remove) m

  (* This exception is triggered if an operation cannot be completed
     because a weak key disappeared. Probably the simplest way to deal
     with this operation is to "compact" (remove the dead keys) and
     retry. *)
  exception Disappeared

  let rec pop_min_nonempty m = match Node.view m with
    | Leaf{key;value} -> KeyValue(key,value),empty
    | Branch{prefix;branching_bit;tree0;tree1} ->
      let res,tree0' = pop_min_nonempty tree0 in
      let restree =
        if tree0' == empty then tree1
        else branch ~prefix ~branching_bit ~tree0:tree0' ~tree1
      in (res,restree)
    | Empty ->
      (* Can only happen in weak sets and maps. *)
      raise Disappeared ;;
  let pop_minimum m = match Node.view m with
    | Empty -> None
    | _ -> Some(pop_min_nonempty m)
  ;;

  let rec pop_max_nonempty m = match Node.view m with
    | Leaf{key;value} -> KeyValue(key,value),empty
    | Branch{prefix;branching_bit;tree0;tree1} ->
      let res,tree1' = pop_max_nonempty tree1 in
      let restree =
        if tree1' == empty then tree0
        else branch ~prefix ~branching_bit ~tree0 ~tree1:tree1'
      in (res,restree)
      (* Can only happen in weak sets and maps. *)
    | Empty -> raise Disappeared
 ;;

  let pop_maximum m = match Node.view m with
    | Empty -> None
    | _ -> Some(pop_max_nonempty m)
  ;;

  let insert: type a map. map t -> a Key.t -> ((a,map) Value.t option -> (a,map) Value.t) ->  map t =
    fun t thekey f ->
    let thekeyint = Key.to_int thekey in
    (* Preserve physical equality whenever possible. *)
    let exception Unmodified in
    try
      let rec loop t = match Node.view t with
        | Empty -> leaf thekey (f None)
        | Leaf{key;value=old} ->
          begin match Key.polyeq key thekey with
            | Eq ->
              let newv = f (Some old) in
              if newv == old then raise Unmodified
              else leaf key newv
            | Diff ->
              let keyint = (Key.to_int key) in
              join thekeyint (leaf thekey (f None)) keyint t
          end
        | Branch{prefix;branching_bit;tree0;tree1} ->
          if match_prefix thekeyint prefix branching_bit then
            if (thekeyint land branching_bit) == 0
            then branch ~prefix ~branching_bit ~tree0:(loop tree0) ~tree1
            else branch ~prefix ~branching_bit ~tree0 ~tree1:(loop tree1)
          else join thekeyint (leaf thekey (f None)) prefix t
      in loop t
    with Unmodified -> t
  ;;

  (* XXXX: This is a better update, that can also remove element, depending on how the join between the old and new values goes.
     Can be useful (e.g. when join is top), I should export that, maybe replace insert with it. *)
  (* TODO: Test. *)
  let update: type a map. map t -> a Key.t -> ((a,map) Value.t option -> (a,map) Value.t option) ->  map t =
    fun t thekey f ->
    let thekeyint = Key.to_int thekey in
    (* Preserve physical equality whenever possible. *)
    let exception Unmodified in
    try
      let rec loop t = match Node.view t with
        | Empty -> begin
            match (f None) with
            | None -> raise Unmodified
            | Some v -> leaf thekey v
          end
        | Leaf{key;value=old} ->
          begin match Key.polyeq key thekey with
            | Eq ->
              let newv = f (Some old) in
              begin match newv with
                | None -> empty
                | Some newv when newv == old -> raise Unmodified
                | Some newv -> leaf key newv end
            | Diff ->
              let keyint = (Key.to_int key) in
              begin match f None with
                | None -> raise Unmodified
                | Some value -> join thekeyint (leaf thekey value) keyint t
              end
          end
        | Branch{prefix;branching_bit;tree0;tree1} ->
          if match_prefix thekeyint prefix branching_bit then
            if (thekeyint land branching_bit) == 0
            then branch ~prefix ~branching_bit ~tree0:(loop tree0) ~tree1
            else branch ~prefix ~branching_bit ~tree0 ~tree1:(loop tree1)
          else begin match f None with
            | None -> raise Unmodified
            | Some value -> join thekeyint (leaf thekey value) prefix t
          end
      in loop t
    with Unmodified -> t


  (* Note: Insert is a bit weird, I am not sure it should be exported. *)
  type 'map polyinsert = { f: 'a . key:'a Key.t -> old:('a,'map) Value.t -> value:('a,'map) Value.t -> ('a,'map) Value.t } [@@unboxed]
  let insert_for_union: type a map. map polyinsert -> a Key.t -> (a,map) Value.t -> map t -> map t =
    fun f thekey value t ->
    let thekeyint = Key.to_int thekey in
    (* Preserve physical equality whenever possible. *)
    let exception Unmodified in
    try
      let rec loop t = match Node.view t with
        | Empty -> leaf thekey value
        | Leaf{key;value=old} ->
          begin match Key.polyeq key thekey with
            | Eq ->
              if value == old then raise Unmodified else
              let newv = f.f ~key ~old ~value in
              if newv == old then raise Unmodified
              else leaf key newv
            | Diff ->
              let keyint = (Key.to_int key) in
              join thekeyint (leaf thekey value) keyint t
          end
        | Branch{prefix;branching_bit;tree0;tree1} ->
          if match_prefix thekeyint prefix branching_bit then
            if (thekeyint land branching_bit) == 0
            then branch ~prefix ~branching_bit ~tree0:(loop tree0) ~tree1
            else branch ~prefix ~branching_bit ~tree0 ~tree1:(loop tree1)
          else join thekeyint (leaf thekey value) prefix t
      in loop t
    with Unmodified -> t
  ;;

  let add t key value = insert t key (fun _ -> value);;

  type ('map1,'map2) polysame_domain_for_all2 = { f: 'a 'b. 'a Key.t -> ('a,'map1) Value.t -> ('a,'map2) Value.t -> bool } [@@unboxed]
  (* Fast equality test between two maps. *)
  let rec reflexive_same_domain_for_all2 ta tb f = match (Node.view ta),(Node.view tb) with
    | _ when ta == tb -> true (* Skip same subtrees thanks to reflexivity. *)
    | Empty, _ | _, Empty -> false
    | Leaf _, Branch _ | Branch _, Leaf _ -> false
    | Leaf{key=keya;value=valuea}, Leaf{key=keyb;value=valueb} ->
      begin match Key.polyeq keya keyb with
        | Diff -> false
        | Eq -> f.f keya valuea valueb
      end
    | Branch{prefix=pa;branching_bit=ma;tree0=ta0;tree1=ta1},
      Branch{prefix=pb;branching_bit=mb;tree0=tb0;tree1=tb1} ->
      pa == pb && ma == mb && reflexive_same_domain_for_all2 ta0 tb0 f && reflexive_same_domain_for_all2 ta1 tb1 f

  let rec nonreflexive_same_domain_for_all2 ta tb f = match (Node.view ta),(Node.view tb) with
    | Empty, _ | _, Empty -> false
    | Leaf _, Branch _ | Branch _, Leaf _ -> false
    | Leaf{key=keya;value=valuea}, Leaf{key=keyb;value=valueb} ->
      begin match Key.polyeq keya keyb with
        | Diff -> false
        | Eq -> f.f keya valuea valueb
      end
    | Branch{prefix=pa;branching_bit=ma;tree0=ta0;tree1=ta1},
      Branch{prefix=pb;branching_bit=mb;tree0=tb0;tree1=tb1} ->
      pa == pb && ma == mb && nonreflexive_same_domain_for_all2 ta0 tb0 f && nonreflexive_same_domain_for_all2 ta1 tb1 f
  ;;

  let rec reflexive_subset_domain_for_all2 ta tb f = match (Node.view ta),(Node.view tb) with
    | _ when ta == tb -> true   (* Skip same subtrees thanks to reflexivity. *)
    | Empty, _ -> true
    | _, Empty -> false
    | Branch _, Leaf _ -> false
    | Leaf {key=keya;value=valuea}, viewb ->
      (* Reimplement find locally, mostly because of typing issues
         (which could be solved if we had a version of find that
         returns a (key,value) pair. *)
      let searched = Key.to_int keya in
      let rec search = function
        | Leaf{key=keyb;value=valueb} ->
          begin match Key.polyeq keya keyb with
            | Diff -> false
            | Eq -> f.f keya valuea valueb
          end
        | Branch{prefix;branching_bit;tree0;tree1} ->
          if (branching_bit land searched == 0)
          then search (Node.view tree0)
          else search (Node.view tree1)
        | Empty -> assert false (* We already saw that tb is not empty. *)
      in search viewb
    | Branch{prefix=pa;branching_bit=ma;tree0=ta0;tree1=ta1},
      Branch{prefix=pb;branching_bit=mb;tree0=tb0;tree1=tb1} ->
      if ma == mb && pa == pb
      (* Same prefix: divide the search. *)
      then
        (reflexive_subset_domain_for_all2 ta0 tb0 f) &&
        (reflexive_subset_domain_for_all2 ta1 tb1 f)
        (* Case where ta have to be included in one of tb0 or tb1. *)
      else if ma < mb && match_prefix pa pb mb
      then if mb land pa == 0
        then reflexive_subset_domain_for_all2 ta tb0 f
        else reflexive_subset_domain_for_all2 ta tb1 f
        (* Any other case: there are elements in ta that are unmatched in tb. *)
      else false
  ;;



  type ('map1,'map2,'map3) polyunion = { f: 'a. 'a Key.t -> ('a,'map1) Value.t -> ('a,'map2) Value.t -> ('a,'map3) Value.t } [@@unboxed]
  let rec idempotent_union ta tb f =
    if ta == tb then ta
    else
      match Node.view ta,Node.view tb with
      | Empty, _ -> tb
      | x, Empty -> ta
      | Leaf{key;value},_ -> insert_for_union ({f=fun ~key ~old ~value -> f.f key value old}) key value tb
      | _,Leaf{key;value} -> insert_for_union ({f=fun ~key ~old ~value -> f.f key old value}) key value ta
      | Branch{prefix=pa;branching_bit=ma;tree0=ta0;tree1=ta1},
        Branch{prefix=pb;branching_bit=mb;tree0=tb0;tree1=tb1} ->
        if ma == mb && pa == pb
        (* Same prefix: merge the subtrees *)
        then
          (* MAYBE: if ta0 == tb0 and ta1 == tb1, we can return ta (or
             tb). Probably not useful. *)
          let tree0 = idempotent_union ta0 tb0 f in
          let tree1 = idempotent_union ta1 tb1 f in
          branch ~prefix:pa ~branching_bit:ma ~tree0 ~tree1
        else if ma > mb && match_prefix pb pa ma
        then if ma land pb == 0
          then branch ~prefix:pa ~branching_bit:ma ~tree0:(idempotent_union ta0 tb f) ~tree1:ta1
          else branch ~prefix:pa ~branching_bit:ma ~tree0:ta0 ~tree1:(idempotent_union ta1 tb f)
        else if ma < mb && match_prefix pa pb mb
        then if mb land pa == 0
          then branch ~prefix:pb ~branching_bit:mb ~tree0:(idempotent_union ta tb0 f) ~tree1:tb1
          else branch ~prefix:pb ~branching_bit:mb ~tree0:tb0 ~tree1:(idempotent_union ta tb1 f)
        else join pa ta pb tb
  ;;

  type ('map1,'map2,'map3) polyinter = { f: 'a. 'a Key.t -> ('a,'map1) Value.t -> ('a,'map2) Value.t -> ('a,'map3) Value.t } [@@unboxed]
  let rec idempotent_inter ta tb f =
    if ta == tb then ta
    else match Node.view ta,Node.view tb with
      | Empty, _ | _, Empty -> empty
      | Leaf{key;value},_ ->
        (try let res = find tb key in
            if res == value then ta else
            let newval = f.f key value res in
            if newval == value then ta else
            leaf key newval
         with Not_found -> empty)
      | _,Leaf{key;value} ->
        (try let res = find ta key in
            if res == value then tb else
            let newval = f.f key value res in
            if newval == value then tb else
            leaf key newval
         with Not_found -> empty)
      | Branch{prefix=pa;branching_bit=ma;tree0=ta0;tree1=ta1},
        Branch{prefix=pb;branching_bit=mb;tree0=tb0;tree1=tb1} ->
        if ma == mb && pa == pb
        (* Same prefix: merge the subtrees *)
        then
          let tree0 = idempotent_inter ta0 tb0 f in
          let tree1 = idempotent_inter ta1 tb1 f in
          branch ~prefix:pa ~branching_bit:ma ~tree0 ~tree1
        else if ma > mb && match_prefix pb pa ma
        then if ma land pb == 0
          then idempotent_inter ta0 tb f
          else idempotent_inter ta1 tb f
        else if ma < mb && match_prefix pa pb mb
        then if mb land pa == 0
          then idempotent_inter ta tb0 f
          else idempotent_inter ta tb1 f
        else empty

  (* Same as above, without the same subtree optimisation. *)
  let rec nonidempotent_inter_no_share ta tb f =
    match Node.view ta,Node.view tb with
    | Empty, _ | _, Empty -> empty
    | Leaf{key;value},_ ->
      (try let res = find tb key in
         leaf key (f.f key value res)
       with Not_found -> empty)
    | _,Leaf{key;value} ->
      (try let res = find ta key in
         leaf key (f.f key res value)
       with Not_found -> empty)
    | Branch{prefix=pa;branching_bit=ma;tree0=ta0;tree1=ta1},
      Branch{prefix=pb;branching_bit=mb;tree0=tb0;tree1=tb1} ->
      if ma == mb && pa == pb
      (* Same prefix: merge the subtrees *)
      then
        let tree0 = nonidempotent_inter_no_share ta0 tb0 f in
        let tree1 = nonidempotent_inter_no_share ta1 tb1 f in
        branch ~prefix:pa ~branching_bit:ma ~tree0 ~tree1
      else if ma > mb && match_prefix pb pa ma
      then if ma land pb == 0
        then nonidempotent_inter_no_share ta0 tb f
        else nonidempotent_inter_no_share ta1 tb f
      else if ma < mb && match_prefix pa pb mb
      then if mb land pa == 0
        then nonidempotent_inter_no_share ta tb0 f
        else nonidempotent_inter_no_share ta tb1 f
      else empty

  type ('map1,'map2,'map3) polyinterfilter = { f: 'a. 'a Key.t -> ('a,'map1) Value.t -> ('a,'map2) Value.t -> ('a,'map3) Value.t option } [@@unboxed]
  let rec idempotent_inter_filter ta tb f =
    if ta == tb then ta
    else match Node.view ta,Node.view tb with
      | Empty, _ | _, Empty -> empty
      | Leaf{key;value},_ ->
        (try let res = find tb key in
           if res == value then ta else
           match (f.f key value res) with
           | Some v when v == value -> ta
           | Some v -> leaf key v
           | None -> empty
         with Not_found -> empty)
      | _,Leaf{key;value} ->
        (try let res = find ta key in
           if res == value then tb else
           match f.f key res value with
           | Some v when v == value -> tb
           | Some v -> leaf key v
           | None -> empty
         with Not_found -> empty)
      | Branch{prefix=pa;branching_bit=ma;tree0=ta0;tree1=ta1},
        Branch{prefix=pb;branching_bit=mb;tree0=tb0;tree1=tb1} ->
        if ma == mb && pa == pb
        (* Same prefix: merge the subtrees *)
        then
          let tree0 = idempotent_inter_filter ta0 tb0 f in
          let tree1 = idempotent_inter_filter ta1 tb1 f in
          branch ~prefix:pa ~branching_bit:ma ~tree0 ~tree1
        else if ma > mb && match_prefix pb pa ma
        then if ma land pb == 0
          then idempotent_inter_filter ta0 tb f
          else idempotent_inter_filter ta1 tb f
        else if ma < mb && match_prefix pa pb mb
        then if mb land pa == 0
          then idempotent_inter_filter ta tb0 f
          else idempotent_inter_filter ta tb1 f
        else empty

  type ('map1,'map2,'map3) polymerge = { f: 'a. 'a Key.t -> ('a,'map1) Value.t option -> ('a,'map2) Value.t option -> ('a,'map3) Value.t option } [@@unboxed]
  let rec slow_merge: type mapa mapb mapc. mapa Node.t -> mapb Node.t -> (mapa,mapb,mapc) polymerge -> mapc Node. t=
    fun ta tb f ->
    let upd_ta ta = filter_map_no_share ta {f=fun key value -> f.f key (Some value) None} in
    let upd_tb tb = filter_map_no_share tb {f=fun key value -> f.f key None (Some value)} in
    let oldf = f in
    match Node.view ta,Node.view tb with
    | Empty, _ -> upd_tb tb
    | x, Empty -> upd_ta ta
    | Leaf{key;value},_ ->
      let found = ref false in
      let f: type a. a Key.t -> (a,mapb) Value.t -> (a,mapc) Value.t option = fun curkey curvalue ->
        match Key.polyeq curkey key with
        | Eq -> found:= true; f.f key (Some value) (Some curvalue)
        | Diff -> f.f curkey None (Some curvalue) in
      let res = filter_map_no_share tb {f} in
      (* If the key of the leaf is not present, add it back.  Note
         that it breaks the assumption that merge is done in ascending
         number of keys; if we wanted that, we would need a
         "filter_map_no_share_add_key" function.  *)
      if !found then res
      else begin
        match oldf.f key (Some value) None with
        | None -> res
        | Some value -> add res key value
      end
    | _, Leaf{key;value} ->
      let found = ref false in
      let f: type a. a Key.t -> (a,mapa) Value.t -> (a,mapc) Value.t option = fun curkey curvalue ->
        match Key.polyeq curkey key with
        | Eq -> found := true; f.f key (Some curvalue) (Some value)
        | Diff -> f.f curkey (Some curvalue) None in
      let res = filter_map_no_share ta {f} in
      if !found then res
      else begin
        match oldf.f key None (Some value) with
        | None -> res
        | Some value -> add res key value
      end
    | Branch{prefix=pa;branching_bit=ma;tree0=ta0;tree1=ta1},
      Branch{prefix=pb;branching_bit=mb;tree0=tb0;tree1=tb1} ->
      if ma == mb && pa == pb
      (* Same prefix: merge the subtrees *)
      then
        branch ~prefix:pa ~branching_bit:ma ~tree0:(slow_merge ta0 tb0 f) ~tree1:(slow_merge ta1 tb1 f)
      else if ma > mb && match_prefix pb pa ma
      then if ma land pb == 0
        then branch ~prefix:pa ~branching_bit:ma ~tree0:(slow_merge ta0 tb f) ~tree1:(upd_ta ta1)
        else branch ~prefix:pa ~branching_bit:ma ~tree0:(upd_ta ta0) ~tree1:(slow_merge ta1 tb f)
      else if ma < mb && match_prefix pa pb mb
      then if mb land pa == 0
        then branch ~prefix:pb ~branching_bit:mb ~tree0:(slow_merge ta tb0 f) ~tree1:(upd_tb tb1)
        else branch ~prefix:pb ~branching_bit:mb ~tree0:(upd_tb tb0) ~tree1:(slow_merge ta tb1 f)
      else join pa (upd_ta ta) pb (upd_tb tb)
  ;;

  type 'map polyiter = { f: 'a. 'a Key.t -> ('a,'map) Value.t -> unit } [@@unboxed]
  let rec iter x f = match Node.view x with
    | Empty -> ()
    | Leaf{key;value} -> f.f key value
    | Branch{tree0;tree1} -> iter tree0 f; iter tree1 f

  type ('acc,'map) polyfold = { f: 'a. 'a Key.t -> ('a,'map) Value.t -> 'acc -> 'acc } [@@unboxed]
  let rec fold m acc f = match Node.view m with
    | Empty -> acc
    | Leaf{key;value} -> f.f key value acc
    | Branch{tree0;tree1} ->
      let acc = fold tree0 acc f in
      let acc = fold tree1 acc f in
      acc

  type 'map polypredicate = { f: 'a. 'a key -> ('a,'map) value -> bool; } [@@unboxed]
  let filter m f = filter_map m {f = fun k v -> if f.f k v then Some v else None }
  let rec for_all m f = match Node.view m with
    | Empty -> true
    | Leaf{key;value} -> f.f key value
    | Branch{tree0; tree1; _ } -> for_all tree0 f && for_all tree1 f

  module WithForeign(Map2:BaseMap_S with type 'a key = 'a key) = struct

    (* Intersects the first map with the values of the second map,
       trying to preserve physical equality of the first map whenever
       possible. *)
    type ('map1,'map2) polyinter_foreign = { f: 'a. 'a key -> ('a,'map1) value -> ('a,'map2) Map2.value -> ('a,'map1) value } [@@unboxed]
    let rec nonidempotent_inter ta tb f =
      match Node.view ta,Map2.view tb with
      | Empty, _ | _, Empty -> Node.empty
      | Leaf{key;value},_ ->
        (try let res = Map2.find tb key in
           let newval = (f.f key value res) in
           if newval == value then ta
           else Node.leaf key newval
         with Not_found -> Node.empty)
      | _,Leaf{key;value} ->
        (try let res = find ta key in
           Node.leaf key (f.f key res value)
         with Not_found -> Node.empty)
      | Branch{prefix=pa;branching_bit=ma;tree0=ta0;tree1=ta1},
        Branch{prefix=pb;branching_bit=mb;tree0=tb0;tree1=tb1} ->
        if ma == mb && pa == pb
        (* Same prefix: merge the subtrees *)
        then
          let tree0 = (nonidempotent_inter ta0 tb0 f) in
          let tree1 = (nonidempotent_inter ta1 tb1 f) in
          if(ta0 == tree0 && ta1 == tree1)
          then ta
          else Node.branch ~prefix:pa ~branching_bit:ma ~tree0 ~tree1
        else if ma > mb && match_prefix pb pa ma
        then if ma land pb == 0
          then nonidempotent_inter ta0 tb f
          else nonidempotent_inter ta1 tb f
        else if ma < mb && match_prefix pa pb mb
        then if mb land pa == 0
          then nonidempotent_inter ta tb0 f
          else nonidempotent_inter ta tb1 f
        else Node.empty
    ;;



  type ('map2,'map1) polyfilter_map_foreign = { f: 'a. 'a Key.t -> ('a,'map2) Map2.value -> ('a,'map1) value option } [@@unboxed]
  let rec filter_map_no_share m (f:('b,'c) polyfilter_map_foreign) = match Map2.view m with
    | Empty -> empty
    | Leaf{key;value} -> (match (f.f key value) with Some v -> leaf key v | None -> empty)
    | Branch{prefix;branching_bit;tree0;tree1} ->
        let tree0 = filter_map_no_share tree0 f in
        let tree1 = filter_map_no_share tree1 f in
        branch ~prefix ~branching_bit ~tree0 ~tree1

  (** Add all the bindings in tb to ta (after transformation).  *)
  type ('map1,'map2) polyinsert_multiple = { f: 'a. 'a Key.t -> ('a,'map1) value option -> ('a,'map2) Map2.value -> ('a,'map1) value option } [@@unboxed]
  let rec insert_multiple (ta:'map1 t) (tb:'map2 Map2.t) f =
    let upd_tb tb = filter_map_no_share tb {f=fun key value -> f.f key None value} in
    match Node.view ta,Map2.view tb with
    | Empty, _ -> upd_tb tb
    | x, Empty -> ta
    | _,Leaf{key;value} ->
      update ta key (fun maybeval -> f.f key maybeval value)
    | Leaf{key;value},_ ->
      let found = ref false in
      let f: type a. a key -> (a,'map2) Map2.value -> (a,'map1) value option =
        fun curkey curvalue ->
          match Key.polyeq key curkey with
          | Eq -> found := true; f.f curkey (Some value) curvalue
          | Diff -> f.f curkey None curvalue
      in
      let res = filter_map_no_share tb {f} in
      if !found then res
      else add res key value
    | Branch{prefix=pa;branching_bit=ma;tree0=ta0;tree1=ta1},
      Branch{prefix=pb;branching_bit=mb;tree0=tb0;tree1=tb1} ->
      if ma == mb && pa == pb
      (* Same prefix: merge the subtrees *)
      then
        let tree0 = insert_multiple ta0 tb0 f in
        let tree1 = insert_multiple ta1 tb1 f in
        if tree0 == ta0 && tree1 == ta1 then ta
        else branch ~prefix:pa ~branching_bit:ma ~tree0 ~tree1
      else if ma > mb && match_prefix pb pa ma
      then if ma land pb == 0
        then
          let ta0' = insert_multiple ta0 tb f in
          if ta0' == ta0 then ta
          else branch ~prefix:pa ~branching_bit:ma ~tree0:ta0' ~tree1:ta1
        else
          let ta1' = insert_multiple ta1 tb f in
          if ta1' == ta1 then ta
          else branch ~prefix:pa ~branching_bit:ma ~tree0:ta0 ~tree1:ta1'
      else if ma < mb && match_prefix pa pb mb
      then if mb land pa == 0
        then
          let tree0 = insert_multiple ta tb0 f in
          let tree1 = upd_tb tb1 in
          branch ~prefix:pb ~branching_bit:mb ~tree0 ~tree1
        else
          let tree0 = upd_tb tb0 in
          let tree1 = insert_multiple ta tb1 f in
          branch ~prefix:pb ~branching_bit:mb ~tree0 ~tree1
      else join pa ta pb (upd_tb tb)


  (* Map difference: (possibly) remove from ta elements that are in tb, the other are preserved, no element is added. *)
  type ('map1,'map2) polyremove_multiple = { f: 'a. 'a Key.t -> ('a,'map1) value -> ('a,'map2) Map2.value -> ('a,'map1) value option } [@@unboxed]
  let rec remove_multiple ta tb f =
    match Node.view ta, Map2.view tb with
    | Empty, _ -> ta
    | _, Empty -> ta
    | Leaf{key;value},_ ->
      begin match Map2.find tb key with
      | exception Not_found -> ta
      | foundv -> begin
          match f.f key value foundv with
          | None -> empty
          | Some v when v == value -> ta
          | Some v -> leaf key v
        end
      end
    | _,Leaf{key;value} ->
      update ta key (fun v -> match v with None -> None | Some v -> f.f key v value)
    | Branch{prefix=pa;branching_bit=ma;tree0=ta0;tree1=ta1},
      Branch{prefix=pb;branching_bit=mb;tree0=tb0;tree1=tb1} ->
      if ma == mb && pa == pb
      (* Same prefix: merge the subtrees *)
      then
        let tree0 = remove_multiple ta0 tb0 f in
        let tree1 = remove_multiple ta1 tb1 f in
        if tree0 == ta0 && tree1 == ta1 then ta
        else branch ~prefix:pa ~branching_bit:ma ~tree0 ~tree1
      else if ma > mb && match_prefix pb pa ma
      then if ma land pb == 0
        then
          let ta0' = remove_multiple ta0 tb f in
          if ta0' == ta0 then ta
          else branch ~prefix:pa ~branching_bit:ma ~tree0:ta0' ~tree1:ta1
        else
          let ta1' = remove_multiple ta1 tb f in
          if ta1' == ta1 then ta
          else branch ~prefix:pa ~branching_bit:ma ~tree0:ta0 ~tree1:ta1'
      else if ma < mb && match_prefix pa pb mb
      then if mb land pa == 0
        then remove_multiple ta tb0 f
        else remove_multiple ta tb1 f
      else ta
  end
end






(* TODO: We should make it a functor, so that we can simplify the
   interface for set independently from how it is constructed. *)
module MakeHeterogeneousSet(Key:HeterogeneousKey) : HeterogeneousSet_S
  with type 'a BaseMap.key = 'a Key.t = struct
  module Node = SetNode(Key);;
  module BaseMap = MakeCustom(Key)(struct type ('a,'b) t = unit end)(Node)

  (* No need to differentiate the values. *)
  include BaseMap

  type t = unit BaseMap.t

  type any_key = Any : 'a key -> any_key

  let mem elt set = BaseMap.mem set elt

  (* Note: as add is simpler, without any insertion function needed,
     maybe it is worth reimplementing it. *)
  let [@specialised] add key map = BaseMap.add map key ()
  let singleton elt = singleton elt ()
  let remove elt set = BaseMap.remove set elt
  let is_singleton set = match BaseMap.is_singleton set with
    | None -> None
    | Some(KeyValue(k,())) -> Some(Any(k))

  (* Likewise with union and inter: we do not have to worry about
     reconciling the values here, so we could reimplement if the
     compiler is not smart enough. *)
  let [@specialised] union =
    let f:(unit,unit,unit) BaseMap.polyunion = {f=fun _ () () -> ()} in
    fun sa sb -> BaseMap.idempotent_union sa sb f

  let [@specialised] inter =
    let f:(unit,unit,unit) BaseMap.polyinter = {f=fun _ () () -> ()} in
    fun sa sb -> (BaseMap.idempotent_inter (* [@specialised] *)) sa sb f

  (* TODO: use reflexive_subset_domain_for_all2. *)
  let disjoint sa sb =assert false
  let iter f set = assert false

  (* TODO: A real implementation of fold would be faster. *)
  type 'acc polyfold = { f: 'a. 'a key -> 'acc -> 'acc } [@@unboxed]
  let fold f set acc =
    let f: type a. a key -> unit -> 'acc -> 'acc = fun k () acc -> f.f k acc in
    BaseMap.fold set acc { f }

  let min_elt t = let KeyValue(m, ()) = BaseMap.min_binding t in Any m
  let max_elt t = let KeyValue(m, ()) = BaseMap.max_binding t in Any m

  let [@specialise] f x = 3

  let pop_maximum t = Option.map (fun (KeyValue(m,()),t) -> Any m,t) (BaseMap.pop_maximum t)
  let pop_minimum t = Option.map (fun (KeyValue(m,()),t) -> Any m,t) (BaseMap.pop_minimum t)

  type polypretty = { f: 'a. Format.formatter -> 'a key -> unit; } [@@unboxed]
  let pretty ?pp_sep f fmt s = BaseMap.pretty ?pp_sep { f = fun fmt k () -> f.f fmt k} fmt s

  let equal t1 t2 = BaseMap.reflexive_same_domain_for_all2 t1 t2 {f=fun _ _ _ -> true}
  let subset t1 t2 = BaseMap.reflexive_subset_domain_for_all2 t1 t2 {f=fun _ _ _ -> true}

  let split k m = let (l, present, r) = BaseMap.split k m in
    (l, Option.is_some present, r)

  type polypredicate = { f: 'a. 'a key -> bool; } [@@unboxed]
  let filter f s = BaseMap.filter s {f = fun k () -> f.f k }
  let for_all f s = BaseMap.for_all s {f = fun k () -> f.f k}
end

module MakeHeterogeneousMap(Key:HeterogeneousKey)(Value:Value) =
  MakeCustom(Key)(Value)(SimpleNode(Key)(Value))



module HomogeneousValue = struct
  type ('a,'map) t = 'map
end

module type HomogeneousKey=sig
  type t
  val to_int: t -> int
end

module KeyFromHomogeneousKey(Key:HomogeneousKey):(HeterogeneousKey with type 'a t = Key.t)  = struct
  type 'a t = Key.t
  (** The type-safe way to do it would be to define this type, to
      guarantee that 'a is always bound to the same type, and Eq is
      safe. But this requires a lot of conversion code, and identity
      functions that may not be well detected. [polyeq] is unsafe in
      that it allows arbitrary conversion of t1 by t2 in t1 t, but
      this unsafety is not exported, and I don't think we can do
      something wrong using it. *)
  (* type 'a t = K: Key.t -> unit t [@@unboxed] *)
  let polyeq: type a b. a t -> b t -> (a,b) cmp =
    fun a b -> match a,b with
      | a, b when (Key.to_int a) == (Key.to_int b) -> Obj.magic Eq
      | _ -> Diff
  let to_int = Key.to_int
end

module MakeCustomHomogeneous
    (Key:HomogeneousKey)
    (Node:Node with type 'a key = Key.t and type ('key,'map) value = 'map):
  Map_S with type key = Key.t and type 'a t = 'a Node.t = struct

  module NewKey(* :Key *) = KeyFromHomogeneousKey(Key)

  module BaseMap = MakeCustom(NewKey)(HomogeneousValue)(Node)
  include BaseMap
  type key = Key.t
  let min_binding m = let KeyValue(key,value) = BaseMap.min_binding m in key,value
  let max_binding m = let KeyValue(key,value) = BaseMap.max_binding m in key,value
  (* let singleton k v = BaseMap.singleton (PolyKey.K k) v *)
  let pop_minimum m =
    match BaseMap.pop_minimum m with
    | None -> None
    | Some(KeyValue(key,value),m) -> Some(key,value,m)

  let pop_maximum m =
    match BaseMap.pop_maximum m with
    | None -> None
    | Some(KeyValue(key,value),m) -> Some(key,value,m)

  let is_singleton m = match BaseMap.is_singleton m with
    | None -> None
    | Some(KeyValue(k,v)) -> Some(k,v)

  let filter m f = BaseMap.filter_map m {f=fun k v -> if f k v then Some v else None}
  (* let for_all m (f : key -> 'a -> bool) = BaseMap.for_all m {f} *)
  let map a f = BaseMap.map a {f}
  let map_no_share a f = BaseMap.map_no_share a {f}
  let filter_map a f = BaseMap.filter_map a {f}
  let filter_map_no_share a f = BaseMap.filter_map_no_share a {f}
  let idempotent_union a b f = BaseMap.idempotent_union a b {f}
  let idempotent_inter a b f = BaseMap.idempotent_inter a b {f}
  let nonidempotent_inter_no_share a b f = BaseMap.nonidempotent_inter_no_share a b {f}
  let idempotent_inter_filter a b f = BaseMap.idempotent_inter_filter a b {f}
  let reflexive_same_domain_for_all2 a b f = BaseMap.reflexive_same_domain_for_all2 a b {f}
  let nonreflexive_same_domain_for_all2 a b f = BaseMap.nonreflexive_same_domain_for_all2 a b {f}
  let reflexive_subset_domain_for_all2 a b f = BaseMap.reflexive_subset_domain_for_all2 a b {f}
  let slow_merge a b f = BaseMap.slow_merge a b {f}
  let iter a f = BaseMap.iter a {f}
  let fold m acc f = BaseMap.fold m acc {f}

  let pretty ?pp_sep f fmt m = BaseMap.pretty ?pp_sep {f} fmt m
end

module MakeMap(Key:HomogeneousKey) = struct
  module NewKey = struct type 'a t = Key.t end
  module Node = SimpleNode(NewKey)(HomogeneousValue)
  include MakeCustomHomogeneous(Key)(Node)
end

module MakeSet(Key:HomogeneousKey):Set_S with type _ BaseMap.key = Key.t = struct
  module HKey = KeyFromHomogeneousKey(Key)
  module S = MakeHeterogeneousSet(HKey)
  include S
  type key = Key.t

  let fold (f: key -> 'acc -> 'acc) set acc = S.fold {f} set acc
  let filter (f: key -> bool) set = S.filter {f} set
  let for_all (f: key -> bool) set = S.for_all {f} set

  let pretty ?pp_sep (f : Format.formatter -> key -> unit) fmt s =
    S.pretty ?pp_sep {f} fmt s

  let is_singleton m = match BaseMap.is_singleton m with
    | None -> None
    | Some(KeyValue(k,())) -> Some k

  let min_elt t = let Any x = min_elt t in x
  let max_elt t = let Any x = max_elt t in x
  let pop_minimum t = Option.map (fun (Any x, t) -> (x,t)) (pop_minimum t)
  let pop_maximum t = Option.map (fun (Any x, t) -> (x,t)) (pop_maximum t)
end
