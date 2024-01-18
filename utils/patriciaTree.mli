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

(** Association maps from key to values, and sets, implemented with
    Patricia Trees, allowing fast merge operations by making use of
    physical equality between subtrees; and custom implementation of
    tree nodes (allowing normal maps, hash-consed maps, weak key or
    value maps, sets, custom maps, etc.)

    This is similar to OCaml's Map, except that:

    - The required signature for keys is different, in that we require
      each key to be mapped to a unique integer identifier.

    - The implementation uses Patricia Tree, as described in Oksasaki
      and Gill's 1998 paper "Fast mergeable integer maps", i.e. it is a
      space-efficient prefix trie over the big-endian representation of
      the

      The main benefit of Patricia Tree is that their representation
      is stable (contrary to maps, inserting nodes in any order will
      return the same shape), which allows different versions of a map
      to share more subtrees in memory, and the operations over two
      maps to benefit from this sharing. The functions in this library
      attempt to maximally preserve sharing and benefit from sharing,
      allowing very important improvements in complexity and running
      time when combining maps or sets is a frequent operation.

   - Finally, the implementation is more customizable, allowing
     notably (key,value) pairs or different types to be in the same map,
     or to choose the memory representation of the nodes of the tree.

   - Some operations like [pop_minimum] and [pop_maximum] make our Set
     suitable as priority queue (but remember that each element in the
     queue must map to a distinct integer). *)

(** Note on complexity: in the following, n represents the size of the
    map when there is one (and [|map1|] is the number of elements in
    [map1]).  The term log(n) correspond to the maximum height of the
    tree, which is log(n) if we assume an even distribution of numbers
    in the map (e.g. random distribution, or integers chosen
    contiguously using a counter). The worst-case height is
    O(max(n,64)) which is actually constant, but not really
    informative; log(n) corresponds to the real complexity in usual
    distributions. *)


type intkey
type mask

(** This module explains how a node is stored in memory, with
    functions to create and view nodes.

    We use a uniform type ['map view] to pattern match on maps and sets
    The actual types ['map t] can be a bit different from ['map view]
    to allow for more efficient representations, but [view] should be a constant
    time operation for quick conversions. *)
module type Node = sig
  (** {2 Types} *)

  type 'key key
  (** The type of keys. *)

  type ('key, 'map) value
  (** The type of value, which depends on the type of the key and the type of the map. *)

  type 'map t
  (** The type of the map, which is parameterized by a type. *)

  (** {2 Constructors; to build values} *)

  val empty : 'map t
  val leaf : 'key key -> ('key, 'map) value -> 'map t
  val branch :
    prefix:intkey ->
    branching_bit:mask -> tree0:'map t -> tree1:'map t -> 'map t

  (** {2 Destructors: access the value} *)

  (** This makes the map nodes accessible to the pattern matching
      algorithm; this corresponds 1:1 to the SimpleNode
      implementation. This just needs to be copy-and-pasted for every
      node type. *)
  type 'map view = private
    | Empty : 'map view
    (** Can happen only at the toplevel: there is no empty interior node. *)
    | Branch : { prefix : intkey; branching_bit : mask;
                 tree0 : 'map t; tree1 : 'map t; } -> 'map view
    (** Branching bit contains only one bit set; the corresponding
        mask is (branching_bit - 1).  The prefixes are normalized: the
        bits below the branching bit are set to zero (i.e. prefix &
        (branching_bit - 1) = 0). *)
    | Leaf : { key : 'key key; value : ('key, 'map) value; } -> 'map view
    (** A key -> value mapping. *)

  val is_empty: 'map t -> bool
  val view: 'a t -> 'a view
  (** Convert the map to a view *)
end

module type NodeWithId = sig
  (* Associate a unique number to each node. *)
  include Node
  val get_id: 'a t -> int
end

module type BaseMap_S = sig
  include Node

  type 'map key_value_pair =
      KeyValue : 'a key * ('a, 'map) value -> 'map key_value_pair
  val min_binding : 'a t -> 'a key_value_pair
  (** @raises [Not_found] if the map is empty *)

  val max_binding : 'a t -> 'a key_value_pair
  (** @raises [Not_found] if the map is empty *)

  val singleton : 'a key -> ('a, 'b) value -> 'b t

  val cardinal : 'a t -> int
  (** The size of the map *)

  val is_singleton : 'a t -> 'a key_value_pair option
  (** [is_singleton m] returns [Some(KeyValue(k,v))] if and only if
      [m] contains a unique binding [k->v]. *)

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
  (** [split key map] splits the map into:
      - submap of [map] whose keys are smaller than [key]
      - value associated to [key] (if present)
      - submap of [map] whose keys are bigger than [key]
      Where the order is given by [Key.to_int]. *)

  type 'map polyiter = { f : 'a. 'a key -> ('a, 'map) value -> unit; } [@@unboxed]
  val iter : 'map t -> 'map polyiter -> unit
  (** [iter m f] calls [f.f] on all bindings of [m], in the order given by [Key.to_int] *)

  type ('acc,'map) polyfold = { f: 'a. 'a key -> ('a,'map) value -> 'acc -> 'acc } [@@unboxed]
  val fold : 'map t -> 'acc -> ('acc,'map) polyfold -> 'acc
  (** [fold m f acc] returns [f.f key_n value_n (... (f.f key_1 value_1 acc))]
      where [(key_1, value_1) ... (key_n, value_n)] are the bindings of [m], in
      the order given by [Key.to_int]. *)

  type 'map polypredicate = { f: 'a. 'a key -> ('a,'map) value -> bool; } [@@unboxed]
  val filter : 'map t -> 'map polypredicate -> 'map t
  (** [filter m f] returns the submap of [m] containing the bindings [k->v]
      such that [f.f k v = true].
      [f.f] is called in the order given by [Key.to_int] *)

  val for_all : 'map t -> 'map polypredicate -> bool
  (** [for_all m f] checks that [f] holds on all bindings of [m]7
      Short-circuiting. *)

  (** In the following, the *no_share function allows taking arguments
      of different types (but cannot share subtrees of the map), while
      the default functions attempt to preserve and benefit from
      sharing the subtrees (using physical equality to detect
      sharing). *)

  type ('map1,'map2) polymap =
    { f : 'a. 'a key -> ('a, 'map1) value -> ('a, 'map2) value; } [@@unboxed]
  val map : 'map t -> ('map,'map) polymap -> 'map t
  val map_no_share : 'map1 t -> ('map1,'map2) polymap -> 'map2 t
  (** [map m f] and [map_no_share m f] replace all bindings [(k,v)] by [(k, f.f k v)].
      Bindings are examined in the order given by [Key.to_int]. *)

  type ('map1,'map2) polyfilter_map =
    { f : 'a. 'a key -> ('a, 'map1) value -> ('a, 'map2) value option; } [@@unboxed]
  val filter_map : 'map t -> ('map,'map) polyfilter_map -> 'map t
  val filter_map_no_share : 'map1 t -> ('map1,'map2) polyfilter_map -> 'map2 t
  (** [filter_map m f] and [filter_map_no_share m f] remove the bindings
      [(k,v)] for which [f.f k v] is [None], and replaces the bindings [(k,v)]
      for which [f.f k v] is [Some v'] by [(k,v')].
      Bindings are examined in the order given by [Key.to_int]. *)

  type 'map polypretty = { f: 'a. Format.formatter -> 'a key -> ('a, 'map) value -> unit } [@@unboxed]
  val pretty :
    ?pp_sep:(Format.formatter -> unit -> unit) -> 'map polypretty ->
    Format.formatter -> 'map t -> unit
  (** Pretty-prints a map using the given formatter.
      [pp_sep] is called once between each binding,
      it defaults to [Format.pp_print_cut].
      Bindings are printed in the order given by [Key.to_int] *)

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
  (** [idempotent_union map1 map2 f] returns a map whose keys is the
      union of the keys of [map1] and [map2]. [f.f] is used to combine
      the values of keys mapped in both maps.
      @assumes [f.f] idempotent (i.e. [f key value value == value])
      [f.f] is called in the order given by [Key.to_int].
      [f.f] is never called on physically equal values.
      Preserves physical equality as much as possible.
      Complexity is O(log(n)*Delta) where Delta is the number of
      different keys between [map1] and [map2]. *)

  type ('map1, 'map2, 'map3) polyinter = {
    f : 'a. 'a key -> ('a, 'map1) value -> ('a, 'map2) value -> ('a, 'map3) value;
  } [@@unboxed]
  val idempotent_inter : 'a t -> 'a t -> ('a, 'a, 'a) polyinter -> 'a t
  (** [idempotent_inter map1 map2 f] returns a map whose keys is the
      intersection of the keys of [map1] and [map2]. [f.f] is used to combine
      the values a key is mapped in both maps.
      @assumes [f.f] idempotent (i.e. [f key value value == value])
      [f.f] is called in the order given by [Key.to_int].
      [f.f] is never called on physically equal values.
      Preserves physical equality as much as possible.
      Complexity is O(log(n)*Delta) where Delta is the number of
      different keys between [map1] and [map2]. *)

  val nonidempotent_inter_no_share : 'a t -> 'b t -> ('a, 'b, 'c) polyinter -> 'c t
  (** [nonidempotent_inter_no_share map1 map2 f] is the same as [idempotent_inter]
      but doesn't preverse physical equality, doesn't assume [f.f] is idempotent,
      and can change the type of values. [f.f] is called on every shared binding.
      [f.f] is called in increasing order of keys.
      O(n) complexity *)


  type ('map1, 'map2, 'map3) polyinterfilter = { f : 'a. 'a key -> ('a, 'map1) value -> ('a, 'map2) value -> ('a, 'map3) value option; } [@@unboxed]
  val idempotent_inter_filter : 'a t -> 'a t -> ('a, 'a, 'a) polyinterfilter -> 'a t
  (** [idempotent_inter_filter map1 map2 f] is the same as [idempotent_inter]
      but [f.f] can return [None] to remove a binding from the resutling map. *)

  type ('map1, 'map2, 'map3) polymerge = {
    f : 'a. 'a key -> ('a, 'map1) value option -> ('a, 'map2) value option -> ('a, 'map3) value  option; } [@@unboxed]
  val slow_merge : 'map1 t -> 'map2 t -> ('map1, 'map2, 'map3) polymerge -> 'map3 t
end

(** Signature for sets implemented using Patricia trees. *)
module type Set_S = sig
  (** Underlying basemap, for cross map/set operations *)
  module BaseMap : BaseMap_S
    with type (_,_) value = unit

  (** This part of the interface should be a subset of Set.S *)
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
  (** Returns the (key,value) where [Key.to_int key] is minimal (in
      unsigned representation of integers); O(log n) complexity. *)

  val max_elt: t -> key
  (** Returns the (key,value) where [Key.to_int key] is maximal (in
      unsigned representation of integers); O(log n) complexity. *)

  (** The following functions are not in Set.S. *)
  val pop_minimum: t -> (key * t) option
  (** [pop_minimum m] returns [None] if [is_empty m], or [Some(key,m')] where
      [key = min_elt m] and [m' = remove m key]. O(log(n)) complexity. *)

  val pop_maximum: t -> (key * t) option
  (** [pop_maximum m] returns [None] if [is_empty m], or [Some(key,m')] where
      [key = max_elt m] and [m' = remove m key]. O(log(n)) complexity. *)

  val pretty :
    ?pp_sep:(Format.formatter -> unit -> unit) ->
    (Format.formatter -> key -> unit) ->
    Format.formatter -> t -> unit
  (** [pp_sep] defaults to [Format.pp_print_cut] *)

  val equal : t -> t -> bool
  val subset : t -> t -> bool
end

(** The signature for maps with a single type for keys and values. *)
module type Map_S = sig
  type key     (** The type of keys. *)
  type 'a t    (** A map from keys to values of type 'a.  *)

  (** Underlying basemap, for cross map/set operations *)
  module BaseMap : BaseMap_S
   with type 'a t = 'a t
    and type _ key = key
    and type (_,'a) value = 'a

  val empty : 'a t
  (** The empty map. *)

  val is_empty : 'a t -> bool
  (** Test if a map is empty; O(1) complexity. *)

  val min_binding : 'a t -> (key * 'a)
  (** Returns the (key,value) where [Key.to_int key] is minimal (in
      unsigned representation of integers); O(log n) complexity. *)

  val max_binding : 'a t -> (key * 'a)
  (** Returns the (key,value) where [Key.to_int key] is maximal; O(log n) complexity. *)

  val singleton: key -> 'a -> 'a t
  (** [singleton key value] creates a map with a single binding, O(1) complexity.  *)

  val cardinal: 'a t -> int
  (** The size of the map *)

  val is_singleton: 'a t -> (key * 'a) option
  (** [is_singleton m] is [Some (k,v)] iff [m] is [singleton k v] *)

  val find: 'a t -> key -> 'a
  (** Return an element in the map, or raise [Not_found], O(log(n)) complexity. *)

  val find_opt: 'a t -> key -> 'a option
  (** Return an element in the map, or [None], O(log(n)) complexity. *)

  val mem: 'a t -> key -> bool
  (** [mem map key] returns [true] iff [key] is bound in [map], O(log(n)) complexity. *)

  val remove: 'a t -> key -> 'a t
  (** Returns a map with the element removed, O(log(n)) complexity. *)

  val pop_minimum: 'a t -> (key * 'a * 'a t) option
  (** [pop_minimum m] returns [None] if [is_empty m], or [Some(key,value,m')] where
      [(key,value) = min_binding m] and [m' = remove m key]. O(log(n)) complexity. *)

  val pop_maximum: 'a t -> (key * 'a * 'a t) option
  (** [pop_maximum m] returns [None] if [is_empty m], or [Some(key,value,m')] where
      [(key,value) = max_binding m] and [m' = remove m key]. O(log(n)) complexity. *)

  val insert: 'a t -> key -> ('a option -> 'a) -> 'a t
  (** [insert map key f] Modifies or insert an element of the map; f
      takes None if the value was not previously bound, and Some old
      where old is the previously bound value otherwise. The function
      preserves physical equality when possible. O(log(n))
      complexity. *)

  val update: 'a t -> key -> ('a option -> 'a option) -> 'a t
  (** [update map key f] Modifies, insert, or remove an element from
      the map; f takes None if the value was not previously bound, and
      Some old where old is the previously bound value otherwise. The
      function preserves physical equality when possible.It returns
      None if the element should be removed O(log(n)) complexity. *)

  val add: 'a t -> key -> 'a -> 'a t
  (** Unconditionally adds a value in the map (independently from
      whether the old value existed). O(log(n)) complexity. *)

  val split: key -> 'a t -> 'a t * 'a option * 'a t
  (** [split key map] splits the map into:
      - submap of [map] whose keys are smaller than [key]
      - value associated to [key] (if present)
      - submap of [map] whose keys are bigger than [key]
      Where the order is given by [Key.to_int]. *)

  val iter: 'a t -> (key -> 'a -> unit) -> unit
  (** Iterate on each (key,value) pair of the map, in increasing order of keys. *)

  val fold: 'a t -> 'acc -> (key -> 'a -> 'acc -> 'acc) -> 'acc
  (** Fold on each (key,value) pair of the map, in increasing order of keys. *)

  val filter : 'a t -> (key -> 'a -> bool) -> 'a t
  (** Returns the submap containing only the key->value pairs satisfying the
      given predicate. [f] is called in increasing number of keys *)

  (* val for_all : 'a t -> (key -> 'a -> bool) -> bool *)
  (** Returns true if the predicate holds on all map bindings. Short-circuiting *)

  (** In the following, the *no_share function allows taking arguments
      of different types (but cannot share subtrees of the map), while
      the default functions attempt to preserve and benefit from
      sharing the subtrees (using physical equality to detect
      sharing). *)

  val map : 'a t -> (key -> 'a -> 'a) -> 'a t
  (** [map m f] returns a map where the [value] bound to each [key] is
      replaced by [f key value]. The subtrees for which the returned
      value is physically the same (i.e. [f key value == value] for
      all the keys in the subtree) are guaranteed to be physically
      equal to the original subtree. O(n) complexity.
      [f] is called in increasing order of keys. *)

  val map_no_share : 'a t -> (key -> 'a -> 'b) -> 'b t
  (** [map_no_share m f] returns a map where the [value] bound to each
      [key] is replaced by [f key value]. O(n) complexity.
      [f] is called in increasing order of keys. *)

  val filter_map : 'a t -> (key -> 'a -> 'a option) -> 'a t
  (** [filter_map m f] returns a map where the [value] bound to each
      [key] is removed (if [f key value] returns [None]), or is
      replaced by [v] ((if [f key value] returns [Some v]). The
      subtrees for which the returned value is physically the same
      (i.e. [f key value = Some v] with [value == v] for all the keys
      in the subtree) are guaranteed to be physically equal to the
      original subtree. O(n) complexity.
      [f] is called in increasing order of keys. *)

  val filter_map_no_share : 'a t -> (key -> 'a -> 'b option) -> 'b t
  (** [filter_map m f] returns a map where the [value] bound to each
      [key] is removed (if [f key value] returns [None]), or is
      replaced by [v] ((if [f key value] returns [Some v]). O(n)
      complexity.
      [f] is called in increasing order of keys. *)

  (** The following functions combine two maps. It is key for the
        performance, when we have large maps who share common subtrees,
        not to visit the nodes in these subtrees. Hence, we have
        specialized versions of these functions that assume properties
        of the function parameter (reflexive relation, idempotent
        operation, etc.)

        When we cannot enjoy these properties, our functions explicitly
        say so (with a nonreflexive or nonidempotent prefix). The names
        are a bit long, but having these names avoids using an
        ineffective code by default, by forcing to know and choose
        between the fast and slow version.

        It is also important to not visit a subtree when there merging
        this subtree with Empty; hence we provide union and inter
        operations. *)

  val reflexive_same_domain_for_all2: 'a t -> 'a t -> (key -> 'a -> 'a -> bool) -> bool
  (** [reflexive_same_domain_for_all2 map1 map2 f] returns true if
      [map1] and [map2] have the same keys, and [f key value1 value2]
      returns true for each mapping pair of keys. We assume that [f]
      is reflexive (i.e. [f key value value] returns true) to avoid
      visiting physically equal subtrees of [map1] and [map2]. The
      complexity is O(log(n)*Delta) where Delta is the number of
      different keys between [map1] and [map2]. *)

  val nonreflexive_same_domain_for_all2: 'a t -> 'b t -> (key -> 'a -> 'b -> bool) -> bool
  (** [nonreflexive_same_domain_for_all2 map1 map2 f] returns true if
      map1 and map2 have the same keys, and [f key value1 value2]
      returns true for each mapping pair of keys. The complexity is
      O(min(|map1|,|map2|)). *)

  val reflexive_subset_domain_for_all2: 'a t -> 'a t -> (key -> 'a -> 'a -> bool) -> bool
  (** [reflexive_subset_domain_for_all2 map1 map2 f] returns true if
      all the keys of [map1] also are in [map2], and [f key (find map1
      key) (find map2 key)] returns [true] when both keys are present
      in the map. We assume that [f] is reflexive (i.e. [f key value
      value] returns true) to avoid visiting physically equal subtrees
      of [map1] and [map2]. The complexity is O(log(n)*Delta) where
      Delta is the number of different keys between [map1] and
      [map2]. *)

  val idempotent_union : 'a t -> 'a t -> (key -> 'a -> 'a -> 'a) -> 'a t
  (** [idempotent_union map1 map2 f] returns a map whose keys is the
      union of the keys of [map1] and [map2]. [f] is used to combine
      the values a key is mapped in both maps. We assume that [f] is
      idempotent (i.e. [f key value value == value]) to avoid visiting
      physically equal subtrees of [map1] and [map2], and also to
      preserve physical equality of the subtreess in that case.  The
      complexity is O(log(n)*Delta) where Delta is the number of
      different keys between [map1] and [map2].
      [f] is called in increasing order of keys.
      [f] is never called on physically equal values. *)

  val idempotent_inter : 'a t -> 'a t -> (key -> 'a -> 'a -> 'a) -> 'a t
  (** [idempotent_inter map1 map2 f] returns a map whose keys is the
      intersection of the keys of [map1] and [map2]. [f] is used to combine
      the values a key is mapped in both maps. We assume that [f] is
      idempotent (i.e. [f key value value == value]) to avoid visiting
      physically equal subtrees of [map1] and [map2], and also to
      preserve physical equality of the subtrees in that case.  The
      complexity is O(log(n)*Delta) where Delta is the number of
      different keys between [map1] and [map2].
      [f] is called in increasing order of keys.
      [f] is never called on physically equal values. *)

  val nonidempotent_inter_no_share : 'a t -> 'b t -> (key -> 'a -> 'b -> 'c) -> 'c t
  (** [nonidempotent_inter_no_share map1 map2 f] returns a map whose keys is
      the intersection of the keys of [map1] and [map2]. [f] is used
      to combine the values a key is mapped in both maps. [f] does not
      need to be idempotent, which imply that we have to visit
      physically equal subtrees of [map1] and [map2].  The complexity
      is O(log(n)*min(|map1|,|map2|)).
      [f] is called in increasing order of keys.
      [f] is called on every shared binding. *)

  val idempotent_inter_filter : 'a t -> 'a t -> (key -> 'a -> 'a -> 'a option) -> 'a t
  (** [idempotent_inter_filter m1 m2 f] is like [idempotent_inter m1
      m2 f] (assuming idempotence, using and preserving physically
      equal subtrees), but it also removes the key->value bindings for
      which [f] returns [None]. *)

  val slow_merge: 'a t -> 'b t -> (key -> 'a option -> 'b option -> 'c option) -> 'c t
  (** [slow_merge m1 m2 f] returns a map whose keys are a subset of the
      keys of [m1] and [m2].  The [f] function is used to combine
      keys, similarly to the [Map.merge] function.  This funcion has
      to traverse all the bindings in [m1] and [m2]; its complexity is
      O(|m1|+|m2|). Use one of faster functions above if you can. *)


  (* Maybe: WithForeign and WithForeignHeterogeneous.  *)

  (** Combination with other kinds of maps. *)
  module WithForeign(Map2:BaseMap_S with type _ key = key):sig
    (** Like [filter_map_no_share], but takes another map. *)
    type ('b,'c) polyfilter_map_foreign = { f: 'a. key -> ('a,'b) Map2.value -> 'c option } [@@unboxed]
    val filter_map_no_share: 'b Map2.t -> ('b, 'c) polyfilter_map_foreign -> 'c t

    (** Like [nonidempotent_inter], but takes another map as an argument. *)
    type ('value,'map2) polyinter_foreign =
      { f: 'a. 'a Map2.key -> 'value -> ('a, 'map2) Map2.value -> 'value  } [@@unboxed]
    val nonidempotent_inter: 'a t -> 'b Map2.t -> ('a, 'b) polyinter_foreign -> 'a t

    (** This is equivalent to multiple calls to [insert], where the key
        and values would be taken from the second map,
        i.e. [insert_multiple f m1 m2] calls [f.f] on every key of [m2],
        says if the corresponding value also exists in [m1], and adds
        or remove the element in [m1] depending on the value of [f.f].
        [f.f] is called in the order of [Key.to_int] *)
    type ('map1,'map2) polyinsert_multiple = { f: 'a. key -> 'map1 option -> ('a,'map2) Map2.value -> 'map1 option } [@@unboxed]
    val insert_multiple: 'a t -> 'b Map2.t -> ('a,'b) polyinsert_multiple -> 'a t

    (** This is equivalent to multiple calls to [remove], where the key
        and values would be taken from the second map;
        i.e. [remove_multiple f m1 m2] calls [f.f] on every binding that
        is in both [m1] and [m2], and either removes or changes it in [m1].
        Bindings are passed to [f.f] in increasing order of [Key.to_int]. *)
    type ('map1,'map2) polyremove_multiple = { f: 'a. key -> 'map1 -> ('a,'map2) Map2.value -> 'map1 option } [@@unboxed]
    val remove_multiple: 'a t -> 'b Map2.t -> ('a,'b) polyremove_multiple -> 'a t
  end

  val pretty :
    ?pp_sep:(Format.formatter -> unit -> unit) ->
    (Format.formatter -> key -> 'a -> unit) ->
    Format.formatter -> 'a t -> unit
  (** [pp_sep] defaults to [Format.pp_print_cut] *)
end

(** The signature of keys when they are all of the same type.  *)
module type HomogeneousKey = sig
  type t

  (** A unique identifier for values of the type. Usually, we use a
      fresh counter that is increased to give a unique id to each
      object. Correctness of the operations requires that different
      values in a tree correspond to different integers. *)
  val to_int: t -> int
end

module MakeMap(Key:HomogeneousKey):Map_S with type key = Key.t
module MakeSet(Key:HomogeneousKey):Set_S with type _ BaseMap.key = Key.t


(**************** Heterogeneous key and values ****************)

(** Same as above, but with Heterogeneous keys (and values, for maps).  *)

(** Module type of a set containing different keys, very similar to
    [Set_S], but with simple type [key] being replaced by type
    constructor ['a key]. *)
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


(** This is the same than [Map_S], except that:

    - The type of [key] is replaced by a type constructor ['k
    key]. Because of that, most higher-order arguments require
    higher-ranking polymorphism, and we provide records that allows to
    path them (e.g. [polyiter],[polymap],[polyunion], etc.)

    - The type of [map] is still parameterized by an argument (['m
    map])

    - The type of values depend on both the type of the key and the
    type of the map, hence the type [('k,'m) value].

    - The type of some return values, like key-value pairs, must be
    concealed existentially, hence the [KeyValue] constructor. *)
module type HeterogeneousMap_S = sig
  include BaseMap_S

  module WithForeign(Map2:BaseMap_S with type 'a key = 'a key):sig
    type ('map1,'map2) polyinter_foreign = { f: 'a. 'a key -> ('a,'map1) value -> ('a,'map2) Map2.value -> ('a,'map1) value } [@@unboxed]

    (** Like inter. Tries to preserve physical equality on the first argument when possible. *)
    val nonidempotent_inter: 'a t -> 'b Map2.t -> ('a,'b) polyinter_foreign -> 'a t

    (** Like [filter_map_no_share], but allows to transform a foreigh map into the current one. *)
    type ('map2,'map1) polyfilter_map_foreign =
      { f : 'a. 'a key -> ('a, 'map2) Map2.value -> ('a, 'map1) value option; } [@@unboxed]
    val filter_map_no_share : 'map2 Map2.t -> ('map2,'map1) polyfilter_map_foreign -> 'map1 t

    type ('map1,'map2) polyinsert_multiple = { f: 'a. 'a key -> ('a,'map1) value option -> ('a,'map2) Map2.value -> ('a,'map1) value option } [@@unboxed]
    val insert_multiple: 'a t -> 'b Map2.t -> ('a,'b) polyinsert_multiple -> 'a t
    (** This is equivalent to multiple calls to [insert], where the key
        and values would be taken from the second map,
        i.e. [insert_multiple f m1 m2] calls [f] on every key of [m2] (in order of [Key.to_inr]),
        says if the corresponding value also exists in [m1], and adds
        or remove the element in [m1] depending on the value of
        [f]. *)

    type ('map1,'map2) polyremove_multiple = { f: 'a. 'a key -> ('a,'map1) value -> ('a,'map2) Map2.value -> ('a,'map1) value option } [@@unboxed]
    val remove_multiple: 'a t -> 'b Map2.t -> ('a,'b) polyremove_multiple -> 'a t
    (** This is equivalent to multiple calls to [remove], where the key
        and values would be taken from the second map;
        i.e. [remove_multiple f m1 m2] calls [f.f] on every binding that
        is in both [m1] and [m2], and either removes or changes it in [m1].
        Bindings are passed to [f.f] in increasing order of [Key.to_int]. *)
  end
end


(** To have heterogeneous keys, we must define a polymorphic equality
    function.  Like in the homogeneous case, it should have the
    requirement that [(to_int a) = (to_int b) ==> polyeq a b = Eq]. *)
type (_, _) cmp = Eq : ('a, 'a) cmp | Diff : ('a, 'b) cmp
module type HeterogeneousKey = sig
  type 'key t
  val to_int : 'key t -> int
  val polyeq : 'a t -> 'b t -> ('a, 'b) cmp
end


(** The moodule type of values, which can be heterogeneous.  *)
module type Value = sig type ('key, 'map) t end

(** To use when the type of the value is the same (but the keys can still be heterogeneous). *)
module HomogeneousValue:Value with type ('a,'map) t = 'map


module MakeHeterogeneousSet(Key:HeterogeneousKey):HeterogeneousSet_S with type 'a BaseMap.key = 'a Key.t
module MakeHeterogeneousMap(Key:HeterogeneousKey)(Value:Value):HeterogeneousMap_S
  with type 'a key = 'a Key.t
   and type ('k,'m) value = ('k,'m) Value.t


(**************** Custom representation of Nodes. ****************)

(** We can also customize the representation and creation of nodes, to
   gain space or time.

   Possibitities include having weak key and/or values, hash-consing,
   giving unique number to nodes or keeping them in sync with the
   disk, lazy evaluation and/or caching, etc. *)








(** Create a Homogeneous Map with a custom [Node].  *)
module MakeCustomHomogeneous
    (Key:HomogeneousKey)
    (Node:Node with type 'a key = Key.t and type ('key,'map) value = 'map)
  :Map_S
    with type key = Key.t
     and type 'm t = 'm Node.t


(** Create an Heterogeneous map with a custom [Node]. *)
module MakeCustom
    (Key:HeterogeneousKey)
    (Value:Value)
    (Node:Node with type 'a key = 'a Key.t and type ('key,'map) value = ('key,'map) Value.t)
  :HeterogeneousMap_S
    with type 'a key = 'a Key.t
     and type ('k,'m) value = ('k,'m) Value.t
     and type 'm t = 'm Node.t

(** Some implementations of Node. *)

(** This module is such that 'map t = 'map view.  *)
module SimpleNode(Key : sig type 'k t end)(Value : Value):Node
  with type 'a key = 'a Key.t
   and type ('key,'map) value = ('key,'map) Value.t

(** Here, nodes also contain a unique id, e.g. so that they can be
    used as keys of maps or hashtables. *)
module NodeWithId(Key : sig type 'k t end)(Value:Value):NodeWithId
  with type 'a key = 'a Key.t
   and type ('key,'map) value = ('key,'map) Value.t


(** Maybe: we can make variations around NodeWithId; e.g. a version
    that does HashConsing, or a version that replicates the node to a
    key-value store on disk, etc. *)

(** An optimized representation for sets, i.e. maps to unit: we do not
    store a reference to unit (note that you can further optimize when
    you know the representation of the key). *)
module SetNode(Key : sig type 'k t end):Node
  with type 'a key = 'a Key.t
   and type ('key,'map) value = unit


(** Node used to implement weak key hashes (the key-binding pair is an
    Ephemeron, the reference to the key is weak, and if the key is
    garbage collected, the binding disappears from the map *)
module WeakNode(Key : sig type 'k t end)(Value : Value):Node
  with type 'a key = 'a Key.t
   and type ('key,'map) value = ('key,'map) Value.t

(** Both a WeakNode and a SetNode, useful to implement Weak sets.  *)
module WeakSetNode(Key : sig type 'k t end):Node
  with type 'a key = 'a Key.t
   and type ('key,'map) value = unit

(* TODO: Functor to make sets from maps. *)
(* TODO: consider the "shape" of a map, and use this to have functions
   that filter a map, or update several elements. Maybe this just
   amounts to providing a "view" function to nodes.  *)
(* TODO: A possibility of customizing the fixpoint in the recursive
   calls, so that we can cache operations or make lazy some of the
   operations. *)
