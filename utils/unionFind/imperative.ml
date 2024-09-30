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

(** {1 Nodes} *)
(** {2 Nodes without values (simple nodes)} *)

module MakeSimpleNode
    (Elt : Parameters.SIMPLE_GENERIC_ELT)
    (Relation : Parameters.GENERIC_GROUP) =
struct
  type 'a t = { mutable parent : 'a parent; payload : 'a Elt.t }
  and 'a parent =
    | Node : 'b t * ('a, 'b) Relation.t -> 'a parent
    | Root

  let polyeq a b = Elt.polyeq a.payload b.payload

  module Relation = Relation

  let get_parent x = x.parent
  let set_parent x v = x.parent <- v

  let payload x = x.payload
  let make_node payload = { payload; parent = Root }
end

module MakeSimpleNumberedNode
    (Elt : PatriciaTree.HETEROGENEOUS_KEY)
    (Relation : Parameters.GENERIC_GROUP) =
struct
  include MakeSimpleNode(Elt)(Relation)

  (** We could also use a [Hashmap] here (although it would require existential types) *)
  module BuiltNodes = PatriciaTree.MakeHeterogeneousMap
    (Elt)(struct type nonrec ('a, _) t = 'a t end)

  let nodes : unit BuiltNodes.t ref = ref BuiltNodes.empty
  let make_node payload =
    let nd = make_node payload in
    nodes := BuiltNodes.add payload nd !nodes;
    nd

  let get_node payload = BuiltNodes.find_opt payload !nodes

  let get_or_make_node payload = match get_node payload with
  | Some t -> t
  | None -> make_node payload
end

(** {2 Nodes with values} *)

module MakeNode
    (Elt : Parameters.SIMPLE_GENERIC_ELT)
    (Relation : Parameters.GENERIC_GROUP)
    (Value : Parameters.SIMPLE_GENERIC_VALUE with type ('a,'b) relation = ('a,'b) Relation.t) =
struct
  type 'a t = { mutable parent : 'a parent; payload : 'a Elt.t }
  and 'a parent =
    | Node : 'b t * ('a, 'b) Relation.t -> 'a parent
    | Root of 'a root
  and 'a root = { mutable value : 'a Value.t; mutable size : int }

  let polyeq a b = Elt.polyeq a.payload b.payload

  module Relation = Relation
  module Value = Value

  let get_parent x = x.parent
  let set_parent x v = x.parent <- v

  let payload x = x.payload
  let make_node payload value = { payload; parent = Root { value; size = 1 } }
end

module MakeNumberedNode
    (Elt : PatriciaTree.HETEROGENEOUS_KEY)
    (Relation : Parameters.GENERIC_GROUP)
    (Value : Parameters.SIMPLE_GENERIC_VALUE with type ('a,'b) relation = ('a,'b) Relation.t) =
struct
  include MakeNode(Elt)(Relation)(Value)

  (** We could also use a [Hashmap] here (although it would require existential types) *)
  module BuiltNodes = PatriciaTree.MakeHeterogeneousMap
      (Elt)(struct type nonrec ('a, _) t = 'a t end)

  let nodes : unit BuiltNodes.t ref = ref BuiltNodes.empty
  let make_node payload value =
    let nd = make_node payload value in
    nodes := BuiltNodes.add payload nd !nodes;
    nd

  let get_node payload = BuiltNodes.find_opt payload !nodes

  let get_or_make_node payload value = match get_node payload with
    | Some t -> t
    | None -> make_node payload value
end

(** {1 Union find structures} *)

module GenericRelationalValued(Node: Parameters.UF_NODE) =
struct
  open Node
  type 'a t = 'a Node.t
  type ('a, 'b) relation = ('a, 'b) Relation.t
  type 'a value = 'a Value.t

  type 'a ret = Exists_b : 'b t * 'b root * ('a, 'b) Relation.t -> 'a ret

  let ( ** ) = Relation.compose
  let ( ~~ ) = Relation.inverse

  let rec find_repr : type a. a t -> a ret =
   fun n ->
    match get_parent n with
    | Root r -> Exists_b (n, r, Relation.identity)
    | Node (parent, rel_parent) ->
        let (Exists_b (repr, root, rel_repr)) = find_repr parent in
        let rel = rel_repr ** rel_parent in
        let () = set_parent n (Node (repr, rel)) in
        Exists_b (repr, root, rel)

  type 'a node_through_relation =
    NodeThoughRelation : 'b t * ('a, 'b) Relation.t -> 'a node_through_relation
  type 'a value_through_relation =
    ValueThroughRelation : 'b Value.t * ('a, 'b) Relation.t -> 'a value_through_relation
  type 'a node_and_value_through_relation =
    NodeValueThroughRelation : 'b t * 'b Value.t * ('a, 'b) Relation.t
        -> 'a node_and_value_through_relation

  let find_representative n =
    let (Exists_b (repr, _, rel)) = find_repr n in
    NodeThoughRelation (repr, rel)

  let find_value n =
    let (Exists_b (_, root, rel)) = find_repr n in
    ValueThroughRelation (root.value, rel)

  let find n =
    let (Exists_b (repr, root, rel)) = find_repr n in
    NodeValueThroughRelation (repr, root.value, rel)

  let check_related a b =
      let Exists_b(ra, _, rel_a) = find_repr a in
      let Exists_b(rb, _, rel_b) = find_repr b in
      match polyeq ra rb with
      | Eq -> Some(~~rel_b ** rel_a)
      | Diff -> None

  let add_value a v =
    let Exists_b(_, root, rel) = find_repr a in
    let v = Value.apply v rel in
    root.value <- Value.meet root.value v

  (** Using the following diagram:

          a --(rel)--> b
          |            |
        rel_a        rel_b
          |            |
          V            v
        repr_a       repr_b

    => repr_a --(inv rel_a; rel; rel_b)--> repr_b (where ";" is composition)
    => repr_b --(inv rel_b; inv rel; rel_a)--> repr_a
  We do need to flip the arguments, as R.compose as the opposite argument order.
  A good thing about generic types is that the typechecker ensure we generate a valid
  relation here. *)
  let union a b rel =
    let (Exists_b (repr_a, root_a, rel_a)) = find_repr a in
    let (Exists_b (repr_b, root_b, rel_b)) = find_repr b in
    match polyeq repr_a repr_b with
    | Eq ->
      let old_rel = ~~rel_b ** rel_a in
      if Relation.equal rel old_rel then Ok () else Error old_rel
    | Diff ->
        (* Favor making [a] a child of [b] over the reverse, as this avoids a
           relation flip. *)
        begin if root_b.size >= root_a.size then (
          let rel = rel_b ** rel ** ~~rel_a in
          root_b.size <- root_b.size + root_a.size;
          root_b.value <- Value.meet root_b.value (Value.apply root_a.value rel);
          set_parent repr_a (Node (repr_b, rel)))
        else
          let rel = rel_a ** ~~rel ** ~~rel_b in
          root_a.size <- root_b.size + root_a.size;
          root_a.value <- Value.meet root_a.value (Value.apply root_b.value rel);
          set_parent repr_b (Node (repr_a, rel))
        end;
        Ok ()
end

module GenericRelational (Node : Parameters.SIMPLE_UF_NODE) =
struct
  open Node
  type 'a t = 'a Node.t
  type ('a, 'b) relation = ('a, 'b) Relation.t

  let ( ** ) = Relation.compose
  let ( ~~ ) = Relation.inverse

  type 'a node_through_relation =
  | NodeThoughRelation : 'b t * ('a, 'b) Relation.t -> 'a node_through_relation

  let rec find_representative : type a. a t -> a node_through_relation =
   fun n ->
    match get_parent n with
    | Root -> NodeThoughRelation (n, Relation.identity)
    | Node (parent, rel_parent) ->
        let (NodeThoughRelation (repr, rel_repr)) = find_representative parent in
        let rel = rel_repr ** rel_parent in
        let () = set_parent n (Node (repr, rel)) in
        NodeThoughRelation (repr, rel)

  let check_related a b =
    let NodeThoughRelation(ra, rel_a) = find_representative a in
    let NodeThoughRelation(rb, rel_b) = find_representative b in
    match polyeq ra rb with
    | Eq -> Some(~~rel_b ** rel_a)
    | Diff -> None

  (** Using the following diagram:

          a --(rel)--> b
          |            |
        rel_a        rel_b
          |            |
          V            v
        repr_a       repr_b

    => repr_a --(inv rel_a; rel; rel_b)--> repr_b (where ";" is composition)
    => repr_b --(inv rel_b; inv rel; rel_a)--> repr_a
  We do need to flip the arguments, as R.compose as the opposite argument order.
  A good thing about generic types is that the typechecker ensure we generate a valid
  relation here. *)
  let union a b rel =
    let NodeThoughRelation(repr_a, rel_a) = find_representative a in
    let NodeThoughRelation(repr_b, rel_b) = find_representative b in
    match polyeq repr_a repr_b with
       | Eq ->
          let old_rel = ~~rel_b ** rel_a in
          if Relation.equal rel old_rel then Ok () else Error old_rel
       | Diff ->
          (* Favor making [a] a child of [b] over the reverse, as this avoids a
             relation flip. *)
          (* begin *)
            let rel = rel_b ** rel ** ~~rel_a in
            (* root_b.size <- root_b.size + root_a.size; *)
            set_parent repr_a (Node (repr_b, rel));
          (* else
            let rel = rel_a ** ~~rel ** ~~rel_b in
            root_a.size <- root_b.size + root_a.size;
            set_parent repr_b (Node (repr_a, rel))
          end; *)
          Ok ()
end
