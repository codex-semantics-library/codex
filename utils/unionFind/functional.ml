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

module GenericRelationalValued
    (Term : Parameters.GENERIC_TERM)
    (Relation : Parameters.GENERIC_GROUP)
    (Value : Parameters.GENERIC_VALUE with type ('a,'b) relation = ('a,'b) Relation.t) =
struct
  module TermSet = PatriciaTree.MakeHeterogeneousSet (Term)
  (** Set of ['a Term.t] *)

  (** Making the value "boxed" helps the typechecker in some first-order polymorphism case
      @see <https://discuss.ocaml.org/t/weird-behaviors-with-first-order-polymorphism/13783> *)
  module PTree_Value = struct
      type ('a, 'b) t = V of 'b [@@unboxed]
  end

  (** {2 Existential wrappers for the return type of find operations} *)

  type 'a term_through_relation =
    TermThroughRel : 'b Term.t * ('a, 'b) Relation.t -> 'a term_through_relation
  type 'a value_through_relation =
    ValueThroughRelation : 'b Value.t option * ('a, 'b) Relation.t -> 'a value_through_relation
  type 'a all_through_relation =
    AllThroughRelation : 'b Term.t * 'b Value.t option * TermSet.t * ('a, 'b) Relation.t -> 'a all_through_relation


  module ReprMap = PatriciaTree.MakeHeterogeneousMap
      (Term)
      (struct type ('a, _) t = 'a term_through_relation end)
  (** Map [_ ReprMap.t] mapping ['a Term.t] to ['a term_through_relation] *)

  module ClassMap = PatriciaTree.MakeHeterogeneousMap (Term) (PTree_Value)
  (** Map ['b ClassMap.t] mapping ['a Term.t] to ['b] *)

  module ValueMap = PatriciaTree.MakeHeterogeneousMap
      (Term)
      (struct type ('a, _) t = 'a Value.t end)

  (** Union-find structure

      Invariants on t:
      - Flat union find: i.e. forall (k -> v) in representatives,
        v is a reprensentative meaning:
        - v in dom(classes) union dom(values)
        - k in classes.find v
      - classes are correct: forall (r -> c) in classes, forall x in c, repr(x) = r
      - dom(representatives) is (almost) minimal:
        - it contains all non-representatives
        - it contains all non-trivial representatives (required for join), i.e:
          - representatives x of non-singleton classes (x in dom(classes))
          - representatives x with attached values (x in dom(values))
          The relation for all non trivial representatives is [Relation.identity]
        - it contains NO trivial representative
      - classes is minimal: if v is a singleton class, v does not appear in classes
      - Sorted union find: forall (k -> v) in representatives, v <= k.
        or, more specifically, Term.to_int v <= Term.to_int k
  *)
  type t = {
    representatives : unit ReprMap.t;
    (** map: ['a Term.t --> ('b Term.t * ('a, 'b) relation)],
        mapping elements to representatives
        representatives do not appear in this map's domain. *)
    classes : TermSet.t ClassMap.t;
    (** map: ['a Term.t --> TermSet.t,] mapping representatives to their classes
        only non-singleton representatives appear in this map's domain *)
    values : unit ValueMap.t;
    (** map: ['a Term.t --> 'a Value.t] mapping representatives to their values
        can contain singleton representatives *)
  }


  let ( ** ) = Relation.compose
  let ( ~~ ) = Relation.inverse
  let ( |>> ) = Value.apply

  (** [are_sorted a b = true] IFF [a] is the chosen representative for [{a,b}] *)
  let are_sorted a b = Int.compare (Term.to_int a) (Term.to_int b) <= 0

  let equal a b =
    ReprMap.reflexive_same_domain_for_all2 a.representatives b.representatives
    { f = fun _ (TermThroughRel(a,ra)) (TermThroughRel(b,rb)) ->
      match Term.polyeq a b with
      | Eq -> Relation.equal ra rb
      | Diff -> false }
    (* By invariants, equality of representatives implies equality of classes *)
    (* &&
    ClassMap.reflexive_same_domain_for_all2 a.classes b.classes
    { f = fun _ (V a) (V b) -> TermSet.equal a b } *)
    &&
    ValueMap.reflexive_same_domain_for_all2 a.values b.values
    { f = fun _ v1 v2 -> Value.equal v1 v2 }

  let empty = {
    representatives = ReprMap.empty;
    classes = ClassMap.empty;
    values = ValueMap.empty;
  }

  (** {2 Find operation} *)

  let find_repr uf x =
    match ReprMap.find uf x with
    | y -> y (* by invariant, we don't need to search further *)
    | exception Not_found -> TermThroughRel (x, Relation.identity)

  let find_representative uf x = find_repr uf.representatives x

  let find_class uf x =
    let (TermThroughRel (x, _)) = find_repr uf.representatives x in
    match ClassMap.find uf.classes x with
    | V y -> y (* find_repr uf y (by invariant, we don't need to search further) *)
    | exception Not_found -> TermSet.singleton x

  let find_value uf x =
    let (TermThroughRel (x, rel)) = find_repr uf.representatives x in
    match ValueMap.find uf.values x with
    | y -> ValueThroughRelation (Some y, rel)
    | exception Not_found -> ValueThroughRelation (None, rel)

  let find uf x =
    let (TermThroughRel (x, rel)) = find_repr uf.representatives x in
    let classe =
      match ClassMap.find uf.classes x with
      | V y -> y (* find_repr uf y (by invariant, we don't need to search further) *)
      | exception Not_found -> TermSet.singleton x
    in
    match ValueMap.find uf.values x with
    | y -> AllThroughRelation (x, Some y, classe, rel)
    | exception Not_found -> AllThroughRelation (x, None, classe, rel)

  (** {2 Printers} *)

  (** Add s -> {s} to classes for all s appearing in values
      This enables iterating over all classes *)
  let add_singleton_classes uf =
    ValueMap.fold uf.values uf.classes {f = fun t _ classes ->
      if ClassMap.mem classes t then classes else
      ClassMap.add classes t (V (TermSet.singleton t))
    }

  let pretty_class reprs fmt classe =
    TermSet.pretty
      ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
      { f = (fun fmt x ->
          let TermThroughRel(_, rel) = find_repr reprs x in
          Format.fprintf fmt "@[%a (via @[%a@])@]" Term.pretty x Relation.pretty rel) }
      fmt classe

  let pretty_class_and_repr uf fmt repr classe =
    match ValueMap.find_opt uf.values repr with
    | Some v -> Format.fprintf fmt "@[%a: (@[%a@]) {@[<hv>%a@]}@]" Term.pretty repr Value.pretty v (pretty_class uf.representatives)
      classe
    | None -> Format.fprintf fmt "@[%a: (top) [@[<hv>%a@]}@]" Term.pretty repr (pretty_class uf.representatives)
    classe

  let pretty_polypretty uf : TermSet.t ClassMap.polypretty =
    { f = (fun fmt repr (V classe) -> pretty_class_and_repr uf fmt repr classe) }

  let pretty fmt uf =
    if ReprMap.is_empty uf.representatives
    then Format.fprintf fmt "Empty"
    else Format.fprintf fmt "@[%a@]"
        (ClassMap.pretty
            ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
            (pretty_polypretty uf))
        uf.classes

  let pretty_debug fmt uf =
    if ReprMap.is_empty uf.representatives
    then Format.fprintf fmt "Empty" else
    Format.fprintf fmt "@[<v>Reprs: @[%a@]@ Classes: @[<v>%a@]@ Values: @[<v>%a@]@]"
      (ReprMap.pretty ~pp_sep:Format.pp_print_space {f=fun fmt k (TermThroughRel(v,r)) ->
        Format.fprintf fmt "@[@[%a@]--(@[%a@])->@[%a@]@]"
        Term.pretty k Relation.pretty r Term.pretty v})
      uf.representatives
      (ClassMap.pretty
        ~pp_sep:Format.pp_print_space
        {f = fun fmt k (V c) -> Format.fprintf fmt "@[%a->{%a}@]" Term.pretty k
            (TermSet.pretty ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ") {f=Term.pretty}) c })
      uf.classes
      (ValueMap.pretty { f = fun fmt v value ->
        Format.fprintf fmt "@[%a->%a@]" Term.pretty v Value.pretty value})
      uf.values

  (** {2 Misc functions} *)

  (** Same as [check_related], but assumes [find] has already been performed
      on the first argument *)
  let semi_check_related (type a b c) uf (ra : c Term.t) (rel_a : (a,c) Relation.t) (b : b Term.t) =
    let TermThroughRel(rb, rel_b) = find_repr uf.representatives b in
    match Term.polyeq ra rb with
    | Eq -> Some(~~rel_b ** rel_a)
    | Diff -> None

  let check_related uf a b =
    let TermThroughRel(ra, rel_a) = find_repr uf.representatives a in
    semi_check_related uf ra rel_a b

  (** Adds a value to the map, maintains the map small by removing top values *)
  let value_map_add vmap x value =
    if Value.is_top value
    then ValueMap.remove vmap x
    else ValueMap.add vmap x value

  let add_value uf a v =
    let AllThroughRelation(repr, val_repr, _, rel) = find uf a in
    let v = v |>> rel in
    match val_repr with
    | None ->
        if Value.is_top v
        then uf
        else
          let uf = { uf with values = ValueMap.add uf.values repr v } in
          if ReprMap.mem uf.representatives repr then uf else
          (* The representative is no longer trivial, add it to the map *)
          { uf with representatives = ReprMap.add uf.representatives repr
                          (TermThroughRel(repr, Relation.identity)) }
    | Some v' ->
        let v = Value.meet v' v in
        if v == v' then uf (* XXX: change to Value.equal ? *)
        else {uf with values = ValueMap.add uf.values repr v}

  (** {2 Union operation} *)

  module ReprMapSet = ReprMap.WithForeign(TermSet.BaseMap)

  (** [make_union uf x y rel]: Performs a simple union
      @assumes [x] and [y] are representatives,
      @assumes that [are_sorted x y]
      @returns the union-find obtained by adding [y -(rel)-> x] in [uf] *)
  let make_union (type x y) uf (x : x Term.t) (y : y Term.t) (rel : (y, x) Relation.t) =
    let class_y =
      match ClassMap.find uf.classes y with
      | V b -> b
      | exception Not_found -> TermSet.singleton y
    in
    (* the new [representatives] field: [uf.representatives] updated so that
        elements that pointed to [y] now point to [x] *)
    let representatives = ReprMapSet.insert_multiple uf.representatives class_y {
      f = (fun (type a) (elt : a Term.t) repr () : a term_through_relation option ->
          match repr with
          | None ->
              begin match Term.polyeq y elt with
              | Eq -> Some (TermThroughRel (x, rel))
              | Diff -> failwith "Broken invariant: All elements of uf.class (save the representative) should appear in uf.representatives"
              end
          | Some (TermThroughRel(repr_elt, rel_elt)) ->
              match Term.polyeq y repr_elt with
              | Diff -> failwith "Broken invariant: All elements of 'uf.class y' should have 'y' as representative"
              | Eq -> Some (TermThroughRel (x, rel ** rel_elt)))
    } in

    (* the new [classes] field: remove [y]'s class and add its elements to [x]'s *)
    let classes = ClassMap.remove uf.classes y in
    let non_trivial = ref false in (* Check if x is now non_trivial, *)
    let classes =
      ClassMap.insert classes x (function
        | Some (V class_x) -> V (TermSet.union class_x class_y)  (* x is already non-trivial*)
        | None ->
          let class_y = (TermSet.add x class_y) in
          non_trivial := Option.is_none (TermSet.is_singleton class_y);
          V class_y)
    in
    let representatives =
      if !non_trivial && not (ReprMap.mem representatives x)
      then ReprMap.add representatives x (TermThroughRel(x, Relation.identity))
      else representatives
    in
    (* the new [value] field: remove [y]'s value and combine it to [x]'s *)
    let values =
      match ValueMap.find uf.values y with
      | exception Not_found -> uf.values (* No info on y, no need to update *)
      | value_y ->
          let values = ValueMap.remove uf.values y in
          match ValueMap.find values x with
          | exception Not_found ->
              value_map_add values x (value_y |>> rel)
          | value_x ->
              value_map_add values x (Value.meet value_x (value_y |>> rel))
    in
    { representatives; classes; values }

  (** Performs union of the classes of x and y

      We obtain the relations between the representatives using the following diagram:

              x --(rel)--> y
              |            |
            rel_x        rel_y
              |            |
              V            v
            repr_x       repr_y

        => repr_x --(inv rel_x; rel; rel_y)--> repr_y (where ";" is composition)
        => repr_y --(inv rel_y; inv rel; rel_x)--> repr_x
      We do need to flip the arguments, as R.compose as the opposite argument order.
      A good thing about generic type is that the typechecker ensure we generate a valid
      relation here.*)
  let union uf x y rel =
    let (TermThroughRel (repr_x, rel_x)) = find_repr uf.representatives x in
    let (TermThroughRel (repr_y, rel_y)) = find_repr uf.representatives y in
    match Term.polyeq repr_x repr_y with
    | Eq ->
      Codex_log.warning "Cannot add a second relation to a class, forgetting it";
      uf
    | Diff ->
        if are_sorted repr_x repr_y then
          make_union uf repr_x repr_y (rel_x ** ~~rel ** ~~rel_y)
        else
          make_union uf repr_y repr_x (rel_y ** rel ** ~~rel_x)

  let join a b =
    let repr_to_clean = ref TermSet.empty in
    (* map : repr_a -> repr_b -> repr_of_intersection for memoization *)
    let new_classes = ref ClassMap.empty in
    let [@inline] memoize_set repr_a repr_b value =
      new_classes := ClassMap.add !new_classes repr_a (
            V (match ClassMap.find !new_classes repr_a with
            | V b -> ClassMap.add b repr_b (V value)
            | exception Not_found -> ClassMap.singleton repr_b (V value)
          ))
    in
    let [@inline] memoize_get repr_a repr_b =
      let V map_a = ClassMap.find !new_classes repr_a in
      let V b = ClassMap.find map_a repr_b in b
    in
    (* The new classes of the join, initialized by the intersection of the old classes *)
    let classes = ref (ClassMap.idempotent_inter_filter a.classes b.classes {f=
      fun (type a) (repr : a Term.t) (PTree_Value.V c1) (PTree_Value.V c2) ->
        let intersection = TermSet.inter c1 c2 in
        if TermSet.is_empty intersection || Option.is_some (TermSet.is_singleton intersection)
        then (
          repr_to_clean := TermSet.add repr !repr_to_clean;
          None)
        else (
          memoize_set repr repr (TermSet.Any repr);
          Some (PTree_Value.V intersection)
        )
    }) in
    (* The new values of the join, initialized by the join of the old values *)
    let values = ref (ValueMap.idempotent_inter a.values b.values {f=fun _ v1 v2 -> Value.join v1 v2}) in


    (* Iterate through the intersection of element->representatives
       (skipping elements that point to the same representative) *)
    let representatives = ReprMap.idempotent_inter_filter a.representatives b.representatives {f =
    fun x rx_a rx_b ->
      let TermThroughRel(repr_a, rel_x_a) = rx_a in
      let TermThroughRel(repr_b, rel_x_b) = rx_b in
      match Term.polyeq repr_a repr_b with
      | Eq when Relation.equal rel_x_a rel_x_b -> Some rx_a
      | _ ->
      match memoize_get repr_a repr_b with
      | TermSet.Any repr -> begin
        match semi_check_related a repr_a rel_x_a repr, semi_check_related b repr_b rel_x_b repr with
        | Some rel_a, Some rel_b when Relation.equal rel_a rel_b -> Some(TermThroughRel(repr, rel_a))
        | Some _, Some _ -> (* We can't join different relation: remove x from the class *)
            classes := ClassMap.update !classes repr (function
              | None -> failwith "element should be in class"
              | Some (V classe) ->
                  let classe = TermSet.remove x classe in
                  if Option.is_some (TermSet.is_singleton classe)
                  then (
                    repr_to_clean := TermSet.add repr !repr_to_clean;
                    None)
                  else Some (V classe));
            None
        | _ -> failwith "Broken memoisation: Some elements of a class have different representatives"
        end
      | exception Not_found ->
          (* Unmemoized value: find the new class, intersection of the old classes *)
          let class_a = find_class a repr_a in
          let class_b = find_class b repr_b in
          let new_class = TermSet.inter class_a class_b in
          (* We know that x is the representative of the new class, since
             [idempotent_inter_filter] iterates through keys in ascending order
             and we haven't hit our memoization cache *)
          (* Compute the new value associated with new representative x *)
          let has_value = match ValueMap.find_opt a.values repr_a, ValueMap.find_opt b.values repr_b with
          | Some va, Some vb ->
              values := value_map_add !values x (Value.join
                  (va |>> ~~rel_x_a) (vb |>> ~~rel_x_b));
              true
          | _ -> false (* One of the values is top, so the join is top *)
          in
          (* Only add the class and new representative if not a singleton.
             We also don't need to memoize if the class is a singleton, since
             it will not show up again. *)
          let has_class = Option.is_none (TermSet.is_singleton new_class) in
          if has_class then (
            classes := ClassMap.add !classes x (V new_class);
            memoize_set repr_a repr_b (TermSet.Any x);
          );
          if has_class || has_value
          then Some (TermThroughRel(x, Relation.identity))
          else None (* Trivial representative *)
    }
  in
  let representatives = ReprMapSet.remove_multiple
    representatives !repr_to_clean { f=fun key rel () ->
      if ValueMap.mem !values key then Some rel else None } in
  { classes = !classes; representatives; values = !values }

  (** There is probably a faster way to do this. *)
  let subseteq a b = equal b (join a b)



  (** Another implementation of join, much slower, but more robust
      used to test the first one. *)
  let join_test a b =
    let representatives = ref ReprMap.empty in
    let classes = ref ClassMap.empty in
    let values = ref ValueMap.empty in
    (* Iterate on pairs of classes, to compute all possible intersections *)
    ClassMap.iter (add_singleton_classes a) { f = fun _ (V class_a) ->
      ClassMap.iter (add_singleton_classes b) { f = fun _ (V class_b) ->
        let inter = TermSet.inter class_a class_b in
        if TermSet.is_empty inter then () else
        let Any x = TermSet.min_elt inter in
        (* Value of new representative *)
        let ValueThroughRelation(va, rel_a) = find_value a x in
        let ValueThroughRelation(vb, rel_b) = find_value b x in
        let has_value = match va, vb with
        | Some va, Some vb ->
            values := value_map_add !values x (Value.join
                (va |>> ~~rel_a) (vb |>> ~~rel_b));
            true
        | _ -> false
        in
        (* Class of new representative *)
        let inter = TermSet.filter {f = fun (type a) (y : a Term.t) ->
          match Term.polyeq x y with
          | Eq -> true (* keep in class but don't add to representatives *)
          | Diff ->
              match check_related a y x, check_related b y x with
              | Some rel_a, Some rel_b when Relation.equal rel_a rel_b ->
                  representatives := ReprMap.add !representatives y (TermThroughRel(x,rel_a));
                  true
              | Some _, Some _ -> false
              | _ -> failwith "Broken invariant"
        } inter in
        let has_class = Option.is_none (TermSet.is_singleton inter) in
        if has_class then (
          classes := ClassMap.add !classes x (V inter)
        );
        if has_class || has_value then (
          representatives := ReprMap.add !representatives x (TermThroughRel(x,Relation.identity));
        )
      }
    };
    { classes = !classes; representatives = !representatives; values = !values }

  let check_invariants uf =
    (* Replace XX.for_all with XX.iter for an exhaustive list of all problems
        This version only returns one counter example per invariant. *)
    let errors = ref [] in
    let pretty_class = pretty_class uf.representatives in
    (* Checks [uf.representatives]:
      - Non self pointing elements:
        - must bind to a representative (which maps to a class which contains them)
      - Self pointing elements:
        - must be non-trivial representatives (in dom(uf.classes) of dom(uf.values))
        - must point to themselves via [Relation.identity] *)
    let _ = ReprMap.for_all uf.representatives {f = fun (type a) (k : a Term.t) (TermThroughRel(r,rel) : a term_through_relation) ->
      match Term.polyeq k r with
      | Diff ->
          let check1 = match ClassMap.find_opt uf.classes r with
          | None -> let message =
              Format.asprintf "Representative contains @[%a -(%a)-> %a@] but %a has a singleton class"
              Term.pretty k Relation.pretty rel Term.pretty r Term.pretty r in
            errors := message :: !errors; false
          | Some (V c) -> if TermSet.mem k c then true else (
            let message =
              Format.asprintf "Representative contains @[%a -(%a)-> %a@] but %a is not in the class of %a {@[%a@]}"
              Term.pretty k Relation.pretty rel Term.pretty r Term.pretty k Term.pretty r pretty_class c in
            errors := message :: !errors; false) in
          let check2 = match ReprMap.find_opt uf.representatives r with
          | None -> true
          | Some(TermThroughRel(r', rel')) -> if Term.polyeq r r' <> Diff then true else (
            let message =
              Format.asprintf "Representative is not flat: @[%a -(%a)-> %a -(%a)-> %a"
              Term.pretty k Relation.pretty rel Term.pretty r Relation.pretty rel' Term.pretty r' in
            errors := message :: !errors; false)
          in check1 && check2
      | Eq ->
          if not (Relation.equal rel Relation.identity) then (
            let message =
              Format.asprintf "Self pointing term in representatives whose relation isn't identity: %a -(%a)-> %a"
              Term.pretty k Relation.pretty rel Term.pretty r in
            errors := message :: !errors; false )
          else if not (ClassMap.mem uf.classes r || ValueMap.mem uf.values r) then (
            let message =
              Format.asprintf "Trivial repr in representatives: %a (self pointing term not in dom(classes) or dom(values))"
              Term.pretty k in
            errors := message :: !errors; false )
          else true } in
    (* check [uf.classes]:
       - all classe have cardinal >= 2
       - all classe have their min element has representatives
       - all classe contain their representatives
       - all elements of a class point to the classe's representative *)
    let _ = ClassMap.for_all uf.classes { f = fun k (V c) ->
      let n = TermSet.cardinal c in
      let check1 = if n >= 2 then true else (
        let message =
          Format.asprintf "The class of %a has size %d (<2) : %a"
          Term.pretty k n pretty_class c in
        errors := message :: !errors; false ) in
      let Any x = TermSet.min_elt c in
      let check2 = if Term.polyeq k x <> Diff then true else (
        let message =
          Format.asprintf "The representative of {@[<hv>%a@]} isn't minimal: it is %a, it should be %a"
          pretty_class c Term.pretty k Term.pretty x in
        errors := message :: !errors; false ) in
      let check3 = if TermSet.mem k c then true else (
        let message =
          Format.asprintf "The class of %a doesn't contain its representative: %a"
          Term.pretty k pretty_class c in
        errors := message :: !errors; false) in
      let check4 = TermSet.for_all {f=fun elt ->
        match ReprMap.find_opt uf.representatives elt with
        | Some (TermThroughRel(x,_)) when (Term.polyeq x k = Diff) ->
          let message =
            Format.asprintf "The class of %a {@[<hv>%a@]} contains %a, which points to a different term: %a"
            Term.pretty k pretty_class c Term.pretty elt Term.pretty x in
          errors := message :: !errors; false
        | None ->
          let message =
            Format.asprintf "The class of %a {@[<hv>%a@]} contains %a, which has no representative"
            Term.pretty k pretty_class c Term.pretty elt in
          errors := message :: !errors; false
        | Some _ -> true} c
      in check1 && check2 && check3 && check4} in
    (* Check [uf.values]:
       - all elements of dom(uf.values) are representative
       - no value verifies [Value.is_top] *)
    let _ = ValueMap.for_all uf.values {f = fun k v ->
      let check1 = match ReprMap.find uf.representatives k with
      | TermThroughRel(k', _) when Term.polyeq k k' <> Diff -> true
      | TermThroughRel(k', rel) -> let message =
            Format.asprintf "%a->%a appears in values, but isn't a repr (%a-(%a)->%a in representatives)"
            Term.pretty k Value.pretty v Term.pretty k Relation.pretty rel Term.pretty k'  in
          errors := message :: !errors; false
      | exception Not_found -> let message =
            Format.asprintf "Missing non-trivial repr: %a->%a appears in values, but not in representatives"
            Term.pretty k Value.pretty v in
          errors := message :: !errors; false
      in let check2 = if not (Value.is_top v) then true else
        let message = Format.asprintf "Top in values: %a->%a" Term.pretty k Value.pretty v in
        errors := message :: !errors; false
      in
      check1 && check2 } in
    if !errors = []
    then Ok ()
    else Error ("BROKEN INVARIANTS:\n- " ^ String.concat "\n- " (List.rev !errors))
end
