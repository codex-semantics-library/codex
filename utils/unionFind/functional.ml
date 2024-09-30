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
    (Elt : Parameters.GENERIC_ELT)
    (Relation : Parameters.GENERIC_GROUP)
    (Value : Parameters.GENERIC_VALUE with type ('a,'b) relation = ('a,'b) Relation.t) =
struct
  module EltSet = PatriciaTree.MakeHeterogeneousSet (Elt)
  (** Set of ['a Elt.t] *)

  (** {2 Existential wrappers for the return type of find operations} *)

  type 'a elt_through_relation =
    EltThroughRel : 'b Elt.t * ('a, 'b) Relation.t -> 'a elt_through_relation
  type 'a value_through_relation =
    ValueThroughRelation : 'b Value.t option * ('a, 'b) Relation.t -> 'a value_through_relation
  type 'a all_through_relation =
    AllThroughRelation : 'b Elt.t * 'b Value.t option * EltSet.t * ('a, 'b) Relation.t -> 'a all_through_relation


  module ReprMap = PatriciaTree.MakeHeterogeneousMap
      (Elt)
      (struct type ('a, _) t = 'a elt_through_relation end)
  (** Map [_ ReprMap.t] mapping ['a Elt.t] to ['a elt_through_relation] *)

  module ClassMap = PatriciaTree.MakeHeterogeneousMap (Elt) (PatriciaTree.WrappedHomogeneousValue)
  (** Map ['b ClassMap.t] mapping ['a Elt.t] to ['b] *)

  module ValueMap = PatriciaTree.MakeHeterogeneousMap
      (Elt)
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
        or, more specifically, Elt.to_int v <= Elt.to_int k
  *)
  type t = {
    representatives : unit ReprMap.t;
    (** map: ['a Elt.t --> ('b Elt.t * ('a, 'b) relation)],
        mapping elements to representatives
        representatives do not appear in this map's domain. *)
    classes : EltSet.t ClassMap.t;
    (** map: ['a Elt.t --> EltSet.t,] mapping representatives to their classes
        only non-singleton representatives appear in this map's domain *)
    values : unit ValueMap.t;
    (** map: ['a Elt.t --> 'a Value.t] mapping representatives to their values
        can contain singleton representatives *)
  }


  let ( ** ) = Relation.compose
  let ( ~~ ) = Relation.inverse
  let ( |>> ) = Value.apply

  (** [are_sorted a b = true] IFF [a] is the chosen representative for [{a,b}] *)
  let are_sorted a b = Int.compare (Elt.to_int a) (Elt.to_int b) <= 0

  let equal a b =
    ReprMap.reflexive_same_domain_for_all2
    { f = fun _ (EltThroughRel(a,ra)) (EltThroughRel(b,rb)) ->
      match Elt.polyeq a b with
      | Eq -> Relation.equal ra rb
      | Diff -> false } a.representatives b.representatives
    (* By invariants, equality of representatives implies equality of classes *)
    (* &&
    ClassMap.reflexive_same_domain_for_all2 a.classes b.classes
    { f = fun _ (V a) (V b) -> EltSet.equal a b } *)
    &&
    ValueMap.reflexive_same_domain_for_all2
    { f = fun _ v1 v2 -> Value.equal v1 v2 } a.values b.values

  let empty = {
    representatives = ReprMap.empty;
    classes = ClassMap.empty;
    values = ValueMap.empty;
  }

  (** {2 Find operation} *)

  let find_repr uf x =
    match ReprMap.find x uf with
    | y -> y (* by invariant, we don't need to search further *)
    | exception Not_found -> EltThroughRel (x, Relation.identity)

  let find_representative uf x = find_repr uf.representatives x

  let find_class uf x =
    let (EltThroughRel (x, _)) = find_repr uf.representatives x in
    match ClassMap.find x uf.classes with
    | Snd y -> y (* find_repr uf y (by invariant, we don't need to search further) *)
    | exception Not_found -> EltSet.singleton x

  let find_value uf x =
    let (EltThroughRel (x, rel)) = find_repr uf.representatives x in
    match ValueMap.find x uf.values with
    | y -> ValueThroughRelation (Some y, rel)
    | exception Not_found -> ValueThroughRelation (None, rel)

  let find uf x =
    let (EltThroughRel (x, rel)) = find_repr uf.representatives x in
    let classe =
      match ClassMap.find x uf.classes with
      | Snd y -> y (* find_repr uf y (by invariant, we don't need to search further) *)
      | exception Not_found -> EltSet.singleton x
    in
    match ValueMap.find x uf.values with
    | y -> AllThroughRelation (x, Some y, classe, rel)
    | exception Not_found -> AllThroughRelation (x, None, classe, rel)

  (** {2 Printers} *)

  module CMapForeign = ClassMap.WithForeign(ValueMap)

  (** Add s -> {s} to classes for all s appearing in values
      This enables iterating over all classes *)
  let add_singleton_classes uf =
    CMapForeign.update_multiple_from_foreign
      uf.values
      { f = fun t x _ -> match x with
        | Some _ -> x
        | None -> Some (Snd (EltSet.singleton t)) }
      uf.classes

  let pretty_class reprs fmt classe =
    EltSet.pretty
      ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
      { f = (fun fmt x ->
          let EltThroughRel(_, rel) = find_repr reprs x in
          Format.fprintf fmt "@[%a (via @[%a@])@]" Elt.pretty x Relation.pretty rel) }
      fmt classe

  let pretty_class_and_repr uf fmt repr classe =
    match ValueMap.find_opt repr uf.values with
    | Some v -> Format.fprintf fmt "@[%a: (@[%a@]) {@[<hv>%a@]}@]" Elt.pretty repr Value.pretty v (pretty_class uf.representatives)
      classe
    | None -> Format.fprintf fmt "@[%a: (top) [@[<hv>%a@]}@]" Elt.pretty repr (pretty_class uf.representatives)
    classe

  let pretty_polypretty uf : EltSet.t ClassMap.polypretty =
    { f = (fun fmt repr (Snd classe) -> pretty_class_and_repr uf fmt repr classe) }

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
      (ReprMap.pretty ~pp_sep:Format.pp_print_space {f=fun fmt k (EltThroughRel(v,r)) ->
        Format.fprintf fmt "@[@[%a@]--(@[%a@])->@[%a@]@]"
        Elt.pretty k Relation.pretty r Elt.pretty v})
      uf.representatives
      (ClassMap.pretty
        ~pp_sep:Format.pp_print_space
        {f = fun fmt k (Snd c) -> Format.fprintf fmt "@[%a->{%a}@]" Elt.pretty k
            (EltSet.pretty ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ") {f=Elt.pretty}) c })
      uf.classes
      (ValueMap.pretty { f = fun fmt v value ->
        Format.fprintf fmt "@[%a->%a@]" Elt.pretty v Value.pretty value})
      uf.values

  (** {2 Misc functions} *)

  (** Same as [check_related], but assumes [find] has already been performed
      on the first argument *)
  let semi_check_related (type a b c) uf (ra : c Elt.t) (rel_a : (a,c) Relation.t) (b : b Elt.t) =
    let EltThroughRel(rb, rel_b) = find_repr uf.representatives b in
    match Elt.polyeq ra rb with
    | Eq -> Some(~~rel_b ** rel_a)
    | Diff -> None

  let check_related uf a b =
    let EltThroughRel(ra, rel_a) = find_repr uf.representatives a in
    semi_check_related uf ra rel_a b

  (** Adds a value to the map, maintains the map small by removing top values *)
  let value_map_add vmap x value =
    if Value.is_top value
    then ValueMap.remove x vmap
    else ValueMap.add x value vmap

  let add_value uf a v =
    let AllThroughRelation(repr, val_repr, _, rel) = find uf a in
    let v = v |>> rel in
    match val_repr with
    | None ->
        if Value.is_top v
        then uf
        else
          let uf = { uf with values = ValueMap.add repr v uf.values } in
          if ReprMap.mem repr uf.representatives then uf else
          (* The representative is no longer trivial, add it to the map *)
          { uf with representatives = ReprMap.add repr
                          (EltThroughRel(repr, Relation.identity)) uf.representatives }
    | Some v' ->
        let v = Value.meet v' v in
        if v == v' then uf (* XXX: change to Value.equal ? *)
        else {uf with values = ValueMap.add repr v uf.values}

  (** {2 Union operation} *)

  module ReprMapSet = ReprMap.WithForeign(EltSet.BaseMap)

  (** [make_union uf x y rel]: Performs a simple union
      @assumes [x] and [y] are representatives,
      @assumes that [are_sorted x y]
      @returns the union-find obtained by adding [y -(rel)-> x] in [uf] *)
  let make_union (type x y) uf (x : x Elt.t) (y : y Elt.t) (rel : (y, x) Relation.t) =
    let class_y =
      match ClassMap.find y uf.classes with
      | Snd b -> b
      | exception Not_found -> EltSet.singleton y
    in
    (* the new [representatives] field: [uf.representatives] updated so that
        elements that pointed to [y] now point to [x] *)
    let representatives = ReprMapSet.update_multiple_from_foreign class_y {
      f = (fun (type a) (elt : a Elt.t) repr () : a elt_through_relation option ->
          match repr with
          | None ->
              begin match Elt.polyeq y elt with
              | Eq -> Some (EltThroughRel (x, rel))
              | Diff -> failwith "Broken invariant: All elements of uf.class (save the representative) should appear in uf.representatives"
              end
          | Some (EltThroughRel(repr_elt, rel_elt)) ->
              match Elt.polyeq y repr_elt with
              | Diff -> failwith "Broken invariant: All elements of 'uf.class y' should have 'y' as representative"
              | Eq -> Some (EltThroughRel (x, rel ** rel_elt)))
    } uf.representatives in

    (* the new [classes] field: remove [y]'s class and add its elements to [x]'s *)
    let classes = ClassMap.remove y uf.classes in
    let classes =
      ClassMap.insert x (function
        | Some (Snd class_x) -> Snd (EltSet.union class_x class_y)  (* x is already non-trivial*)
        | None ->
          let class_y = (EltSet.add x class_y) in
          Snd class_y) classes
    in
    let representatives =
      (* x is now non-trivial (its class must contain y > x) *)
      ReprMap.add x (EltThroughRel(x, Relation.identity)) representatives
    in
    (* the new [value] field: remove [y]'s value and combine it to [x]'s *)
    let values =
      match ValueMap.find y uf.values with
      | exception Not_found -> uf.values (* No info on y, no need to update *)
      | value_y ->
          let values = ValueMap.remove y uf.values in
          match ValueMap.find x values with
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
    let (EltThroughRel (repr_x, rel_x)) = find_repr uf.representatives x in
    let (EltThroughRel (repr_y, rel_y)) = find_repr uf.representatives y in
    match Elt.polyeq repr_x repr_y with
    | Eq ->
      let old_rel = ~~rel_y ** rel_x in
      if Relation.equal rel old_rel then Ok uf else Error old_rel
    | Diff ->
        Ok (if are_sorted repr_x repr_y then
          make_union uf repr_x repr_y (rel_x ** ~~rel ** ~~rel_y)
        else
          make_union uf repr_y repr_x (rel_y ** rel ** ~~rel_x))

  let join a b =
    let repr_to_clean = ref EltSet.empty in
    (* map : repr_a -> repr_b -> repr_of_intersection for memoization *)
    let new_classes = ref ClassMap.empty in
    let [@inline] memoize_set repr_a repr_b value =
      new_classes := ClassMap.add repr_a (
            Snd (match ClassMap.find repr_a !new_classes with
            | Snd b -> ClassMap.add repr_b (Snd value) b
            | exception Not_found -> ClassMap.singleton repr_b (Snd value)
          )) !new_classes
    in
    let [@inline] memoize_get repr_a repr_b =
      let Snd map_a = ClassMap.find repr_a !new_classes in
      let Snd b = ClassMap.find repr_b map_a in b
    in
    (* The new classes of the join, initialized by the intersection of the old classes *)
    let classes = ref (ClassMap.idempotent_inter_filter {f=
      fun (type a) (repr : a Elt.t) (PatriciaTree.Snd c1) (PatriciaTree.Snd c2) ->
        let intersection = EltSet.inter c1 c2 in
        if EltSet.is_empty intersection || Option.is_some (EltSet.is_singleton intersection)
        then (
          repr_to_clean := EltSet.add repr !repr_to_clean;
          None)
        else (
          memoize_set repr repr (EltSet.Any repr);
          Some (PatriciaTree.Snd intersection)
        )
    } a.classes b.classes) in
    (* The new values of the join, initialized by the join of the old values *)
    let values = ref (ValueMap.idempotent_inter_filter
        {f=fun repr v1 v2 ->
          let v = Value.join v1 v2 in
          if Value.is_top v then  (
            repr_to_clean := EltSet.add repr !repr_to_clean;
            None)
          else Some v
          }
        a.values b.values) in

    (* Iterate through the intersection of element->representatives
       (skipping elements that point to the same representative) *)
    let representatives = ReprMap.idempotent_inter_filter {f =
    fun x rx_a rx_b ->
      let EltThroughRel(repr_a, rel_x_a) = rx_a in
      let EltThroughRel(repr_b, rel_x_b) = rx_b in
      (* match Elt.polyeq repr_a repr_b with
      | Eq when Relation.equal rel_x_a rel_x_b -> Some rx_a
      | _ -> *)
      match memoize_get repr_a repr_b with
      | EltSet.Any repr -> begin
        match semi_check_related a repr_a rel_x_a repr, semi_check_related b repr_b rel_x_b repr with
        | Some rel_a, Some rel_b when Relation.equal rel_a rel_b -> Some(EltThroughRel(repr, rel_a))
        | Some _, Some _ -> (* We can't join different relation: remove x from the class *)
            classes := ClassMap.update repr (function
              | None -> failwith "element should be in class"
              | Some (Snd classe) ->
                  let classe = EltSet.remove x classe in
                  if Option.is_some (EltSet.is_singleton classe)
                  then (
                    repr_to_clean := EltSet.add repr !repr_to_clean;
                    None)
                  else Some (Snd classe)) !classes;
            None
        | _ -> failwith "Broken memoisation: Some elements of a class have different representatives"
        end
      | exception Not_found ->
          (* Unmemoized value: find the new class, intersection of the old classes *)
          let class_a = find_class a repr_a in
          let class_b = find_class b repr_b in
          let new_class = EltSet.inter class_a class_b in
          (* We know that x is the representative of the new class, since
             [idempotent_inter_filter] iterates through keys in ascending order
             and we haven't hit our memoization cache *)
          (* Compute the new value associated with new representative x *)
          let has_value = match ValueMap.find_opt repr_a a.values, ValueMap.find_opt repr_b b.values with
          | Some va, Some vb ->
              let v = Value.join (va |>> ~~rel_x_a) (vb |>> ~~rel_x_b) in
              if Value.is_top v then false else (values := ValueMap.add x v !values; true)
          | _ -> false (* One of the values is top, so the join is top *)
          in
          (* Only add the class and new representative if not a singleton.
             We also don't need to memoize if the class is a singleton, since
             it will not show up again. *)
          let has_class = Option.is_none (EltSet.is_singleton new_class) in
          if has_class then (
            classes := ClassMap.add x (Snd new_class) !classes;
            memoize_set repr_a repr_b (EltSet.Any x);
          );
          if has_class || has_value
          then Some (EltThroughRel(x, Relation.identity))
          else None (* Trivial representative *)
    } a.representatives b.representatives
  in
  let representatives = ReprMapSet.update_multiple_from_inter_with_foreign
     !repr_to_clean { f=fun key rel () ->
      if ValueMap.mem key !values || ClassMap.mem key !classes then Some rel else None } representatives in
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
    ClassMap.iter { f = fun _ (Snd class_a) ->
      ClassMap.iter { f = fun _ (Snd class_b) ->
        let inter = EltSet.inter class_a class_b in
        if EltSet.is_empty inter then () else
        let Any x = EltSet.unsigned_min_elt inter in
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
        let inter = EltSet.filter {f = fun (type a) (y : a Elt.t) ->
          match Elt.polyeq x y with
          | Eq -> true (* keep in class but don't add to representatives *)
          | Diff ->
              match check_related a y x, check_related b y x with
              | Some rel_a, Some rel_b when Relation.equal rel_a rel_b ->
                  representatives := ReprMap.add y (EltThroughRel(x,rel_a)) !representatives;
                  true
              | Some _, Some _ -> false
              | _ -> failwith "Broken invariant"
        } inter in
        let has_class = Option.is_none (EltSet.is_singleton inter) in
        if has_class then (
          classes := ClassMap.add x (Snd inter) !classes
        );
        if has_class || has_value then (
          representatives := ReprMap.add x (EltThroughRel(x,Relation.identity)) !representatives;
        )
      } (add_singleton_classes b)
    } (add_singleton_classes a);
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
    let _ = ReprMap.for_all {f =
      fun (type a) (k : a Elt.t) (EltThroughRel(r,rel) : a elt_through_relation) ->
      match Elt.polyeq k r with
      | Diff ->
          let check1 = match ClassMap.find_opt r uf.classes with
          | None -> let message =
              Format.asprintf "Representative contains @[%a -(%a)-> %a@] but %a has a singleton class"
              Elt.pretty k Relation.pretty rel Elt.pretty r Elt.pretty r in
            errors := message :: !errors; false
          | Some (Snd c) -> if EltSet.mem k c then true else (
            let message =
              Format.asprintf "Representative contains @[%a -(%a)-> %a@] but %a is not in the class of %a {@[%a@]}"
              Elt.pretty k Relation.pretty rel Elt.pretty r Elt.pretty k Elt.pretty r pretty_class c in
            errors := message :: !errors; false) in
          let check2 = match ReprMap.find_opt r uf.representatives with
          | None -> true
          | Some(EltThroughRel(r', rel')) -> if Elt.polyeq r r' <> Diff then true else (
            let message =
              Format.asprintf "Representative is not flat: @[%a -(%a)-> %a -(%a)-> %a"
              Elt.pretty k Relation.pretty rel Elt.pretty r Relation.pretty rel' Elt.pretty r' in
            errors := message :: !errors; false)
          in check1 && check2
      | Eq ->
          if not (Relation.equal rel Relation.identity) then (
            let message =
              Format.asprintf "Self pointing elt in representatives whose relation isn't identity: %a -(%a)-> %a"
              Elt.pretty k Relation.pretty rel Elt.pretty r in
            errors := message :: !errors; false )
          else if not (ClassMap.mem r uf.classes || ValueMap.mem r uf.values) then (
            let message =
              Format.asprintf "Trivial repr in representatives: %a (self pointing elt not in dom(classes) or dom(values))"
              Elt.pretty k in
            errors := message :: !errors; false )
          else true } uf.representatives in
    (* check [uf.classes]:
       - all classe have cardinal >= 2
       - all classe have their min element has representatives
       - all classe contain their representatives
       - all elements of a class point to the classe's representative *)
    let _ = ClassMap.for_all { f = fun k (Snd c) ->
      let n = EltSet.cardinal c in
      let check1 = if n >= 2 then true else (
        let message =
          Format.asprintf "The class of %a has size %d (<2) : %a"
          Elt.pretty k n pretty_class c in
        errors := message :: !errors; false ) in
      let Any x = EltSet.unsigned_min_elt c in
      let check2 = if Elt.polyeq k x <> Diff then true else (
        let message =
          Format.asprintf "The representative of {@[<hv>%a@]} isn't minimal: it is %a, it should be %a"
          pretty_class c Elt.pretty k Elt.pretty x in
        errors := message :: !errors; false ) in
      let check3 = if EltSet.mem k c then true else (
        let message =
          Format.asprintf "The class of %a doesn't contain its representative: %a"
          Elt.pretty k pretty_class c in
        errors := message :: !errors; false) in
      let check4 = EltSet.for_all {f=fun elt ->
        match ReprMap.find_opt elt uf.representatives with
        | Some (EltThroughRel(x,_)) when (Elt.polyeq x k = Diff) ->
          let message =
            Format.asprintf "The class of %a {@[<hv>%a@]} contains %a, which points to a different elt: %a"
            Elt.pretty k pretty_class c Elt.pretty elt Elt.pretty x in
          errors := message :: !errors; false
        | None ->
          let message =
            Format.asprintf "The class of %a {@[<hv>%a@]} contains %a, which has no representative"
            Elt.pretty k pretty_class c Elt.pretty elt in
          errors := message :: !errors; false
        | Some _ -> true} c
      in check1 && check2 && check3 && check4} uf.classes in
    (* Check [uf.values]:
       - all elements of dom(uf.values) are representative
       - no value verifies [Value.is_top] *)
    let _ = ValueMap.for_all {f = fun k v ->
      let check1 = match ReprMap.find k uf.representatives with
      | EltThroughRel(k', _) when Elt.polyeq k k' <> Diff -> true
      | EltThroughRel(k', rel) -> let message =
            Format.asprintf "%a->%a appears in values, but isn't a repr (%a-(%a)->%a in representatives)"
            Elt.pretty k Value.pretty v Elt.pretty k Relation.pretty rel Elt.pretty k'  in
          errors := message :: !errors; false
      | exception Not_found -> let message =
            Format.asprintf "Missing non-trivial repr: %a->%a appears in values, but not in representatives"
            Elt.pretty k Value.pretty v in
          errors := message :: !errors; false
      in let check2 = if not (Value.is_top v) then true else
        let message = Format.asprintf "Top in values: %a->%a" Elt.pretty k Value.pretty v in
        errors := message :: !errors; false
      in
      check1 && check2 } uf.values in
    if !errors = []
    then Ok ()
    else Error ("BROKEN INVARIANTS:\n- " ^ String.concat "\n- " (List.rev !errors))
end
