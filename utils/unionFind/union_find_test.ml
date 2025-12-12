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

open Union_Find

let%test_module "TestUnionFind" = (module struct
  module Elt = struct
    type _ t = IntTerm : int -> int t [@@unboxed]

    let to_int (type a) (IntTerm n : a t) = n

    let polyeq (type a b) (IntTerm a : a t) (IntTerm b : b t) : (a,b) PatriciaTree.cmp =
       if a = b then Eq else Diff

    let pretty (type a) fmt (IntTerm x : a t) = Format.fprintf fmt "%d" x
  end

  module Relation (*: Parameters.GENERIC_GROUP*) = struct
    type ('a, 'b) t =
      | Equal : ('a, 'a) t
      | Add : Z.t -> (int, int) t

    let identity = Equal

    let compose (type a b c) (x: (b, c) t) (y: (a,b) t) : (a,c) t = match x, y with
      | Equal, _ -> y
      | _, Equal -> x
      | Add a, Add b ->
          let sum = Z.(a + b) in
          if sum = Z.zero then Equal else Add sum

    let inverse (type a b) (x : (a,b) t) : (b,a) t = match x with
      | Equal -> x
      | Add a -> Add Z.(-a)

    let equal (type a b) (x: (a, b) t) (y: (a,b) t) = match x, y with
      | Equal, Equal -> true
      | Add a, Add b -> Z.equal a b
      | _ -> false

    let pretty fmt (type a b) (x: (a, b) t) = match x with
      | Equal -> Format.fprintf fmt "Equal"
      | Add x -> Format.fprintf fmt "Add(%a)" Z.pp_print x

    let pretty_with_terms _ _ _ _ _ _ = assert false
  end

  module Value (*: Parameters.GENERIC_VALUE*) = struct
    type ('a,'b) relation = ('a,'b) Relation.t
    type _ t =
      | Empty : int t
      | Interval : Z.t option * Z.t option -> int t

    let opt_pretty fmt = function
      | None -> Format.fprintf fmt "inf"
      | Some d -> Format.fprintf fmt "%a" Z.pp_print d

    let pretty (type a) fmt : a t -> unit = function
      | Empty -> Format.fprintf fmt "Empty"
      | Interval(a,b) -> Format.fprintf fmt "[%a,%a]" opt_pretty a opt_pretty b

    let equal (type a) (x: a t) (y: a t) = match x, y with
      | Empty, Empty -> true
      | Interval(xm,xM), Interval(ym, yM) -> xm = ym && xM = yM
      | _ -> false

    let interval min max =
      match min, max with
      | None, _ -> Interval(None, max)
      | _, None -> Interval(min, None)
      | Some min, Some max -> if Z.leq min max then Interval (Some min, Some max) else Empty

    let apply (type a b) (x : a t) (b : (a,b) relation) : b t =
      match b with
      | Equal -> x
      | Add n -> match x with
          | Empty -> Empty
          | Interval (min, max) ->
              let map = Option.map (fun x -> Z.(x + n)) in
              interval (map min) (map max)

    let opt_or f a b = match a, b with
      | Some a, Some b -> Some (f a b)
      | (Some _ as a), None
      | None, (Some _ as a) -> a
      | None, None -> None

    let opt_and f a b = match a, b with
      | Some a, Some b -> Some (f a b)
      | _ -> None

    let meet (type a) (a : a t) (b : a t) : a t =
      match a, b with
      | Empty, _ -> Empty
      | _, Empty -> Empty
      | Interval (min_a, max_a), Interval (min_b, max_b) ->
          let new_min = opt_or max min_a min_b in
          let new_max = opt_or min max_a max_b in
          interval new_min new_max

    let join (type a) (a : a t) (b : a t) : a t =
      match a, b with
      | Empty, x | x, Empty -> x
      | Interval (min_a, max_a), Interval (min_b, max_b) ->
          let new_min = opt_and min min_a min_b in
          let new_max = opt_and max max_a max_b in
          interval new_min new_max

    let is_top (type a) (x: a t) =
      match x with
      | Interval (None, None) -> true
      | _ -> false

    let top = Interval (None, None)
  end

  module ImpNode = Imperative.MakeValuedNode(Elt)(Relation)(Value)
  module Imp = Imperative.GenericRelationalValued(ImpNode)
  module Fun = Functional.GenericRelationalValued(Elt)(Relation)(Value)

  let relation_of_int = function
    | 0 -> Relation.Equal
    | n -> Relation.Add (Z.of_int n)

  module IntSet = Set.Make(Int)

  let rec remove_duplicate cast set = function
    | [] -> [], set
    | t::q when IntSet.mem (cast t) set -> remove_duplicate cast set q
    | t::q ->
        let set = IntSet.add (cast t) set in
        let q, set = remove_duplicate cast set q in
        t::q, set

  (** Transform a list of list into a partition (list of list with no duplicates) *)
  let rec make_partition cast set = function
    | [] -> []
    | t::q -> let t, set = remove_duplicate cast set t in
              let q = make_partition cast set q in
              if t = [] then q else t::q

  let gen_partition =
    QCheck.map
      ~rev:(fun x -> x)
      (make_partition (fun (i,_,_) -> i) IntSet.empty)
      QCheck.(small_list (small_list (triple pos_int (option int) (option int))))

  let to_z = Option.map Z.of_int

  (** Initialize functional union-find from generator *)
  let fun_model_from_gen gen =
    let rec create_class hd uf = function
      | [] -> uf
      | (i, rel, _) :: q ->
          let rel = match rel with None -> Relation.Equal | Some x -> relation_of_int x in
          match Fun.union uf hd (IntTerm i) rel with
          | Ok uf -> create_class hd uf q
          | Error _rel -> failwith "Duplicate union"
    in
    List.fold_left (fun uf classe -> match classe with
      | [] -> uf
      | (i, im, iM)::q ->
          let hd = Elt.IntTerm i in
          let uf = Fun.add_value uf hd (Value.interval (to_z im) (to_z iM)) in
          create_class hd uf q

    ) Fun.empty gen

  (** Initialize imperative union-find from generator *)
  let imp_model_from_gen gen =
    let rec create_class hd = function
    | [] -> []
    | (i, rel, _) :: q ->
        let rel = match rel with None -> Relation.Equal | Some x -> relation_of_int x in
        let node = ImpNode.make_node (IntTerm i) Value.top in
        match Imp.union hd node rel with
        | Ok () -> node :: create_class hd q
        | Error _rel -> failwith "Duplicate union"
    in
    List.map (function
      | [] -> []
      | (i, im, iM)::q ->
          let hd = Elt.IntTerm i in
          let hd = ImpNode.make_node hd (Value.interval (to_z im) (to_z iM)) in
          hd::(create_class hd q)
    ) gen

  let payload = ImpNode.payload

  (** Checks functional and imperative models agree *)
  let check_models_agree fun_model imp_model =
    let check_class hd t =
        let imp_rel = Option.get (Imp.check_related t hd) in
        let fun_rel = Option.get (Fun.check_related fun_model (payload t) (payload hd)) in
        if Relation.equal imp_rel fun_rel then true
        else (
          QCheck2.Test.fail_reportf
            "Different relations between %a and %a :\n %a (imperative) and %a (functional)@."
            Elt.pretty (payload t) Elt.pretty (payload hd)
            Relation.pretty imp_rel Relation.pretty fun_rel
        )
    in
    List.for_all (function
      | [] -> true
      | hd::rest -> List.for_all (check_class hd) rest)
    imp_model

  let check_invariants fun_model =
    match Fun.check_invariants fun_model with
    | Ok () -> true
    | Error msg -> QCheck.Test.fail_reportf "ERROR WITH STRUCTURE:@.  @[%a@]@.%s"
          Fun.pretty_debug fun_model msg

  let list_snd = function
    | [] -> failwith "Empty list"
    | t::[] -> t
    | _::t::_ -> t

  (** Performs unions: recursively unites the first two classes of [imp_model]
      using the relation given by [unions].
      Stops when [imp_model] or [unions] runs out *)
  let rec make_unions fun_model imp_model unions = match imp_model, unions with
    | [], _ | _, [] | [_], _ -> fun_model, imp_model
    | cls_a::cls_b::classes, i::is ->
        let elt_a = list_snd cls_a in
        let elt_b = list_snd cls_b in
        let relation = relation_of_int i in
        match Fun.union fun_model (payload elt_a) (payload elt_b) relation with
        | Error _ -> failwith "Non-disjoint union"
        | Ok fun_model ->
        match Imp.union elt_a elt_b relation with
        | Ok () -> make_unions fun_model ((cls_a @ cls_b) :: classes) is
        | Error _ -> failwith "Non-disjoint union, but only in imperative"

  let test_agree = QCheck.Test.make ~count:1000 ~name:"Functional and Imperative Build" gen_partition
    (fun part -> check_models_agree
      (fun_model_from_gen part) (imp_model_from_gen part))
  let () = QCheck.Test.check_exn test_agree

  let test_invariants = QCheck.Test.make ~count:1000 ~name:"Functional Build Invariants" gen_partition
    (fun part -> check_invariants (fun_model_from_gen part))
  let () = QCheck.Test.check_exn test_invariants

  let test_union = QCheck.Test.make ~count:1000 ~name:"Functional and Imperative Union"
    QCheck.(pair gen_partition (small_list int))
    (fun (part, unions) ->
      let fun_model, imp_model = make_unions (fun_model_from_gen part) (imp_model_from_gen part) unions in
      check_invariants fun_model && check_models_agree fun_model imp_model)
  let () = QCheck.Test.check_exn test_union

  let test_join = QCheck.Test.make ~count:1000 ~name:"Functional Random Joins"
    QCheck.(pair gen_partition gen_partition)
    (fun (p1, p2) ->
        let model1 = fun_model_from_gen p1 in
        let model2 = fun_model_from_gen p2 in
        let join1 = Fun.join model1 model2 in
        let join2 = Fun.join_test model1 model2 in
        check_invariants join1 && check_invariants join2 &&
        if Fun.equal join1 join2 then true
        else QCheck.Test.fail_report "Joins don't match")
  let () = QCheck.Test.check_exn test_join

  let test_join_with_previous = QCheck.Test.make ~count:1000 ~name:"Functional Joins with previous state"
    QCheck.(pair gen_partition (small_list int))
    (fun (part, unions) ->
      let fun_model = fun_model_from_gen part in
      let fun_model', _ = make_unions fun_model (imp_model_from_gen part) unions in
      let join = Fun.join fun_model fun_model' in
      let join2 = Fun.join_test fun_model fun_model' in
      check_invariants join && check_invariants join2 &&
      if Fun.equal join fun_model then true
      else QCheck.Test.fail_reportf "Joins don't match:@.1: @[%a@]@.2: @[%a@]@.3: @[%a@]@."
        Fun.pretty_debug fun_model Fun.pretty_debug fun_model' Fun.pretty_debug join &&
      if Fun.equal join2 fun_model then true
      else QCheck.Test.fail_reportf "Join_tests don't match:@.1: @[%a@]@.2: @[%a@]@.3: @[%a@]@."
        Fun.pretty_debug fun_model Fun.pretty_debug fun_model' Fun.pretty_debug join2
      )
  let () = QCheck.Test.check_exn test_join_with_previous

  let test_join_two_different_fun (part, unions1, unions2) =
    let fun_model = fun_model_from_gen part in
    let fun_model1, _ = make_unions fun_model (imp_model_from_gen part) unions1 in
    let fun_model2, _ = make_unions fun_model (List.rev (imp_model_from_gen part)) unions2 in
    let join = Fun.join fun_model1 fun_model2 in
    let join2 = Fun.join_test fun_model1 fun_model2 in
    check_invariants join &&
    check_invariants join2 &&
    if Fun.equal join join2 then true
    else QCheck.Test.fail_reportf "Joins don't match:@.1: @[%a@]@.2: @[%a@]@.3: @[%a@]@.4: @[%a@]@.5: @[%a@]@."
      Fun.pretty_debug fun_model Fun.pretty_debug fun_model1 Fun.pretty_debug fun_model2
      Fun.pretty_debug join Fun.pretty_debug join2
    && Fun.subseteq fun_model join
    && Fun.subseteq fun_model join2

  let test_join_two_different = QCheck.Test.make ~count:1000 ~name:"Functional Joins after two different unions"
    QCheck.(triple gen_partition (small_list int) (small_list int))
    test_join_two_different_fun
  let () = QCheck.Test.check_exn test_join_two_different
end)
