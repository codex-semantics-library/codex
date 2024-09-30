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


module Log = Tracelog.Make(struct let category = "Domains.Constraints.NonRelational" end);;

(** The heart of the domain is a mapping from constraint variables to
   their basis. If a variable is absent from the mapping, it means
   that the variable is mapped to the "top" basis. Thus, the empty map
   denotes top, union of domains is intersection of mappings, and
   intersection of domains is union of the mappings. We do not need to
   represent the bottom domain because it is handled by
   constraint_domain. *)
module type Map_S = sig
  type t
  type key
  type value
  val top: t

  (** We use add when you don't know if the new value is going to be
     smaller than the previous one. This is necessary in forward
     evaluation because the constraints can already exist. *)
  val add: key -> value -> t -> t

  (* (** Use replace when you know that the new value is smaller than the
     previous one. Used when propagating assume conditions. *)
  val replace: key -> value -> t -> t *)
  val inter: t -> t -> t
  val join: t -> t -> t

  val includes_or_widen: t -> t -> bool * t

  val pretty: Format.formatter -> t -> unit
  val find: key -> t -> value
end

module type Key = sig
  include PatriciaTree.KEY
  val pretty: Format.formatter -> t -> unit
end

module type Value = Lattices.Sig.Join_Semi_Lattice_With_Inter_Bottom

module TC = Transfer_functions.Term

module Const_eval = Constraints.Const_eval

module Make
    (C: Constraints.Constraints_sig.Constraints)
    (B: Single_value_abstraction.Sig.Numeric_Basis)
    (V: Constraints.Relations.GROUP_ACTION
      with type binary = B.binary
      and type integer = B.integer
      and type boolean = B.boolean
      and type ('a, 'b) relation = ('a, 'b) C.Relation.t)
= struct
  module Constraints = C

  (** {2 Constraints, used as keys of the maps}            *)
  (*********************************************************)

  module BinaryConstraint = struct
    type t = TC.binary Constraints.t
    let pretty = Constraints.pretty
    let get_level = Constraints.level
    let to_int = Constraints.hash
  end

  module IntegerConstraint = struct
    type t = TC.integer Constraints.t
    let pretty = Constraints.pretty
    let get_level = Constraints.level
    let to_int = Constraints.hash
  end

  module BooleanConstraint = struct
    type t = TC.boolean Constraints.t
    let pretty = Constraints.pretty
    let get_level = Constraints.level
    let to_int = Constraints.hash
  end


  (** {2 Implementation of the maps using Patrica Trees} *)
  (*******************************************************)


  module MakeMap(Key:Key)(Value:Value)(*:Map_S with type key = Key.t
                                          and type value = Value.t *) =
  struct
    include PatriciaTree.MakeMap(Key)

    let pretty fmt x =
      Format.fprintf fmt "@[<v>%a@]"
      (pretty
        (fun fmt key value -> Format.fprintf fmt "%a -> %a" Key.pretty key Value.pretty value))
      x

    type nonrec t = Value.t t
    type key = Key.t
    type value = Value.t

    let top = empty
    let inter a b = idempotent_union (fun _ v1 v2 -> Value.inter v1 v2) a b

    (** Since absence of information is top, we use inter here. *)
    let join a b = idempotent_inter (fun _ v1 v2 -> Value.join v1 v2) a b

    let includes_or_widen a b =
      let is_included = ref true in
      let res = idempotent_inter (fun _ v1 v2 ->
          let bool, res = Value.includes_or_widen v1 v2 in
          is_included := !is_included && bool;
          res
        ) a b
      in
      !is_included, res

    (* Sligthtly more optimized versions when we use M3. *)
    let add key value map = insert key (function
      | None -> value
      | Some old -> Value.inter old value) map
  end

  module BooleanMap = MakeMap(BooleanConstraint)(B.Boolean_Lattice)
  module IntegerMap = MakeMap(IntegerConstraint)(B.Integer_Lattice)

  (** Binary is special as lattice operations requires to know the size. *)
  module BinaryMap = struct
    module Map = PatriciaTree.MakeMap(BinaryConstraint)

    let join a b = Map.idempotent_inter
      (fun (Constraints.Binary{size;_}) a b -> B.Binary_Lattice.join ~size a b) a b

    let includes_or_widen a b =
      let is_included = ref true in
      let res = Map.idempotent_inter (fun (Constraints.Binary{size;_} as cr) v1 v2 ->
          let bool, res = B.Binary_Lattice.includes_or_widen ~size ~previous:v1 v2 in
          Log.debug(fun p -> p "includes or widen %a %a %a res (%b,%a)"
                       (BinaryConstraint.pretty) cr
                       (B.Binary_Lattice.pretty ~size) v1
                       (B.Binary_Lattice.pretty ~size) v2
                       bool (B.Binary_Lattice.pretty ~size) res);
          is_included := !is_included && bool;
          res
        ) a b  in
      !is_included, res


    let inter a b = Map.idempotent_union
      (fun (Constraints.Binary{size;_}) a b -> B.Binary_Lattice.inter ~size a b) a b

    let add key value map =
      let Constraints.Binary{size; _} = key in
      Map.insert key (function
        | None -> value
        | Some old -> B.Binary_Lattice.inter ~size old value) map

    let pretty fmt x =
      Map.iter (fun (Constraints.Binary{size;_} as key) value ->
          Format.fprintf fmt "%a -> %a@\n"
            Constraints.pretty key
            (B.Binary_Lattice.pretty ~size) value) x

    let find = Map.find
    type key = BinaryConstraint.t
    type value = B.Binary_Lattice.t
    type t = value Map.t
    let top = Map.empty
  end

  module Const_eval = Const_eval.Make(Constraints)

  (** {2 Domain implementation}                   *)
  (************************************************)

  let name = "Base_Domain(" ^ B.name ^ ")"

  module B = B

  type binary = TC.binary Constraints.t
  type integer = TC.integer Constraints.t
  type boolean = TC.boolean Constraints.t

  type t = {
    boolean_map: BooleanMap.t;
    integer_map: IntegerMap.t;
    binary_map: BinaryMap.t;
  }

  let pretty fmt t =
    Format.fprintf fmt "@[<v>%a@ %a@ %a@]"
      BooleanMap.pretty t.boolean_map
      IntegerMap.pretty t.integer_map
      BinaryMap.pretty t.binary_map

  let equal = (==)

  let top = {
    boolean_map = BooleanMap.top;
    integer_map = IntegerMap.top;
    binary_map = BinaryMap.top
  }

  let inter a b = {
    boolean_map = BooleanMap.inter a.boolean_map b.boolean_map;
    integer_map = IntegerMap.inter a.integer_map b.integer_map;
    binary_map = BinaryMap.inter a.binary_map b.binary_map;
  }

  let join a b = {
    boolean_map = BooleanMap.join a.boolean_map b.boolean_map;
    integer_map = IntegerMap.join a.integer_map b.integer_map;
    binary_map = BinaryMap.join a.binary_map b.binary_map;
  }

  let includes_or_widen previous next =
    let boolean_cond,boolean_map =
      BooleanMap.includes_or_widen previous.boolean_map next.boolean_map
    in
    let integer_cond,integer_map =
      IntegerMap.includes_or_widen previous.integer_map next.integer_map
    in
    let binary_cond,binary_map =
      BinaryMap.includes_or_widen previous.binary_map next.binary_map
    in
    boolean_cond && integer_cond && binary_cond,
    { boolean_map; integer_map; binary_map }

  let boolean_get_cst x =
    match Const_eval.boolean x with
    | exception Const_eval.Empty -> B.Boolean_Lattice.bottom
    | exception Const_eval.Not_a_constant -> B.Boolean_Lattice.bottom
    | x -> B.Boolean_Lattice.singleton x
  let binary_get_cst x =
    let Constraints.Binary{size; _} = x in
    match Const_eval.binary x with
    | exception Const_eval.Empty -> B.Binary_Lattice.bottom ~size
    | exception Const_eval.Not_a_constant -> assert false
    | k -> B.Binary_Lattice.singleton ~size k
  let integer_get_cst x =
    match Const_eval.integer x with
    | exception Const_eval.Empty -> B.Integer_Lattice.bottom
    | exception Const_eval.Not_a_constant -> assert false
    | x -> B.Integer_Lattice.singleton x

  let query_parent (type tp tc) (term : tp Constraints.t) map (relation : (tc, tp) Constraints.Relation.t) =
    match term with
    | Constraints.Binary _ ->
        let value = BinaryMap.find term map.binary_map in
        V.apply_relation value relation BinaryMapping
    | Constraints.Integer _ ->
        let value = IntegerMap.find term map.integer_map in
        V.apply_relation value relation IntegerMapping
    | Constraints.Bool _ ->
        let value = BooleanMap.find term map.boolean_map in
        V.apply_relation value relation BooleanMapping

  let refine_parent replace (type tp vp tc vc)
      (term : tp Constraints.t) map
      (relation : (tc, tp) Constraints.Relation.t)
      (vc : vc) (proofc : (vc, tc) V.mapping) =
    match term with
    | Constraints.Binary _ ->
        let vp =
          try BinaryMap.find term map.binary_map
          with Not_found -> B.Binary_Lattice.top ~size:(Constraints.size_of term) in
        let vp = V.refine_relation vp vc relation BinaryMapping proofc in
        begin match vp with
        | Some vp -> (* Map.add performs intersections as needed *)
                    { map with binary_map = BinaryMap.add term vp map.binary_map }
        | None -> map
        end
    | Constraints.Integer _ ->
        let vp =
          try IntegerMap.find term map.integer_map
          with Not_found -> B.Integer_Lattice.top in
        let vp = V.refine_relation vp vc relation IntegerMapping proofc in
        begin match vp with
        | Some vp -> { map with integer_map = IntegerMap.add term vp map.integer_map }
        | None -> map
        end
    | Constraints.Bool _ ->
        let vp =
          try BooleanMap.find term map.boolean_map
          with Not_found -> B.Boolean_Lattice.top in
        let vp = V.refine_relation vp vc relation BooleanMapping proofc in
        begin match vp with
        | Some vp -> { map with boolean_map = BooleanMap.add term vp map.boolean_map }
        | None -> map
        end

  let query_binary term map =
    if Constraints.level term == -1
    then binary_get_cst term
    else let NodeThoughRelation(repr, rel) = Constraints.UnionFind.find_representative term in
    let Wrap(x, BinaryMapping) = query_parent repr map rel in
    x
  let query_boolean term map =
    if Constraints.level term == -1
    then boolean_get_cst term
    else let NodeThoughRelation(repr, rel) = Constraints.UnionFind.find_representative term in
    let Wrap(x, BooleanMapping) = query_parent repr map rel in
    x
  let query_integer term map =
    if Constraints.level term == -1
    then integer_get_cst term
    else let NodeThoughRelation(repr, rel) = Constraints.UnionFind.find_representative term in
    let Wrap(x, IntegerMapping) = query_parent repr map rel in
    x

  let [@inline] replace term v dom proof =
    if Constraints.level term == -1
    then [], dom
    else let NodeThoughRelation(repr, rel) = Constraints.UnionFind.find_representative term in
    [Constraints.Any repr], refine_parent false repr dom rel v proof
  let [@inline] replace_bool a v dom = replace a v dom BooleanMapping
  let [@inline] replace_integer a v dom = replace a v dom IntegerMapping
  let [@inline] replace_binary a v dom = replace a v dom BinaryMapping

  module Query = struct
    include B
    let boolean t x = query_boolean x t
    let integer t x = query_integer x t
    let binary ~size t x = query_binary x t
  end

  let [@inline] add term v dom proof =
    if Constraints.level term == -1
    then dom
    else let NodeThoughRelation(repr, rel) = Constraints.UnionFind.find_representative term in
    refine_parent false repr dom rel v proof
  let [@inline] add_boolean x value map =  add x value map BooleanMapping
  let [@inline] add_integer x value map =  add x value map IntegerMapping
  let [@inline] add_binary x value map = add x value map BinaryMapping


  (** {2 Transfer functions}              *)
  (****************************************)

  let [@inline always] copy query add ~src ~dst dom =
    let value = query src dom in
    add dst value dom

  let boolean_pretty dom fmt x =
    let res = query_boolean x dom in
    B.Boolean_Lattice.pretty fmt res

  let integer_pretty dom fmt x =
    let res = query_integer x dom in
    B.Integer_Lattice.pretty fmt res

  let binary_pretty ~size dom fmt x =
    let res = query_binary x dom in
    (* Format.fprintf fmt "%a -> %a" Constraints.pretty x *)
    (B.Binary_Lattice.pretty ~size) fmt res

  let nondet ~doma ~tupa ~domb ~tupb ~tupres =
    let copy_constraints tup dom =
      Immutable_array.fold_left2 (fun dom (Constraints.Any nondet) (Constraints.Any orig) ->
          match nondet,orig with
          | Constraints.((Binary _ as nondet)), Constraints.((Binary _ as orig)) ->
            copy query_binary add_binary ~src:orig ~dst:nondet dom
          | Constraints.((Integer _ as nondet)), Constraints.((Integer _ as orig)) ->
            copy query_integer add_integer ~src:orig ~dst:nondet dom
          | Constraints.((Bool _ as nondet)), Constraints.((Bool _ as orig)) ->
            copy query_boolean add_boolean ~src:orig ~dst:nondet dom
          | _ -> assert false
        ) dom tupres tup
    in
    let doma = copy_constraints tupa doma in
    let domb = copy_constraints tupb domb in
    let dom = join doma domb in
    dom

  (**************** Widened fixpoint step ****************)

  let widened_fixpoint_step ~previous ~previous_tup ~next ~next_tup includes ~res_tup =
    Log.debug (fun p -> p "widened_fixpoint_step: previous@\n%a next:@\n%a" pretty previous pretty next);
    let copy_constraints tup dom =
      Immutable_array.fold_left2 (fun dom (Constraints.Any nondet) (Constraints.Any orig) ->
          Log.debug (fun p -> p "Copying %a to %a" Constraints.pretty  orig Constraints.pretty nondet);
          match nondet,orig with
          | Constraints.((Binary _ as nondet)), Constraints.((Binary _ as orig)) ->
            copy query_binary add_binary ~src:orig ~dst:nondet dom
          | Constraints.((Integer _ as nondet)), Constraints.((Integer _ as orig)) ->
            copy query_integer add_integer ~src:orig ~dst:nondet dom
          | Constraints.((Bool _ as nondet)), Constraints.((Bool _ as orig)) ->
            copy query_boolean add_boolean ~src:orig ~dst:nondet dom
          | _ -> assert false
        ) dom res_tup tup
    in
    let previous_dom = copy_constraints previous_tup previous in
    let next_dom = copy_constraints next_tup next in
    let bool,dom = includes_or_widen previous_dom next_dom in
    dom,includes && bool

  (**************** Old fixpoint step ****************)

  let fixpoint_open() = ()

  type ('term, 'value) mapping =
    | MappingBinary : (TC.binary, B.binary) mapping
    | MappingInteger : (TC.integer, B.integer) mapping
    | MappingBoolean : (TC.boolean, B.boolean) mapping

  (** We use the property that ['term] is either
      {!TC.boolean}, {!TC.binary} or {!TC.integer} and that
      ['value] is the matching {!B.boolean}, {!B.binary} or {!B.integer}

      Full correctness will also rely on the property that objects at the
      same indexes of the arrays will always have the same type, so we still need
      a fallthrough case. *)
  type ('term,'value) widened_value = {
    (* constrain: 'term Constraints.t; *)
    widened: 'value;
    previous: 'value;
    has_converged: bool;
    proof: ('term, 'value) mapping;
  }

  (** With proper type function in OCaml, we could get rid of this constructor
      and instead use*)
  type widened_value_sum = AnyVW : (_, _) widened_value -> widened_value_sum [@@unboxed]

  let iteration_threshold = 20

  let fixpoint_step ~lvl ~iteration _actual_dom  ~actuals arg_dom ~args final_dom ~finals =
    let n = Immutable_array.length actuals in
    assert (n == Immutable_array.length args);
    assert (n == Immutable_array.length finals);

    let fixpoint_reached = ref true in
    let widened_values = Immutable_array.init n (fun i ->
        let Constraints.Any actual = Immutable_array.get actuals i in
        let Constraints.Any arg = Immutable_array.get args i in
        let Constraints.Any final = Immutable_array.get finals i in
        match actual,arg,final with
        | (Constraints.Binary {size=sizeactual} as actual), (Constraints.Binary {size=sizearg} as arg), (Constraints.Binary {size=sizefinal} as final) ->
          assert(sizeactual == sizearg && sizearg == sizefinal);
          let size = sizeactual in
          let actualv = query_binary actual arg_dom in
          let argv = query_binary arg arg_dom in
          let finalv = query_binary final final_dom in
          let joined = B.Binary_Lattice.join ~size actualv finalv in
          let (has_converged,widened) = B.Binary_Lattice.includes_or_widen ~size ~previous:argv joined in
          (* Codex_log.feedback "index %d: @[<v>actual: %a %a@ arg: %a %a@ final: %a %a@ has_converged: %b@ widened: %a@]"
          i
          Constraints.pretty actual (B.Binary_Lattice.pretty ~size) actualv
          Constraints.pretty arg (B.Binary_Lattice.pretty ~size) argv
          Constraints.pretty final (B.Binary_Lattice.pretty ~size) finalv
          has_converged (B.Binary_Lattice.pretty ~size) widened; *)
          fixpoint_reached := !fixpoint_reached && has_converged;
          AnyVW{ widened; previous=argv; has_converged; proof=MappingBinary }
        | (Constraints.Integer _ as actual), (Constraints.Integer _ as arg), (Constraints.Integer _ as final) ->
          let actualv = query_integer actual arg_dom in
          let argv = query_integer arg arg_dom in
          let finalv = query_integer final final_dom in
          let joined = B.Integer_Lattice.join actualv finalv in
          let (has_converged,widened) = B.Integer_Lattice.includes_or_widen ~previous:argv joined in
          fixpoint_reached := !fixpoint_reached && has_converged;
          AnyVW{ widened; previous=argv; has_converged; proof=MappingInteger }
        | (Constraints.Bool _ as actual), (Constraints.Bool _ as arg), (Constraints.Bool _ as final) ->
          let actualv = query_boolean actual arg_dom in
          let argv = query_boolean arg arg_dom in
          let finalv = query_boolean final final_dom in
          let joined = B.Boolean_Lattice.join actualv finalv in
          let (has_converged,widened) = B.Boolean_Lattice.includes_or_widen ~previous:argv joined in
          fixpoint_reached := !fixpoint_reached && has_converged;
          AnyVW{ widened; previous=argv; has_converged; proof=MappingBoolean }
        | _ -> failwith  "Typing issue"
      ) in
    !fixpoint_reached, (fun ~close res ->
        let res = Immutable_array.fold_left2 (fun dom (Constraints.Any constrain) (AnyVW widened_value) ->
            match constrain,widened_value with
            | Constraints.Binary _, { widened; previous; has_converged; proof=MappingBinary } ->
              (* To avoid non convergence (see t143.c):
                 if we haven't reached the global fixed point yet, but this specific
                 variable has, we keep the previous value (no shrinking yet). *)
              let value =
                if iteration<iteration_threshold || !fixpoint_reached || has_converged=false
                then widened else previous in
              add_binary constrain value dom
            | Constraints.Integer _, { widened; previous; has_converged; proof=MappingInteger } ->
              let value =
                if iteration<iteration_threshold || !fixpoint_reached || has_converged=false
                then widened else previous in
              add_integer constrain value dom
            | Constraints.Bool _, { widened; previous; has_converged; proof=MappingBoolean } ->
              let value =
                if iteration<iteration_threshold || !fixpoint_reached || has_converged=false
                then widened else previous in
              add_boolean constrain value dom
            | _ -> failwith "Typing issue"
          ) arg_dom res widened_values in
        (* Codex_log.feedback "fpstep bool %b @\nres %a @\narg_dom%a@\nfinal_dom%a" close pretty res pretty arg_dom pretty final_dom; *)
        res
      )

  (** {3 Forward propagation}                                      *)
  (*****************************************************************)

  module Domain_Arity = struct
    type 'r ar0 = t -> 'r -> t
    type ('a,'r) ar1 = t -> 'a -> 'r -> t
    type ('a,'b,'r) ar2 = t -> 'a -> 'b -> 'r -> t
    type ('a,'b,'c,'r) ar3 = t -> 'a -> 'b -> 'c -> 'r -> t
    type ('a,'r) variadic = t -> 'a list -> t
  end

  (** Defines transfer functions of the form dom[res <- f(a,b)]. *)

  let ar0_boolean f dom res = add_boolean res f dom
  let ar0_integer f dom res = add_integer res f dom
  let ar0_binary _str f dom res = add_binary res f dom

  let [@inline always] ar1 query_arg add_res f dom a res =
    let va = query_arg a dom in
    add_res res (f va) dom

  let ar1_boolean_boolean _str = ar1 query_boolean add_boolean
  let ar1_integer_integer = ar1 query_integer add_integer
  let ar1_binary_binary _str = ar1 query_binary add_binary
  let ar1_boolean_binary = ar1 query_boolean add_binary

  let [@inline always] ar2 query_arg add_res f dom a b res =
    let va = query_arg a dom in
    let vb = query_arg b dom in
    add_res res (f va vb) dom

  let ar2_boolean_boolean_boolean _str = ar2 query_boolean add_boolean
  let ar2_integer_integer_boolean = ar2 query_integer add_boolean
  let ar2_integer_integer_integer = ar2 query_integer add_integer
  let ar2_binary_binary_binary _str = ar2 query_binary add_binary
  let ar2_binary_binary_boolean _str = ar2 query_binary add_boolean

  module Boolean_Forward = struct
    let (||) = ar2_boolean_boolean_boolean "||" B.Boolean_Forward.(||)
    let (&&) = ar2_boolean_boolean_boolean "&&" B.Boolean_Forward.(&&)
    let (not) = ar1_boolean_boolean "not" B.Boolean_Forward.not
    let true_ = ar0_boolean B.Boolean_Forward.true_
    let false_  = ar0_boolean B.Boolean_Forward.false_
  end

  module Integer_Forward = struct
    let ile = ar2_integer_integer_boolean B.Integer_Forward.ile
    let ieq = ar2_integer_integer_boolean B.Integer_Forward.ieq
    let iconst k = ar0_integer (B.Integer_Forward.iconst k)
    let one  = iconst Z.one
    let zero = iconst Z.zero

    let ixor = ar2_integer_integer_integer B.Integer_Forward.ixor
    let ior  = ar2_integer_integer_integer B.Integer_Forward.ior
    let iand = ar2_integer_integer_integer B.Integer_Forward.iand
    let ishr = ar2_integer_integer_integer B.Integer_Forward.ishr
    let ishl = ar2_integer_integer_integer B.Integer_Forward.ishl
    let imod = ar2_integer_integer_integer B.Integer_Forward.imod
    let idiv = ar2_integer_integer_integer B.Integer_Forward.idiv
    let imul = ar2_integer_integer_integer B.Integer_Forward.imul
    let iadd = ar2_integer_integer_integer B.Integer_Forward.iadd
    let isub = ar2_integer_integer_integer B.Integer_Forward.isub
    let itimes k = ar1_integer_integer (B.Integer_Forward.itimes k)
  end

  module Binary_Forward = struct
    let biconst ~size k = ar0_binary (Z.to_string k) (B.Binary_Forward.biconst ~size k)
    let beq    ~size = ar2_binary_binary_boolean "beq" @@ B.Binary_Forward.beq ~size
    let biule  ~size = ar2_binary_binary_boolean "biule" @@ B.Binary_Forward.biule ~size
    let bisle  ~size = ar2_binary_binary_boolean "bisle"  @@ B.Binary_Forward.bisle ~size
    let biadd  ~size ~nsw ~nuw ~nusw = ar2_binary_binary_binary "biadd" @@ B.Binary_Forward.biadd ~size ~nsw ~nuw ~nusw
    let bisub  ~size ~nsw ~nuw ~nusw = ar2_binary_binary_binary "bisub" @@ B.Binary_Forward.bisub ~size ~nsw ~nuw ~nusw
    let bimul  ~size ~nsw ~nuw = ar2_binary_binary_binary "bimul" @@ B.Binary_Forward.bimul ~size ~nsw ~nuw
    let bshl   ~size ~nsw ~nuw = ar2_binary_binary_binary "bshl" @@ B.Binary_Forward.bshl ~size ~nsw ~nuw
    let bxor   ~size = ar2_binary_binary_binary "bxor" @@ B.Binary_Forward.bxor ~size
    let bor    ~size = ar2_binary_binary_binary "bor" @@ B.Binary_Forward.bor ~size
    let band   ~size = ar2_binary_binary_binary "band" @@ B.Binary_Forward.band ~size
    let bashr  ~size = ar2_binary_binary_binary "bashr" @@ B.Binary_Forward.bashr ~size
    let blshr  ~size = ar2_binary_binary_binary "blshr" @@ B.Binary_Forward.blshr ~size
    let bisdiv ~size = ar2_binary_binary_binary "bisdiv" @@ B.Binary_Forward.bisdiv ~size
    let biudiv ~size = ar2_binary_binary_binary "biudiv" @@ B.Binary_Forward.biudiv ~size
    let bismod ~size = ar2_binary_binary_binary "bismod" @@ B.Binary_Forward.bismod ~size
    let biumod ~size = ar2_binary_binary_binary "biumod" @@ B.Binary_Forward.biumod ~size

    let bsext ~size ~oldsize = ar1_binary_binary "bsext" @@ B.Binary_Forward.bsext ~size ~oldsize
    let buext ~size ~oldsize = ar1_binary_binary "buext" @@ B.Binary_Forward.buext ~size ~oldsize
    let bchoose ~size cond = ar1_binary_binary "bchoose" @@ B.Binary_Forward.bchoose ~size cond
    let bofbool ~size = ar1_boolean_binary @@ B.Binary_Forward.bofbool ~size
    let bconcat ~size1 ~size2 = ar2_binary_binary_binary "bconcat" @@ B.Binary_Forward.bconcat ~size1 ~size2
    let bextract ~size ~index ~oldsize = ar1_binary_binary "bextract" @@ B.Binary_Forward.bextract ~size ~index ~oldsize
    let valid ~size _ = assert false
    let valid_ptr_arith ~size _ = assert false
    let baddr ~size _  = assert false
    (* let biconst ~size k = ar0_binary (Z.to_string k) (B.Binary_Forward.biconst ~size k) *)
    let buninit ~size _ = assert false
    let bshift ~size ~offset ~max _ = assert false
    let bindex ~size _ = assert false
  end

  let binary_empty ~size = ar0_binary "empty" @@ B.Binary_Lattice.bottom ~size
  let integer_empty = ar0_integer B.Integer_Lattice.bottom
  let boolean_empty = ar0_boolean B.Boolean_Lattice.bottom

  let binary_unknown ~size = ar0_binary "unknown" (B.Binary_Lattice.top ~size)
  let integer_unknown = ar0_integer B.Integer_Lattice.top
  let boolean_unknown = ar0_boolean B.Boolean_Lattice.top



  (** {3 Backward propagation}                                      *)
  (******************************************************************)

  module Backward_Interp = struct
    let [@inline] add_new_value replace a va ((acc, map) as pair) =
      match va with
      | None -> pair
      | Some va ->
          let list, dom = replace a va map in
          list @ acc, dom

    let ar1 query_arg query_res replace dom a res f =
      let v = query_arg a dom in
      let vres = query_res res dom in
      let v = f v vres in
      ([], dom) |> add_new_value replace a v

    let ar1_boolean_boolean = ar1 query_boolean query_boolean replace_bool
    let ar1_integer_integer = ar1 query_integer query_integer replace_integer
    let ar1_binary_binary = ar1 query_binary query_binary replace_binary

    let ar2 query_arg query_res replace dom a b res f =
      let va = query_arg a dom in
      let vb = query_arg b dom in
      let vres = query_res res dom in
      let va, vb = f va vb vres in
      ([], dom) |> add_new_value replace a va |> add_new_value replace b vb

    let ar2_boolean_boolean_boolean = ar2 query_boolean query_boolean replace_bool
    let ar2_integer_integer_boolean = ar2 query_integer query_boolean replace_integer
    let ar2_integer_integer_integer = ar2 query_integer query_integer replace_integer
    let ar2_binary_binary_boolean = ar2 query_binary query_boolean replace_binary
    let ar2_binary_binary_binary = ar2 query_binary query_binary replace_binary

    (** Refinement of one term. *)
    let backward_interp dom (Constraints.Any x)=
      let open Constraints in
      let open TC in
      match x with
      | Bool{term=T2{tag=Or;a;b}} -> ar2_boolean_boolean_boolean dom a b x B.Boolean_Backward.(||)
      | Bool{term=T2{tag=And;a;b}} -> ar2_boolean_boolean_boolean dom a b x B.Boolean_Backward.(&&)
      | Bool{term=T2{tag=Ieq;a;b}} -> ar2_integer_integer_boolean dom a b x B.Integer_Backward.ieq
      | Bool{term=T2{tag=Ile;a;b}} -> ar2_integer_integer_boolean dom a b x B.Integer_Backward.ile
      | Bool{term=T1{tag=Not;a}} -> ar1_boolean_boolean dom a x B.Boolean_Backward.not
      | Bool{term=T0 _} -> [], dom

      | Integer{term=T2{tag=Iadd;a;b}} -> ar2_integer_integer_integer dom a b x B.Integer_Backward.iadd
      | Integer{term=T2{tag=Isub;a;b}} -> ar2_integer_integer_integer dom a b x B.Integer_Backward.isub
      | Integer{term=T2{tag=Imul;a;b}} -> ar2_integer_integer_integer dom a b x B.Integer_Backward.imul
      | Integer{term=T2{tag=Idiv;a;b}} -> ar2_integer_integer_integer dom a b x B.Integer_Backward.idiv
      | Integer{term=T2{tag=Imod;a;b}} -> ar2_integer_integer_integer dom a b x B.Integer_Backward.imod
      | Integer{term=T2{tag=Ishl;a;b}} -> ar2_integer_integer_integer dom a b x B.Integer_Backward.ishl
      | Integer{term=T2{tag=Ishr;a;b}} -> ar2_integer_integer_integer dom a b x B.Integer_Backward.ishr
      | Integer{term=T2{tag=Iand;a;b}} -> ar2_integer_integer_integer dom a b x B.Integer_Backward.iand
      | Integer{term=T2{tag=Ior;a;b}}  -> ar2_integer_integer_integer dom a b x B.Integer_Backward.ior
      | Integer{term=T2{tag=Ixor;a;b}} -> ar2_integer_integer_integer dom a b x B.Integer_Backward.ixor
      | Integer{term=T1{tag=Itimes k;a}} -> ar1_integer_integer dom a x (B.Integer_Backward.itimes k)
      | Integer{term=T0 _} -> [], dom


      | Bool{term=T2{tag=Beq size;a;b}} ->   ar2_binary_binary_boolean dom a b x @@ B.Binary_Backward.beq   ~size
      | Bool{term=T2{tag=Bisle size;a;b}} -> ar2_binary_binary_boolean dom a b x @@ B.Binary_Backward.bisle ~size
      | Bool{term=T2{tag=Biule size;a;b}} -> ar2_binary_binary_boolean dom a b x @@ B.Binary_Backward.biule ~size
      | Binary{term=T2{tag=Biadd{size;nsw;nuw;nusw};a;b}} ->  ar2_binary_binary_binary dom a b x @@ B.Binary_Backward.biadd  ~size ~nsw ~nuw ~nusw
      | Binary{term=T2{tag=Bisub{size;nsw;nuw;nusw};a;b}} ->  ar2_binary_binary_binary dom a b x @@ B.Binary_Backward.bisub  ~size ~nsw ~nuw ~nusw
      | Binary{term=T2{tag=Bimul{size;nsw;nuw};a;b}} ->  ar2_binary_binary_binary dom a b x @@ B.Binary_Backward.bimul  ~size ~nsw ~nuw
      | Binary{term=T2{tag=Bshl{size;nsw;nuw};a;b}} ->   ar2_binary_binary_binary dom a b x @@ B.Binary_Backward.bshl   ~size ~nsw ~nuw
      | Binary{term=T2{tag=Bisdiv(size);a;b}} ->         ar2_binary_binary_binary dom a b x @@ B.Binary_Backward.bisdiv ~size
      | Binary{term=T2{tag=Bismod(size);a;b}} ->         ar2_binary_binary_binary dom a b x @@ B.Binary_Backward.bismod ~size
      | Binary{term=T2{tag=Biudiv(size);a;b}} ->         ar2_binary_binary_binary dom a b x @@ B.Binary_Backward.biudiv ~size
      | Binary{term=T2{tag=Biumod(size);a;b}} ->         ar2_binary_binary_binary dom a b x @@ B.Binary_Backward.biumod ~size
      | Binary{term=T2{tag=Bashr(size);a;b}} ->          ar2_binary_binary_binary dom a b x @@ B.Binary_Backward.bashr  ~size
      | Binary{term=T2{tag=Blshr(size);a;b}} ->          ar2_binary_binary_binary dom a b x @@ B.Binary_Backward.blshr  ~size
      | Binary{term=T2{tag=Band(size);a;b}} ->           ar2_binary_binary_binary dom a b x @@ B.Binary_Backward.band   ~size
      | Binary{term=T2{tag=Bor (size);a;b}} ->           ar2_binary_binary_binary dom a b x @@ B.Binary_Backward.bor    ~size
      | Binary{term=T2{tag=Bxor(size);a;b}} ->           ar2_binary_binary_binary dom a b x @@ B.Binary_Backward.bxor   ~size
      | Binary{term=T2{tag=Bconcat(size1,size2);a;b}} -> ar2_binary_binary_binary dom a b x @@ B.Binary_Backward.bconcat ~size1 ~size2
      | Binary{term=T1{tag=Bsext(size);a=Binary{size=oldsize} as a}} -> ar1_binary_binary dom a x @@ B.Binary_Backward.bsext ~size ~oldsize
      | Binary{term=T1{tag=Buext(size);a=Binary{size=oldsize} as a}} -> ar1_binary_binary dom a x @@ B.Binary_Backward.buext ~size ~oldsize
      | Binary{term=T1{tag=Bextract{size;index;oldsize};a}} ->          ar1_binary_binary dom a x @@ B.Binary_Backward.bextract ~size ~index ~oldsize
      | Binary{term=T0 _} -> [], dom



      | Bool{term=(Mu_formal _|Inductive_var _ | Unknown _) } -> [], dom
      | Integer{term=(Mu_formal _|Inductive_var _ | Unknown _) } -> [], dom
      | Binary{term=(Mu_formal _|Inductive_var _ | Unknown _) } -> [], dom

      (* We can, but do not, propagate across mu. *)
      | Binary{term=Tuple_get(_,(Mu _ | Inductive_vars _))} -> [], dom
      | Integer{term=Tuple_get(_,(Mu _| Inductive_vars _))} -> [], dom
      | Bool{term=Tuple_get(_,(Mu _| Inductive_vars _))} -> [], dom

      (* We could also propagate across nondet. *)
      | Bool{term=Tuple_get(_,Nondet _)} -> [], dom
      | Binary{term=Tuple_get(_,Nondet _)} -> [], dom
      | Integer{term=Tuple_get(_,Nondet _)} -> [], dom


      | t -> Codex_log.fatal "backward_interp on %a" Constraints.pretty t

  end

  let backward_interp = Backward_Interp.backward_interp


  (** {3 Forward propagation}                                       *)
  (******************************************************************)

  let forward_interp dom (Constraints.Any x)=
    let open Constraints in
    let open TC in
    match x with
    | Bool{term=T0 _} -> dom
    | Binary{term=T0 _} -> dom
    | Integer{term=T0 _} -> dom

    | Bool{term=T2{tag=Or;a;b}} ->  Boolean_Forward.(||) dom a b x
    | Bool{term=T2{tag=And;a;b}} -> Boolean_Forward.(&&) dom a b x
    | Bool{term=T1{tag=Not;a}} ->   Boolean_Forward.not dom a x


    | Bool{term=T2{tag=Ieq;a;b}} -> Integer_Forward.ieq dom a b x
    | Bool{term=T2{tag=Ile;a;b}} -> Integer_Forward.ile dom a b x
    | Integer{term=T2{tag=Iadd;a;b}} -> Integer_Forward.iadd dom a b x
    | Integer{term=T2{tag=Isub;a;b}} -> Integer_Forward.isub dom a b x
    | Integer{term=T2{tag=Imul;a;b}} -> Integer_Forward.imul dom a b x
    | Integer{term=T2{tag=Idiv;a;b}} -> Integer_Forward.idiv dom a b x
    | Integer{term=T2{tag=Imod;a;b}} -> Integer_Forward.imod dom a b x
    | Integer{term=T2{tag=Ishl;a;b}} -> Integer_Forward.ishl dom a b x
    | Integer{term=T2{tag=Ishr;a;b}} -> Integer_Forward.ishr dom a b x
    | Integer{term=T2{tag=Iand;a;b}} -> Integer_Forward.iand dom a b x
    | Integer{term=T2{tag=Ior;a;b}}  -> Integer_Forward.ior  dom a b x
    | Integer{term=T2{tag=Ixor;a;b}} -> Integer_Forward.ixor dom a b x
    | Integer{term=T1{tag=Itimes k;a}} -> Integer_Forward.itimes k dom a x

    | Bool{term=T2{tag=Beq size;a;b}} ->   Binary_Forward.beq   ~size dom a b x
    | Bool{term=T2{tag=Bisle size;a;b}} -> Binary_Forward.bisle ~size dom a b x
    | Bool{term=T2{tag=Biule size;a;b}} -> Binary_Forward.biule ~size dom a b x
    | Binary{term=T2{tag=Biadd{size;nsw;nuw;nusw};a;b}} ->  Binary_Forward.biadd  ~size ~nsw ~nuw ~nusw dom a b x
    | Binary{term=T2{tag=Bisub{size;nsw;nuw;nusw};a;b}} ->  Binary_Forward.bisub  ~size ~nsw ~nuw ~nusw dom a b x
    | Binary{term=T2{tag=Bimul{size;nsw;nuw};a;b}} ->  Binary_Forward.bimul  ~size ~nsw ~nuw dom a b x
    | Binary{term=T2{tag=Bshl{size;nsw;nuw};a;b}} ->   Binary_Forward.bshl   ~size ~nsw ~nuw dom a b x
    | Binary{term=T2{tag=Bisdiv(size);a;b}} ->         Binary_Forward.bisdiv ~size           dom a b x
    | Binary{term=T2{tag=Bismod(size);a;b}} ->         Binary_Forward.bismod ~size           dom a b x
    | Binary{term=T2{tag=Biudiv(size);a;b}} ->         Binary_Forward.biudiv ~size           dom a b x
    | Binary{term=T2{tag=Biumod(size);a;b}} ->         Binary_Forward.biumod ~size           dom a b x
    | Binary{term=T2{tag=Bashr(size);a;b}} ->          Binary_Forward.bashr  ~size           dom a b x
    | Binary{term=T2{tag=Blshr(size);a;b}} ->          Binary_Forward.blshr  ~size           dom a b x
    | Binary{term=T2{tag=Band(size);a;b}} ->           Binary_Forward.band   ~size           dom a b x
    | Binary{term=T2{tag=Bor (size);a;b}} ->           Binary_Forward.bor    ~size           dom a b x
    | Binary{term=T2{tag=Bxor(size);a;b}} ->           Binary_Forward.bxor   ~size           dom a b x
    | Binary{term=T2{tag=Bconcat(size1,size2);a;b}} -> Binary_Forward.bconcat ~size1 ~size2  dom a b x
    | Binary{term=T1{tag=Bsext(size);a=Binary{size=oldsize} as a}} -> Binary_Forward.bsext ~size ~oldsize           dom a x
    | Binary{term=T1{tag=Buext(size);a=Binary{size=oldsize} as a}} -> Binary_Forward.buext ~size ~oldsize           dom a x
    | Binary{term=T1{tag=Bextract{size;index;oldsize};a}} ->          Binary_Forward.bextract ~size ~index ~oldsize dom a x

    | Bool{term=(Mu_formal _|Inductive_var _ | Unknown _) } -> dom
    | Integer{term=(Mu_formal _|Inductive_var _ | Unknown _) } -> dom
    | Binary{term=(Mu_formal _|Inductive_var _ | Unknown _) } -> dom

    (* We can, but do not, propagate across mu. *)
    | Binary{term=Tuple_get(_,(Mu _|Inductive_vars _))} -> dom
    | Integer{term=Tuple_get(_,(Mu _|Inductive_vars _))} -> dom
    | Bool{term=Tuple_get(_,(Mu _|Inductive_vars _))} -> dom

    (* We could also propagate across nondet. *)
    (* Maybe the "Context" should have a term, and we should propagate when it becomes bottom.  *)
    | Bool{term=Tuple_get(_,Nondet _)} -> dom
    | Binary{term=Tuple_get(_,Nondet _)} -> dom
    | Integer{term=Tuple_get(_,Nondet _)} -> dom


    | t -> Codex_log.fatal "forward_interp on %a" Constraints.pretty t


  (** {3 Backward propagation}                                      *)
  (******************************************************************)

  module MakeWorklist(Comp:sig
      val compare: Constraints.any -> Constraints.any -> int
    end) = struct

    module Heap = Binary_heap.Make(struct
        type t = Constraints.any
        let compare = Comp.compare
      end)

    let create = Heap.create
    let add = Heap.add
    let pop_minimum = Heap.pop_minimum
  end

  module BackwardWorklist = MakeWorklist(struct
      open Constraints
      let compare a b = (Any.get_id_int b) - (Any.get_id_int a)
    end)

  module ForwardWorklist = MakeWorklist(struct
      open Constraints
      let compare a b = (Any.get_id_int a) - (Any.get_id_int b)
    end)

  let backward_propagate dom worklist fwd_worklist (* constrain *) =
    let limit_count = ref 0 in
    let limit = 1000 in
    let exception Stop in
    (* Backward propagate until the worklist is empty. *)
    let rec loop dom =
      try
        let (Constraints.Any(max_elt) as max_any) = BackwardWorklist.pop_minimum worklist in
        ForwardWorklist.add fwd_worklist max_any;
        (* Possibly: also add the parents to the backward worklist;
           but make sure that elements are processed only once. *)
        (* Constraints.Parents.iter_on_parents max_elt (fun parent ->
           *     ForwardWorklist.add fwd_worklist parent cond
           *   ); *)
        (* TODO: If we find bottom, there is probably no need to
           propagate it further; by forward propagation, we should
           (probably? always?) find that the original condition is
           bottom. The only "legitimate" bottom should be the one
           bound to empty; backward propagation to bottom probably
           indicates an empty state. This may be important as backward
           propagation of bottom may propagate bottom up to the
           beginning of the program. *)
        let changed, newdom = backward_interp dom max_any in
        (* Codex_log.feedback "Backward propagation of %a:@\nold:%a@\nnew:%a" *)
        (*   Constraints.pretty max_elt pretty dom pretty newdom; *)
        let dom = newdom in
        changed |> List.iter (fun any ->
            let Constraints.Any x = any in
            assert (Constraints.Any.get_id_int any < Constraints.Any.get_id_int max_any);
            (* Note: because we usse a binary heap, if an element is
               already in the list, we add it twice. We could pair
               with a Hash to avoid that if this was problematic (it
               probably is not). *)
            BackwardWorklist.add worklist any;
            incr limit_count;
            if !limit_count > limit
            then (Codex_log.warning "Propagation: limit found"; raise Stop)
          );
        loop dom
      with Binary_heap.Empty | Stop -> dom
    in loop dom
  ;;

  let forward_propagate dom fwd_worklist =
    let rec loop dom =
      try
        let (Constraints.Any(max_elt) as max_any) = ForwardWorklist.pop_minimum fwd_worklist in
        let newdom = forward_interp dom max_any in
        (* Codex_log.feedback "Forward propagation of %a:@\nold:%a@\nnew:%a" *)
        (*   Constraints.pretty max_elt pretty dom pretty newdom; *)
        loop newdom
      with Binary_heap.Empty -> dom
    in loop dom
  ;;


  let assume dom cond =
    (* let old_dom = dom in *)
    assert(query_boolean cond dom = Lattices.Quadrivalent.Top);

    let dummy = Constraints.Any(Constraints.Build.Boolean.true_) in
    let worklist = (BackwardWorklist.create ~dummy 50) in
    let fwd_worklist = (ForwardWorklist.create ~dummy 50) in

    let boolean_map = BooleanMap.add cond Lattices.Quadrivalent.True dom.boolean_map in
    let dom = { dom with boolean_map } in
    BackwardWorklist.add worklist (Constraints.Any cond);
    let dom = backward_propagate dom worklist fwd_worklist in
    let dom = forward_propagate dom fwd_worklist in
    (* Codex_log.feedback "after assume %a:@\nold %a@\nnew %a"
        Constraints.pretty cond pretty old_dom pretty dom; *)
    Some dom

end
