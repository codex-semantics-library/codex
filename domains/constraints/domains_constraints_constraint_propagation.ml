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

(* Note: if true, very slow and memory hungry on solitaire,2048... *)
let option_lazy_copy_conditionmaps = false;;

(* Slightly changes performances, which may indicate that we should be
   using immutable maps instead. Note: we do now. *)
let option_push_copy_conditionmaps = false;;

(* Pushing and popping allows to ditch the partial results during
   fixpoint computation. Not pushing and popping everytime means that
   informations about constraints that are not used anymore stays in
   the map. It makes some benchmarks much slower and memory
   hungly. Maybe this could be a solution if we used a weak key map. *)
let option_push_and_pop = true;;

(* If we push and pop, should we do it on every restart? If we don't,
   it is not as slow as not pushing at all, but it is still slower
   than doing it. *)
let option_push_and_pop_on_restart = true;;

(* If true, when two terms with different conditions enter the
   worklist, their condition is merged. Else the term is twice in the
   list. Does not show a big difference in precision or execution time
   in my benchmarks. *)
let option_worklist_merge_condition = true

(* Allows to gain precision in some benchmarks; but it is really slow
   in others, so we don't activate it by default. Probably smashing arrays
   would solve the problem. *)
let option_propagate_across_nondet = false

module TC = Transfer_functions.Term;;

module Sum = struct
  type ('a,'b,'c) t =
    | Boolean of 'a
    | Integer of 'b
    | Binary of 'c
      
end;;

module Constr = Constraints

module Make
    (Constraints:Constr.Constraints_sig.Constraints
     with type Condition.t = Cudd.Man.d Cudd.Bdd.t
     (* with type Condition.t = Constraints_condition.ConditionDom.t *)
    )
    (B:Single_value_abstraction.Sig.Binary_Integer_Basis) =
struct

  let name = "Domain_term_propagating_basis(" ^ B.name ^ ")"

  module Constraints = Constraints;;

  (* We use a single store to put all constraints; we never perform
     intersection etc. because everything is already in the store.

     We have a map constraint -> condition -> value (instead of
     condition -> constraint -> value) to maximize sharing between the
     different conditions. *)
  module MakeConditionMap = Constr.Condition.CuddPIPartition.MakeConditionMap;;
  module type SConditionMap = Constr.Condition.CuddPIPartition.SConditionMap;;
  module ConditionMap = Constr.Condition.CuddPIPartition.ConditionMap;;
  module Condition = Constr.Condition.CuddPIPartition.Condition;;

  (* module MakeConditionMap = Constraints_condition.DomPIPartition.MakeConditionMap;;
   * module type SConditionMap = Constraints_condition.DomPIPartition.SConditionMap;;
   * module ConditionMap = Constraints_condition.DomPIPartition.ConditionMap;;
   * module Condition = Constraints_condition.DomPIPartition.Condition;; *)
  

  (* Fully path-sensitive version. Quite costly, but improves precision in some cases.
     (Is worse in others, which is not expected). *)
  (* TODO: This should be a parameter of the functor. *)
  (* module MakeConditionMap = Constraints_condition.CuddPSPartition.MakeConditionMap;;
   * module type SConditionMap = Constraints_condition.CuddPSPartition.SConditionMap;;
   * module ConditionMap = Constraints_condition.CuddPSPartition.ConditionMap;;
   * module Condition = Constraints_condition.CuddPSPartition.Condition;; *)

  module BinaryConditionMap = struct
    module M = MakeConditionMap(struct
        include B.Binary_Lattice
        let pretty = pretty ~size:128
      end
      );;
    include M
    let find_join ~size cm cond = find ~join:(B.Binary_Lattice.join ~size) ~bottom:(B.Binary_Lattice.bottom ~size) cm cond
  end
  module IntegerConditionMap = struct
    include MakeConditionMap(B.Integer_Lattice);;
    let find_join cm cond = find ~join:B.Integer_Lattice.join ~bottom:B.Integer_Lattice.bottom cm cond
  end
  module BooleanConditionMap = struct
    include MakeConditionMap(B.Boolean_Lattice)
    let find_join cm cond = find ~join:B.Boolean_Lattice.join ~bottom:B.Boolean_Lattice.bottom cm cond
  end
    ;;

  module BinaryConstraint = struct
    include (Constraints:module type of Constraints with type 'a t := 'a Constraints.t)
    type t = TC.binary Constraints.t
    let to_int = Constraints.hash        
  end

  module IntegerConstraint = struct
    include (Constraints:module type of Constraints with type 'a t := 'a Constraints.t)
    type t = TC.integer Constraints.t
    let to_int = Constraints.hash        
  end

  module BooleanConstraint = struct
    include (Constraints:module type of Constraints with type 'a t := 'a Constraints.t)    
    type t = TC.boolean Constraints.t
    let to_int = Constraints.hash
  end


  module type State = sig
    val push: unit -> unit
    val pop: unit -> unit
    val get_boolean: TC.boolean Constraints.t -> BooleanConditionMap.t
    val get_integer: TC.integer Constraints.t -> IntegerConditionMap.t
    val get_binary: TC.binary Constraints.t -> BinaryConditionMap.t
    val set_boolean: TC.boolean Constraints.t -> BooleanConditionMap.t -> unit
    val set_integer: TC.integer Constraints.t -> IntegerConditionMap.t -> unit
    val set_binary: TC.binary Constraints.t -> BinaryConditionMap.t -> unit
  end

  (* One implementation of State. Slower than StateMap. *)
  module StateHashtblList:State = struct

    module BinaryConstraintHashtbl = Ephemeron.K1.Make(BinaryConstraint);;
    module IntegerConstraintHashtbl = Ephemeron.K1.Make(IntegerConstraint);;
    module BooleanConstraintHashtbl = Ephemeron.K1.Make(BooleanConstraint);;

    type state = {
      boolean: BooleanConditionMap.t BooleanConstraintHashtbl.t;
      integer: IntegerConditionMap.t IntegerConstraintHashtbl.t;
      binary: BinaryConditionMap.t BinaryConstraintHashtbl.t;
    }

    let statel = ref [];;
    let push_fresh() = 
      let state = {
        boolean = BooleanConstraintHashtbl.create 30;
        integer = IntegerConstraintHashtbl.create 30;
        binary = BinaryConstraintHashtbl.create 30;
      } in
      statel := state::(!statel)
    ;;
    push_fresh();;    

    let push_copy() =
      let old = List.hd (!statel) in
      let state = {
        boolean = BooleanConstraintHashtbl.copy old.boolean;
        integer = IntegerConstraintHashtbl.copy old.integer;
        binary = BinaryConstraintHashtbl.copy old.binary;
      } in
      statel := state::(!statel)
    ;;

    let push = if option_push_copy_conditionmaps then push_copy else push_fresh;;

    let pop() = statel := List.tl !statel

    (**************** State access functions. ****************)

    let get_boolean x =
      let rec loop = function
        | [] -> BooleanConditionMap.create_partial
        | a::b ->
          try BooleanConstraintHashtbl.find a.boolean x
          with Not_found ->
            let res = loop b in
            if option_lazy_copy_conditionmaps then
              BooleanConstraintHashtbl.replace a.boolean x res;
            res
      in loop !statel
    ;;

    let rget_integer x l =
      let rec loop = function
        | [] -> IntegerConditionMap.create_partial
        | a::b ->
          try IntegerConstraintHashtbl.find a.integer x
          with Not_found ->
            let res = loop b in
            if option_lazy_copy_conditionmaps then
              IntegerConstraintHashtbl.replace a.integer x res;
            res
      in loop l
    ;;
    let get_integer x = rget_integer x !statel
        
    let set_boolean x value = BooleanConstraintHashtbl.replace (List.hd !statel).boolean x value
    let set_integer x value =
      (* Codex_log.feedback "Set integer cm %a %a" Constraints.pretty x IntegerConditionMap.pretty value; *)
      IntegerConstraintHashtbl.replace (List.hd !statel).integer x value

    let get_binary _ = assert false
    let set_binary _ = assert false
  end

  module StateMap:State = struct
    (* MAYBE: I think the most efficient structure would be an AMT
       (array map trie) with in-place updates, but a lazy copy
       operation. Only operations are find, add, and copy. Currently,
       Okasakimap is the best choice. *)
    module BinaryConstraintMap = Okasakimap.Make(BinaryConstraint);;
    module IntegerConstraintMap = Okasakimap.Make(IntegerConstraint);;
    module BooleanConstraintMap = Okasakimap.Make(BooleanConstraint);;

    type state = {
      mutable boolean: BooleanConditionMap.t BooleanConstraintMap.t;
      mutable integer: IntegerConditionMap.t IntegerConstraintMap.t;
      mutable binary: BinaryConditionMap.t BinaryConstraintMap.t;
    }

    let statel = ref [
        {boolean = BooleanConstraintMap.empty;
         integer = IntegerConstraintMap.empty;
         binary = BinaryConstraintMap.empty;
        }
      ];;

    let push() =
      let old = List.hd (!statel) in
      let state = {
        boolean = old.boolean;
        integer = old.integer;
        binary = old.binary;
      } in
      statel := state::(!statel)
    ;;

    let pop() = statel := List.tl !statel

    (**************** State access functions. ****************)

    let get_boolean x =
      try  BooleanConstraintMap.find x (List.hd !statel).boolean
      with Not_found -> BooleanConditionMap.create_partial
    ;;
    let get_integer x =
      try  IntegerConstraintMap.find x (List.hd !statel).integer
      with Not_found -> IntegerConditionMap.create_partial
    ;;
    let get_binary x =
      try  BinaryConstraintMap.find x (List.hd !statel).binary
      with Not_found -> BinaryConditionMap.M.create_partial
    ;;
    
    let set_boolean x value =
      let state = List.hd !statel in
      state.boolean <- BooleanConstraintMap.add x value state.boolean
    let set_integer x value =
      let state = List.hd !statel in
      state.integer <- IntegerConstraintMap.add x value state.integer
    let set_binary x value =
      let state = List.hd !statel in
      (* Codex_log.feedback "Set binary cm %a %a" Constraints.pretty x BinaryConditionMap.pretty value; *)
      state.binary <- BinaryConstraintMap.add x value state.binary
    
  end

  (* StateMap is much faster. *)
  (* module State:State = StateHashtblList *)
  module State:State = StateMap

  (* The domain does not store anything except the path condition. 
     Everything is in State, the store. *)
  type domain = { condition: TC.boolean Constraints.t; } [@@unboxed]
  type t = domain;;

  let get_bdd x = match x.condition with
    | Constraints.Bool{bdd} -> bdd
  ;;


  (**************** Domain interaction functions. ****************)


  type binary = TC.binary Constraints.t  
  type integer = TC.integer Constraints.t
  type boolean = TC.boolean Constraints.t

  let pretty fmt x = Constraints.pretty fmt x.condition
  let equal a b = Constraints.equal a.condition b.condition
  let top = { condition = Constraints.Build.Boolean.true_ }

  let inter a b = assert false; (* No longer used. *)
    let condition = Constraints.Build.Boolean.(&&) a.condition b.condition in
    (* The way I use assume, one of the constraint is always more
       precise than the other. This is important if we want to use the
       efficient constraint propagation based on dominators. *)
    if Codex_config.assume_simple_asts () then
      assert(Constraints.equal a.condition condition || Constraints.equal b.condition condition);
    (* This assertion is false because I sometimes add assertions on expressions. 
       It could probably be made true. *)
    (* assert((Constraints.equal a.condition b.condition)
     *        || (Constraints.equal a.condition condition && Constraints.equal b.condition top.condition)
     *        || (Constraints.equal b.condition condition && Constraints.equal a.condition top.condition)); *)
    { condition }

  let join a b = 
    { condition = Constraints.Build.Boolean.(||) a.condition b.condition }

  (**************** Forward evaluation. ****************)

  module Domain_Arity = struct
    type 'r ar0 = t -> 'r -> t
    type ('a,'r) ar1 = t -> 'a -> 'r -> t
    type ('a,'b,'r) ar2 = t -> 'a -> 'b -> 'r -> t
    type ('a,'b,'c,'r) ar3 = t -> 'a -> 'b -> 'c -> 'r -> t
    type ('a,'r) variadic = t -> 'a list -> t
  end



  module No_Reevaluation = struct
    let get_query_integer x cond = IntegerConditionMap.find_join (State.get_integer x) cond
    let get_query_boolean x cond = BooleanConditionMap.find_join (State.get_boolean x) cond
    let get_query_binary x cond =
      let Constraints.Binary{size} = x in
      BinaryConditionMap.find_join ~size (State.get_binary x) @@ cond

    let ar2
        (type a) get_cma gqa (module La:SConditionMap with type value = a) ~joina ~bottoma ~intera
        (type b) get_cmb gqb (module Lb:SConditionMap with type value = b) ~joinb ~bottomb ~interb
        (type res)  get_cmres (module Lres:SConditionMap with type value = res) ~interres
        set_cmres
        f dom a b (* ctx *) res =
      let condition = get_bdd dom in
      let cma = get_cma a in
      let cmb = get_cmb b in
      let cmres = get_cmres res in            
      let cmres' =
        ConditionMap.ar2 (module La) ~joina ~bottoma (module Lb) ~joinb ~bottomb (module Lres) ~interres condition f
          cma cmb cmres
      in
      set_cmres res cmres';
      dom
    ;;

    let ar1
        (type a) get_cma gqa (module La:SConditionMap with type value = a) ~joina ~bottoma ~intera
        (type res)  get_cmres (module Lres:SConditionMap with type value = res) ~interres
        set_cmres
        f dom a (* ctx *) res =
      let condition = get_bdd dom in
      let cma = get_cma a in
      let cmres = get_cmres res in            
      let cmres' =
        ConditionMap.ar1 (module La) ~joina ~bottoma (module Lres) ~interres condition f
          cma cmres
      in
      set_cmres res cmres';
      dom
    ;;

    let ar0
        (type res)  get_cmres (module Lres:SConditionMap with type value = res) ~interres
        set_cmres
        f dom (* ctx *) res =
      let condition = get_bdd dom in
      let cmres = get_cmres res in            
      let cmres' =
        ConditionMap.ar0 (module Lres) ~interres condition f
          cmres
      in
      set_cmres res cmres';
      dom
    ;;

    
  end

  module With_Reevaluation = struct
    (* This version allows to re-evaluate a constraint when we cannot
       find the result of its evaluation. Currently this happens when
       we perform extraction when serializing the argument; this can
       also happen for constants.

       Note that we do not re-compute the result with ConditionMap.ar,
       and we use refine + find instead, so this may be less precise
       in theory *)

    (* Get the value of a constraint for a condition, re-compute it if
       needed, and store the result of the recomputation. *)
    let rec get_query_integer constr cond =
      let cm = State.get_integer constr in
      try 
        IntegerConditionMap.find_join cm cond
      with Condition_map.Never_refined ->
        (* Note: it is usual to reevaluate constants, because of rewrites. *)
        if Constraints.level constr >= 0 then        
          Codex_log.warning "I need to re-evaluate %a (whose computation was not saved)" Constraints.pretty constr;
        let res = match constr with
        | Constraints.(Integer{term=T0{tag=TC.Iconst k}}) ->
          B.Integer_Forward.iconst k
        | Constraints.(Integer{term=T2{
            tag=TC.Imod; a;
            b=Integer{term=T0{tag=TC.Iconst k}}}}) ->
          let av = get_query_integer a cond in
          B.Integer_Forward.imod av (B.Integer_Forward.iconst k)
        | Constraints.(Integer{term=T2{
            tag=TC.Iand; a;
            b=Integer{term=T0{tag=TC.Iconst k}}}}) ->
          let av = get_query_integer a cond in
          B.Integer_Forward.iand av (B.Integer_Forward.iconst k)
        | Constraints.(Integer{term=T2{
            tag=TC.Ishr; a;
            b=Integer{term=T0{tag=TC.Iconst k}}}}) ->
          let av = get_query_integer a cond in
          B.Integer_Forward.ishr av (B.Integer_Forward.iconst k)
        | _ -> Codex_log.fatal "Should not be reevaluating something else %a" Constraints.pretty constr
        in State.set_integer constr @@ IntegerConditionMap.refine ~inter:(B.Integer_Lattice.inter) cm ~cond res;
        res
    ;;

    let rec get_query_binary constr cond =
      let Constraints.Binary{size} = constr in
      let cm = State.get_binary constr in
      try
        BinaryConditionMap.find_join ~size cm cond
      with Condition_map.Never_refined ->
        (* Note: it is usual to reevaluate constants, because of rewrites. *)
        if Constraints.level constr >= 0 then
          Codex_log.warning "I need to re-evaluate %a (whose computation was not saved)"
            Constraints.pretty constr;
        let res = match constr with
        | Constraints.(Binary{term=T0{tag=TC.Biconst(size,k)}}) ->
          B.Binary_Forward.biconst ~size k
        | Constraints.(Binary{term=Empty}) ->
          B.Binary_Lattice.bottom ~size
        | Constraints.(Binary{term=T1{tag=TC.Bextract{size;index;oldsize};a}}) ->
          let av = get_query_binary a cond in
          B.Binary_Forward.bextract ~size ~index ~oldsize av
        | _ -> Codex_log.fatal "Should not be reevaluating something else %a" Constraints.pretty constr
        in State.set_binary constr @@ BinaryConditionMap.refine ~inter:(B.Binary_Lattice.inter ~size) cm ~cond res;
        res
    ;;

    let get_query_boolean x cond =
      let cm = (State.get_boolean x) in
      try BooleanConditionMap.find_join cm cond
      with Condition_map.Never_refined ->
        let res = match x with
          | Constraints.(Bool{term=Empty}) ->
            B.Boolean_Lattice.bottom
          | _ -> Codex_log.fatal "Should not be reevaluating something else %a" Constraints.pretty x
        in State.set_boolean x @@ BooleanConditionMap.refine ~inter:(B.Boolean_Lattice.inter) cm ~cond res;
        res

        
    let recompute_binary  constr cond = ignore @@ get_query_binary constr cond
    let recompute_integer constr cond = ignore @@ get_query_integer constr cond

    let ar2
        (type a) get_cma gqa (module La:SConditionMap with type value = a) ~joina ~bottoma ~intera
        (type b) get_cmb gqb (module Lb:SConditionMap with type value = b) ~joinb ~bottomb ~interb
        (type res)  get_cmres (module Lres:SConditionMap with type value = res) ~interres
        set_cmres
        f dom a b res =
      try No_Reevaluation.ar2
            get_cma gqa (module La) ~joina ~bottoma ~intera
            get_cmb gqb (module Lb) ~joinb ~bottomb ~interb
            get_cmres (module Lres) ~interres
            set_cmres f dom a b res
      with Condition_map.Never_refined ->
        let cond = (get_bdd dom) in
        gqa a cond;
        gqb b cond;
        No_Reevaluation.ar2
            get_cma gqa (module La) ~joina ~bottoma ~intera
            get_cmb gqb (module Lb) ~joinb ~bottomb ~interb
            get_cmres (module Lres) ~interres
            set_cmres f dom a b res
    ;;

    let ar1
        (type a) get_cma gqa (module La:SConditionMap with type value = a) ~joina ~bottoma ~intera
        (type res)  get_cmres (module Lres:SConditionMap with type value = res) ~interres
        set_cmres
        f dom a res =
      try No_Reevaluation.ar1 get_cma gqa (module La) ~joina ~bottoma ~intera
            get_cmres (module Lres) ~interres
            set_cmres f dom a res
      with Condition_map.Never_refined ->
        gqa a (get_bdd dom);
        No_Reevaluation.ar1 get_cma gqa (module La) ~joina ~bottoma ~intera
          get_cmres (module Lres) ~interres
          set_cmres f dom a res
    ;;


    let ar0 = No_Reevaluation.ar0
    
  end

  (* include No_Reevaluation *)  
  include With_Reevaluation
    
  let ar2_integer_integer_boolean =
    ar2 State.get_integer get_query_integer (module IntegerConditionMap) ~joina:B.Integer_Lattice.join ~bottoma:B.Integer_Lattice.bottom ~intera:B.Integer_Lattice.inter
      State.get_integer get_query_integer  (module IntegerConditionMap) ~joinb:B.Integer_Lattice.join  ~bottomb:B.Integer_Lattice.bottom ~interb:B.Integer_Lattice.inter
      State.get_boolean (module BooleanConditionMap) ~interres:B.Boolean_Lattice.inter
      State.set_boolean
  ;;

  let ar2_integer_integer_integer =
    ar2 State.get_integer get_query_integer (module IntegerConditionMap) ~joina:B.Integer_Lattice.join ~bottoma:B.Integer_Lattice.bottom ~intera:B.Integer_Lattice.inter
      State.get_integer get_query_integer  (module IntegerConditionMap)  ~joinb:B.Integer_Lattice.join ~bottomb:B.Integer_Lattice.bottom ~interb:B.Integer_Lattice.inter
      State.get_integer (module IntegerConditionMap) ~interres:B.Integer_Lattice.inter
      State.set_integer
  ;;



  let ar2_binary_binary_boolean ~sizea ~sizeb =
    (* Note: take advantage of the fact that currently, all the
       binary predicates require both arguments to have the same
       size. *)      
    assert(sizea == sizeb);
    let join = (B.Binary_Lattice.join ~size:sizea) in
    let bottom = (B.Binary_Lattice.bottom ~size:sizea) in      
    let inter = (B.Binary_Lattice.inter ~size:sizea) in      
    ar2 State.get_binary get_query_binary (module BinaryConditionMap) ~joina:join ~bottoma:bottom ~intera:inter
      State.get_binary get_query_binary  (module BinaryConditionMap) ~joinb:join ~bottomb:bottom ~interb:inter
      State.get_boolean (module BooleanConditionMap) ~interres:B.Boolean_Lattice.inter
      State.set_boolean
  ;;

  let ar2_binary_binary_binary ~sizea ~sizeb ~sizeres =
    (* Note: optimizing for the same size case does not change the
       benchmarks. *)
    let joina = (B.Binary_Lattice.join ~size:sizea) in
    let joinb = (B.Binary_Lattice.join ~size:sizeb) in
    let bottoma = (B.Binary_Lattice.bottom ~size:sizea) in
    let bottomb = (B.Binary_Lattice.bottom ~size:sizeb) in      
    let intera = (B.Binary_Lattice.inter ~size:sizea) in
    let interb = (B.Binary_Lattice.inter ~size:sizeb) in
    let interres = (B.Binary_Lattice.inter ~size:sizeres) in                  
    let get_query_binary = get_query_binary (* ~join *) in      
    ar2 State.get_binary get_query_binary (module BinaryConditionMap) ~joina ~bottoma ~intera
      State.get_binary get_query_binary  (module BinaryConditionMap) ~joinb ~bottomb ~interb
      State.get_binary (module BinaryConditionMap) ~interres
      State.set_binary
  ;;

  let ar2_boolean_boolean_boolean =
    ar2 State.get_boolean get_query_boolean (module BooleanConditionMap) ~joina:(B.Boolean_Lattice.join) ~bottoma:B.Boolean_Lattice.bottom ~intera:B.Boolean_Lattice.inter
      State.get_boolean get_query_boolean (module BooleanConditionMap) ~joinb:(B.Boolean_Lattice.join) ~bottomb:B.Boolean_Lattice.bottom ~interb:B.Boolean_Lattice.inter
      State.get_boolean (module BooleanConditionMap) ~interres:B.Boolean_Lattice.inter
      State.set_boolean
  ;;


  let ar1_boolean_boolean =
    ar1 State.get_boolean get_query_boolean (module BooleanConditionMap) ~joina:B.Boolean_Lattice.join ~bottoma:B.Boolean_Lattice.bottom ~intera:B.Boolean_Lattice.inter
      State.get_boolean (module BooleanConditionMap) ~interres:B.Boolean_Lattice.inter
      State.set_boolean
  ;;

  let ar1_integer_integer =
    ar1 State.get_integer get_query_integer (module IntegerConditionMap) ~joina:B.Integer_Lattice.join ~bottoma:B.Integer_Lattice.bottom ~intera:B.Integer_Lattice.inter
      State.get_integer (module IntegerConditionMap)  ~interres:B.Integer_Lattice.inter 
      State.set_integer
  ;;

  let ar1_boolean_binary ~sizeres =
    let joina = B.Boolean_Lattice.join in
    let intera = B.Boolean_Lattice.inter in
    let bottoma = B.Boolean_Lattice.bottom in
    let interres = B.Binary_Lattice.inter ~size:sizeres in
    ar1 State.get_boolean get_query_boolean (module BooleanConditionMap) ~joina ~bottoma ~intera
      State.get_binary (module BinaryConditionMap) ~interres
      State.set_binary
  ;;


  let ar1_binary_binary ~sizea ~sizeres =
    let joina = B.Binary_Lattice.join ~size:sizea in
    let intera = B.Binary_Lattice.inter ~size:sizea in
    let bottoma = B.Binary_Lattice.bottom ~size:sizea in
    let interres = B.Binary_Lattice.inter ~size:sizeres in
    ar1 State.get_binary get_query_binary (module BinaryConditionMap) ~joina ~bottoma ~intera
      State.get_binary (module BinaryConditionMap) ~interres
      State.set_binary
  ;;

  let ar0_binary ~size =
    ar0 State.get_binary (module BinaryConditionMap) ~interres:(B.Binary_Lattice.inter ~size)
      State.set_binary
  ;;

  let ar0_integer =
    ar0 State.get_integer (module IntegerConditionMap) ~interres:B.Integer_Lattice.inter
      State.set_integer
  ;;

  let ar0_boolean =
    ar0 State.get_boolean (module BooleanConditionMap) ~interres:B.Boolean_Lattice.inter
      State.set_boolean
  ;;

  module Query = struct
    include B
    let boolean t x = get_query_boolean x @@ get_bdd t
    let integer t x = get_query_integer x @@ get_bdd t
    let binary ~size t x = get_query_binary x @@ get_bdd t
  end


  let boolean_pretty cond fmt x = B.Boolean_Lattice.pretty fmt @@ Query.boolean cond x
  let integer_pretty cond fmt x = B.Integer_Lattice.pretty fmt @@ Query.integer cond x
  let binary_pretty ~size cond fmt x = B.Binary_Lattice.pretty ~size fmt @@ Query.binary ~size cond x


  module Boolean_Forward = struct
    let (&&) = ar2_boolean_boolean_boolean B.Boolean_Forward.(&&)
    let (||) = ar2_boolean_boolean_boolean B.Boolean_Forward.(||)          
    let not = ar1_boolean_boolean B.Boolean_Forward.not
    let true_ = ar0_boolean B.Boolean_Forward.true_
    let false_ = ar0_boolean B.Boolean_Forward.false_
    let assume _ = assert false
    let unknown ?level _ = assert false
  end


  module Integer_Forward = struct
    let ieq = ar2_integer_integer_boolean B.Integer_Forward.ieq
    let ile = ar2_integer_integer_boolean B.Integer_Forward.ile
    let iadd = ar2_integer_integer_integer B.Integer_Forward.iadd
    let isub = ar2_integer_integer_integer B.Integer_Forward.isub          
    let imul = ar2_integer_integer_integer B.Integer_Forward.imul
    let idiv = ar2_integer_integer_integer B.Integer_Forward.idiv
    let imod = ar2_integer_integer_integer B.Integer_Forward.imod
    let ishl = ar2_integer_integer_integer B.Integer_Forward.ishl
    let ishr = ar2_integer_integer_integer B.Integer_Forward.ishr
    let iand = ar2_integer_integer_integer B.Integer_Forward.iand
    let ior = ar2_integer_integer_integer B.Integer_Forward.ior
    let ixor = ar2_integer_integer_integer B.Integer_Forward.ixor
    let itimes k = ar1_integer_integer (B.Integer_Forward.itimes k)
    let iconst k = ar0_integer (B.Integer_Forward.iconst k)
    let one = iconst Z.one
    let zero = iconst Z.zero
  end

  module Binary_Forward = struct
    let beq    ~size = ar2_binary_binary_boolean ~sizea:size ~sizeb:size @@ B.Binary_Forward.beq ~size
    let biule  ~size = ar2_binary_binary_boolean ~sizea:size ~sizeb:size @@ B.Binary_Forward.biule ~size
    let bisle  ~size = ar2_binary_binary_boolean ~sizea:size ~sizeb:size @@ B.Binary_Forward.bisle ~size
    let biadd  ~size ~nsw ~nuw ~nusw = ar2_binary_binary_binary ~sizea:size ~sizeb:size ~sizeres:size @@ B.Binary_Forward.biadd ~size ~nsw ~nuw ~nusw
    let bisub  ~size ~nsw ~nuw ~nusw = ar2_binary_binary_binary ~sizea:size ~sizeb:size ~sizeres:size @@ B.Binary_Forward.bisub ~size ~nsw ~nuw ~nusw
    let bimul  ~size ~nsw ~nuw = ar2_binary_binary_binary ~sizea:size ~sizeb:size ~sizeres:size @@ B.Binary_Forward.bimul ~size ~nsw ~nuw
    let bxor   ~size = ar2_binary_binary_binary ~sizea:size ~sizeb:size ~sizeres:size @@ B.Binary_Forward.bxor ~size
    let bor    ~size = ar2_binary_binary_binary ~sizea:size ~sizeb:size ~sizeres:size @@ B.Binary_Forward.bor ~size
    let band   ~size = ar2_binary_binary_binary ~sizea:size ~sizeb:size ~sizeres:size @@ B.Binary_Forward.band ~size
    let bashr  ~size = ar2_binary_binary_binary ~sizea:size ~sizeb:size ~sizeres:size @@ B.Binary_Forward.bashr ~size
    let blshr  ~size = ar2_binary_binary_binary ~sizea:size ~sizeb:size ~sizeres:size @@ B.Binary_Forward.blshr ~size
    let bshl   ~size ~nsw ~nuw = ar2_binary_binary_binary ~sizea:size ~sizeb:size ~sizeres:size @@ B.Binary_Forward.bshl ~size ~nsw ~nuw
    let bisdiv ~size = ar2_binary_binary_binary ~sizea:size ~sizeb:size ~sizeres:size @@ B.Binary_Forward.bisdiv ~size
    let biudiv ~size = ar2_binary_binary_binary ~sizea:size ~sizeb:size ~sizeres:size @@ B.Binary_Forward.biudiv ~size
    let bismod ~size = ar2_binary_binary_binary ~sizea:size ~sizeb:size ~sizeres:size @@ B.Binary_Forward.bismod ~size
    let biumod ~size = ar2_binary_binary_binary ~sizea:size ~sizeb:size ~sizeres:size @@ B.Binary_Forward.biumod ~size
        
    let bsext ~size ~oldsize = ar1_binary_binary ~sizea:oldsize ~sizeres:size @@ B.Binary_Forward.bsext ~size ~oldsize
    let buext ~size ~oldsize = ar1_binary_binary ~sizea:oldsize ~sizeres:size @@ B.Binary_Forward.buext ~size ~oldsize
    let bchoose ~size cond = ar1_binary_binary ~sizea:size ~sizeres:size @@ B.Binary_Forward.bchoose ~size cond
    let bofbool ~size = ar1_boolean_binary ~sizeres:size @@ B.Binary_Forward.bofbool ~size                             
    let bconcat ~size1 ~size2 = ar2_binary_binary_binary ~sizea:size1 ~sizeb:size2 ~sizeres:(size1+size2) @@ B.Binary_Forward.bconcat ~size1 ~size2
    let bextract ~size ~index ~oldsize = ar1_binary_binary ~sizea:oldsize ~sizeres:size @@ B.Binary_Forward.bextract ~size ~index ~oldsize
    let valid ~size _ = assert false
    let valid_ptr_arith ~size _ = assert false
    let baddr ~size _  = assert false
    let biconst ~size k = ar0_binary ~size (B.Binary_Forward.biconst ~size k)
    let buninit ~size _ = assert false
    let bshift ~size ~offset ~max _ = assert false
    let bindex ~size _ = assert false
    let assume ~size _ = assert false
  end

  
  let binary_empty ~size = ar0_binary ~size (B.Binary_Lattice.bottom ~size)  
  let integer_empty = ar0_integer (B.Integer_Lattice.bottom)
  let boolean_empty = ar0_boolean (B.Boolean_Lattice.bottom)

  let binary_unknown ~size = ar0_binary ~size (B.Binary_Lattice.top ~size)  
  let integer_unknown = ar0_integer (B.Integer_Lattice.top)
  let boolean_unknown = ar0_boolean (B.Boolean_Lattice.top)        

  (**************** Nondet.  ****************)

  let [@inline always] iter3_condition_map tupa tupb tupres
      ~fboolean ~finteger ~fbinary =
    let n = Immutable_array.length tupa in
    for i = 0 to n - 1 do
      let Constraints.Any a = Immutable_array.get tupa i in
      let Constraints.Any b = Immutable_array.get tupb i in
      let Constraints.Any res = Immutable_array.get tupres i in
      (* Codex_log.feedback "iter3 %d a %a b %a res %a" i
         Constraints.pretty a Constraints.pretty b Constraints.pretty res; *)

      match a,b,res with
      | (Constraints.Bool _ as a), (Constraints.Bool _ as b), (Constraints.Bool _ as res) ->
        fboolean a b res
      | (Constraints.Integer _ as a), (Constraints.Integer _ as b), (Constraints.Integer _ as res) ->
        finteger a b res
      | (Constraints.Binary _ as a), (Constraints.Binary _ as b), (Constraints.Binary _ as res) ->
        fbinary a b res
      | _ -> failwith "Typing error"
    done
  ;;

  let nondet_disjoint ~doma ~tupa ~domb ~tupb ~tupres =
    let resdom = {condition = Constraints.Build.Boolean.(||) doma.condition domb.condition} in

    let conda = get_bdd doma in
    let condb = get_bdd domb in
    let notconda = Condition.(!~) conda in
    let notcondb = Condition.(!~) condb in

    let fboolean a b res =
      let cma = State.get_boolean a in
      let cmb = State.get_boolean b in
      let old = State.get_boolean res in
      let cmres = ConditionMap.nondet_disjoint (module BooleanConditionMap)
          ~join:B.Boolean_Lattice.join ~bottom:B.Boolean_Lattice.bottom ~inter:B.Boolean_Lattice.inter
          ~conda ~notconda ~cma
          ~condb ~notcondb ~cmb
          ~old
      in State.set_boolean res cmres
    in

    let finteger a b res =
      let cma = State.get_integer a in
      let cmb = State.get_integer b in
      let old = State.get_integer res in
      let cmres = ConditionMap.nondet_disjoint (module IntegerConditionMap)
          ~join:B.Integer_Lattice.join ~bottom:B.Integer_Lattice.bottom ~inter:B.Integer_Lattice.inter
          ~conda ~notconda ~cma
          ~condb ~notcondb ~cmb
          ~old
      in State.set_integer res cmres
    in

    let fbinary a b res =
      let fonce a b res =
        let Constraints.Binary{size} = res in
        let cma = State.get_binary a in
        let cmb = State.get_binary b in
        let old = State.get_binary res in
        let cmres = ConditionMap.nondet_disjoint (module BinaryConditionMap)
            ~join:(B.Binary_Lattice.join ~size) ~bottom:(B.Binary_Lattice.bottom ~size) ~inter:(B.Binary_Lattice.inter ~size)
            ~conda ~notconda ~cma
            ~condb ~notcondb ~cmb
            ~old
        in
        (* Codex_log.feedback "nondet disjoint %a %a (%a) (%a) (%a) res (%a)"
         *   Condition.pretty conda
         *   Condition.pretty condb          
         *   BinaryConditionMap.pretty cma
         *   BinaryConditionMap.pretty cmb
         *   BinaryConditionMap.pretty old
         *   BinaryConditionMap.pretty cmres;
         * let av = (Constraints_condition.DomPIPartition.M.find conda cma) in
         * Codex_log.feedback "nondet disjoint 2 %a %a %a"
         *   (B.Binary_Lattice.pretty ~size) (Constraints_condition.DomPIPartition.M.find conda cma)
         *   (B.Binary_Lattice.pretty ~size) (Constraints_condition.DomPIPartition.M.find condb cmb)
         *   (BinaryConditionMap.pretty) (Constraints_condition.DomPIPartition.M.refine conda ~inter:(B.Binary_Lattice.inter ~size) ~join:(fun _ _ -> assert false) av old); *)
        State.set_binary res cmres
      in
      
      try fonce a b res
      (* Retry once if this fails. *)
      with Condition_map.Never_refined ->
        With_Reevaluation.recompute_binary a conda;
        With_Reevaluation.recompute_binary b condb;
        fonce a b res
    in
    iter3_condition_map tupa tupb tupres ~fboolean ~finteger ~fbinary;
    resdom
  ;;

  (* Limited to the cases where the conditions are the same. *)
  let nondet_non_disjoint ~doma ~tupa ~domb ~tupb ~tupres =
    let domres = join doma domb in
    let cond = get_bdd domres in
    let conda = get_bdd doma in
    let condb = get_bdd domb in
    let notcond = Condition.(!~) cond in

    let fboolean a b res =
      let cma = State.get_boolean a in
      let cmb = State.get_boolean b in
      let old = State.get_boolean res in
      let cmres = ConditionMap.nondet_non_disjoint (module BooleanConditionMap)
          ~join:B.Boolean_Lattice.join ~bottom:B.Boolean_Lattice.bottom ~inter:B.Boolean_Lattice.inter
          ~conda ~cma ~condb ~cmb ~condaorb:cond ~notcondaorb:notcond ~old
      in State.set_boolean res cmres
    in

    let finteger a b res =
      let cma = State.get_integer a in
      let cmb = State.get_integer b in
      let old = State.get_integer res in
      let cmres = ConditionMap.nondet_non_disjoint (module IntegerConditionMap)
          ~join:B.Integer_Lattice.join ~bottom:B.Integer_Lattice.bottom ~inter:B.Integer_Lattice.inter
          ~conda ~cma ~condb ~cmb ~condaorb:cond ~notcondaorb:notcond ~old
      in State.set_integer res cmres
    in

    let fbinary a b res =
      let fonce a b res =
        let Constraints.Binary{size} = res in
        let cma = State.get_binary a in
        let cmb = State.get_binary b in
        let old = State.get_binary res in
        let cmres = ConditionMap.nondet_non_disjoint (module BinaryConditionMap)
            ~join:(B.Binary_Lattice.join ~size) ~bottom:(B.Binary_Lattice.bottom ~size) ~inter:(B.Binary_Lattice.inter ~size)
            ~conda ~cma ~condb ~cmb ~condaorb:cond ~notcondaorb:notcond ~old
        in State.set_binary res cmres
      in
      try fonce a b res
      (* Retry once if this fails. *)
      with Condition_map.Never_refined ->
        With_Reevaluation.recompute_binary a conda;
        With_Reevaluation.recompute_binary b condb;
        fonce a b res
    in
    iter3_condition_map tupa tupb tupres ~fboolean ~finteger ~fbinary;
    domres
  ;;

  let nondet ~doma ~tupa ~domb ~tupb ~tupres =
    (* More precise if we reuse the condition for doma or domb in the future. *)
    if Condition.disjoint (get_bdd doma) (get_bdd domb)
    then nondet_disjoint ~doma ~tupa ~domb ~tupb ~tupres
    else nondet_non_disjoint ~doma ~domb ~tupa ~tupb ~tupres
  ;;

  (**************** Fixpoint step. ****************)


  let fixpoint_open () =
    if option_push_and_pop then State.push() else ()
  ;;

  let fixpoint_step ~lvl actual_dom  ~actuals arg_dom ~args final_dom ~finals =
    let arg_cond = get_bdd arg_dom in
    let final_cond = get_bdd final_dom in
    let actual_cond = arg_cond in

    let fp_reached = ref true in
    let n = Immutable_array.length finals in
    let widened_values = Array.init n (fun i ->
        let Constraints.Any actual = Immutable_array.get actuals i in
        let Constraints.Any arg = Immutable_array.get args i in
        let Constraints.Any final = Immutable_array.get finals i in
        (*Codex_log.feedback "fixpoint step %d: %a %a %a" i Constraints.pretty actual Constraints.pretty arg Constraints.pretty final;*)
        match actual,arg,final with
        | (Constraints.Bool _ as actual),
          (Constraints.Bool _ as arg),
          (Constraints.Bool _ as final) ->
          (* backup_evaluation actual_cond actual; *)          
          let actualv = BooleanConditionMap.find_join (State.get_boolean actual) actual_cond in
          let argv = BooleanConditionMap.find_join (State.get_boolean arg) arg_cond in
          let finalv = BooleanConditionMap.find_join (State.get_boolean final) final_cond in
          let joined = B.Boolean_Lattice.join actualv finalv in
          let (bool,widened) =
            if Codex_config.widen ()
            then B.Boolean_Lattice.includes_or_widen ~previous:argv joined
            else B.Boolean_Lattice.includes argv joined, joined
          in
          fp_reached := !fp_reached && bool;
          Sum.Boolean widened
        | (Constraints.Integer _ as actual),
          (Constraints.Integer _ as arg),
          (Constraints.Integer _ as final) ->
          let actualv = get_query_integer actual actual_cond in
          let argv = get_query_integer arg arg_cond in          
          let finalv = IntegerConditionMap.find_join (State.get_integer final) final_cond in
          let joined = B.Integer_Lattice.join actualv finalv in
          let (bool,widened) =
            if Codex_config.widen ()
            then B.Integer_Lattice.includes_or_widen ~previous:argv joined
            else B.Integer_Lattice.includes argv joined, joined
          in
          (* Codex_log.feedback "actualv %a argv %a finalv %a widened %a bool %b"
           *   B.Integer_Lattice.pretty actualv B.Integer_Lattice.pretty argv
             B.Integer_Lattice.pretty finalv B.Integer_Lattice.pretty widened bool; *)
          fp_reached := !fp_reached && bool;
          Sum.Integer widened
        | (Constraints.Binary {size=sizeactual} as actual),
          (Constraints.Binary {size=sizearg} as arg),
          (Constraints.Binary {size=sizefinal} as final) ->
          (* Codex_log.feedback "sizeactual %d sizefinal %d sizearg %d" sizeactual sizefinal sizearg;
           * Codex_log.feedback "actual %a final %a arg %a" Constraints.pretty actual Constraints.pretty final Constraints.pretty arg; *)
          assert(sizeactual == sizearg);
          assert(sizeactual == sizefinal);
          let size = sizeactual in
          let actualv = get_query_binary actual actual_cond in
          let argv = get_query_binary arg arg_cond in          
          let finalv = BinaryConditionMap.find_join ~size (State.get_binary final) final_cond in
          let joined = B.Binary_Lattice.join ~size actualv finalv in
          let (bool,widened) =
            if Codex_config.widen ()
            then B.Binary_Lattice.includes_or_widen ~size ~previous:argv joined
            else B.Binary_Lattice.includes ~size argv joined, joined
          in
          (* Codex_log.feedback "actualv %a argv %a finalv %a joined %a widened %a bool %b"
           *   (B.Binary_Lattice.pretty ~size) actualv (B.Binary_Lattice.pretty ~size) argv
           *   (B.Binary_Lattice.pretty ~size) finalv (B.Binary_Lattice.pretty ~size) joined
           *   (B.Binary_Lattice.pretty ~size) widened bool; *)
          fp_reached := !fp_reached && bool;
          Sum.Binary widened
        | _ -> failwith "Type issue"
      ) in
    !fp_reached, (fun ~close res ->
        (if option_push_and_pop then
           if option_push_and_pop_on_restart
           then
             begin
               State.pop();
               (if not close then State.push())
             end
           else begin
             (if close then State.pop())
           end);
        widened_values|> Array.iteri (fun i widened ->
            let Constraints.Any(res) = Immutable_array.get res i in
            (* Codex_log.feedback "fpstep %d" i;
             * Codex_log.feedback "fpstep %d: writing to %a" i Constraints.pretty res; *)
            match res, widened with
            | Constraints.Bool _ as res, Sum.Boolean widened ->
              State.set_boolean res @@ BooleanConditionMap.refine ~inter:B.Boolean_Lattice.inter BooleanConditionMap.create_partial widened ~cond:Condition.one
            | Constraints.Integer _ as res, Sum.Integer widened ->
              State.set_integer res @@ IntegerConditionMap.refine ~inter:B.Integer_Lattice.inter IntegerConditionMap.create_partial widened ~cond:Condition.one
            | Constraints.Binary {size} as res, Sum.Binary widened ->
              State.set_binary res @@ BinaryConditionMap.refine ~inter:(B.Binary_Lattice.inter ~size) BinaryConditionMap.create_partial widened ~cond:Condition.one
           
            | _ -> failwith "typing error"
          );
        (* Codex_log.feedback "fpstep: returning %a" Constraints.pretty actual_dom.condition; *)
        (* actual_dom *)
        arg_dom
      )
  ;;

  (**************** Backward evaluation; assume. ****************)

  module Backward = struct

    (* Backward transfer functions. *)
    let get_bdd x = x;;

    let ar2
        (type a) get_cma set_cma (module La:SConditionMap with type value = a) ~joina ~bottoma ~intera
        (type b) get_cmb set_cmb (module Lb:SConditionMap with type value = b) ~joinb ~bottomb ~interb
        (type res)  get_cmres (module Lres:SConditionMap with type value = res) ~joinres ~bottomres
        f dom a b (* ctx *) res =

      let condition = get_bdd dom in
      let cma = get_cma a in
      let cmb = get_cmb b in
      let cmres = get_cmres res in

      try 
        let ((conda,cma'),(condb,cmb')) =
          ConditionMap.ar2_bwd (module La) ~joina ~bottoma ~intera (module Lb) ~joinb ~bottomb ~interb (module Lres) ~joinres ~bottomres condition f
            cma cmb cmres in
        (* Codex_log.feedback "ar2: changed %a %a" Condition.pretty conda Condition.pretty condb; *)

        let changed =
          if Condition.is_empty conda then []
          else (set_cma a cma'; [conda, Constraints.Any a])
        in
        let changed =
          if Condition.is_empty condb then changed
          else (set_cmb b cmb'; (condb, Constraints.Any b)::changed)
        in
        changed
      with Condition_map.Never_refined ->
        Codex_log.warning "A backward evaluation failed";
        []            
    ;;

    let ar1
        (type a) get_cma set_cma (module La:SConditionMap with type value = a) ~joina ~bottoma ~intera
        (type res)  get_cmres (module Lres:SConditionMap with type value = res) ~joinres ~bottomres
        f dom a (* ctx *) res =
      let condition = get_bdd dom in
      let cma = get_cma a in
      let cmres = get_cmres res in

      try 
        let (conda,cma') =
          ConditionMap.ar1_bwd (module La) ~joina ~bottoma ~intera (module Lres) ~joinres ~bottomres condition f
            cma cmres in

        let changed =
          if Condition.is_empty conda then []
          else (set_cma a cma'; [conda, Constraints.Any a])
        in
        changed
      with Condition_map.Never_refined ->
        Codex_log.warning "A backward evaluation failed";
        []
    ;;


    let ar2_binary_binary_boolean ~sizea ~sizeb =
      assert(sizea == sizeb);
      let joina = B.Binary_Lattice.join ~size:sizea in
      let joinb = joina in
      let intera = B.Binary_Lattice.inter ~size:sizea in
      let interb = intera in
      let bottoma = B.Binary_Lattice.bottom ~size:sizea in
      let bottomb = bottoma in
      let bottomres = B.Boolean_Lattice.bottom in            
      ar2 State.get_binary State.set_binary (module BinaryConditionMap) ~joina ~bottoma ~intera
        State.get_binary State.set_binary (module BinaryConditionMap) ~joinb ~bottomb ~interb
        State.get_boolean (module BooleanConditionMap) ~joinres:B.Boolean_Lattice.join ~bottomres
    ;;

    let ar2_binary_binary_binary ~sizea ~sizeb ~sizeres =
      let joina = B.Binary_Lattice.join ~size:sizea in
      let joinb = B.Binary_Lattice.join ~size:sizeb in
      let joinres = B.Binary_Lattice.join ~size:sizeres in
      let intera = B.Binary_Lattice.inter ~size:sizea in
      let interb = B.Binary_Lattice.inter ~size:sizeb in
      let bottoma = B.Binary_Lattice.bottom ~size:sizea in
      let bottomb = bottoma in
      let bottomres = bottoma in
      ar2 State.get_binary State.set_binary (module BinaryConditionMap) ~joina ~bottoma ~intera
        State.get_binary State.set_binary (module BinaryConditionMap) ~joinb ~bottomb ~interb
        State.get_binary (module BinaryConditionMap) ~joinres ~bottomres
    ;;

    let ar1_binary_binary ~sizea ~sizeres =
      let joina = B.Binary_Lattice.join ~size:sizea in
      let intera = B.Binary_Lattice.inter ~size:sizea in      
      let joinres = B.Binary_Lattice.join ~size:sizeres in
      let bottoma = B.Binary_Lattice.bottom ~size:sizea in
      let bottomres = B.Binary_Lattice.bottom ~size:sizeres in      
      ar1 State.get_binary State.set_binary (module BinaryConditionMap) ~joina ~bottoma ~intera
        State.get_binary (module BinaryConditionMap) ~joinres ~bottomres
    ;;

    let ar1_boolean_binary ~sizeres =
      let joina = B.Boolean_Lattice.join in
      let intera = B.Boolean_Lattice.inter in      
      let joinres = B.Binary_Lattice.join ~size:sizeres in
      let bottoma = B.Boolean_Lattice.bottom in
      let bottomres = B.Binary_Lattice.bottom ~size:sizeres in      
      ar1 State.get_boolean State.set_boolean (module BooleanConditionMap) ~joina ~bottoma ~intera
        State.get_binary (module BinaryConditionMap) ~joinres ~bottomres
    ;;

    
    let ar2_integer_integer_boolean =
      ar2 State.get_integer State.set_integer (module IntegerConditionMap) ~joina:B.Integer_Lattice.join ~bottoma:B.Integer_Lattice.bottom ~intera:B.Integer_Lattice.inter
        State.get_integer State.set_integer (module IntegerConditionMap)   ~joinb:B.Integer_Lattice.join ~bottomb:B.Integer_Lattice.bottom ~interb:B.Integer_Lattice.inter
        State.get_boolean (module BooleanConditionMap) ~joinres:B.Boolean_Lattice.join ~bottomres:B.Boolean_Lattice.bottom
    ;;

    let ar2_integer_integer_integer =
      ar2 State.get_integer State.set_integer (module IntegerConditionMap) ~joina:B.Integer_Lattice.join ~bottoma:B.Integer_Lattice.bottom ~intera:B.Integer_Lattice.inter
        State.get_integer State.set_integer (module IntegerConditionMap)   ~joinb:B.Integer_Lattice.join ~bottomb:B.Integer_Lattice.bottom ~interb:B.Integer_Lattice.inter
        State.get_integer (module IntegerConditionMap) ~joinres:B.Integer_Lattice.join ~bottomres:B.Integer_Lattice.bottom
    ;;

    let ar2_boolean_boolean_boolean =
      ar2 State.get_boolean State.set_boolean (module BooleanConditionMap)  ~joina:B.Boolean_Lattice.join ~bottoma:B.Boolean_Lattice.bottom ~intera:B.Boolean_Lattice.inter
        State.get_boolean State.set_boolean (module BooleanConditionMap)    ~joinb:B.Boolean_Lattice.join ~bottomb:B.Boolean_Lattice.bottom ~interb:B.Boolean_Lattice.inter
        State.get_boolean (module BooleanConditionMap)  ~joinres:B.Boolean_Lattice.join ~bottomres:B.Boolean_Lattice.bottom
    ;;

    let ar1_boolean_boolean =
      ar1 State.get_boolean State.set_boolean (module BooleanConditionMap) ~joina:B.Boolean_Lattice.join ~bottoma:B.Boolean_Lattice.bottom ~intera:B.Boolean_Lattice.inter
        State.get_boolean (module BooleanConditionMap) ~joinres:B.Boolean_Lattice.join ~bottomres:B.Boolean_Lattice.bottom
    ;;

    let ar1_integer_integer =
      ar1 State.get_integer State.set_integer (module IntegerConditionMap) ~joina:B.Integer_Lattice.join ~bottoma:B.Integer_Lattice.bottom ~intera:B.Integer_Lattice.inter
        State.get_integer (module IntegerConditionMap) ~joinres:B.Integer_Lattice.join ~bottomres:B.Integer_Lattice.bottom
    ;;

    module Binary = struct
      let beq    ~size = ar2_binary_binary_boolean ~sizea:size ~sizeb:size @@ B.Binary_Backward.beq ~size
      let biule  ~size = ar2_binary_binary_boolean ~sizea:size ~sizeb:size @@ B.Binary_Backward.biule ~size
      let bisle  ~size = ar2_binary_binary_boolean ~sizea:size ~sizeb:size @@ B.Binary_Backward.bisle ~size        
      let biadd  ~size ~nsw ~nuw ~nusw = ar2_binary_binary_binary ~sizea:size ~sizeb:size ~sizeres:size @@ B.Binary_Backward.biadd ~size ~nsw ~nuw ~nusw
      let bisub  ~size ~nsw ~nuw ~nusw = ar2_binary_binary_binary ~sizea:size ~sizeb:size ~sizeres:size @@ B.Binary_Backward.bisub ~size ~nsw ~nuw ~nusw
      let bimul  ~size ~nsw ~nuw = ar2_binary_binary_binary ~sizea:size ~sizeb:size ~sizeres:size @@ B.Binary_Backward.bimul ~size ~nsw ~nuw
      let bxor   ~size = ar2_binary_binary_binary ~sizea:size ~sizeb:size ~sizeres:size @@ B.Binary_Backward.bxor ~size
      let bor    ~size = ar2_binary_binary_binary ~sizea:size ~sizeb:size ~sizeres:size @@ B.Binary_Backward.bor ~size
      let band   ~size = ar2_binary_binary_binary ~sizea:size ~sizeb:size ~sizeres:size @@ B.Binary_Backward.band ~size
      let bashr  ~size = ar2_binary_binary_binary ~sizea:size ~sizeb:size ~sizeres:size @@ B.Binary_Backward.bashr ~size
      let blshr  ~size = ar2_binary_binary_binary ~sizea:size ~sizeb:size ~sizeres:size @@ B.Binary_Backward.blshr ~size
      let bshl   ~size ~nsw ~nuw = ar2_binary_binary_binary ~sizea:size ~sizeb:size ~sizeres:size @@ B.Binary_Backward.bshl ~size ~nsw ~nuw
      let bisdiv ~size = ar2_binary_binary_binary ~sizea:size ~sizeb:size ~sizeres:size @@ B.Binary_Backward.bisdiv ~size
      let biudiv ~size = ar2_binary_binary_binary ~sizea:size ~sizeb:size ~sizeres:size @@ B.Binary_Backward.biudiv ~size
      let bismod ~size = ar2_binary_binary_binary ~sizea:size ~sizeb:size ~sizeres:size @@ B.Binary_Backward.bismod ~size
      let biumod ~size = ar2_binary_binary_binary ~sizea:size ~sizeb:size ~sizeres:size @@ B.Binary_Backward.biumod ~size
      let bsext ~size ~oldsize = ar1_binary_binary ~sizea:oldsize ~sizeres:size @@ B.Binary_Backward.bsext ~size ~oldsize
      let buext ~size ~oldsize = ar1_binary_binary ~sizea:oldsize ~sizeres:size @@ B.Binary_Backward.buext ~size ~oldsize
      let bofbool ~size = ar1_boolean_binary ~sizeres:size @@ B.Binary_Backward.bofbool ~size                                       
      let bconcat ~size1 ~size2 = ar2_binary_binary_binary ~sizea:size1 ~sizeb:size2 ~sizeres:(size1+size2) @@ B.Binary_Backward.bconcat ~size1 ~size2
      let bextract ~size ~index ~oldsize = ar1_binary_binary ~sizea:oldsize ~sizeres:size @@ B.Binary_Backward.bextract ~size ~index ~oldsize
      let id ~size = ar1_binary_binary ~sizea:size ~sizeres:size (fun a res ->
          let inter = B.Binary_Lattice.inter ~size a res in
          if B.Binary_Lattice.equal a inter then None
          else Some inter)
      
    end

    
    module Integer = struct
      let ieq = ar2_integer_integer_boolean B.Integer_Backward.ieq
      let ile = ar2_integer_integer_boolean B.Integer_Backward.ile
      let iadd = ar2_integer_integer_integer B.Integer_Backward.iadd
      let isub = ar2_integer_integer_integer B.Integer_Backward.isub          
      let imul = ar2_integer_integer_integer B.Integer_Backward.imul
      let idiv = ar2_integer_integer_integer B.Integer_Backward.idiv
      let imod = ar2_integer_integer_integer B.Integer_Backward.imod
      let ishl = ar2_integer_integer_integer B.Integer_Backward.ishl
      let ishr = ar2_integer_integer_integer B.Integer_Backward.ishr
      let iand = ar2_integer_integer_integer B.Integer_Backward.iand
      let ior = ar2_integer_integer_integer B.Integer_Backward.ior
      let ixor = ar2_integer_integer_integer B.Integer_Backward.ixor
      let itimes k = ar1_integer_integer (B.Integer_Backward.itimes k)

      let id = ar1_integer_integer (fun a res ->
          let inter = B.Integer_Lattice.inter a res in
          if B.Integer_Lattice.equal a inter then None
          else Some inter)
    end
    module Boolean = struct
      let (&&) = ar2_boolean_boolean_boolean B.Boolean_Backward.(&&)
      let (||) = ar2_boolean_boolean_boolean B.Boolean_Backward.(||)          
      let not = ar1_boolean_boolean B.Boolean_Backward.not
      let id = ar1_boolean_boolean (fun a res ->
          let inter = B.Boolean_Lattice.inter a res in
          if B.Boolean_Lattice.equal a inter then None
          else Some inter)          
    end



    
    
    let backward_interp condition (Constraints.Any constr) =
      let open TC in
      let open Constraints in
      (* Codex_log.feedback "backwar interp on %a" Constraints.pretty constr; *)
      match constr with
      | Bool{term=T0 _} -> []
      | Integer{term=T0 _} -> []
      | Binary{term=T0 _} -> []
      | Bool{term=(Mu_formal _|Unknown _) } -> []
      | Integer{term=(Mu_formal _|Unknown _) } -> []
      | Binary{term=(Mu_formal _|Unknown _) } -> []
      (* Note that we cannot blindly propagate backward with nondet. 
         The reason is the following deduction rules:

         nondet(x,y) = 30 |- x = 30 \/ y = 30
         x = 30 /\ y = 30 |- nondet(x,y) = 30.

         "= 30" can be replaced by any predicate. To understand the first rule, we must remember 
         that the condition under which nondet(x,y) = 30 imply that we have nondet(x,y) = x; in which
         case we cannot deduce anything from y.

         However, if the definition condition for x and y are disjoint, then we can separately
         reason about the cases where "nondet(x,y) = x" and "nondet(x,y) = y". *)
      | Binary{size;term=Tuple_get(j,Nondet {a;conda_bool;b;condb_bool})} ->
        if not option_propagate_across_nondet then []
        else
          let Constraints.Bool{bdd=conda} =conda_bool in
          let Constraints.Bool{bdd=condb} =condb_bool in        
          if not @@ Condition.disjoint conda condb then []
          else
            let constra = Constraints.Build.Tuple.get_binary ~size j a in
            let constrb = Constraints.Build.Tuple.get_binary ~size j b in
            let conda = Condition.(&&~) conda condition in
            let condb = Condition.(&&~) condb condition in
            (Binary.id ~size conda constra constr) @ (Binary.id ~size condb constrb constr)
      | Integer{term=Tuple_get(j,Nondet {a;conda_bool;b;condb_bool})} ->
        if not option_propagate_across_nondet then []
        else
          let Constraints.Bool{bdd=conda} =conda_bool in
          let Constraints.Bool{bdd=condb} =condb_bool in        
          if not @@ Condition.disjoint conda condb then []
          else
            let constra = Constraints.Build.Tuple.get_integer j a in
            let constrb = Constraints.Build.Tuple.get_integer j b in
            let conda = Condition.(&&~) conda condition in
            let condb = Condition.(&&~) condb condition in
            (Integer.id conda constra constr) @ (Integer.id condb constrb constr)
      | Bool{term=Tuple_get(j,Nondet {a;conda_bool;b;condb_bool})} ->
        if not option_propagate_across_nondet then []
        else
          let Constraints.Bool{bdd=conda} =conda_bool in
          let Constraints.Bool{bdd=condb} =condb_bool in        
          if not @@ Condition.disjoint conda condb then []
          else
            let constra = Constraints.Build.Tuple.get_boolean j a in
            let constrb = Constraints.Build.Tuple.get_boolean j b in
            let conda = Condition.(&&~) conda condition in
            let condb = Condition.(&&~) condb condition in
            (Boolean.id conda constra constr) @ (Boolean.id condb constrb constr)

      (* Like in the nondet case, we cannot propagate backward across mu. *)
      | Bool{term=Tuple_get(j,Mu _)} -> []
      | Integer{term=Tuple_get(j,Mu _)} -> []
      | Binary{term=Tuple_get(j,Mu _)} -> []
      | Binary{term=T1{tag=Bchoose(_,_)}} -> []
      | Binary{term=T2{tag=Bunion(_,_);a;b}} -> []    

      | Bool{term=T2{tag=And;a;b}} -> Boolean.(&&) condition a b constr
      | Bool{term=T2{tag=Or;a;b}} ->
        Codex_log.warning "Backward propagation on ||: more precise results\
                           could be obtained by propagating on individual conditions";
        Boolean.(||) condition a b constr
      | Bool{term=T1{tag=Not;a}} -> Boolean.not condition a constr

      | Bool{term=T2{tag=Beq size;a;b}} ->       Binary.beq    ~size condition a b constr
      | Bool{term=T2{tag=Bisle size;a;b}} ->     Binary.bisle  ~size condition a b constr
      | Bool{term=T2{tag=Biule size;a;b}} ->     Binary.biule  ~size condition a b constr
      | Binary{term=T2{tag=Biadd{size;nsw;nuw;nusw};a;b}} ->  Binary.biadd  ~size ~nsw ~nuw ~nusw condition a b constr
      | Binary{term=T2{tag=Bisub{size;nsw;nuw;nusw};a;b}} ->  Binary.bisub  ~size ~nsw ~nuw ~nusw condition a b constr
      | Binary{term=T2{tag=Bimul{size;nsw;nuw};a;b}} ->  Binary.bimul  ~size ~nsw ~nuw condition a b constr
      | Binary{term=T2{tag=Bshl{size;nsw;nuw};a;b}} ->   Binary.bshl   ~size ~nsw ~nuw condition a b constr
      | Binary{term=T2{tag=Bisdiv(size);a;b}} -> Binary.bisdiv ~size condition a b constr
      | Binary{term=T2{tag=Bismod(size);a;b}} -> Binary.bismod ~size condition a b constr
      | Binary{term=T2{tag=Biudiv(size);a;b}} -> Binary.biudiv ~size condition a b constr
      | Binary{term=T2{tag=Biumod(size);a;b}} -> Binary.biumod ~size condition a b constr
      | Binary{term=T2{tag=Bashr(size);a;b}} ->  Binary.bashr  ~size condition a b constr
      | Binary{term=T2{tag=Blshr(size);a;b}} ->  Binary.blshr  ~size condition a b constr
      | Binary{term=T2{tag=Band(size);a;b}} ->   Binary.band   ~size condition a b constr
      | Binary{term=T2{tag=Bor (size);a;b}} ->   Binary.bor    ~size condition a b constr
      | Binary{term=T2{tag=Bxor(size);a;b}} ->   Binary.bxor   ~size condition a b constr
      | Binary{term=T2{tag=Bconcat(size1,size2);a;b}} -> Binary.bconcat ~size1 ~size2 condition a b constr
      | Binary{term=T1{tag=Bsext(size);a=Binary{size=oldsize} as a}} -> Binary.bsext ~size ~oldsize condition a constr
      | Binary{term=T1{tag=Buext(size);a=Binary{size=oldsize} as a}} -> Binary.buext ~size ~oldsize condition a constr
      | Binary{term=T1{tag=Bofbool(size);a=Bool _ as a}} -> Binary.bofbool ~size condition a constr                                                                      
      | Binary{term=T1{tag=Bextract{size;index;oldsize};a}} -> Binary.bextract ~size ~index ~oldsize condition a constr

    
      | Bool{term=T2{tag=Ieq;a;b}}     -> Integer.ieq  condition a b constr
      | Bool{term=T2{tag=Ile;a;b}}     -> Integer.ile  condition a b constr
      | Integer{term=T2{tag=Iadd;a;b}} -> Integer.iadd condition a b constr
      | Integer{term=T2{tag=Isub;a;b}} -> Integer.isub condition a b constr
      | Integer{term=T2{tag=Imul;a;b}} -> Integer.imul condition a b constr
      | Integer{term=T2{tag=Idiv;a;b}} -> Integer.idiv condition a b constr
      | Integer{term=T2{tag=Imod;a;b}} -> Integer.imod condition a b constr
      | Integer{term=T2{tag=Ishl;a;b}} -> Integer.ishl condition a b constr
      | Integer{term=T2{tag=Ishr;a;b}} -> Integer.ishr condition a b constr
      | Integer{term=T2{tag=Iand;a;b}} -> Integer.iand condition a b constr
      | Integer{term=T2{tag=Ior;a;b}}  -> Integer.ior  condition a b constr
      | Integer{term=T2{tag=Ixor;a;b}} -> Integer.ixor condition a b constr
      | Integer{term=T1{tag=Itimes k;a}} -> Integer.itimes k condition a constr
      (* | _ -> Codex_log.fatal "backward interp on %a not implemented" Constraints.pretty constr *)

      | Bool{term=Empty} -> []
      | Integer{term=Empty} -> []
      | Binary{term=Empty} -> []                             
    ;;    

  end

  module MakeWorklist(Comp:sig 
      val compare: Constraints.any -> Constraints.any -> int 
    end) = struct

    module Heap = Binary_heap.Make(struct 
        type t = Constraints.any
        let compare = Comp.compare
      end)

    module Hash = Hashtbl.Make(Constraints.Any);;

    type t = { heap: Heap.t;
               hash: Condition.t Hash.t }

    let create n = { heap = Heap.create (Constraints.Any(Constraints.Build.Boolean.true_)) n; hash = Hash.create n };;

    (* Note that we join conditions; else this would lead to
       exponential behaviour (when there is a if, we need to
       consider the code before or after the if only once). 

       But this means that not using None when propagation makes no
       improvement can degrade precision (and not only degrade
       performance). *)

    let add {heap;hash} any cond =
      (* Codex_log.feedback "worklist add %a" Constraints.pretty (let Constraints.Any x = any in Obj.magic x); *)
      Heap.add heap any;
      try let old =  Hash.find hash any in
        Hash.replace hash any (Condition.union old cond)
      with Not_found -> Hash.replace hash any cond
    ;;

    let pop_minimum {heap;hash} = 
      let any = Heap.pop_minimum heap in
      let cond = Hash.find hash any in
      (* Remove from hash? Normally this is not needed, because it
         won't be reused. *)
      (* Hash.remove hash any; *)
      (any,cond)
    ;;


    module Alt = struct
      (* Alternate version: consider conditions separately. Can have
         exponentional behaviour on if cascades. *)
      let add {heap;hash} any cond =
        Heap.add heap any;
        Hash.add hash any cond
      ;;

      let rec pop_minimum {heap;hash} =
        let any = Heap.minimum heap in
        try let cond = Hash.find hash any in
            Hash.remove hash any;
            (any,cond)
        with Not_found ->
          Heap.remove heap;
          pop_minimum {heap;hash}
      ;;
    end

    let (add,pop_maximum) =
      if option_worklist_merge_condition then (add,pop_minimum)
      else Alt.add,Alt.pop_minimum
    ;;

  end

  module BackwardWorklist = MakeWorklist(struct 
      open Constraints
      let compare a b = (Any.get_id_int b) - (Any.get_id_int a)
    end)

  module ForwardWorklist = MakeWorklist(struct 
      open Constraints
      let compare a b = (Any.get_id_int a) - (Any.get_id_int b)
    end)


  let backward_propagate  worklist =
    let fwd_worklist = ForwardWorklist.create 10 in
    begin try 
        let limit_count = ref 0 in
        let limit = 1000 in
        (* Possibly: also add the parents to the backward worklist;
           but make sure that elements are processed only once. *)
        while true do
          let (Constraints.Any(max_elt) as max_any,cond) = 
            BackwardWorklist.pop_maximum worklist 
          in
          Constraints.Parents.iter_on_parents max_elt (fun parent ->
              ForwardWorklist.add fwd_worklist parent cond
            );
          let changed = Backward.backward_interp cond max_any in
          changed |> List.iter (fun (cond,any) -> 
              let Constraints.Any x = any in
              (* Kernel.feedback "adding %a" Constraints.pretty x; *)
              assert (Constraints.Any.get_id_int any < Constraints.Any.get_id_int max_any);
              BackwardWorklist.add worklist any cond;
              incr limit_count;
              if !limit_count > limit
              then (Codex_log.warning "Propagation: limit found"; raise Binary_heap.Empty)
            )
        done
      with Binary_heap.Empty -> ()
    end;
    fwd_worklist
  ;;


  let assume dom cond =
    (* Should not require assumption on already-known facts. *)
    assert (get_query_boolean cond (get_bdd dom) = Lattices.Quadrivalent.Top);
    (* This avoids repeating in every domain. *)
    (* Codex_log.feedback "Maybe assuming on %a" Constraints.pretty cond; *)      
    let rescond = Constraints.Build.Boolean.(&&) cond dom.condition in
    (* We could special case these, but they never happen in practice. *)
    (* if Constraints.equal rescond dom.condition
     * then assert false
     * else if Constraints.equal rescond Constraints.Build.Boolean.false_
     * then assert false
     * else *)
    let resdom = {condition=rescond} in
    let cmap = State.get_boolean cond in
    (* The former does more sharing of condition. Both works, but the
       former is (~10%) slower and memory hungry. *)
    (* let Constraints.Bool{bdd} = cond in *)
    let bdd = get_bdd resdom in
    let cmap = BooleanConditionMap.refine ~inter:B.Boolean_Lattice.inter cmap ~cond:bdd B.Boolean_Forward.true_ in
    State.set_boolean cond cmap;
    let worklist = BackwardWorklist.create 30 in
    BackwardWorklist.add worklist (Constraints.Any cond) (get_bdd resdom);
    let _ = backward_propagate worklist in
    Some resdom
  ;;

end
