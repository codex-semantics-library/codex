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


(** This file corresponds to the symbolic expression domain described
    in [POPL'23] and [PLDI'24]. *)

module Log = Tracelog.Make(struct let category = "Domains.Constraint_domain2" end);;

[@@@warning "-33"]              (* Constraints.() gives spurious warning in 4.04.2 *)

(* Should we use first-order formula (over-approximation) or horn
   clauses (exact) to the SMT solver. *)
let option_translate: [ `First_order | `Horn ] = `Horn;;

(* How should we pretty-print constraints: only their value (easier to read),
   symbolically (better when there are issues with evaluation), or print both
   (more complete debugging info).  *)
let option_pretty_constraints: [`Symbolic | `Value | `Both ] = `Value

(* Option to introduce a fresh variable at every fixpoint
   step. Mandatory if we do not push/pop.

   Results: faster on almost all benchmarks, except papabench;
   probably the variable order is better that way for bdd
   construction. It also fixes a precision issues in t072. *)
let option_fresh_variable_every_time =
  (not Domains_constraints_constraint_propagation.option_push_and_pop) ||
  (not Domains_constraints_constraint_propagation.option_push_and_pop_on_restart) ||
  true;;

(* If true, whenever the result of a computation is a constant, we
   simplify the constraint using the constant. This allows to detect
   equality more often, and to have fewer variables.
   It shows a small gain on the benchmarks. *)
let option_semantic_simplification_constraints = true

module TC = Transfer_functions.Term
module Constraints_smt = Constraints.Smt

module Make
    (Constraints:Constraints.Constraints_sig.Constraints)
    (Domain:Constraint_domains_sig.Domain_S with module Constraints = Constraints)
  :Domain_sig.Base
    with type binary = TC.binary Constraints.t
    and type boolean = TC.boolean Constraints.t
= struct


  let name() = "Constraint_Domain2(" ^ (Domain.name) ^ ")";;
  let unique_id() = Domain_sig.Fresh_id.fresh @@ name();;

  type 'a identifier = 'a Constraints.t

  module Types = struct
    type binary = TC.binary identifier
    type integer = TC.integer identifier
    type boolean = TC.boolean identifier
  end

  include Types

  module Identifier = struct

    let pretty fmt x = Format.fprintf fmt "%a" Constraints.pretty x

    (* This is used by the above domains to decide whether the values
       are the same, and if they should be serialized. It is important
       to serialize if the constraints are equal and the domains are
       different, else we loose information about conditions. *)
    let _equal a b =
      let res = Constraints.equal a b in res
    ;;

    (* Actually, this check is done really often, and this
       approximation yields the correct result most of the
       time. Moreover, if we do not detect equality here, we will
       probably do it in fixpoint_step or nondet. Using this
       implementation gave me a 10-20% speedup in execution time. *)
    let equal a b = a == b;;

    (* This does not check for equality of the [domain] fields. It is not
       necessary for the only current use case: a map indexed by [t]s to
       implement focusing. *)
    let compare = Constraints.compare

    let hash = Constraints.hash

    let to_int = Constraints.hash
  end

  module Binary = struct
    type t = binary
    include Identifier
  end

  module Integer = struct
    type t = integer
    include Identifier
  end

  module Boolean = struct
    type t = boolean
    include Identifier
  end

  (**************** Context ****************)


  type context =
    {
      unique_id: int;

      (* Used for SMT queries. Could be computed form context_. *)
      mutable path_condition: Boolean.t;
      level:int;
      (* The signature of operations like biadd does not return a new context,
         so we change it inplace for these operations. This imperative signature may be useful
         e.g. to use the optimisations in Apron.  *)
      mutable domain:Domain.t
    }
  ;;

  (* Pair of constraints *)
  module TC_Binary = struct
    type t = TC.binary identifier
    include Identifier
  end

  module TC_Integer = struct
    type t = TC.integer identifier
    include Identifier
  end

  module TC_Boolean = struct
    type t = TC.boolean identifier
    include Identifier
  end

  module Pair(Key : Datatype_sig.S) = struct
    type t = Key.t * Key.t

    let compare (a1,b1) (a2,b2) =
      let c = Key.compare a1 a2 in
      if c <> 0 then c
      else Key.compare b1 b2

    let equal (a1,b1) (a2,b2) = Key.equal a1 a2 && Key.equal b1 b2

    let sdbm x y = y + (x lsl 16) + (x lsl 6) - x;;

    let hash (a,b) =
      sdbm (Key.hash a) (Key.hash b)
  end

  module MapPair = struct
    module Key = struct
      type 'key t = 'key identifier
      let to_int = Constraints.hash

      (* This implementation is correct, as here physical equality
         implies type equality. *)
      let polyeq: 'a t -> 'b t -> ('a, 'b) PatriciaTree.cmp = fun a b ->
        let open PatriciaTree in
        if (Obj.magic a == Obj.magic b)
        then (Obj.magic Eq)
        else Diff
      ;;

    end
    module Value2 = struct
      type ('key,'value) t = 'key Constraints.t
    end
    module Map2 = PatriciaTree.MakeHeterogeneousMap(Key)(Value2)
    module Value1 = struct
      type ('key,'value) t = 'key Constraints.t Map2.t
    end
    module Map1 = PatriciaTree.MakeHeterogeneousMap(Key)(Value1)
    type t = unit Map2.t Map1.t
    let find: 'a Key.t -> 'a Key.t -> t -> 'a Key.t =
      fun key1 key2 map1 ->
      let map2 = Map1.find key1 map1 in
      Map2.find key2 map2
    let mem: 'a Key.t -> 'a Key.t -> t -> bool =
      fun key1 key2 map1 ->
      match Map1.find key1 map1 with
      | exception Not_found -> false
      | map2 -> Map2.mem key2 map2
    ;;

    let add: 'a Key.t -> 'a Key.t -> 'a Key.t -> t -> t =
      fun key1 key2 value map1 ->
      let map2 = match Map1.find key1 map1 with
        | exception Not_found -> Map2.singleton key2 value
        | map2 -> Map2.add key2 value map2
      in Map1.add key1 map2 map1
    ;;
    let empty = Map1.empty
  end


  module Common = struct
    module Key = struct
      include Constraints.Any
      let to_int = get_id_int
    end
    module Map = PatriciaTree.MakeMap(Key)
    module Set = PatriciaTree.MakeSet(Key)
  end


  (* Set of pairs of constraints. *)
  module SetPair = struct
    open Common
    type t = Set.t Map.t

    let mem key1 key2 map1 =
      match Map.find key1 map1 with
      | exception Not_found -> false
      | map2 -> Set.mem key2 map2
    ;;

    let add key1 key2 value map1 =
      let set2 = match Map.find key1 map1 with
        | exception Not_found -> Set.singleton key2
        | set2 -> Set.add key2 set2
      in Map.add key1 set2 map1
    ;;
    let empty = Map.empty

  end


  (**************** Context ****************)


  module Context = struct
    type t = context
    let level ctx = ctx.level

    let copy x = { x with path_condition = x.path_condition; domain = x.domain }

    (* Having a ConsLeftEmpty/ConsRightEmpty here could maybe allow
        getting rid of the empty constructor (that would become a
        special case of phi instead).  But we would still have to
        create new name for the new phi variable, and not reuse the
        left or the right term.

        TODO: Think how this could allow the implementation of sum
        and option types in the base language with a "normal"
        (without option types, but with phi that can have empty
        arguments) SSA for the integer language. If that works, it
        would be very nice. *)

    type 'a mapping =
      | EmptyMapping: unit mapping
      | ConsSame: 'a Constraints.t * 'b mapping -> ('a identifier * 'b) mapping
      | ConsDifferent: 'a Constraints.t * 'a Constraints.t * 'b mapping -> ('a identifier * 'b) mapping
    ;;


    (* MAYBE: it could be further simplified into a set of pairs, but
       then tuple order might change between fixpoint iterations (but
       possibly not, if we create fresh variables with ids always in
       the same order.

       However, the handling differs between nondet and fixpoint step,
       so possibly not a good idea.

       One easy simplification would be to get rid of ConsSame. *)
    type 'a in_tuple = { mapping: 'a mapping;  } [@@unboxed]
    type 'a in_acc = bool * 'a in_tuple
    type 'a out_tuple = { phi: MapPair.t }

    type ('a,'b) result =
        Result: bool * 'some in_tuple * (t -> 'some out_tuple -> 'a * 'b out_tuple) -> ('a,'b) result

    type empty_tuple = unit
    let empty_tuple () = { mapping = EmptyMapping; }

  end
  open Context

  (* Initialize all the constants so that we know they are in, and we do not need to change
     the context for them (we can just return the identifier). *)
  let root_domain() =
    let domain = Domain.top in
    let domain = Domain.Boolean_Forward.true_ domain Constraints.Build.Boolean.true_ in
    let domain = Domain.Boolean_Forward.false_ domain Constraints.Build.Boolean.false_ in
    let domain = Domain.boolean_empty domain Constraints.Build.Boolean.empty in
    let domain = Domain.Integer_Forward.iconst Z.zero domain Constraints.Build.Integer.zero in
    let domain = Domain.Integer_Forward.iconst Z.one domain Constraints.Build.Integer.one in
    let domain = Domain.integer_empty domain Constraints.Build.Integer.empty in
    domain
  ;;

  let unique_id_ref = ref 0;;
  let get_unique_id() = incr unique_id_ref; !unique_id_ref;;

  let root_context() =

    let ctx = {
      unique_id = get_unique_id();
      path_condition = Constraints.Build.Boolean.true_;
      level = 0;
      domain = root_domain()
    }
    in
    ctx
  ;;

  let context_pretty fmt ctx = Format.fprintf fmt "Context{id=%d,%a}" ctx.unique_id Domain.pretty ctx.domain

  (**************** Transfer functions ****************)

  (* We use the state monad using domain as the state. *)
  type 'a m = Domain.t -> 'a * Domain.t
  let return x = fun domain -> x,domain
  let (>>=) m f = fun dom1 -> let (res,dom2) = m dom1 in (f res) dom2

  (* Special version of the run function of the State monad. *)
  let run ctx f =
    let dom = ctx.domain in
      let (res:'a identifier),dom = f dom in
      (* Codex_log.feedback "run: constraint = %a" Constraints.pretty (Domain.to_constraint dom); *)
      (* Codex_log.feedback "run: created identifier %a" Identifier.pretty res; *)
      ctx.domain <- dom;
      res
  ;;

  (* include Transfer_functions.Builtin.Make(Types)(Context) *)

  (* This optimization is very important, else in particular we build
     complex formula that are actually empty. *)
  (* Note: We can just build the constrain here, because the value for
     the boolean constants is already evaluated. *)
  let opt_boolean x domain =
      match Domain.Query.boolean domain x with
      | Lattices.Quadrivalent.True -> Constraints.Build.Boolean.true_, domain
      | Lattices.Quadrivalent.False -> Constraints.Build.Boolean.false_, domain
      | Lattices.Quadrivalent.Bottom -> Constraints.Build.Boolean.empty, domain
      | _ -> x, domain
  ;;

  let ar0 constrain fdomain domain =
    constrain,fdomain domain constrain
  ;;

  let ar0_boolean = ar0
  let ar0_integer ctx = ar0
  let ar0_binary = ar0

  let iconst k = ar0 (Constraints.Build.Integer.iconst k) (Domain.Integer_Forward.iconst k);;
  let biconst ~size k = ar0 (Constraints.Build.Binary.biconst ~size k) (Domain.Binary_Forward.biconst ~size k);;

  let opt_integer x domain =
    if option_semantic_simplification_constraints
    then
        let value = Domain.Query.integer domain x in
        match Domain.Query.is_singleton_int value with
        | None -> x, domain
        | Some k -> iconst k domain
    else x, domain
  ;;

  let opt_binary ~size x domain =
    if option_semantic_simplification_constraints
    then
        let value = Domain.Query.binary ~size domain x in
        match Domain.Query.binary_is_singleton ~size value with
        | None -> x,domain
        | Some k -> biconst ~size k domain
    else x,domain
  ;;


  let ar1 fconstrain fdomain a domain =
      let res = fconstrain a in
      let dom = fdomain domain a res in
      res, dom
  ;;

  let _ar1_integer_integer = ar1
  let ar1_integer_integer ctx fconstrain fdomain a =
    run ctx @@ (ar1 fconstrain fdomain a >>= opt_integer)
  ;;

  let _ar1_binary_binary ~size = ar1
  let ar1_binary_binary ~size ctx fconstrain fdomain a  =
    run ctx @@ (ar1 fconstrain fdomain a >>= opt_binary ~size)
  ;;

  let ar1_boolean_binary ~size ctx  fconstrain fdomain a  =
    run ctx @@ (ar1 fconstrain fdomain a >>= opt_binary ~size)
  ;;




  let ar1_boolean_boolean ctx fconstrain fdomain a  =
    run ctx @@ (ar1 fconstrain fdomain a >>= opt_boolean)
  ;;


  let ar2 ctx fconstrain fdomain a b dom =
      let res = fconstrain a b in
      let dom = fdomain dom a b res in
      res, dom
  ;;


  let ar2' ctx f a b dom =
    let (constrain,dom) = f dom a b in
    constrain, dom
  ;;

  let ar2_binary_binary_binary' ~size ctx f a b =
    run ctx @@ (ar2' ctx f a b >>= opt_binary ~size)


  let ar2_binary_binary_binary ~size ctx fconstrain fdomain a b =
    run ctx @@ (ar2 ctx fconstrain fdomain a b >>= opt_binary ~size)


  let ar2_integer_integer_integer ctx fconstrain fdomain a b =
    run ctx @@ (ar2 ctx fconstrain fdomain a b >>= opt_integer)

  let ar2_boolean_boolean_boolean ctx fconstrain fdomain a b =
    (ar2 ctx fconstrain fdomain a b >>= opt_boolean)
  ;;

  let ar2_integer_integer_boolean =
    (if option_semantic_simplification_constraints
    then ar2_boolean_boolean_boolean
    else ar2 )
  ;;

  let ar2_binary_binary_boolean ~size =
    if option_semantic_simplification_constraints
    then ar2_boolean_boolean_boolean
    else ar2
  ;;

  let ar2_integer_integer_boolean ctx fconstrain fdomain a b =
    run ctx @@ ar2_integer_integer_boolean ctx fconstrain fdomain a b
  ;;

  let ar2_binary_binary_boolean ~size ctx fconstrain fdomain a b =
    run ctx @@ ar2_binary_binary_boolean ~size ctx fconstrain fdomain a b
  ;;

  let ar2_boolean_boolean_boolean ctx fconstrain fdomain a b =
    run ctx @@ ar2_boolean_boolean_boolean ctx fconstrain fdomain a b
  ;;

  (* It is more precise to assume individual simple constraints; this function does that. *)
  let rec assume ctx cond pos domain =
    (* Codex_log.feedback "rec assuming %b %a %a" pos Constraints.pretty cond Domain.pretty domain; *)
    let join a b pos domain =
       Codex_log.warning "This was never tested";
       let dom1 = assume ctx a pos domain in
       let dom2 = assume ctx b pos domain in
       let tup = Immutable_array.empty in
       match dom1,dom2 with
       | None, dom | dom, None -> dom
       | Some doma, Some domb -> Some(Domain.nondet ~doma ~tupa:tup ~domb ~tupb:tup ~tupres:tup)
    in
    let inter a b pos domain = match assume ctx a pos domain with
       | None -> None
       | Some domain -> assume ctx b pos domain
    in
    match pos, cond with
    | _, Constraints.(Bool{term=T1{tag=TC.Not;a=Constraints.Bool _ as a}}) ->
       assume ctx a (not pos) domain
    | true,Constraints.(Bool{term=T2{tag=TC.And; a=Constraints.Bool _ as a; b=Constraints.Bool _ as b}}) ->
       inter a b pos domain
    (* Same, but we cannot factorize with OCaml 4.05 *)
    | false,Constraints.(Bool{term=T2{tag=TC.Or; a=Constraints.Bool _ as a; b=Constraints.Bool _ as b}}) ->
       inter a b pos domain
    | true, Constraints.(Bool{term=T2{tag=TC.Or; a=Constraints.Bool _ as a; b=Constraints.Bool _ as b}}) ->
       join a b pos domain
    | false, Constraints.(Bool{term=T2{tag=TC.And; a=Constraints.Bool _ as a; b=Constraints.Bool _ as b}}) ->
       join a b pos domain
    | _ ->
       match pos, Domain.Query.convert_to_quadrivalent @@ Domain.Query.boolean domain cond with
       | (_, Lattices.Quadrivalent.Bottom)
         | true, Lattices.Quadrivalent.False
         | false, Lattices.Quadrivalent.True -> None
       | true, Lattices.Quadrivalent.True
         | false,Lattices.Quadrivalent.False -> Some domain
       | (true, Lattices.Quadrivalent.Top) -> Domain.assume domain cond
       | (false,Lattices.Quadrivalent.Top) ->
          (* Need to evaluate the condition before propagation. *)
          let ncond = Constraints.Build.Boolean.not cond in
          let domain = Domain.Boolean_Forward.not domain cond ncond in
          Domain.assume domain ncond

  let assume ctx cond =
    (* Codex_log.feedback "Constraint_domain2.newassume %b %b" (cond = Bottom) (ctx.domain = None); *)

    (* Codex_log.feedback "Constraint_domain2.newassume: non bottom case %a" Constraints.pretty constraincond; *)
    let domain = assume ctx cond true ctx.domain in
    match domain with
    | None -> None
    | Some domain ->
      begin
        (* Test in case the condition became bottom after the propagation *)
        match Domain.Query.convert_to_quadrivalent @@ Domain.Query.boolean domain cond with
        | Lattices.Quadrivalent.(False | Bottom) -> None
        | Lattices.Quadrivalent.(True | Top) ->
          Some { domain;
                 unique_id = get_unique_id();
                 path_condition = Constraints.Build.Boolean.(&&) cond ctx.path_condition ;
                 level = ctx.level }
      end

  ;;

  (* Eventually, we did not assume simple constraints individually, and it performs better. *)
  let assume ctx cond =
    (* Codex_log.feedback "Constraint_domain2.newassume %b %b" (cond = Bottom) (ctx.domain = None); *)
    (* Codex_log.feedback "Constraint_domain2.newassume: non bottom case %a" Constraints.pretty constraincond; *)
    match Domain.Query.convert_to_quadrivalent @@ Domain.Query.boolean ctx.domain cond with
      | Lattices.Quadrivalent.True -> Some ctx
      | Lattices.Quadrivalent.(False | Bottom) -> None
      | Lattices.Quadrivalent.Top ->
        begin
          match Domain.assume ctx.domain cond with
          | None -> None
          | Some domain -> begin
              match Domain.Query.convert_to_quadrivalent @@ Domain.Query.boolean domain cond with
              | Lattices.Quadrivalent.(False | Bottom) -> None
              | Lattices.Quadrivalent.(True | Top) ->
                Some { domain;
                       unique_id = get_unique_id();
                       path_condition = Constraints.Build.Boolean.(&&) cond ctx.path_condition ;
                       level = ctx.level }
            end
        end
  ;;




  let imperative_assume ctx cond =
    match assume ctx cond with
    | None -> raise Domain_sig.Bottom (* Should not happen, the condition
                                   should only limit fresh values, not
                                    create a bottom. *)
    | Some newctx -> begin
        ctx.domain <- newctx.domain;
        ctx.path_condition <- Constraints.Build.Boolean.(&&) cond ctx.path_condition
      end
  ;;

  let imperative_assign_context ctx newctx =
    ctx.domain <- newctx.domain;
    ctx.path_condition <- newctx.path_condition
  ;;

  module Boolean_Forward = struct
    let (||) ctx = ar2_boolean_boolean_boolean ctx Constraints.Build.Boolean.(||) Domain.Boolean_Forward.(||)
    let (&&) ctx = ar2_boolean_boolean_boolean ctx Constraints.Build.Boolean.(&&) Domain.Boolean_Forward.(&&)
    let not ctx = ar1_boolean_boolean ctx Constraints.Build.Boolean.not Domain.Boolean_Forward.not
    (* Note: we avoid creating those every time. *)
    let true_ ctx = run ctx @@ ar0_boolean Constraints.Build.Boolean.true_ Domain.Boolean_Forward.true_
    let true_ = let res = Constraints.Build.Boolean.true_ in fun ctx -> res
    let false_ ctx = run ctx @@ ar0_boolean Constraints.Build.Boolean.false_ Domain.Boolean_Forward.false_
    let false_ = let res = Constraints.Build.Boolean.false_ in fun ctx -> res
  end

  module Integer_Forward' = struct
    let ile ctx = ar2_integer_integer_boolean ctx Constraints.Build.Integer.ile Domain.Integer_Forward.ile
    let ieq ctx = ar2_integer_integer_boolean ctx Constraints.Build.Integer.ieq Domain.Integer_Forward.ieq

    let iconst k ctx = run ctx @@ ar0_integer ctx (Constraints.Build.Integer.iconst k) (Domain.Integer_Forward.iconst k)
    let one ctx = iconst Z.one ctx
    let one = let res = Constraints.Build.Integer.one in fun ctx -> res
    let zero ctx = iconst Z.zero ctx
    let zero = let res = Constraints.Build.Integer.zero in fun ctx -> res

    let ixor ctx = ar2_integer_integer_integer ctx Constraints.Build.Integer.ixor Domain.Integer_Forward.ixor
    let ior  ctx = ar2_integer_integer_integer ctx Constraints.Build.Integer.ior  Domain.Integer_Forward.ior
    let iand ctx = ar2_integer_integer_integer ctx Constraints.Build.Integer.iand Domain.Integer_Forward.iand
    let ishr ctx = ar2_integer_integer_integer ctx Constraints.Build.Integer.ishr Domain.Integer_Forward.ishr
    let ishl ctx = ar2_integer_integer_integer ctx Constraints.Build.Integer.ishl Domain.Integer_Forward.ishl
    let imod ctx = ar2_integer_integer_integer ctx Constraints.Build.Integer.imod Domain.Integer_Forward.imod
    let idiv ctx = ar2_integer_integer_integer ctx Constraints.Build.Integer.idiv Domain.Integer_Forward.idiv
    let imul ctx = ar2_integer_integer_integer ctx Constraints.Build.Integer.imul Domain.Integer_Forward.imul
    let iadd ctx = ar2_integer_integer_integer ctx Constraints.Build.Integer.iadd Domain.Integer_Forward.iadd
    let isub ctx = ar2_integer_integer_integer ctx Constraints.Build.Integer.isub Domain.Integer_Forward.isub
    let itimes k ctx = ar1_integer_integer ctx (Constraints.Build.Integer.itimes k) (Domain.Integer_Forward.itimes k)
  end

  module Integer_Forward = struct
    include Integer_Forward'

    (* Rewrite x + x as 2 * x. *)
    let iadd ctx a' b' = match a',b' with
      | a,b ->
        if a == b
        then
          let two = Z.of_int 2 in
          let two = (Constraints.Build.Integer.iconst two) in
          let constrain = Constraints.Build.Integer.imul two a in
          ctx.domain <- Domain.Integer_Forward.imul ctx.domain two a constrain;
          constrain
        else iadd ctx a' b'
    ;;

  end

  module Binary_Forward = struct

    module No_Rewrite = struct
      let default fconstrain fdomain dom a b =
        let constrain = fconstrain a b in
        let domain = fdomain dom a b constrain in
        constrain,domain

      let biadd ~size ~nsw ~nuw ~nusw = default (Constraints.Build.Binary.biadd ~size ~nsw ~nuw ~nusw) (Domain.Binary_Forward.biadd ~size ~nsw ~nuw ~nusw)
      let bisub ~size ~nsw ~nuw ~nusw = default (Constraints.Build.Binary.bisub ~size ~nsw ~nuw ~nusw) (Domain.Binary_Forward.bisub ~size ~nsw ~nuw ~nusw)
      let bimul ~size ~nsw ~nuw = default (Constraints.Build.Binary.bimul ~size ~nsw ~nuw) (Domain.Binary_Forward.bimul ~size ~nsw ~nuw)

      let bxor ~size = default (Constraints.Build.Binary.bxor ~size) (Domain.Binary_Forward.bxor ~size)
      let bor ~size = default (Constraints.Build.Binary.bor ~size) (Domain.Binary_Forward.bor ~size)
      let band ~size = default (Constraints.Build.Binary.band ~size) (Domain.Binary_Forward.band ~size)

      let bashr ~size = default (Constraints.Build.Binary.bashr ~size) (Domain.Binary_Forward.bashr ~size)
      let blshr ~size = default (Constraints.Build.Binary.blshr ~size) (Domain.Binary_Forward.blshr ~size)
      let bshl ~size ~nsw ~nuw = default (Constraints.Build.Binary.bshl ~size ~nsw ~nuw) (Domain.Binary_Forward.bshl ~size ~nsw ~nuw)

      let bisdiv ~size = default (Constraints.Build.Binary.bisdiv ~size) (Domain.Binary_Forward.bisdiv ~size)
      let biudiv ~size = default (Constraints.Build.Binary.biudiv ~size) (Domain.Binary_Forward.biudiv ~size)
      let bismod ~size = default (Constraints.Build.Binary.bismod ~size) (Domain.Binary_Forward.bismod ~size)
      let biumod ~size = default (Constraints.Build.Binary.biumod ~size) (Domain.Binary_Forward.biumod ~size)

      let bextract ~size ~index ~oldsize dom a =
        let constrain = Constraints.Build.Binary.bextract ~size ~index ~oldsize a in
        let domain = Domain.Binary_Forward.bextract ~size ~index ~oldsize dom a constrain in
        constrain, domain
    end

    (* Rewrite shifts + additions/soustractions into multiplication by a
       constant. This is a common pattern in binary analysis. *)
    module Rewrite = struct
      include No_Rewrite

      let extract = function
        | Constraints.(Binary{term=T2{tag=TC.Bimul _size;
                                      a=Binary{term=T0{tag=TC.Biconst(_size2,k)}};
                                      b=Binary _ as b}}) -> (k,b)
        | Constraints.(Binary{term=T2{tag=TC.Bimul _size;
                                      a=Binary _ as a;
                                      b=Binary{term=T0{tag=TC.Biconst(_size2,k)}}}}) -> (k,a)
        | x-> (Z.one, x)
      ;;

      let extract_sum = function
        | Constraints.(Binary{term=T2{tag=TC.Biadd _size;
                                      a=Binary _ as a;
                                      b=Binary{term=T0{tag=TC.Biconst(_size2,k)};}}}) ->
            (a,k)
        | Constraints.(Binary{term=T2{tag=TC.Biadd _size;
                                      a=Binary{term=T0{tag=TC.Biconst(_size2,k)}};
                                      b=Binary _ as b;}}) ->
            (b,k)
        | x -> (x, Z.zero)

      let rec bimul ~size ~nsw ~nuw domain a b =
        match a,b with
        | Constraints.(Binary{term=T0{tag=TC.Biconst(_size1,k1)}}),
          Constraints.(Binary{term=T2{tag=TC.Bimul _size;a=Binary{term=T0{tag=TC.Biconst(_size2,k2)}};b=Binary _ as x}}) ->
          bitimes ~size ~nsw ~nuw domain (Z.mul k1 k2) x
        | Constraints.(Binary{term=T2{tag=TC.Bimul _size;a=Binary{term=T0{tag=TC.Biconst(_size2,k2)}};b=Binary _ as x}}),
          Constraints.(Binary{term=T0{tag=TC.Biconst(_size1,k1)}}) ->
          bitimes ~size ~nsw ~nuw domain (Z.mul k1 k2) x
        | _ -> No_Rewrite.bimul ~size ~nsw ~nuw domain a b

      and bitimes ~size ~nsw ~nuw domain k x =
        if Z.equal k Z.zero then (Constraints.Build.Binary.biconst ~size Z.zero), domain
        else if Z.equal k Z.one then (x,domain)
        else
          let default () =
            let constr = (Constraints.Build.Binary.biconst ~size k) in
            (* optional, as domains should handle evaluation of constants themselves *)
            let domain = Domain.Binary_Forward.biconst ~size k domain constr in
            No_Rewrite.bimul ~size ~nsw ~nuw domain constr x
          in
          (* Simplify k*(x' + c) into k*(x' - d), with 0<d<c, when possible.
             Useful when the compiler does e.g.  16*(N+(2^28-1)) instead of
             straightforwardly 16*(N-1), for optimization reasons. When that happens,
             when dividing by 16 to get the index in an array, we have another
             rewrite that returns N+(2^28-1) and that's actually incorrect, so
             we want to return N-1 instead. It looks like bad engineering that
             the other rewrite (in [bisdiv]) may be unsound, and that we have
             to fix it here, though. *)
          (* The rewrite is based on the following theorem:
             Let k,x,c be in [0,2^size - 1]. Assume k*c < 2^size. Let r be the
             remainer in the Euclidean division of kc by 2^size. Then:
             - If k divides (2^size-r) and r > 2^size - k*c, then by defining
               d = (2^size-r) / k, we have
               * k(x+c) congruent to k(x-d) modulo 2^size, and
               * 0 < d < c.
             - Otherwise, then there exists no d that satisfies the two
               constraints above.
          *)
          let x',c = extract_sum x in
          let two_pow_size = Z.shift_left Z.one size in
          let kc = Z.mul k c in
          let q,r = Z.ediv_rem kc two_pow_size in
          let cond1 = Z.equal q Z.zero in
          let cond2 = Z.equal Z.zero @@ Z.erem (Z.sub two_pow_size r) k in
          let cond3 = Z.gt r (Z.sub two_pow_size kc) in
          if cond1 && cond2 && cond3 then
            let d = Z.divexact (Z.sub two_pow_size r) k in
            let constr_d = Constraints.Build.Binary.biconst ~size d in
            let domain = Domain.Binary_Forward.biconst ~size d domain constr_d in
            let interm,domain = No_Rewrite.bisub ~size ~nsw ~nuw ~nusw:false domain x' constr_d in
            let constr_k = Constraints.Build.Binary.biconst ~size k in
            let domain = Domain.Binary_Forward.biconst ~size k domain constr_k in
            No_Rewrite.bimul ~size ~nsw ~nuw domain constr_k interm
          else
            default ()

      (* XXX: need to check for signed overflow, otherwise unsound *)
      and bisdiv ~size domain a b =
      match b with
      | Constraints.(Binary{term=T0{tag=TC.Biconst(_size,k)}}) when not @@ Z.equal k Z.zero ->
          let ka,va = extract a in
          if Z.equal Z.zero (Z.rem ka k) then
            bitimes ~size ~nsw:false ~nuw:false domain (Z.divexact ka k) va
          else
            No_Rewrite.bisdiv ~size domain a b
      | _ -> No_Rewrite.bisdiv ~size domain a b

      and biadd ~size ~nsw ~nuw ~nusw domain a b =
        match a,b with
        | Constraints.(Binary{term=T0{tag=TC.Biconst(_size,k)}}), _
              when Z.equal k Z.zero ->
            b, domain
        | _, Constraints.(Binary{term=T0{tag=TC.Biconst(_size,k)}})
              when Z.equal k Z.zero ->
            a, domain
        | Constraints.(Binary{term=T2{tag=TC.Bisub _size';
                                      a=Binary _ as x;
                                      b=Binary{term=T0{tag=TC.Biconst(_size1,k1)}}}},
                       Binary{term=T0{tag=TC.Biconst (_size2, k2)}})
              when Z.equal k1 k2 ->
            (* (x-k)+k -> x *)
            x, domain
        | Constraints.(Binary{term=T0{tag=TC.Biconst (_size2, k2)}},
                       Binary{term=T2{tag=TC.Bisub _size';
                                      a=Binary _ as x;
                                      b=Binary{term=T0{tag=TC.Biconst(_size1,k1)}}}})
              when Z.equal k1 k2 ->
            (* k+(x-k) -> x *)
            x, domain
        | Constraints.(Binary{term=T2{tag=TC.Biadd _size';
                                      a=Binary _ as x;
                                      b=Binary{term=T0{tag=TC.Biconst(_size1,k1)}}}},
                       Binary{term=T0{tag=TC.Biconst (_size2, k2)}})
              when Z.equal (Z.rem (Z.add k1 k2) (Z.shift_left Z.one size)) Z.zero ->
            (* (x+k1)+k2 -> x when k1+k2 = 0 mod 2^size *)
            x, domain
        | Constraints.(Binary{term=T2{tag=TC.Biadd _size';
                                      a=Binary{term=T0{tag=TC.Biconst(_size1,k1)}};
                                      b=Binary _ as x}},
                       Binary{term=T0{tag=TC.Biconst (_size2, k2)}})
              when Z.equal (Z.rem (Z.add k1 k2) (Z.shift_left Z.one size)) Z.zero ->
            (* (k1+x)+k2 -> x when k1+k2 = 0 mod 2^size *)
            x, domain

        | Constraints.(Binary{term=T2{tag=TC.Biadd _size;
                                      a=Binary{term=T0{tag=TC.Biconst(_size1,k1)}};
                                      b=Binary _ as x}},
                       Binary{term=T0{tag=TC.Biconst (_size2, k2)}})
        | Constraints.(Binary{term=T0{tag=TC.Biconst (_size2, k2)}},
                       Binary{term=T2{tag=TC.Biadd _size;
                                      a=Binary{term=T0{tag=TC.Biconst(_size1,k1)}};
                                      b=Binary _ as x}})
        | Constraints.(Binary{term=T2{tag=TC.Biadd _size;
                                      a=Binary _ as x;
                                      b=Binary{term=T0{tag=TC.Biconst(_size1,k1)}}}},
                       Binary{term=T0{tag=TC.Biconst (_size2, k2)}})
        | Constraints.(Binary{term=T0{tag=TC.Biconst (_size2, k2)}},
                       Binary{term=T2{tag=TC.Biadd _size;
                                      a=Binary _ as x;
                                      b=Binary{term=T0{tag=TC.Biconst(_size1,k1)}}}})
              ->
            let k = Z.add k2 k1 in
            let c = Constraints.Build.Binary.biconst ~size k in
            let domain = Domain.Binary_Forward.biconst ~size k domain c in
            let res = Constraints.Build.Binary.biadd ~size ~nsw ~nuw ~nusw x c in
            let domain = Domain.Binary_Forward.biadd ~size ~nsw ~nuw ~nusw domain x c res in
            res, domain
        | _ ->
            let (ka,va) = extract a and (kb,vb) = extract b in
            if Constraints.equal va vb
            then bitimes ~size ~nsw ~nuw domain (Z.add ka kb) va
            else No_Rewrite.biadd ~size ~nsw ~nuw ~nusw domain a b

      and bisub ~size ~nsw ~nuw ~nusw domain a b =
        match a,b with
        | _, Constraints.(Binary{term=T0{tag=TC.Biconst(_size,k)}})
              when Z.equal k Z.zero ->
            a, domain

        | Constraints.(Binary{term=T2{tag=TC.Biadd _size;
                                      a=Binary{term=T0{tag=TC.Biconst(_size1,k1)}};
                                      b=Binary _ as x}},
                       Binary{term=T0{tag=TC.Biconst (_size2, k2)}})
                when Z.equal k1 k2 ->
              x, domain
        | Constraints.(Binary{term=T2{tag=TC.Biadd _size;
                                      a=Binary _ as x;
                                      b=Binary{term=T0{tag=TC.Biconst(_size1,k1)}}}},
                       Binary{term=T0{tag=TC.Biconst (_size2, k2)}})
              when Z.equal k1 k2 ->
            x, domain

        | _ ->
            let (ka,va) = extract a and (kb,vb) = extract b in
            if Constraints.equal va vb
            (* k_a*x - k_b*x -> (k_a - k_b)*x *)
            then bitimes ~size ~nsw ~nuw domain (Z.sub ka kb) va
            else if Constraints.equal va b
            (* k_a*x - x -> (k_a - 1)*x *)
            then bitimes ~size ~nsw ~nuw domain (Z.pred ka) va
            else No_Rewrite.bisub ~size ~nsw ~nuw ~nusw domain a b
      ;;

      let bshl ~size ~nsw ~nuw domain a b =
        match b with
        | Constraints.(Binary{term=T0{tag=TC.Biconst(_size,k)}}) ->
          let two_power_k = Z.(lsl) Z.one @@ Z.to_int k in
          bitimes ~size ~nsw ~nuw domain two_power_k a
        | _ -> No_Rewrite.bshl ~size ~nsw ~nuw domain a b

      let band ~size domain a b =
        if Constraints.equal a b then
          (* a & a *)
          a, domain
        else No_Rewrite.band ~size domain a b

      let rec bextract ~size ~index ~oldsize domain c =
        if (size = oldsize) && (index = 0) then c, domain
        else
        begin match c with
          | Constraints.(Binary{term=T2{tag=Bconcat(size1,size2);a;b}}) ->
              if (index < size2) && (size <= size2) then (bextract ~size ~index ~oldsize:size2 domain b)
              else
                if (index >= size2) && (size <= size1) then (bextract ~size ~index:(index - size2) ~oldsize:size1 domain a)
                else No_Rewrite.bextract ~size ~index ~oldsize domain c
          | _ -> No_Rewrite.bextract ~size ~index ~oldsize domain c
        end

    end

    module R = Rewrite

    let biadd ~size ~nsw ~nuw ~nusw ctx = ar2_binary_binary_binary' ~size ctx @@ R.biadd ~size ~nsw ~nuw ~nusw
    let bisub ~size ~nsw ~nuw ~nusw ctx = ar2_binary_binary_binary' ~size ctx @@ R.bisub ~size ~nsw ~nuw ~nusw
    let bimul ~size ~nsw ~nuw ctx = ar2_binary_binary_binary' ~size ctx @@ R.bimul ~size ~nsw ~nuw
    let bxor ~size ctx = ar2_binary_binary_binary' ~size ctx @@ R.bxor ~size
    let band ~size ctx = ar2_binary_binary_binary' ~size ctx @@ R.band ~size
    let bor ~size ctx = ar2_binary_binary_binary' ~size ctx @@ R.bor ~size
    let bashr ~size ctx = ar2_binary_binary_binary' ~size ctx @@ R.bashr ~size
    let blshr ~size ctx = ar2_binary_binary_binary' ~size ctx @@ R.blshr ~size
    let bshl ~size ~nsw ~nuw ctx = ar2_binary_binary_binary' ~size ctx @@ R.bshl ~size ~nsw ~nuw

    let bisdiv ~size ctx = ar2_binary_binary_binary' ~size ctx @@ R.bisdiv ~size
    let biudiv ~size ctx = ar2_binary_binary_binary' ~size ctx @@ R.biudiv ~size
    let bismod ~size ctx = ar2_binary_binary_binary' ~size ctx @@ R.bismod ~size
    let biumod ~size ctx = ar2_binary_binary_binary' ~size ctx @@ R.biumod ~size

    let bconcat ~size1 ~size2 ctx = ar2_binary_binary_binary ~size:(size1 + size2) ctx (Constraints.Build.Binary.bconcat ~size1 ~size2) (Domain.Binary_Forward.bconcat ~size1 ~size2)
    let bextract ~size ~index ~oldsize ctx = ar1_binary_binary ~size ctx  (Constraints.Build.Binary.bextract ~size ~index ~oldsize) (Domain.Binary_Forward.bextract ~size ~index ~oldsize)

    let biconst ~size k ctx = run ctx @@ ar0_binary (Constraints.Build.Binary.biconst ~size k) (Domain.Binary_Forward.biconst ~size k)

    let beq ~size ctx = ar2_binary_binary_boolean ~size ctx (Constraints.Build.Binary.beq ~size) (Domain.Binary_Forward.beq ~size)

    (* Quite ugly: we use ar2' just to compare the constraints. This optimises equality. *)
    let beq ~size ctx a b =
      let exception Opt of bool in
      try begin
        let _x,_dom = ar2' ctx (fun domain a b -> raise (Opt(Constraints.equal a b))) a b ctx.domain in
        (* Return boolean_empty? *)
        assert false
      end
      with Opt res ->
        if res
        then Boolean_Forward.true_ ctx
        else beq ~size ctx a b
    ;;

    let biule ~size ctx = ar2_binary_binary_boolean ~size ctx (Constraints.Build.Binary.biule ~size) (Domain.Binary_Forward.biule ~size)
    let bisle ~size ctx = ar2_binary_binary_boolean ~size ctx (Constraints.Build.Binary.bisle ~size) (Domain.Binary_Forward.bisle ~size)

    (* let bisle ~size ctx a b =
      let diff = bisub ~size ~nsw:false ~nuw:false ~nusw:false ctx b a in
      let zero = biconst ~size Z.zero ctx in
      let leq_zero = bisle ~size ctx diff zero in
      let eq_zero = beq ~size ctx diff zero in
      let geq_zero = Boolean_Forward.((||) ctx (not ctx leq_zero) eq_zero) in
      match Domain.Query.convert_to_quadrivalent @@ Domain.Query.boolean ctx.domain geq_zero with
      | Lattices.Quadrivalent.(True | False | Bottom) -> geq_zero
      | Lattices.Quadrivalent.Top -> bisle ~size ctx a b *)

    let biule ~size ctx a b =
      biule ~size ctx a b

    let bsext ~size ~oldsize ctx = ar1_binary_binary ~size ctx (Constraints.Build.Binary.bsext ~size ~oldsize) (Domain.Binary_Forward.bsext ~size ~oldsize)
    let buext ~size ~oldsize ctx = ar1_binary_binary ~size ctx (Constraints.Build.Binary.buext ~size ~oldsize) (Domain.Binary_Forward.buext ~size ~oldsize)
    let bchoose ~size cond ctx = ar1_binary_binary ~size ctx (Constraints.Build.Binary.bchoose ~size cond) (Domain.Binary_Forward.bchoose ~size cond)
    let bofbool ~size ctx = ar1_boolean_binary ~size ctx (Constraints.Build.Binary.bofbool ~size) (Domain.Binary_Forward.bofbool ~size)
    let bconcat ~size1 ~size2 ctx = ar2_binary_binary_binary ~size:(size1 + size2) ctx (Constraints.Build.Binary.bconcat ~size1 ~size2) (Domain.Binary_Forward.bconcat ~size1 ~size2)
    (* let bextract ~size ~index ~oldsize ctx = ar1_binary_binary ~size ctx  (Constraints.Build.Binary.bextract ~size ~index ~oldsize) (Domain.Binary_Forward.bextract ~size ~index ~oldsize) *)

    let bextract ~size ~index ~oldsize ctx a =
      let res, dom = R.bextract ~size ~index ~oldsize ctx.domain a in
      ctx.domain <- dom;
      res

    (* let bextract ~size ~index ~oldsize ctx = ar1_binary_binary ~size ctx  (Constraints.Build.Binary.bextract ~size ~index ~oldsize) (Domain.Binary_Forward.bextract ~size ~index ~oldsize) *)
    let biconst ~size k ctx = run ctx @@ ar0_binary (Constraints.Build.Binary.biconst ~size k) (Domain.Binary_Forward.biconst ~size k)
    let buninit ~size _ = assert false
    let bshift ~size ~offset ~max _ = assert false
    let bindex ~size _ = assert false
    let valid ~size _ = assert false
    let valid_ptr_arith ~size _ = assert false


    (** --------------------------- Bit-stealing operations --------------------------- **)


    (* Simplication 1 : ((p + t) + k) & m = 0 ==> (t - k) = 0 if m = 2^{n}-1 /\ k \in [-m;m] /\ (p & m) = 0 /\ (t & ~m) = 0 *)
    (* example : ((p + tag) - 3) & 7 = 0 ==> (tag - 3) = 0 if (p & 7) = 0 /\ (tag & ~7) = 0 *)

    (* Simplication 2 : (((p + t) + k)[0..l]) & m = 0 ==> (t - k) = 0 if m = 2^{n}-1 /\ n < l  /\ k \in [-m;m] /\ (p & m) = 0 /\ (t & ~m) = 0 *)
    (* example : ((p + tag) - 3) & 7 = 0 ==> (tag - 3) = 0 if (p & 7) = 0 /\ (tag & ~7) = 0 *)

    let beq ~size ctx a b =
      match a,b with
      | Constraints.(Binary{term=T0{tag=TC.Biconst(_size4,r)}}),
        Constraints.(Binary{term=T2{tag=TC.Band _size1;
                                    a=(Binary{term=T0{tag=TC.Biconst(_size3,l)}});
                                    b=(Binary{term=T2{tag=TC.Biadd _size2;
                                          a=(Binary{term=T2{tag=TC.Biadd _size6;
                                                a=Binary _ ;
                                                b=Binary _
                                            }});
                                          b=(Binary{term=T0{tag=TC.Biconst(_size7,k)}})
                                      }})
                                  }})
            when Z.equal Z.zero r
          ->
            assert false

      | Constraints.(Binary{term=T0{tag=TC.Biconst(_size4,r)}}),
        Constraints.(Binary{term=T2{tag=TC.Band _size1;
                                    a=(Binary{term=T0{tag=TC.Biconst(_size3,l)}} as mask);
                                    b=(Binary{term=T1{tag=TC.Bextract{size;index;oldsize};
                                          a=(Binary{term=T2{tag=TC.Biadd _size2;
                                                a=(Binary{term=T0{tag=TC.Biconst(_size7,k)}} as ofs);
                                                b=(Binary{term=T2{tag=TC.Biadd{size=_size8;nsw;nuw;nusw};
                                                      a=Binary _ as tag;
                                                      b=Binary _ as ptr
                                                  }})
                                            }})
                                      }})

                                  }})
            when Z.equal Z.zero r
          ->
            (* if m = 2^{n}-1 /\ k \in [-m;m] /\ (p & m) = 0 /\ (t & ~m) = 0 & (i = 0 && msb <= j)  *)
            Log.debug (fun p -> p "l : %a" Z.pp_print l);
            let lsb = Z.trailing_zeros l in (* position of the least significant bit *)
            Log.debug (fun p -> p "lsb : %d" lsb);
            let m = Z.shift_right l lsb in
            Log.debug (fun p -> p "m : %a" Z.pp_print m);
            let msucc = Z.succ m in
            let msb = Z.log2 msucc in
            Log.debug (fun p -> p "msb : %d" msb);
            let c1 = Z.equal msucc (Z.shift_left Z.one msb) in  (* m = 2^{n}-1 *)
            Log.debug (fun p -> p "is_mask : %b" c1);
            let k = Z.signed_extract k lsb msb in
            Log.debug (fun p -> p "offset : %a" Z.pp_print k);
            let c2 = Z.leq (Z.neg m) k && Z.leq k m in          (* k in [-m;m] *)
            Log.debug (fun p -> p "is offset in [-mask;mask] : %b" c2);
            let zero = biconst ~size:oldsize Z.zero ctx in
            let c3 = beq ~size:oldsize ctx (band ~size:oldsize ctx ptr mask) zero in
            Log.debug (fun p -> p "c3 : %a" (Domain.boolean_pretty ctx.domain) c3);
            let neg_mask = biconst ~size:oldsize (Z.lognot l) ctx in
            Log.debug (fun p -> p "neg_mask : %a" (Domain.binary_pretty ~size:oldsize ctx.domain) neg_mask);
            let c4 = beq ~size:oldsize ctx (band ~size:oldsize ctx tag neg_mask) zero in
            Log.debug (fun p -> p "c4 : %a" (Domain.boolean_pretty ctx.domain) c4);
            let c5 =
              match Domain.Query.(convert_to_quadrivalent @@ boolean ctx.domain @@ Boolean_Forward.(&&) ctx c3 c4) with
              | Lattices.Quadrivalent.True -> true
              | _ -> false
            in
            Log.debug (fun p -> p "c5 : %b" c5);
            let c6 = index = 0 && msb <= size in                (* *)
            Log.debug (fun p -> p "c6 : %b" c6);
            Log.debug (fun p -> p "c1 /\ c2 /\ c5 /\ c6 : %b" (c1 && c2 && c5 && c6));
            if c1 && c2 && c5 && c6 then
              beq ~size:oldsize ctx (biadd ~size ~nsw ~nuw ~nusw ctx tag ofs) zero    (* (tag + offset) = 0 *)

            else assert false

      | _ -> beq ~size ctx a b

    (* let rec beq_aux ~size ctx a b =
      Codex_log.debug "Constraint_domain2.beq (aux) %a %a" Identifier.pretty a Identifier.pretty b ;
      match a,b with (* to improve *)
      | Constraints.(Binary{term=T2{tag=TC.Band _size1; a=(Binary{term=T0{tag=TC.Biconst(_size2,k)}} as u); b=Binary _ as v}}), r
      | Constraints.(Binary{term=T2{tag=TC.Band _size1; a=Binary _ as v; b=(Binary{term=T0{tag=TC.Biconst(_size2,k)}} as u)}}), r
      | r, Constraints.(Binary{term=T2{tag=TC.Band _size1; a=(Binary{term=T0{tag=TC.Biconst(_size2,k)}} as u); b=Binary _ as v}})
      | r, Constraints.(Binary{term=T2{tag=TC.Band _size1; a=Binary _ as v; b=(Binary{term=T0{tag=TC.Biconst(_size2,k)}} as u)}}) ->
        begin
          Codex_log.debug "Constraint_domain2.beq reaching case ((x + y) & v) = r" ;
          (* ((x + y) & v) = r *)
          match v with
          | Constraints.(Binary{term=T1{tag=TC.Bextract{size;index;oldsize};a=x}}) when 0 = index && (Z.to_int k) < size ->
            begin
              let zero = biconst ~size:(oldsize - size) Z.zero ctx in
              let r = bconcat ~size1:(oldsize - size) ~size2:size ctx zero r in
              let u = bconcat ~size1:(oldsize - size) ~size2:size ctx zero u in
              let v = band ~size:oldsize ctx x u in
              Codex_log.debug "beq %a %a" Identifier.pretty v Identifier.pretty r ;
              beq_aux ~size:oldsize ctx v r
            end

          | Constraints.(Binary{term=T2{tag=TC.Biadd _size1; a=(Binary{term=T0{tag=TC.Biconst(_size2,l)}} as x); b=Binary _ as y}})
          | Constraints.(Binary{term=T2{tag=TC.Biadd _size1; a=Binary _ as y; b=(Binary{term=T0{tag=TC.Biconst(_size2,l)}} as x)}}) ->
            begin
              (* ((x + (m + n)) & v) = r *)
              match y with
              | Constraints.(Binary{term=T2{tag=TC.Biadd _size1; a=Binary _ as m; b=Binary _ as n}}) ->
                let r1 = band ~size ctx m u in
                let r2 = band ~size ctx n u in
                begin
                  match r1,r2 with
                  | Constraints.(Binary{term=T0{tag=TC.Biconst(_size,i)}}), _ ->
                    let r3 = bisub ~size ~nsw:false ~nuw:false ~nusw:false ctx r r1 in
                    let r4 = biadd ~size ~nsw:false ~nuw:false ~nusw:false ctx r3 x in
                    beq ~size ctx r4 r3
                  | _, Constraints.(Binary{term=T0{tag=TC.Biconst(_size,i)}}) ->
                    let r3 = bisub ~size ~nsw:false ~nuw:false ~nusw:false ctx r r2 in
                    let r4 = biadd ~size ~nsw:false ~nuw:false ~nusw:false ctx r3 x in
                    beq ~size ctx r4 r3
                  | _ -> assert false
                end
              | _ -> assert false
            end
          | _ -> beq ~size ctx a b
        end

      | _ -> beq ~size ctx a b ;; *)

    (*
    let bisle ~size ctx a b =
      Codex_log.debug "Constraint_domain2.bisle %a %a" Constraints.pretty a Constraints.pretty b ;
      let res = propagate_comp ~size bisle ctx a b in
      Codex_log.debug "Constraint_domain2.bisle returning %a" Constraints.pretty res ;
      res

    let beq ~size ctx a b =
      let res = propagate_comp ~size beq ctx a b in
      (* let res = beq ~size ctx a b in *)
      res
    *)

  end
  module Memory_Forward = Assert_False_Transfer_Functions.Memory.Memory_Forward

  (* Note: we do not create them every time *)
  (* let integer_empty ctx = let res = ar0_integer ctx Constraints.Build.Integer.empty Domain.integer_empty in run ctx res *)
  let integer_empty = let res = Constraints.Build.Integer.empty in fun ctx -> res
  (* let boolean_empty = let res = ar0_boolean Constraints.Build.Boolean.empty Domain.boolean_empty in fun ctx -> run ctx res *)
  let boolean_empty = let res = Constraints.Build.Boolean.empty in fun ctx -> res

  (* MAYBE: Avoid re-creation in most common cases *)
  let binary_empty ~size ctx =
    let res = ar0_binary (Constraints.Build.Binary.empty ~size) (Domain.binary_empty ~size) in
    run ctx res
  ;;

  let boolean_unknown ctx = run ctx @@ ar0_boolean (Constraints.Build.Boolean.unknown ~level:ctx.level) (Domain.boolean_unknown)
  let binary_unknown ~size ctx = run ctx @@ ar0_binary (Constraints.Build.Binary.unknown ~level:ctx.level ~size) (Domain.binary_unknown ~size)
  let integer_unknown ctx = run ctx @@ ar0_integer ctx (Constraints.Build.Integer.unknown ~level:ctx.level) (Domain.integer_unknown);;
  ;;



  (**************** Pretty printing ****************)

  module type Pretty_Constraints = sig
    val boolean_pretty: Context.t -> Format.formatter -> boolean -> unit
    val integer_pretty: Context.t -> Format.formatter -> integer -> unit
    val binary_pretty: size:int -> Context.t -> Format.formatter -> binary -> unit
  end

  module Pretty_Both:Pretty_Constraints = struct

    let boolean_pretty ctx fmt x =
        (* Format.fprintf fmt "value %a domain %a"
         *   (Domain.boolean_pretty x.domain) x.constrain
         *   Domain.pretty x.domain *)
          Format.fprintf fmt "%a (value %a)"
            Constraints.pretty x
            (Domain.boolean_pretty ctx.domain) x
    ;;

    let integer_pretty ctx fmt x =
          Format.fprintf fmt "%a (value %a)"
            Constraints.pretty x
            (Domain.integer_pretty ctx.domain) x
    ;;

    let binary_pretty ~size ctx fmt x =
          Format.fprintf fmt "%a (value %a)"
            Constraints.pretty x
          (Domain.binary_pretty ~size ctx.domain) x
    ;;

  end

  module Pretty_Value:Pretty_Constraints = struct

    let boolean_pretty ctx fmt x =
        (* Format.fprintf fmt "value %a domain %a"
         *   (Domain.boolean_pretty x.domain) x.constrain
         *   Domain.pretty x.domain *)
        Domain.boolean_pretty ctx.domain fmt x
    ;;

    let integer_pretty ctx fmt x =
      Domain.integer_pretty ctx.domain fmt x
        (* Format.fprintf fmt "value %a domain %a"
         *   (Domain.integer_pretty x.domain) x.constrain
         *   Domain.pretty x.domain *)
    ;;

    let binary_pretty ~size ctx fmt x =
        Domain.binary_pretty ~size ctx.domain fmt x
        (* Format.fprintf fmt "value %a domain %a"
         *   (Domain.integer_pretty x.domain) x.constrain
         *   Domain.pretty x.domain *)
    ;;

  end

  module Pretty_Symbolic:Pretty_Constraints = struct
    let boolean_pretty ctx = Identifier.pretty
    let integer_pretty ctx = Identifier.pretty
    let binary_pretty ~size ctx = Identifier.pretty
  end

  let boolean_pretty,integer_pretty,binary_pretty =
    let (module X : Pretty_Constraints) = (match option_pretty_constraints with
      | `Value -> (module Pretty_Value)
      | `Both -> (module Pretty_Both)
      | `Symbolic -> (module Pretty_Symbolic))
    in X.boolean_pretty,X.integer_pretty,X.binary_pretty
  ;;

  (**************** Tuple construction and fixpoint/nondet. ****************)

  (* Note that this operation is purely syntactic. Normally, the use
     of semantic information to transform identifiers is already done. *)

  let binary_is_empty ~size ctx (b:binary) = Constraints.equal b (Constraints.Build.Binary.empty ~size)
  let integer_is_empty ctx (i:integer) = Constraints.equal i Constraints.Build.Integer.empty
  let boolean_is_empty ctx (b:boolean) = Constraints.equal b Constraints.Build.Boolean.empty

  let serialize: 'hd 'tail . t -> 'hd identifier -> t -> 'hd identifier -> 'tail in_acc -> ('hd identifier, 'tail) Context.result =
    fun  ctxa a ctxb b (included, acc) ->
      let mapping =
        if Constraints.equal a b then ConsSame(a,acc.mapping)
        else ConsDifferent(a,b,acc.mapping)
      in
      let deserialize ctx ({phi} as out_tup) =
        (* Note: if I used different serializes, I would not have to re-test here. *)
        let constrain =
          if Constraints.equal a b then a
          else begin
            match MapPair.find a b phi  with
            | exception Not_found -> assert false
            | x -> x
          end
        in
        constrain, out_tup
      in
      Result(included, { mapping}, deserialize)
  ;;

  let serialize_binary ~size = serialize
  let serialize_integer = serialize
  let serialize_boolean = serialize

  (**************** Nondet and union. ****************)

  (* Traverse the phi arguments and make it a tuple. *)
  let build_phi_arguments in_tup =
    let filtereda,filteredb, map =
      let rec loop: type a. _ -> _ -> _ -> a mapping -> _ = fun la lb map -> function
        | EmptyMapping -> la,lb,map
        (* TODO: We could get rid of ConsSame, and maybe this filtering, by changing serialize. *)
        | ConsSame(_,mapping) -> loop la lb map mapping
        (* Global value numbering: remove redundant pairs.  *)
        | ConsDifferent(a,b,mapping) when MapPair.mem a b map -> loop la lb map mapping
        | ConsDifferent(a,b,mapping) ->
          let map = MapPair.add a b a map in
          loop ((Constraints.Any a)::la) ((Constraints.Any b)::lb) map mapping
      in loop [] [] MapPair.empty in_tup.mapping
    in filtereda,filteredb, map
  ;;

  (* Build the phi that will go in out_tup. *)
  let build_phi tupa tupb tupres =
    Immutable_array.fold_left3 (fun map a b c ->
        match a,b,c with
        | Constraints.(Any(Binary _ as a), Any(Binary _ as b), Any(Binary _ as c)) -> MapPair.add a b c map
        | Constraints.(Any(Integer _ as a), Any(Integer _ as b), Any(Integer _ as c)) -> MapPair.add a b c map
        | Constraints.(Any(Bool _ as a), Any(Bool _ as b), Any(Bool _ as c)) -> MapPair.add a b c map
        | _ -> Log.fatal (fun p -> p "Wrong type assertion")) MapPair.empty tupa tupb tupres
  ;;



  let nondet_same_context (ctx:context) (in_tup: _ in_tuple) =

    let filtereda,filteredb, map = build_phi_arguments in_tup in

    let tupa = Immutable_array.of_list filtereda in
    let tupb = Immutable_array.of_list filteredb in

    let dom = ctx.domain in
    let doma = dom in
    let domb = dom in

    let conda_bool = ctx.path_condition in
    let condb_bool = ctx.path_condition in

    let tupres =
      Constraints.Build.Tuple.nondet ~level:ctx.level
        ~conda_bool ~a:tupa
        ~condb_bool ~b:tupb
    in
    let domain =  Domain.nondet ~doma ~tupa ~domb ~tupb ~tupres in

    ctx.domain <- domain;

    let phi = build_phi tupa tupb tupres in
    {phi}
  ;;

  let union cond (ctx:context) (in_tup: _ in_tuple) =

    let filtereda,filteredb, map = build_phi_arguments in_tup in

    let tupa = Immutable_array.of_list filtereda in
    let tupb = Immutable_array.of_list filteredb in
    let tupres = Immutable_array.map2 (fun (Constraints.Any x1) (Constraints.Any x2) ->
       match (x1,x2) with
       | Constraints.Binary{size=s1}, Constraints.Binary{size=s2} ->
          assert(s1 == s2);
          Constraints.Any(Constraints.Build.Binary.bunion ~size:s1 cond x1 x2)
       | Constraints.Bool _, Constraints.Bool _ ->
         Constraints.Any(Constraints.Build.Boolean.bunion x1 x2)
       | _ -> assert false ) tupa tupb in
    let domres = ctx.domain in

    (* We reuse nondet for now. *)
    let domain =  Domain.nondet ~doma:domres ~tupa ~domb:domres ~tupb ~tupres in
    ctx.domain <- domain;
    let phi = build_phi tupa tupb tupres in
    {phi}
  ;;

  let typed_nondet2 (ctxa:context) (ctxb:context) (in_tup: _ in_tuple) =

    (* We filter etc. but we try to preserve the serialization order
       (the code could be simplified otherwise, in particular in_tup
       could consist in the mapping.

       We reuse MapPair as a set here; we reuse the first element as a
       value for typing purposes. Possibly we could implement SetPair
       instead. *)
    let filtereda,filteredb, map = build_phi_arguments in_tup in

    let tupa = Immutable_array.of_list filtereda in
    let tupb = Immutable_array.of_list filteredb in

    let doma = ctxa.domain in
    let domb = ctxb.domain in

    let conda_bool = ctxa.path_condition in
    let condb_bool = ctxb.path_condition in

    assert(ctxa.level = ctxb.level);

    (* Note: we could build the tuple by a map operation of the MapPair instead. *)
    let tupres =
      Constraints.Build.Tuple.nondet ~level:ctxa.level
        ~conda_bool ~a:tupa
        ~condb_bool ~b:tupb
    in
    let domain =  Domain.nondet ~doma ~tupa ~domb ~tupb ~tupres in

    let ctx = { domain = domain; level = ctxa.level; unique_id = get_unique_id();
                path_condition = Constraints.Build.Boolean.(||) ctxa.path_condition ctxb.path_condition; } in


    (* Build the final map. *)
    let phi = build_phi tupa tupb tupres in
    ctx,{phi}
  ;;


  (**************** New fixpoint computation. ****************)

  let widened_fixpoint_step ~widening_id ~previous ~next (includes,(in_tup:_ in_tuple)) =


    let widening_id: Domain_sig.Widening_Id.t = widening_id in
    let widening_id: int = (widening_id :> int) in

    Log.debug (fun p -> p "widened_fixpoint_step: arg is %b %a %a" includes context_pretty previous context_pretty next);

    (* Fixpoint step: generates fresh variables until we reach a
       fixpoint (we don't need to generate new variable
       anymores). Furthermore this domain is used as a functor to
       provide terms for the numerical domains.

       A simple detection of fixpoint would consist in waiting for
       next to be the same twice in a row. We can have fewer
       iterations by detecting when we do no longer need to introduce
       a variable, and when all previously introduced variables are
       mapped to a different expression. We use a unique widening_id
       to avoid confusing the variables that we created with others.

       This could could possibly be simplified by having [in_tup]
       being just a set of pairs. It seems better to preserve the
       ordering of the tuple (e.g. for debugging reasons), so we
       maintain the list of pairs variant instead.

       MAYBE: Also check that the variables are in order (they should
       be whent the fixpoint is reached; currently they are not,
       probably because we dont assign numbers in the tuple in the
       right order. *)



    (* Check if the parameter x is an inductive variable for the current widening point.  *)
    let is_inductive_variable (type u) (x:u Constraints.t) i :bool =
      match x with
      | Constraints.Bool{term = Constraints.Inductive_var {widening_id=w}} -> (w = widening_id)
      | Constraints.Binary{term = Constraints.Inductive_var {widening_id=w}} -> (w = widening_id)
      | Constraints.Integer{term = Constraints.Inductive_var {widening_id=w}} -> (w = widening_id)
      | Constraints.(Binary{term= Tuple_get(j,Inductive_vars{widening_id=w})}) -> (w = widening_id) (* && i = j *)
      | Constraints.(Bool{term= Tuple_get(j,Inductive_vars{widening_id=w})}) -> (w = widening_id)   (* && i = j *)
      | Constraints.(Integer{term= Tuple_get(j,Inductive_vars{widening_id=w})}) -> (w = widening_id)(* && i = j *)
      | _ -> false
    in

    (* In the [in_tup] pair of phi arguments, those coming from
       [previous] may already be inductive variables, and those from
       [next] are not (but may be the definition of these inductive
       variables).

       There are several cases:
       - [ConsSame] is the case phi(e,e) = e, we don't need to introduce any variable.
         TODO: Actually, we may even not register them in in_tup.
       - [ConsDifferent] phi(e1,e2): we create a new variable.
       - [ConsDifferent] phi(x,e) when every x is attached to the same
         definition e: we can reuse the variable x.

       If both phi(x,e1) and phi(x,e2) exist, then we need to create
       different variables x1 and x2 in the next iteration.

       To simplify, we create fresh variable on each iteration, unless
       it is the last iteration (every [ConsDifferent] case is of the
       form phi(x,e) where every x is attached to a single e.
    *)

    (* TODO: If the inductive_vars are part of a tuple, we may want to
       build a mapping (or an array) from indices in the tuple to
       their definition, that we may use to build the definition. Or
       we can just check that the induction variables are in the right
       order. *)
    let filtereda,filteredb,map,includes,i =
      let rec loop: type a. _ -> _ -> _ -> bool -> int -> a mapping -> _ = fun la lb map includes i -> function
        | EmptyMapping -> la,lb,map,includes,i
        (* We skip it when we have both arguments. *)
        | ConsSame(_,mapping) -> loop la lb map includes i mapping
        (* Global value numbering: if in the map, this cas was already handled. *)
        | ConsDifferent(a,b,mapping) when MapPair.mem a b map -> loop la lb map includes i mapping
        | ConsDifferent(a,b,mapping) ->
          let la' = ((Constraints.Any a)::la) and lb' = ((Constraints.Any b)::lb) in
          let oldmap = map in
          let map = MapPair.add a b a map in
          (* We have not reached a fixpoint, and need to introduce new
             induction variables:
             - if we still have some phi(a,b) where a is not an
               induction variable
             - if we have phi(a,b1) and phi(a,b2) where a is an
               induction variable: in this case we need to split the
               induction variable in two, i.e. the fixpoint is not
               finished. *)
          let includes =
            if is_inductive_variable a i
            then begin
              let two_definitions_for_a =  MapPair.Map1.mem a oldmap in
              not two_definitions_for_a && includes
            end else false
          in
          loop la' lb' map includes (i + 1) mapping
      in loop [] [] MapPair.empty includes 0 in_tup.mapping
    in

    let previous_tup = Immutable_array.of_list filtereda in
    let next_tup = Immutable_array.of_list filteredb in

    let ctxa = previous and ctxb = next in
    assert(ctxa.level = ctxb.level);

    let res_tup =
      if includes then begin
        (* Fixpoint reached on the symbolic expression domain reuse
           the variables., and update the inductive definition (by
           assignment). *)
        if Codex_config.term_group_inductive_variable_by_tuple then
          Log.error(fun p -> p "TODO: Change the definition of the Inductive_var here.")
        else
        Immutable_array.iter2 (fun phi def ->
            let open Constraints in
            let Any phi = phi in let Any def = def in
            let f: type a b. a t * b t -> unit = fun (phi,def) -> match (phi,def) with
            | Binary({term=Inductive_var x}),Binary _ -> x.definition <- def
            | Integer({term=Inductive_var x}),Integer _ -> x.definition <- def
            | Bool({term=Inductive_var x}),Bool _ -> x.definition <- def
            | _ -> Log.fatal (fun p -> p "Type mismatch in phi arguments");
            in f (phi,def)
            ) previous_tup next_tup;
        previous_tup
      end
      else
        (* Don't bother about the old variables, just create fresh ones. *)
        (* Note: we could have an induction step number in the name,
           to ensure that we don't confuse variables coming from
           different steps. *)
        Constraints.Build.Tuple.inductive_vars ~widening_id:(widening_id :> int) ~level:ctxa.level  ~def:next_tup
    in

    let domain,bool =
      Domain.widened_fixpoint_step
        ~previous:previous.domain ~previous_tup
        ~next:next.domain ~next_tup
        includes ~res_tup
    in
    Log.debug (fun p -> p "includes %b bool %b" includes bool);

    let ctx = { domain = domain; level = ctxa.level; unique_id = get_unique_id();
                path_condition = Constraints.Build.Boolean.(||) ctxa.path_condition ctxb.path_condition; } in

    (* Build the final map. *)
    let phi =
      Immutable_array.fold_left3 (fun map a b c ->
          match a,b,c with
          | Constraints.(Any(Binary _ as a), Any(Binary _ as b), Any(Binary _ as c)) -> MapPair.add a b c map
          | Constraints.(Any(Integer _ as a), Any(Integer _ as b), Any(Integer _ as c)) -> MapPair.add a b c map
          | Constraints.(Any(Bool _ as a), Any(Bool _ as b), Any(Bool _ as c)) -> MapPair.add a b c map
          | _ -> Log.fatal (fun p -> p "Wrong type assertion")) MapPair.empty previous_tup next_tup res_tup
    in
    ctx,bool,{phi}
  ;;

  (**************** Fixpoint computation. ****************)


  let typed_fixpoint_step ~iteration ~init ~arg:(arg:Context.t) ~body ((included, in_tup):'a in_acc) =

    (* Codex_log.debug "typed_fixpoint_step init:%a arg:%a body:%a" context_pretty init context_pretty arg context_pretty body ;  *)
    let init:context = init in
    let body:context = body in
    let cur_level = body.level in

    let init_dom =  init.domain in
    let final_dom =  body.domain in

    (* Codex_log.feedback "fixpoint step: arg_dom constraint = %a" Constraints.pretty (Domain.to_constraint arg_dom); *)
    (* Codex_log.feedback "fixpoint step: final_dom constraint = %a" Constraints.pretty (Domain.to_constraint final_dom);     *)
    (* Codex_log.feedback "argument level = %d,@ context level = %d" (Constraints.level @@ Domain.to_constraint arg_dom) cur_level; *)
    assert(Constraints.level @@ init.path_condition < cur_level);

    let fixpoint_reached = ref included in

    let actuals,old_args,finals,map,init_dom =
      let rec loop: type a. _ -> _ -> _ -> _ -> _ -> a mapping -> _ = fun actuals old_args finals map init_dom -> function
        | EmptyMapping -> actuals,old_args,finals,map,init_dom
        | ConsSame(x,mapping) ->
          (* Codex_log.debug "constraint x = %a" Constraints.pretty x ; *)
          (* Codex_log.debug "constraint level of x = %d" (Constraints.level x) ; *)
          (* Codex_log.debug "current level is %d" cur_level ;           *)
          assert(Constraints.level x < cur_level);
          loop actuals old_args finals map init_dom mapping

        (* Global value numbering. *)
        | ConsDifferent(input,final,mapping) when MapPair.mem input final map ->
          loop actuals old_args finals map init_dom mapping

        | ConsDifferent(input,final,mapping) ->
          assert(Constraints.level input <= cur_level);
          (* assert(Constraints.level final <= cur_level); *)
          if Constraints.level final > cur_level then
            raise (Failure "Trying to join two SSA constraints of the same level during widening") ; (* TODO : use the domain of array to try and solve this *)
          let init_dom,old_arg,actual =
            input |> fun (type a) (input:a Constraints.t) -> match input with

            (* If already a variable. *)
            | Constraints.Bool{term = Constraints.Mu_formal {level;actual=(actual_constrain,_)}}
              when level == cur_level ->
              init_dom,Constraints.Any input,Constraints.Any actual_constrain
            | Constraints.Integer{term = Constraints.Mu_formal {level;actual=(actual_constrain,_)}}
              when level == cur_level ->
              init_dom,Constraints.Any input,Constraints.Any actual_constrain
            | Constraints.Binary{term = Constraints.Mu_formal {level;actual=(actual_constrain,_)}}
              when level == cur_level ->
              init_dom,Constraints.Any input,Constraints.Any actual_constrain


            (* Normal introduction of a variable. *)
            | _ when Constraints.level input < cur_level -> begin
                fixpoint_reached := false;
                init_dom,Constraints.Any input, Constraints.Any input
              end
            (* In some rare cases (e.g. reads of different sizes may
               happen at the same memory location), the memory
               domain makes small modifications to an argument
               already introduced. We make some simple substitutions
               to support this. *)
            | _ when Constraints.level input == cur_level ->
              fixpoint_reached := false;
              (* Note that this is not just substitution, but also
                 an evaluation of the function in the domain. *)
              let rec subst: type a. a Constraints.t -> a identifier =
                let open Constraints in function
                  | Integer{term=T2{tag=TC.Imod;a;b=Integer{term=T0{tag=TC.Iconst k}}}} ->
                    Integer_Forward.imod init (subst a) (Integer_Forward.iconst k init)
                  | Integer{term=T2{tag=TC.Ishr;a;b=Integer{term=T0{tag=TC.Iconst k}}}} ->
                    Integer_Forward.ishr init (subst a) (Integer_Forward.iconst k init)
                  | Integer{term=Mu_formal{actual=(v,_)}} -> v
                  | Binary{term=T1{tag=TC.Bextract{size;index;oldsize};a}} ->
                    Binary_Forward.bextract init ~size ~index ~oldsize (subst a)
                  (* | Binary{term=T1{tag=TC.Bofbool(size);a}} -> Binary_Forward.bofbool ~size init (subst a) *)
                  | Binary{term=Unknown level} as v -> v
                  | Binary{term=Mu_formal{actual=(v,_)}} -> v

                  | Binary{term=T2{tag=TC.Biadd{size;nsw;nuw;nusw};a;b=Binary{term=T0{tag=TC.Biconst (size1,k)}}}} ->
                    Binary_Forward.biadd ~size ~nsw ~nuw ~nusw init (subst a) (Binary_Forward.biconst ~size:size1 k init)
                  | Binary{term=T2{tag=TC.Biadd{size;nsw;nuw;nusw};a=Binary{term=T0{tag=TC.Biconst (size1,k)}};b}} ->
                    Binary_Forward.biadd ~size ~nsw ~nuw ~nusw init (Binary_Forward.biconst ~size:size1 k init) (subst b)
                  | Binary{term=T2{tag=TC.Bisub{size;nsw;nuw;nusw};a;b=Binary{term=T0{tag=TC.Biconst (size1,k)}}}} ->
                    Binary_Forward.bisub ~size ~nsw ~nuw ~nusw init (subst a) (Binary_Forward.biconst ~size:size1 k init)
                  | Binary{term=T2{tag=TC.Bisub{size;nsw;nuw;nusw};a=Binary{term=T0{tag=TC.Biconst (size1,k)}};b}} ->
                    Binary_Forward.bisub ~size ~nsw ~nuw ~nusw init (Binary_Forward.biconst ~size:size1 k init) (subst b)
                  | Binary{term=T2{tag=TC.Bimul{size;nsw;nuw};a;b=Binary{term=T0{tag=TC.Biconst (size1,k)}}}} ->
                    Binary_Forward.bimul ~size ~nsw ~nuw init (subst a) (Binary_Forward.biconst ~size:size1 k init)
                  | Binary{term=T2{tag=TC.Bimul{size;nsw;nuw};a=Binary{term=T0{tag=TC.Biconst (size1,k)}};b}} ->
                    Binary_Forward.bimul ~size ~nsw ~nuw init (Binary_Forward.biconst ~size:size1 k init) (subst b)

                  | constr -> Codex_log.fatal "in typed_fixpoint_step, invalid constr = %a"
                                Constraints.pretty constr
              in
              begin match subst input with
                | substed ->
                   assert(Constraints.level @@ init.path_condition < cur_level);
                  let substed = Constraints.Any substed in

                  (* Save the result of evaluation in init_dom. *)
                  (* For now we do nothing but we would have if functions like bextract
                     return a new domain instead of updating it imperatively. *)
                  init_dom,(Constraints.Any input), substed
              end
            | _ ->
              assert(Constraints.level input > cur_level);
              assert false (* This case should not happen. *)
          in loop (actual::actuals) (old_arg::old_args)
            ((Constraints.Any final)::finals) (MapPair.add input final input map) init_dom mapping
      in loop [] [] [] MapPair.empty init_dom in_tup.mapping
    in

    (* This assertion is not always true, as we can call subst,
       which imperatively changes the context.  *)
    (* assert(init_dom == init.domain); *)
    let actuals = Immutable_array.of_list actuals in
    let old_args = Immutable_array.of_list old_args in
    let finals = Immutable_array.of_list finals in

    let cond_init = init.path_condition in
    (* Codex_log.feedback "Cond arg is %a" Constraints.pretty cond_arg; *)

    (* It is important that the domain corresponding to the argument
       is suitable for use outside of the mu, when we leave the mu. *)
    assert(Constraints.level cond_init < cur_level);

    let bool,domainf = Domain.fixpoint_step ~iteration ~lvl:cur_level init_dom ~actuals arg.domain ~args:old_args final_dom ~finals in
    let res = !fixpoint_reached && bool in

    let continuef ~close =
      let constraints =
        if close then begin
          let body_cond = body.path_condition in
          Constraints.Build.Tuple.mu
            ~level:(cur_level - 1)
            ~init:actuals ~var:old_args ~body:finals ~body_cond
          end
        else
          (* Restart *)
          let new_args = actuals |> Immutable_array.map (fun actual ->
              let intro constrain x =
                if option_fresh_variable_every_time
                || Constraints.level constrain != cur_level (* Is not already a variable. *)
                then
                  (* TODO: Remove the actual_cond argument, that is not needed in this implementation. *)
                  Constraints.Build.Mu_Formal.intro
                    ~level:cur_level ~actual:constrain ~actual_cond:Constraints.Build.Boolean.true_ x
                else constrain
              in
              match actual with
              | Constraints.(Any (Bool _ as c)) -> Constraints.Any (intro c TC.Boolean)
              | Constraints.(Any (Integer _ as c)) -> Constraints.Any (intro c TC.Integer)
              | Constraints.(Any (Binary {size} as c)) -> Constraints.Any (intro c @@ TC.Binary size))
          in
          new_args
      in
      let domain = domainf ~close constraints in
      (* assert(Constraints.level @@ Domain.to_constraint domain < cur_level); *)

      let phi = build_phi old_args finals constraints in
      let restup = {phi} in
      let resctx:Context.t = if close then init else arg in
      restup, {resctx with domain = domain }

    in
    res,continuef

  let mu_context_open parent_ctx =
    Domain.fixpoint_open();
    let ctx = { unique_id = get_unique_id();
                level = parent_ctx.level + 1;
                domain = parent_ctx.domain;
                path_condition = parent_ctx.path_condition;
              }
    in ctx


  (**************** Queries ****************)

  module Query = struct
    include Domain.Query
    let boolean (ctx:context) x =
        let domain = ctx.domain in
        Domain.Query.boolean domain x
    ;;

    (* This version is more precise as propagating constraints using assume may discover
       additional contradictions. *)
    let boolean ctx x =
      match boolean ctx x with
      | Lattices.Quadrivalent.(False | True | Bottom) as x -> x
      | Lattices.Quadrivalent.Top ->
        match assume ctx x, assume ctx (Boolean_Forward.not ctx x) with
        | Some _, Some _ -> Lattices.Quadrivalent.Top
        | None, None -> Lattices.Quadrivalent.Bottom
        | None, Some _ -> Lattices.Quadrivalent.False
        | Some _, None -> Lattices.Quadrivalent.True
    ;;

    let binary ~size (ctx:context) x =
        let domain = ctx.domain in
        Domain.Query.binary ~size domain x
    let integer (ctx:context) x =
        let domain = ctx.domain in
        Domain.Query.integer domain x

    let reachable _ = assert false
  end

  let assume_binary ~size = assert false

  let binary_is_empty = binary_is_empty
  let integer_is_empty = integer_is_empty
  let boolean_is_empty = boolean_is_empty


  module Satisfiable = struct

    module To_SMT_FirstOrder = Constraints_smt.MakeFirstOrder(Constraints);;
    module To_SMT_Horn = Constraints_smt.MakeHorn(Constraints);;

    let satisfiable (ctx:context) bool =
      let condition = ctx.path_condition in
      (* Codex_log.feedback "Satisfiable? condition = %a" Constraints.pretty condition; *)
        Smtbackend.Smtlib.with_z3
          (fun (module SMT:Smtbackend.Smtlib.Untyped_Muz) ->
             let translate = match option_translate with
               | `Horn -> let module Inst = To_SMT_Horn(SMT) in Inst.translate
               | `First_order -> let module Inst = To_SMT_FirstOrder(SMT) in Inst.translate
             in
             translate @@ Constraints.Build.Boolean.(&&) bool condition)
    ;;

  let boolean_unknown ctx =
    let res = boolean_unknown ctx in
    Log.debug (fun p -> p "Constraint_domain2.boolean_unknown %a" (boolean_pretty ctx) res);
    res

  module Binary_Forward = struct
    include Binary_Forward

    let bofbool ~size ctx b =
      let res = bofbool ~size ctx b in
      Log.debug (fun p -> p "Constraint_domain2.bofbool %a" (binary_pretty ~size ctx) res);
      res
  end

  end

  let satisfiable = Satisfiable.satisfiable
  (* MAYBE: take pred into account. *)
  let binary_unknown_typed ~size ctx typ = binary_unknown ~size ctx
  let query_boolean = Query.boolean
end
