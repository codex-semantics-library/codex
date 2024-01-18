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
  :Domain_sig.Base = struct

  let name = "Constraint_Domain2(" ^ Domain.name ^ ")";;
  let unique_id = Domain_sig.Fresh_id.fresh name;;
  
  type 'a identifier = 'a Constraints.t

  module Types = struct
    type binary = TC.binary identifier
    type memory = unit identifier
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

    let hash _ = assert false
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
      mutable domain:Domain.t;
    }
  ;;

  module Context = struct
    type t = context
    let level ctx = ctx.level
    
    type 'a mapping =
      | EmptyMapping: unit mapping
      | ConsSame: 'a Constraints.t * 'b mapping -> ('a identifier * 'b) mapping
      | ConsDifferent: 'a Constraints.t * 'a Constraints.t * 'b mapping -> ('a identifier * 'b) mapping
    ;;

    type 'a in_tuple = { mapping: 'a mapping; } [@@unboxed]
    type 'a in_acc = bool * 'a in_tuple

    (* We reconstruct the identifiers on-demand. MAYBE: have the bottom case. *)
    type 'a out_tuple = {
      index: int;
      odomain: Domain.t;
      mapping: 'a mapping;
      constraints: Constraints.any Immutable_array.t
    }

    type ('a,'b) result =
        Result: bool * 'some in_tuple * (t -> 'some out_tuple -> 'a * 'b out_tuple) -> ('a,'b) result

    type empty_tuple = unit
    let empty_tuple = { mapping = EmptyMapping; }

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
  
  include Transfer_functions.Builtin.Make(Types)(Context)

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
                 level = ctx.level;  }
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
                       level = ctx.level;  }
            end
        end
  ;;    



  
  let imperative_assume ctx cond =
    match assume ctx cond with
    | None -> assert false       (* Should not happen, the condition
                                   should only limit fresh values, not
                                    create a bottom. *)
    | Some newctx -> begin
        ctx.domain <- newctx.domain;
        ctx.path_condition <- Constraints.Build.Boolean.(&&) cond ctx.path_condition
      end
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
    
    let bisle ~size ctx a b = 
      let diff = bisub ~size ~nsw:false ~nuw:false ~nusw:false ctx b a in
      let zero = biconst ~size Z.zero ctx in
      let leq_zero = bisle ~size ctx diff zero in
      let eq_zero = beq ~size ctx diff zero in
      let geq_zero = Boolean_Forward.((||) ctx (not ctx leq_zero) eq_zero) in
      match Domain.Query.convert_to_quadrivalent @@ Domain.Query.boolean ctx.domain geq_zero with
      | Lattices.Quadrivalent.(True | False | Bottom) -> geq_zero
      | Lattices.Quadrivalent.Top -> bisle ~size ctx a b

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
                                    a=(Binary{term=T0{tag=TC.Biconst(_size3,l)}} as mask);
                                    b=(Binary{term=T2{tag=TC.Biadd _size2;
                                          a=(Binary{term=T2{tag=TC.Biadd _size6;
                                                a=Binary _ as g;
                                                b=Binary _ as h
                                            }});
                                          b=(Binary{term=T0{tag=TC.Biconst(_size7,k)}} as ofs)
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
            Codex_log.debug "l : %a" Z.pp_print l ;
            let lsb = Z.trailing_zeros l in (* position of the least significant bit *)
            Codex_log.debug "lsb : %d" lsb ;
            let m = Z.shift_right l lsb in
            Codex_log.debug "m : %a" Z.pp_print m ;
            let msucc = Z.succ m in 
            let msb = Z.log2 msucc in
            Codex_log.debug "msb : %d" msb ;
            let c1 = Z.equal msucc (Z.shift_left Z.one msb) in  (* m = 2^{n}-1 *)
            Codex_log.debug "is_mask : %b" c1 ;
            let k = Z.signed_extract k lsb msb in
            Codex_log.debug "offset : %a" Z.pp_print k ;
            let c2 = Z.leq (Z.neg m) k && Z.leq k m in          (* k in [-m;m] *)
            Codex_log.debug "is offset in [-mask;mask] : %b" c2 ;
            let zero = biconst ~size:oldsize Z.zero ctx in 
            let c3 = beq ~size:oldsize ctx (band ~size:oldsize ctx ptr mask) zero in
            Codex_log.debug "c3 : %a" (Domain.boolean_pretty ctx.domain) c3 ;
            let neg_mask = biconst ~size:oldsize (Z.lognot l) ctx in  
            Codex_log.debug "neg_mask : %a" (Domain.binary_pretty ~size:oldsize ctx.domain) neg_mask ;
            let c4 = beq ~size:oldsize ctx (band ~size:oldsize ctx tag neg_mask) zero in
            Codex_log.debug "c4 : %a" (Domain.boolean_pretty ctx.domain) c4 ;
            let c5 =
              match Domain.Query.(convert_to_quadrivalent @@ boolean ctx.domain @@ Boolean_Forward.(&&) ctx c3 c4) with
              | Lattices.Quadrivalent.True -> true
              | _ -> false
            in
            Codex_log.debug "c5 : %b" c5 ;
            let c6 = index = 0 && msb <= size in                (* *)
            Codex_log.debug "c6 : %b" c6 ;
            Codex_log.debug "c1 /\ c2 /\ c5 /\ c6 : %b" (c1 && c2 && c5 && c6) ;  
            if c1 && c2 && c5 && c6 then
              beq ~size:oldsize ctx (biadd ~size ~nsw ~nuw ~nusw ctx tag ofs) zero    (* (tag + offset) = 0 *)

            else assert false
          
      | _ -> beq ~size ctx a b

    let beq ~size ctx a b =
      Codex_log.debug "Constraint_domain2.beq %a %a" Constraints.pretty a Constraints.pretty b ;
      let res = beq ~size ctx a b in
      Codex_log.debug "Constraint_domain2.beq returning %a" Constraints.pretty res ;
      res

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

  let memory_pretty ctx fmt = assert false

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
  let serialize (type a)
      (type_check:Constraints.any -> a Constraints.t)
      (_ctxa:Context.t) (a:a identifier) (_ctxb:Context.t) (b:a identifier) ((included, acc):'b in_acc) =
      let mapping =
        if Constraints.equal a b
        then ConsSame(a,acc.mapping)
        else ConsDifferent(a,b,acc.mapping)
      in
      let deserialize ctx {index;odomain=domain;mapping;constraints} =
        let constrain,mapping,index =
          match mapping with
          | ConsSame(x,mapping) ->
            (* let _ = type_check @@ Immutable_array.get constraints index  in *)
            x,mapping,index
          | ConsDifferent(_,_,mapping) ->
            let index = index - 1 in
            (* Cannot avoid this type check due to the use of an array. *)
            let constrain = type_check @@ Immutable_array.get constraints index in
            constrain,mapping,index
        in
        constrain,{index;odomain=domain;mapping;constraints}
      in
      Result(included, { mapping}, deserialize)
  ;;
  
  (* let counter = ref 0 ;;

  let serialize_binary ~size (ctxa : Context.t) (a : binary) (ctxb : Context.t) (b : binary) ((inc, tup) as acc : 'a in_acc) : (binary, 'a) result =
    incr counter ;
    let num = !counter in
    Codex_log.debug "Constraint_domain2.serialize no.%d ctxa:%a of level %d ctxb:%a of level %d" num context_pretty ctxa ctxa.level context_pretty ctxb ctxb.level ;
    Codex_log.debug "Constraint_domain2.serialize size %d %a %a with constraints of level %d and %d" size Identifier.pretty a Identifier.pretty b (Constraints.level a) (Constraints.level b);
    let c1 = Query.binary_is_empty ~size @@ Query.binary ~size ctxa a in
    let c2 = Query.binary_is_empty ~size @@ Query.binary ~size ctxb b in
    let levela = Constraints.level a in
    let levelb = Constraints.level b in
    if c1 && levelb < ctxa.level && levelb < ctxb.level then (Codex_log.debug "joining without join(1)" ; Result(inc, tup, fun ctx out -> b, out) )
    else
      if c2 && levela < ctxa.level && levela < ctxb.level then (Codex_log.debug "joining without join(1)" ; Result(inc, tup, fun ctx out -> a, out))
      else
        serialize (function (Constraints.(Any(Binary {size=s} as x))) when s == size ->
          Codex_log.debug "Constraint_domain2.serialize no.%d" num ;
          (* Codex_log.debug "Constraint_domain2.returning %a" Identifier.pretty x ; x | _ -> assert false) *)
          Codex_log.debug "Constraint_domain2.returning %a" Identifier.pretty x ; x
          | (Constraints.(Any(Binary {size=s} as x))) -> Codex_log.debug "size %d and s %d should be equal" size s ; assert false
          | _ -> Codex_log.debug "a and b should be binary values" ;  assert false) 
          ctxa a ctxb b acc
  *)

  let serialize_memory _ = assert false

  let serialize_binary ~size ctxa a ctxb b acc =
    serialize (function (Constraints.(Any(Binary {size=s} as x))) when s == size -> x | _ -> assert false)
      ctxa a ctxb b acc
  let serialize_integer ctxa a ctxb b acc =
    serialize (function (Constraints.(Any(Integer _ as x))) -> x | _ -> assert false)
      ctxa a ctxb b acc
  let serialize_boolean ctxa a ctxb b acc =
    serialize (function (Constraints.(Any(Bool _ as x))) -> x | _ -> assert false)
      ctxa a ctxb b acc

  (**************** Nondet and union. ****************)

  let nondet_same_context (ctx:context) (in_tup: _ in_tuple) =

    let filtereda,filteredb,total =
      let rec loop: type a. _ -> _ -> _ -> a mapping -> _ = fun la lb count -> function
        | EmptyMapping -> la,lb,count
        | ConsSame(_,mapping) -> loop la lb count mapping
        | ConsDifferent(a,b,mapping) -> loop ((Constraints.Any a)::la) ((Constraints.Any b)::lb) (count + 1) mapping
      in loop [] [] 0 in_tup.mapping
    in

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

    { mapping = in_tup.mapping; odomain=domain; index = total; constraints = tupres}
  ;;

  let union cond (ctx:context) (in_tup: _ in_tuple) =

    let filtereda,filteredb,total =
      let rec loop: type a. _ -> _ -> _ -> a mapping -> _ = fun la lb count -> function
        | EmptyMapping -> la,lb,count
        | ConsSame(_,mapping) -> loop la lb count mapping
        | ConsDifferent(a,b,mapping) -> loop ((Constraints.Any a)::la) ((Constraints.Any b)::lb) (count + 1) mapping
      in loop [] [] 0 in_tup.mapping
    in

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

    { mapping = in_tup.mapping; odomain=domain; index = total; constraints = tupres}
  ;;

  let typed_nondet2 (ctxa:context) (ctxb:context) (in_tup: _ in_tuple) =

    let filtereda,filteredb,total =
      let rec loop: type a. _ -> _ -> _ -> a mapping -> _ = fun la lb count -> function
        | EmptyMapping -> la,lb,count
        | ConsSame(_,mapping) -> loop la lb count mapping
        | ConsDifferent(a,b,mapping) -> loop ((Constraints.Any a)::la) ((Constraints.Any b)::lb) (count + 1) mapping
      in loop [] [] 0 in_tup.mapping
    in

    let tupa = Immutable_array.of_list filtereda in
    let tupb = Immutable_array.of_list filteredb in      

    let doma = ctxa.domain in
    let domb = ctxb.domain in

    let conda_bool = ctxa.path_condition in
    let condb_bool = ctxb.path_condition in

    assert(ctxa.level = ctxb.level);
    
    let tupres =
      Constraints.Build.Tuple.nondet ~level:ctxa.level
        ~conda_bool ~a:tupa
        ~condb_bool ~b:tupb
    in
    let domain =  Domain.nondet ~doma ~tupa ~domb ~tupb ~tupres in

    let ctx = { domain = domain; level = ctxa.level; unique_id = get_unique_id();
                path_condition = Constraints.Build.Boolean.(||) ctxa.path_condition ctxb.path_condition } in

    ctx, { mapping = in_tup.mapping; odomain=domain; index = total; constraints = tupres}
  ;;


  
  (**************** Fixpoint computation. ****************)
  

  let typed_fixpoint_step ~init ~arg:(arg:Context.t) ~body ((included, in_tup):'a in_acc) =

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
    
    let actuals,old_args,finals,total,init_dom =
      let rec loop: type a. _ -> _ -> _ -> _ -> _ -> a mapping -> _ = fun actuals old_args finals count init_dom -> function
        | EmptyMapping -> actuals,old_args,finals,count,init_dom
        | ConsSame(x,mapping) ->
          (* Codex_log.debug "constraint x = %a" Constraints.pretty x ; *)
          (* Codex_log.debug "constraint level of x = %d" (Constraints.level x) ; *)
          (* Codex_log.debug "current level is %d" cur_level ;           *)
          assert(Constraints.level x < cur_level);
          loop actuals old_args finals count init_dom mapping
        | ConsDifferent(input,final,mapping) ->
          assert(Constraints.level input <= cur_level);
          assert(Constraints.level final <= cur_level);            
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
                  | Binary{term=Mu_formal{actual=(v,_)}} -> v
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
                  init_dom,substed, substed
              end
            | _ ->
              assert(Constraints.level input > cur_level);
              assert false (* This case should not happen. *)
          in loop (actual::actuals) (old_arg::old_args) 
            ((Constraints.Any final)::finals) (count + 1) init_dom mapping
      in loop [] [] [] 0 init_dom in_tup.mapping
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

    let bool,domainf = Domain.fixpoint_step ~lvl:cur_level init_dom ~actuals arg.domain ~args:old_args final_dom ~finals in
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
      let restup = { mapping = in_tup.mapping; odomain=domain; index = total; constraints} in
      let resctx:Context.t = if close then init else arg in
      restup, {resctx with domain = domain}

    in
    res,continuef

  let mu_context_open parent_ctx =
    Domain.fixpoint_open();
    let ctx = { unique_id = get_unique_id();
                level = parent_ctx.level + 1;
                domain = parent_ctx.domain;
                path_condition = parent_ctx.path_condition
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

  let memory_is_empty _ = assert false
  let binary_is_empty ~size ctx (b:binary) = Constraints.equal b (Constraints.Build.Binary.empty ~size)
  let integer_is_empty _ = assert false
  let boolean_is_empty _ = assert false

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

    
  end

  let reachable _ _ = assert false
  let satisfiable = Satisfiable.satisfiable

  let should_focus ~size:_ _ = assert false
  let may_alias ~ptr_size:_ _ = assert false
  (* MAYBE: take pred into account. *)
  let binary_unknown_typed ~size ctx typ = binary_unknown ~size ctx
  let query_boolean = Query.boolean
end
