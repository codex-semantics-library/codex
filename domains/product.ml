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

(* Note that we do not combine the information coming from both
   domains; there is no generic way to do it. Combination should be
   done in the caller of this functor.

   XXX: This is because we are not really making a product of domains,
   but merely calling two domains with the same input. Hmm, still
   we should be able to intersect the queries; weird.

*)


(* Make every operation twice. We need a pair of boolean identifiers,
   a pair of binary identifiers, etc. Only truth_value benefits from
   the interaction.

   TODO: call eval + inject on all boolean operations, then on all operations. *)


module Make(P1: Sig.BASE)(P2: Sig.BASE): Sig.BASE
  with type Context.t = P1.Context.t * P2.Context.t
  and type binary = P1.binary * P2.binary
  and type boolean = P1.boolean * P2.boolean
  and type Query.Binary_Lattice.t = P1.Query.Binary_Lattice.t * P2.Query.Binary_Lattice.t
=
struct

  let name() = (P1.name()) ^ "*" ^ (P2.name())
  let unique_id() = Sig.Fresh_id.fresh @@ name();;

  module Types = struct
    type boolean = P1.boolean * P2.boolean
    type binary = P1.binary * P2.binary
    type enum = P1.enum * P2.enum
  end
  include Types


  (**************** Context ****************)
  module Context = struct
    type t = P1.Context.t * P2.Context.t
    let copy (x,y) = P1.Context.copy x, P2.Context.copy y

    let assign (ctx1,ctx2) (newctx1,newctx2) =
      P1.Context.assign ctx1 newctx1; P2.Context.assign ctx2 newctx2

    let level (x1,x2) =
      let res = P1.Context.level x1 in
      assert(res == P2.Context.level x2);
      res

    type 'a in_tuple = In: 'a P1.Context.in_tuple * 'b P2.Context.in_tuple -> ('a * 'b) in_tuple
    type 'a in_acc = bool * 'a in_tuple
    type 'b out_tuple = Out: 'a P1.Context.out_tuple * 'b P2.Context.out_tuple -> ('a * 'b) out_tuple
    type empty_tuple = (P1.Context.empty_tuple * P2.Context.empty_tuple)
    let empty_tuple () = In(P1.Context.empty_tuple (),P2.Context.empty_tuple ())
    type ('a,'b) result =
        Result: bool * 'some in_tuple * (t -> 'some out_tuple -> 'a * 'b out_tuple) -> ('a,'b) result
  end
  open Context

  let root_context () = (P1.root_context(), P2.root_context())
  let context_pretty fmt (ctx1,ctx2) = Format.fprintf fmt "{P1=%a|P2=%a}" P1.context_pretty ctx1 P2.context_pretty ctx2

  let mu_context_open (ctx1,ctx2) = (P1.mu_context_open ctx1, P2.mu_context_open ctx2)

  let assume (ctx1,ctx2) (cond1,cond2) =
    match P1.assume ctx1 cond1, P2.assume ctx2 cond2 with
    | None, _ | _, None -> None
    | Some ctx1, Some ctx2 -> Some (ctx1,ctx2)

  (**************** Serialisation ****************)


  type 'elt higher1 = {subf1: 'tl. P1.Context.t -> 'elt -> P1.Context.t -> 'elt -> 'tl P1.Context.in_acc -> ('elt,'tl) P1.Context.result  } [@@unboxed]
  type 'elt higher2 = {subf2: 'tl. P2.Context.t -> 'elt -> P2.Context.t -> 'elt -> 'tl P2.Context.in_acc -> ('elt,'tl) P2.Context.result  } [@@unboxed]

  let serialize (type c) {subf1} {subf2} (ctxa1,ctxa2) (a1,a2) (ctxb1,ctxb2) (b1,b2) (acc:c Context.in_acc): (_,c) Context.result =
    let (included, In (acc1,acc2)) = acc in
    let P1.Context.Result(inc1,acc1,d1) = subf1 ctxa1 a1 ctxb1 b1 (included,acc1) in
    let P2.Context.Result(inc2,acc2,d2) = subf2 ctxa2 a2 ctxb2 b2 (included,acc2) in
    Result(inc1 && inc2, In(acc1,acc2), fun (ctx1,ctx2) (Out(tup1,tup2)) ->
      let r1,tup1 = d1 ctx1 tup1 in
      let r2,tup2 = d2 ctx2 tup2 in
      let tup = Context.Out(tup1,tup2) in
      (r1,r2),tup)
  ;;

  let serialize_boolean ctxa a ctxb b acc =
    serialize {subf1=P1.serialize_boolean} {subf2=P2.serialize_boolean} ctxa a ctxb b acc

  let serialize_binary ~widens ~size ctxa a ctxb b acc =
    serialize
      {subf1=fun ctxa a ctxb b acc -> P1.serialize_binary ~widens ~size ctxa a ctxb b acc}
      {subf2=fun ctxa a ctxb b acc -> P2.serialize_binary ~widens ~size ctxa a ctxb b acc}
      ctxa a ctxb b acc

  let serialize_enum ctxa a ctxb b acc =
    serialize {subf1=P1.serialize_enum} {subf2=P2.serialize_enum} ctxa a ctxb b acc

  let typed_nondet2 (type c) (ctxa1,ctxa2) (ctxb1,ctxb2) (In(tup1,tup2):c in_tuple): Context.t * c out_tuple =
    let ctx1,res1 = P1.typed_nondet2 ctxa1 ctxb1 tup1 in
    let ctx2,res2 = P2.typed_nondet2 ctxa2 ctxb2 tup2 in
    (ctx1,ctx2), Out(res1,res2)
  ;;

  let nondet_same_context (type c) (ctx1,ctx2)  (In(tup1,tup2):c in_tuple): c out_tuple =
    let res1 = P1.nondet_same_context ctx1 tup1 in
    let res2 = P2.nondet_same_context ctx2 tup2 in
    Out(res1,res2)
  ;;



  let typed_fixpoint_step (type c) ~iteration ~init:(init1,init2) ~arg:(arg1,arg2) ~body:(body1,body2) (included,In(tup1,tup2):c in_acc):
    bool * (close:bool -> c out_tuple * Context.t) =
    let bool1,k1 = P1.typed_fixpoint_step ~iteration ~init:init1 ~arg:arg1 ~body:body1 (included, tup1) in
    let bool2,k2 = P2.typed_fixpoint_step ~iteration ~init:init2 ~arg:arg2 ~body:body2 (bool1, tup2) in
    bool2, (fun ~close ->
        let tup1,ctx1 = k1 ~close in
        let tup2,ctx2 = k2 ~close in
        Out(tup1,tup2),(ctx1,ctx2)
      )
  ;;

  let widened_fixpoint_step
      (type c) ~widening_id ~previous:(prev1,prev2) ~next:(next1,next2)  (included,In(tup1,tup2):c in_acc):
    Context.t * bool * c out_tuple =
    let ctx1,bool1,out1 = P1.widened_fixpoint_step ~widening_id ~previous:prev1 ~next:next1 (included, tup1) in
    let ctx2,bool2,out2 = P2.widened_fixpoint_step ~widening_id ~previous:prev2 ~next:next2 ((included && bool1), tup2) in
    (ctx1,ctx2),bool2,Out(out1,out2)
  ;;


  module Binary = Datatype_sig.Prod2(P1.Binary)(P2.Binary)
  module Boolean = Datatype_sig.Prod2(P1.Boolean)(P2.Boolean)
  module Enum = Datatype_sig.Prod2(P1.Enum)(P2.Enum)

  (**************** Transfer functions ****************)

  (* TODO: We could probably factor using arity. *)
  module Boolean_Forward = struct
    let not (ctx1,ctx2) (v1,v2) = (P1.Boolean_Forward.not ctx1 v1,
                                   P2.Boolean_Forward.not ctx2 v2)
    let (&&) (ctx1,ctx2) (a1,a2) (b1,b2) = (P1.Boolean_Forward.(&&) ctx1 a1 b1,
                                            P2.Boolean_Forward.(&&) ctx2 a2 b2);;
    let (||) (ctx1,ctx2) (a1,a2) (b1,b2) = (P1.Boolean_Forward.(||) ctx1 a1 b1,
                                            P2.Boolean_Forward.(||) ctx2 a2 b2);;
    let true_ (ctx1,ctx2) = P1.Boolean_Forward.true_ ctx1,
                            P2.Boolean_Forward.true_ ctx2;;
    let false_ (ctx1,ctx2) = P1.Boolean_Forward.false_ ctx1,
                             P2.Boolean_Forward.false_ ctx2;;
  end

  module Binary_Forward = struct
    module BF1 = P1.Binary_Forward
    module BF2 = P2.Binary_Forward

    let buninit ~size (ctx1,ctx2) = BF1.buninit ~size ctx1, BF2.buninit ~size ctx2;;
    let biconst ~size k (ctx1,ctx2) =
      BF1.biconst ~size k ctx1, BF2.biconst ~size k ctx2;;

    let bop1 ~size op1 op2 (ctx1,ctx2) (v1,v2) =
      op1 ~size ctx1 v1, op2 ~size ctx2 v2
    ;;
    let valid ~size acc_typ = bop1 (BF1.valid acc_typ) (BF2.valid acc_typ) ~size
    let bsext ~size ~oldsize = bop1 (BF1.bsext ~oldsize) (BF2.bsext ~oldsize) ~size
    let buext ~size ~oldsize = bop1 (BF1.buext ~oldsize) (BF2.buext ~oldsize) ~size
    let bofbool ~size = bop1 (BF1.bofbool) (BF2.bofbool) ~size
    let bchoose ~size cond = bop1 (BF1.bchoose cond) (BF2.bchoose cond) ~size

    let bextract ~size ~index ~oldsize (ctx1,ctx2) (v1,v2) =
      BF1.bextract ~size ~index ~oldsize ctx1 v1, BF2.bextract ~size ~index ~oldsize ctx2 v2;;

    let bop2_no_size op1 op2 (ctx1,ctx2) (v1,v2) (w1,w2) =
      op1 ctx1 v1 w1, op2 ctx2 v2 w2

    let bpred = bop2_no_size

    (* Note: We cannot cross the results with inject_boolean in
       general: if domains abstracts unrelated pieces of information,
       the fact that one is true does not imply that the other
       is. E.g.  if one domain abstracts the set of memory regions to
       which a pointer may points, and the other the possible offsets
       of the pointers, we cannot deduce from a[0] != b[0] that 0 != 0.

       So, crossing informations like this should take place in the
       caller of this functor. *)

    let bisle ~size = bpred (BF1.bisle ~size) (BF2.bisle ~size);;
    let biule ~size = bpred (BF1.biule ~size) (BF2.biule ~size);;
    let beq   ~size = bpred (BF1.beq   ~size) (BF2.beq ~size);;


    let bop2_flags ~size ~flags bop1 bop2 = bop2_no_size (bop1 ~size ~flags) (bop2 ~size ~flags)
    let bop2_all_flags ~size ~nsw ~nuw ~nusw bop1 bop2 = bop2_no_size (bop1 ~size ~nsw ~nuw ~nusw) (bop2 ~size ~nsw ~nuw ~nusw)
    let bop2 ~size bop1 bop2 = bop2_no_size (bop1 ~size) (bop2 ~size)

    let valid_ptr_arith ~size arith_typ (ctx1,ctx2) (v1,v2) (w1,w2) =
      (BF1.valid_ptr_arith ~size arith_typ ctx1 v1 w1), (BF2.valid_ptr_arith ~size arith_typ ctx2 v2 w2)

    let biadd = bop2_flags BF1.biadd BF2.biadd
    let bisub = bop2_flags BF1.bisub BF2.bisub
    let bimul = bop2_flags BF1.bimul BF2.bimul
    let bshl =  bop2_flags BF1.bshl BF2.bshl

    let blshr = bop2 BF1.blshr BF2.blshr
    let bashr = bop2 BF1.bashr BF2.bashr

    let biumod = bop2 BF1.biumod BF2.biumod
    let biudiv = bop2 BF1.biudiv BF2.biudiv
    let bismod = bop2 BF1.bismod BF2.bismod
    let bisdiv = bop2 BF1.bisdiv BF2.bisdiv

    let bxor = bop2 BF1.bxor BF2.bxor
    let bor = bop2 BF1.bor BF2.bor
    let band = bop2 BF1.band BF2.band

    let bconcat ~size1 ~size2 = bop2_no_size (BF1.bconcat ~size1 ~size2) (BF2.bconcat ~size1 ~size2)

    let bshift ~size ~offset ~max _ = assert false
    let bindex ~size _ = assert false


  end

  module Enum_Forward = struct
    let caseof ~case (ctx1,ctx2) (v1,v2) =
      P1.Enum_Forward.caseof ~case ctx1 v1, P2.Enum_Forward.caseof ~case ctx2 v2
    let enum_const ~case (ctx1,ctx2) = P1.Enum_Forward.enum_const ~case ctx1,
                                       P2.Enum_Forward.enum_const ~case ctx2
  end

  (**************** Tuple ****************)

  (* include Operator.Builtin.Make(Types)(Context) *)

  (**************** Requests ****************)
  let boolean_pretty (ctx1,ctx2) fmt (mem1,mem2) =
    Format.fprintf fmt "(%a,%a)" (P1.boolean_pretty ctx1) mem1 (P2.boolean_pretty ctx2) mem2;;
  let boolean_is_empty (mem1,mem2) = assert false
  let binary_pretty ~size (ctx1,ctx2) fmt (mem1,mem2) =
    Format.fprintf fmt "(%a,%a)"
      (P1.binary_pretty ~size ctx1) mem1
      (P2.binary_pretty ~size ctx2) mem2;;
  let binary_is_empty ~size (mem1,mem2) = assert false
  let enum_pretty (ctx1,ctx2) fmt (mem1,mem2) =
    Format.fprintf fmt "(%a,%a)" (P1.enum_pretty ctx1) mem1 (P2.enum_pretty ctx2) mem2;;

  module Query = struct
    module Boolean_Lattice = struct
      include Lattices.Quadrivalent
    end
    (* module Binary = Lattices.Prod.Prod2_With_Inter_Bottom(P1.Query.Binary)(P2.Query.Binary) *)
    module Binary_Lattice = struct
      module A = P1.Query
      module B = P2.Query

      include Lattices.Unimplemented.Bitvector_Lattice(struct
          type nonrec t = A.Binary_Lattice.t * B.Binary_Lattice.t
          let loc = __LOC__
        end)

      let equal (a1,b1) (a2,b2) = A.Binary_Lattice.equal a1 a2 && B.Binary_Lattice.equal b1 b2
      let compare (a1,b1) (a2,b2) =
        let ra = A.Binary_Lattice.compare a1 a2 in
        if ra != 0 then ra
        else B.Binary_Lattice.compare b1 b2
      let hash (a,b) = Hashing.hash2 (A.Binary_Lattice.hash a) (B.Binary_Lattice.hash b)
      let pretty ~size fmt (a,b) = Format.fprintf fmt "(%a,%a)" (A.Binary_Lattice.pretty ~size) a (B.Binary_Lattice.pretty ~size) b

      let top ~size = P1.Query.Binary_Lattice.top ~size, P2.Query.Binary_Lattice.top ~size

      let inter ~size (a1,a2) (b1,b2) =
        P1.Query.Binary_Lattice.inter ~size a1 b1, P2.Query.Binary_Lattice.inter ~size a2 b2
      let is_bottom ~size _ = assert false
      let includes ~size _ = assert false
      let join ~size _ = assert false
      let bottom ~size = P1.Query.Binary_Lattice.bottom ~size, P2.Query.Binary_Lattice.bottom ~size
      let widen ~size ~previous _ = assert false
      let includes_or_widen ~size ~previous _ = assert false
      let singleton ~size k = P1.Query.Binary_Lattice.singleton ~size k,
                              P2.Query.Binary_Lattice.singleton ~size k
      let is_empty ~size (x1,x2) =
        A.Binary_Lattice.is_empty ~size x1
        || B.Binary_Lattice.is_empty ~size x2

    end


    (* To be filled by the caller of this functor. *)
    let convert_to_quadrivalent _ = assert false
    let binary_to_ival ~signed ~size _ = assert false

    let binary_fold_crop ~size bin ~inf ~sup f acc = assert false

    let binary ~size (ctx1,ctx2) (id1,id2) =
      (P1.Query.binary ~size ctx1 id1,
       P2.Query.binary ~size ctx2 id2)

    let binary_is_singleton ~size (a,b) =
      match P1.Query.Binary_Lattice.is_singleton ~size a with
      | None -> P2.Query.Binary_Lattice.is_singleton ~size b
      | x -> x
    ;;

    module Enum_Lattice = Lattices.Unimplemented.Enum_Lattice(struct let loc = __LOC__ end)

    let enum _ = assert false
    let is_singleton_enum _ = assert false
    let enum_to_values _ = assert false

  end

  let satisfiable (ctx1,ctx2) (b1,b2) =
    match P1.satisfiable ctx1 b1 with
    | (Smtbackend.Smtlib_sig.Unsat | Smtbackend.Smtlib_sig.Sat _ ) as r -> r
    | _ -> P2.satisfiable ctx2 b2
  ;;

  let binary_empty ~size (ctx1,ctx2) = P1.binary_empty ctx1 ~size, P2.binary_empty ctx2 ~size
  let boolean_empty (ctx1,ctx2) = P1.boolean_empty ctx1, P2.boolean_empty ctx2
  let enum_empty (ctx1,ctx2) = P1.enum_empty ctx1, P2.enum_empty ctx2

  let binary_unknown ~size (ctx1,ctx2) = P1.binary_unknown ctx1 ~size, P2.binary_unknown ctx2 ~size
  let boolean_unknown (ctx1,ctx2) = P1.boolean_unknown ctx1, P2.boolean_unknown ctx2
  let enum_unknown ~enumsize (ctx1,ctx2) = P1.enum_unknown ~enumsize ctx1, P2.enum_unknown ~enumsize ctx2

  let union _ = assert false
  let binary_unknown_typed ~size:_ _ = assert false
  let query_boolean (ctx1,ctx2) (id1,id2) =
    let l1 = (P1.query_boolean ctx1 id1) in
    let l2 = (P2.query_boolean ctx2 id2) in
    Lattices.Quadrivalent.inter l1 l2

end
