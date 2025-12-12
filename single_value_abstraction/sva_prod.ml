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

(* TODO: produce of lattices should go in prod_lattice.  Here, we have
   only the produce of transfer functions.  *)

open Sva_sig;;

(* Default (non-reduced) product. We could apply a reduction function
   (e.g. propagate the bottoms) *)
module Prod_Bitvector_Forward
  (A:WITH_BITVECTOR_FORWARD with type boolean = Sva_quadrivalent.boolean)
  (B:WITH_BITVECTOR_FORWARD
   with type boolean = A.boolean)
  (* :With_Bitvector_Forward *) =
struct
  module Bitvector_Forward = struct

    module ABF = A.Bitvector_Forward
    module BBF = B.Bitvector_Forward

    let buext ~size ~oldsize (a,b) =
      (ABF.buext ~size ~oldsize a, BBF.buext ~size ~oldsize b)
    let bsext ~size ~oldsize (a,b) =
      (ABF.bsext ~size ~oldsize a, BBF.bsext ~size ~oldsize b)
    let bofbool ~size x =
      (ABF.bofbool ~size x, BBF.bofbool ~size x)
    let bextract ~size ~index ~oldsize (a,b) =
      (ABF.bextract ~size ~index ~oldsize a, BBF.bextract ~size ~index ~oldsize b)

    let bop2 fa fb = fun ~size (a1,b1) (a2,b2) ->
      (fa ~size a1 a2, fb ~size b1 b2)

    let bop2_flags fa fb = fun ~size ~flags (a1,b1) (a2,b2) ->
      (fa ~size ~flags a1 a2, fb ~size ~flags b1 b2)

    let biadd = bop2_flags ABF.biadd BBF.biadd
    let bisub = bop2_flags ABF.bisub BBF.bisub
    let bimul = bop2_flags ABF.bimul BBF.bimul
    let bimul_add ~size ~prod ~offset (l,r) = (ABF.bimul_add ~size ~prod ~offset l, BBF.bimul_add ~size ~prod ~offset r)
    let bisdiv = bop2 ABF.bisdiv BBF.bisdiv
    let bismod = bop2 ABF.bismod BBF.bismod
    let biudiv = bop2 ABF.biudiv BBF.biudiv
    let biumod = bop2 ABF.biumod BBF.biumod
    let band = bop2 ABF.band BBF.band
    let bor = bop2 ABF.bor BBF.bor
    let bxor = bop2 ABF.bxor BBF.bxor
    let bshl = bop2_flags ABF.bshl BBF.bshl
    let bashr = bop2 ABF.bashr BBF.bashr
    let blshr = bop2 ABF.blshr BBF.blshr

    let bconcat ~size1 ~size2 (a1,b1) (a2,b2) =
      ABF.bconcat ~size1 ~size2 a1 a2, BBF.bconcat ~size1 ~size2 b1 b2

    let bpred fa fb = fun (a1,b1) (a2,b2) ->
      Sva_quadrivalent.Boolean_Lattice.inter (fa a1 a2) (fb b1 b2)

    let beq   ~size = bpred (ABF.beq   ~size) (BBF.beq   ~size)
    let bisle ~size = bpred (ABF.bisle ~size) (BBF.bisle ~size)
    let biule ~size = bpred (ABF.biule ~size) (BBF.biule ~size)

    let biconst ~size k = (ABF.biconst ~size k, BBF.biconst ~size k)
  end
end

module Prod_Bitvector_Backward
  (A:WITH_BITVECTOR_BACKWARD with type boolean = Sva_quadrivalent.boolean)
  (B:WITH_BITVECTOR_BACKWARD
   with type boolean = A.boolean) =
struct

  module ABB = A.Bitvector_Backward;;
  module BBB = B.Bitvector_Backward;;

  let coalesce_nones olda oldb a b = match a,b with
    | None,None -> None
    | Some v, None -> Some(v, oldb)
    | None, Some v -> Some(olda, v)
    | Some va, Some vb -> Some(va,vb)


  let bpred2 op1 op2 (a1,a2) (b1,b2) res =
    let (newa1,newb1) = op1 a1 b1 res in
    let (newa2,newb2) = op2 a2 b2 res in
    (coalesce_nones a1 a2 newa1 newa2,
     coalesce_nones b1 b2 newb1 newb2)
  ;;

  let beq   ~size = bpred2 (ABB.beq   ~size) (BBB.beq   ~size)
  let biule ~size = bpred2 (ABB.biule ~size) (BBB.biule ~size)
  let bisle ~size = bpred2 (ABB.bisle ~size) (BBB.bisle ~size)

  let bop2 op1 op2 (a1,a2) (b1,b2) (res1,res2) =
    let (newa1,newb1) = op1 a1 b1 res1 in
    let (newa2,newb2) = op2 a2 b2 res2 in
    (coalesce_nones a1 a2 newa1 newa2,
     coalesce_nones b1 b2 newb1 newb2)

  let bop2_flags ~size ~flags op1 op2 =
    bop2 (op1 ~size ~flags) (op2 ~size ~flags)      
  let bop2 ~size op1 op2 = bop2 (op1 ~size) (op2 ~size)


  let biadd = bop2_flags ABB.biadd BBB.biadd
  let bisub = bop2_flags ABB.bisub BBB.bisub
  let bimul = bop2_flags ABB.bimul BBB.bimul
  let bimul_add ~size ~prod ~offset (l,r) (res_l, res_r) =
    coalesce_nones l r
      (ABB.bimul_add ~size ~prod ~offset l res_l)
      (BBB.bimul_add ~size ~prod ~offset r res_r)
  let band = bop2 ABB.band BBB.band
  let bor = bop2 ABB.bor BBB.bor
  let bxor = bop2 ABB.bxor BBB.bxor
  let bisdiv = bop2 ABB.bisdiv BBB.bisdiv
  let bismod = bop2 ABB.bismod BBB.bismod
  let biudiv = bop2 ABB.biudiv BBB.biudiv
  let biumod = bop2 ABB.biumod BBB.biumod
  let bashr = bop2 ABB.bashr BBB.bashr
  let blshr = bop2 ABB.blshr BBB.blshr
  let bshl = bop2_flags ABB.bshl BBB.bshl

  let bconcat ~size1 ~size2 (xa,xb) (ya,yb) (resa,resb) =
    let (newxa,newya) = ABB.bconcat ~size1 ~size2 xa ya resa in
    let (newxb,newyb) = BBB.bconcat ~size1 ~size2 xb yb resb in
    (coalesce_nones xa xb newxa newxb,
     coalesce_nones ya yb newya newyb)
  ;;

  let bop1 opa opb (xa,xb) (resa,resb) =
    let newxa = opa xa resa in
    let newxb = opb xb resb in
    coalesce_nones xa xb newxa newxb
  ;;

  let bsext ~size ~oldsize x res = bop1 (ABB.bsext ~size ~oldsize) (BBB.bsext ~size ~oldsize) x res
  let buext ~size ~oldsize x res = bop1 (ABB.buext ~size ~oldsize) (BBB.buext ~size ~oldsize) x res
  let bofbool ~size a res = assert false
  let bextract ~size ~index ~oldsize x res =
    bop1 (ABB.bextract ~size ~index ~oldsize) (BBB.bextract ~size ~index ~oldsize) x res
end

module Prod_Bitvector
  (A:BITVECTOR with type boolean = Sva_quadrivalent.boolean)
  (B:BITVECTOR with type boolean = A.boolean) =
struct
  let name = "Prod_Bitvector(" ^ A.name ^ "*" ^ B.name ^ ")";;

  module Boolean_Lattice = A.Boolean_Lattice
  module Bitvector_Lattice = struct
    type t = A.Bitvector_Lattice.t * B.Bitvector_Lattice.t
    module L  = Lattices.Unimplemented.Bitvector_Lattice(struct
        type nonrec t = t
        let loc = __LOC__
      end)
    include (L:module type of L with type t := t)

    let equal (a1,b1) (a2,b2) = A.Bitvector_Lattice.equal a1 a2 && B.Bitvector_Lattice.equal b1 b2
    let compare (a1,b1) (a2,b2) =
      let ra = A.Bitvector_Lattice.compare a1 a2 in
      if ra != 0 then ra
      else B.Bitvector_Lattice.compare b1 b2

    let hash (a,b) = Hashing.hash2 (A.Bitvector_Lattice.hash a) (B.Bitvector_Lattice.hash b)
    let pretty ~size fmt (a,b) =
    (* Used to test the product itself. *)
      (* if(not @@ A.Bitvector_Lattice.equal a (Obj.magic b)) then
       *   Codex_log.fatal "Error differnt %a %a"  (A.Bitvector_Lattice.pretty ~size) a (B.Bitvector_Lattice.pretty ~size) b;
       * Format.fprintf fmt "%a" (A.Bitvector_Lattice.pretty ~size) a *)
    Format.fprintf fmt "(%a,%a)" (A.Bitvector_Lattice.pretty ~size) a (B.Bitvector_Lattice.pretty ~size) b
    ;;

    let includes ~size (a1,a2) (b1,b2) = A.Bitvector_Lattice.includes ~size a1 b1 && B.Bitvector_Lattice.includes ~size a2 b2
    let widen ~size ~previous:(a1,a2) (b1,b2) = (A.Bitvector_Lattice.widen ~size ~previous:a1 b1),(B.Bitvector_Lattice.widen ~size ~previous:a2 b2)
    let includes_or_widen ~size ~previous next =
      if includes ~size previous next then (true,next)
      else (false,widen ~size ~previous next)
    ;;
    let join ~size (a1,a2) (b1,b2) = (A.Bitvector_Lattice.join ~size a1 b1, B.Bitvector_Lattice.join ~size a2 b2)
    let bottom ~size = A.Bitvector_Lattice.bottom ~size,B.Bitvector_Lattice.bottom ~size
    let top ~size = A.Bitvector_Lattice.top ~size,B.Bitvector_Lattice.top ~size
    let is_bottom ~size (a,b) = A.Bitvector_Lattice.is_bottom ~size a || B.Bitvector_Lattice.is_bottom ~size b
    let inter ~size (a1,a2) (b1,b2) = (A.Bitvector_Lattice.inter ~size a1 b1, B.Bitvector_Lattice.inter ~size a2 b2)
    let singleton ~size k = A.Bitvector_Lattice.singleton ~size k,
                            B.Bitvector_Lattice.singleton ~size k
    let is_singleton ~size (a,b) =
      match A.Bitvector_Lattice.is_singleton ~size a with
      | None -> B.Bitvector_Lattice.is_singleton ~size b
      | x -> x

    let is_empty ~size (a,b) =
      A.Bitvector_Lattice.is_empty ~size a || B.Bitvector_Lattice.is_empty ~size b

    let to_known_bits ~size (a,b) =
      Lattices.Known_Bits.inter ~size
        (A.Bitvector_Lattice.to_known_bits ~size a)
        (B.Bitvector_Lattice.to_known_bits ~size b)

  end
  module Boolean_Forward = A.Boolean_Forward
  module Boolean_Backward = A.Boolean_Backward
  type boolean = A.boolean
  type bitvector = Bitvector_Lattice.t

  module Bitvector_Forward = struct
    module AB = Prod_Bitvector_Forward(A)(B)
    include AB.Bitvector_Forward
  end
  module Bitvector_Backward = struct
    module AB = Prod_Bitvector_Backward(A)(B)
    include AB
  end

end
