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

(* TODO: produce of lattices should go in prod_lattice.  Here, we have
   only the produce of transfer functions.  *)

open Basis_sig;;

(* Default (non-reduced) product. We could apply a reduction function
   (e.g. propagate the bottoms) *)
module Prod_Binary_Forward
  (A:With_Binary_Forward with type boolean = Quadrivalent_basis.boolean)
  (B:With_Binary_Forward
   with type boolean = A.boolean)
  (* :With_Binary_Forward *) =
struct
  type boolean = A.boolean
  type binary = A.binary * B.binary
  module Binary_Forward = struct

    module ABF = A.Binary_Forward
    module BBF = B.Binary_Forward

    let buext ~size ~oldsize (a,b) =
      (ABF.buext ~size ~oldsize a, BBF.buext ~size ~oldsize b)
    let bsext ~size ~oldsize (a,b) =
      (ABF.bsext ~size ~oldsize a, BBF.bsext ~size ~oldsize b)
    let bofbool ~size x =
      (ABF.bofbool ~size x, BBF.bofbool ~size x)
    let bchoose ~size cond (a,b) =
      (ABF.bchoose ~size cond a, BBF.bchoose ~size cond b)
    let bextract ~size ~index ~oldsize (a,b) =
      (ABF.bextract ~size ~index ~oldsize a, BBF.bextract ~size ~index ~oldsize b)

    let bop2 fa fb = fun ~size (a1,b1) (a2,b2) ->
      (fa ~size a1 a2, fb ~size b1 b2)

    let bop2_flags fa fb = fun ~size ~nsw ~nuw (a1,b1) (a2,b2) ->
      (fa ~size ~nsw ~nuw a1 a2, fb ~size ~nsw ~nuw b1 b2)

    let bop2_all_flags fa fb = fun ~size ~nsw ~nuw ~nusw (a1,b1) (a2,b2) ->
      (fa ~size ~nsw ~nuw ~nusw a1 a2, fb ~size ~nsw ~nuw ~nusw b1 b2)
    
    
    let biadd = bop2_all_flags ABF.biadd BBF.biadd
    let bisub = bop2_all_flags ABF.bisub BBF.bisub
    let bimul = bop2_flags ABF.bimul BBF.bimul
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
      Quadrivalent_basis.Boolean_Lattice.inter (fa a1 a2) (fb b1 b2)

    let beq   ~size = bpred (ABF.beq   ~size) (BBF.beq   ~size)
    let bisle ~size = bpred (ABF.bisle ~size) (BBF.bisle ~size)
    let biule ~size = bpred (ABF.biule ~size) (BBF.biule ~size)

    let biconst ~size k = (ABF.biconst ~size k, BBF.biconst ~size k)
    let buninit ~size = (ABF.buninit ~size, BBF.buninit ~size)
      
    let valid ~size acc_type (a,b) =
      Quadrivalent_basis.Boolean_Lattice.inter (ABF.valid ~size acc_type a) (BBF.valid ~size acc_type b)
    ;;

    let valid_ptr_arith ~size =
      bpred (ABF.valid_ptr_arith ~size) (BBF.valid_ptr_arith ~size)

    let bshift ~size ~offset ~max _ = assert false
    let bindex ~size _ = assert false
    
  end
end

module Prod_Binary_Backward
  (A:With_Binary_Backward with type boolean = Quadrivalent_basis.boolean)
  (B:With_Binary_Backward
   with type boolean = A.boolean) =
struct

  module ABB = A.Binary_Backward;;
  module BBB = B.Binary_Backward;;  
  
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

  let bop2_flags ~size ~nsw ~nuw op1 op2 =
    bop2 (op1 ~size ~nsw ~nuw) (op2 ~size ~nsw ~nuw)   
  let bop2_all_flags ~size ~nsw ~nuw ~nusw op1 op2 =
    bop2 (op1 ~size ~nsw ~nuw ~nusw) (op2 ~size ~nsw ~nuw ~nusw)        
  let bop2 ~size op1 op2 = bop2 (op1 ~size) (op2 ~size)


  let biadd = bop2_all_flags ABB.biadd BBB.biadd
  let bisub = bop2_all_flags ABB.bisub BBB.bisub    
  let bimul = bop2_flags ABB.bimul BBB.bimul
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
  
    
  (* let bitimes ~size _ _ _ = None
   * let nondet ~size l result = List.map (fun _ -> None) l *)
  let assume ~size _cond _store _result = assert false
  let bsext ~size ~oldsize x res = bop1 (ABB.bsext ~size ~oldsize) (BBB.bsext ~size ~oldsize) x res
  let buext ~size ~oldsize x res = bop1 (ABB.buext ~size ~oldsize) (BBB.buext ~size ~oldsize) x res
  let bofbool ~size a res = assert false
  let bchoose ~size = assert false

  let bextract ~size ~index ~oldsize x res = 
    bop1 (ABB.bextract ~size ~index ~oldsize) (BBB.bextract ~size ~index ~oldsize) x res
  ;;
  let valid ~size _ _ = assert false
  let valid_ptr_arith ~size _ _ _ = assert false
  let bshift ~size ~offset ~max _ = assert false
  let bindex ~size _ = assert false    
    
end

module Prod_Binary
  (A:Binary_Basis with type boolean = Quadrivalent_basis.boolean)
  (B:Binary_Basis
   with type boolean = A.boolean) =
struct
  let name = "Prod_Binary(" ^ A.name ^ "*" ^ B.name ^ ")";;
  
  module Boolean_Lattice = A.Boolean_Lattice
  module Binary_Lattice = struct
    (* include Prod_Lattice.Prod2_With_Inter_Bottom(A.Binary_Lattice)(B.Binary_Lattice) *)
    (* include Datatype_sig.Prod2(A.Binary_Lattice)(B.Binary_Lattice.Binary_Lattice);; *)
    let equal (a1,b1) (a2,b2) = A.Binary_Lattice.equal a1 a2 && B.Binary_Lattice.equal b1 b2
    let compare (a1,b1) (a2,b2) =
      let ra = A.Binary_Lattice.compare a1 a2 in
      if ra != 0 then ra
      else B.Binary_Lattice.compare b1 b2
    let sdbm x y = y + (x lsl 16) + (x lsl 6) - x;;          
    let hash (a,b) = sdbm (A.Binary_Lattice.hash a) (B.Binary_Lattice.hash b)
    let pretty ~size fmt (a,b) =
    (* Used to test the product itself. *)
      (* if(not @@ A.Binary_Lattice.equal a (Obj.magic b)) then
       *   Codex_log.fatal "Error differnt %a %a"  (A.Binary_Lattice.pretty ~size) a (B.Binary_Lattice.pretty ~size) b;
       * Format.fprintf fmt "%a" (A.Binary_Lattice.pretty ~size) a *)
    Format.fprintf fmt "(%a,%a)" (A.Binary_Lattice.pretty ~size) a (B.Binary_Lattice.pretty ~size) b
    ;;
    
    let includes ~size (a1,a2) (b1,b2) = A.Binary_Lattice.includes ~size a1 b1 && B.Binary_Lattice.includes ~size a2 b2        
    let widen ~size ~previous:(a1,a2) (b1,b2) = (A.Binary_Lattice.widen ~size ~previous:a1 b1),(B.Binary_Lattice.widen ~size ~previous:a2 b2)
    let includes_or_widen ~size ~previous next =
      if includes ~size previous next then (true,next)
      else (false,widen ~size ~previous next)
    ;;
    let join ~size (a1,a2) (b1,b2) = (A.Binary_Lattice.join ~size a1 b1, B.Binary_Lattice.join ~size a2 b2)
    let bottom ~size = A.Binary_Lattice.bottom ~size,B.Binary_Lattice.bottom ~size
    let top ~size = A.Binary_Lattice.top ~size,B.Binary_Lattice.top ~size
    let is_bottom ~size (a,b) = A.Binary_Lattice.is_bottom ~size a || B.Binary_Lattice.is_bottom ~size b
    let inter ~size (a1,a2) (b1,b2) = (A.Binary_Lattice.inter ~size a1 b1, B.Binary_Lattice.inter ~size a2 b2)
    let singleton ~size k = A.Binary_Lattice.singleton ~size k,
                            B.Binary_Lattice.singleton ~size k
    type t = A.Binary_Lattice.t * B.Binary_Lattice.t
  end
  module Boolean_Forward = A.Boolean_Forward
  module Boolean_Backward = A.Boolean_Backward
  type boolean = A.boolean
  type binary = Binary_Lattice.t

  module Binary_Forward = struct
    module AB = Prod_Binary_Forward(A)(B)
    include AB.Binary_Forward
  end
  module Binary_Backward = struct
    module AB = Prod_Binary_Backward(A)(B)
    include AB
  end
  let truth_value bool1 = bool1
  include Basis_sig.Dummy_Conversions



  let binary_is_empty ~size (a,b) = A.binary_is_empty ~size a || B.binary_is_empty ~size b
  
  let binary_to_ival ~signed ~size (a,b) =
    Framac_ival.Ival.narrow (A.binary_to_ival ~signed ~size a) (B.binary_to_ival ~signed ~size b)

  let binary_to_known_bits ~size (a,b) =
    Lattices.Known_Bits.inter ~size
      (A.binary_to_known_bits ~size a) (B.binary_to_known_bits ~size b)
  ;;
  
  (* A good technique would be to use a set, but first get the Ival to
     get bounds so that we enumerate the set without being to big.
     But as one of the components will be an ival any way, we can just
     do that. Eventually we could ask if one element is in the
     concretization of the other. *)
  let binary_fold_crop ~size (a,b) ~inf ~sup acc f = A.binary_fold_crop ~size a ~inf ~sup acc f

  
  let binary_is_singleton ~size (a,b) =
    match A.binary_is_singleton ~size a with
    | None -> B.binary_is_singleton ~size b
    | x -> x

  let convert_to_quadrivalent x = x
  
end
