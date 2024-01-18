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

module type Reduce = sig
  type t1
  type t2
  val reduce : size:int -> t1 * t2 -> t1 * t2
end

module type Reduced_prod_functor = functor
  (B1 : Basis_sig.Binary_Integer_Basis)
  (B2 : Basis_sig.Binary_Integer_Basis)
  (R : Reduce with
    type t1 = B1.binary
    and type t2 = B2.binary)
  -> Basis_sig.Binary_Integer_Basis

(** A domain that abstracts a bitvector, trying to track whether it is equal to
   zero or not. *)
module Make
    (B1 : Basis_sig.Binary_Integer_Basis)
    (B2 : Basis_sig.Binary_Integer_Basis)
    (R : Reduce with
      type t1 = B1.binary
      and type t2 = B2.binary)
    : Basis_sig.Binary_Integer_Basis = struct
  let name = Printf.sprintf "Reduced_prod(%s)(%s)" B1.name B2.name

  include Quadrivalent_basis

  module Integer_Lattice = struct
    include Lattices.Unit (* TODO: Should be in Binary_Basis. *)
    let singleton _ = assert false
  end
  type integer = Integer_Lattice.t
  module Integer_Backward = Basis_Assert_False_Transfer_Functions.Integer_Backward
  module Integer_Forward = Basis_Assert_False_Transfer_Functions.Integer_Forward
  let is_singleton_int _ = assert false
  let convert_to_ival _ = assert false

  module Binary_Lattice = struct
    module L1 = B1.Binary_Lattice
    module L2 = B2.Binary_Lattice

    type t =
      | Bot
      | NonBot of L1.t * L2.t (** invariant: none of the arguments are bottom. *)

    let mk ~size (x1,x2) =
      if L1.is_bottom ~size x1 || L2.is_bottom ~size x2 then Bot
      else NonBot (x1,x2)

    let unpack ~size = function
      | Bot -> (L1.bottom ~size, L2.bottom ~size)
      | NonBot (x1,x2) -> x1,x2

    let bottom ~size:_ = Bot
    let is_bottom ~size:_ = function Bot -> true | _ -> false

    let top ~size = NonBot (L1.top ~size, L2.top ~size)

    let pretty ~size fmt =
      let open Format in function
      | Bot -> pp_print_string fmt "Bottom"
      | NonBot (x1,x2) ->
          fprintf fmt "@[<hov 2>(%a,@ %a)@]" (L1.pretty ~size) x1 (L2.pretty ~size) x2

    let includes ~size x y =
      match x,y with
      | _,Bot -> true
      | Bot,_ -> false
      | NonBot (x1,x2), NonBot (y1,y2) ->
          L1.includes ~size x1 y1 && L2.includes ~size x2 y2

    let join ~size x y =
      match x,y with
      | Bot,a | a,Bot -> a
      | NonBot (x1,x2), NonBot (y1,y2) ->
          NonBot (L1.join ~size x1 y1, L2.join ~size x2 y2)

    let inter ~size x y =
      match x,y with
      | Bot,_ | _,Bot -> Bot
      | NonBot (x1,x2), NonBot (y1,y2) ->
          NonBot (L1.inter ~size x1 y1, L2.inter ~size x2 y2)

    let hash = function
      | Bot -> Hashtbl.hash 0
      | NonBot (x,y) -> Hashtbl.hash (1, L1.hash x, L2.hash y)

    let compare x y =
      match x,y with
      | Bot,Bot -> 0
      | Bot, NonBot _ -> -1
      | NonBot _,Bot -> 1
      | NonBot (x1,x2), NonBot (y1,y2) ->
          let c = L1.compare x1 y1 in
          if c <> 0 then c else L2.compare x2 y2

    let equal x y =
      match x,y with
      | Bot,Bot -> true
      | NonBot (x1,x2), NonBot (y1,y2) ->
          L1.equal x1 y1 && L2.equal x2 y2
      | _ -> false

    let widen ~size ~previous x =
      let p1,p2 = match previous with
        | Bot -> L1.bottom ~size, L2.bottom ~size | NonBot (x,y) -> x,y in
      let x1,x2 = match x with
        | Bot -> L1.bottom ~size, L2.bottom ~size | NonBot (x,y) -> x,y in
      let res1,res2 =
        L1.widen ~size ~previous:p1 x1, L2.widen ~size ~previous:p2 x2 in
      if L1.is_bottom ~size res1 || L2.is_bottom ~size res2 then Bot
      else NonBot (res1,res2)

    let includes_or_widen ~size ~previous x =
      if includes ~size previous x then (true, x) else (false, widen ~size ~previous x)

    let singleton ~size i =
      NonBot (L1.singleton ~size i, L2.singleton ~size i)
  end

  type binary = Binary_Lattice.t

  module Binary_Forward = struct
    open Binary_Lattice
    module BF1 = B1.Binary_Forward
    module BF2 = B2.Binary_Forward

    let biconst ~size const =
      NonBot (BF1.biconst ~size const, BF2.biconst ~size const)

    let buninit ~size:_ = assert false

    let bop1 ~size f1 f2 x =
      let x1,x2 = unpack ~size x in
      mk ~size @@ R.reduce ~size @@ (f1 x1, f2 x2)

    let buext ~size ~oldsize =
      bop1 ~size (BF1.buext ~size ~oldsize) (BF2.buext ~size ~oldsize)
    let bsext ~size ~oldsize =
      bop1 ~size (BF1.bsext ~size ~oldsize) (BF2.bsext ~size ~oldsize)
    let bextract ~size ~index ~oldsize =
      bop1 ~size (BF1.bextract ~size ~index ~oldsize)
        (BF2.bextract ~size ~index ~oldsize)

    let bop2 f1 f2 = fun ~size x y ->
      let x1,x2 = unpack ~size x in
      let y1,y2 = unpack ~size y in
      mk ~size @@ R.reduce ~size (f1 ~size x1 y1, f2 ~size x2 y2)

    let bop2_wrapped f1 f2 = fun ~size ~nsw ~nuw x y ->
      let x1,x2 = unpack ~size x in
      let y1,y2 = unpack ~size y in
      mk ~size @@ R.reduce ~size (f1 ~size ~nsw ~nuw x1 y1, f2 ~size ~nsw ~nuw x2 y2)

    let bop2_wrapped_complex f1 f2 = fun ~size ~nsw ~nuw ~nusw x y ->
      let x1,x2 = unpack ~size x in
      let y1,y2 = unpack ~size y in
      mk ~size @@ R.reduce ~size (f1 ~size ~nsw ~nuw ~nusw x1 y1, f2 ~size ~nsw ~nuw ~nusw x2 y2)

    let biadd = bop2_wrapped_complex BF1.biadd BF2.biadd
    let bisub = bop2_wrapped_complex BF1.bisub BF2.bisub
    let bimul = bop2_wrapped BF1.bimul BF2.bimul
    let bisdiv = bop2 BF1.bisdiv BF2.bisdiv
    let bismod = bop2 BF1.bismod BF2.bismod
    let biudiv = bop2 BF1.biudiv BF2.biudiv
    let biumod = bop2 BF1.biumod BF2.biumod
    let band = bop2 BF1.band BF2.band
    let bor = bop2 BF1.bor BF2.bor
    let bxor = bop2 BF1.bxor BF2.bxor
    let bshl = bop2_wrapped BF1.bshl BF2.bshl
    let bashr = bop2 BF1.bashr BF2.bashr
    let blshr = bop2 BF1.blshr BF2.blshr

    let bofbool ~size b =
      mk ~size (BF1.bofbool ~size b, BF2.bofbool ~size b)

    let bconcat ~size1 ~size2 a b =
      let a1,a2 = unpack ~size:size1 a in
      let b1,b2 = unpack ~size:size2 b in
      mk ~size:(size1+size2)
        (BF1.bconcat ~size1 ~size2 a1 b1, BF2.bconcat ~size1 ~size2 a2 b2)

    (* Functions only useful for some memory domains, not used here *)
    let bindex ~size:_ = assert false
    let bshift ~size:_ = assert false
    let valid ~size:_ = assert false
    let valid_ptr_arith ~size:_ _ = assert false

    let bpred ~size f1 f2 = fun a b ->
      let a1,a2 = unpack ~size a in
      let b1,b2 = unpack ~size b in
      Quadrivalent_basis.Boolean_Lattice.inter (f1 ~size a1 b1) (f2 ~size a2 b2)

    let beq   ~size = bpred ~size BF1.beq   BF2.beq
    let bisle ~size = bpred ~size BF1.bisle BF2.bisle
    let biule ~size = bpred ~size BF1.biule BF2.biule
    let bchoose ~size:_ _cond _x = assert false
  end

  module Binary_Backward = struct
    module BB1 = B1.Binary_Backward
    module BB2 = B2.Binary_Backward
    open Binary_Lattice

    let coalesce_nones ~size olda oldb a b = match a,b with
      | None,None -> None
      | Some v, None -> Some (mk ~size @@ R.reduce ~size (v, oldb))
      | None, Some v -> Some (mk ~size @@ R.reduce ~size (olda, v))
      | Some va, Some vb -> Some (mk ~size (va,vb))

    let bpred2 ~size op1 op2 x y res =
      let x1,x2 = unpack ~size x in
      let y1,y2 = unpack ~size y in
      let (newa1,newb1) = op1 ~size x1 y1 res in
      let (newa2,newb2) = op2~size  x2 y2 res in
      (coalesce_nones ~size x1 x2 newa1 newa2,
       coalesce_nones ~size y1 y2 newb1 newb2)

    let beq   ~size = bpred2 ~size BB1.beq   BB2.beq
    let biule ~size = bpred2 ~size BB1.biule BB2.biule
    let bisle ~size = bpred2 ~size BB1.bisle BB2.bisle

    let bop2_common ~size op1 op2 x y res =
      let x1,x2 = unpack ~size x in
      let y1,y2 = unpack ~size y in
      let res1,res2 = unpack ~size res in
      let (newa1,newb1) = op1 x1 y1 res1 in
      let (newa2,newb2) = op2 x2 y2 res2 in
      (coalesce_nones ~size x1 x2 newa1 newa2,
       coalesce_nones ~size y1 y2 newb1 newb2)

    let bop2_wrapped ~size ~nsw ~nuw op1 op2 =
      bop2_common ~size (op1 ~size ~nsw ~nuw) (op2 ~size ~nsw ~nuw)

    let bop2_wrapped_complex ~size ~nsw ~nuw ~nusw op1 op2 =
      bop2_common ~size (op1 ~size ~nsw ~nuw ~nusw) (op2 ~size ~nsw ~nuw ~nusw)

    let bop2 ~size op1 op2 = bop2_common ~size (op1 ~size) (op2 ~size)

    let biadd = bop2_wrapped_complex BB1.biadd BB2.biadd
    let bisub = bop2_wrapped_complex BB1.bisub BB2.bisub
    let bimul = bop2_wrapped BB1.bimul BB2.bimul
    let band = bop2 BB1.band BB2.band
    let bor = bop2 BB1.bor BB2.bor
    let bxor = bop2 BB1.bxor BB2.bxor
    let bisdiv = bop2 BB1.bisdiv BB2.bisdiv
    let bismod = bop2 BB1.bismod BB2.bismod
    let biudiv = bop2 BB1.biudiv BB2.biudiv
    let biumod = bop2 BB1.biumod BB2.biumod
    let bashr = bop2 BB1.bashr BB2.bashr
    let blshr = bop2 BB1.blshr BB2.blshr
    let bshl = bop2_wrapped BB1.bshl BB2.bshl

    let bsext ~size:_ ~oldsize:_ _ _ = (None)
    let buext ~size:_ ~oldsize:_ _ _ = (None)
    let bconcat ~size1:_ ~size2:_ _ _ _ = (None,None)
    let bextract ~size:_ ~index:_ ~oldsize:_ _ _ = (None)
    let valid ~size:_ _ _ _ = (None)
    let valid_ptr_arith ~size:_ _ _ _ = (None,None)

    let bshift ~size:_ ~offset:_ ~max:_ _ = assert false
    let bindex ~size:_ _ = assert false

    let bofbool ~size bool bin =
      match bin with
      | Bot -> Some Lattices.Quadrivalent.Bottom
      | NonBot (bin1,bin2) ->
          match BB1.bofbool ~size bool bin1, BB2.bofbool ~size bool bin2 with
          | x, None -> x
          | None, x -> x
          | Some b1, Some b2 ->
              Some (if b1 <> b2 then Lattices.Quadrivalent.Bottom else b1)

    let bchoose ~size:_ _ = assert false

  end

  let binary_to_ival ~signed ~size x =
    let x1,x2 = Binary_Lattice.unpack ~size x in
    Framac_ival.Ival.narrow (B1.binary_to_ival ~signed ~size x1)
      (B2.binary_to_ival ~signed ~size x2)

  let binary_is_singleton ~size x =
    let x1,x2 = Binary_Lattice.unpack ~size x in
    match B1.binary_is_singleton ~size x1 with
    | None -> B2.binary_is_singleton ~size x2
    | x -> x

  (* Performs the folding on the second component. So the user should take care
   * to put the most precise domain second. *)
  let binary_fold_crop ~size x ~inf ~sup acc f =
    let _,x2 = Binary_Lattice.unpack ~size x in
    B2.binary_fold_crop ~size x2 ~inf ~sup acc f

  let binary_is_empty ~size x =
    let x1,x2 = Binary_Lattice.unpack ~size x in
    B1.binary_is_empty ~size x1 || B2.binary_is_empty ~size x2

  let binary_to_known_bits ~size x =
    let x1,x2 = Binary_Lattice.unpack ~size x in
    let kb1 = B1.binary_to_known_bits ~size x1 in
    let kb2 = B2.binary_to_known_bits ~size x2 in
    Lattices.Known_Bits.inter ~size kb1 kb2
end
