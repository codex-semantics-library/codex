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

module In_bits = Units.In_bits
module Make(Terms: Sig.TERMS) = struct

  module TC = Operator.Function_symbol
  exception Empty
  exception Not_a_constant

  let wraps ~(size:In_bits.t) x = Z.signed_extract x 0 (size:>int)
  let wrapu ~(size:In_bits.t) x = Z.extract x 0 (size:>int)

  (* For binaries, we return a Z whose last size bits correspond to
     the requested bitvector. *)
  let rec binary x = match x with
    | Terms.(Binary{term=T0{tag=TC.Biconst(size,k)}}) -> k
    | Terms.(Binary{term=T1{tag;a};size}) -> begin match tag with
        | TC.Bextract{size;oldsize;index} ->
          Z.extract (binary a) (index:>int) (size:>int)
        | TC.Buext _ ->
          let Terms.Binary{size=oldsize} = a in
          wrapu ~size:oldsize (binary a)
        | TC.Bsext _ ->
          let Terms.Binary{size=oldsize} = a in
          wraps ~size:oldsize (binary a)
        | TC.Bofbool _ -> assert false
        | TC.Bchoose _ -> binary a
      end
    | Terms.(Binary{term=T2{tag;a;b};size}) -> begin try
          match tag with
        | TC.Biadd _ -> Z.add (binary a) (binary b)
        | TC.Bisub _ -> Z.sub (binary a) (binary b)
        | TC.Bimul _ -> Z.mul (binary a) (binary b)
        | TC.Biudiv _ -> Z.div (wrapu ~size @@ binary a) (wrapu ~size @@ binary b)
        | TC.Bisdiv _ -> Z.div (wraps ~size @@ binary a) (wraps ~size @@ binary b)
        | TC.Biumod _ -> Z.rem (wrapu ~size @@ binary a) (wrapu ~size @@ binary b)
        | TC.Bismod _ -> Z.rem (wraps ~size @@ binary a) (wraps ~size @@ binary b)
        | TC.Bshl _ ->  Z.shift_left (binary a) (Z.to_int @@ binary b)
        | TC.Bashr _ -> Z.shift_right (wraps ~size @@ binary a) (Z.to_int @@ binary b)
        | TC.Blshr _ -> Z.shift_right (wrapu ~size @@ binary a)  (Z.to_int @@ binary b)
        | TC.Band _ -> Z.logand (binary a) (binary b)
        | TC.Bor _ -> Z.logor (binary a) (binary b)
        | TC.Bxor _ -> Z.logxor (binary a) (binary b)
        | TC.Bunion _ -> assert false
        | TC.Bconcat (size1,size2) ->
          Z.logor (Z.shift_left (wrapu ~size:size1 @@ binary a) (size2:>int)) (wrapu ~size:size2 @@ binary b)
        with Division_by_zero -> raise Empty
      end
    | Terms.(Binary{term=Empty}) -> raise Empty
    | _ -> Codex_log.fatal "Const eval on %a" Terms.pretty x

  and bitvector x = binary x

  and integer x = match x with
    | Terms.(Integer{term=T0{tag=TC.Iconst k}}) -> k
    | Terms.(Integer{term=T1{tag;a}}) ->
      (* Codex_log.warning "A constant integer was not simplified: %a" Terms.pretty x; *)
      (match tag with
       | TC.Itimes k -> Z.mul k (integer a)
       | _ -> .
      )

    | Terms.(Integer{term=T2{tag;a;b}}) ->
      (* Codex_log.warning "A constant integer was not simplified: %a" Terms.pretty x; *)
      (match tag with
       | TC.Imul -> Z.mul (integer a) (integer b)
       | TC.Iadd -> Z.add (integer a) (integer b)
       | TC.Isub -> Z.sub (integer a) (integer b)
       | TC.Idiv ->
         let b = integer b in
         if Z.equal b Z.zero then raise Empty
         else Z.div (integer a) b
       | TC.Imod -> Z.rem (integer a) (integer b)
       | TC.Ishl -> Z.shift_left (integer a) (Z.to_int (integer b))
       | TC.Ishr -> Z.shift_right (integer a) (Z.to_int (integer b))
       | TC.Ior -> Z.logor (integer a) (integer b)
       | TC.Iand -> Z.logand (integer a) (integer b)
       | TC.Ixor -> Z.logxor (integer a) (integer b)
      )
    | Terms.(Integer{term=Empty}) -> raise Empty
    | _ -> Codex_log.fatal "get const on %a" Terms.pretty x

  and boolean x = match x with
    | Terms.(Bool{term=T0{tag=TC.True}}) -> true
    | Terms.(Bool{term=T0{tag=TC.False}}) -> false
    | Terms.(Bool{term=Empty}) -> raise Empty
    | Terms.(Bool{term=T1{tag;a}}) ->
      (* Codex_log.warning "A constant boolean was not simplified: %a" Terms.pretty x; *)
      (match tag with
       | TC.Not -> not (boolean a)
       | TC.CaseOf _ -> assert false
       | _ -> .
      )
    | Terms.(Bool{term=T2{tag;a;b}}) ->
      (* Codex_log.warning "A constant boolean was not simplified: %a" Terms.pretty x; *)
      (match tag with
       | TC.Ile -> Z.leq (integer a) (integer b)
       | TC.Ieq -> Z.equal (integer a) (integer b)
       | TC.And -> (&&) (boolean a) (boolean b)
       | TC.BoolUnion -> assert false
       | TC.Or ->  (||) (boolean a) (boolean b)
       | TC.Beq _ -> Z.equal (binary a) (binary b)
       | TC.Biule size -> Z.leq (wrapu ~size @@ binary a) (wrapu ~size @@ binary b)
       | TC.Bisle size -> Z.leq (wraps ~size @@ binary a) (wraps ~size @@ binary b)
       | _ -> .
      )
    | _ -> Codex_log.fatal "get const on %a" Terms.pretty x

  and enum x = match x with
    | Terms.(Enum{term=Empty}) -> raise Empty
    | _ -> Codex_log.fatal "get const on %a" Terms.pretty x

end
