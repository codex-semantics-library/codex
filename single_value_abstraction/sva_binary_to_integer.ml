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

(* Lifts a binary to its integer property. *)

open Sva_sig
module In_bits = Units.In_bits

(* TODO: En fait on a surtout besoin de top; on pourrait avoir une autre basis pour les booleens. *)
module Lift
  (I:INTEGER with type boolean = Sva_quadrivalent.boolean):
  BITVECTOR with type bitvector = I.integer
                   and type boolean = I.boolean =
struct
  let name = "Binary_to_integer_basis.Lift(" ^ I.name ^ ")"
  module Boolean_Lattice = I.Boolean_Lattice
  module Bitvector_Lattice = Lattices.Bitvector_Of_Integer.Make(I.Integer_Lattice)
  type bitvector = I.integer
  type boolean = I.boolean
  module Boolean_Forward = I.Boolean_Forward
  module Boolean_Backward = I.Boolean_Backward

  module IL = I.Integer_Lattice
  module IF = I.Integer_Forward
  module IB = I.Integer_Backward

  (* TODO: Gerer le modulo; non seulement le debordement entier, mais
     aussi le "wrap" quand on passe de signé à non signé... Le modulo
     signé = ajouter 2^n-1, faire le modulo 2^n normal, et soustraire
     2^n-1. Trouver les opérations pour lesquelles il est nécessaire
     de faire le modulo. *)

  (* Devrait etre fait dans le module entier? En meme temps, on a
     besoin de la taille....  *)
  module Bitvector_Forward = struct
    let biconst ~size i = IF.iconst i
    let blshr ~size = IF.ishr
    let bashr ~size = IF.ishr
    let bshl ~size ~flags = IF.ishl
    let biumod ~size a b = IF.imod a b
    let biudiv ~size a b = IF.idiv a b
    let bismod ~size a b = IF.imod a b
    let bisdiv ~size a b = IF.idiv a b
    let bsext ~size ~oldsize x = x
    let buext ~size ~oldsize x = x     (* Note: wrong if x is not in the right interval *)
    let bofbool ~size _ = assert false
    let bxor ~size a b = IF.ixor a b
    let bor ~size a b = IF.ior a b
    let band ~size a b = IF.iand a b
    let bextract ~size ~index ~oldsize b =
      let size = In_bits.to_int size in
      let index = In_bits.to_int index in
      let oldsize = In_bits.to_int oldsize in      
      if size == oldsize then b
      else let modu = IF.imod b (IF.iconst (Z.shift_left Z.one (index + size))) in
        let result = IF.idiv modu (IF.iconst (Z.shift_left Z.one index)) in
        result

    let bconcat ~size1 ~size2 _ _ = I.Integer_Lattice.top ()
    let biule ~size a b = IF.ile a b
    let bisle ~size a b = IF.ile a b
    let beq ~size a b = IF.ieq a b
    let bimul ~size ~flags a b = IF.imul a b
    let biadd ~size ~flags a b = IF.iadd a b
    let bisub ~size ~flags a b = IF.isub a b
    let bimul_add ~size ~prod ~offset x = IF.iadd (IF.imul (IF.iconst prod) x) (IF.iconst offset)
  end

  (* TODO: Gerer le modulo; non seulement le debordement entier, mais aussi le "wrap" de  *)
  (* Devrait etre fait dans le module entier? En meme temps, on a
     besoin de la taille....  *)
  module Bitvector_Backward = struct
    include Sva_noop_transfer_functions.Bitvector_Backward

    (* Compose base operators into easier-to-use ones. *)

    (* Note: I don't think there is an easy way to have a DSL to
       express these operations in a generic way, e.g. like a
       monad. This shows in arity: the way we perform composition
       depends on the arity of the main operator, and of the
       operators that it calls. Of course there are some
       factorisations, e.g. both predicates call substraction in the
       same way. *)

    let unary_minus_fwd a = IF.itimes Z.minus_one a;;
    let unary_minus_bwd a res = IB.itimes Z.minus_one a res;;

    let _isub_fwd a b = IF.iadd a (unary_minus_fwd b);;
    let _isub_bwd a b res =
      let minusb = unary_minus_fwd b in
      let (newa,newminusb) = IB.iadd a minusb res in
      let newb =
        match newminusb with
        | None -> None
        | Some newminusb -> unary_minus_bwd b newminusb
      in (newa,newb)
    ;;

    let biule ~size = IB.ile
    let bisle ~size = IB.ile

    let biadd ~size ~flags  = IB.iadd
    let biumod ~size = IB.imod
    let biudiv ~size = IB.idiv
    let bismod ~size = IB.imod
    let bisdiv ~size = IB.idiv
    let bimul_add ~size ~prod ~offset x res =
      let prod = IF.iconst prod in
      let intermediate = (IF.imul prod x) in
      (* We could check that the second arguments match the constants here, and
         raise bottom if that is not the case... *)
      match IB.iadd intermediate (IF.iconst offset) res |> fst with
      | None -> None
      | Some intermediate' -> IB.imul prod x intermediate' |> snd


  (* Note: comparison between having predicates like a == b and the need to do (a - b) == 0.

     When the predicate is known to be false, the backward propagation
     of a == b with "diff_if_singleton" is more efficient than
     suppressing the 0 from (a - b), and adding it back, especially if
     a and b are not singletons.

     On the other hand, for other predicates like a <= b the
     normalization clearly simplifies the work (i.e. less
     implementation time); moreover it concentrates information on (a
     - b), even if we cannot reduce them. E.g. it allows to prove
     correct if (a != b) d = 3/a - b without any knowledge about a and
     b. *)
  end

  (* let binary_to_ival _ = assert false  *)
  (* let _binary_fold_crop ~size x ~inf ~sup acc f = *)
  (*   Sva_ival.Integer_Lattice.fold_crop (binary_to_ival ~signed:true ~size x) ~inf ~sup f acc *)

end
