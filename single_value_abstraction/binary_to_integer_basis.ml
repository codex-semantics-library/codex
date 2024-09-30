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

(* Lifts a binary to its integer property. *)

open Basis_sig;;

(* TODO: En fait on a surtout besoin de top; on pourrait avoir une autre basis pour les booleens. *)
module Lift
  (I:Integer_Basis with type boolean = Quadrivalent_basis.boolean):
  Binary_Basis with type binary = I.integer
               and type boolean = I.boolean
                      =
struct
  let name = "Binary_to_integer_basis.Lift(" ^ I.name ^ ")"
  module Boolean_Lattice = I.Boolean_Lattice
  module Binary_Lattice = struct
    include I.Integer_Lattice
    let includes ~size = includes
    let is_bottom ~size = I.Integer_Lattice.is_bottom
    let bottom ~size = bottom
    let top ~size = top
    let inter ~size = inter
    let join ~size = join
    let pretty ~size = I.Integer_Lattice.pretty
    let widen ~size ~previous next = widen ~previous next
    let includes_or_widen ~size ~previous next = includes_or_widen ~previous next
    let singleton ~size = singleton
  end
  type binary = I.integer
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
  module Binary_Forward = struct
    let buninit ~size = IL.bottom
    let biconst ~size i = IF.iconst i
    let valid ~size _ _ = Quadrivalent_basis.Top
    let valid_ptr_arith ~size _ _ _ = Quadrivalent_basis.Top
    let blshr ~size = IF.ishr
    let bashr ~size = IF.ishr
    let bshl ~size ~nsw ~nuw = IF.ishl
    let biumod ~size a b = IF.imod a b
    let biudiv ~size a b = IF.idiv a b
    let bismod ~size a b = IF.imod a b
    let bisdiv ~size a b = IF.idiv a b
    let bsext ~size ~oldsize x = x
    let buext ~size ~oldsize x = x     (* Note: wrong if x is not in the right interval *)
    let bofbool ~size _ = assert false
    let bchoose ~size cond x = x
    let bxor ~size a b = I.Integer_Lattice.top
    let bor ~size a b = I.Integer_Lattice.top
    let band ~size a b = I.Integer_Lattice.top
    let bextract ~size ~index ~oldsize b =
      if size == oldsize then b
      else let modu = IF.imod b (IF.iconst (Z.shift_left Z.one (index + size))) in
        let result = IF.idiv modu (IF.iconst (Z.shift_left Z.one index)) in
        result

    let bconcat ~size1 ~size2 _ _ = I.Integer_Lattice.top
    let biule ~size a b = IF.ile a b
    let bisle ~size a b = IF.ile a b
    let beq ~size a b = IF.ieq a b
    let bimul ~size ~nsw ~nuw a b = IF.imul a b
    let biadd ~size ~nsw ~nuw ~nusw a b = IF.iadd a b
    let bisub ~size ~nsw ~nuw ~nusw a b = IF.isub a b

    let bshift ~size ~offset ~max _ = assert false
    let bindex ~size _ = assert false
  end

  (* TODO: Gerer le modulo; non seulement le debordement entier, mais aussi le "wrap" de  *)
  (* Devrait etre fait dans le module entier? En meme temps, on a
     besoin de la taille....  *)
  module Binary_Backward = struct
    include Basis_noop_transfer_functions.Binary_Backward

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
    
    let isub_fwd a b = IF.iadd a (unary_minus_fwd b);;
    let isub_bwd a b res =
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

    let biadd ~size  ~nsw ~nuw ~nusw = IB.iadd
    let biumod ~size = IB.imod
    let biudiv ~size = IB.idiv
    let bismod ~size = IB.imod
    let bisdiv ~size = IB.idiv

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

  let truth_value = I.truth_value
  include Basis_sig.Dummy_Conversions
  let convert_to_quadrivalent x = x
  let binary_to_known_bits ~size _ = assert false
  let binary_to_ival ~signed ~size x = (assert signed (* unsigned unimplemented *); I.convert_to_ival x)
  let binary_is_empty ~size x = Ival_basis.Integer_Lattice.is_bottom @@ binary_to_ival ~signed:true ~size x
  let binary_fold_crop ~size x ~inf ~sup acc f =
    Ival_basis.integer_fold_crop (binary_to_ival ~signed:true ~size x) ~inf ~sup f acc
  let binary_is_singleton ~size = I.is_singleton_int
              
end
