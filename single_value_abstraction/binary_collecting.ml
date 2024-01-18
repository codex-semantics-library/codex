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

let name = "Collecting";;

include Quadrivalent_basis;;

(* Quadrivalent viewed as a set. *)
module Quadrivalent = struct
  include Boolean_Lattice;;

  let empty = Bottom
  
  let add bool x = match x,bool with
    | Top, _ -> Top
    | True, true -> True
    | True, false -> Top
    | False, true -> Top
    | False, false -> False
    | Bottom, false -> False
    | Bottom, true -> True
  ;;

  
end

module ZSet = Lattices.BVSet.ZSet;;                    
module Binary_Lattice = struct
  include Lattices.BVSet;;
  let widen ~size ~previous x = join ~size previous x
  let includes_or_widen ~size ~previous x = assert false
  let includes ~size _ = assert false
  let is_bottom ~size x = x = ZSet.empty
end

module Concrete = Transfer_functions.Concrete.Bitvector_Interp

type binary = Binary_Lattice.t

[@@@ocaml.warning "-38"]
exception Empty = Transfer_functions.Concrete.Empty
[@@@ocaml.warning "+38"]                    

let binary_is_empty ~size x = ZSet.is_empty x
let binary_empty = 3
let binary_fold_crop ~size _ ~inf ~sup = assert false
let binary_is_singleton ~size = assert false
let binary_to_known_bits ~size = assert false
let binary_to_ival ~signed ~size = assert false
                     
module Binary_Forward = struct

  let lift1 op a =
    ZSet.fold(fun a acc ->
        ZSet.add (op a) acc) a ZSet.empty
  ;;

  
  let lift2 op a b =
    ZSet.fold (fun a acc ->
        ZSet.fold(fun b acc ->
            match op a b with
            | exception Empty -> acc
            | r -> ZSet.add r acc) b acc) a ZSet.empty
  ;;

  let lift2_pred op a b =
    ZSet.fold (fun a acc ->
        ZSet.fold(fun b acc ->
            Quadrivalent.add (op a b) acc) b acc) a Quadrivalent.empty
  ;;

  let biconst ~size k = ZSet.singleton k
  
  
  (* Note: this changes also the sign bit in Zarith, but we do not use it. *)
  let bnot ~size = lift1 (fun x -> Z.extract ( Z.(~!) x) 0 size)
  let band ~size = lift2 (Z.(land));;
  let bor  ~size = lift2 (Z.(lor));;
  let bxor ~size = lift2 (Z.(lxor));;
  let biadd ~size ~nsw ~nuw ~nusw = lift2 (Concrete.biadd ~size ~nsw ~nuw ~nusw);;
  let bisub ~size ~nsw ~nuw ~nusw = lift2 (Concrete.bisub ~size ~nsw ~nuw ~nusw);;
  let bimul ~size ~nsw ~nuw = lift2 (Concrete.bimul ~nsw ~nuw ~size);;
  let bisdiv ~size = lift2 (Concrete.bisdiv ~size);;
  let biudiv ~size = lift2 (Concrete.biudiv ~size);;        
  let bismod ~size = lift2 (Concrete.bismod ~size);;
  let biumod ~size = lift2 (Concrete.biumod ~size);;        

  

  let buext ~size ~oldsize x = x
                             
  let bsext ~size ~oldsize =
    lift1 (fun x -> Z.extract (Z.signed_extract x 0 oldsize) 0 size)
  ;;

  let bshl ~size ~nsw ~nuw  = lift2 (Concrete.bshl ~size ~nsw ~nuw);;

  let blshr ~size = lift2 (Concrete.blshr ~size);;
  let bashr ~size = lift2 (Concrete.bashr ~size);;

  let bconcat ~size1 ~size2 = lift2 (Concrete.bconcat ~size1 ~size2);;

  let bextract ~size ~index ~oldsize = 
    lift1 (Concrete.bextract ~index ~size ~oldsize);;
  ;;
  
  let beq ~size = lift2_pred (Z.equal);;
  let biule ~size = lift2_pred (Z.leq);;
  let bisle ~size = lift2_pred (Concrete.bisle ~size);;

  let pretty fmt x =
    let l = ZSet.elements x |> List.map (Z.format "%b") in
    Format.fprintf fmt "[";
    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",") Format.pp_print_string) fmt l;
    Format.fprintf fmt "]";    
  ;;

  let buninit ~size = assert false
  let bchoose ~size _ = assert false
  let bindex ~size _ = assert false
  let bshift ~size ~offset ~max _ = assert false
  let valid_ptr_arith ~size _ = assert false
  let valid ~size _ = assert false
  let bofbool ~size = function
    | Bottom -> ZSet.empty
    | True -> biconst ~size Z.one
    | False -> biconst ~size Z.zero
    | Top -> Binary_Lattice.join ~size (biconst ~size Z.zero) (biconst ~size Z.one)
;;


end

module Binary_Backward = struct
  include Basis_Assert_False_Transfer_Functions.Binary_Backward
end
