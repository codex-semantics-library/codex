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

let _name = "Collecting";;

include Sva_quadrivalent;;

module In_bits = Units.In_bits

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
module Bitvector_Lattice = struct
  include Lattices.Unimplemented.Bitvector_Lattice(struct
      type t = Lattices.BVSet.t
      let loc = __LOC__
    end)
  include Lattices.BVSet;;
  let widen ~size ~previous x = join ~size previous x
  let is_bottom ~size x = x = ZSet.empty
end

module Concrete = Operator.Concrete.Bitvector_Interp

type bitvector = Bitvector_Lattice.t

[@@@ocaml.warning "-38"]
exception Empty = Operator.Concrete.Empty
[@@@ocaml.warning "+38"]

module Bitvector_Forward = struct

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

  let band ~size = lift2 (Z.(land));;
  let bor  ~size = lift2 (Z.(lor));;
  let bxor ~size = lift2 (Z.(lxor));;
  let biadd ~size ~flags = lift2 (Concrete.biadd ~size ~flags);;
  let bisub ~size ~flags = lift2 (Concrete.bisub ~size ~flags);;
  let bimul ~size ~flags = lift2 (Concrete.bimul ~flags ~size);;
  let bimul_add ~size ~prod ~offset = lift1 (fun x -> Z.(prod * x + offset))
  let bisdiv ~size = lift2 (Concrete.bisdiv ~size);;
  let biudiv ~size = lift2 (Concrete.biudiv ~size);;
  let bismod ~size = lift2 (Concrete.bismod ~size);;
  let biumod ~size = lift2 (Concrete.biumod ~size);;



  let buext ~size ~oldsize x = x

  let bsext ~size ~oldsize =
    let size = In_bits.to_int size and oldsize = In_bits.to_int oldsize in
    lift1 (fun x -> Z.extract (Z.signed_extract x 0 oldsize) 0 size)
  ;;

  let bshl ~size ~flags  = lift2 (Concrete.bshl ~size ~flags);;

  let blshr ~size = lift2 (Concrete.blshr ~size);;
  let bashr ~size = lift2 (Concrete.bashr ~size);;

  let bconcat ~size1 ~size2 = lift2 (Concrete.bconcat ~size1 ~size2);;

  let bextract ~size ~index ~oldsize =
    lift1 (Concrete.bextract ~index ~size ~oldsize);;
  ;;

  let beq ~size = lift2_pred (Z.equal);;
  let biule ~size = lift2_pred (Z.leq);;
  let bisle ~size = lift2_pred (Concrete.bisle ~size);;

  let _pretty fmt x =
    let l = ZSet.elements x |> List.map (Z.format "%b") in
    Format.fprintf fmt "[";
    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",") Format.pp_print_string) fmt l;
    Format.fprintf fmt "]"

  let bofbool ~size = function
    | Bottom -> ZSet.empty
    | True -> biconst ~size Z.one
    | False -> biconst ~size Z.zero
    | Top -> Bitvector_Lattice.join ~size (biconst ~size Z.zero) (biconst ~size Z.one)
;;


end

module Bitvector_Backward = struct
  include Sva_Assert_False_Transfer_Functions.Bitvector_Backward
end
