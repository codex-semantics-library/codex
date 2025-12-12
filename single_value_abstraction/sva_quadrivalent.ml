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

module Boolean_Lattice = Lattices.Quadrivalent
include Boolean_Lattice

let name = "Single_value_abstraction.Quadrivalent"

module Boolean_Forward = struct
  let true_ = True
  let false_ = False

  let not = function
    | Bottom -> Bottom
    | True -> False
    | False -> True
    | Top -> Top
  ;;

  let (&&) a b = match (a,b) with
    | Bottom, _ | _, Bottom -> Bottom
    | False, _ | _, False -> False
    | True, x | x, True -> x
    | Top, Top -> Top

  let (||) a b = match (a,b) with
    | Bottom, _ | _, Bottom -> Bottom
    | True, _ | _, True -> True
    | False, x | x, False -> x
    | Top, Top -> Top

end


let refine older newer = match older,newer with
  | _, Top -> None
  | Bottom, _ -> None
  | _, Bottom -> Some Bottom
  | True, True -> None
  | True, False -> Some Bottom
  | False, False -> None
  | False, True -> Some Bottom
  | Top, True -> Some True
  | Top, False -> Some False

module Boolean_Backward = struct

  (* Note: if the result is bottom it means that one of the argument
     should be bottom, but as we do not know which, we cannot refine
     anything. *)
  let (&&) a1 a2 res = match res with
    | Top -> None,None
    | Bottom -> None, None
    | True -> (refine a1 True), (refine a2 True)
    | False -> (match a1,a2 with
        | True, x -> None, (refine x False)
        | x, True -> (refine x False), None
        | False, _ | _, False -> None,None
        | Bottom, _ | _, Bottom -> None, None
        | Top, Top -> None,None
      )
  ;;

  let (||) a1 a2 res = match res with
    | Top -> None,None
    | Bottom -> None, None
    | False -> (refine a1 False), (refine a2 False)
    | True -> (match a1,a2 with
        | False, x -> None, (refine x True)
        | x, False -> (refine x True), None
        | True, _ | _, True -> None,None
        | Bottom, _ | _, Bottom -> None, None
        | Top, Top -> None,None
      )
  ;;

  let not x res =
    let notres = Boolean_Forward.not res in
    let red = Boolean_Lattice.inter x notres in
    if Boolean_Lattice.equal red res then None else Some red

end

let refine ~older ~newer = refine older newer
