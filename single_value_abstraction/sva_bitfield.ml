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

module B = Sva_quadrivalent

module Enum_Lattice = Lattices.Bitfield
include Enum_Lattice

type boolean = B.boolean
type enum = t

module Enum_Forward = struct
  let caseof ~case a =
    if is_bottom a then Lattices.Quadrivalent.Bottom
    else 
      let c = singleton case in
      if Z.equal a c then B.Boolean_Forward.true_
      else if includes a c then B.Top
      else B.Boolean_Forward.false_

  let enum_const ~case = singleton case
end

module Enum_Backward = struct

  let caseof ~case a res =
    match res with 
    | B.Top -> None
    | B.Bottom -> Some (bottom ())
    | B.True -> Some (inter a (singleton case))
    | B.False -> 
      let notc = Z.lognot @@ singleton case in
      Some (Z.logand a notc)
end
