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

(* Product with intersection semantics: elements are in the
   intersections of the concretizations. *)

open Lattice_sig;;
  
module Prod2(L1:JOIN_SEMI_LATTICE)(L2:JOIN_SEMI_LATTICE)
  :JOIN_SEMI_LATTICE with type t = L1.t * L2.t =
struct
  include Datatype_sig.Prod2(L1)(L2);;
  let includes (a1,a2) (b1,b2) = L1.includes a1 b1 && L2.includes a2 b2
  let widen ~previous:(a1,a2) (b1,b2) = (L1.widen ~previous:a1 b1),(L2.widen ~previous:a2 b2)
  let includes_or_widen ~previous next =
    if includes previous next then (true,previous)
    else (false,widen ~previous next)
  ;;
  let join (a1,a2) (b1,b2) = (L1.join a1 b1, L2.join a2 b2)
end

module Prod2_With_Bottom(L1:JOIN_SEMI_LATTICE_WITH_BOTTOM)(L2:JOIN_SEMI_LATTICE_WITH_BOTTOM) = struct
  include Datatype_sig.Prod2(L1)(L2);;
  let includes (a1,a2) (b1,b2) = L1.includes a1 b1 && L2.includes a2 b2
  let widen ~previous:(a1,a2) (b1,b2) = (L1.widen ~previous:a1 b1),(L2.widen ~previous:a2 b2)
  let includes_or_widen ~previous next =
    if includes previous next then (true,next)
    else (false,widen ~previous next)
  ;;

  let join (a1,a2) (b1,b2) = (L1.join a1 b1, L2.join a2 b2)
  let bottom () = L1.bottom (), L2.bottom ()
  let is_bottom (a,b) = L1.is_bottom a || L2.is_bottom b
end

module Prod2_With_Inter_Bottom
    (L1:JOIN_SEMI_LATTICE_WITH_INTER_BOTTOM)
    (L2:JOIN_SEMI_LATTICE_WITH_INTER_BOTTOM) = struct
  include Prod2_With_Bottom(L1)(L2);;
  let inter (a1,a2) (b1,b2) = (L1.inter a1 b1, L2.inter a2 b2)
end

