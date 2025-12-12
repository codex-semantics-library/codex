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

(* With_Noop: Dummy operations. *)

module Boolean_Backward = struct
  let (||) _ _ _ = (None,None)
  let (&&) _ _ _ = (None,None)
  let not _ _ = None
  let nondet l result = List.map (fun _ -> None) l
  let assume _cond _store _result = (None,None)
end

module Integer_Backward = struct
  let ieq0 a _ = None
  let ige0 a _ = None
  let itimes k a = None
  let iadd _ _ _ = (None,None)
  let imul _ _ _ = (None,None)
  let idiv _ _ _ = (None,None)
  let imod _ _ _ = (None,None)
  let ishl _ _ _ = (None,None)
  let ishr _ _ _ = (None,None)
  let assume _ _ _ = (None,None)
end



module Binary_Backward = struct
  let beq _ _ _ = (None,None)
  let biult _ _ _ = (None,None)
  let biule _ _ _ = (None,None)
  let bislt _ _ _ = (None,None)
  let bisle _ _ _ = (None,None)
  let bitimes _ _ _ = None
  let biadd _ _ _ = (None,None)
  let bimul ~size _ _ _ = (None,None)
  let bxor ~size _ _ _ = (None,None)
  let band ~size _ _ _ = (None,None)
  let bor ~size _ _ _ = (None,None)
  let nondet ~size l result = List.map (fun _ -> None) l
  let assume ~size _cond _store _result = (None,None)
  let bsext ~size _ _ = (None)
  let buext ~size _ _ = (None)
  let bashr ~size _ _ _ = (None,None)
  let blshr ~size _ _ _ = (None,None)
  let bshl ~size _ _ _ = (None,None)
  let bisdiv _ _ _ = (None,None)
  let bconcat l result = List.map (fun _ -> None) l
  let bismod _ _ _ = (None,None)
  let bextract ~size ~index _ _ = (None)
  let valid ~size _ _ = (None)
end


module Memory_Backward = struct
  let nondet l result = List.map (fun _ -> None) l
  let assume _cond _store _result = (None,None)
  let load ~size _ _ _ = (None,None)
  let store ~size _ _ _ _ = (None,None,None)
  let var () _ = ()
end

let memory_is_bottom _ = false
let boolean_is_bottom _ = false
let binary_is_bottom ~size _ = false
