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

(* TODO: backward transfer functions from usage of the inverse
   function + inter. E.g. use - when there was a +, etc. *)

module StringSet = Set.Make(struct
    type t = string
    let compare = compare
  end)

let past_warnings = ref StringSet.empty
let warn_once what =
  if not (StringSet.mem what !past_warnings) then begin
    past_warnings := StringSet.add what !past_warnings;
    Codex_log.warning "No backpropagation for '%s'" what
  end

module Integer_Backward = struct
  let assume _ _ _  = warn_once "assume"; (None,None)
  let iunknown _ _  = warn_once "iunknown"; (None)
  let imod _ _ _  = warn_once "imod"; (None,None)
  let idiv _ _ _  = warn_once "idiv"; (None,None)
  let imul _ _ _  = warn_once "imul"; (None,None)
  let iadd _ _ _  = warn_once "iadd"; (None,None)
  let itimes k _ _  = warn_once "itimes"; None
  let ige0 _ _  = warn_once "ige0"; None
  let ieq0 _ _  = warn_once "ieq0"; None
  let ishl _ _ _  = warn_once "ishl"; (None,None)
  let ishr _ _ _  = warn_once "ishr"; (None,None)
  let iand _ _ _  = warn_once "iand"; (None,None)
  let ior _ _ _  = warn_once "ior"; (None,None)
  let ixor _ _ _  = warn_once "ixor"; (None,None)
end

module Binary_Backward = struct
  let beq ~size _ _ _  = warn_once "beq"; (None,None)
  let biule ~size _ _ _  = warn_once "biule"; (None,None)
  let bisle ~size _ _ _  = warn_once "bisle"; (None,None)
  let bitimes ~size _ _ _  = warn_once "bitimes"; None
  let biadd ~size ~nsw ~nuw ~nusw _ _ _  = warn_once "biadd"; (None,None)
  let bisub ~size ~nsw ~nuw ~nusw _ _ _  = warn_once "bisub"; (None,None)
  let bimul ~size ~nsw ~nuw _ _ _  = warn_once "bimul"; (None,None)
  let bxor ~size _ _ _  = warn_once "bxor"; (None,None)
  let band ~size _ _ _  = warn_once "band"; (None,None)
  let bor ~size _ _ _  = warn_once "bor"; (None,None)
  let nondet ~size l result  = warn_once "nondet"; List.map (fun _ -> None) l
  let assume ~size _cond _store _result  = warn_once "assume"; (None,None)
  let bsext ~size ~oldsize _ _  = warn_once "bsext"; (None)
  let buext ~size ~oldsize _ _  = warn_once "buext"; (None)
  let bofbool ~size _ _  = warn_once "bofbool"; (None)
  let bchoose ~size cond a b = (None)                                                
  let bashr ~size _ _ _  = warn_once "bashr"; (None,None)
  let blshr ~size _ _ _  = warn_once "blshr"; (None,None)
  let bshl ~size ~nsw ~nuw _ _ _  = warn_once "bshl"; (None,None)
  let bisdiv ~size _ _ _  = warn_once "bisdiv"; (None,None)
  let biudiv ~size _ _ _  = warn_once "biudiv"; (None,None)
  let bconcat ~size1 ~size2 a b _  = warn_once "bconcat"; (None,None)
  let bismod ~size _ _ _  = warn_once "bismod"; (None,None)
  let biumod ~size _ _ _  = warn_once "biumod"; (None,None)
  let bextract ~size ~index ~oldsize _ _  = warn_once "bextract"; (None)
  let valid ~size _ _ _ = warn_once "valid"; (None)
  let valid_ptr_arith ~size _ _ _ = warn_once "valid_ptr_arith"; (None,None)
  let bshift ~size ~offset ~max _ _  = warn_once "bshift"; (None)
  let bindex ~size _ _k _ _  = warn_once "bindex"; (None,None)

end


module Memory_Forward = struct
  let var _ = assert false
  let nondet _ = assert false
  let assume _ = assert false
  let store ~size _ = assert false
  let memcpy ~size _ = assert false    
  let load ~size _ = assert false
  let builtin _ = assert false
  let malloc ~id ~malloc_size = assert false
  let free _ = assert false
  let unknown ~level = assert false
end
module Memory_Backward = Memory_Forward;;
