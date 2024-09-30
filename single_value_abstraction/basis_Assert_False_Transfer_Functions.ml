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

module Integer_Backward = struct
  let assume _ _ = assert false
  let iunknown () = assert false
  let imod _ _ = assert false
  let idiv _ _ = assert false
  let imul _ _ = assert false
  let iadd _ _ = assert false
  let itimes k  _ = assert false
  let ige0 _ = assert false
  let ieq0 _ = assert false
  let ishl _ _ = assert false
  let ishr _ _ = assert false
  let ior _ _ = assert false
  let ixor _ _ = assert false
  let iand _ _ = assert false        
  let one  = ()
  let zero = one
  let isub _ = assert false
  let ieq _ = assert false
  let ile _ = assert false
  let ilt _ = assert false
  let iconst _ = ()
end

module Integer_Forward = struct
  include Integer_Backward
end

module Binary_Backward = struct
  let beq   ~size _ _ = assert false
  let biule ~size _ _ = assert false
  let bisle ~size _ _ = assert false
  let bitimes ~size _ _ = assert false
  let biadd ~size ~nsw ~nuw ~nusw _ _ = assert false
  let bisub ~size ~nsw ~nuw ~nusw _ _ = assert false
  let bimul ~size ~nsw ~nuw _ _ = assert false
  let bxor ~size _ _ = assert false
  let band ~size _ _ = assert false
  let bor ~size _ _ = assert false
  let nondet ~size l result = assert false
  let assume ~size _ _ = assert false
  let bsext ~size ~oldsize _ = assert false
  let buext ~size ~oldsize _ = assert false
  let bofbool ~size _ = assert false
  let bchoose ~size _ _ = assert false    
  let bashr ~size _ _ = assert false
  let blshr ~size _ _ = assert false
  let bshl ~size ~nsw ~nuw _ _ = assert false
  let bisdiv ~size _ _ = assert false
  let biudiv ~size _ _ = assert false
  let bconcat ~size1 ~size2 a b  = assert false
  let bismod ~size _ _ = assert false
  let biumod ~size _ _ = assert false
  let bextract ~size ~index ~oldsize _ = assert false
  let valid ~size _ = assert false
  let valid_ptr_arith ~size _ = assert false
  let buninit ~size = assert false
  let bunknown ~size  = assert false
  let baddr ~size _ = assert false
  let biconst ~size _ = assert false
  let bshift ~size ~offset ~max = assert false
  let bindex ~size _ = assert false
end

module Binary_Forward = Binary_Backward

module Block_Forward = struct
  let sizeof _ = assert false
  let concat ~size1 ~size2 _ = assert false
  let load ~size _ = assert false
  let store ~size _ = assert false
  let binary_to_block ~size _ = assert false
end
module Block_Backward = Block_Forward;;

module Memory_Backward = struct
  let var _ = assert false
  let nondet _ = assert false
  let assume _ = assert false
  let store ~size _ = assert false
  let memcpy ~size _ = assert false    
  let load ~size _ = assert false
  let malloc ~id ~malloc_size _ = assert false
  let free _ = assert false
  let unknown _ = assert false
end
module Memory_Forward = Memory_Backward;;
