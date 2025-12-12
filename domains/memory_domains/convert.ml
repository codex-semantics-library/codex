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

(** Context conversion procedures: pass through the values by just
   changing the context. *)

module Convert_Memory_Forward
  (C: Sig.Convert_Contexts)
  (D: Memory_sig.WITH_MEMORY_FORWARD with module Context = C.To) =
struct
  module C = Sig.Make_Convert(C)
  module F = struct include D;; include D.Memory_Forward end
  include Operator.Conversions.Convert_Memory_Forward(C)(F)
end

(* This will help to the transition in a top-down manner, starting
   from the translation and top-level domain to the lower-level
   domain.

   The idea is to support both interfaces, and use conversion to
   simplify the support for both. I can have a signature for both
   domains, and an "AddMonadic" functor to support both domains. *)
module Convert_to_monadic(D: Memory_sig.Base) = struct

  include Sig.Convert_to_monadic(D: Memory_sig.Base)

  module Types = struct
    include Types
    type memory = D.memory
    type block = D.block
    type offset = D.offset
    type value = D.binary
    type address = D.binary
    type enum = D.enum
  end

  module Block_Forward = Operator.Conversions.Convert_Block_Forward(Conversion)(struct include Types include D.Block_Forward end)
  module Memory_Forward = Operator.Conversions.Convert_Memory_Forward(Conversion)(struct include Types include D.Memory_Forward end)
end
