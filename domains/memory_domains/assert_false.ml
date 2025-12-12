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

(* Used to create dummy types to fill the signatures, even when the
   abstract domain does not support the type. *)
module Block = struct
  module Block = Datatype_sig.Unit
  module Block_Forward = struct
    let sizeof _ = assert false
    let concat _ = assert false
    let load ~size _ = assert false
    let store ~size _ = assert false
    let binary_to_block ~size = assert false
  end
  module Block_Backward = Block_Forward;;
end

module Memory = struct
  module Memory = Datatype_sig.Unit
  module Memory_Forward = struct
    let assume _ = assert false
    let load ~size _ = assert false
    let store ~size _ = assert false
    let load_block _ = assert false
    let store_block _ = assert false
    let memcpy ~size _ = assert false
    let malloc ~id ~malloc_size = assert false
    let free _ = assert false
    let unknown ~level = assert false
  end
  module Memory_Backward = Memory_Forward;;
  let memory_is_bottom _ = assert false
end


(* A commodity to start implementing domains; start from here, then complete things as needed. *)
module Domain
= struct

  include Assert_false_domain.Domain

  module Types = struct
    include Types
    type memory = unit
    type offset = unit
    type block = unit
  end
  include Types

  module Block_Forward = Block.Block_Forward

  module Memory_Forward = Memory.Memory_Forward

  let serialize_memory ~widens _ = assert false ;;
  let serialize_block ~widens _ = assert false ;;

  let memory_is_bottom ctx = assert false

  (*************** Unknown ***************)

  let block_unknown _ = assert false

 (**************** Pretty printing ****************)

  let block_pretty ctx fmt = assert false
  let offset_pretty ctx fmt = assert false
  let memory_pretty ctx fmt = assert false

  (****************** Bottom ******************)

  let block_empty _ = assert false
  let memory_empty _ = assert false

  (**************** Queries ****************)

  module Query = struct
    include Query
    let reachable _ = assert false
  end

  let memory_is_empty _ = assert false

  (* Assume. *)
  let assume_memory _ctx _ _ = assert false

  let reachable _ = assert false

  let should_focus ~size:_ _ = assert false
  let may_alias ~ptr_size:_ _ ~size1:_ ~size2:_ = assert false
  let is_weak ~size _ _ = assert false

  (* include Operator.Builtin.Make(Types)(Context) *)

  let global_symbol _ = assert false
  let add_global_symbol ~size:_ _ = assert false
  let builtin_show_each_in _ = assert false
  let check_type ~size _ = assert false
  let type_of ~size:_ _  = assert false
  let analyze_summary _ = assert false
  let addresses_in_block _ = assert false
  let addresses_in_memory _ = assert false
  let addresses_in_binary ~size:_ _ = assert false

  (** Function relative to offsets (to be removed) **)

  let offset_pretty _ = assert false
  let offset_zero ~max:_ _ = assert false
  let offset_shift ~offset ~max _ = assert false
  let offset_le _ = assert false
  let offset_eq _ = assert false

end
