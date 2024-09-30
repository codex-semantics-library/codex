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


module MakeOffset (Scalar : Domain_sig.Base)
: Memory_sig.Offset with module Context = Scalar.Context = struct

  include Scalar

  module Scalar = Scalar
  module Context = Scalar.Context
  
  type boolean = Scalar.boolean
  type offset = unit

  module Offset = struct
    let pretty _ = assert false
    let equal _ = assert false
  end

  let offset_pretty ~size:_ _ = assert false
  let offset_empty ~size:_ _ = assert false
  let serialize_offset ~size:_ _ = assert false

  let offset_zero ~size:_ ~max:_ _ = assert false
  let offset_shift ~size:_ ~offset:_ ~max:_ _ = assert false
  let offset_index ~size:_ _ = assert false
  let offset_sub ~size:_ _ = assert false
  let offset_le ~size:_ _ = assert false
  let offset_eq ~size:_ _ = assert false
  let offset_within_bounds ~size:_ _ = assert false
  let offset_choose ~size:_ _ = assert false

  let boolean2scalar_bool _ = assert false
  let scalar_bool2boolean _ = assert false

end

module MakeRegion (Sub : Memory_sig.Fixed_size_value_domain)
: Memory_sig.Block with module Value = Sub = struct

  include Sub.Scalar

  module Offset = MakeOffset (Sub.Scalar)
  module Value = Sub    
  module Context = Sub.Context
  type offset = Offset.offset

  
  type block = unit
  let pretty _ = assert false

  let load ~size:_ _ = assert false
  let store ~size:_ _ = assert false

  include Assert_False_Memory_Transfer_Functions.Block.Block_Backward

  let serialize _ = assert false

  let initial _ = assert false
  let unknown ~level:_ _ = assert false

  let block_empty _ = assert false

  let shared_addresses _ = assert false

end

module MakeDomain (Sub:Memory_sig.Whole_Memory_domain)
: Memory_sig.Complete_domain
   with module Address = Sub.Address
    and module Block.Value = Sub.Address
    and type Memory.address = Sub.Address.binary
    and type Memory.boolean = Sub.Address.boolean
= struct

  module Address = Sub.Address
  module Block = MakeRegion (Address)
  module Memory = Sub.Memory (Block)(struct let ctx x = x,fun x -> x end)
  
end