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

(* Useful to complete functors. That way, everybody can take All
   as a parameter. *)

open Sva_sig

module Make(UnimplementedId:sig val loc:string end) = struct

  module Dummy_Enum_Lattice = Lattices.Unimplemented.Enum_Lattice(UnimplementedId)
  module Dummy_Integer_Lattice = Lattices.Unimplemented.Integer_Lattice(struct
      type t = unit
      include UnimplementedId end)    

  module Complete_Binary(B:BITVECTOR):ALL
    with type bitvector = B.bitvector
     and type boolean = B.boolean
  = struct
    include B
    module Memory_Lattice = Lattices.Unit
    type memory = Memory_Lattice.t
    module Memory_Backward = Sva_noop_transfer_functions.Memory_Backward
    module Memory_Forward = Sva_noop_transfer_functions.Memory_Forward

    module Block_Lattice = Lattices.Unit
    type block = Memory_Lattice.t
    module Block_Backward = Sva_noop_transfer_functions.Block_Backward
    module Block_Forward = Sva_noop_transfer_functions.Block_Forward

    module Integer_Lattice = Dummy_Integer_Lattice
    type integer = Integer_Lattice.t
    module Integer_Backward = Sva_Assert_False_Transfer_Functions.Integer_Backward
    module Integer_Forward = Sva_Assert_False_Transfer_Functions.Integer_Forward

    module Enum_Lattice = Dummy_Enum_Lattice
    type enum = Enum_Lattice.t
    module Enum_Backward = Sva_Assert_False_Transfer_Functions.Enum_Backward
    module Enum_Forward = Sva_Assert_False_Transfer_Functions.Enum_Forward
  end

  module Complete_Bitvector_Enum(B:BITVECTOR_ENUM):ALL
    with type bitvector = B.bitvector
     and type boolean = B.boolean
  = struct
    include B
    module Memory_Lattice = Lattices.Unit
    type memory = Memory_Lattice.t
    module Memory_Backward = Sva_noop_transfer_functions.Memory_Backward
    module Memory_Forward = Sva_noop_transfer_functions.Memory_Forward

    module Integer_Lattice = Dummy_Integer_Lattice
    type integer = Integer_Lattice.t
    module Integer_Backward = Sva_Assert_False_Transfer_Functions.Integer_Backward
    module Integer_Forward = Sva_Assert_False_Transfer_Functions.Integer_Forward

    module Block_Lattice = Lattices.Unit
    type block = Memory_Lattice.t
    module Block_Backward = Sva_noop_transfer_functions.Block_Backward
    module Block_Forward = Sva_noop_transfer_functions.Block_Forward
  end

  module Complete_Integer(B:INTEGER):ALL
    with type integer = B.integer
     and type boolean = B.boolean
  = struct
    include B
    module Memory_Lattice = Lattices.Unit
    type memory = Memory_Lattice.t
    module Memory_Backward = Sva_noop_transfer_functions.Memory_Backward
    module Memory_Forward = Sva_noop_transfer_functions.Memory_Forward

    module Block_Lattice = Lattices.Unit
    type block = Memory_Lattice.t
    module Block_Backward = Sva_noop_transfer_functions.Block_Backward
    module Block_Forward = Sva_noop_transfer_functions.Block_Forward

    module Bitvector_Lattice =  Lattices.Unimplemented.Bitvector_Lattice(struct
        type t = unit
        let loc = __LOC__
      end)

    type bitvector = Bitvector_Lattice.t
    module Bitvector_Backward = Sva_Assert_False_Transfer_Functions.Bitvector_Backward
    module Bitvector_Forward = Sva_Assert_False_Transfer_Functions.Bitvector_Forward

    module Enum_Lattice = Dummy_Enum_Lattice  
    type enum = Enum_Lattice.t
    module Enum_Backward = Sva_Assert_False_Transfer_Functions.Enum_Backward
    module Enum_Forward = Sva_Assert_False_Transfer_Functions.Enum_Forward

  end

  module Dummy_All:ALL = struct

    let _name = "Dummy"

    include Sva_quadrivalent

    module Integer_Lattice = Dummy_Integer_Lattice
    type integer = Integer_Lattice.t
    module Integer_Backward = Sva_Assert_False_Transfer_Functions.Integer_Backward
    module Integer_Forward = Sva_Assert_False_Transfer_Functions.Integer_Forward

    module Bitvector_Lattice =  Lattices.Unimplemented.Bitvector_Lattice(struct
        type t = unit
        let loc = __LOC__
      end)

    type bitvector = Bitvector_Lattice.t
    module Bitvector_Backward = Sva_Assert_False_Transfer_Functions.Bitvector_Backward
    module Bitvector_Forward = Sva_Assert_False_Transfer_Functions.Bitvector_Forward

    module Enum_Lattice = Dummy_Enum_Lattice
    type enum = Enum_Lattice.t
    module Enum_Backward = Sva_Assert_False_Transfer_Functions.Enum_Backward
    module Enum_Forward = Sva_Assert_False_Transfer_Functions.Enum_Forward

    module Memory_Lattice = Lattices.Unit
    type memory = Memory_Lattice.t
    module Memory_Backward = Sva_noop_transfer_functions.Memory_Backward
    module Memory_Forward = Sva_noop_transfer_functions.Memory_Forward

    module Block_Lattice = Lattices.Unit
    type block = Memory_Lattice.t
    module Block_Backward = Sva_noop_transfer_functions.Block_Backward
    module Block_Forward = Sva_noop_transfer_functions.Block_Forward
  end
end
