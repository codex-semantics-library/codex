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

(* Useful to complete functors. That way, everybody can take All_Basis
   as a parameter. *)

open Basis_sig

module Complete_Binary(B:Binary_Basis):All_Basis
  with type binary = B.binary
   and type boolean = B.boolean
= struct
  let name = "Dummy"
  include Basis_sig.Dummy_Conversions  
  include B
  module Memory_Lattice = Lattices.Unit
  type memory = Memory_Lattice.t
  module Memory_Backward = Basis_noop_transfer_functions.Memory_Backward
  module Memory_Forward = Basis_noop_transfer_functions.Memory_Forward

  module Block_Lattice = Lattices.Unit
  type block = Memory_Lattice.t
  module Block_Backward = Basis_noop_transfer_functions.Block_Backward
  module Block_Forward = Basis_noop_transfer_functions.Block_Forward

  module Integer_Lattice = struct
    include Lattices.Unit (* TODO: Should be in Binary_Basis. *)
    let singleton _ = assert false
  end
  type integer = Integer_Lattice.t
  module Integer_Backward = Basis_Assert_False_Transfer_Functions.Integer_Backward
  module Integer_Forward = Basis_Assert_False_Transfer_Functions.Integer_Forward
  let intro_integer () = assert false
end

module Complete_Integer(B:Integer_Basis):All_Basis
  with type integer = B.integer
   and type boolean = B.boolean
= struct
  let name = "Complete_Integer(" ^ B.name ^ ")"
  include Basis_sig.Dummy_Conversions
  include B
  module Memory_Lattice = Lattices.Unit
  type memory = Memory_Lattice.t
  module Memory_Backward = Basis_noop_transfer_functions.Memory_Backward
  module Memory_Forward = Basis_noop_transfer_functions.Memory_Forward

  module Block_Lattice = Lattices.Unit
  type block = Memory_Lattice.t
  module Block_Backward = Basis_noop_transfer_functions.Block_Backward
  module Block_Forward = Basis_noop_transfer_functions.Block_Forward

  module Binary_Lattice = struct
    include Lattices.Unit
    let includes ~size = assert false
    let is_bottom ~size = assert false
    let bottom ~size = assert false
    let top ~size = assert false    
    let inter ~size = assert false
    let join ~size = assert false      
    let pretty ~size = assert false
    let widen ~size ~previous = assert false
    let includes_or_widen ~size ~previous = assert false
    let singleton ~size = assert false
  end
  type binary = Binary_Lattice.t
  module Binary_Backward = Basis_Assert_False_Transfer_Functions.Binary_Backward
  module Binary_Forward = Basis_Assert_False_Transfer_Functions.Binary_Forward
  let binary_to_ival ~signed ~size _ = assert false
  let binary_to_known_bits ~size _ = assert false    
  let binary_is_empty ~size _ = assert false    
  let binary_fold_crop ~size bin ~inf ~sup f acc = assert false
  let binary_is_singleton ~size _ = assert false

end

module Dummy_All_Basis:All_Basis = struct
  
  let name = "Dummy"

  include Quadrivalent_basis

  module Integer_Lattice = struct
    include Lattices.Unit (* TODO: Should be in Binary_Basis. *)
    let singleton _ = assert false
  end
  type integer = Integer_Lattice.t
  module Integer_Backward = Basis_Assert_False_Transfer_Functions.Integer_Backward
  module Integer_Forward = Basis_Assert_False_Transfer_Functions.Integer_Forward
  let is_singleton_int _ = assert false
  let convert_to_ival _ = assert false

  module Binary_Lattice = struct
    include Lattices.Unit
    let includes ~size = assert false
    let is_bottom ~size = assert false
    let bottom ~size = assert false
    let top ~size = assert false
    let inter ~size = assert false
    let join ~size = assert false                    
    let pretty ~size = assert false
    let widen ~size ~previous = assert false
    let includes_or_widen ~size ~previous = assert false
    let singleton ~size = assert false
  end 

  
  type binary = Binary_Lattice.t
  module Binary_Backward = Basis_Assert_False_Transfer_Functions.Binary_Backward
  module Binary_Forward = Basis_Assert_False_Transfer_Functions.Binary_Forward
  let binary_to_ival ~signed ~size _ = assert false
  let binary_to_known_bits ~size _ = assert false    
  let binary_is_empty ~size _ = assert false    
  let binary_fold_crop ~size bin ~inf ~sup f acc = assert false    
  let binary_is_singleton ~size _ = assert false

  module Memory_Lattice = Lattices.Unit
  type memory = Memory_Lattice.t
  module Memory_Backward = Basis_noop_transfer_functions.Memory_Backward
  module Memory_Forward = Basis_noop_transfer_functions.Memory_Forward

  module Block_Lattice = Lattices.Unit
  type block = Memory_Lattice.t
  module Block_Backward = Basis_noop_transfer_functions.Block_Backward
  module Block_Forward = Basis_noop_transfer_functions.Block_Forward

end
