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

open Lattice_sig


(* Dummy version of the lattices to help get started.

   We use generator to help understand where the dummy value was
   implemented. *)

let not_implemented loc func = failwith ("In module instantiated " ^ loc ^ ":\nfunction " ^ func ^ " not implemented");;

module Enum_Lattice(UnimplementedId:sig val loc:string end):ENUM_LATTICE = struct
    include Unit_Lattice
    let top ~size = not_implemented UnimplementedId.loc __FUNCTION__
    let is_singleton _ = not_implemented UnimplementedId.loc __FUNCTION__
    let fold_on_cases _ = not_implemented UnimplementedId.loc __FUNCTION__
end


module Bitvector_Lattice(UnimplementedId:sig
    type t
    val loc:string
  end):BITVECTOR_LATTICE with type t = UnimplementedId.t = struct
  type t = UnimplementedId.t
  let equal _ = not_implemented UnimplementedId.loc __FUNCTION__
  let compare _ = not_implemented UnimplementedId.loc __FUNCTION__
  let hash _ = not_implemented UnimplementedId.loc __FUNCTION__      
  let is_singleton ~size _x = not_implemented UnimplementedId.loc __FUNCTION__
  let is_empty ~size _x = not_implemented UnimplementedId.loc __FUNCTION__
  let fold_crop_signed ~size _x ~inf ~sup _acc _f  = not_implemented UnimplementedId.loc __FUNCTION__
  let fold_crop_unsigned ~size _x ~inf ~sup _acc _f  = not_implemented UnimplementedId.loc __FUNCTION__    
  let is_bottom ~size = not_implemented UnimplementedId.loc __FUNCTION__
  let bottom ~size = not_implemented UnimplementedId.loc __FUNCTION__
  let includes ~size = not_implemented UnimplementedId.loc __FUNCTION__
  let top ~size = not_implemented UnimplementedId.loc __FUNCTION__
  let inter ~size = not_implemented UnimplementedId.loc __FUNCTION__
  let join ~size = not_implemented UnimplementedId.loc __FUNCTION__
  let pretty ~size = not_implemented UnimplementedId.loc __FUNCTION__
  let widen ~size ~previous _ = not_implemented UnimplementedId.loc __FUNCTION__
  let includes_or_widen ~size ~previous _ = not_implemented UnimplementedId.loc __FUNCTION__
  let singleton ~size = not_implemented UnimplementedId.loc __FUNCTION__
  let to_known_bits ~size = not_implemented UnimplementedId.loc __FUNCTION__
  let to_unsigned_interval ~size = not_implemented UnimplementedId.loc __FUNCTION__
  let to_signed_interval ~size = not_implemented UnimplementedId.loc __FUNCTION__      
end

module Integer_Lattice(UnimplementedId:sig
    type t
    val loc:string
  end):INTEGER_LATTICE with type t = UnimplementedId.t = struct
  type t = UnimplementedId.t
  let equal _ = not_implemented UnimplementedId.loc __FUNCTION__
  let compare _ = not_implemented UnimplementedId.loc __FUNCTION__
  let hash _ = not_implemented UnimplementedId.loc __FUNCTION__
  let pretty _ = not_implemented UnimplementedId.loc __FUNCTION__
  let join _ = not_implemented UnimplementedId.loc __FUNCTION__
  let includes_or_widen ~previous = not_implemented UnimplementedId.loc __FUNCTION__
  let includes _ = not_implemented UnimplementedId.loc __FUNCTION__
  let widen ~previous = not_implemented UnimplementedId.loc __FUNCTION__
  let inter _ = not_implemented UnimplementedId.loc __FUNCTION__
  let bottom _ = not_implemented UnimplementedId.loc __FUNCTION__
  let is_bottom _ = not_implemented UnimplementedId.loc __FUNCTION__
  let top _ = not_implemented UnimplementedId.loc __FUNCTION__
  let singleton _ = not_implemented UnimplementedId.loc __FUNCTION__
  let is_singleton _ = not_implemented UnimplementedId.loc __FUNCTION__
  let fold_crop _ = not_implemented UnimplementedId.loc __FUNCTION__      
end
