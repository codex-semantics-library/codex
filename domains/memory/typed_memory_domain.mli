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

module Ctypes = Types.Ctypes

module type S = sig
  module Sub : Memory_sig.Memory_domain
  (* module TS : Ctypes.Type_settings *)

  include Memory_sig.Memory_domain with module Address.Context = Sub.Address.Context

  type binary = Sub.Address.binary
  
  (* val global_symbol : Scalar.Context.t -> string -> int * Scalar.binary
  val add_global_symbol : size:int -> Scalar.Context.t -> string -> Scalar.binary -> unit
  
  val check_invariant : size:int -> Scalar.Context.t -> Ctypes.Pred.t -> binary -> bool *)
end

module Make
    (Scalar:Domain_sig.Base)
    (Sub:Memory_sig.Memory_domain with module Address.Context = Scalar.Context
                                   and module Address.Scalar = Scalar)
: S with module Sub = Sub
