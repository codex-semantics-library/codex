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

(* include Libase *)
module Domain_sig = Domains.Domain_sig
module With_focusing = Domains.With_focusing
module Domains_constraints_constraint_propagation = Domains.Domains_constraints_constraint_propagation
module Type_domain = Domains.Type_domain
module Region_numeric_offset = Domains.Region_numeric_offset
module Constraint_domain2 = Domains.Constraint_domain2
module Bitwise_domain = Domains.Bitwise_domain
module Memory_domain = Domains.Memory_domain
module Wholify = Domains.Wholify
module Typed_memory_domain = Domains.Typed_memory_domain
module Region_separation = Domains.Region_separation
module Region_suffix_tree = Domains.Region_suffix_tree
module Fully_expanded_finite_region = Domains.Fully_expanded_finite_region

module Codex_log = Codex_log  
module Datatype_sig = Datatype_sig

module Transfer_functions = Transfer_functions
module Memory_sig = Domains.Memory_sig
module Framac_ival = Framac_ival
module Extstdlib = Extstdlib
module Ival_basis = Single_value_abstraction.Ival
module Codex_config = Codex_config
module Types = Types
