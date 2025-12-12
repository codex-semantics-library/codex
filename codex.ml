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

(** XXX: Entry poitn to our documentation? *)


module Operator = Operator
module Fixpoint = Fixpoint
module Codex_config = Codex_config
module Codex_log = Codex_log
module Hook = Hook
module Types = Types
module Lattices = Lattices
module Single_value_abstraction = Single_value_abstraction
module Utils =  struct
  module Interval_map = Interval_map
  module Datatype_sig = Datatype_sig
end

module Ext = struct
  module Framac_ival = Framac_ival (* Temporary *)
end

module Domains = Domains

module Extstdlib = Extstdlib
module Gui = Gui
