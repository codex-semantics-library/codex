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

open Codex.Domains.Memory_domains

(** Setup the analysis for a given abstract domain. *)
module Analyze(Domain:With_focusing.S_with_types): sig

  (** [run f] Do a complete analysis starting from entry point
      [f]. The function can be called multiple times, in which case
      the results are accumulated. Thereafter, each call to [run] is
      called  " an analysis". *)
  val run: Frama_c_kernel.Cil_types.kernel_function -> unit

  (** Dump the list of all collected alarms in all the analyses to
      [out_channel]. *)
  val dump_alarms : out_channel -> unit

  (** Dump the status of every assertion in all the analyses to
      [out_channel]. *)
  val dump_assertions : out_channel -> unit

  (** Dump a mapping from each expression of the programm to its value
      to [out_channel]. *)
  val dump_exp_table : out_channel -> unit

  (** Dump additional statistics collected on all analyses to
      out_channel. *)
  val dump_extra : out_channel -> unit

  (** Dump GC statistics collected during all analyses on
      out_channel. *)
  val dump_gc : out_channel -> unit

  (** [print_html filename] prints the results as an HTML file. *)
  val print_html: string -> unit

  (** [print_webapp filename] prints the results as an interactive
      HTML file. *)  
  val print_webapp: string -> unit
end
