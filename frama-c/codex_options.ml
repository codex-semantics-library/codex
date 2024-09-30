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

open Frama_c_kernel
let help_msg = "analyze using codex"

include Plugin.Register
  (struct
    let name = "codex"
    let shortname = "codex"
    let help = help_msg
   end)
;;

module Domains =
  String_set
    (struct
      let option_name = "-codex-domains"
      let arg_name = "domain_number"
      let help = "Selects one of the statically built abstract domains"
     end)
;;


(* We declare it to Frama-C here, but it is read specially,
   because we need its value in the top-level of the modules. *)
module ConfigFile = String(struct
  let option_name = "-codex-config-file"
  let arg_name = "<json file>"
  let default = ""
  let help = "Static configuration options for codex"
end)


(* enabling/disabling plugin *)
module Enabled =
  False
    (struct
       let option_name = "-codex"
       let help = "when on (off by default), " ^ help_msg
     end)

module Print =
  False
    (struct
       let option_name = "-codex-print"
       let help = "when on (off by default), print the return term of the main function"
     end)


module PrintValue =
  True
    (struct
       let option_name = "-codex-print-value"
       let help = "when on (on by default), print values at the end of the main function"
     end)

module TypeConfigurationFile =
  String
    (struct
      let option_name = "-codex-type-file"
      let arg_name = "<file>"
      let default = ""
      let help = "Path to the file containing the type definitions"
    end)

module HtmlDump =
  String
    (struct
      let option_name = "-codex-html-dump"
      let arg_name = "<file>"
      let default = ""
      let help = "when on (off by default), dumps an html file with the results of the analysis"
    end)

module ExpDump =
  String
    (struct
      let option_name = "-codex-exp-dump"
      let arg_name = "<file>"
      let default = ""
      let help = "when on (off by default), dumps the value of each expression to a file"
    end)

module UseTypeDomain =
  False
    (struct
      let option_name = "-codex-use-type-domain"
      let help = "when on (off by default), use the type domain (experimental)"
    end)

(* Useful to check the correctness of the results; especially when the
   test can be full unrolled with value, which then provides an upper
   bound on the atteignable precision. *)
module CompareWithValue =
  String
    (struct
      let option_name = "-codex-compare-with-value"
      let arg_name = "<file>"
      let default = ""
      let help = "when on (off by default), dumps a comparison of values found of each expression\
 to a file, with the results of value"
    end)

let needs_exp_term_mapping() =
  HtmlDump.get() <> "" || ExpDump.get() <> "" || CompareWithValue.get() <> ""

module Print_Force_Lazyness =
  False
    (struct
       let option_name = "-codex-print-force-lazyness"
       let help = "when on (off by default), print can force an evaluation that would otherwise be lazy"
     end)


module Verbose_Terms =
  Int
    (struct
      let option_name = "-codex-verbose-terms"
      let help = "0 = do not print terms; 1 = print short terms; 2 = fully print terms"
      let default = 0
      let arg_name = "level"
     end)


module UnknownUninitialized =
  False
    (struct
      let option_name = "-codex-unknown-uninitialized"
      let help = "when on (off by default), uninitialized vars are considered as containing an unknown value (instead of no value)"
     end)

module Focusing =
  False
    (struct
      let option_name = "-codex-focusing"
      let help = "activate focusing (off by default)"
    end)

module UseLoopDomain =
  False
    (struct
      let option_name = "-codex-use-loop-domain"
      let help = "when on (off by default), use the loop domain (experimental)"
    end)


module FixpointUseRegexp =
  True
    (struct
      let option_name = "-codex-fixpoint-use-regexp"
      let help = "when on (on by default), use the fixpoint engine based on regular expressions. Otherwise, use WTO."
    end)


module SparseNonRelationalDomain =
  False
    (struct
      let option_name = "-codex-use-sparse-nonrelational-domain"
      let help = "when on (off by default), use the sparse nonrelational domain"
    end)

module SerializeCache =
  False
    (struct
      let option_name = "-codex-serialize-cache"
      let help = "when on (off by default), serializes the cache with the rest of the
          memory, though it can cause problem with SSA contraint domain(experimental)"
    end)

module UseWeakTypes =
  False
    (struct
      let option_name = "-codex-use-weak-types"
      let help = "when on (off by default), malloc uses weak types (experimental)"
    end)

module OverflowAlarms =
  False
    (struct
        let option_name = "-codex-overflow-alarms"
        let help = "when on (off by default), add alarms for signed integer addition/subtraction over/underflow."
      end)


let performance = register_category "performance";;
let performance_warning x = feedback ~dkey:performance x;;

let emitter =
  Emitter.create
    "Codex"
    [ Emitter.Property_status; Emitter.Alarm ]
    ~correctness:[]
    ~tuning:[]


module Location = struct

  include struct
    include (Tracelog:sig type location = Tracelog.location = .. end)
    type location +=
      | Function of Kernel_function.t
      | Instruction of Cil_types.instr
      | JoinPoint of Cil_types.kinstr
      | Expression of Cil_types.exp
  end
  type t = location

  let pp_loc fmt location = match location with
    | Function kf -> Format.fprintf fmt "In function `%a':@ " Kernel_function.pretty kf
    | Instruction i -> Format.fprintf fmt "In instruction `%a':@ " Cil_datatype.Instr.pretty i
    | Expression e -> Format.fprintf fmt "In expression `%a':@ " Cil_datatype.Exp.pretty e
    | _ -> assert false

  let pp_loc_stack fmt stack = match stack with
    | [] -> () (* Format.fprintf fmt "At <toplevel>" *)
    | _ -> Format.fprintf fmt "@[<hv>"; List.iter (pp_loc fmt) (List.rev stack); Format.fprintf fmt "@]"

  let () = Tracelog.set_pp_location_stack pp_loc_stack
end
(* Cil_datatype.Instr.loc will be useful. *)
