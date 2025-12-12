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

(** This file registers in the Frama-C kernel the options for Frama-C/Codex.
    If needed, see the Frama-C plugin development guide for a description. *)

open Frama_c_kernel
let help_msg = "perform static analysis using Frama-C/Codex."

include Plugin.Register
  (struct
    let name = "codex"
    let shortname = "codex"
    let help = help_msg
   end)
;;

(* enabling/disabling plugin *)
module Enabled =
  False
    (struct
       let option_name = "-codex"
       let help = "when on (off by default), " ^ help_msg
     end)

module TypeConfigurationFile =
  String
    (struct
      let option_name = "-codex-type-file"
      let arg_name = "file"
      let default = ""
      let help = "Path to the file containing the type definitions"
    end)

module HtmlDump =
  False
    (struct
      let option_name = "-codex-html-dump"
      let help = "when on (off by default), dumps an html file with the results of the analysis of the function"
    end)

module ExpDump =
  False
    (struct
      let option_name = "-codex-exp-dump"
      let help = "If set, dumps the value of each expression to <prefix><funname> file."
    end)

module OutputPrefix =
  String
    (struct
      let option_name = "-codex-output-prefix"
      let arg_name = "<prefix>"
      let default = ""
      let help = "when set, add a prefix to the name of cdump and html files that are dumped."
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
      let arg_name = "file"
      let default = ""
      let help = "when on (off by default), dumps a comparison of values found of each expression \
to a file, with the results of value"
    end)

let needs_exp_term_mapping() =
  HtmlDump.get() || ExpDump.is_set() || CompareWithValue.get() <> ""


type svcomp_property =
  | No_overflow
  | Valid_memsafety
  | Valid_memcleanup
  | Termination
  | Def_behavior
  | No_data_race
  | Unreach_call of {suffix:string option}
  | Coverage_error_call
  | Coverage_branches
  | Coverage_statements
  | Coverage_conditions

let pp_svcomp_property fmt = function
  | No_overflow -> Format.fprintf fmt "No_overflow"
  | Valid_memsafety -> Format.fprintf fmt "Valid_memsafety"
  | Valid_memcleanup -> Format.fprintf fmt "Valid_memcleanup"
  | Termination -> Format.fprintf fmt "Termination"
  | Def_behavior -> Format.fprintf fmt "Def_behavior"
  | No_data_race -> Format.fprintf fmt "No_data_race"
  | Unreach_call _ -> Format.fprintf fmt "Unreach_call"
  | Coverage_error_call -> Format.fprintf fmt "Coverage_error_call"
  | Coverage_branches -> Format.fprintf fmt "Coverage_branches"
  | Coverage_statements -> Format.fprintf fmt "Coverage_statements"
  | Coverage_conditions -> Format.fprintf fmt "Coverage_conditions"

module SVComp_expected_properties =
  struct
    include String_map(struct
        include Datatype.Bool
        type key = string
        let to_string  i = string_of_bool i

        let of_string s = bool_of_string s
      end)(struct
        let default = Datatype.String.Map.empty
        let option_name = "-codex-svcomp-expected-properties"
        let arg_name =""
        let help = "mapping from keys in {unreach_call,no_overflow} to {true,false}, where e.g. no_overflow=true means that we should be proving that the program has no overflow."
      end)

    let property_of_string =
      let module String = Stdlib.String in function
      | "no-overflow" -> No_overflow
      | "valid-memsafety" -> Valid_memsafety
      | "valid-memcleanup" -> Valid_memcleanup
      | "termination" -> Termination
      | "def-behavior" -> Def_behavior
      | "no-data-race" -> No_data_race
      | "unreach-call" -> Unreach_call {suffix=None}
      | "coverage-error-call" -> Coverage_error_call
      | "coverage-branches" -> Coverage_branches
      | "coverage-statements" -> Coverage_statements
      | "coverage-conditions" -> Coverage_conditions
      | str when String.starts_with ~prefix:"unreach-call-" str ->
        let idx = (String.length "unreach-call-") in
        let suffix = String.sub str idx (String.length str - idx) in
        Unreach_call{suffix=Some suffix}
      | str -> failwith (Printf.sprintf "Error: Unrecognized svcomp property `%s' in option %s\n" str name)


    let get() =
      get()
      |> Frama_c_kernel.Datatype.String.Map.bindings
      |> List.map (fun (key,value) -> property_of_string key,value)

  end


module Print_Force_Lazyness =
  False
    (struct
       let option_name = "-codex-print-force-lazyness"
       let help = "when on (off by default), print can force an evaluation that would otherwise be lazy"
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

module BitwiseDomain =
  False
    (struct
      let option_name = "-codex-use-bitwise-domain"
      let help = "when on (off by default), use the bitwise domain"
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

module TryHard =
  True
    (struct
        let option_name = "-codex-try-hard"
        let help = "when on (on by default), try to prove unproved user assertions using goal-oriented procedures (such as SMT)"
      end)

module AnalyzeFunctions =
    String_list
      (struct
        let option_name = "-codex-analyze-functions"
        let arg_name = "func-names"
        let help = "analyze all the functions specified; if no args are specified, all functions in the ctype spec will be analyzed"
          end)

module InferTypes =
    False
      (struct
        let option_name= "-codex-infer-spec"
        let help = "EXPERIMENTAL: tries to refine the type specification by running the analysis several times"
          end)

module UseUnionFind = True(struct
  let option_name = "-codex-use-union-find"
  let help = "Use the union-find domain"
end)

module ExtraMetrics = False(struct
  let option_name = "-codex-extra-metrics"
  let help = "Print union-find related metrics at the end of the analysis"
end)

module GcStats = False(struct
  let option_name = "-codex-gc-stats"
  let help = "Print GC stats"
end)

module BackPropagationLimit = Int(struct
  let option_name = "-codex-backpropagations"
  let arg_name = "N"
  let help = "Maximum number of backpropagations to perform"
  let default = Codex_config.get_back_propagation_limit ()
end)

module VariableDisplay = struct
  include String(struct
    let option_name = "-codex-variable-display"
    let arg_name = "MODE"
    let help = "How codex should display variable information. \
          \ One of 'value' (only print inferred value, default)\
          \ 'symbolic' (print associated symbolic term)\
          \ 'both' (print both value and symbolic term)\
          \ 'relation' (print value, symbolic term, and union-find relation associated with this term)."
    let default = "value"
  end)
  let get () =
    let open Domains.Term_domain in
    match get () with
    | "value" -> Value
    | "symbolic" -> Symbolic
    | "both" -> Both
    | "relation" -> Relation
    | _ -> assert false
end
let () = VariableDisplay.set_possible_values ["value"; "symbolic"; "both"; "relation"]


let performance = register_category "performance";;
let performance_warning x = feedback ~dkey:performance x;;

let emitter =
  Emitter.create
    "Codex"
    [ Emitter.Property_status; Emitter.Alarm ]
    ~correctness:[]
    ~tuning:[]


(** TODO: We should have a "callstack" or "Location" module.

    With functions to retrieve the current callstack, current kinstr
    etc. which would be centralized.

    And maybe functions like inside_function, inside_kinstr etc.

    Internally this would rely on Tracelogs loc, but we would not see
    it from the outside.

    Maybe we could check internal invariants, such as we cannot be
    inside a kinstr inside another kinstr etc, on construction.

    The driving design of this module: we use OCaml's interpreter
    structure of calls and return to manage a (global, or per-domain)
    interpreter callstack.

    Note: maybe assume, the current context and the control flow graph
    could be handled this way? Indeed, I could probably retrieve the
    CFG using this infrastructure. Not sure that it is a good idea though.


*)
module Location = struct

  module L = struct
    include (Tracelog:sig type location = Tracelog.location = .. end)
    type location +=
      | Function of Kernel_function.t  (* Inside a function. *)
      | Kinstr of Cil_types.kinstr     (* An instruction, guard, or return statement. *)
      | JoinPoint of Cil_types.kinstr * Cil_types.kinstr  (* Should we have the predecessors too? *)
      | FunctionReturn                 (* When returning from a function call. *)
      | Expression of Cil_types.exp    (* Inside an expression. *)
    type t = location

    (* let equal a b = *)
    (*   if a == b then true *)
    (*   else match a,b with *)
    (*   | Function kfa, Function kfb -> Kernel_function.equal kfa kfb *)
    (*   | Instruction ia, Instruction ib -> Cil_datatype.Instr.equal ia ib *)
    (*   | JoinPoint kia, JoinPoint kib -> Cil_datatype.Kinstr.equal kia kib *)
    (*   | Expression a, Expression b -> assert false *)
    (*   | _, _ -> assert false *)
  end

  include L

  let pp_loc fmt location = match location with
    | Function kf -> Format.fprintf fmt "In function `%a':@ " Kernel_function.pretty kf
    | Kinstr i -> Format.fprintf fmt "In instruction `%a':@ " Cil_datatype.Kinstr.pretty i
    | FunctionReturn -> Format.fprintf fmt "When returning from function:@ "
    | Expression e -> Format.fprintf fmt "In expression `%a':@ " Cil_datatype.Exp.pretty e
    | JoinPoint(ka,kb) -> Format.fprintf fmt "At join point between `%a' and `%a':@ " Cil_datatype.Kinstr.pretty ka Cil_datatype.Kinstr.pretty kb
    | _ -> assert false

  let pp_loc_stack fmt stack = match stack with
    | [] -> () (* Format.fprintf fmt "At <toplevel>" *)
    | _ -> Format.fprintf fmt "@[<hv>"; List.iter (pp_loc fmt) (List.rev stack); Format.fprintf fmt "@]"

  let () = Tracelog.set_pp_location_stack pp_loc_stack
end
(* Cil_datatype.Instr.loc will be useful. *)
