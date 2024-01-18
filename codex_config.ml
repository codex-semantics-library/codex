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

(* This file defines a set of static and dynamic parameters. 

   Static parameters, if muted, are muted very early: this file should
   be the first executed, and it depends on nothing else. After that
   the configuration does not change: for the rest of the application,
   the parameters are considered as being static.

   This simplifies adding configuration options to the rest of the
   code; for instance it generally allows using local modules instead
   of functors. Also, we can use a purely static configuration here,
   to generate an optimized version of Codex.

   Dynamic parameters are supposed to be changed dynamically, at any
   time (e.g. during an interactive session). *)

(* All parameters are function from unit to their result, in case they
   become dynamic later. *)


(** Domains options *)

let r_ptr_size = ref @@ -333;;
let set_ptr_size x = r_ptr_size := x;;
let ptr_size () = !r_ptr_size;;

(* Dummy size when size of functions is required (e.g. allocation of a dummy base). *)
let function_size () = 0;;


(* If true, record the parents of a variable (i.e. set of variables
   whose defs immediately depends on the variable). Necessary for
   re-forward constraint propagation. *)
let constraints_register_parents() = false;;

(* Do we consider array elements individually, or do we squash all
   cells together. *)
let array_expansion(): [`full_expansion|`full_squashing] = `full_expansion;;

(** Constraints generation options.  *)

(* When true, we put an assume whenever there is an alarm. This makes
   the analysis more precise, but also slower; especially the set of
   conditions on which we depend (represented as a BDD) can become
   much larger. *)
let assume_alarms() = true;;

(* Translate binary constraints to integer constraints. Sound only if there is no
   signed nor unsigned overflow, but works much better than bitvector reasoning. *)
let translation_to_smt_use_integer () = true

(** Debugging options *)

(* When dumping a term to a SMT solver, dump the input to the SMT solver. *)
let print_smt_input() = false (* true *)(* false *)

(** Goal-oriented options *)

(* Should we try to prove unproved alarms using goal-oriented procedures. *)
let try_hard_on_alarms() = false;;

(* Should we try to prove unproved user assertions using goal-oriented procedures. *)
let try_hard_on_assertions() = true;;

(* Should goal-oriented procedures attempt to perform deductive verification? *)
let try_hard_with_deductive_verification() = true

(* Should goal-oriented procedures attempt to perform symbolic execution? *)
let try_hard_with_symbolic_execution() = true

(* Should goal-oriented procedures attempt to perform software model checking with muz? *)
let try_hard_with_muz() = true

(* Which muz engine to use. Valid values include clp for symbolic
   execution, and pdr for property-directed reachability. *)
(* Now it has spacer too. *)
let muz_engine() = "pdr"
(* let muz_engine() = "clp" *)
let muz_engine() = "spacer"

(* Whether to also check assertions that have been proved by abstract
   interpretation with the goal-oriented procedures. This is mainly
   used for debugging. *)
let try_hard_double_check() = false

(* Number of seconds before the SMT solver times out. *)
let smt_timeout() = 10;;


(* None means: cannot write to an absolute address (default for C). *)
let r_valid_absolute_addresses:(Z.t * Z.t) option ref = ref None;;
let set_valid_absolute_addresses (min,max) =
  assert(Z.geq min Z.zero);
  if(Z.equal min Z.zero) then Printf.eprintf "Warning: zero (nullptr) considered a valid address\n";
  (assert (Z.geq max Z.one));
  r_valid_absolute_addresses := Some (min,max);;
let valid_absolute_addresses() = !r_valid_absolute_addresses;;

let r_show_memory_on_exit = ref true
let show_memory_on_exit() = !r_show_memory_on_exit
let set_show_memory_on_exit bool = r_show_memory_on_exit := bool
let _ = at_exit (fun () ->
    if show_memory_on_exit() then
      let minor,promoted,major = Gc.counters() in
      let allocated = minor +. major -. promoted in
      let l1 = String.length (Printf.sprintf "%.0f" allocated) in
      Printf.eprintf "minor_words:     %*.0f\n\
                      major_words:     %*.0f\n\
                      promoted_words:  %*.0f\n\
                      total_allocated: %.0f\n"
        l1 minor l1 major l1 promoted allocated
  )
;;

let r_assume_simple_asts = ref true
let set_assume_simple_asts b = r_assume_simple_asts := b
let assume_simple_asts () = !r_assume_simple_asts


(* let r_widen = ref true
 * let set_widen b = r_widen := b *)
(* let widen () = !r_widen *)
(* If false, do not perform widening, only joins. Note that
   convergence will be slow on most programs. *)
let widen() = true

(* Should malloc be handled only using the weak type domain. *)
let handle_malloc_as_unknown_typed_pointers() = (* false *)true

(* If false, the behaviour is more predictable (e.g. garbage
   collection cannot interfere, so ids are better used).
   Moreover, it seems that this improves performances (probably because
   more constraints are reused).
   MAYBE: Use "ancient" and move these constraints there. 
   This would also provide a unique id, using address_of.
 *)
let hash_cons_constraints_with_weak_table() = false
