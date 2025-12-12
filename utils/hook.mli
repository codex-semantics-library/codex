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

(** A hook is a place where functions are registered. These functions
    are called when some events happen. This is useful to execute
    imperative actions such as loading/storing data to files, opening
    or cleaning resources, etc. *)

type 'a hook
(** A hook containing functions called with a 'a argument (often unit).  *)

val add_hook: 'a hook -> name:string -> ('a -> unit) -> unit
(** Add a function to be called when the hook is run.  The name is used
    for debugging purposes. *)

val run_hook: 'a hook -> 'a -> unit
(** Execute all functions in the hook, with a given argument. *)

(* MAYBE: special hooks, that also return arguments. *)

(* val before_init: unit hook *)
(* A hook that runs once, before Codex is initialized. *)
(* val after_init: unit hook *)
(* A hook that runs once, after Codex is initialized. *)

val startup: unit hook
(** This hook runs once, after the command line has been parsed,
    essential utilities (e.g. logging) is setup, and the library has
    been initialized. *)

val after_domain_build: unit hook
(** This hook runs once, after the domain for the analysis has been built *)

(* Maybe: hooks before/after an analysis, e.g. for cleaning hash
   tables and resetting counters. But, it may be better to
   re-instantiate a functor for this. *)

val exit: unit hook
(** This hook runs once when the program linked with Codex exits. It
    allows cleaning up resources, saving files etc. This hook is
    already registered to run. *)

(* TODO: advices, i.e. functions that run before, after, or "around"
   some events (corresponding to other functions), but with access to
   the argument and return value. Could be useful when
   entering/exiting an analysis for instance. *)
