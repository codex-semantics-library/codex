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

(* Tracelog: captures Trace and logs, but also collect data for later
   visualization, all at a low performance and development cost. *)

(** A function that behaves like printf. *)
type 'a printf = ('a, Format.formatter, unit) format -> 'a

(** A log message takes a printf function and print with it. This
    avoid the need to parse and evaluate arguments when we do not
    log.  *)
type 'a log =  'a printf -> unit;;

(** Verbosity level, from less to most verbose. Default is to display
    everything below `Warning. Feedback levels are from less to more
    verbose (normal levels are from 0 to 2, but you can pass bigger numbers.)   *)
val set_verbosity_level: [`Result | `Error | `Warning | `Feedback of int | `Trace | `Debug ] -> unit


module Make(Category:sig val category:string end):
sig

  (** Printing from most to least important. *)
  val result : 'a log -> unit
  val error : 'a log -> unit
  val warning : 'a log -> unit
  val feedback : int -> 'a log -> unit
  val debug : 'a log -> unit


  (** Classic error messages. *)
  val fatal : 'a log -> 'b
  val not_yet_implemented : 'a log -> unit

  (** Tracing functions. *)

  (** [trace log pp_ret f] first display log, then executes f and
      prints its return value using pp_ret. *)
  val trace: 'a log -> (Format.formatter -> 'b -> unit) -> (unit -> 'b) -> 'b

end

(* TODO:
   - Other log outputs: HTML output, Emacs (using treemacs) output.

   - Organize categories using a trie (maybe the result of Make should
     have a Make functor itself)

   - Extend tracing with location information of the source code that we analyze.

   - Extend tracing with trace event identifiers and use that to
     record tables (or just sequence of log events) attaching a value to
     (object,traceid) pairs. We can easily generate a unique id with a
     tree structure for trace events, and we should be able to use that
     display the values attached to some objects only for selected
     traces.

   - Remember the trace event identifier corresponding to the creation
     of some objects, like symbolic values, to easily find the context
     where an object is created.

   - Allows logging large objects in a piecewise way; e.g. for memory
     maps, abstract elements (mapping symbolic values to singe value
     abstraction), the dependencies of symbolic expressions, etc.
     There could be one log per such objects, where we could optionally
     print it in full when requested. *)

