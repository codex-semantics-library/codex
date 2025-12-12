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

(** Tracelog is the main logging module for codex.

    Instanciate a new logger with {!Tracelog.Make}:
    {@ocaml[
      module Log = Tracelog.Make(struct let category = "MyCategory" end)
    ]}
    You can then use logging functions
    {{!S.error}[Log.error]}, {{!S.warning}[Log.warning]}, {{!S.notice}[Log.notice]}, {{!S.info}[Log.info]},
    and {{!S.error}[Log.debug]} (in increasing verbosity level). Additionally,
    {{!S.fatal}[Log.fatal]} prints and exits. Finally, {{!S.trace}[Log.trace]}
    can be used to trace entry/return into a function.

    For a short tutorial, see {!page-"While tutorial - chapter 1".tracelog}.

    You can change
    the global level using {!set_verbosity_level} (set to [`Notice] by default).
*)

(* Tracelog: captures Trace and logs, but also collect data for later
   visualization, all at a low performance and development cost. *)

(** A function that behaves like printf. *)
type 'a printf = ('a, Format.formatter, unit) format -> 'a

(** A log message takes a printf function and print with it. This
    avoid the need to parse and evaluate arguments when we do not
    log.  *)
type 'a log =  'a printf -> unit
val set_log_binary_trace: bool -> unit

(** Verbosity level, from less to most verbose. Default is to display
    everything below `Warning. Feedback levels are from less to more
    verbose (normal levels are from 0 to 2, but you can pass bigger numbers.)   *)
val set_verbosity_level: [`Error | `Warning | `Notice | `Info | `Debug ] -> unit
val get_verbosity_level: unit -> [`Error | `Warning | `Notice | `Info | `Debug ]

type location = ..

(**reset the location stack, USE ONLY IF YOU KNOW WHAT YOU ARE DOING*)
val reset_location_stack : unit -> unit

(** The innermost location set by trace.  *)
val current_location: unit -> location option

(** The current stack of locations set by trace.  *)
val current_location_stack: unit -> location list


(** If we have location information, it will be printed using this function. *)
val set_pp_location_stack: (Format.formatter -> location list -> unit) -> unit


module type S = sig

  (** Printing from most to least important. [info] and [debug] levels
      are not printed by default. *)
  val error : 'a log -> unit
  val warning : 'a log -> unit
  val notice : 'a log -> unit
  val info : 'a log -> unit
  val debug : 'a log -> unit


  (** Classic error messages.  Fatal will raise the [Fatal] exception. *)
  val fatal : 'a log -> 'b
  val not_yet_implemented : 'a log -> unit

  (* Tracing functions. *)
  (** [trace log pp_ret f] first display log, then executes f and
      prints its return value using pp_ret. *)
  val trace: 'a log -> ?loc:location  -> ?bintrace:'c Syntax_tree.Location_identifier.t -> ?pp_ret:(Format.formatter -> 'b -> unit) -> (unit -> 'b) -> 'b


  (** [fatal_handler] Fatal handler, will properly collect the backtrace for [fatal] and print it.
      It also collects other uncaught exception and returns an error message. *)
  val fatal_handler: ('a -> 'b) -> ('a -> 'b)

end

(** Standard functor: verbosity level is controlled by [set_verbosity_level]. *)
module Make(Category:sig val category:string end):S

(** This is useful when developping and debugging a specific module:
    we can enable debugging forthis module by replacing the [Make]
    call by [MakeDebug]. *)
module MakeDebug(Category:sig val category:string end):S

(** Completely silence this debug output. *)
module MakeSilent(Category:sig val category:string end):S


(**Outputs nothing, usefull for developpers who want to hide a domain by simply replacing the usual [Log = Make(...)] by [Log = Dummy]*)
module Dummy:S
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
