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

(** [Operator] defines the syntax and semantics of the
    expressions of the internal languages used by the Codex
    analyzers. It defines operations such as addition (on bitvectors
    or integers), logical/bitwise and (on booleans, integers and bitvectors), etc. In
    addition, it contains several utility modules (such as
    pretty-printers) for code dealing with these operators.

    Most of our passes do not use a term representation of an AST, but
    instead calls "constructor functions" manipulating expressions,
    similarly to the {{: https://dx.doi.org/10.1017/s0956796809007205}{e Tagless-final} of Carette, Kiselyov and Shan (2009)}.
    Thus, syntax means here that we define signatures (see {!syntax}) .

    We do also provide a tag that can be used in an AST representation of
    the language (module {!Function_symbol}).

    We define a concrete semantics of these operators in
    module {!Concrete}, which can be used to interpret constant terms.

    Finally, {!Conversions} contain helpers when doing domain
    transformations. *)

(** {1 Unique identifiers } *)

include Operator_ids

(** {1 Alarms } *)

(** In the concrete, an alarm would correspond to an exception/panic
    due to a partial operator.

    In the abstract, it corresponds to a (possible) error reported to the user. *)
module Alarm = Operator_alarm


module Flags = Flags

(** {1:syntax Syntax: signature of operators } *)

module Sig = Operator_sig
include Sig

(** {1 Concrete (reference) implementation giving a meaning to operators } *)

module Concrete = Operator_concrete

(** {1 Function symbols}  *)

module Function_symbol = Function_symbol

(** {1 Conversions}  *)

module Conversions = Operator_conversions

(** {1 Automatic logging}  *)

(** Similar to conversion, converts transfer functions to the same
    thing but that logs its call. *)

module Autolog = Operator_autolog
