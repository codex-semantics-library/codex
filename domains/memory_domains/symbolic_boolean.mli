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

module type AbstractBoolean = sig
  module Scalar : Sig.BASE

  type boolean

  val compare : boolean -> boolean -> int
  val pretty : Format.formatter -> boolean -> unit
  val not : Scalar.Context.t -> boolean -> boolean
  val query_boolean : Scalar.Context.t -> boolean -> Sig.Quadrivalent.t
  val assume : Scalar.Context.t -> boolean -> Scalar.Context.t option
  val boolean_empty : Scalar.Context.t -> boolean
  val boolean_unknown : Scalar.Context.t -> boolean
  val satisfiable : Scalar.Context.t -> boolean -> Smtbackend.Smtlib_sig.sat

  val serialize_boolean :
    Scalar.Context.t ->
    boolean ->
    Scalar.Context.t ->
    boolean ->
    'a Scalar.Context.in_acc ->
    (boolean, 'a) Scalar.Context.result
end

module Make (B : AbstractBoolean) : sig
  module Scalar := B.Scalar
  include Sig.With_Boolean with module Context = B.Scalar.Context

  val lift : B.boolean -> boolean
  val assume : Context.t -> boolean -> Context.t option
  val satisfiable : Context.t -> boolean -> Smtbackend.Smtlib_sig.sat
end
