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

module Log = Tracelog.Make (struct
  let category = "Symbolic_boolean"
end)

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

module Make (B : AbstractBoolean) = struct
  type boolean =
    | B of B.boolean
    | BAnd of boolean * boolean
    | BOr of boolean * boolean
    | BTrue
    | BFalse

  module Scalar = B.Scalar
  module Context = Scalar.Context

  let lift b = B b

  module Boolean = struct
    type t = boolean

    let rec compare a b =
      match (a, b) with
      | B a, B b -> B.compare a b
      | BFalse, _ -> 1
      | _, BFalse -> -1
      | _, BTrue -> 1
      | BTrue, _ -> -1
      | B _, _ -> -1
      | _, B _ -> 1
      | BAnd (la, ra), BAnd (lb, rb) ->
          let c = compare la lb in
          if c <> 0 then c else compare ra rb
      | BAnd _, _ -> -1
      | _, BAnd _ -> 1
      | BOr (la, ra), BOr (lb, rb) ->
          let c = compare la lb in
          if c <> 0 then c else compare ra rb

    let equal a b = compare a b = 0
    let sdbm x y = y + (x lsl 16) + (x lsl 6) - x
    let hash _ = assert false

    let rec pretty fmt = function
      | B b -> B.pretty fmt b
      | BTrue -> Format.fprintf fmt "True"
      | BFalse -> Format.fprintf fmt "False"
      | BAnd (l, r) -> Format.fprintf fmt "BAnd(@[%a@,@ %a@])" pretty l pretty r
      | BOr (l, r) -> Format.fprintf fmt "BOr(@[%a@,@ %a@])" pretty l pretty r
  end

  module Query = struct
    include Scalar.Query

    let rec boolean ctx = function
      | B b -> B.query_boolean ctx b
      | BTrue -> Single_value_abstraction.Quadrivalent.Boolean_Forward.true_
      | BFalse -> Single_value_abstraction.Quadrivalent.Boolean_Forward.false_
      | BAnd (l, r) ->
          Single_value_abstraction.Quadrivalent.Boolean_Forward.( && )
            (boolean ctx l) (boolean ctx r)
      | BOr (l, r) ->
          Single_value_abstraction.Quadrivalent.Boolean_Forward.( || )
            (boolean ctx l) (boolean ctx r)
  end

  let boolean_empty ctx = B (B.boolean_empty ctx)
  let boolean_unknown ctx = B (B.boolean_unknown ctx)

  let rec boolean_pretty ctx fmt = function
    | B b -> B.pretty fmt b
    | BTrue -> Format.fprintf fmt "SymbTrue"
    | BFalse -> Format.fprintf fmt "SymbFalse"
    | BAnd (b1, b2) ->
        Format.fprintf fmt "%a and %a" (boolean_pretty ctx) b1
          (boolean_pretty ctx) b2
    | BOr (b1, b2) ->
        Format.fprintf fmt "%a or %a" (boolean_pretty ctx) b1
          (boolean_pretty ctx) b2

  let rec serialize_boolean :
      'a.
      Scalar.Context.t ->
      boolean ->
      Scalar.Context.t ->
      boolean ->
      bool * 'a Scalar.Context.in_tuple ->
      (boolean, 'a) Scalar.Context.result =
   fun ctxa a ctxb b (inc, tup) ->
    Log.trace (fun p ->
        p "serialize_boolean %a %a" (boolean_pretty ctxa) a
          (boolean_pretty ctxb) b)
    @@ fun () ->
    match (a, b) with
    | B a, B b ->
        let (Scalar.Context.Result (inc, tup, desb)) =
          B.serialize_boolean ctxa a ctxb b (inc, tup)
        in
        Scalar.Context.Result
          ( inc,
            tup,
            fun ctx out ->
              let b, out = desb ctx out in
              (B b, out) )
    | BTrue, BTrue | BFalse, BFalse ->
        Scalar.Context.Result (inc, tup, fun ctx out -> (a, out))
    | (BAnd (la, ra), BAnd (lb, rb) | BOr (la, ra), BOr (lb, rb)) as f ->
        let foo x y =
          match f with BAnd _, _ -> BAnd (x, y) | _ -> BOr (x, y)
        in
        let (Scalar.Context.Result (inc, tup, desl)) =
          serialize_boolean ctxa la ctxb lb (inc, tup)
        in
        let (Scalar.Context.Result (inc, tup, desr)) =
          serialize_boolean ctxa ra ctxb rb (inc, tup)
        in
        Scalar.Context.Result
          ( inc,
            tup,
            fun ctx out ->
              let r, out = desr ctx out in
              let l, out = desl ctx out in
              (foo l r, out) )
    | _ ->
        Log.warning (fun p -> p "serializing with default value!");
        Scalar.Context.Result
          (inc, tup, fun ctx out -> (boolean_unknown ctx, out))

  let option_merge merge left right =
    match (left, right) with
    | None, None -> None
    | None, Some x | Some x, None -> Some x
    | Some l, Some r -> merge l r

  let rec assume ctx = function
    | B b -> B.assume ctx b
    | BTrue -> Some ctx
    | BFalse -> None
    | BAnd (l, r) -> Option.bind (assume ctx l) (fun ctx -> assume ctx r)
    | BOr (l, r) -> (
        let ctxl = assume ctx l in
        let ctxr = assume ctx r in
        match (ctxl, ctxr) with
        | None, None -> None
        | Some ctx, None | None, Some ctx -> Some ctx
        | Some ctxl, Some ctxr ->
            Some
              (fst
              @@ B.Scalar.typed_nondet2 ctxl ctxr
                   (B.Scalar.Context.empty_tuple ())))

  let query_boolean = Query.boolean

  module Boolean_Forward = struct
    let rec not ctx = function
      | B b -> B (B.not ctx b)
      | BTrue -> BFalse
      | BFalse -> BTrue
      | BAnd (l, r) -> BOr (not ctx l, not ctx r)
      | BOr (l, r) -> BAnd (not ctx l, not ctx r)

    let ( && ) ctx a b = BAnd (a, b)
    let ( || ) ctx a b = BOr (a, b)
    let true_ ctx = BTrue
    let false_ ctx = BFalse
  end

  let satisfiable ctx = function
    | B b -> B.satisfiable ctx b
    | BFalse -> Smtbackend.Smtlib_sig.Unsat
    | BTrue -> Scalar.satisfiable ctx (Scalar.Boolean_Forward.true_ ctx)
    | _ -> assert false
end
