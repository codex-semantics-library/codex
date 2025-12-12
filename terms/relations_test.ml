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

module TestGroup(X : sig
  type gen
  type kind
  include Union_Find.Parameters.GENERIC_GROUP

  val gen : gen QCheck.arbitrary
  val of_gen : gen -> (kind, kind) t
end) = struct

  let ( ** ) = X.compose

  let test_identity = QCheck.Test.make ~count:1000 ~name:"Test identity" X.gen
    (fun i ->
      let i = X.of_gen i in
      X.equal (i ** X.identity) i &&
      X.equal (X.identity ** i) i)
  let () = QCheck.Test.check_exn test_identity

  let test_inverse = QCheck.Test.make ~count:1000 ~name:"Test inverse" X.gen
    (fun i ->
      let i = X.of_gen i in
      let inv = X.inverse i in
      if
      X.equal (i ** inv) X.identity &&
      X.equal (inv ** i) X.identity then true else (
        Format.printf "Mismatch: inv %a = %a, the compositions %a or %a is not id@."
        X.pretty i X.pretty inv X.pretty (i**inv) X.pretty (inv**i); false
      ))
  let () = QCheck.Test.check_exn test_inverse

  let test_compose_assoc = QCheck.Test.make ~count:1000 ~name:"Test compose_assoc"
    (QCheck.tup3 X.gen X.gen X.gen)
    (fun (i,j,k) ->
      let i = X.of_gen i in
      let j = X.of_gen j in
      let k = X.of_gen k in
      X.equal ((i ** j) ** k) (i ** (j ** k)))
let () = QCheck.Test.check_exn test_compose_assoc
end

open Operator.Function_symbol
open Terms.Relations

let gen_sign = QCheck.bool
let factor_of_bool x = if x then Additive.PlusOne else Additive.MinusOne

let%test_module "TestAdditiveBool" = (module TestGroup(struct
  type gen = bool
  type kind = boolean

  include Additive

  let gen = gen_sign
  let of_gen is_positive = if is_positive then identity else boolean_not
end))

let%test_module "TestAdditiveBitvector" = (module TestGroup(struct
  type gen = bool * int32
  type kind = bitvector
  let size = 32 |> Units.In_bits.of_int

  include Additive

  let gen = QCheck.tup2 gen_sign QCheck.int32
  let of_gen (is_positive, z) = additive_bitvector ~size (factor_of_bool is_positive) (Z.of_int32 z)
end))

let%test_module "TestAdditiveInteger" = (module TestGroup(struct
  type gen = bool * int64
  type kind = integer

  include Additive

  let gen = QCheck.tup2 gen_sign QCheck.int64
  let of_gen (is_positive, z) = additive_integer (factor_of_bool is_positive) (Z.of_int64 z)
end))

let%test_module "TestRotateBitvector" = (module TestGroup(struct
  type gen = int * int64
  type kind = bitvector
  let size = 32  |> Units.In_bits.of_int

  include XOR_Rotate

  let gen = QCheck.tup2 QCheck.int QCheck.int64
  let of_gen (is_positive, z) = xr_xor_rotate ~size ~rotate:is_positive ~xor:(Z.of_int64 z)
end))

let%test_module "TestLinearEquality" = (module TestGroup(struct
  type gen = int64 * int64 * int64
  type kind = bitvector

  include LinearTwoVarEquality

  let non_zero i = Z.of_int64 (if Int64.equal i Int64.zero then Int64.one else i)

  let gen = QCheck.tup3 QCheck.int64 QCheck.int64 QCheck.int64
  let of_gen (f1, f2, offset) =
    let f1 = non_zero f1 in
    let f2 = non_zero f2 in
    make ~size:(32 |> Units.In_bits.of_int) ~f1 ~f2 (Z.of_int64 offset)
end))
