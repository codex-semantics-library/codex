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

open Operator.Function_symbol
module In_bits = Units.In_bits


module type GROUP_ACTION = sig
  type bitvector
  type integer
  type boolean
  type enum
  type ('a, 'b) relation

  type (_, _) mapping =
    | BitvectorMapping: (bitvector,  Operator.Function_symbol.bitvector) mapping
    | IntegerMapping: (integer, Operator.Function_symbol.integer) mapping
    | BooleanMapping: (boolean, Operator.Function_symbol.boolean) mapping
    | EnumMapping: (enum, Operator.Function_symbol.enum) mapping

  type 'term wrapper = Wrap: ('value * ('value, 'term) mapping) -> 'term wrapper

  val apply_relation:
    'value_parent ->
    ('term_child, 'term_parent) relation ->
    ('value_parent, 'term_parent) mapping ->
    'term_child wrapper

  val refine_relation:
    'value_parent ->
    'value_child ->
    ('term_child, 'term_parent) relation ->
    ('value_parent, 'term_parent) mapping ->
    ('value_child, 'term_child) mapping ->
    'value_parent option
end

module Equality = struct
  type ('a, 'b) t = Equal: ('a, 'a) t
  let compose (type a b c) (Equal: (b,c) t) (Equal: (a,b) t): (a,c) t = Equal
  let inverse (type a b) (Equal: (a,b) t): (b,a) t = Equal
  let identity = Equal
  let equal (type a b) (Equal: (a,b) t) (Equal: (a,b) t) = true
  let pretty (type a b) fmt (Equal: (a,b) t) = Format.fprintf fmt "Equal"
  let pretty_with_terms pp_x x pp_y y fmt _ = Format.fprintf fmt "@[%a = %a@]" pp_x x pp_y y

  module Action(B: sig type bitvector type integer type boolean type enum end) = struct
    include B
    type ('a, 'b) relation = ('a, 'b) t

    type (_, _) mapping =
      | BitvectorMapping: (bitvector,  Operator.Function_symbol.bitvector) mapping
      | IntegerMapping: (integer, Operator.Function_symbol.integer) mapping
      | BooleanMapping: (boolean, Operator.Function_symbol.boolean) mapping
      | EnumMapping: (enum, Operator.Function_symbol.enum) mapping

    type 'term wrapper = Wrap: ('value * ('value, 'term) mapping) -> 'term wrapper

    let apply_relation (type value_parent term_parent term_child)
        (value_parent: value_parent) (Equal: (term_child, term_parent) relation)
        (map: (value_parent, term_parent) mapping): term_child wrapper =
      match map with
      | BitvectorMapping -> Wrap(value_parent, map)
      | IntegerMapping -> Wrap(value_parent, map)
      | BooleanMapping -> Wrap(value_parent, map)
      | EnumMapping -> Wrap(value_parent, map)

    let refine_relation (type value_parent value_child term_parent term_child)
        (value_parent: value_parent) (value_child: value_child)
        (Equal: (term_child, term_parent) relation)
        (mp: (value_parent, term_parent) mapping)
        (mc: (value_child, term_child) mapping): value_parent option =
      match mp, mc with
      | BitvectorMapping, BitvectorMapping -> Some value_child
      | IntegerMapping, IntegerMapping -> Some value_child
      | BooleanMapping, BooleanMapping -> Some value_child
      | EnumMapping, EnumMapping -> Some value_child
  end
end

module Additive = struct
  type delta = PlusOne | MinusOne

  type (_, _) t =
    | Identity: ('a, 'a) t
    | Add_Modulo: { factor: delta; size:In_bits.t; amount: Z.t } -> (bitvector, bitvector) t
    | Add_Unbounded: delta * Z.t -> (integer, integer) t
    | Bool_Not: (boolean, boolean) t

  let additive_identity = Identity
  let additive_bitvector ~(size:In_bits.t) factor x =
    let x' = Z.signed_extract x 0 (size:>int) in
    if Z.equal x' Z.zero && factor = PlusOne
    then Identity
    else Add_Modulo { factor; size; amount=x }
  let additive_integer factor x =
    if Z.equal x Z.zero && factor = PlusOne
    then Identity
    else Add_Unbounded (factor, x)
  let boolean_not = Bool_Not

  let pretty_sign fmt factor = if factor = MinusOne then Format.fprintf fmt "-"

  let identity = additive_identity

  let flip = function
    | PlusOne -> MinusOne
    | MinusOne -> PlusOne
  let sign_compose x y = match x with
    | PlusOne -> y
    | MinusOne -> flip y

  (** Only print "+" if number is positive *)
  let pretty_int_sign fmt l = if Z.geq l Z.zero then Format.fprintf fmt "+"

  let pretty (type a b) fmt (x: (a,b) t) = match x with
    | Identity -> Format.fprintf fmt "id"
    | Add_Unbounded (z,l) -> Format.fprintf fmt "x->%ax%a%a" pretty_sign z
        pretty_int_sign l
        Framac_ival.Abstract_interp.Int.pretty l
    | Add_Modulo{factor; size; amount} ->
        Format.fprintf fmt "x->%ax%a%a[%d]" pretty_sign factor
        pretty_int_sign amount
        Framac_ival.Abstract_interp.Int.pretty amount (size:>int)
    | Bool_Not -> Format.fprintf fmt "neg"
  let pretty_with_terms pp_x x pp_y y fmt (type a b) (rel: (a,b) t) = match rel with
    | Identity -> Format.fprintf fmt "[%a = %a]" pp_x x pp_y y
    | Add_Unbounded(z,l) ->
        Format.fprintf fmt "[%a = %a %s %a]" pp_x x pp_y y (match z with PlusOne -> "+" | MinusOne -> "-") Z.pp_print l
    | Add_Modulo {factor;amount;size} ->
        Format.fprintf fmt "%a->%a%a%a%a[%d]" pp_x x pretty_sign factor pp_y y
        pretty_int_sign amount
        Framac_ival.Abstract_interp.Int.pretty amount (size:>int)
    | Bool_Not -> Format.fprintf fmt "[%a = bnot %a]" pp_x x pp_y y


  (* val --(y)--> val' --(x)-->
     where y: delta * val + b
       and x: delta'* val'+b'
    is delta'*delta*val + b' + delta'*b *)
  let compose (type a b c) (x: (b,c) t) (y: (a,b) t): (a,c) t =
    match x, y with
    | Identity, c -> c
    | c, Identity -> c
    | Add_Unbounded (zl, off_l), Add_Unbounded (zr, off_r) ->
        additive_integer (sign_compose zl zr)
        (if zr=PlusOne then Z.(off_l + off_r) else Z.(off_r - off_l))
    | Add_Modulo { factor=zl; size=sl; amount=off_l },
      Add_Modulo { factor=zr; size=sr; amount=off_r } ->
        assert(sl=sr);
        additive_bitvector ~size:sl (sign_compose zl zr)
        (if zr=PlusOne then Z.(off_l + off_r) else Z.(off_r - off_l))
    | Bool_Not, Bool_Not -> Identity

  (* For y = delta*x + b
     Then x = delta*y - delta*b *)
  let inverse (type a b) (x: (a,b) t): (b,a) t =
    match x with
    | Identity -> x
    | Bool_Not -> x
    | Add_Unbounded (z,l) -> Add_Unbounded (z, if z = PlusOne then Z.neg l else l)
    | Add_Modulo {factor; size; amount} ->
        additive_bitvector ~size factor
        (if factor = PlusOne then Z.neg amount else amount)

  let equal x y = x = y

  module Action(B: Single_value_abstraction.Sig.NUMERIC_ENUM) = struct
    type bitvector = B.bitvector
    type integer = B.integer
    type boolean = B.boolean
    type enum = B.enum

    type ('a, 'b) relation = ('a, 'b) t

    type (_, _) mapping =
      | BitvectorMapping: (bitvector,  Operator.Function_symbol.bitvector) mapping
      | IntegerMapping: (integer, Operator.Function_symbol.integer) mapping
      | BooleanMapping: (boolean, Operator.Function_symbol.boolean) mapping
      | EnumMapping: (enum, Operator.Function_symbol.enum) mapping

    type 'term wrapper = Wrap: ('value * ('value, 'term) mapping) -> 'term wrapper

    let apply_relation (type value_parent term_parent term_child)
        (value_parent: value_parent) (rel: (term_child, term_parent) relation)
        (map: (value_parent, term_parent) mapping): term_child wrapper =
      match rel, map with
      | Identity, BitvectorMapping -> Wrap(value_parent, map)
      | Identity, IntegerMapping -> Wrap(value_parent, map)
      | Identity, BooleanMapping -> Wrap(value_parent, map)
      | Identity, EnumMapping -> Wrap(value_parent, map)
      | Bool_Not, BooleanMapping -> Wrap(B.Boolean_Forward.not value_parent, map)
      | Add_Unbounded(is_pos, z), IntegerMapping ->
          let const = B.Integer_Forward.iconst z in
          Wrap((if is_pos = PlusOne
            then B.Integer_Forward.iadd const value_parent
            else B.Integer_Forward.isub const value_parent), map)
      | Add_Modulo x, BitvectorMapping ->
          let const = B.Bitvector_Forward.biconst ~size:x.size x.amount in
          Wrap((if x.factor = PlusOne
                then B.Bitvector_Forward.biadd else B.Bitvector_Forward.bisub )
                 ~flags:(Operator.Flags.Biadd.pack ~nsw:false ~nuw:false ~nusw:false)
             ~size:x.size const value_parent, map)

    let refine_relation (type value_parent value_child term_parent term_child)
        (value_parent: value_parent) (value_child: value_child)
        (rel: (term_child, term_parent) relation)
        (mp: (value_parent, term_parent) mapping)
        (mc: (value_child, term_child) mapping): value_parent option =
      match rel, mp, mc with
      | Identity, BitvectorMapping, BitvectorMapping -> Some value_child
      | Identity, IntegerMapping, IntegerMapping -> Some value_child
      | Identity, BooleanMapping, BooleanMapping -> Some value_child
      | Identity, EnumMapping, EnumMapping -> Some value_child
      | Bool_Not, BooleanMapping, BooleanMapping -> B.Boolean_Backward.not value_parent value_child
      | Add_Unbounded(is_pos, z), IntegerMapping, IntegerMapping ->
          let const = B.Integer_Forward.iconst z in
          let (_, new_value) = if is_pos = PlusOne
            then B.Integer_Backward.iadd const value_parent value_child
            else B.Integer_Backward.isub const value_parent value_child in
          new_value
      | Add_Modulo x, BitvectorMapping, BitvectorMapping ->
          let const = B.Bitvector_Forward.biconst ~size:x.size x.amount in
          let (_, new_value) = if x.factor = PlusOne
            then B.Bitvector_Backward.biadd ~flags:(Operator.Flags.Biadd.pack ~nsw:false ~nuw:false ~nusw:false) ~size:x.size const value_parent value_child
            else B.Bitvector_Backward.bisub ~flags:(Operator.Flags.Bisub.pack ~nsw:false ~nuw:false ~nusw:false) ~size:x.size const value_parent value_child in
          new_value
  end
  (* let inverse x =
    let y = inverse x in
    Format.printf "Inverse %a = %a@."
    pretty x pretty y;
    y *)
end

module XOR_Rotate = struct
  type (_, _) t =
    | XR_Identity: ('a, 'a) t
    | XR_BNot: (boolean, boolean) t
    | XR_XOR_rotate: { rotate: int; xor: Z.t; size:In_bits.t } -> (bitvector, bitvector) t

  let xr_identity = XR_Identity
  let xr_bnot = XR_BNot
  let xr_xor_rotate ~rotate ~xor ~(size:In_bits.t) =
    let xor = Z.extract xor 0 (size:>int) in
    let rotate = rotate mod (size:>int) in
    let rotate = if rotate < 0 then rotate+(size:>int) else rotate in
    if Int.equal rotate 0 && Z.(equal xor zero)
      then xr_identity
      else XR_XOR_rotate {rotate; xor; size}

  let pretty (type a b) fmt: (a, b) t -> unit = function
    | XR_Identity -> Format.fprintf fmt "XR_Identity"
    | XR_BNot -> Format.fprintf fmt "XR_BNot"
    | XR_XOR_rotate a -> Format.fprintf fmt "XR_XOR_Rotate@[{rot=%d; xor=%a; %d}@]"
      a.rotate Framac_ival.Abstract_interp.Int.pretty a.xor (a.size:>int)
  let pretty_with_terms pp_x x pp_y y fmt (type a b) (rel: (a,b) t) = match rel with
    | XR_Identity -> Format.fprintf fmt "[%a = %a]" pp_x x pp_y y
    | XR_BNot -> Format.fprintf fmt "[%a = bnot %a]" pp_x x pp_y y
    | XR_XOR_rotate a ->
        Format.fprintf fmt "@[%a = (%a xor %a) rot %d [%d]@]"
        pp_x x pp_y y Framac_ival.Abstract_interp.Int.pretty a.xor a.rotate (a.size:>int)

  let rotate ~(size:In_bits.t) shift value =
    if shift == 0 then value
    else
      let top = Z.extract value ((size:>int)-shift) (shift) in
      Z.logor (Z.shift_left value shift) top

  (**{v
                             y = (x << s) lxor c
                      y lxor c = x << s
              (y lxor c) << -s = x
      (y << -s) lxor (c << -s) = x
      v}*)
  let inverse (type a b) : (a,b) t -> (b, a) t = function
    | XR_Identity -> XR_Identity
    | XR_BNot -> XR_BNot
    | XR_XOR_rotate x ->
      let rot = if x.rotate = 0 then 0 else (x.size:>int)-x.rotate in
      xr_xor_rotate ~size:x.size ~rotate:rot
        ~xor:(rotate ~size:x.size rot x.xor)

  (** {v
         y = (x << s) lxor c  AND  z = (y << s') lxor c'
      so z = ((x << s lxor c) << s' lxor c')
      so z = x << (s + s') lxor ((c << s') lxor c')
      v}*)
  let compose (type a b c) (x: (b,c) t) (y: (a,b) t): (a,c) t =
    match x, y with
    | XR_Identity, c -> c
    | c, XR_Identity -> c
    | XR_BNot, XR_BNot -> XR_Identity
    | XR_XOR_rotate x, XR_XOR_rotate y ->
        assert (x.size = y.size);
        let rot = x.rotate + y.rotate in
        let rot = if rot > (x.size:>int) then rot - (x.size:>int) else rot in
        xr_xor_rotate ~size:x.size ~rotate:rot ~xor:(
          Z.logxor (rotate ~size:x.size x.rotate y.xor) x.xor
        )

  let equal x y = x = y

  let identity = XR_Identity
end

module LinearTwoVarEquality = struct
  type (_, _) t =
    | Identity: ('a, 'a) t
    | Linear_Equality: { size: In_bits.t; f1: Z.t; f2: Z.t; offset: Z.t } -> (bitvector, bitvector) t

  let identity = Identity
  let make ~size:sz ~f1 ~f2 offset =
    let open Z in
    assert (not (equal f1 zero || equal f2 zero));
    if equal offset zero && equal f1 Z.( - f2)
    then identity
    else
      let gcd = gcd f1 (gcd f2 offset) in
      let gcd = if Z.leq f1 Z.zero then - gcd else gcd in (* ensure invariant f1 > 0 *)
      if equal gcd one
      then Linear_Equality{size=sz; f1; f2; offset}
      else (* ensure factors are minimal *)
        Linear_Equality{size=sz; f1=f1 / gcd; f2=f2 / gcd; offset=offset / gcd}

  let f1 : type a b. (a,b) t -> Z.t = function
    | Identity -> Z.one
    | Linear_Equality{f1;_} -> f1
  let f2 : type a b. (a,b) t -> Z.t = function
    | Identity -> Z.minus_one
    | Linear_Equality{f2;_} -> f2
  let offset : type a b. (a,b) t -> Z.t = function
    | Identity -> Z.zero
    | Linear_Equality{offset;_}-> offset

  let inverse (type a b) (x: (a,b) t): (b,a) t =
    match x with
    | Identity -> Identity
    | Linear_Equality r ->
        let open Z in
        if lt r.f2 zero
        then Linear_Equality { r with f1 = - r.f2; f2 = - r.f1; offset = - r.offset }
        else Linear_Equality { r with f1 = r.f2; f2 = r.f1 }

  (** Pretty printer for z factors:
      {[
      | 1 -> positive
      | -1 -> "-"
      | x when x >= 0 -> positive ^ x
      | x -> x (* automatically preceded by a "-" *)
      ]} *)
  let pretty_factor positive fmt z =
    if Z.equal z Z.one
    then Format.fprintf fmt "%s" positive
    else
      if Z.equal z Z.minus_one
      then Format.fprintf fmt "-"
      else if Z.geq z Z.zero
        then Format.fprintf fmt "%s%a" positive Z.pp_print z
        else Format.fprintf fmt "%a" Z.pp_print z

  let pretty (type a b) fmt (x: (a,b) t) = match x with
    | Identity -> Format.fprintf fmt "id"
    | Linear_Equality{f1; f2; size; offset} ->
        Format.fprintf fmt "%ax%ay=%a[%d]"
        (pretty_factor "") f1 (pretty_factor "+") f2 Z.pp_print offset (size:>int)
  let pretty_with_terms pp_x x pp_y y fmt (type a b) (rel: (a,b) t) = match rel with
    | Identity -> Format.fprintf fmt "[%a = %a]" pp_x x pp_y y
    | Linear_Equality{f1; f2; size; offset} ->
        Format.fprintf fmt "@[%a%a %a %a = %a[%d]@]"
        (pretty_factor "") f1 pp_x x (pretty_factor "+") f2 pp_y y Z.pp_print offset (size:>int)

  let equal x y = x = y

  (** Greatest common multiple *)
  let gcm a b = Z.((a * b) / gcd a b)

  let compose (type a b c) (x: (b,c) t) (y: (a,b) t): (a,c) t =
    match x, y with
    | Identity, c -> c
    | c, Identity -> c
    | Linear_Equality y, Linear_Equality x ->
        (* x.f1 A + x.f2 B = x.offset /\ y.f1 B + y.f2 C = y.offset
        implies
          mul_x x.f1 A + GCM B = mul_x x.offset (where mul_x = gcm x.f2 y.f1 / x.f2)
          GCM B + mul_y y.f2 C = mul_y y.offset (where mul_y = gcm x.f2 y.f1 / x.f2)
        Subbing both:
          mul_x x.f1 A - mul_y y.f2 C = mul_x x.offset - mul_y y.offset *)
        assert (x.size = y.size);
        let gcm = gcm x.f2 y.f1 in
        let open Z in
        let mul_x = gcm / x.f2 in
        let mul_y = - gcm / y.f1 in
        make
          ~size:x.size
          ~f1:(x.f1 * mul_x)
          ~f2:(y.f2 * mul_y)
          (x.offset*mul_x + y.offset * mul_y)

  module Action(B: Single_value_abstraction.Sig.NUMERIC_ENUM) = struct
    type bitvector = B.bitvector
    type integer = B.integer
    type boolean = B.boolean
    type enum = B.enum

    type ('a, 'b) relation = ('a, 'b) t

    type (_, _) mapping =
      | BitvectorMapping: (bitvector, Operator.Function_symbol.bitvector) mapping
      | IntegerMapping: (integer, Operator.Function_symbol.integer) mapping
      | BooleanMapping: (boolean, Operator.Function_symbol.boolean) mapping
      | EnumMapping: (enum, Operator.Function_symbol.enum) mapping

    type 'term wrapper = Wrap: ('value * ('value, 'term) mapping) -> 'term wrapper

    let apply_relation (type value_parent term_parent term_child)
        (value: value_parent) (rel: (term_child, term_parent) relation)
        (map: (value_parent, term_parent) mapping): term_child wrapper =
      match rel, map with
      | Identity, BitvectorMapping -> Wrap(value, map)
      | Identity, IntegerMapping -> Wrap(value, map)
      | Identity, BooleanMapping -> Wrap(value, map)
      | Identity, EnumMapping -> Wrap(value, map)
      | Linear_Equality x, BitvectorMapping ->
          let value = B.Bitvector_Forward.bimul_add ~size:x.size ~prod:(Z.neg x.f2) ~offset:x.offset value in
          (* If f1 is not one, some division is required... *)
          let value =
            if Z.equal x.f1 Z.one
            then value
            else B.Bitvector_Forward.bisdiv ~size:x.size value (B.Bitvector_Forward.biconst ~size:x.size x.f1)
          in Wrap(value, BitvectorMapping)

    let refine_relation (type value_parent value_child term_parent term_child)
        (value_parent: value_parent) (value_child: value_child)
        (rel: (term_child, term_parent) relation)
        (mp: (value_parent, term_parent) mapping)
        (mc: (value_child, term_child) mapping): value_parent option =
      match rel, mp, mc with
      | Identity, BitvectorMapping, BitvectorMapping -> Some value_child
      | Identity, IntegerMapping, IntegerMapping -> Some value_child
      | Identity, BooleanMapping, BooleanMapping -> Some value_child
      | Identity, EnumMapping, EnumMapping -> Some value_child
      | Linear_Equality x, BitvectorMapping, BitvectorMapping ->
          (* Start by doing forward propagation through all layers (but the last),
             just like in apply_relation *)
          let value_mid = B.Bitvector_Forward.bimul_add ~size:x.size ~prod:(Z.neg x.f2) ~offset:x.offset value_parent in
          (* Now backpropagate up each layer in turn *)
          match
            if Z.equal x.f1 Z.one
            then Some value_child
            else fst @@ B.Bitvector_Backward.bisdiv ~size:x.size value_mid (B.Bitvector_Forward.biconst ~size:x.size x.f1) value_child
          with
          | None -> None
          | Some value_mid' ->
              let value_mid = B.Bitvector_Lattice.inter ~size:x.size value_mid value_mid' in
              B.Bitvector_Backward.bimul_add  ~size:x.size ~prod:(Z.neg x.f2) ~offset:x.offset value_parent value_mid
  end
end
