open Transfer_functions.Term

module type GROUP_ACTION = sig
  type binary
  type integer
  type boolean
  type ('a, 'b) relation

  type (_, _) mapping =
    | BinaryMapping: (binary,  Transfer_functions.Term.binary) mapping
    | IntegerMapping: (integer, Transfer_functions.Term.integer) mapping
    | BooleanMapping: (boolean, Transfer_functions.Term.boolean) mapping

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

  module Action(B: sig type binary type integer type boolean end) = struct
    include B
    type ('a, 'b) relation = ('a, 'b) t

    type (_, _) mapping =
      | BinaryMapping: (binary,  Transfer_functions.Term.binary) mapping
      | IntegerMapping: (integer, Transfer_functions.Term.integer) mapping
      | BooleanMapping: (boolean, Transfer_functions.Term.boolean) mapping

    type 'term wrapper = Wrap: ('value * ('value, 'term) mapping) -> 'term wrapper

    let apply_relation (type value_parent term_parent term_child)
        (value_parent: value_parent) (Equal: (term_child, term_parent) relation)
        (map: (value_parent, term_parent) mapping): term_child wrapper =
      match map with
      | BinaryMapping -> Wrap(value_parent, map)
      | IntegerMapping -> Wrap(value_parent, map)
      | BooleanMapping -> Wrap(value_parent, map)

    let refine_relation (type value_parent value_child term_parent term_child)
        (value_parent: value_parent) (value_child: value_child)
        (Equal: (term_child, term_parent) relation)
        (mp: (value_parent, term_parent) mapping)
        (mc: (value_child, term_child) mapping): value_parent option =
      match mp, mc with
      | BinaryMapping, BinaryMapping -> Some value_child
      | IntegerMapping, IntegerMapping -> Some value_child
      | BooleanMapping, BooleanMapping -> Some value_child
  end
end

module Additive = struct
  type delta = PlusOne | MinusOne

  type (_, _) t =
    | Identity: ('a, 'a) t
    | Add_Modulo: { factor: delta; size: int; amount: Z.t } -> (binary, binary) t
    | Add_Unbounded: delta * Z.t -> (integer, integer) t
    | Bool_Not: (boolean, boolean) t

  let additive_identity = Identity
  let additive_binary ~size factor x =
    let x' = Z.signed_extract x 0 size in
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
              Framac_ival.Abstract_interp.Int.pretty amount size
          | Bool_Not -> Format.fprintf fmt "neg"

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
        additive_binary ~size:sl (sign_compose zl zr)
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
        additive_binary ~size factor
        (if factor = PlusOne then Z.neg amount else amount)

  let equal x y = x = y

  module Action(B: Single_value_abstraction.Sig.Numeric_Basis) = struct
    type binary = B.binary
    type integer = B.integer
    type boolean = B.boolean

    type ('a, 'b) relation = ('a, 'b) t

    type (_, _) mapping =
      | BinaryMapping: (binary,  Transfer_functions.Term.binary) mapping
      | IntegerMapping: (integer, Transfer_functions.Term.integer) mapping
      | BooleanMapping: (boolean, Transfer_functions.Term.boolean) mapping

    type 'term wrapper = Wrap: ('value * ('value, 'term) mapping) -> 'term wrapper

    let apply_relation (type value_parent term_parent term_child)
        (value_parent: value_parent) (rel: (term_child, term_parent) relation)
        (map: (value_parent, term_parent) mapping): term_child wrapper =
      match rel, map with
      | Identity, BinaryMapping -> Wrap(value_parent, map)
      | Identity, IntegerMapping -> Wrap(value_parent, map)
      | Identity, BooleanMapping -> Wrap(value_parent, map)
      | Bool_Not, BooleanMapping -> Wrap(B.Boolean_Forward.not value_parent, map)
      | Add_Unbounded(is_pos, z), IntegerMapping ->
          let const = B.Integer_Forward.iconst z in
          Wrap((if is_pos = PlusOne
            then B.Integer_Forward.iadd const value_parent
            else B.Integer_Forward.isub const value_parent), map)
      | Add_Modulo x, BinaryMapping ->
          let const = B.Binary_Forward.biconst ~size:x.size x.amount in
          Wrap((if x.factor = PlusOne then B.Binary_Forward.biadd else B.Binary_Forward.bisub)
            ~nuw:false ~nsw:false ~nusw:false ~size:x.size const value_parent, map)

    let refine_relation (type value_parent value_child term_parent term_child)
        (value_parent: value_parent) (value_child: value_child)
        (rel: (term_child, term_parent) relation)
        (mp: (value_parent, term_parent) mapping)
        (mc: (value_child, term_child) mapping): value_parent option =
      match rel, mp, mc with
      | Identity, BinaryMapping, BinaryMapping -> Some value_child
      | Identity, IntegerMapping, IntegerMapping -> Some value_child
      | Identity, BooleanMapping, BooleanMapping -> Some value_child
      | Bool_Not, BooleanMapping, BooleanMapping -> B.Boolean_Backward.not value_parent value_child
      | Add_Unbounded(is_pos, z), IntegerMapping, IntegerMapping ->
          let const = B.Integer_Forward.iconst z in
          let (_, new_value) = if is_pos = PlusOne
            then B.Integer_Backward.iadd const value_parent value_child
            else B.Integer_Backward.isub const value_parent value_child in
          new_value
      | Add_Modulo x, BinaryMapping, BinaryMapping ->
          let const = B.Binary_Forward.biconst ~size:x.size x.amount in
          let (_, new_value) = if x.factor = PlusOne
            then B.Binary_Backward.biadd ~nuw:false ~nsw:false ~nusw:false ~size:x.size const value_parent value_child
            else B.Binary_Backward.bisub ~nuw:false ~nsw:false ~nusw:false ~size:x.size const value_parent value_child in
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
    | XR_XOR_rotate: { rotate: int; xor: Z.t; size: int } -> (binary, binary) t

  let xr_identity = XR_Identity
  let xr_bnot = XR_BNot
  let xr_xor_rotate ~rotate ~xor ~size =
    let xor = Z.extract xor 0 size in
    let rotate = rotate mod size in
    let rotate = if rotate < 0 then rotate+size else rotate in
    if Int.equal rotate 0 && Z.(equal xor zero)
      then xr_identity
      else XR_XOR_rotate {rotate; xor; size}

  let pretty (type a b) fmt: (a, b) t -> unit = function
    | XR_Identity -> Format.fprintf fmt "XR_Identity"
    | XR_BNot -> Format.fprintf fmt "XR_BNot"
    | XR_XOR_rotate a -> Format.fprintf fmt "XR_XOR_Rotate@[{rot=%d; xor=%a; %d}@]"
      a.rotate Framac_ival.Abstract_interp.Int.pretty a.xor a.size

  let rotate ~size shift value =
    if shift == 0 then value
    else
      let top = Z.extract value (size-shift) (shift) in
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
      let rot = if x.rotate = 0 then 0 else x.size-x.rotate in
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
        let rot = if rot > x.size then rot - x.size else rot in
        xr_xor_rotate ~size:x.size ~rotate:rot ~xor:(
          Z.logxor (rotate ~size:x.size x.rotate y.xor) x.xor
        )

  let equal = (=)
  let identity = XR_Identity
end
