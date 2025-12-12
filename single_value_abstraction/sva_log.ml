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

module Sig = Sva_sig

module No_Log_Numeric_Enum(Log:Tracelog.S)(Sub:Sig.NUMERIC_ENUM):Sig.NUMERIC_ENUM = Sub

module Log_Numeric_Enum(Log:Tracelog.S)(Sub:Sig.NUMERIC_ENUM):Sig.NUMERIC_ENUM = struct
  let name = Sub.name

  type boolean = Sub.boolean
  type bitvector = Sub.bitvector
  type integer = Sub.integer
  type enum = Sub.enum

  module Boolean_Lattice = Sub.Boolean_Lattice
  module Bitvector_Lattice = Sub.Bitvector_Lattice
  module Enum_Lattice = Sub.Enum_Lattice
  module Integer_Lattice = struct
    include Sub.Integer_Lattice
    let join a b =
      Log.trace (fun p -> p "join %a %a" pretty a pretty b) ~pp_ret:pretty (fun () -> join a b)

    let includes_or_widen ~previous next =
      Log.trace (fun p -> p "includes_or_widen %a %a" pretty previous pretty next) ~pp_ret:(fun fmt (p, b) ->
        Format.fprintf fmt "%b,%a" p pretty b)
        (fun () -> includes_or_widen ~previous next)
  end

  open Operator

  module Conversion = struct
    type boolean = Sub.boolean
    type integer = Sub.integer
    type enum = Sub.enum
    type bitvector = Sub.bitvector

    type 'a pp = Format.formatter -> 'a -> unit

    let bool_printer = Sub.Boolean_Lattice.pretty
    let enum_printer = Sub.Enum_Lattice.pretty
    let integer_printer = Sub.Integer_Lattice.pretty
    let bv_printer = Sub.Bitvector_Lattice.pretty
  end

  module Forward_Conversion = struct
    include Conversion
    module Arity = Forward_Arity

    let ar0 pp_ret pp f =
      Log.trace (fun p -> p "forward %t" pp) ~pp_ret @@ fun () -> f

    let ar1 ppa pp_ret pp f a =
      Log.trace (fun p -> p "forward %t %a" pp ppa a) ~pp_ret @@ fun () -> f a

    let ar2 ppa ppb pp_ret pp f a b =
      Log.trace (fun p -> p "forward %t %a %a" pp ppa a ppb b) ~pp_ret
      @@ fun () -> f a b
  end

  module Backward_Conversion = struct
    include Conversion
    module Arity = Backward_Arity

    let pp_option pp fmt = function
      | None -> Format.fprintf fmt "unchanged"
      | Some x -> pp fmt x

    let pp_option_pair ppa ppb fmt (a, b) =
      Format.fprintf fmt "(%a,%a)" (pp_option ppa) a (pp_option ppb) b

    let ar0 pp_ret pp f = assert false

    let ar1 ppa pp_ret pp f a r =
      Log.trace
        (fun p -> p "backward %t %a %a" pp ppa a pp_ret r)
        ~pp_ret:(pp_option ppa)
      @@ fun () -> f a r

    let ar2 ppa ppb pp_ret pp f a b r =
      Log.trace
        (fun p -> p "backward %t %a %a %a" pp ppa a ppb b pp_ret r)
        ~pp_ret:(pp_option_pair ppa ppb)
      @@ fun () -> f a b r
  end

  module Boolean_Forward = struct
    include Autolog.Log_Boolean_Backward(Forward_Conversion)(Sub.Boolean_Forward)
    let true_ = Sub.Boolean_Forward.true_
    let false_ = Sub.Boolean_Forward.false_
  end

  module Boolean_Backward =
    Autolog.Log_Boolean_Backward(Backward_Conversion)(Sub.Boolean_Backward)

  module Integer_Forward = struct
    include Autolog.Log_Integer_Backward(Forward_Conversion)(Sub.Integer_Forward)

    let zero = Sub.Integer_Forward.zero
    let one = Sub.Integer_Forward.one

    let iconst z =
      Forward_Conversion.ar0 Forward_Conversion.integer_printer
        (fun fmt -> Format.fprintf fmt "iconst %s" (Z.to_string z))
        (Sub.Integer_Forward.iconst z)
  end

  module Integer_Backward =
    Autolog.Log_Integer_Backward(Backward_Conversion) (Sub.Integer_Backward)



  module Bitvector_Forward =
    Autolog.Log_Bitvector_Forward_With_Bimul_add(Forward_Conversion)(Sub.Bitvector_Forward)

  module Bitvector_Backward =
    Autolog.Log_Bitvector_Forward_With_Bimul_add
      (Backward_Conversion)
      (struct
        include Sub.Bitvector_Backward
        let biconst ~size _ = assert false
      end)

  module Enum_Forward = Autolog.Log_Enum_Forward(Forward_Conversion)(Sub.Enum_Forward)

  module Enum_Backward =
    Autolog.Log_Enum_Forward
      (Backward_Conversion)
      (struct
        include Sub.Enum_Backward
        let enum_const ~case = assert false
      end)
end
