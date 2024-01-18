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

(* Note: this was not heavily tested or reviewed. *)

let name = "Sentinel"

include Quadrivalent_basis

module Integer_Lattice = struct
  include Lattices.Unit (* TODO: Should be in Binary_Basis. *)
  let singleton _ = assert false
end
type integer = Integer_Lattice.t
module Integer_Backward = Basis_Assert_False_Transfer_Functions.Integer_Backward
module Integer_Forward = Basis_Assert_False_Transfer_Functions.Integer_Forward
let is_singleton_int _ = assert false
let convert_to_ival _ = assert false

module Binary_Lattice = struct
  type t = Bot | Zero | NonZero | SentinelTop

  let bottom ~size:_ = Bot
  let is_bottom ~size:_ = function Bot -> true | _ -> false

  let top ~size:_ = SentinelTop

  let pretty ~size:_ fmt =
    let open Format in function
    | Bot -> pp_print_string fmt "Bottom"
    | Zero -> pp_print_string fmt "Zero"
    | NonZero -> pp_print_string fmt "NonZero"
    | SentinelTop -> pp_print_string fmt "MbZero"

  let includes ~size:_ x y =
    match x,y with
    | _,Bot -> true
    | Zero,NonZero | NonZero,Zero -> false
    | Zero,Zero | NonZero,NonZero -> true
    | SentinelTop,_ -> true
    | _ -> false

  let join ~size:_ x y =
    match x,y with
    | Bot,a | a,Bot -> a
    | Zero,Zero -> Zero
    | NonZero,NonZero -> NonZero
    | _ -> SentinelTop

  let inter ~size:_ x y =
    match x,y with
    | Bot,_ | _,Bot -> Bot
    | Zero,NonZero | NonZero,Zero -> Bot
    | NonZero,_ | _,NonZero -> NonZero
    | Zero,_ | _,Zero -> Zero
    | SentinelTop,SentinelTop -> SentinelTop

  let hash = function
    | Bot -> Hashtbl.hash 0
    | Zero -> Hashtbl.hash 1
    | NonZero -> Hashtbl.hash 2
    | SentinelTop -> Hashtbl.hash 3

  let compare x y =
    match x,y with
    | Bot,Bot -> 0
    | Bot,_ -> -1
    | _,Bot -> 1
    | Zero,Zero -> 0
    | Zero,_ -> -1
    | _,Zero -> 1
    | NonZero,NonZero -> 0
    | NonZero, SentinelTop -> -1
    | SentinelTop, NonZero -> 1
    | SentinelTop, SentinelTop -> 0

  let equal x y = compare x y = 0

  let widen ~size ~previous x =
    join ~size previous x

  let includes_or_widen ~size ~previous x =
    if includes ~size previous x then (true, x) else (false, widen ~size ~previous x)

  let singleton ~size:_ i =
    if Z.equal i Z.zero then Zero
    else NonZero
end

type binary = Binary_Lattice.t

module Binary_Forward = struct
  open Binary_Lattice

  let biconst ~size:_ const =
    if Z.equal const Z.zero then Zero
    else NonZero

  let buninit ~size:_ = assert false

  let biadd ~size:_ ~nsw:_ ~nuw:_  ~nusw:_ x y =
    match x,y with
    | Bot,_ | _,Bot -> Bot
    | Zero,Zero -> Zero
    | NonZero,Zero | Zero,NonZero -> NonZero
    | _ -> SentinelTop

  let bisub ~size:_ ~nsw:_ ~nuw:_  ~nusw:_ x y =
    match x,y with
    | Bot,_ | _,Bot -> Bot
    | Zero,Zero -> Zero
    | NonZero,Zero | Zero,NonZero -> NonZero
    | _ -> SentinelTop

  let bimul ~size:_ ~nsw:_ ~nuw:_ x y =
    match x,y with
    | Bot,_ | _,Bot -> Bot
    | Zero,_ | _,Zero -> Zero
    | NonZero,NonZero -> NonZero
    | _ -> SentinelTop

  let bisdiv ~size:_ x y =
    match x,y with
    | Bot,_ | _,Bot | _,Zero -> Bot
    | Zero,_ -> Zero
    | NonZero,_ -> SentinelTop
    | SentinelTop, (NonZero|SentinelTop) -> SentinelTop

  let bismod ~size:_ x y =
    match x,y with
    | Bot,_ | _,Bot | _,Zero -> Bot
    | Zero,_ -> Zero
    | _ -> SentinelTop

  let biudiv = bisdiv
  let biumod = bismod

  let band ~size:_ x y =
    match x,y with
    | Bot,_ | _,Bot -> Bot
    | Zero,_ | _,Zero -> Zero
    | _ -> SentinelTop

  let bor ~size:_ x y =
    match x,y with
    | Bot,_ | _,Bot -> Bot
    | Zero,Zero -> Zero
    | NonZero,_ | _,NonZero -> NonZero
    | _ -> SentinelTop

  let bxor ~size:_ x y =
    match x,y with
    | Bot,_ | _,Bot -> Bot
    | Zero,Zero -> Zero
    | Zero,NonZero | NonZero,Zero -> NonZero
    | _ -> SentinelTop

  let bshl ~size:_ ~nsw:_ ~nuw:_ x y =
    match x,y with
    | Bot,_ | _,Bot -> Bot
    | Zero,_ -> Zero
    | NonZero,Zero -> NonZero
    | _ -> SentinelTop

  let bashr ~size:_ x y =
    match x,y with
    | Bot,_ | _,Bot -> Bot
    | Zero,_ -> Zero
    | NonZero,Zero -> NonZero
    | _ -> SentinelTop

  let blshr ~size:_ x y =
    match x,y with
    | Bot,_ | _,Bot -> Bot
    | Zero,_ -> Zero
    | NonZero,Zero -> NonZero
    | _ -> SentinelTop

  let bconcat ~size1:_ ~size2:_ x y =
    match x,y with
    | Bot,_ | _,Bot -> Bot
    | Zero,Zero -> Zero
    | NonZero,_ | _,NonZero -> NonZero
    | _ -> SentinelTop

  let beq ~size:_ x y =
    match x,y with
    | Bot,_ | _,Bot -> Lattices.Quadrivalent.Bottom
    | Zero,Zero -> Lattices.Quadrivalent.True
    | Zero,NonZero | NonZero,Zero -> Lattices.Quadrivalent.False
    | _ -> Lattices.Quadrivalent.Top

  let bisle ~size:_ x y =
    match x,y with
    | Bot,_ | _,Bot -> Lattices.Quadrivalent.Bottom
    | _ -> Lattices.Quadrivalent.Top

  let biule ~size:_ x y =
    match x,y with
    | Bot,_ | _,Bot -> Lattices.Quadrivalent.Bottom
    | Zero,_ -> Lattices.Quadrivalent.True
    | NonZero,Zero -> Lattices.Quadrivalent.False
    | _ -> Lattices.Quadrivalent.Top

  let buext ~size:_ ~oldsize:_ x =
    x
  let bsext ~size:_ ~oldsize:_ x =
    x
  let bextract ~size:_ ~index:_ ~oldsize:_ = function
    | Bot -> Bot
    | Zero -> Zero
    | _ -> SentinelTop

  let valid ~size:_ = assert false
  let valid_ptr_arith ~size:_ = assert false
  let bshift ~size:_ = assert false
  let bindex ~size:_ = assert false

  let bofbool ~size:_ =
    let open Lattices.Quadrivalent in function
    | Bottom -> Bot
    | False -> Zero
    | True -> NonZero
    | Top -> SentinelTop

  let bchoose ~size:_ _ = assert false
end

module Binary_Backward = struct
  open Binary_Lattice

  let beq ~size:_ x y res =
    match res with
    | Lattices.Quadrivalent.True ->
      begin match x,y with
      | Zero,SentinelTop -> None, Some Zero
      | SentinelTop,Zero -> Some Zero, None
      | NonZero,SentinelTop -> None, Some NonZero
      | SentinelTop,NonZero -> Some NonZero, None
      | Zero,Zero | NonZero,NonZero -> None,None
      | SentinelTop,SentinelTop -> None,None
      | Bot,_ | _,Bot -> None,None
      | NonZero,Zero | Zero,NonZero -> assert false
      end
    | Lattices.Quadrivalent.False ->
      begin match x,y with
      | SentinelTop,Zero -> Some NonZero, None
      | Zero,SentinelTop -> None, Some NonZero
      | Zero,Zero -> Some Bot, Some Bot
      | _ -> None,None
      end
     | _ -> None,None

  let biule ~size:_ x y res =
    match res with
    | Lattices.Quadrivalent.True ->
      begin match x,y with
      | SentinelTop,Zero -> Some Zero, None
      | NonZero,SentinelTop -> None, Some NonZero
      | _ -> None,None
      end
    | Lattices.Quadrivalent.False ->
      begin match x,y with
      | SentinelTop,Zero | SentinelTop,NonZero -> Some NonZero, None
      | _ -> None,None
      end
    | _ -> None,None

  let bisle ~size:_ x y res =
    match res with
    | Lattices.Quadrivalent.True -> None,None
    | Lattices.Quadrivalent.False ->
      begin match x,y with
      | SentinelTop,Zero -> Some NonZero, None
      | Zero,SentinelTop -> None, Some NonZero
      | _ -> None,None
      end
    | _ -> None,None

  let biadd ~size:_ ~nsw:_ ~nuw:_ ~nusw:_ x y = function
    | Zero ->
      begin match x,y with
      | Zero,SentinelTop -> None, Some Zero
      | SentinelTop,Zero -> Some Zero, None
      | NonZero,SentinelTop -> None, Some NonZero
      | SentinelTop,NonZero -> Some NonZero, None
      | _ -> None,None
      end
    | NonZero ->
      begin match x,y with
      | Zero,SentinelTop -> None, Some NonZero
      | SentinelTop,Zero -> Some NonZero, None
      | _ -> None,None
      end
    | _ -> None,None

  let bisub ~size:_ ~nsw:_ ~nuw:_ ~nusw:_ x y = function
    | Zero ->
      begin match x,y with
      | Zero,SentinelTop -> None, Some Zero
      | SentinelTop,Zero -> Some Zero, None
      | NonZero,SentinelTop -> None, Some NonZero
      | SentinelTop,NonZero -> Some NonZero, None
      | _ -> None,None
      end
    | NonZero ->
      begin match x,y with
      | Zero,SentinelTop -> None, Some NonZero
      | SentinelTop,Zero -> Some NonZero, None
      | _ -> None,None
      end
    | _ -> None,None

  let bimul ~size:_ ~nsw:_ ~nuw:_ x y = function
    | Zero ->
      begin match x,y with
      | NonZero,SentinelTop -> None, Some Zero
      | SentinelTop,NonZero -> Some Zero, None
      | _ -> None,None
      end
    | NonZero ->
      begin match x,y with
      | SentinelTop,SentinelTop -> Some NonZero, Some NonZero
      | NonZero,SentinelTop -> None, Some NonZero
      | SentinelTop,NonZero -> Some NonZero, None
      | _ -> None,None
      end
    | _ -> None,None

  let band ~size:_ _ _ = function
    | NonZero -> Some NonZero, Some NonZero
    | _ -> None,None

  let bor ~size:_ x y = function
    | Zero -> Some Zero, Some Zero
    | NonZero ->
      begin match x,y with
      | SentinelTop,Zero -> Some NonZero, None
      | Zero,SentinelTop -> None, Some NonZero
      | _ -> None,None
      end
    | _ -> None,None

  let bxor ~size:_ x y = function
    | Zero ->
      begin match x,y with
      | Zero,SentinelTop -> None, Some Zero
      | SentinelTop,Zero -> Some Zero, None
      | NonZero,SentinelTop -> None, Some NonZero
      | SentinelTop,NonZero -> Some NonZero, None
      | _ -> None,None
      end
    | NonZero ->
      begin match x,y with
      | Zero,SentinelTop -> None, Some NonZero
      | SentinelTop,Zero -> Some NonZero, None
      | _ -> None,None
      end
    | _ -> None,None

  let bisdiv ~size:_ x y = function
    | Zero -> Some Zero, None
    | NonZero -> Some NonZero, Some NonZero
    | Bot ->
      begin match x,y with
      | Bot,_ | _,Bot -> None,None
      | _ -> None, Some Zero
      end
    | _ -> None,None

  let biudiv ~size:_ x y = function
    | Zero -> Some Zero, None
    | NonZero -> Some NonZero, Some NonZero
    | Bot ->
      begin match x,y with
      | Bot,_ | _,Bot -> None,None
      | _ -> None, Some Zero
      end
    | _ -> None,None

  let bismod ~size:_ x y = function
    | NonZero -> Some NonZero, Some NonZero
    | Bot ->
      begin match x,y with
      | Bot,_ | _,Bot -> None,None
      | _ -> None, Some Zero
      end
    | _ -> None,None

  let biumod ~size:_ x y = function
    | NonZero -> Some NonZero, Some NonZero
    | Bot ->
      begin match x,y with
      | Bot,_ | _,Bot -> None,None
      | _ -> None, Some Zero
      end
    | _ -> None,None

  let bashr ~size:_ x y = function
    | Zero ->
      begin match x,y with
      | SentinelTop,Zero -> Some Zero, None
      | NonZero,SentinelTop -> None, Some NonZero
      | _ -> None,None
      end
    | NonZero -> Some NonZero, None
    | _ -> None,None

  let blshr ~size x y = bashr ~size x y

  let bshl ~size ~nsw:_ ~nuw:_ x y = bashr ~size x y

  let bsext ~size:_ ~oldsize:_ _ _ = (None)
  let buext ~size:_ ~oldsize:_ _ _ = (None)
  let bconcat ~size1:_ ~size2:_ _ _ _ = (None,None)
  let bextract ~size:_ ~index:_ ~oldsize:_ _ _ = (None)
  let valid ~size:_ _ _ _ = (None)
  let valid_ptr_arith ~size:_ _ _ _ = (None,None)

  let bshift ~size:_ ~offset:_ ~max:_ _ = assert false
  let bindex ~size:_ _ = assert false    

  let bofbool ~size:_ _bool =
    let open Lattices.Quadrivalent in function
    | Bot -> Some Bottom
    | Zero -> Some False
    | NonZero -> Some True
    | SentinelTop -> Some Top

  let bchoose ~size:_ _ = assert false
end

let binary_to_ival ~signed ~size =
  let open Binary_Lattice in function
  | Zero -> Framac_ival.Ival.of_int 0
  | NonZero ->
      if signed then Framac_ival.Ival.top
      else
        (* Range is [1,2^size-1]. *)
        Framac_ival.Ival.inject_range (Some Z.one)
          (Some (Z.sub (Z.shift_left Z.one size) Z.one))
  | SentinelTop -> Framac_ival.Ival.top
  | Bot -> Framac_ival.Ival.bottom

let binary_is_singleton ~size:_ =
  let open Binary_Lattice in function
  | Zero -> Some Z.zero
  | _ -> None

let binary_fold_crop ~size:_ = assert false
let binary_is_empty ~size:_ =
  let open Binary_Lattice in function
  | Bot -> true
  | _ -> false

let is_zero = function
| Binary_Lattice.(Bot | Zero) -> true
| Binary_Lattice.(NonZero | SentinelTop) -> false

let zero = Binary_Lattice.Zero
let nonzero = Binary_Lattice.NonZero

let binary_to_known_bits ~size =
  let open Binary_Lattice in
  function
  | Bot -> Lattices.Known_Bits.bottom ~size
  | Zero -> (Z.zero, Z.zero)
  | NonZero | SentinelTop -> Lattices.Known_Bits.top ~size
