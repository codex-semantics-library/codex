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

(* Standard abstraction for booleans; powerset of {true;false} *)

(* TODO: Should become Quadrivalent_Lattice. And should go into lattice? *)
type boolean = Boolean_standard.Quadrivalent.t = 
  | Bottom
  | True
  | False
  | Top

module BooleanDT = (struct
  type t = boolean
  let pretty fmt t =
    let string = match t with
      | Bottom -> "{}"
      | True -> "{true}"
      | False -> "{false}"
      | Top -> "{true;false}"
    in Format.fprintf fmt "%s" string

  let _equal = (==)

  let compare (a:boolean) (b:boolean) = Stdlib.compare a b
  let hash = function
    | Bottom -> 0
    | True -> 1
    | False -> 2
    | Top -> 3

end)

module Boolean_Lattice = struct
  include BooleanDT
  let bottom () = Bottom
  let boolean_bottom = bottom ()
  let is_bottom x = (x = Bottom)

  let top () = Top

  let singleton = function
    | true -> True
    | false -> False
  
  let truth_value x = x

  let of_bools ~may_be_false ~may_be_true =
    match (may_be_false,may_be_true) with
    | (false,false) -> Bottom
    | (false,true) -> True
    | (true,false) -> False
    | (true,true) -> Top

  (* Conversion to a couple (may_be_false,may_be_true) *)
  let to_bools = function
    | Bottom -> (false,false)
    | True -> (false,true)
    | False -> (true,false)
    | Top -> (true,true)

  let join a b = match (a,b) with
    | Bottom, x | x, Bottom -> x
    | False,False -> False
    | True, True -> True
    | True, False | False, True -> Top
    | Top, _ | _, Top -> Top

  let includes a b = match a,b with
    | Top,_ -> true
    | _, Bottom -> true
    | True,True | False,False -> true
    | _ -> false

  let widen ~previous b = join previous b

  let includes_or_widen ~previous b =
    if includes previous b then (true,b) else (false,join previous b)

  let inter a b = match (a,b) with
    | Bottom, _ | _, Bottom -> Bottom
    | Top, x | x, Top -> x
    | True, False | False, True -> Bottom
    | True, True -> True
    | False, False -> False
  ;;

end

let to_quadrivalent x = x

include Boolean_Lattice

let equal = (==)
