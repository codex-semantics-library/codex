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

module type Unit = sig
  type t = private int
  val compare: t -> t -> int    
  val of_int: int -> t
  val to_int: t -> int
  val (+): t -> t -> t
  val (-): t -> t -> t
  val ( <= ) : t -> t -> bool
  val ( < ) : t -> t -> bool
  val ( >= ) : t -> t -> bool
  val ( > ) : t -> t -> bool        
  val zero: t
  val one: t
end

module Common = struct
  type t = int
  let compare (a:int) (b:int) = compare a b
  let of_int x = x
  let to_int x = x
  let (+) = (+)
  let (-) = (-)            
  let (<) = (<)
  let (<=) = (<=)
  let (>) = (>)
  let (>=) = (>=)
  let double x = 2 * x
  let zero = 0
  let one = 1
end

module rec In_bits:sig
  include Unit
  val in_bytes: t -> In_bytes.t
  val double: t -> t
  val zero:t
  val s32:t
end = struct
  include Common
  let in_bytes x =
    assert(x land 7 = 0);
    assert(x >= 0);
    x lsr 3
  let s32 = 32
end

and In_bytes:sig
  include Unit
  val in_bits: t -> In_bits.t
  val times: int -> t -> t
end = struct
  include Common   
  let in_bits x =
    assert(x <= Int.max_int lsr 3);
    x lsl 3
  let times a b = a * b
end
