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

(** To avoid confusion between sizes in bits and bytes, we use
    separate types. *)
module type Unit = sig
  type t = private int
  val compare: t -> t -> int    
  val of_int : int -> t
  val to_int : t -> int
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( <= ) : t -> t -> bool
  val ( < ) : t -> t -> bool
  val ( >= ) : t -> t -> bool
  val ( > ) : t -> t -> bool
  val zero: t
  val one: t
end

(** Sizes and offsets in bits.  *)
module rec In_bits :sig
  include Unit
  val in_bytes : t -> In_bytes.t

  (** Doubling the size is useful to avoid overflow errors. *)  
  val double : t -> t

  val s32: t                    (* 32bit *)
end

(** Sizes and offsets in bytes.  *)
and In_bytes : sig
  include Unit
  val in_bits : t -> In_bits.t

  (** [times k x] is [k*x]. This is useful when multiplying array
      length with the size of array elements. Don't use this for
      conversion to sizes in bits. *)
  val times: int -> t -> t
end
