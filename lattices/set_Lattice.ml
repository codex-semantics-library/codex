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

open Lattice_sig

module type S = sig
  include Set.S
  include Join_Semi_Lattice with type t := t
  include With_Bottom with type t := t
  val is_bottom: t -> bool
  include With_Inter with type t := t
  val eq: t -> t -> Quadrivalent_Lattice.t
  val intersects: t -> t -> bool
end

module Make(E:Datatype_sig.S) = struct
  include Datatype_sig.Set(E)
  (* let inter = E.Set.inter *)
  let is_bottom = is_empty
  let bottom = empty
  let join = union
(* let is_included = subset *)
  let includes a b = subset b a
  let widen ~previous x = join previous x

  let includes_or_widen ~previous next =
    if includes previous next then (true,previous)
    else (false,widen ~previous next)

  (* let join_and_is_included a b =  *)
  (*   let ab = join a b in *)
  (*   (ab,E.Set.equal ab b) *)

  let eq a b =
    (* let open Lattice_sig in *)
    if is_empty (inter a b)
    then Quadrivalent_Lattice.False
    else let ca = choose a and cb = choose b in
         if E.equal ca cb && is_empty (remove ca a) && is_empty (remove cb b)
         then Quadrivalent_Lattice.True
         else Quadrivalent_Lattice.Top

  (* Could be made more efficient if we add access to the underlying
     implementation. *)
  let intersects a b = not (is_empty (inter a b));;

           
end
