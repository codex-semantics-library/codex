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

type t =
  | Show_each_in of string
;;

  let hash b = match b with
    | Show_each_in s -> 997 * Hashtbl.hash s

  let equal a b = match a,b with
    | Show_each_in s1, Show_each_in s2 -> s1 = s2

  let pretty fmt x = match x with
    | Show_each_in s -> Format.fprintf fmt "show_each%s" s


module type With_Builtin = sig
  type binary
  type memory
  type boolean

  module Context:sig type t end
  
  (* Note: we provide the size of the binaries too. *)
  val builtin_show_each_in: string -> Context.t -> (int * binary) list -> memory -> memory
  
end



(* Default implementation for all builtins.
   TODO: Should be implemented in the Codex language. *)
module Make
    (Types:sig
       type binary
       type memory
       type boolean
     end)
    (Context:sig type t end):
  With_Builtin with type binary := Types.binary
                and type memory := Types.memory
                and type boolean := Types.boolean
                and module Context := Context
= struct
  include Types

  let builtin_show_each_in _ = assert false

end
