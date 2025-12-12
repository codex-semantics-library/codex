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

type (_, _) eq = Refl : ('a, 'a) eq

type 'a tags = ..
module type TAG = sig
  type t
  type 'a tags += Tag : t tags
end
type 'a t = (module TAG with type t = 'a)

let create (type a) () : a t =
  let module T = struct
    type t = a
    type _ tags +=
      | Tag : a tags
  end in
  (module T)

let equal (type a b) ((module A) : a t) ((module B) : b t) : (a, b) eq =
  match A.Tag with
  | B.Tag -> Refl
  | _ -> failwith "Runtime tags differ"
