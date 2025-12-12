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

(* http://burtleburtle.net/bob/hash/doobs.html *)
(* http://www.cse.yorku.ca/~oz/hash.html *)
(* let sdbm x y = x * 65599 + y *)
let [@inline always] sdbm x y = y + (x lsl 16) + (x lsl 6) - x

let [@inline always] hash2 x y = sdbm x y
let [@inline always] hash3 x y z = sdbm x (sdbm y z)
let [@inline always] hash4 x y z t = sdbm x (hash3 y z t)
let [@inline always] hash5 x y z t u = sdbm x (hash4 y z t u)

let [@inline always] hash_list f m = List.fold_left (fun acc elt -> sdbm (f elt) acc) 0 m

let [@inline always] hash_fast x = Obj.tag (Obj.repr x)

let [@inline always]  hash_sum ~nb ~total:_ hash = hash2 nb hash
