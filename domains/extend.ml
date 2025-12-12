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

module Make(D:Sig.Minimal) = struct

  let imperative_assume ctx bool =
    let ctx' = D.assume ctx bool in
    match ctx' with
    | None -> assert false
    | Some ctx' -> D.Context.assign ctx ctx'

end

module MakeForAADT(D:sig
    module Scalar:Sig.BASE
    include Memory_sig.WITH_BOOLEAN_REDEFINITION with module Context := Scalar.Context
  end) = struct

  let imperative_assume ctx bool =
    let ctx' = D.assume ctx bool in
    match ctx' with
    | None -> assert false
    | Some ctx' -> D.Scalar.Context.assign ctx ctx'


  
end
