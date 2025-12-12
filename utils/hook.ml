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

type 'a hook = (string * ('a -> unit)) Stack.t

let add_hook h  ~name f = Stack.push (name,f) h
let run_hook h arg =
  h |> Stack.iter (fun (name,f) ->
      (* Printf.printf "Running hook %s\n" name; *)
      try f arg
  with exn ->
    Format.eprintf "ERROR: When running hook '%s': %s@."
    name (Printexc.to_string exn)
    )

(* let before_init = Stack.create() *)
(* let after_init = Stack.create()     *)
let startup = Stack.create()
let after_domain_build = Stack.create ()
let exit = Stack.create()

let () =
  Stdlib.at_exit (fun () -> run_hook exit ())
