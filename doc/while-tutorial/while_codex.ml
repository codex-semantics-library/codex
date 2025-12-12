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

(** Executes the various while analyses on the provided example.
    Analysis and examples are selected via CLI options. *)

open Whilelib

let cinterpreter program =
  let init = Cinterpreter.initial_state () in
  let _ = Cinterpreter.interp_stmt program init in ()

let intv program =
  let init = Analysis_sva.initial_state () in
  let _ = Analysis_sva.analyze_stmt init program in ()

let intv_refine program =
  let init = Analysis_sva.initial_state () in
  let _ = Analysis_sva.analyze_stmt_refine init program in ()

let usage = "while_codex [--analysis NAME] [--example NAME]"

let codex program =
  let init = Some (While_analysis.initial_state ()) in
  let _ = While_analysis.analyze_stmt init program in ()

let analyzers = [
  ("cinterpreter", cinterpreter);
  ("intv", intv);
  ("intv_refine", intv_refine);
  ("codex", codex);
]

let enum_symbol list ref name doc =
  let keys = List.map fst list in
  name,
  Arg.Symbol (keys, fun key -> ref := List.assoc key list),
  doc

let main () =
  let analyzer = ref cinterpreter in
  let example = ref (List.hd While_examples.examples |> snd) in
  let speclist = [
    enum_symbol analyzers analyzer "--analyzer" " select which analyzer to run.";
    enum_symbol While_examples.examples example "--example" " select which example to run.";
  ] in
  let () = Arg.parse speclist (fun _ -> ()) usage in
  let () = Tracelog.set_verbosity_level `Info in
  !analyzer !example

let () = main ()
