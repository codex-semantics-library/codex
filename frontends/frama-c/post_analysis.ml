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

module Log = Tracelog.Make (struct
  let category = "Post_analysis"
end)

module TypedC = Types.TypedC
module Type_check_tree = Types.Type_check_tree

type c_function = Frama_c_kernel.Kernel_function.t

module type Runner = sig
  val runner : unit -> unit
end

module Fixpoint (R : Runner) : Runner = struct
  open Type_check_tree
  open TypedC

  let rec increment_t : type a. a t -> bool =
   fun t -> find_first_error t (fun typ _ -> increment_type typ)

  and increment_type typ =
    let typ = inlined typ in
    Log.trace
      (fun p -> p "increment_type %a" pp typ)
      ~pp_ret:(fun p b ->
        Format.fprintf p "%a:%a" Format.pp_print_bool b pp typ)
    @@ fun () -> increment_pred typ.pred

  and increment_pred =
    let open TypedC.Pred in
    function
    | Mutval (({ value = true } as mut), pred) ->
        mut.value <- false;
        false
    | _ -> true

  let increment iteration_count =
    let dir = "infered_spec" in
    if not (Sys.file_exists dir) then Sys.mkdir dir 0o755;
    fun r ->
      Format.printf "ITERATION NUMBER %i\n" iteration_count;
      let oc =
        open_out @@ Format.asprintf "%s/iteration_%i.typedc" dir iteration_count
      in
      let spec_fmt = Format.formatter_of_out_channel oc in
      let ctype_spec =
        Format.asprintf "/* ITERATION NUMBER %i */\n\n\n%a" iteration_count
          TypedC.print_spec ()
      in
      Format.fprintf spec_fmt "%s" ctype_spec;
      close_out oc;
      Log.debug (fun p ->
          p "%s@;Starting iteration number %i@.%!" ctype_spec iteration_count);

      (*all of whats above is just setup*)
      (*below is the real deal*)
      r ();
      Log.debug (fun p -> p "%i ERRORS\n%!" (number_of_errors ()));
      for_all_logs { f = increment_t }

  let runner () =
    let rec runner_counting i =
      if i >= 100 then exit 2
      else if increment i R.runner then
        Format.printf "ANALYSIS DONE with %i iterations!\n" i
      else (
        reset ();
        runner_counting (i + 1))
    in
    runner_counting 0
end

module Make (R : Runner) : Runner = Fixpoint (R)
