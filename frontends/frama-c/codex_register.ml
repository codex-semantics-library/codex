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

module Base = Frama_c_kernel.Base
module Cil= Frama_c_kernel.Cil
module Cil_const = Frama_c_kernel.Cil_const
module Globals = Frama_c_kernel.Globals
module Kernel = Frama_c_kernel.Kernel
module Boot = Frama_c_kernel.Boot


module Log = Tracelog.Make(struct let category = "Codex_register" end);;

open Codex

module Tracelog_with_alarms = struct
  include Codex_log.Tracelog_Log
  let alarm x = Emit_alarm.emit_alarm x
end

module CodexLibraryInitializer = struct
  let initialize_logging () =
    Codex_log.register (module Tracelog_with_alarms)

  let set_verbosity_level () =
    (* Rough correspondance between Frama-C command line and Tracelog verbosity levels. *)
    let verbose_lvl = Codex_options.Verbose.get() in
    let debug_lvl = Codex_options.Debug.get() in
    let verbosity_level = match (verbose_lvl, debug_lvl) with
      | _, x when x >= 1 -> `Debug
      | x, _ when x >= 2 -> `Info
      | 0, 0 | _ -> `Notice
    in Tracelog.set_verbosity_level verbosity_level

  let initialize_machine_dependant_constants () =
    Codex_config.set_ptr_size @@ (Cil.bitsSizeOf Cil_const.voidPtrType |> Units.In_bits.of_int);
    Codex_config.set_back_propagation_limit (Codex_options.BackPropagationLimit.get ())

  let initialize_data_model () =
    let data_model =
      match Cil.bitsSizeOf Cil_const.voidPtrType with
      | 32 -> `ILP32
      | 64 -> begin match Cil.bitsSizeOf Cil_const.longType with
          | 32 -> `LLP64
          | 64 -> `LP64
          | _ -> failwith "Unknown data model"
        end
      | _ -> failwith "Unknown data model"
    in Types.Parse_ctypes.init ~data_model

  let initialize_absolute_addresses () =
    (* Initialize the parts of memory that is directly accessible "absolute addresses" *)
    let max = Z.of_string @@ Z.to_string @@ Base.max_valid_absolute_address () in
    let min = Z.of_string @@ Z.to_string @@ Base.min_valid_absolute_address () in
    if Z.equal max Z.minus_one then ()
    else begin
      (* Revert the multiplication of addresses by 8 done by Frama-C.  *)
      let max = Z.succ max in
      assert(Z.equal Z.zero @@ Z.(land) min (Z.of_int 7));
      assert(Z.equal Z.zero @@ Z.(land) max (Z.of_int 7));
      let min = Z.shift_right min 3 in
      let max = Z.shift_right max 3 in
      Codex_log.feedback "Init codex library min=%s %s  max=%s" (Z.to_string min) (Z.to_string @@ Base.min_valid_absolute_address ()) (Z.to_string max);
      Codex_config.set_valid_absolute_addresses (min,max);
    end

  let initialize_terms_pretty_printing () =
    if not @@ Codex_options.VariableDisplay.is_default()
    then Codex.Domains.Term_domain.set_pretty_terms (Codex_options.VariableDisplay.get ())

  let initialize_hooks () = Codex.Hook.(run_hook startup ())
end

let init_codex_library () =
  let initialize = (fun () ->
    CodexLibraryInitializer.(
      initialize_logging ();
      set_verbosity_level ();
      (* TODO: Some (all?) of these arguments should be passed as
         arguments to functors when they are instantiated. *)
      initialize_machine_dependant_constants ();
      initialize_data_model ();
      initialize_absolute_addresses ();
      initialize_terms_pretty_printing ();
      initialize_hooks ()
      (* Codex_config.set_assume_simple_asts false; *)
    ))
  in
  Log.trace (fun p -> p "init_codex_library") initialize


(***************************************************************)
(**************** Building the abstract domain. ****************)
(***************************************************************)


module Relation = Terms.Relations.LinearTwoVarEquality
module type TERMS = Terms.Sig.TERMS
  with type ('a,'b) Relation.t = ('a,'b) Relation.t

module Memory = Codex.Domains.Memory_domains

(* Note: depends on the values of initialization, which may be muted,
   so we delay its creation. But putting all this code in a top-level
   functor improves performance (probably because of better
   inlining). *)
(* let build_domain():(module Domain_sig.Base) =  *)
let build_domain ~use_type_domain :(module Memory.With_focusing.S_with_types) =
  let open Codex.Domains in

  let module TermsCudd =
    Terms.Builder.Make
      (Terms.Condition.ConditionCudd)
      (Relation)
      () in
  (* let module Terms = *)
  (*   Terms.Terms.MakeConstraints(Terms.Condition.ConditionDom)() in *)
  let module TermsInt =
    Terms.Builder.Make
      (Terms.Condition.ConditionInt)
      (Relation)
      () in

  let module Terms =
    (val
      if not @@ Codex_options.SparseNonRelationalDomain.get()
      then (module TermsInt: TERMS)
      else (module TermsCudd))
  in



  (* module NumericBasis = Basis.Ival;; *)
  let module BinaryBasis = Single_value_abstraction.Ival in
  let module EnumBasis = Single_value_abstraction.Bitfield in

  let module BinaryEnumBasis = struct
    module Log = Tracelog.Make(struct let category = "NumericBasis" end)
    include BinaryBasis
    include EnumBasis
  end in
  (* module BinaryBasis = Bitwise_basis *)
  (* module BinaryBasis = Basis_prod.Prod_Binary(Single_value_abstraction.Ival)(Single_value_abstraction.Ival);; *)
  (* module Constraint_Domain' = Constraint_domain2_domains.Make(Terms)(Single_value_abstraction.Ival)
   * module Constraint_Domain'' = Constraint_Domain'.Domain *)

  (* let module NumericBasis = Single_value_abstraction.Dummy.Complete_Binary(BinaryBasis) in *)
  let module NumericBasis = BinaryEnumBasis in
  let module NumericBasisLog = NumericBasis.Log in
  let compute_sva_statistics = Sys.getenv_opt "CODEX_COMPUTE_STATS" in
  let module NumericBasis =
    (val match compute_sva_statistics with
      | None | Some "false" | Some "0" -> (module NumericBasis:Single_value_abstraction.Sig.NUMERIC_ENUM)
      | _ -> (module Single_value_abstraction.Stats.Make(NumericBasis)))
  in
  let module NumericBasis = Single_value_abstraction.Log.Log_Numeric_Enum(NumericBasisLog)(NumericBasis) in

  (* module Apron_Domain = Domains_constraints_apron.Make(Terms) *)

  (* module Prod = Domains_constraints_product.Make(Terms)(Propag_Domain)(NonRel_Domain) *)
  (* module Prod_Domain = Domains_constraints_product.Make(Terms)(Propag_Domain)(Apron_Domain) *)

  (* let module IntegerD = Constraint_domain.Make_Integer(NumericBasis) *)
  (* module IntegerD = Constraint_domain2.Make(Terms)(NonRel_Domain) *)
  (* let module IntegerD = Constraint_domain2.Make(Terms)(Propag_Domain) in *)
  (* module IntegerD = Constraint_domain2.Make(Terms)(Apron_Domain) *)
  (* module IntegerD = Constraint_domain2.Make(Terms)(Prod_Domain) *)
  (* let module Param = Lift_integer_domain_to_binary_domain.Make(IntegerD) in *)
  let module (* BinaryD1 *) SparseNonRelational = Term_domain.Make(TermsCudd)(Term_based.Propagation.Make(TermsCudd)(NumericBasis)) in

  let module NonRelationTerms = Term_based.Nonrelational.MakeUF(Terms)(NumericBasis)(Relation.Action(NumericBasis)) in
  (* Packaged modules don't support "with module F = F" constraints, so we need this hack.
     https://discuss.ocaml.org/t/module-type-constraints-in-first-class-modules/16155 *)
  let open (struct module type DOMAIN = Term_based_sig.Domain_S with module Terms = Terms end) in
  let module NonRel = (val
    if Codex_options.UseUnionFind.get ()
    then (module Term_based.Union_find.MakeLinearTwoVarEquality(NonRelationTerms))
    else (module NonRelationTerms)
    : DOMAIN
  ) in
  (* let module WithOverflowGuards = Domains_constraints_overflow_check.Make(WithAdditive) in *)
  let module (* BinaryD2 *) DenseNonRelational = Overflow_checks.Make(Term_domain.Make(Terms)(NonRel)) in

  let module NonRelational =
    (val
      if Codex_options.SparseNonRelationalDomain.get()
      then (module SparseNonRelational: Domains.Sig.BASE_WITH_INTEGER)
      else (module DenseNonRelational))
  in

  let module LiftedFromInteger = Integer2binary.Make(NonRelational) in
  let module NonRelational =
    (val
      if true                   (* TODO: An option lift_from_integer. *)
      then (module NonRelational: Domains.Sig.BASE)
      else (module LiftedFromInteger: Domains.Sig.BASE))
  in
  let module NonRelational = Log.Log_Domain(Tracelog.Make(struct let category = "NumericDomain" end))(NonRelational) in

  (* Do we use the loop domain. Depends on the kind of constraints. *)
  let use_loop_domain = Codex_options.UseLoopDomain.get() in
  let module Scalar =
    (val match Codex_options.SparseNonRelationalDomain.get(), use_loop_domain with
       | _, false -> (module NonRelational: Domains.Sig.BASE)
       | true, true -> (module Loop_domain.Make(TermsCudd)(SparseNonRelational): Domains.Sig.BASE)
       | false,true -> (module Loop_domain.Make(Terms)(DenseNonRelational): Domains.Sig.BASE))
  in

  let module Scalar =
    (val
      if Codex_options.BitwiseDomain.get()
      then (module Bitwise.Make(Scalar): Domains.Sig.BASE)
      else (module Scalar))
  in

  (**************** Offset. ****************)
  let offset_domain = `Region_suffix_tree in

  (* All the Offset domains follow these restriction; we could lift it if needed. *)
  let module OFF_MK_B = struct
    module type SIG =
      Memory_sig.OFFSET_AND_MAKE_BLOCK
      with module Scalar = Scalar
       and type Offset.boolean = Scalar.boolean
  end in

  let module Region_numeric:OFF_MK_B.SIG = Memory.Region_numeric_offset.Make(Scalar) in
  let module Region_suffix_tree_fully_expanded:OFF_MK_B.SIG = Memory.Region_suffix_tree.MakeFullyExpanded(Scalar) in
  let module Region_suffix_tree:OFF_MK_B.SIG = Memory.Region_suffix_tree.Make(Scalar) in


  let module Offset_and_Mk_Block : OFF_MK_B.SIG =
    (val match offset_domain with
    | `Region_suffix_tree -> (module Region_suffix_tree:OFF_MK_B.SIG)
    | `Region_suffix_tree_fully_expanded -> (module Region_suffix_tree_fully_expanded:OFF_MK_B.SIG)
    | `Region_numeric -> (module Region_numeric:OFF_MK_B.SIG))
  in

  (**************** Address and above. ****************)

  let module MyRegion_separation = Memory.Region_separation.Make(Offset_and_Mk_Block) in
  let module Wholified = Memory.Wholify.Make(MyRegion_separation) in
  let module Wholified_with_union = Memory.Value_union_concatenation.Make(Wholified) in
  let module MemD = Wholified_with_union in
  let module Value = MemD.Address in
  let module Block_smashing_domain = Memory.Block_smashing.Make (Value)(Offset_and_Mk_Block.Offset) in
  let module Flexible_array_member_domain = Memory.Flexible_array_member.MakeComplete (MemD)(Block_smashing_domain) in

  (* TODO : Improve the parts with With_focusing signatures !! *)
  let module C_Whole_Program = struct
    module MemD = Flexible_array_member_domain
    module Value = MemD.Value
    module Block = MemD.Block
    module Memory = MemD.Memory
    include Domains.Memory_domains.Memory_domain.Make(Flexible_array_member_domain.Value)(Block)(Memory)
    let flush_cache _ctx mem = mem
  end in
  let module D =
    (val if not use_type_domain then
           (module C_Whole_Program : Memory.With_focusing.BASE_WITH_TYPES)
         else
           (module struct
             (* Should work with the same MemD, but currently we loose precision if we use the wholified version. *)
             (* module Region_separation = Region_separation.Make(Region_suffix_tree) *)
             module rec Typed_address_domain :
               (Memory_sig.ADDRESS_AND_MAKE_MEMORY
                with module Scalar = Scalar) =
               Memory.Typed_address.Make
                 (MyRegion_separation)
                 (Flexible_array_member_domain.Value)

             and Wholified :
               (Memory_sig.WHOLE_MEMORY_DOMAIN
                with module Scalar = Scalar) =
               Memory.Wholify.Make
                 (Typed_address_domain)

             and Value_union_concatenation_domain :
               (Memory_sig.WHOLE_MEMORY_DOMAIN
                with module Scalar = Scalar) =
               Memory.Value_union_concatenation.Make (Wholified)

             and Flexible_array_member_domain :
               (Memory_sig.COMPLETE_DOMAIN
                with module Scalar = Scalar) =
               Memory.Flexible_array_member.MakeComplete
                 (Value_union_concatenation_domain)
                 (Memory.Block_smashing.Make
                    (Value_union_concatenation_domain.Address)
                    (Value_union_concatenation_domain.Offset))

             module MemD = Flexible_array_member_domain
             module Value = MemD.Value
             module Block = MemD.Block
             module Memory = MemD.Memory
             module Memory_Domain = Domains.Memory_domains.Memory_domain.Make (Value) (Block) (Memory)

             (* include With_focusing *)
             include Memory_Domain

             let flush_cache _ctx mem = mem
           end : Memory.With_focusing.BASE_WITH_TYPES))
  in
  let module Analyzed_Domain = D in
  let module Without_Focusing = struct
    include Analyzed_Domain
  end in



  let module Final = (val
    if not use_type_domain
    then
      let module Without_Focusing = struct
        include Without_Focusing

        let analyze_summary ctx funtyp args mem =
          let res,ret = analyze_summary ctx funtyp args in res,ret,mem

        let serialize_memory_and_cache ~widens ctxa mema ctxb memb _ acc =
          serialize_memory ~widens ctxa mema ctxb memb acc
      end in
      (module Without_Focusing: Memory.With_focusing.S_with_types)
    else
      (* let module With_Focusing = struct
        With_focusing.Make_with_types(Without_Focusing) in
      (module With_Focusing)) in *)
      let module Without_Focusing = struct
        include Memory.With_focusing.Make_with_types(Without_Focusing)

        let serialize_memory_and_cache ~widens ctxa mema ctxb memb entries acc =
          if Codex_options.SerializeCache.get() then
            serialize_memory_and_cache ~widens ctxa mema ctxb memb entries acc
          else serialize_memory ~widens ctxa mema ctxb memb acc
      end in
      (module Without_Focusing: Memory.With_focusing.S_with_types)) in
  let module Final = Domains.Log.Log_With_Focusing(Tracelog.Make(struct let category = "TopDomain" end))(Final) in
  (module Final)


let build_domain ~use_type_domain :(module Memory.With_focusing.S_with_types) =
  Log.trace (fun p -> p "build_domain") @@ fun () -> build_domain ~use_type_domain


(***********************************************)
(**************** Running Codex ****************)
(***********************************************)


let run () =
  if Codex_options.HtmlDump.get() then
    (Printf.printf "set_log_binary_trace true";
     Binarytrace.init();
     Tracelog.set_log_binary_trace true);
  init_codex_library ();
  let svcomp_expected_properties =
    Codex_options.SVComp_expected_properties.get ()
  in
  let rec loop_over_svcomp_properties = function
    | [] -> ()
    | hd :: tl ->
      let open Codex_options in
      (match hd with
       | No_overflow, _ -> Codex_options.OverflowAlarms.set true
       | property, _ -> Log.warning (fun p -> p "Unsupported SV-COMP property: %a" Codex_options.pp_svcomp_property property));
      loop_over_svcomp_properties tl
  in loop_over_svcomp_properties svcomp_expected_properties;

  (* let module Tmp = Domain_for_c.Make() in *)
  (* let module Analyzed_Domain2 = (val Codex.Domain_for_c.v:Domain_sig.Base) in *)
  (* let module Analyzed_Domain = Codex.Domain_for_c.Analyzed_Domain in *)

  (* let module To_SMT_Domain = A8.Binary_With_Term.TermD in *)
  (* let module To_SMT_Domain = A8.Binary_With_Term.TermD in     *)

  (* Instead of taking to parameters, To_SMT_Domain should be a
     parameter to a functor inside Run_For(Analyzed_Domain). *)
  (* let module Instance = Run_For(Analyzed_Domain) in *)
  let use_type_domain = Codex_options.UseTypeDomain.get () in
  let module Analyzed_Domain = (val build_domain ~use_type_domain) in
  Hook.run_hook Hook.after_domain_build ();

  let parse_ctypes () =
    Log.trace (fun p -> p "parce_ctypes") @@ fun () ->
    let file = Codex_options.TypeConfigurationFile.get() in
    let infer_spec = Codex_options.InferTypes.get () in
    if file = "" then () else Types.Parse_ctypes.parse_file ~infer_spec file
  in
  if use_type_domain then parse_ctypes ();
  
  (* let module Analyzed_Domain_Standard =  (val build_domain()) in *)
  (* let module DA = Direct_analysis.Analyze(Analyzed_Domain_Standard) in *)
  let instantiate_and_dump suffix f =
    (* Instantiate a new "direct analysis". *)
    let module DA = Direct_analysis.Analyze (Analyzed_Domain) in

    (* Analyze with it. *)
    f DA.run;
    
    (* Dump the alarms and assertions with exp_dump if it exists. *)
    (match Codex_options.ExpDump.is_set () with
     | false ->
       DA.dump_alarms stdout;
       DA.dump_assertions stdout
     | true ->
       let prefix = Codex_options.OutputPrefix.get () in
       (*if we analyse only one function, use it as a name for cdump*)
       let file = prefix ^ suffix in
       let outc = open_out file in
       DA.dump_exp_table outc;
       DA.dump_alarms outc;
       DA.dump_assertions outc;
       if Codex_options.ExtraMetrics.get () then DA.dump_extra outc;
       if Codex_options.GcStats.get () then DA.dump_gc outc;
       close_out outc);
  in

    

  let get_func_name entry_func_name =
    try (Globals.Functions.find_by_name entry_func_name)
    with Not_found ->
      Log.fatal (fun p ->
          p "Could not find function named `%s'" entry_func_name)
  in

  let functions_to_analyze =
    let functions_to_analyze = 
      if Kernel.MainFunction.is_set () then [ Kernel.MainFunction.get () ]
      else if Codex_options.AnalyzeFunctions.is_set () then
        Codex_options.AnalyzeFunctions.get ()
      else Types.TypedC.get_function_names ()
    in
    match functions_to_analyze with
    | [] ->
      Log.fatal (fun p ->
          p
            "You need to specify functions to analyze (in the type files, \
             or using option -codex-analyze-functions")
      (*we dont try to catch errors when analysing a single entry
        point, this is mostly to ensure legacy CI compatibility*)
    | l -> l
  in
  let exit_code = ref 0 in

  let run_one_function da_run f = 
    try da_run @@ get_func_name f
    with e ->
      let backtrace = Printexc.get_backtrace() in
      exit_code := 1;
      Log.error (fun p -> p "The analysis of %s crashed with error: %s\nBacktrace:\n%s"
                    f (Printexc.to_string e) backtrace)
  in
  let run_on_all_functions() =
    let aggregate_results = false in 
    if aggregate_results then
      instantiate_and_dump ".cdump"
        (fun da_run ->
          List.iter (run_one_function da_run) functions_to_analyze)
    else
      functions_to_analyze |> List.iter (fun f ->
          instantiate_and_dump (f ^ ".cdump") (fun da_run -> run_one_function da_run f))
  in

  (if Codex_options.InferTypes.get()
   then
     let module PA =
       Post_analysis.Make(struct let runner = run_on_all_functions end) in
     PA.runner()
   else
     run_on_all_functions());

  flush_all ();
  exit !exit_code

let run() =
  if Codex_options.Enabled.get () then run()

let run = Log.fatal_handler run
let () = Boot.Main.extend run

(* TODO: The processing of every option should be done here.

   We should have "C dialect options" to specify which UB we want to
   analyze. There are not a lot of other options (maybe the way to
   perform fixpoint computation). *)
