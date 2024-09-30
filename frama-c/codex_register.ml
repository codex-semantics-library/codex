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

module Base = Frama_c_kernel.Base
module Cil = Frama_c_kernel.Cil
module Globals = Frama_c_kernel.Globals
module Kernel = Frama_c_kernel.Kernel
module Db = Frama_c_kernel.Db


module Log = Tracelog.Make(struct let category = "Codex_register" end);;

open Codex
module Domain_sig = Codex.Domains.Domain_sig
open Codex.Domains
  (* module A0 = struct include Domains.Simple_memory.Bare  ;; include To_add end *)
  (* module A1 = struct include Domains.Simple_memory.With_Bottom  ;; include To_add end *)

  (* module M = Domains.Finite_memory_symbolic_binary.Make(Domains.Simple_binary) *)
  (* module A2 = struct include M.Bare;; include To_add end *)
  (* module A3 = struct include M.With_Bottom;; include To_add end *)

  (* module FMArg = struct *)
  (*   include Domains.Eager_evaluating_basis.Make_Binary_Finite(Simple_binary_basis) *)
  (* end *)
  (* module M2 = Domains.Finite_memory.Make(FMArg) *)
  (* module A4 = struct include M2;; include To_add end *)

  (* module A4_With_Bottom = struct *)
  (*   include With_Bottom.Make_Memory_Forward(A4) *)
  (*   include To_add *)
  (* end *)

(* module Symbolic_Binary = Domains.Evaluating.Make_Memory_Binary_Boolean(Domains.Disjunctive_binary) *)


module FramaC_Log:Codex_log.S = struct
  type category = Codex_options.category

  let performance = Codex_options.register_category "performance";;
  let imprecision = Codex_options.register_category "imprecision";;
  let alarm = Codex_options.register_category "alarm";;
  let check = Codex_options.register_category "check";;
  let warning fmt = Codex_options.warning fmt
  let error fmt = Codex_options.error fmt
  let result fmt = Codex_options.result fmt
  let alarm txt = Codex_options.warning (* ~dkey:alarm  *) "%s" txt
  let check txt = Codex_options.feedback  ~dkey:check "%s" txt
  let performance_warning fmt = Codex_options.feedback fmt ~dkey:performance
  let imprecision_warning fmt = Codex_options.feedback fmt ~dkey:imprecision
  let debug ?level ?category fmt =
    let dkey = match category with
      | None -> None
      | Some c ->  Some(Codex_options.register_category c)
    in
    Codex_options.debug ?dkey fmt
  let fatal fmt = Codex_options.fatal fmt
  let feedback fmt = Codex_options.feedback fmt

end;;

module Tracelog_with_alarms = struct
  include Codex_log.Tracelog_Log

  let alarm = Frama_c_alarms.alarm
end
(* module A6 = struct *)
(*   include With_Bottom.Make_Memory_Forward(Domains.Finite_memory.Make(Domains.Disjunctive_binary))       *)
(*     (\* include With_Bottom.Make_Memory_Forward(Domains.Finite_memory.Make(Domains.Simple_binary)) *\) *)
(*   include To_add *)
(*   end *)

(* module A7 = struct *)

(*   (\* module Mem = (Domains.Finite_memory_symbolic_binary.Make(Domains.Disjunctive_binary)) *\) *)
(*   (\* include With_Bottom.Make_Memory_Forward(Domains.Finite_memory.Make(Domains.Disjunctive_binary)) *\) *)
(*   include With_Bottom.Make_Memory_Forward(Mem.With_Empty) *)
(*   include To_add *)
(* end *)


(* module Make_From_Binary(DD:(\* Finite_memory_sig.Finite_Memory_Value *\)Domain_sig.Base *)
(*                       with type Query.Boolean.t = Lattices.Quadrivalent.t) = struct *)
(*   (\* module TermD = Term_domain.Make(DD.Ref_Addr) *\) *)
(*   module Binary_With_Term = Prod_with_term(DD) *)
(*   include With_Bottom.Make_Memory_Forward(Finite_memory.Make(Binary_With_Term)) *)
(*   include To_add *)
(* end *)
(* module A8 = Make_From_Binary(Domains.Disjunctive_binary) *)

let abstract_domain x:(module Domain_sig.Base) = match x with
  (* | "0" -> (module A0:Domains.Evaluating.Arg) *)
  (* | "1" -> (module A1:Domains.Evaluating.Arg) *)
  (* | "2" -> (module A2:Domains.Evaluating.Arg) *)
  (* | "3" -> (module A3:Domains.Evaluating.Arg) *)
  (* | "6" -> (module A6:Domains.Evaluating.Arg) *)
  (* | "4" -> (module A4_With_Bottom:Domains.Evaluating.Arg) *)
  | _ -> assert false


let init_codex_library () =
  (* Codex_log.register (module FramaC_Log); *)
  (* Codex_log.register (module Codex_log.Tracelog_Log); *)
  Codex_log.register (module Tracelog_with_alarms);
  (* Rough correspondance between Frama-C command line and Tracelog verbosity levels. *)
  let verbosity_level = match Codex_options.Verbose.get(), Codex_options.Debug.get() with
    | _, x when x >= 1 -> `Debug
    | x, _ when x >= 2 -> `Info
    | 0, 0 | _ -> `Notice
  in Tracelog.set_verbosity_level verbosity_level;
  let max =   Z.of_string @@ Z.to_string @@ Base.max_valid_absolute_address () in
  let min =   Z.of_string @@ Z.to_string @@ Base.min_valid_absolute_address () in
  if Z.equal max Z.minus_one
  then ()
  else begin
    (* Revert the multiplication of addresses by 8 done by Frama-C.  *)
    let max = Z.succ max in
    assert(Z.equal Z.zero @@ Z.(land) min (Z.of_int 7));
    assert(Z.equal Z.zero @@ Z.(land) max (Z.of_int 7));
    let min = Z.shift_right min 3 in
    let max = Z.shift_right max 3 in
    Codex_log.feedback "Init codex library min=%s %s  max=%s" (Z.to_string min) (Z.to_string @@ Base.min_valid_absolute_address ()) (Z.to_string max);
    Codex_config.set_valid_absolute_addresses (min,max);
  end;
  Codex_config.set_ptr_size @@ Cil.bitsSizeOf Cil.voidPtrType;
  (* Codex_config.set_assume_simple_asts false; *)
;;

module Rels = Constraints.Relations
module Relation = Rels.Additive
module type CONSTRAINT_SIG = Constraints.Constraints_sig.Constraints
  with type ('a,'b) Relation.t = ('a,'b) Relation.t


(* Note: depends on the values of initialization, which may be muted,
   so we delay its creation. But putting all this code in a top-level
   functor improves performance (probably because of better
   inlining). *)
(* let build_domain():(module Domain_sig.Base) =  *)
let build_domain ~use_type_domain :(module With_focusing.S_with_types) =



  let module ConstraintsCudd =
    Constraints.Constraints.MakeConstraints
      (Constraints.Condition.ConditionCudd)
      (Relation)
      () in
  (* let module Constraints = *)
  (*   Constraints.Constraints.MakeConstraints(Constraints.Condition.ConditionDom)() in *)
  let module ConstraintsInt =
    Constraints.Constraints.MakeConstraints
      (Constraints.Condition.ConditionInt)
      (Relation)
      () in

  let module Constraints =
    (val
      if not @@ Codex_options.SparseNonRelationalDomain.get()
      then (module ConstraintsInt: CONSTRAINT_SIG)
      else (module ConstraintsCudd))
  in



  (* module NumericBasis = Basis.Ival;; *)
  let module BinaryBasis = Single_value_abstraction.Ival in
  (* module BinaryBasis = Bitwise_basis *)
  (* module BinaryBasis = Basis_prod.Prod_Binary(Single_value_abstraction.Ival)(Single_value_abstraction.Ival);; *)
  (* module Constraint_Domain' = Constraint_domain2_domains.Make(Constraints)(Single_value_abstraction.Ival)
   * module Constraint_Domain'' = Constraint_Domain'.Domain *)

  let module NumericBasis = Single_value_abstraction.Dummy.Complete_Binary(BinaryBasis) in

  (* module Apron_Domain = Domains_constraints_apron.Make(Constraints) *)

  (* module Prod = Domains_constraints_product.Make(Constraints)(Propag_Domain)(NonRel_Domain) *)
  (* module Prod_Domain = Domains_constraints_product.Make(Constraints)(Propag_Domain)(Apron_Domain) *)

  (* let module IntegerD = Constraint_domain.Make_Integer(NumericBasis) *)
  (* module IntegerD = Constraint_domain2.Make(Constraints)(NonRel_Domain) *)
  (* let module IntegerD = Constraint_domain2.Make(Constraints)(Propag_Domain) in *)
  (* module IntegerD = Constraint_domain2.Make(Constraints)(Apron_Domain) *)
  (* module IntegerD = Constraint_domain2.Make(Constraints)(Prod_Domain) *)
  (* let module Param = Lift_integer_domain_to_binary_domain.Make(IntegerD) in *)
  let module (* BinaryD1 *) SparseNonRelational = Constraint_domain2.Make(ConstraintsCudd)(Domains_constraints_constraint_propagation.Make(ConstraintsCudd)(NumericBasis)) in

  let module NonRelationConstraints = Domains_constraints_nonrelational.Make(Constraints)(NumericBasis)(Relation.Action(NumericBasis)) in
  let module WithAdditive = Domains_constraints_unionfind_builder.MakeAdditive(NonRelationConstraints) in
  let module WithOverflowGuards = Domains_constraints_overflow_check.Make(WithAdditive) in
  let module (* BinaryD2 *) DenseNonRelational = Constraint_domain2.Make(Constraints)(WithOverflowGuards) in

  let module NonRelational =
    (val
      if Codex_options.SparseNonRelationalDomain.get()
      then (module SparseNonRelational:Domain_sig.Base)
      else (module DenseNonRelational))
  in

  (* Do we use the loop domain. Depends on the kind of constraints. *)
  let use_loop_domain = Codex_options.UseLoopDomain.get() in
  let module Scalar =
    (val match Codex_options.SparseNonRelationalDomain.get(), use_loop_domain with
       | _, false -> (module NonRelational:Domain_sig.Base)
       | true, true -> (module Loop_domain.Make(ConstraintsCudd)(SparseNonRelational):Domain_sig.Base)
       | false,true -> (module Loop_domain.Make(Constraints)(DenseNonRelational):Domain_sig.Base))
  in

  (**************** Offset. ****************)
  let offset_domain = `Region_suffix_tree in

  (* All the Offset domains follow these restriction; we could lift it if needed. *)
  let module Offset_Sig = struct
    module type Sig =
      Memory_sig.Offset_Memory_domain
      with module Offset.Context = Scalar.Context
       and module Offset.Scalar = Scalar
       and type Offset.boolean = Scalar.boolean
  end in

  let module Region_numeric:Offset_Sig.Sig = Region_numeric_offset.Make(Scalar) in
  let module Region_suffix_tree_fully_expanded:Offset_Sig.Sig = Region_suffix_tree.MakeFullyExpanded(Scalar) in
  let module Region_suffix_tree:Offset_Sig.Sig = Region_suffix_tree.Make(Scalar) in


  let module Offset =
    (val match offset_domain with
    | `Region_suffix_tree -> (module Region_suffix_tree:Offset_Sig.Sig)
    | `Region_suffix_tree_fully_expanded -> (module Region_suffix_tree_fully_expanded:Offset_Sig.Sig)
    | `Region_numeric -> (module Region_numeric:Offset_Sig.Sig))
  in

  (**************** Address and above. ****************)

  let module MyRegion_separation = Region_separation.Make(Offset)(struct
      let ctx x = x,fun x -> x;;
      let serialize_binary = Scalar.serialize_binary
    end) in
  let module Wholified = Wholify.Make(MyRegion_separation)(struct
      let ctx x = x,fun x -> x;;
    end) in
  let module Wholified_with_union = Value_union_concatenation.Make(Wholified) in
  let module MemD = Wholified_with_union in
  let module Value = MemD.Address in
  let module Block = Value_to_region.MakeRegion (Value) in
  let module Memory = MemD.Memory(Block)(struct let ctx x = x,fun x -> x end) in

  (* TODO : Improve the parts with With_focusing signatures !! *)
  let module C_Whole_Program = struct
    include Memory_domain.Make(Value)(Block)(Memory)
    let flush_cache _ctx mem = mem
  end in

  let module D =
    (val
      if not use_type_domain
      then (module C_Whole_Program:With_focusing.Base_with_types)
      else (module
      struct

        (* Should work with the same MemD, but currently we loose precision if we use the wholified version. *)
        (* module Region_separation = Region_separation.Make(Region_suffix_tree) *)
        module rec Typed_address_domain : (Memory_sig.Memory_domain with module Address.Context = Scalar.Context and module Address.Scalar = Scalar) = Typed_address.Make(MyRegion_separation)(Value_to_region_domain.Address)
        and Wholified : (Memory_sig.Whole_Memory_domain with module Address.Context = Scalar.Context and module Address.Scalar = Scalar) = Wholify.Make(Typed_address_domain)(struct let ctx x = x,fun x -> x end)
        and Value_union_concatenation_domain : (Memory_sig.Whole_Memory_domain with module Address.Context = Scalar.Context and module Address.Scalar = Scalar) = Value_union_concatenation.Make(Wholified)
        and Value_to_region_domain : (Memory_sig.Complete_domain with module Address.Context = Scalar.Context and module Address.Scalar = Scalar) = Value_to_region.MakeDomain(Value_union_concatenation_domain)

        module MemD = Value_to_region_domain
        module Value = MemD.Address
        module Block = MemD.Block
        module Memory = MemD.Memory
        module Memory_Domain = Memory_domain.Make(Value)(Block)(Memory)
        (* include With_focusing *)
        include Memory_Domain
        let flush_cache _ctx mem = mem
        end:With_focusing.Base_with_types))
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

        let serialize_memory_and_cache ctxa mema ctxb memb entries acc =
          serialize_memory ctxa mema ctxb memb acc
      end in
      (module Without_Focusing:With_focusing.S_with_types)
    else
      (* let module With_Focusing = struct
        With_focusing.Make_with_types(Without_Focusing) in
      (module With_Focusing)) in *)
      let module Without_Focusing = struct
        include With_focusing.Make_with_types(Without_Focusing)

        let serialize_memory_and_cache ctxa mema ctxb memb entries acc =
          if Codex_options.SerializeCache.get() then
            serialize_memory_and_cache ctxa mema ctxb memb entries acc
          else serialize_memory ctxa mema ctxb memb acc
      end in
      (module Without_Focusing:With_focusing.S_with_types)) in
  (module Final)
;;

let run () =
  init_codex_library();

  if Codex_options.Enabled.get()
  then
    let kf =
      let entry_func_name = (Kernel.MainFunction.get()) in
      try Globals.Functions.find_by_name entry_func_name
      with Not_found ->
        Log.fatal (fun p -> p "Could not find function named `%s'"
                      entry_func_name)
    in


    (* let module Tmp = Domain_for_c.Make() in *)
    (* let module Analyzed_Domain2 = (val Codex.Domain_for_c.v:Domain_sig.Base) in *)
    (* let module Analyzed_Domain = Codex.Domain_for_c.Analyzed_Domain in *)

    (* let module To_SMT_Domain = A8.Binary_With_Term.TermD in *)
    (* let module To_SMT_Domain = A8.Binary_With_Term.TermD in     *)

    (* Instead of taking to parameters, To_SMT_Domain should be a
       parameter to a functor inside Run_For(Analyzed_Domain). *)
    (* let module Instance = Run_For(Analyzed_Domain) in *)
    let use_type_domain = Codex_options.UseTypeDomain.get() in
    let module Analyzed_Domain = (val build_domain ~use_type_domain) in
    (* let module Analyzed_Domain_Standard =  (val build_domain()) in *)
    (* let module DA = Direct_analysis.Analyze(Analyzed_Domain_Standard) in *)
    let module DA = Direct_analysis.Analyze(Analyzed_Domain) in
    DA.run kf;


    (* let (term,assertions,reachable_table,exp_to_term) = Compilation_to_term.compile kf in *)
    (* Codex_options.feedback "compilation done"; *)
    (* Gc.full_major(); *)
    (* (if Codex_options.Print.get() then *)
    (*   match term with *)
    (*   | None -> *)
    (*     Codex_options.feedback "Term at the end of %s function: bottom" (Kernel.MainFunction.get()) *)
    (*   | Some term -> *)
    (*     Codex_options.feedback "Term at the end of %s function: %a" *)
    (*       (Kernel.MainFunction.get()) Term.pretty term); *)

    (* let module Instance = Run_For(Analyzed_Domain) in *)


    (* Instance.run exp_to_term term assertions reachable_table; *)

    flush_all()

;;

let run = Log.fatal_handler run;;


Db.Main.extend run;;
