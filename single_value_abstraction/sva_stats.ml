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

module In_bits = Units.In_bits
module IntMap = Map.Make(Int)

module Log = Tracelog.Make(struct let category = "sva_stats" end)

(** Aggregated statistics for each transfer function.  *)
type func_stats = {
  nb_args: int;    (** How many arguments does the function take. *)
  nb_calls: int;   (** How many times the function was called. *)
  nb_constant_returns: int;   (** How many times the return value was constant. *)
  nb_constant_calls: int IntMap.t; (** For each nb_constant, how many calls with that number of constant args.  *)
  return_size: int IntMap.t option; (** For each size, how many calls returning that size, if applicable.  *)
  total_time: (int, Stats.compact) Stats.t; (** Total time spend in this function, in microseconds, across all calls *)
}

let micro_units = "µ" :: "m" :: Stats.unit_prefixes
let micro_units_with_s = List.map (fun x -> x^"s") micro_units
let nano_units = "n" :: micro_units

(** Print individual stats for each function in individual order *)
let print_stats fmt runs =
  (* Dump in alphabetical order. *)
  let print_percent = Stats.pp_percent ~justify:false () in
  let pp_micro = Stats.pp_with_unit ~unit_prefixes:micro_units ~separator:" " () in
  let pp_nano = (Stats.pp_with_unit ~unit_prefixes:nano_units ~separator:" " ()) in
  List.iter (fun (name, stats) ->
      Format.fprintf fmt "%s:@,  @[<v>" name;
      Format.fprintf fmt "nb_args: %d@," stats.nb_args;
      Format.fprintf fmt "nb_calls: %d@," stats.nb_calls;
      Format.fprintf fmt "nb_constant_returns: %d (%a)@," stats.nb_constant_returns
        print_percent (stats.nb_constant_returns,stats.nb_calls);
      stats.nb_constant_calls |> IntMap.iter (fun nb calls ->
          Format.fprintf fmt "calls with %d constant args: %d (%a)@,"
            nb calls print_percent (calls,stats.nb_calls));
      Option.iter (IntMap.iter (fun size calls ->
            Format.fprintf fmt "calls returning a bitvector of size %d: %d (%a)@,"
              size calls print_percent (calls,stats.nb_calls))) stats.return_size;
      Format.fprintf fmt "total_time: %as (avg: %as, min: %as, max: %as)@]@,"
      pp_micro (Stats.sum stats.total_time)
      pp_nano (int_of_float (Stats.average stats.total_time *. 1000.))
      pp_micro (Stats.min stats.total_time) pp_micro (Stats.max stats.total_time)) runs

let string_ljust string nb = string ^ String.make (nb-String.length string) ' '

let pp_spread ?(justify=true) units extra fmt stats =
  let pp = (Stats.pp_with_unit ~justify ~unit_prefixes:units ()) in
  let pp' = (Stats.pp_with_unit ~justify ~unit_prefixes:(extra::units) ()) in
  Format.fprintf fmt "%a - %a - %a"
  pp (Stats.min stats)
  (* pp' (Stats.q1 stats *. 1000. |> int_of_float) *)
  pp' (Stats.average stats *. 1000. |> int_of_float)
  (* pp' (Stats.q3 stats *. 1000. |> int_of_float) *)
  pp (Stats.max stats)


(** Print a summary comparing each functions total runtime *)
let print_summary fmt runs =
  let print_percent = Stats.pp_percent ~justify:true () in
  let (max_len, total_time, total_calls) = List.fold_left (fun (max_len, times, calls) (name, stats) ->
    (max max_len (String.length name), times + Stats.sum stats.total_time, calls + stats.nb_calls)) (4,0,0) runs in
    Format.fprintf fmt "Summary format: 'total%% (min - avg - max)'@.";
  Format.fprintf fmt "%s  %s  %s@," (string_ljust "FUNC" max_len) "TIME                             " "CALLS                                     ";

  (* Sort by descending total runtime, using name to disambiguate*)
  List.sort (fun (_,t1) (_,t2) -> compare (-Stats.sum t1.total_time,t1.nb_calls) (-Stats.sum t2.total_time,t2.nb_calls)) runs |>
  List.iter (fun (name,stats) ->
    Format.fprintf fmt "%s  %a (%a)  %a @,"
        (string_ljust name max_len)
        print_percent (Stats.sum stats.total_time, total_time)
        (pp_spread micro_units_with_s "ns") stats.total_time
        print_percent (stats.nb_calls, total_calls)
    )

module Make(Sub:Sva_sig.NUMERIC_ENUM):Sva_sig.NUMERIC_ENUM = struct
  module StatLogger = Stats.StatLogger(struct
    let id = "sva_stats"

    type stat = func_stats

    let combine _ old_stat new_stat =
      assert(new_stat.nb_args = old_stat.nb_args);
      {
        nb_args = new_stat.nb_args;
        nb_calls = old_stat.nb_calls + new_stat.nb_calls;
        nb_constant_returns = old_stat.nb_constant_returns + new_stat.nb_constant_returns;
        total_time = Stats.concat old_stat.total_time new_stat.total_time;
        nb_constant_calls = IntMap.union (fun _ l r -> Some (l + r)) old_stat.nb_constant_calls new_stat.nb_constant_calls;
        return_size = match new_stat.return_size, old_stat.return_size with
          | None, None -> None
          | None, Some _ | Some _, None -> assert false
          | Some l, Some r -> Some(IntMap.union (fun _ l r -> Some (l + r)) l r)
      }
  end)()

  (** Stat about a single call.
     - nb_args: arguments to the transfer function
     - nb_constant_args: how many arguments of the calls represent singleton
     - is_return_constant: is the return value a singleton
     - return_value_size: None, or the size of the bitvector if it is a bitvector. *)
  let add_stat name ~nb_constant_args ~nb_args ~is_return_constant ~return_value_size ~total_time =
    StatLogger.add name {
      nb_args;
      nb_calls = 1;
      nb_constant_returns = if is_return_constant then 1 else 0;
      nb_constant_calls = IntMap.singleton nb_args 1;
      return_size = Option.map (fun size -> IntMap.singleton (In_bits.to_int size) 1) return_value_size;
      total_time = Stats.compact_int_singleton total_time;
    }

  let () =
    Hook.add_hook ~name:"display sva stats" Hook.exit (fun () ->
      let runs = StatLogger.to_list () in
      let oc = open_out "/tmp/sva_stats.txt" in
      let fmt = Format.formatter_of_out_channel oc in
      Format.fprintf fmt "@[<v>%a@,@,%a@]" print_stats (List.sort (fun (l,_) (r,_) -> compare l r) runs) print_summary runs;
      close_out oc;
      Log.info (fun p -> p "Stats printed to /tmp/sva_stats.txt@."))

  let name = Sub.name

  type boolean = Sub.boolean
  type bitvector = Sub.bitvector
  type integer = Sub.integer
  type enum = Sub.enum

  module Boolean_Lattice = Sub.Boolean_Lattice
  module Bitvector_Lattice = Sub.Bitvector_Lattice
  module Enum_Lattice = Sub.Enum_Lattice
  module Integer_Lattice = Sub.Integer_Lattice

  let is_constant_boolean = let open Lattices.Quadrivalent in function
    | True | False -> true | _ -> false

  let is_constant_bitvector ~size x =
    Option.is_some @@ Sub.Bitvector_Lattice.is_singleton ~size x

  let is_constant_enum x  =
    Option.is_some @@ Sub.Enum_Lattice.is_singleton x

  let ar0 (is_constant_ret,return_value_size) name op arg =
    let nb_constant_args = 0 in
    let nb_args = 0 in
    let () = Record_time.record_time () in
    let ret = op arg in
    let total_time = Record_time.return_time () in
    let is_return_constant = is_constant_ret ret in
    add_stat name ~nb_constant_args ~nb_args ~is_return_constant ~return_value_size ~total_time;
    ret

  let ar1 is_constant_arg (is_constant_ret,return_value_size) name op = fun arg ->
    let nb_constant_args = is_constant_arg arg |> Bool.to_int in
    let nb_args = 1 in
    let () = Record_time.record_time () in
    let ret = op arg in
    let total_time = Record_time.return_time () in
    let is_return_constant = is_constant_ret ret in
    add_stat name ~nb_constant_args ~nb_args ~is_return_constant ~return_value_size ~total_time;
    ret

  let rev_ar1 is_constant_arg is_constant_ret name op arg ret =
    let nb_constant_args =
      (is_constant_arg arg |> Bool.to_int)
    + (is_constant_ret ret |> Bool.to_int) in
    let nb_args = 2 in
    let () = Record_time.record_time () in
    let arg' = op arg ret in
    let total_time = Record_time.return_time () in
    let is_return_constant = match arg' with
    | Some a -> is_constant_arg a
    | _ -> false
    in
    add_stat ("rev_"^name) ~nb_constant_args ~nb_args ~return_value_size:None ~is_return_constant ~total_time;
    arg'

  let ar2 is_constant_arg1 is_constant_arg2 (is_constant_ret,return_value_size) name op = fun arg1 arg2 ->
    let nb_constant_args =
        (is_constant_arg1 arg1 |> Bool.to_int)
      + (is_constant_arg2 arg2 |> Bool.to_int) in
    let nb_args = 2 in
    let () = Record_time.record_time () in
    let ret = op arg1 arg2 in
    let total_time = Record_time.return_time () in
    let is_return_constant = is_constant_ret ret in
    add_stat name ~nb_constant_args ~nb_args ~is_return_constant ~return_value_size ~total_time;
    ret

  let rev_ar2 is_constant_arg1 is_constant_arg2 is_constant_ret name op arg1 arg2 ret =
      let nb_constant_args =
        (is_constant_arg1 arg1 |> Bool.to_int)
      + (is_constant_arg2 arg2 |> Bool.to_int)
      + (is_constant_ret ret |> Bool.to_int) in
      let nb_args = 3 in
      let () = Record_time.record_time () in
      let arg1', arg2' = op arg1 arg2 ret in
      let total_time = Record_time.return_time () in
      let is_return_constant = match arg1', arg2' with
      | Some a, Some b -> is_constant_arg1 a && is_constant_arg2 b
      | _ -> false
      in
      add_stat ("rev_"^name) ~nb_constant_args ~nb_args ~return_value_size:None ~is_return_constant ~total_time;
      arg1', arg2'

  let ret_boolean = (is_constant_boolean,None)
  let ret_bitvector size = (is_constant_bitvector ~size,Some size)
  let ret_enum = (is_constant_enum, None)


  module Boolean_Forward = struct
    module SBF = Sub.Boolean_Forward
    let true_ = SBF.true_
    let false_ = SBF.false_
    let not = ar1 is_constant_boolean ret_boolean "not" SBF.not
    let (&&) = ar2 is_constant_boolean is_constant_boolean ret_boolean "(&&)" SBF.(&&)
    let (||) = ar2 is_constant_boolean is_constant_boolean ret_boolean "(||)" SBF.(||)
  end
  module Boolean_Backward = struct
    module SBF = Sub.Boolean_Backward
    let not = rev_ar1 is_constant_boolean is_constant_boolean "not" SBF.not
    let (&&) = rev_ar2 is_constant_boolean is_constant_boolean is_constant_boolean "(&&)" SBF.(&&)
    let (||) = rev_ar2 is_constant_boolean is_constant_boolean is_constant_boolean "(||)" SBF.(||)
  end

  module Integer_Forward = Sub.Integer_Forward
  module Integer_Backward = Sub.Integer_Backward

  module Bitvector_Forward = struct
    module SBF = Sub.Bitvector_Forward

    let binpred name op ~size = ar2
        (is_constant_bitvector ~size)
        (is_constant_bitvector ~size)
        ret_boolean name (op ~size)

    let binop name op ~size = ar2
        (is_constant_bitvector ~size)
        (is_constant_bitvector ~size)
        (ret_bitvector size) name (op ~size)

    let binop_flags name op = fun ~size ~flags a b ->
      binop name (fun ~size -> op ~size ~flags) ~size a b


    let biadd = binop_flags "biadd" SBF.biadd
    let bisub = binop_flags "bisub" SBF.bisub
    let bimul = binop_flags "bimul" SBF.bimul
    let bshl =  binop_flags "bshl"  SBF.bshl

    let bisdiv = binop "bisdiv" SBF.bisdiv
    let bismod = binop "bismod" SBF.bismod
    let biudiv = binop "biudiv" SBF.biudiv
    let biumod = binop "biumod" SBF.biumod

    let bashr = binop "bashr" SBF.bashr
    let blshr = binop "blshr" SBF.blshr
    let band  = binop "band"  SBF.band
    let bor   = binop "bor"   SBF.bor
    let bxor =  binop "bxor"  SBF.bxor

    let beq =   binpred "beq" SBF.beq
    let bisle = binpred "bisle" SBF.bisle
    let biule = binpred "biule" SBF.biule

    let bconcat ~size1 ~size2 =
      ar2
        (is_constant_bitvector ~size:size1)
        (is_constant_bitvector ~size:size2)
        (ret_bitvector @@ In_bits.(size1 + size2))
        "bconcat" (SBF.bconcat ~size1 ~size2)

    let unop name op argsize retsize =
      ar1
        (is_constant_bitvector ~size:argsize)
        (ret_bitvector retsize) name op

    let buext ~size ~oldsize = unop "buext" (SBF.buext ~size ~oldsize) oldsize size
    let bsext ~size ~oldsize = unop "bsext" (SBF.bsext ~size ~oldsize) oldsize size
    let bextract ~size ~index ~oldsize =
      unop "bextract" (SBF.bextract ~size ~index ~oldsize) oldsize size
    let bimul_add ~size ~prod ~offset =
      unop "bimul_add" (SBF.bimul_add ~size ~prod ~offset) size size

    let bofbool ~size =
      ar1
        is_constant_boolean
        (ret_bitvector size) "bofbool" (SBF.bofbool ~size)

    let biconst ~size k = ar0 (ret_bitvector size) "biconst" (SBF.biconst ~size) k
  end
  module Bitvector_Backward = struct
    module SBF = Sub.Bitvector_Backward

    let binpred name op ~size = rev_ar2
        (is_constant_bitvector ~size)
        (is_constant_bitvector ~size)
        is_constant_boolean name (op ~size)

    let binop name op ~size = rev_ar2
        (is_constant_bitvector ~size)
        (is_constant_bitvector ~size)
        (is_constant_bitvector ~size) name (op ~size)

    let binop_flags name op ~size ~flags = binop name (fun ~size -> op ~size ~flags) ~size

    let biadd = binop_flags "biadd" SBF.biadd
    let bisub = binop_flags "bisub" SBF.bisub
    let bimul = binop_flags "bimul" SBF.bimul
    let bshl =  binop_flags "bshl"  SBF.bshl

    let bisdiv = binop "bisdiv" SBF.bisdiv
    let bismod = binop "bismod" SBF.bismod
    let biudiv = binop "biudiv" SBF.biudiv
    let biumod = binop "biumod" SBF.biumod

    let bashr = binop "bashr" SBF.bashr
    let blshr = binop "blshr" SBF.blshr
    let band  = binop "band"  SBF.band
    let bor   = binop "bor"   SBF.bor
    let bxor =  binop "bxor"  SBF.bxor

    let beq =   binpred "beq" SBF.beq
    let bisle = binpred "bisle" SBF.bisle
    let biule = binpred "biule" SBF.biule

    let bconcat ~size1 ~size2 =
      rev_ar2
        (is_constant_bitvector ~size:size1)
        (is_constant_bitvector ~size:size2)
        (is_constant_bitvector ~size:In_bits.(size1 + size2))
        "bconcat" (SBF.bconcat ~size1 ~size2)

    let unop name op argsize retsize = rev_ar1
      (is_constant_bitvector ~size:argsize)
      (is_constant_bitvector ~size:retsize)
      name op
    let bextract ~size ~index ~oldsize = unop "bextract" (SBF.bextract ~size ~index ~oldsize) oldsize size
    let buext ~size ~oldsize = unop "buext" (SBF.buext ~size ~oldsize) oldsize size
    let bsext ~size ~oldsize = unop "bsext" (SBF.bsext ~size ~oldsize) oldsize size
    let bimul_add ~size ~prod ~offset = unop "bimul_add" (SBF.bimul_add ~size ~prod ~offset) size size
    let bofbool ~size = rev_ar1 is_constant_boolean (is_constant_bitvector ~size) "bofbool" (SBF.bofbool ~size)
  end

  module Enum_Forward = struct
    let caseof ~case =
      ar1 is_constant_enum ret_boolean "caseof" (Sub.Enum_Forward.caseof ~case)

    let enum_const ~case =
      ar0 ret_enum "enum_const" (fun case -> Sub.Enum_Forward.enum_const ~case) case

  end
  module Enum_Backward = struct
    let caseof ~case =
      rev_ar1 is_constant_enum is_constant_boolean "caseof" (Sub.Enum_Backward.caseof ~case)
  end

end
