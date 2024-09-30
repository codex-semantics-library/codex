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

include Smtbackend_smtlib_sig

module Make(P:Param_S) =
struct

  type t = unit -> unit
  type logic = t
  type command = t

  type satisfiable = Sat | Unsat | Unknown;;

  let pr_int i () = P.print @@ string_of_int i

  let ar0 str () =
    P.print "("; P.print str; P.print ")";;
  
  let ar1 str (a:t) () =
    P.print "("; P.print str; P.print " "; a(); P.print ")";;  

  let ar2 str (a:t) (b:t) () =
    P.print "("; P.print str; P.print " "; a(); P.print " "; b(); P.print ")";;  

  let ar3 str (a:t) (b:t) (c:t) () =
    P.print "("; P.print str; P.print " "; a(); P.print " "; b(); P.print " "; c(); P.print ")";;  

  let ar_list l () =
    P.print "(";
    (match l with
    | [] -> ()
    | a::b -> a(); b |> List.iter (fun x -> P.print " "; (x())));
    P.print ")"
  ;;

  let read_sat() =
    (* P.print "\n"; *)
    (* P.flush(); *)
    let input =
      match input_line P.inc with
      | exception End_of_file -> Codex_log.fatal "Could not read the solver answer. Is z3 installed?"
      | input -> input
    in
    match input with
    | "sat" -> Sat
    | "unsat" -> Unsat
    | "unknown" -> Codex_log.warning "Non-timeout unknown"; Unknown
    | "timeout" -> Unknown
    | _ -> failwith ("input line " ^ input)

  
  let check_sat () =
    ar0 "check-sat" (); P.print "\n"; P.flush();
    read_sat()
  ;;
    
  let set_option string =
    P.print "(set-option "; P.print string; P.print ")\n";;
    
  let get_assignment () =
    ar0 "get-assignment" (); P.print "\n"; P.flush()
  ;;
    


  let qf_uf () = P.print "QF_UF"
  let qf_lia () = P.print "QF_LIA"
  let qf_nia () = P.print "QF_NIA"
  let qf_lra () = P.print "QF_LRA"
  let qf_auflia () = P.print "QF_AUFLIA"
  let auflia () = P.print "AUFLIA"
  let auflira () = P.print "AUFLIRA"
  let aufnira () = P.print "AUFNIRA"
  let lra () = P.print "LRA"
  let qf_idl () = P.print "QF_IDL"
  let qf_rdl () = P.print "QF_RDL"
  let qf_ufidl () = P.print "QF_UFIDL"
  let qf_bv () = P.print "QF_BV"
  let qf_ax () = P.print "QF_AX"
  let qf_abv () = P.print "QF_ABV"
  let qf_aubv () = P.print "QF_AUBV"
  let horn () = P.print "HORN"

  let sort str = (fun () -> P.print str)
  
  let bool = sort "Bool"
  let int = sort "Int"      

  let gensym_ctr = ref 0;;
  let gensym ?name:(name="x") () =
    let count = !gensym_ctr in
    incr gensym_ctr;
    name ^ (string_of_int count)
  ;;

  (* When given, the name should alredy be uniquified. Not always true for prevous versions. *)
  let _gensym_ ?name () =
    match name with
    | None -> incr gensym_ctr; "x" ^ (string_of_int !gensym_ctr)
    | Some "nondet" -> incr gensym_ctr; "x" ^ (string_of_int !gensym_ctr)
    | Some "var" -> incr gensym_ctr; "x" ^ (string_of_int !gensym_ctr)
    | Some "mu" -> incr gensym_ctr; "x" ^ (string_of_int !gensym_ctr)
    | Some name -> name


  let declare_var ?name sort =
    let name = gensym() in
    let pr_name () = P.print name in
    (* let command () = *)
    P.print "(declare-fun "; P.print name; P.print " () "; sort(); P.print")\n";
    (* command,  *) pr_name
  ;;

  let define_var ?name sort value =
    let name = gensym() in
    let pr_name () = P.print name in
    (* let command () = *)
      P.print "(define-fun "; P.print name; P.print " () "; sort(); P.print " "; value(); P.print")\n";
    (* command, *)pr_name
  ;;

  
  
  let let_ ?name (v:unit -> unit) (f:(unit -> unit) -> (unit -> unit)):(unit -> unit) =
    fun () ->
      let name = gensym() in
      let prname = fun () -> P.print name in
      P.print "(let (("; P.print name; P.print " "; v(); P.print ")) ";
      f prname (); P.print ")"
  ;;

  let quantif str ?name (sort:unit -> unit) (f:(unit -> unit)-> (unit -> unit)) =
    fun () ->
      let name = gensym() in
      let prname = fun () -> P.print name in
      P.print "("; P.print str; P.print "(("; P.print name; P.print " "; sort(); P.print ")) ";
      f prname (); P.print ")"
  ;;

  let forall = quantif "forall";;
  let exists = quantif "exists";;
  
  let assert_ cond = let () = ar1 "assert" cond () in P.print "\n";;
  let set_logic log = ar1 "set-logic" log (); P.print "\n";;
  
  let (||) = ar2 "or";;
  let (&&) = ar2 "and";;
  let (=>) = ar2 "=>";;
  let not = ar1 "not";;
  let true_ () = P.print "true";;
  let false_ () = P.print "false";;

  let array = ar2 "Array"
  let select = ar2 "select";;  
  let store = ar3 "store";;

  (**************** bitvectors ****************)


  let bitvec int = ar1 "_ BitVec" (pr_int int);;
  (* let bvlit int =  *)

  let bvult = ar2 "bvult"
  let bvlshr = ar2 "bvlshr"
  let bvshl = ar2 "bvshl"
  let bvurem = ar2 "bvurem"
  let bvudiv = ar2 "bvudiv"
  let bvmul = ar2 "bvmul"
  let bvadd = ar2 "bvadd"
  let bvor = ar2 "bvor"
  let bvand = ar2 "bvand"
  let bvneg = ar1 "bvneg"
  let bvnot = ar1 "bvnot"
  let concat = ar2 "concat"

  let bvlit ~size k () =
    let x =
      if Z.geq k Z.zero then k
      else Z.(+) k @@ Z.shift_left Z.one size
    in

    (* If we can use hexadecimal *)
    if (size land 3) == 0 then begin
      let u = Z.format "%x" x in
      let leading_zeroes = (size lsr 2) - (String.length u) in
      P.print "#x";
      for _ = 0 to leading_zeroes - 1 do
        P.print "0"
      done;
      P.print u
    end
    else begin
      (* We use binary *)
      let u = Z.format "%b" x in
      let leading_zeroes = size - (String.length u) in
      P.print "#b";
      for _ = 0 to leading_zeroes - 1 do
        P.print "0"
      done;
      P.print u
    end
  ;;

  (* Only works when Integer is provided with Zarith. *)
  let bvlit ~size (k:Z.t) () = bvlit ~size (Obj.magic k) ();;

  (* Does not output the right number of zeroes. *)
  (* let bvlit64 = make_bvlit (Printf.sprintf "%Lx") *)


  let bvxor = ar2 "bvxor"
  let bvsdiv = ar2 "bvsdiv"
  let bvsrem = ar2 "bvsrem"
  let bvule = ar2 "bvule"
  let bvslt = ar2 "bvslt"
  let bvsle = ar2 "bvsle"

  let bvashr = ar2 "bvashr"
  let bvsmod = ar2 "bvsmod"

  let zero_extend i = ar1 @@ "(_ zero_extend " ^ (string_of_int i) ^ ")"
  let sign_extend i = ar1 @@ "(_ sign_extend " ^ (string_of_int i) ^ ")"
  let extract ~first ~last = ar1 @@ "(_ extract " ^ (string_of_int last) ^ " " ^ (string_of_int first) ^ ")"
  

  (* Muz. *)
  (* type relation = value t list -> value t *)

  type relation = (unit -> unit) list -> unit -> unit

  let declare_rel ?name sorts =
    let name = gensym ?name () in
    let pr_name list () = match list with
      | [] -> P.print name
      | _ -> P.print ("(" ^ name); List.iter (fun x -> P.print " "; x()) list; P.print ")" in
    P.print "(declare-rel "; P.print name; P.print " "; ar_list sorts (); P.print")\n";
    pr_name,name
  ;;
  
  (* let query rel list  = P.print "(query"; ar_list (rel::list) (); P.print ")\n";; *)
  let query bool = P.print "(query "; bool(); P.print ")\n"; P.flush();
    read_sat()

  let query2 string = P.print "(query "; P.print string; P.print ")\n"; P.flush();
    read_sat()

  let fact conclusion =
    P.print "(rule "; conclusion(); P.print ")\n"
  ;;

  let rule premices conclusion =
    if premices == [] then fact conclusion
    else begin
      P.print "(rule (=> (and "; List.iter (fun x -> P.print"\n     "; x()) premices; P.print ")\n   ";
      conclusion(); P.print "))\n"
    end
  ;;

  let and_list = function
    | [] -> true_
    | [x] -> x
    | l -> begin
        fun () -> P.print "(and "; List.iter (fun x -> P.print " "; x()) l; P.print ")"
    end
  ;;

  let or_list = function
    | [] -> false_
    | [x] -> x
    | l -> begin
        fun () -> P.print "(or "; List.iter (fun x -> P.print " "; x()) l; P.print ")"
    end
  ;;
  
  let declare_muz_var ?name sort =
    let name = gensym ?name () in
    let pr_name () = P.print name in
    (* let command () = *)
    P.print "(declare-var "; P.print name; P.print " "; sort(); P.print")\n";
    (* command,  *) pr_name
  ;;


  (**************** Ints ****************)


  let numeral x () = P.print @@ string_of_int x;;
  let numeralz x () = P.print @@ Z.to_string x;;  
  let (<) = ar2 "<"
  let (<=) = ar2 "<="      
  let (=) = ar2 "="
  let modu = ar2 "mod"
  let div = ar2 "div"
  let (+) = ar2 "+"
  let (-) = ar2 "-"
  let ( * ) = ar2 "*"                        
  let neg = ar1 "-"
  
end

module Make_Typed(P:Param_S):Typed_S = struct
  include Make(P)
  type bitvector
  type ('a,'b) array
  type 'a sort = t
  type 'a value = t
      
end

module Make_Untyped(P:Param_S):Untyped_S = struct
  include Make(P)
  type sort = t
  type value = t
end

module Make_Untyped_Muz(P:Param_S):Untyped_Muz = struct
  include Make(P)
  type sort = t
  type value = t  
end

let with_z3 ?(executable="z3") (f:(module Untyped_Muz) -> 'a) =
  let executable = "z3" in

  (* Note: careful with functions that pass the env, as the PATH may
     be modified, which is problematic on Nix. *)
  let (pout,pin) = Unix.open_process
      (executable ^ " -T:" ^ (Codex_config.smt_timeout() |> string_of_int) ^ " -in") in
  let module SMT = Make_Untyped_Muz(struct
      let print str =
        (* Duplicate the output for debugging *)
        if Codex_config.print_smt_input() then Stdlib.print_string str;
        Stdlib.output_string pin str;;

      let inc = pout
      let flush () = Stdlib.flush pin
    end
    ) in
  f (module SMT:Untyped_Muz);;
