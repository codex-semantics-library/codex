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

type counter_example = unit

type sat =
  | Sat of counter_example
  | Unsat
  | Unknown


module type Common_S = sig

  (* An SMTLIB printer, with an higher-order abstract syntax
     interface, that does not require any buffering (for fast
     output). *)
  type logic
  type command

  
 (**************** Logics ****************)
  val qf_uf: logic            (* uninterpreted functions. *)
  val qf_lia: logic           (* linear arithmetic. *)
  val qf_nia: logic           (* general arithmetic. *)
  val qf_lra: logic           (* linear real arithmetic. *)
  val qf_auflia: logic        (* lia + uf + array. *)
  val auflia: logic
  val auflira: logic
  val aufnira: logic
  val lra: logic
  val qf_idl: logic
  val qf_rdl: logic
  val qf_ufidl: logic
  val qf_bv: logic            (* bitvectors *)
  val qf_ax: logic            (* arrays *)
  val qf_abv: logic           (* array + bitvectors *)
  val qf_aubv: logic          (* array + bitvectors + uninterpreted functions *)
  val horn: logic             (* Extension *)
      
  (**************** Commands ****************)

  type satisfiable = Sat | Unsat | Unknown;;
  val check_sat: unit -> satisfiable
  val set_logic: logic -> unit

  val set_option: string -> unit
  
end


module type Typed_S = sig
  include Common_S
  type 'a sort
  type 'a value

  (**************** Commands ****************)

  val assert_: bool value -> unit (* command t   *)

  (**************** Bool and generic functions ****************)
  val bool: bool sort

  val let_: ?name:string -> 'a value -> ('a value -> 'b value) -> 'b value
  val forall: ?name:string -> 'a sort -> ('a value -> bool value) -> bool value
  val exists: ?name:string -> 'a sort -> ('a value -> bool value) -> bool value      

  val (=): 'a value -> 'a value -> bool value
  val (||): bool value -> bool value -> bool value            
  
  (* Int theory *)
  val int: int sort      
  val numeral: int -> int value
  val (<): int value -> int value -> bool value

  (* Array theory, with extensibility: arrays are equal iff all elements are equal. *)
  type ('a,'b) array            (* Array from 'a sort to 'b sort. *)
      
  val array: 'a sort -> 'b sort -> ('a,'b) array sort
  val select: ('a,'b) array value -> 'a value -> 'b value
  val store: ('a,'b) array value -> 'a value -> 'b value -> ('a,'b) array value    

  (* Fixed_Size_Bitvectors theory *)
  type bitvector                (* Bitvector of a given size. TODO: compute sizes statically with GADTs? Works with extract?*)

  val bitvec: int -> bitvector sort

  (* Smallest bits in the integer *)
  val bvlit: size:int -> Z.t -> bitvector value
  
  val concat: bitvector value -> bitvector value -> bitvector value
  val bvnot: bitvector value -> bitvector value
  val bvneg: bitvector value -> bitvector value

  val bvand: bitvector value -> bitvector value -> bitvector value
  val bvor: bitvector value -> bitvector value -> bitvector value
  val bvadd: bitvector value -> bitvector value -> bitvector value
  val bvmul: bitvector value -> bitvector value -> bitvector value
  val bvudiv: bitvector value -> bitvector value -> bitvector value
  val bvurem: bitvector value -> bitvector value -> bitvector value
  val bvshl: bitvector value -> bitvector value -> bitvector value
  val bvlshr: bitvector value -> bitvector value -> bitvector value
  val bvult: bitvector value -> bitvector value -> bool value

  (* Extensionso Fixed_Size_Bitvectors, but described in 
     http://smtlib.cs.uiowa.edu/logics-all.shtml.  *)
  val bvxor: bitvector value -> bitvector value -> bitvector value      
  val bvsdiv: bitvector value -> bitvector value -> bitvector value
  val bvsrem: bitvector value -> bitvector value -> bitvector value
  val bvule: bitvector value -> bitvector value -> bool value
  val bvslt: bitvector value -> bitvector value -> bool value
  val bvsle: bitvector value -> bitvector value -> bool value            

end


module type Untyped_S = sig

  include Common_S
  
  type sort
  type value


  (**************** Commands ****************)
  
  (* Execute the command.  *)

  val assert_: value -> unit
  val get_assignment: unit -> unit

  (* TODO: Difficult to make declare fun generic in terms of arity. *)
  (* MAYBE: A monadic interface would make explicit the fact that
     declare_var has a side-effect. *)
  val declare_var: ?name:string -> sort -> value
  val define_var: ?name:string -> sort -> value -> value

  (**************** Bool and generic functions ****************)
  val bool: sort

  val let_: ?name:string -> value -> (value -> value) -> value
  val forall: ?name:string -> sort -> (value -> value) -> value
  val exists: ?name:string -> sort -> (value -> value) -> value      

  val (=): value -> value -> value
  val (||): value -> value -> value
  val (&&): value -> value -> value
  val (=>): value -> value -> value

  val and_list: value list -> value
  val or_list: value list -> value      
  
  val not: value -> value
  val true_: value
  val false_: value            
  
  (* Int theory *)
  val int: sort      
  val numeral: int -> value
  val numeralz: Z.t -> value      
  val (<): value -> value -> value
  val (<=): value -> value -> value      
  val (-): value -> value -> value
  val neg: value -> value
  val (+): value -> value -> value
  val ( * ): value -> value -> value
  val div: value -> value -> value
  val modu: value -> value -> value                  


  (* Array theory, with extensibility: arrays are equal iff all elements are equal. *)

  val array: sort -> sort -> sort
  val select: value -> value -> value
  val store: value -> value -> value -> value    

  (* Fixed_Size_Bitvectors theory *)
  val bitvec: int -> sort

  (* Smallest bits in the integer *)
  val bvlit: size:int -> Z.t -> value
  
  val concat: value -> value -> value
  val bvnot: value -> value
  val bvneg: value -> value

  val bvand: value -> value -> value
  val bvor: value -> value -> value
  val bvadd: value -> value -> value
  val bvmul: value -> value -> value
  val bvudiv: value -> value -> value
  val bvurem: value -> value -> value
  val bvshl: value -> value -> value
  val bvlshr: value -> value -> value
  val bvult: value -> value -> value

  val extract: first:int -> last:int -> value -> value
  
  (* Extensions to Fixed_Size_Bitvectors, but described in 
     http://smtlib.cs.uiowa.edu/logics-all.shtml.  *)
  val bvxor: value -> value -> value      
  val bvsdiv: value -> value -> value
  val bvsrem: value -> value -> value
  val bvsmod: value -> value -> value      
  val bvule: value -> value -> value
  val bvslt: value -> value -> value
  val bvsle: value -> value -> value
  val bvashr: value -> value -> value

  (* The int indicates how much bit to extend. *)
  val sign_extend: int -> value -> value
  val zero_extend: int -> value -> value      
  
end

(**************** Mu-z extensions, described in http://rise4fun.com/Z3/tutorialcontent/fixedpoints ****************)
module type Untyped_Muz = sig
  include Untyped_S

  type relation = value list -> value

  val declare_rel: ?name:string -> sort list -> relation * string
  
  (* A horn rule, with premices and a conclusion. *)
  val rule: value list -> value -> unit
  val fact: value -> unit

  (* val query: relation t -> value list -> unit *)
  val query: value -> satisfiable
  (* Note: query is used differently on different versions of z3. *)
  val query2: string -> satisfiable
  val declare_muz_var: ?name:string -> sort -> value

end

module type Param_S = sig
  val print : string -> unit
  val inc: Stdlib.in_channel
  val flush: unit -> unit
end
