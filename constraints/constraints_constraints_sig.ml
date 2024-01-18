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

open Transfer_functions.Term

module type Constraints = sig

  module Condition:Condition_map.Condition

  type parents

  module Id:sig
    type 'a t
    val to_int: 'a t -> int
  end

  (* Loop nesting where the variable was defined. When feasible, we
     automatically compute it to be at the highest possible scope. *)
  type level = int

  type 'a complete_term = private
    | Tuple_get: int * tuple_ -> 'res complete_term
    | Empty: 'res complete_term
    | Unknown: level -> 'res complete_term
    | Mu_formal:{level:level; actual: ('res t * Transfer_functions.Term.boolean t) } -> 'res complete_term (* if level != 0; intro != None *)
    | T0: { tag: (ar0,'res) term } -> 'res complete_term
    | T1: { tag: ('a ar1,'res) term; a: 'a t; level: level} -> 'res complete_term
    | T2: { tag: (('a,'b) ar2,'res) term; a: 'a t; b:'b t; level:level } -> 'res complete_term

  
  and 'a t = 
    | Bool: { mutable parents: parents; id: boolean Id.t; term: boolean complete_term; bdd:Condition.t} -> boolean t
    | Integer: { mutable parents: parents; id: integer Id.t; term: integer complete_term; } -> integer t
    | Binary: { mutable parents: parents; id: binary Id.t; term: binary complete_term; size: int} -> binary t

  and any = Any: 'res t -> any 
  and tuple = any Immutable_array.t
  and tuple_ = private
    | Nondet of {id: tuple Id.t; level: level;
                 a:tuple; conda_bool: boolean t;
                 b:tuple; condb_bool: boolean t}
    | Mu of { id: tuple Id.t; level: level; init:tuple;var:tuple;body:tuple;body_cond: boolean t}

  module Any:sig
    type t = any
    val hash: t -> int
    val equal: t -> t -> bool
    val compare: t -> t -> int
    val get_id_int: t -> int
  end

  module Tuple:sig
    type t = tuple_
    val hash: t -> int
    val equal: t -> t -> bool
    val compare: t -> t -> int
  end

  
  module Parents:sig
    val iter_on_parents: 'a t -> (any -> unit) -> unit
  end

  val compare: 'a t -> 'b t -> int
  val hash: 'a t -> int
  val equal: 'a t -> 'b t -> bool

  (* Print the definitions of the transitive dependencies of a
     constraint of tuple that were not already printed. *)
  val pretty_deps: Format.formatter -> 'a t -> unit
  val pretty_tuple_deps: Format.formatter -> tuple -> unit    

  (* Print the constraint/tuple with its dependencies (if not already
     printed). *)
  val pretty: Format.formatter -> 'a t -> unit
  val pretty_tuple: Format.formatter -> tuple -> unit

  (* Prints the level of nesting for a constraints. Constants are -1, topmost is 0. *)
  val level: 'a t -> int

  val size_of: binary t -> int
  
  module Build:sig

    module Boolean:sig
      val empty: boolean t
      val bunion: boolean t -> boolean t -> boolean t          
      val unknown: level:int -> boolean t          
      include Transfer_functions.Boolean_Forward
        with module Arity := Transfer_functions.Forward_Arity
         and type boolean = boolean t
    end

    module Integer:sig
      val empty: integer t
      val unknown: level:int -> integer t          
      include Transfer_functions.Integer_Forward
        with module Arity := Transfer_functions.Forward_Arity
         and type boolean = boolean t
         and type integer = integer t                 
    end

    module Binary:sig
      val empty: size:int -> binary t
      val bunion: size:int -> Transfer_functions.Condition.t -> binary t -> binary t -> binary t
      val unknown: size:int -> level:int -> binary t          
      include Transfer_functions.Binary_Forward
        with module Arity := Transfer_functions.Forward_Arity
         and type boolean = boolean t
         and type binary = binary t
    end

    
    module Mu_Formal:sig
      val intro: level:int -> actual:'a t ->  actual_cond: Transfer_functions.Term.boolean t -> 'a Transfer_functions.Term.typ -> 'a t
    end
    
    module Tuple:sig
      val get_binary: size:int -> int -> tuple ->  Transfer_functions.Term.binary t      
      val get_integer:int -> tuple ->  Transfer_functions.Term.integer t
      val get_boolean:int -> tuple ->  Transfer_functions.Term.boolean t
      val nondet: level:int -> conda_bool:Transfer_functions.Term.boolean t ->  a:tuple ->
        condb_bool:Transfer_functions.Term.boolean t -> b:tuple -> tuple
      val mu: level:int -> init:tuple -> var:tuple -> body:tuple -> body_cond:Transfer_functions.Term.boolean t -> tuple
    end
    
  end

  module Utils: sig

    val get_term: 'a t -> 'a complete_term

  end
  
  
end
