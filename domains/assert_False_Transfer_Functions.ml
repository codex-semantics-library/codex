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

(* Used to create dummy types to fill the signatures, even when the
   abstract domain does not support the type. *)
module Memory = struct
  module Memory = Datatype_sig.Unit
  module Memory_Forward = struct
    let assume _ = assert false
    let store ~size _ = assert false
    let memcpy ~size _ = assert false      
    let load ~size _ = assert false
    let malloc ~id ~malloc_size = assert false
    let free _ = assert false
    let unknown ~level = assert false
  end
  module Memory_Backward = Memory_Forward;;
  let memory_is_bottom _ = assert false
end


module Boolean = struct
  module Boolean_Backward = struct
    let (||) _ _ _ = assert false
    let (&&) _ _ _ = assert false
    let not _ _ = assert false
    let assume _cond _store _result = assert false
  end

  module Boolean_Forward = struct
    let (||) _ _  = assert false
    let (&&) _ _  = assert false
    let not _  = assert false
    let assume _cond _store = assert false
    let true_ _ = assert false
    let false_ _ = assert false
    let unknown ?level _ = assert false
  end

end

module Integer = struct
  module Integer_Forward = struct
       let one _ = assert false
       let zero _ = assert false
       let ile _ = assert false
       let ieq _ = assert false
       let isub _ = assert false
       let iconst _ = assert false
       let assume _ = assert false
       let iunknown _ = assert false
       let ixor _ = assert false
       let ior _ = assert false
       let iand _ = assert false
       let ishr _ = assert false
       let ishl _ = assert false
       let imod _ = assert false
       let idiv _ = assert false
       let imul _ = assert false
       let iadd _ = assert false
       let itimes _ = assert false
  end
end

module Binary = struct

  module Binary_Forward = struct
    let beq   ~size _ _ = assert false
    let biule ~size _ _ = assert false
    let bisle ~size _ _ = assert false
    let bitimes ~size _ _ = assert false
    let biadd ~size ~nsw ~nuw ~nusw _ _ = assert false
    let bisub ~size ~nsw ~nuw ~nusw _ _ = assert false      
    let bimul ~size ~nsw ~nuw _ _ = assert false
    let bxor ~size _ _ = assert false
    let band ~size _ _ = assert false
    let bor ~size _ _ = assert false
    let nondet ~size l = assert false
    let assume ~size _cond _store = assert false
    let bsext ~size ~oldsize _ = assert false
    let buext ~size ~oldsize _ = assert false
    let bofbool ~size _ = assert false
    let bchoose ~size _ _ = assert false
    let bashr ~size _ _ = assert false
    let blshr ~size _ _ = assert false
    let bshl ~size ~nsw ~nuw _ _ = assert false
    let bisdiv ~size _ _ = assert false
    let biudiv ~size _ _ = assert false      
    let bconcat ~size1 ~size2 b1 b2 = assert false
    let bismod ~size _ _ = assert false
    let biumod ~size _ _ = assert false      
    let bextract ~size ~index ~oldsize _ = assert false
    let valid ~size _ = assert false
    let valid_ptr_arith ~size _ = assert false
    let bunknown ~size _ = assert false
    let baddr ~size _  = assert false
    let biconst ~size _ = assert false
    let buninit ~size _ = assert false
    let bshift ~size ~offset ~max _ = assert false
    let bindex ~size _ = assert false
  end


  module Binary_Backward = struct
    let beq _ _ _ = assert false
    let biule _ _ _ = assert false
    let bisle _ _ _ = assert false
    let bitimes _ _ _ = assert false
    let biadd _ _ _ = assert false
    let bimul ~size _ _ _ = assert false
    let bxor ~size _ _ _ = assert false
    let band ~size _ _ _ = assert false
    let bor ~size _ _ _ = assert false
    let assume ~size _cond _store _result = assert false
    let bsext ~size _ _ = assert false
    let buext ~size _ _ = assert false
    let bashr ~size _ _ _ = assert false
    let blshr ~size _ _ _ = assert false
    let bshl ~size ~nsw ~nuw _ _ _ = assert false
    let bisdiv _ _ _ = assert false
    let bconcat ~size1 ~size2 b1 b2 result = assert false
    let bismod _ _ _ = assert false
    let bextract ~size ~index ~oldsize _ _ = assert false
    let valid ~size _ _ = assert false
    let bshift ~size _ = assert false
    let bindex ~size _ = assert false
  end

end


(* A commodity to start implementing domains; start from here, then complete things as needed. *)
module Domain:Domain_sig.Base
= struct

  let name = "assert false"
  let unique_id = Domain_sig.Fresh_id.fresh name;;

  module Types = struct
    type binary = unit
    type memory = unit
    type integer = unit
    type boolean = unit
  end
  include Types

  (**************** Root context ****************)

  type mu_context = {
    mu_ctx: context;              (* Context corresponding to this mu_context *)
    mu_parent_ctx: context;       (* Parent of the current context. *)
  }

  and root_context = {
    root_ctx: context;            (* Context corresponding to this root_context *)
  }

  and context_ =
    | Mu_context of mu_context
    | Root_context of root_context

  and context =
    { ctx:context_;
      level:int;
    }
  ;;

  let root_context() =
    let rec root_ctx = {
      root_ctx = ctx;
    }
    and ctx = {
      ctx = Root_context root_ctx;
      level = 0;
    }
    in ctx
  ;;

  let context_pretty _ = assert false

  (**************** Fixpoint computation ****************)
  let mu_context_fixpoint_step3 ctx ~arg_body = assert false            
  let mu_context_upcast ctx = assert false
  let mu_context_open parent_ctx =
    let rec mu_ctx = { mu_ctx = ctx; mu_parent_ctx = parent_ctx}
    and ctx = { ctx = Mu_context mu_ctx;
                level = parent_ctx.level + 1;
              }
    in ctx

  
  module Serialize = struct

    module T = Types
    (*  A lazy version of serialization functions. *)
    type 'a in_tuple =
      | InEmpty: unit in_tuple
      | InInteger: T.integer * T.integer * 'a in_tuple -> (T.integer * 'a) in_tuple
      | InBoolean: T.boolean * T.boolean * 'a in_tuple -> (T.boolean * 'a) in_tuple
      | InMemory: T.memory * T.memory * 'a in_tuple -> (T.memory * 'a) in_tuple        
      | InBinary: int * T.binary * T.binary * 'a in_tuple -> (T.binary * 'a) in_tuple                

    type 'a in_acc = bool * 'a in_tuple

    type 'a out_tuple =
      | OutEmpty: unit out_tuple
      | OutInteger: T.integer * 'a out_tuple -> (T.integer * 'a) out_tuple
      | OutBoolean: T.boolean * 'a out_tuple -> (T.boolean * 'a) out_tuple
      | OutMemory: T.memory * 'a out_tuple -> (T.memory * 'a) out_tuple        
      | OutBinary: int * T.binary * 'a out_tuple -> (T.binary * 'a) out_tuple                

    type empty_tuple = unit
    let empty_tuple = InEmpty
    let push_integers a b tup = InInteger(a,b,tup)
    let push_booleans a b tup = InBoolean(a,b,tup)
    let push_binaries ~size a b tup = InBinary(size,a,b,tup)
    let push_memories a b tup = InMemory(a,b,tup)      

    [@@@warning "-8"]
    let pop_integer (OutInteger(x,tup)) = x,tup
    let pop_boolean (OutBoolean(x,tup)) = x,tup
    let pop_memory (OutMemory(x,tup)) = x,tup                                        
    let pop_binary (OutBinary(_,x,tup)) = x,tup
    [@@@warning "+8"]                                          


    module Context = struct
      type t = context
      let level x = x.level
      type nonrec 'a in_tuple = 'a in_tuple
      type nonrec 'a out_tuple = 'a out_tuple
      type nonrec 'a in_acc = 'a in_acc
      type nonrec empty_tuple = empty_tuple
      let empty_tuple = empty_tuple
      type ('a,'b) result = Result: bool * 'some in_tuple * (t -> 'some out_tuple -> 'a * 'b out_tuple) -> ('a,'b) result        
    end
    open Context


    (* On pourrait vouloir fournir ces fonctions a la place de celles
       que j'utilise (push_int,pop_int). Mais elles sont peut-etre moins pratiques pour le
       produit de domaines? *)
    let serialize_integer: 'any -> T.integer -> 'any -> T.integer -> 'a in_acc -> (T.integer,'a) result =
      fun _ a _ b (included,old) ->
        let tup = push_integers a b old in
        Result(included, tup, fun _ctx x -> pop_integer x)
    ;;

    let serialize_boolean: 'any -> T.boolean -> 'any -> T.boolean -> 'a in_acc -> (T.boolean,'a) result =
      fun _ a _ b (included,old) ->
        (* Instead of using a default implementation, we should be
           immediately serializing to the domain below for instance. *)
        (* Codex_log.warning "avoid using the default implementation of serialize_* functions"; *)
        let tup = push_booleans a b old in
        Result(included, tup, fun _ctx x -> pop_boolean x)
    ;;

    let serialize_memory: 'any -> T.memory -> 'any -> T.memory -> 'a in_acc -> (T.memory,'a) result =
      fun _ a _ b (included,old) ->
        let tup = push_memories a b old in
        Result(included, tup, fun _ctx x -> pop_memory x)
    ;;


    let serialize_binary: size:int -> 'any -> T.binary -> 'any -> T.binary -> 'a in_acc -> (T.binary,'a) result =
      fun ~size _ a _ b (inc,old) ->
        let tup = push_binaries ~size a b old in
        Result(inc, tup, fun _ctx x -> pop_binary x)
    ;;

    let typed_nondet2 _ = assert false
    let nondet_same_context _ = assert false      
    let typed_fixpoint_step _ = assert false    
  end

  include Serialize

  module Boolean_Forward = Boolean.Boolean_Forward
  module Integer_Forward = Integer.Integer_Forward                                 
  module Binary_Forward = Binary.Binary_Forward
  module Memory_Forward = Memory.Memory_Forward

  module Binary = Datatype_sig.Unit
  module Integer = Datatype_sig.Unit
  module Boolean = Datatype_sig.Unit                                         

  let memory_is_bottom ctx = assert false
  let binary_is_bottom ~size ctx = assert false
  let boolean_is_bottom ctx = assert false
  
 (**************** Pretty printing ****************)

  let memory_pretty ctx fmt = assert false
  let binary_pretty ~size ctx fmt = assert false
  let boolean_pretty ctx fmt = assert false
  let integer_pretty _ = assert false

  (**************** Tuple fonctions ****************)

  
  let tuple_bottom _ = assert false
  let new_tuple_nondet2 _ = assert false
  let typed_fixpoint_step ~init:_ ~arg:_ ~body:_  = assert false

  let tuple_pretty _ = assert false
    


  let assume ctx bool = assert false
  let imperative_assume ctx bool = assert false    
  
  (**************** Queries ****************)
  
  module Query = struct
    let reachable _ = assert false
    let boolean _ = assert false
    let binary ~size _ = assert false
    let integer _ = assert false
    let convert_to_ival _ = assert false
    let convert_to_quadrivalent _ = assert false
    let binary_to_ival ~signed ~size _ = assert false
    let binary_to_known_bits ~size _ = assert false      
    let binary_is_empty ~size _ = assert false      
    let binary_fold_crop ~size bin ~inf ~sup f acc = assert false      
    let is_singleton_int _ = assert false
    let binary_is_singleton ~size _ = assert false            
    module Boolean_Lattice = Lattices.Quadrivalent
    module Integer_Lattice = Lattices.Unit
    module Binary_Lattice = struct
      include Lattices.Unit
      let is_bottom ~size = assert false
      let bottom ~size = assert false
      let includes ~size = assert false
      let top ~size = assert false
      let inter ~size = assert false
      let join ~size = assert false      
      let pretty ~size = assert false
      let widen ~size ~previous _ = assert false
      let includes_or_widen ~size ~previous _ = assert false
      let singleton ~size = assert false
    end
  end

  let memory_is_empty _ = assert false
  let binary_is_empty ~size _ = assert false
  let integer_is_empty _ = assert false
  let boolean_is_empty _ = assert false
  let builtin _ = assert false

  
  let binary_empty ~size _ = assert false
  let integer_empty _ = assert false
  let boolean_empty _ = assert false        

  let binary_unknown  ~size _ = assert false
  let integer_unknown _ = assert false
  let boolean_unknown _ = assert false        

  

  (* Assume. *)
  let assume_memory _ctx _ _ = assert false
  let assume_binary ~size _ _ = assert false
  let assume_boolean _ctx _ _ = assert false
  let assume_integer _ctx _ _ = assert false


  
  
  let reachable _ = assert false
  let satisfiable _ = assert false

  let unknown_condition _ ?level = assert false
  let unknown_choice _ ?level = assert false    

  (* type binary_set = binary *)
  type choice = unit
  type condition = unit
  let union _ = assert false
  let choose_binary ~size _ = assert false

  let should_focus ~size:_ _ = assert false
  let may_alias ~ptr_size:_ _ ~size1:_ ~size2:_ = assert false
  let binary_unknown_typed ~size:_ _ = assert false
  let query_boolean = Query.boolean

  include Transfer_functions.Builtin.Make(Types)(Context)

end
