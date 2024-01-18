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



module Quadrivalent = Lattices.Quadrivalent

(* A Context represent a set of paths leading to the current state (it
   corresponds to a path condition in symbolic execution)

   Note: we use a module for Context, instead of a type, so that it
   can be used as a functor argument and replace the Arity. 
   
   TODO: Rename Context to AbsState: the context is now a representation
   of the state at a program point. *)

module type Context = sig
  type t
  val level:t -> int


  (** Types for serialization. *)


  (** The type of the tuples of argument to nondet (i.e., arguments of a phi function). *)
  type 'a in_tuple
  type empty_tuple
  val empty_tuple: empty_tuple in_tuple    
  
  (** An accumulator is a set of arguments to nondet, and an accumulated inclusion check.  *)
  type 'a in_acc = bool * 'a in_tuple

  (** The type of the result of the phi function. *)
  type 'a out_tuple

  (* We use a GADT because 'some is existentially quantified: we don't want
     the type of in_tuple to appear in serialization function, as, for instance,
     what we put in in in_tuple can depend on some condition. 

     The boolean
   * expresses whether the **second** operand of the serialization was included
   * in the **first** one. *)
  type ('a,'b) result =
      Result: bool * 'some in_tuple * (t -> 'some out_tuple -> 'a * 'b out_tuple) -> ('a,'b) result

  
end

(* TODO: This should return the context, and have the form of a state
   monad (i.e. state -> 'a * state).

   We can do it gradually, e.g. changing the booleans or the binary
   first.

   One of the benefits of this is that it could enable having transfer
   functions that create alarms (and reduce their state using the
   alarm).

   Note: having a state and option monad (state -> '(a * state) option) works 
   but it makes the transfer functions heavier to write. Maybe we should 
   raise an exception instead. *)
module Context_Arity_Forward(Context:Context) = struct
  type 'r ar0 = Context.t -> 'r
  type ('a,'r) ar1 = Context.t -> 'a -> 'r
  type ('a,'b,'r) ar2 = Context.t -> 'a -> 'b -> 'r
  type ('a,'b,'c,'r) ar3 = Context.t -> 'a -> 'b -> 'c -> 'r
  type ('a,'r) variadic = Context.t -> 'a list -> 'r
end



(* Monadic arity is what we should be aiming to get. *)
module Monadic_Context(Context:Context) = struct
  (* This is used to combine complex expressions. *)
  let (let*) m f = fun ctx -> let (v,ctx) = m ctx in f v ctx;;
  type 'r ar0 = Context.t -> ('r * Context.t)
  type ('a,'r) ar1 = 'a -> Context.t -> ('r * Context.t)
  type ('a,'b,'r) ar2 = 'a -> 'b -> Context.t -> ('r * Context.t)
  type ('a,'b,'c,'r) ar3 = 'a -> 'b -> 'c -> Context.t -> ('r * Context.t)
  type ('a,'r) variadic = 'a list -> Context.t -> ('r * Context.t) 
end

(* General design notes:

   - Functors should state its minimal needs and provides the maximum
   features. But optional features of a functor, depending on an
   optional feature of an argument, should be done using "additional
   features functor". This sometimes make their use impractical, as is
   the multiplication of possible configurations. 

   For this reason, we tend to have the "base case" require more
   features than strictly necessary, when these extra features can be
   easily implemented using an approximate version (like the is_empty
   functions).

   TODO: This is the case for backward propagation functions, so they
   could always be required.

   - The types are always lifted, so that it is easy to add type
   equality/substitution in module constraints. Modules are only used
   for things that are not supposed to be changed, because it is
   difficult to "insert" additional values inside a submodule. That is
   why we do not use recursive submodules. 

   - There is a hierarchy of types: boolean is standalone, binary
   depends on boolean, and memory depends on both. 

   - When including modules with additional components (e.g. size_int
   for Ref_Addr), always put the component with largest fields at the
   beginning, else destructive substitution of modules does not work. 

   - The main purpose of this file is to provide a "standard" way to
   name the operations on domains. *)
(* Note: for simplicity, we now just require everybody to provide "Base";
   we have helper "Assert_False" modules to fill the holes when needed. *)


(****************************************************************)
(* Forward transfer functions *)

module type With_Boolean_Forward = sig
  type boolean
  module Context:Context
  module Boolean_Forward:Transfer_functions.Boolean_Forward
    with module Arity := Context_Arity_Forward(Context) and type boolean := boolean
end

module type With_Integer_Forward = sig
  type boolean
  type integer
  module Context:Context
  module Integer_Forward:Transfer_functions.Integer_Forward
    with module Arity := Context_Arity_Forward(Context)
    and type boolean := boolean
    and type integer := integer
end

module type With_Binary_Forward = sig
  type boolean
  type binary
  module Context:Context
  module Binary_Forward:Transfer_functions.Binary_Forward
    with module Arity := Context_Arity_Forward(Context)
    and type boolean := boolean
    and type binary := binary
end

module type With_Memory_Forward = sig
  type boolean
  type binary
  type memory
  module Context:Context
  module Memory_Forward:Transfer_functions.Memory_Forward
    with module Arity := Context_Arity_Forward(Context)
    and type boolean := boolean
    and type binary := binary
    and type memory := memory
end

(****************************************************************)
(* Queries. *)

(* Queries allow to ask the domain an overapproximation of the set of
   concrete objects to which a dimension concretizes. This set of
   object must be finitely represented, but the choice of this
   representation is left to the domain. It is required that these
   objects can be converted to some standard representations.

   In addition, we require this set of object to be represented by a
   lattice, so that it is possible to test inclusion and perform
   approximation of union on these set of objects.
*)

(* Note: since Quadrivalent exactly represents the powerset of
   {true,false}, there is no point in using another type. *)
module type Boolean_Lattice = Single_value_abstraction.Sig.Boolean_Lattice with type t = Lattices.Quadrivalent.t
module type Integer_Lattice = Single_value_abstraction.Sig.Integer_Lattice
module type Binary_Lattice = Single_value_abstraction.Sig.Binary_Lattice
module type Memory_Lattice = Single_value_abstraction.Sig.Memory_Lattice                                                               


module type With_Integer_Queries = sig
  module Context:Context  
  module Integer_Lattice:Integer_Lattice
  type integer
  val query_integer: Context.t -> integer -> Integer_Lattice.t
  include Single_value_abstraction.Sig.Integer_Conversions with type integer := Integer_Lattice.t
end

module type With_Queries = sig
  module Context:Context
  type binary
  type memory

  (* Note: I am trying to phase out these intermediate lattices. *)
  module Query:sig
    module Binary_Lattice:Binary_Lattice

    include Single_value_abstraction.Sig.Binary_Conversions with type binary := Binary_Lattice.t
  (* TODO: This supersedes "truth_value" *)

  val binary: size:int -> Context.t ->  binary -> Binary_Lattice.t

  (* Reachable means that the set of memory states is not empty. 
     - If we return {True,False}, then the memory may be reachable;
     - If we return {True}, then the memory is reachable;
     - If we return {False} or {}, the memory is not reachable. *)
  val reachable: Context.t -> memory -> Quadrivalent.t
  end

  (** [should_focus ~size ctx mem addr] asks the domain whether it is useful to
     "focus" (or "unfold", i.e. try to represent precisely the memory region
     pointed to by [addr], as long as aliasing info ensures that it is sound) a
     loaded value. [size] should be the size of [addr]. If the answer is yes,
     then the returned value should contain three things about the region to
     focus: its base, its size (in bits) and the offset of [addr] in it (in
     bits). *)
  val should_focus : size:int -> Context.t -> memory -> binary ->
    (binary * int * int) option

  (** [may_alias ~ptr_size ~size1 ~size2 ctx addr1 addr2] should return whether
      the region starting at [addr1] of size [size1] bytes and the region
      starting at [addr2] of size [size2] bytes may have a non-empty
      intersection. This function is used by focusing abstractions to discard a
      focused region when writing in a possibly aliased address. [ptr_size] is
      the size in bits of both [addr1] and [addr2]. *)
  val may_alias : ptr_size:int -> Context.t -> size1:int -> size2:int -> binary -> binary -> bool
end

module type With_Types = sig
  module Context : Context
  type binary

  (** Returns an unknown value with a given type. *)
  val binary_unknown_typed : size:int -> Context.t -> Types.Ctypes.typ -> binary
end

(****************************************************************)  
(* Other extensions. *)

module type With_Partitionning = sig
  type 'a decision
  type boolean
  (* The function goes from the strategy to the partitionning map;
     this requires the polymorphic argument. *)
  val boolean_split: ('a -> 'a -> 'a decision) -> boolean -> boolean
end

(****************************************************************)
(* Context *)

module type With_Context = sig
  module Context:Context

  (* Opens a new context, corresponding to the initial scope. *)
  val root_context: unit -> Context.t

  (* Dumps what is known about the current context: useful for debugging. *)
  val context_pretty: Format.formatter -> Context.t -> unit
end

(****************************************************************)
(* Guards *)


module type With_Assume = sig
  type boolean
  module Context:Context

  (** Corresponds to the creation of a new basic block, accessible only 
      if the condition is met. None means bottom. *)
  val assume: Context.t -> boolean -> Context.t option

  (** Because the transfer functions imperatively change the context,
     they cannot use assume, that returns a new context. Temporarily,
     we provide this instead (it should be applied only to fresh
      symbolic variables and not modify the set of valuations of the other symbolic variables.
      In particular, the condition must never make the context bottom).

      The good long-term solution would be to make every transfer
     function return a new Context.t option, viewing the context as
     some state monad. *)
  val imperative_assume: Context.t -> boolean -> unit
  
end


(****************************************************************)
(* Fixpoint iteration, and base for all abstract domains. *)

module type With_Nondet = sig
  module Context:Context

  (* "Joining" variables from two basic blocks together, returning a new basic block.  *)
  (* This joins two basic blocks and returns a new basic block. The
     in_tuple and out_tuple corresponds to the phi operations in SSA.
 *)
  val typed_nondet2: Context.t -> Context.t -> 'a Context.in_tuple -> Context.t * 'a Context.out_tuple

  (* Additionally, one may compute a non-deterministic choice between
     two values in the same basic block

     It can be seen as equivalent as calling typed_nondet2 by passing the same context twice, 
     which would return the same context. *)
  (* Note: this function imperatively modifies the context.
     It should return a new context; this should be done when
     load and store will return a new context. *)
  val nondet_same_context: Context.t -> 'a Context.in_tuple -> 'a Context.out_tuple
  
end

(* Note: all domains could have thee same interface; but some would
   have assert false and unit for the types they do not handle. We
   could even list the types that are handled by a domain, and check
   that everything is correct on functor instantiation. E.g. a functor
   translating memory to binary know how to handle binary and memory,
   and requires binary; it is able to pass the other types through.

   Other things could be checked, e.g. whether the domain is
   relational or not (possible optimizations), or if bottom is
   handled/necessary (evaluating domains do not require elements below
   to have a bottom.) *)

module type With_Fixpoint_Computation = sig
  module Context:Context

  (* Opening a new context can also be seen as opening a new scope:
     new variables can be introduced, but variables of the parent
     scope can still be seen. *)
  val mu_context_open: Context.t -> Context.t

  (* Fixpoint step is a combination of inclusion checking +
     widening. It takes two arguments: a tuple of values which is the previous
     argument of the mu (the previous beginning of the loop), and a
     corresponding tuple which is the result of the evaluation of the body (the
     end of the loop).  Internally, it stores the arguments of the mu.

     It returns a boolean which says if the fixpoint is reached, and a
     function. If the fixpoint is reached, we can "close" the mu, and
     the function returns a tuple corresponding to the mu. We can
     always "restart" the mu, in which case the function returns a new
     arg. *)
  (* MAYBE: return other informations with the tuple, for instance if
     we detect that the function is finitely unrollable. *)
  (* Init is the context leading to the loop entry,
     Arg is the context at the loop entry (obtained by mu_context_open or by the last fixpoint_step operation)
     and body the one at the end of the loop.  *)
  val typed_fixpoint_step: init:Context.t -> arg:Context.t -> body:Context.t -> (bool * 'a Context.in_tuple) ->
    bool * (close:bool -> 'a Context.out_tuple * Context.t)
end

module Fresh_id:sig
  type t = private int
  val fresh: string -> t (* Arg: the name of the module; may be useful later. *)
end = struct
  type t = int
  let count = ref 0
  let fresh name =
    let v = incr count; !count in
    Codex_log.debug "Registering domain %s with id %d" name v;
    v
end


(* Identifying domains. *)
module type With_Id = sig
  val unique_id: Fresh_id.t
  val name: string
end

(**************** Optional types that can be used in the domain. ****************)



(****************************************************************)
(* Base module types describing operations on one or several types of terms. *)

(* Notes on the base operations:

   - Pretty is required everywhere, and used for debugging.

   - Equality refers to equality of the concretization. It can be
   approximate, i.e. it is ok to return false when we cannot detect
   that elements are equal; however when used as keys of
   datastructures, equality should probably at least return true for
   elements that are (==).

   - TODO: Do compare and hash have to respect equality? Map and Set
   do not need "equal", but Hashtbl does. So it seems that at least
   hash should respect equality, i.e. equal elements should have the
   same hash; which is not obvious when structurally different
   elements are detected as equal (e.g. different representations of
   empty). Or maybe it does not need, but in this case it is
   undefined whether different abstract values with same
   concretization represent different binding in the table (if by
   chance the hash is the same, they will share a binding; else they
   may have different bindings). 

   - compare and hash do not need to be implemented if the
   datastructures are not used.

*)


(* We document the boolean cases, as integer are pretty similar. *)

module type With_Boolean = sig
  module Context:Context
  type boolean

  module Boolean:Datatype_sig.S with type t = boolean
  val boolean_pretty: Context.t -> Format.formatter -> boolean -> unit
  
  val serialize_boolean: Context.t -> boolean -> Context.t -> boolean -> 'a Context.in_acc -> (boolean,'a) Context.result

  (* Empty denotes that the concretization has no value (or it is
     the concrete value representing the absence of value). Note
     that this does not necessarily imply that some error occured;
     for instance the offset of an uninitialized pointer can be
     represented with empty. Emptyness testing is a simple way of
     communicating between domains. *)
  val boolean_empty: Context.t -> boolean

  (* TODO: get rid of these levels, the context suffices now that it is flow-sensitive. *)
  val boolean_unknown: Context.t -> boolean
  module Boolean_Forward:Transfer_functions.Boolean_Forward
    with module Arity := Context_Arity_Forward(Context)
     and type boolean := boolean

  val query_boolean: Context.t -> boolean -> Lattices.Quadrivalent.t
end


module type With_Integer = sig
  module Context:Context
  type integer
  type boolean
  module Integer:Datatype_sig.S with type t = integer

  (* TODO: An "integer_value", that returns a range (and congruence
     information) of the value. And perhaps another representation
     that uses multi-intervals. *)

  (* Can return true if provably empty; false is always safe.  *)
  val integer_is_empty: Context.t -> integer -> bool
  val integer_pretty: Context.t -> Format.formatter -> integer -> unit
  
  val serialize_integer: Context.t -> integer -> Context.t -> integer -> 'a Context.in_acc -> (integer,'a) Context.result
  val integer_empty: Context.t -> integer
  val integer_unknown: Context.t -> integer
  module Integer_Forward:Transfer_functions.Integer_Forward
    with module Arity := Context_Arity_Forward(Context)
     and type boolean := boolean
     and type integer := integer
end


module type With_Binary = sig
  module Context:Context
  type binary
  type boolean

  module Binary:Datatype_sig.S with type t = binary
  val binary_pretty: size:int -> Context.t -> Format.formatter -> binary -> unit

  val serialize_binary: size:int -> Context.t -> binary -> Context.t -> binary -> 'a Context.in_acc -> (binary,'a) Context.result
  val binary_empty: size:int -> Context.t -> binary
  val binary_unknown: size:int -> Context.t -> binary
  include With_Binary_Forward with module Context := Context
                                and type binary := binary
                                and type boolean := boolean
end


module type With_Memory = sig
  module Context:Context
  type boolean    
  type binary
  type memory
  
  val serialize_memory: Context.t -> memory -> Context.t -> memory -> 'a Context.in_acc -> (memory,'a) Context.result
  val memory_pretty: Context.t -> Format.formatter -> memory -> unit

  
  (* val memory_empty: Context.t -> memory *)
  (* val memory_unknown: level:int -> Context.t -> memory *)
  include With_Memory_Forward with module Context := Context
                               and type memory := memory
                               and type binary := binary
                               and type boolean := boolean
end

(**************** Complete instantiations. ****************)


(** This signature is useful when we don't have any new flow-sensitive state and just
    need all the things on the top of the stack to stay the same. *)
module type Minimal_No_Boolean = sig
  include With_Id
  include With_Context

  type boolean

  (* Guards *)
  include With_Assume with module Context := Context
                       and type boolean := boolean

  (* Joining variables together. *)
  include With_Nondet with module Context := Context
  
  (* Fixpoint computation. *)
  include With_Fixpoint_Computation with module Context := Context

end


(** This signature does not have pre-built values, except booleans.  *)
module type Minimal = sig
  include Minimal_No_Boolean

  (* The boolean domain should be present everywhere, as we need it or guards. *)
  include With_Boolean with module Context := Context
                        and type boolean := boolean
end
(* Note: We could have store a "current_context" context inside the
   domain, which would avoids the need to pass context,mu_context
   etc. everytime. *)
(* Note: we could now organize this differently, using different types. *)
module type Base = sig
  include Minimal

  type binary
  type memory

  include With_Queries with module Context := Context
                             and type binary := binary
                             and type memory := memory

  include With_Types with module Context := Context
                     and type binary := binary

  include With_Binary with module Context := Context
                        and type binary := binary
                        and type boolean := boolean

  include With_Memory with module Context := Context
                        and type memory := memory
                        and type binary := binary
                        and type boolean := boolean

  (* Builtin functions. *)
  include Transfer_functions.Builtin.With_Builtin with type binary := binary
                                                   and type boolean := boolean
                                                   and type memory := memory
                                                   and module Context := Context

  (* Set operations. Note that we do not distinguish binary from binary sets. *)
  (* Note that union reuses the serialize machinery. *)
  val union: Transfer_functions.Condition.t -> Context.t -> 'a Context.in_tuple -> 'a Context.out_tuple

  (* Check if an assertion is satisfiable (i.e. a trace exists that makes it true). *)
  val satisfiable: Context.t -> boolean -> Smtbackend.Smtlib_sig.sat

  (* Check if the memory is reachable. sat means reachable. *)
  val reachable: Context.t -> memory -> Smtbackend.Smtlib_sig.sat
  
end

module type Base_with_integer = sig
  include Base
  include With_Integer with module Context := Context
                        and type boolean := boolean
  include With_Integer_Queries with module Context := Context
                                and type integer := integer
      
end

(****************************************************************)
(* Context conversion procedures: pass through the values by just
   changing the context. *)
module type Convert_Contexts = sig
  module From:Context
  module To:Context
  val convert: From.t -> To.t
end

module Make_Convert(C:Convert_Contexts) = struct
  (* Note: context conversion goes in the opposite direction than
     transfer function conversion.*)
  module From_Arity = Context_Arity_Forward(C.To)
  module To_Arity = Context_Arity_Forward(C.From)
  let ar0 f ctx = f (C.convert ctx)
  let ar1 = ar0
  let ar2 = ar0
  let ar3 = ar0
  let variadic = ar0
end

module Convert_Boolean_Forward
  (C:Convert_Contexts)
  (D:With_Boolean_Forward with module Context = C.To) =
struct
  module C = Make_Convert(C)
  module F = struct include D;; include D.Boolean_Forward end
  include Transfer_functions.Conversions.Convert_Boolean_Forward(C)(F)
end

module Convert_Integer_Forward
  (C:Convert_Contexts)
  (D:With_Integer_Forward with module Context = C.To) =
struct
  module C = Make_Convert(C)
  module F = struct include D;; include D.Integer_Forward end
  include Transfer_functions.Conversions.Convert_Integer_Forward(C)(F)
end


module Convert_Binary_Forward
  (C:Convert_Contexts)
  (D:With_Binary_Forward with module Context = C.To) =
struct
  module C = Make_Convert(C)
  module F = struct include D;; include D.Binary_Forward end
  include Transfer_functions.Conversions.Convert_Binary_Forward(C)(F)
end

module Convert_Memory_Forward
  (C:Convert_Contexts)
  (D:With_Memory_Forward with module Context = C.To) =
struct
  module C = Make_Convert(C)
  module F = struct include D;; include D.Memory_Forward end
  include Transfer_functions.Conversions.Convert_Memory_Forward(C)(F)
end


(* This will help to the transition in a top-down manner, starting
   from the translation and top-level domain to the lower-level
   domain.

   The idea is to support both interfaces, and use conversion to
   simplify the support for both. I can have a signature for both
   domains, and an "AddMonadic" functor to support both domains. *)
module Convert_to_monadic(D:Base) = struct

  module Conversion = struct
    module From_Arity = Context_Arity_Forward(D.Context);;
    module To_Arity = Monadic_Context(D.Context);;
    let ar0 f = (fun ctx -> f ctx, ctx)
    let ar1 f = (fun a ctx -> f ctx a,ctx)
    let ar2 f = (fun a b ctx -> f ctx a b,ctx)
    let ar3 f = (fun a b c ctx -> f ctx a b c,ctx)                                
  end

  module Types = struct
    type boolean = D.boolean
    type binary = D.binary
    type memory = D.memory
  end
  
  module Boolean_Forward = Transfer_functions.Conversions.Convert_Boolean_Forward(Conversion)(struct include Types include D.Boolean_Forward end)
  module Binary_Forward = Transfer_functions.Conversions.Convert_Binary_Forward(Conversion)(struct include Types include D.Binary_Forward end)
  module Memory_Forward = Transfer_functions.Conversions.Convert_Memory_Forward(Conversion)(struct include Types include D.Memory_Forward end)
  
end
