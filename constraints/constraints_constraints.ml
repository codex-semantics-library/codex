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

include Constraints_constraints_sig;;

module TC = Transfer_functions.Term;;

module type Parents = sig
  type 'a parents
  module Make(Param:sig
      type elt
      val get_parents: elt -> elt parents
      val set_parents: elt -> elt parents -> unit
    end):sig
    (* An empty set of parents. *)
    val create: unit -> Param.elt parents

    (* Add one parent to one child. *)
    val add_one_parent: child:Param.elt -> parent:Param.elt -> unit

    (* Iterate on all parents of a child. *)
    val iter_on_parents: child:Param.elt -> (Param.elt -> unit) -> unit
  end
end


module ParentsWeakArray:Parents = struct

  (* WeakSet takes too much memory, we use weak array instead. *)
  type 'a parents = 'a Weak.t


  module Make(P:sig
      type elt
      val get_parents: elt -> elt parents
      val set_parents: elt -> elt parents -> unit
    end) = struct

    (* We assume that most constraints will have at least one parent.  *)
    let create() = Weak.create 1;;


    (* Linear search *)
    let add_one_parent ~child ~parent =
      try 
        let parents = P.get_parents child in
        let length = Weak.length parents in
        assert (length > 0);
        for i = 0 to length - 1 do
          if not (Weak.check parents i) 
          then begin
            Weak.set parents i (Some parent);
            raise Exit
          end
        done;
        (* Complexity warning. Note that we could always drop parents
           if we really need to. *)
        (* Note: I think this now happens many when I do weak updates:
           the new value is in a nondet with all the cells in the array. *)
        if length >= 32
        then
          (Codex_log.performance_warning
             "Complexity warning: a constrain has more than %d parents" length;
           (* raise Exit *)
           (* Kernel.warning "child %a parent %a" *)
           (*  length Pretty.Pretty.pretty child Pretty.Pretty.pretty parent *)
          );

        let newparents = Weak.create (2 * length) in
        Weak.blit parents 0 newparents 0 length;
        P.set_parents  child newparents;
        Weak.set newparents length (Some parent)
      with Exit -> ()
    ;;

    let iter_on_parents ~child f =
      let parents = P.get_parents child in
      for i = 0 to (Weak.length parents) - 1 do
        match Weak.get parents i with
        | None -> ()
        | Some x -> f @@ x
      done
    ;;

  end

    (* MAYBE: also add the index of the last parent added, and start
       from here. This should allow amortized O(1) insertion if we
       explore from the start when the array is full, and O(1) if
       we do not try to reuse de-allocated array cells.
       
       We could also have a bitmap with the free elements in an int,
       and find the correct one with find_first_set. The bitmap would
       be updated on iter_on_parents. More complex, and probably not
       as useful.

       To have small bitmaps unboxed and large boxed, we could just
       use Zarith; there are popcount and trailing_zeros functions.

       For now, just warn when we have a large number of parents,
       which is the main problem anyway. The goal is to have a low
       memory consumption, so an extra index is useless. *)
  
end

module ParentsUnit:Parents = struct

  (* WeakSet takes too much memory, we use weak array instead. *)
  type 'a parents = unit


  module Make(P:sig
      type elt
      val get_parents: elt -> elt parents
      val set_parents: elt -> elt parents -> unit
    end) = struct

    let create() = ()
    let add_one_parent ~child ~parent = ()
    let iter_on_parents ~child f = ()
  end
end

let _parent_maker:(module Parents) =
  if Codex_config.constraints_register_parents()
  then (module ParentsWeakArray)
  else (module ParentsUnit);;
module ParentMaker = (val _parent_maker);;

module MakeConstraints(Condition:Condition_map.Condition)() = struct
  type boolean = TC.boolean
  type integer = TC.integer
  type binary = TC.binary

  module Condition = Condition
  
  type ('arg,'res) term = ('arg,'res) TC.term

  module Id:sig
    type 'a t = Id: int -> 'a t  [@@unboxed];;
    val fresh: unit -> 'a t
    val equal: 'a t -> 'b t -> bool
    val hash: 'a t -> int
    val to_int: 'a t -> int
  end = struct

    type 'a t = Id: int -> 'a t [@@unboxed];;

    let count = ref 0;;
    let fresh() = incr count; Id !count;;

    let equal (Id a) (Id b) = a == b;;

    let hash (Id a) = a;;

    let to_int = hash

  end

    type level = int;;

      (* MAYBE: Completely replace booleans by bdds. Alternatively: hide
         the presence of bdd, we only manipulate boolean constraints. *)
    type 'res complete_term =
    | Tuple_get: int * tuple_ -> 'res complete_term
    | Empty: 'res complete_term
    | Unknown: level -> 'res complete_term
    | Mu_formal:{ level:level; actual: ('res t * TC.boolean t) } -> 'res complete_term 
    | T0: { tag: (TC.ar0,'res) term } -> 'res complete_term
    | T1: { tag: ('a TC.ar1,'res) term; a: 'a t; level:level} -> 'res complete_term
    | T2: { tag: (('a,'b) TC.ar2,'res) term; a: 'a t; b:'b t; level:level } -> 'res complete_term

  and 'res t =
    | Bool: { mutable parents: parents; id: boolean Id.t; term: boolean complete_term; bdd:Condition.t} -> boolean t
    | Integer: { mutable parents: parents; id: integer Id.t; term: integer complete_term; } -> integer t
    | Binary: { mutable parents: parents; id: binary Id.t; term: binary complete_term; size: int} -> binary t

(* TODO: Cannot be unboxed (yet) because of the #!@# float array
   hack...?  Actually , it seems more to be a bug in OCaml: the value
   is never a float. Note: seems to be fixed in 4.11 *)
  and any = Any: 'res t -> any [@@boxed ]

(* Note: Cannot use any (because the boxing clashes with using Weak),
   and we need to make the type disappear: so we use Obj.t instead. *)
  and parents = Obj.t ParentMaker.parents

  (* The tuple that we manipulate *)
  and tuple = any Immutable_array.t

  (* The real tuple. *)
  and tuple_ =
    | Nondet of {id: tuple Id.t; level: level;
                 a:tuple; conda_bool: boolean t;
                 b:tuple; condb_bool: boolean t}    
    | Mu of { id: tuple Id.t; level:level; init:tuple;var:tuple;body:tuple;body_cond:boolean t}

   type 'a constraints = 'a t

  let get_id: type a. a t -> a Id.t = function
    | Bool {id} -> id
    | Integer {id} -> id
    | Binary {id} -> id

  let get_term: type a. a t -> a complete_term = function
    | Bool {term} -> term
    | Integer {term} -> term
    | Binary {term} -> term

  let get_id_int: type a. a t -> int = function
    | Bool {id=Id.Id id} -> id
    | Integer {id=Id.Id id} -> id
    | Binary {id=Id.Id id} -> id

  let compare: type a b. a t -> b t -> int = fun a b ->
    Stdlib.compare (get_id_int a) (get_id_int b)

  let equal: type a b. a t -> b t -> bool = fun a b ->
    (get_id_int a) == (get_id_int b)

 (* Relies on the fact that for all variants, the position is the same in memory. *)
  module Unsafe = struct

    let get_id: type a. a t -> a Id.t = fun x ->
      let x:TC.boolean t = (Obj.magic x) in
      match x with
      | Bool {id} -> (Obj.magic id)
    ;;

    let get_term: type a. a t -> a complete_term = fun x ->
      let x:TC.boolean t = (Obj.magic x) in
      match x with
      | Bool {term} -> (Obj.magic term)
    ;;

    let get_id_int: type a. a t -> int = fun x ->
      let x:TC.boolean t = (Obj.magic x) in
      match x with
      | Bool {id=Id.Id id} -> id
    ;;
    
    let compare: type a b. a t -> b t -> int = fun a b ->
      Stdlib.compare (get_id_int a) (get_id_int b)

    let equal: type a b. a t -> b t -> bool = fun a b ->
      (Obj.magic a) == (Obj.magic b)
  end

  include Unsafe


  let hash x = get_id_int x;;

  let any_equal (Any a) (Any b) = Id.equal (get_id a) (get_id b)
  ;;

  let level_term: type a. a complete_term -> level = function
    | Tuple_get(_,Nondet{level}) -> level
    | Tuple_get(_,Mu{level}) -> level      
    | Empty -> -1
    | Unknown level -> level
    | Mu_formal {level} -> level
    | T0 _ -> -1
    | T1 {level} -> level
    | T2 {level} -> level
  ;;

  let level x = level_term @@ get_term x;;

  let size_of: binary t -> int = function Binary{size} -> size
  

  module Any = struct

    type 'a constraints = 'a t
    type t = any

    let hash (Any x) = get_id_int x

    let equal (Any a) (Any b) = (get_id_int a) == (get_id_int b);;

    let get_id_int (Any a) = get_id_int a;;

    let compare a b = Stdlib.compare (get_id_int a) (get_id_int b)
  end

  module Tuple = struct
    type t = tuple_
    let equal a b = match (a,b) with
      | Mu{id=Id.Id(id1)}, Mu{id=Id.Id(id2)} -> id1 == id2
      | Nondet{id=Id.Id(id1)}, Nondet{id=Id.Id(id2)} -> id1 == id2
      | _ -> false
    let hash = function
      | Mu{id=Id.Id(id)} -> id
      | Nondet{id=Id.Id(id)} -> id
  
    let compare a b = Stdlib.compare (hash a) (hash b)
  end
  
  module Pretty = struct
    type 'a pack =
      | Unit: TC.ar0 pack
      | Single: 'a t -> 'a TC.ar1 pack
      | Pair: 'a t * 'b t -> ('a,'b) TC.ar2 pack

    module rec PrettyArg:TC.Pretty_Arg
      with type 'a pack = 'a pack
       and type 'a t = 'a t 
    = struct
      type nonrec 'a t = 'a t
      type nonrec 'a pack = 'a pack
      (* The eta expansion is important to avoid having a recursive module exception. *)
      let pretty fmt x = Pretty.pretty fmt x
      let extract1 (Single x) = x
      let extract2 (Pair(a,b)) = a,b
    end
    and PrettyApply:TC.Pretty_Result
      with type 'a pack = 'a pack
       and type 'a t = 'a t
    = TC.Pretty(PrettyArg)
    and Pretty:sig
      val pretty: Format.formatter -> 'a t -> unit
      val pretty_deps: Format.formatter -> 'a t -> unit
      val pretty_tuple_deps: Format.formatter -> tuple -> unit
      val pretty_tuple: Format.formatter -> tuple -> unit
    end
    = struct

      (* If we don't use prefer_predictability_over_performance, then 
         already printed definitions may come again in the logs. *)
      module AnyEphe =
        (val if Codex_config.hash_cons_constraints_with_weak_table()
          then (module Ephemeron.K1.Make(Any):Hashtbl.S with type key = Any.t)
          else (module Hashtbl.Make(Any):Hashtbl.S with type key = Any.t))
      ;;

      module TupleEphe =
        (val if Codex_config.hash_cons_constraints_with_weak_table()
          then (module Ephemeron.K1.Make(Tuple):Hashtbl.S with type key = Tuple.t)
          else (module Hashtbl.Make(Tuple):Hashtbl.S with type key = Tuple.t))
      ;;

      (* Ensures that definitions are printed only once. *)
      let printed = AnyEphe.create 17;;
      let printed_tuple = TupleEphe.create 17;;

      let rec pretty_definition: type a. Format.formatter -> a t -> unit = fun fmt cons ->
        if not @@ AnyEphe.mem printed (Any cons)
        then begin
          AnyEphe.replace printed (Any cons) ();
          match cons with
          | Bool{id=Id.Id id;term} -> pretty_definition_complete_term "b" id fmt term;
          | Integer{id=Id.Id id;term} -> pretty_definition_complete_term "i" id fmt term;
          | Binary{id=Id.Id id;term} -> pretty_definition_complete_term "B" id fmt term;
        end

      and pretty_definition_complete_term:
        type a. string -> int -> Format.formatter -> a complete_term -> unit =
        fun str id fmt cons ->

      let print_def tag x =
        Format.fprintf fmt "let %s%d = " str id;
        PrettyApply.pretty_destruct fmt tag x;
        Format.fprintf fmt "@\n"
      in match cons with
        | T0{tag=(TC.Biconst _)} -> ()
        | T0{tag} -> print_def tag Unit
        | T1{tag;a} -> pretty_definition fmt a; print_def tag (Single a)
        | T2{tag;a;b} -> pretty_definition fmt a; pretty_definition fmt b; print_def tag (Pair(a,b))
        | Tuple_get(i,tup) ->
          pretty_definition_tuple fmt tup;

          Format.fprintf fmt "let %s%d = tuple_get(%d,%a)@\n" str id i pretty_tuple tup
        | Empty -> Format.fprintf fmt "let %s%d = empty@\n" str id
        | Unknown _level -> Format.fprintf fmt "let %s%d = unknown(%d)@\n" str id _level
        | Mu_formal {level} -> Format.fprintf fmt "let %s%d = var(%d)@\n" str id level

      and pretty: type a. Format.formatter -> a t -> unit = fun fmt cons ->
        (* assert (AnyWeak.mem printed (Any cons)); *)
        pretty_definition fmt cons;
        match cons with
        | Bool{id=Id.Id id;term} -> Format.fprintf fmt "b%d" id
        | Integer{id=Id.Id id;term} -> Format.fprintf fmt "i%d" id
        | Binary{term=T0{tag=(TC.Biconst(size,k)) as tag}} -> PrettyApply.pretty_destruct fmt tag Unit
        | Binary{id=Id.Id id;term} -> Format.fprintf fmt "B%d" id

      and pretty_tuple fmt tup =
        pretty_definition_tuple fmt tup;
        match tup with
        | Nondet({id=Id.Id id}) -> Format.fprintf fmt "T%d" id
        | Mu {id=Id.Id id} -> Format.fprintf fmt "T%d" id                          

      and pretty_definition_tuple fmt tup =
        if not @@ TupleEphe.mem printed_tuple tup
        then begin
          TupleEphe.replace printed_tuple tup ();
          match tup with
          | Nondet({id=Id.Id id;a;b;conda_bool;condb_bool}) ->
            pretty_definition_tuple_ fmt a; 
            pretty_definition_tuple_ fmt b;
            pretty_definition fmt conda_bool;
            pretty_definition fmt condb_bool;            
            Format.fprintf fmt "let T%d = nondet(@[<hv> assume(@[<hv>%a@],@[<hv> %a@]),@ assume(@[<hv>%a@],@[<hv> %a@])@])@\n" id pretty conda_bool pretty_tuple_ a pretty condb_bool pretty_tuple_ b
          | Mu{id=Id.Id id;var;body;init;body_cond} ->
            pretty_definition_tuple_ fmt init;
            Format.fprintf fmt "let T%d = %a |> (mu %a" id pretty_tuple_ init pretty_tuple_ var;
            Format.fprintf fmt ".@,  @[<hv>";            
            pretty_definition_tuple_ fmt body;
            pretty_definition fmt body_cond;
            Format.fprintf fmt "assume(%a,%a)@])@\n" pretty body_cond pretty_tuple_ body 
        end

      and pretty_any fmt = function
        | Any x -> pretty fmt x

      and pretty_definition_any fmt = function
        | Any x -> pretty_definition fmt x

      and pretty_tuple_ fmt x =
        Immutable_array.pp_array ~pre:"<" ~suf:">" ~sep:"," pretty_any fmt x

      and pretty_definition_tuple_ fmt x = 
        Immutable_array.iter (pretty_definition_any fmt) x
      ;;

      let pretty_deps = pretty_definition
      let pretty_tuple_deps = pretty_definition_tuple_
      let pretty_tuple = pretty_tuple_    

    end
  end

  include Pretty.Pretty


  module Parents = struct

    module Param = struct

      type elt = Obj.t
      
      let get_parents: type a. a constraints -> parents = function
        | Bool{parents} -> parents
        | Integer{parents} -> parents
        | Binary{parents} -> parents
      ;;

      let get_parents x = get_parents @@ Obj.obj x
      
      let set_parents: type a. a constraints -> parents -> unit = fun x parents ->
        match x with
        | Bool r -> r.parents <- parents
        | Integer r  -> r.parents <- parents
        | Binary r -> r.parents <- parents
      ;;

      let set_parents x par = set_parents (Obj.obj x) par
      
    end

    module Made = ParentMaker.Make(Param)

    let create = Made.create

    let add_one_parent ~child ~parent =
      (* Constants cannot be improved, and have lots of parents.  Do
         not register parents for constants.  *)
      let is_constant: type a. a constraints -> 'b = function
        | Bool{term=(T0 _ |Empty) } -> true
        | Integer{term=(T0 _ |Empty) } -> true
        | Binary{term=(T0 _ |Empty) } -> true
        | _ -> false
       in
       if is_constant child then ()
       else Made.add_one_parent ~child:(Obj.repr child) ~parent:(Obj.repr parent)
    ;;

    let add_to_childs: type a. a constraints -> unit = fun parent ->
      let add_parent_term: type res. res constraints -> res complete_term -> unit = fun parent term ->
        match term with
        | Empty | Unknown _ | Mu_formal _ | T0 _ -> ()
        | Tuple_get(i,Mu _) -> ()       (* Do not redo fixpoints. *)
        | Tuple_get(i,Nondet{a;b}) ->
          let Any xa = Immutable_array.get a i in
          add_one_parent ~child:xa ~parent;
          let Any xb = Immutable_array.get b i in
          add_one_parent ~child:xb ~parent
        | T1 {a} -> add_one_parent ~child:a ~parent
        | T2 {a;b} ->
          add_one_parent ~child:a ~parent; 
          add_one_parent ~child:b ~parent
      in
      let term = get_term parent in
      add_parent_term parent term
    ;;

    (* With this we can remove the set of parents altogether (thereby
       allowing for no forward constraint propagation), if it takes
       too long (e.g. for now, on huge arrays which should be dealt
       with.) *)
    (* let add_to_childs _ = ();; *)

    let iter_on_parents child f =
      let child = Obj.repr child in
      let f x = f (Any (Obj.obj x)) in
      Made.iter_on_parents ~child f

    
  end

  (* Note: my benchs showed that, at least for the way I generate
     constraints, hashconsing is very important. *)
  module HashCons = struct

    (* http://burtleburtle.net/bob/hash/doobs.html *)
    (* http://www.cse.yorku.ca/~oz/hash.html *)
    let sdbm x y = x * 65599 + y;;
    let sdbm x y = y + (x lsl 16) + (x lsl 6) - x;;

    let hash2 x y = sdbm x y;;
    let hash3 x y z = sdbm x @@ sdbm y z
    let hash4 x y z t = sdbm x @@ hash3 x y z 

    (* Note: we use different hashtables according to the type of the
       complete term. This should reduce the amount of collisions? *)
    

    module Param = struct

      let equal: type a. a complete_term -> a complete_term -> bool = fun a b ->
        match (a,b) with
        | Tuple_get(i,x),Tuple_get(j,y) -> i == j && (assert false)
        | Empty,_ -> assert false
        | Unknown _,_ -> assert false
        | Mu_formal _ ,_ -> assert false
        | T0 {tag=tag1},T0{tag=tag2} -> TC.equal tag1 tag2
        | T1 {tag=tag1;a=a1},T1{tag=tag2;a=a2} ->
          TC.equal tag1 tag2 && (get_id_int a1) == (get_id_int a2)
        | T2 {tag=tag1;a=a1;b=b1},T2{tag=tag2;a=a2;b=b2} ->
          TC.equal tag1 tag2 && (get_id_int a1) == (get_id_int a2) && (get_id_int b1) == (get_id_int b2)
        | _ -> false
      
      let hash: type a. a complete_term -> int = function
        | Tuple_get(i,_) -> assert false (* TODO: create an id for tuples. *)
        | Empty -> assert false          (* Should not be hashed. *)
        | Unknown _ -> assert false        (* Should not be hashed: created every time. *)
        | Mu_formal _ -> assert false            (* Should not be hashed: fresh every time. *)
        | T0 {tag } -> TC.hash tag
        | T1 {tag; a} -> hash2 (TC.hash tag) (get_id_int a)
        | T2 {tag; a; b} -> hash3 (TC.hash tag) (get_id_int a) (get_id_int b)


    end


    (* Note: If prefer_predictability_over_performance is false, the
       variables numbers can vary according to garbage collection
       cycles.

       TODO We should probably hash-cons nothing, or just the
       constants, this should be predictable and probably more
       efficient. 
      
       Note: actually my benchmarks seem to run better 
       (more efficiently, and often taking less memory) when this is activated. *)
    module BoolParam = struct type t = boolean complete_term include Param end
    module HashBool =
      (val if Codex_config.hash_cons_constraints_with_weak_table()
        then (module Ephemeron.K1.Make(BoolParam):Hashtbl.S with type key = boolean complete_term)
        else (module Hashtbl.Make(BoolParam):Hashtbl.S with type key = boolean complete_term))
    ;;

    module IntParam = struct type t = integer complete_term include Param end
    module HashInt =
      (val if Codex_config.hash_cons_constraints_with_weak_table()
        then (module Ephemeron.K1.Make(IntParam):Hashtbl.S with type key = integer complete_term)
        else (module Hashtbl.Make(IntParam):Hashtbl.S with type key = integer complete_term))
    ;;

    module BinParam = struct type t = binary complete_term include Param end
    module HashBin =
      (val if Codex_config.hash_cons_constraints_with_weak_table()
        then (module Ephemeron.K1.Make(BinParam):Hashtbl.S with type key = binary complete_term)
        else (module Hashtbl.Make(BinParam):Hashtbl.S with type key = binary complete_term))
    ;;

    let bool_hash = HashBool.create 117;;
    let int_hash = HashInt.create 117;;
    let bin_hash = HashBin.create 117;;
    module BDDHash = Ephemeron.K1.Make(Condition);;
    let bddhash = BDDHash.create 117;; 

    module X = Hashtbl.Make(Condition);;

    let boolean term bdd =
      try HashBool.find bool_hash term
      with Not_found ->
        let bdd = bdd() in
        try let x =  BDDHash.find bddhash bdd in
          (* Kernel.feedback "found boolean by bdd: old %a new %a" pretty x pretty (Bool{parents=Parents.create();term;bdd;id=Id.fresh()}); *)
          x
        with Not_found ->
          let res = 
            Bool { parents = Parents.create(); id = Id.fresh(); term; bdd }
          in
          (* Codex_log.feedback "Creating boolean %a" pretty res; *)
          HashBool.replace bool_hash term res;
          BDDHash.replace bddhash bdd res;
      (*   Kernel.feedback "bdd: %a term %a" Condition.pretty bdd *)
      (* pretty res; *)
        Parents.add_to_childs res;
        res
    ;;

    let integer term =
      try HashInt.find int_hash term
      with Not_found ->
        let res = 
          Integer { parents = Parents.create(); id = Id.fresh(); term }
        in HashInt.replace int_hash term res;
        Parents.add_to_childs res;
        res
    ;;

    let binary ~size term =
      try HashBin.find bin_hash term
      with Not_found ->
        let res = 
          Binary { parents = Parents.create(); id = Id.fresh(); term; size }
        in HashBin.replace bin_hash term res;
        Parents.add_to_childs res;
        res
    ;;
    
  end

(* Similar to Hashcons, except that we do not need to hashcons because
   what we create is always fresh. *)
  module Fresh = struct

    (* Note: possibly no need to call add_to_childs here. *)

    let boolean term bdd =
      let res = 
        Bool { parents = Parents.create(); id = Id.fresh(); term; bdd } in
      HashCons.BDDHash.replace HashCons.bddhash bdd res;
      Parents.add_to_childs res;
      res
    ;;
    
    let integer term =
      let res = Integer { parents = Parents.create(); id = Id.fresh(); term } in
      Parents.add_to_childs res; res
    ;;

    let binary ~size term =
      let res = Binary { parents = Parents.create(); id = Id.fresh(); term; size } in
      Parents.add_to_childs res; res
    ;;

  end

  module Build = struct

    (* Note on smart constructors rewriting. Since we also store the
       conditions for correct definition, we can perform all kind of
       rewrites; it is OK to have 0 * x -> 0 (we don't loose the
       condition on x, because it is already handled by the caller of
       this module).

       But we have to avoid creating new terms (which may not have
       been evaluated by the domains); we can only reuse already
       existing terms. The only exception to this is the creation of
       new constant terms: we mandate that the domains are able to
       'auto-evaluate' them. *)
    module Boolean = struct

      let bunion (Bool ba as a) (Bool bb as b) =
        HashCons.boolean
          (T2 { tag = TC.BoolUnion; a; b; level = max (level a) (level b)})
          (fun () -> Condition.union ba.bdd bb.bdd)
      ;;
      
      let (&&) (Bool ba as a) (Bool bb as b) =
        HashCons.boolean
          (T2 { tag = TC.And; a; b; level = max (level a) (level b)})
          (fun () -> Condition.inter ba.bdd bb.bdd);;

      (* Optimized version. Maybe not worth it. *)
      let (&&) (Bool ba as a) (Bool bb as b) =
        match ba.term,bb.term with
        | (T0 {tag = TC.True}), _ -> b
        | (T0 {tag = TC.False}), _ -> a
        | _, (T0 {tag = TC.True}) -> a
        | _, (T0 {tag = TC.False}) -> b
        | _, _ when a == b -> a
        | _ -> (&&) a b
      ;;

      let (||) (Bool ba as a) (Bool bb as b) =
        HashCons.boolean
          (T2 { tag = TC.Or; a; b; level = max (level a) (level b)})
          (fun () -> Condition.union ba.bdd bb.bdd);;

      (* Optimized version. Maybe not worth it. *)
      let (||) (Bool ba as a) (Bool bb as b) =
        match ba.term,bb.term with
        | (T0 {tag = TC.True}), _ -> a
        | (T0 {tag = TC.False}), _ -> b
        | _, (T0 {tag = TC.True}) -> b
        | _, (T0 {tag = TC.False}) -> a
        | _, _ when a == b -> a
        | _ -> (||) a b
      ;;

      (* Note: we need to use boolean_with_id here, because the value
         of the bdd field is special. *)
      let true_ = Fresh.boolean (T0{tag=TC.True}) Condition.all;;
      let false_ = Fresh.boolean (T0{tag=TC.False}) Condition.empty;;

      (* Optimisation: avoid creating terms when we can.  *)
      let not ((Bool ba as a):boolean t) =
        match ba.term with
        | T0{tag= TC.True} -> false_
        | T0{tag= TC.False} -> true_
        | _ ->
          HashCons.boolean
            (T1 { tag = TC.Not; a; level = level a})
            (fun () -> Condition.complement ba.bdd)
      ;;


      let unknown ~level =
        assert(level >= 0);
        Fresh.boolean (Unknown level) @@ Condition.var ()


      (* Both representations of empty are possible: a variable which
         is initially empty, and a special empty constructor. We favor
         the second which is semantically better grounded (and yields
         better translations, etc.). But using the former sometimes
         slightly improves performances. *)
      (* let empty = unknown ~level:(0);; *)
      let empty = Fresh.boolean Empty @@ Condition.var();;

      (* It is important to use these smart constructors to avoid
         creating huge BDDs representing empty. *)
      let (&&) a b =
        if a == empty then empty
        else if b == empty then empty
        else (&&) a b
      ;;

      let not a =
        if a == empty then empty
        else not a
      ;;
      
      let (||) a b =
        if a == empty then empty
        else if b == empty then empty
        else (||) a b
      ;;

      type boolean = TC.boolean t
      let assume _ = assert false (* Not used. *)

      
    end

    module Integer = struct
      let iconst k = HashCons.integer @@ T0 { tag=TC.Iconst k};;
      let itimes k a = HashCons.integer @@ T1 { tag=TC.Itimes k; a; level = level a};;
      let zero = iconst Z.zero
      let one = iconst Z.one
      let unknown ~level = Fresh.integer (Unknown level)
      let empty = Fresh.integer Empty
      
      let ar2_integer_integer_integer tag a b =
        HashCons.integer @@ T2 {tag; a; b; level = max (level a) (level b)};;

      let ar2_integer_integer_integer_commutative tag a b =
        (* This comparison ensures that constants are on the right. *)
        let ge a b =
          let lvla = level a and lvlb = level b in
          if lvla = lvlb then get_id a >= get_id b
          else lvla >= lvlb
        in
        let (a,b) = 
          if ge a b then (a,b) else (b,a) in
        ar2_integer_integer_integer tag a b
      ;;

      
      let iadd = ar2_integer_integer_integer_commutative TC.Iadd
      let imul = ar2_integer_integer_integer_commutative TC.Imul
      let idiv = ar2_integer_integer_integer TC.Idiv
      let imod = ar2_integer_integer_integer TC.Imod
      let ishl = ar2_integer_integer_integer TC.Ishl
      let ishr = ar2_integer_integer_integer TC.Ishr
      let iand = ar2_integer_integer_integer_commutative TC.Iand
      let ior = ar2_integer_integer_integer_commutative  TC.Ior
      let ixor = ar2_integer_integer_integer_commutative TC.Ixor
      let isub = ar2_integer_integer_integer TC.Isub


      let iadd:  integer t -> integer t -> integer t = fun a b ->
        match (a,b) with
        | Integer{term=T0{tag=TC.Iconst k}}, x when Z.equal k Z.zero -> x
        | x,Integer{term=T0{tag=TC.Iconst k}} when Z.equal k Z.zero -> x          
        | _,_ -> iadd a b
      ;;

      let iadd: integer t -> integer t -> integer t = fun a b ->
        if a == b then begin
          imul (iconst @@ Z.of_int 2) a
        end
        else iadd a b
      
      let imul: integer t -> integer t -> integer t = fun a b ->
        match (a,b) with
        | Integer{term=T0{tag=TC.Iconst k}}, x when Z.equal k Z.zero -> a
        | x,Integer{term=T0{tag=TC.Iconst k}} when Z.equal k Z.zero -> b
        | Integer{term=T0{tag=TC.Iconst k}}, x when Z.equal k Z.one -> x
        | x,Integer{term=T0{tag=TC.Iconst k}} when Z.equal k Z.one -> x          
        | _,_ -> imul a b
      ;;
      
      

      let old_ishr = ishr
      let old_iand = iand;;
      let old_imod = imod;;      
      (* Note: this creates a new Constraint, the constant "k1 + k2",
         which may have not been evaluated yet by the domain.

         On the other hand, this is necessary to guarantee equality
         between the same values in a structure (when there are
         various extracts patterns that lead to the same value).

         The solution is to require that abstract domain can evaluate
         constants by themselves. *)
      let rec ishr: integer t -> integer t -> integer t = fun a b ->
        match (a,b) with
        | Integer{term=T2{ tag=TC.Ishr; a; b=Integer{term=T0{tag=TC.Iconst k1}}}},
          Integer{term=T0{tag=TC.Iconst k2}} ->
          (* Codex_log.feedback "in ishr %s" @@ Printexc.get_backtrace(); *)
          let a:integer t = a in
          ishr a (iconst @@ Z.add k1 k2)
        | Integer{term=T2{ tag=TC.Iand; a; b=Integer{term=T0{tag=TC.Iconst k1}}}},
          Integer{term=T0{tag=TC.Iconst k2}} ->
          let a:integer t = a in
          iand (ishr a b) (iconst @@ Z.shift_right k1 @@ Z.to_int k2)
          
        (* Note: modulo is not the best way to do this. *)
        (* MAYBE: just add an extract_bits operation to integers *)
        (* | Integer{term=T2{tag=TC.Imod; a; b=Integer{term=T0{tag=TC.Iconst k1}}}},
         *   Integer{term=T0{tag=TC.Iconst k2}} when Z.(equal zero @@ k1 land (one lsl Z.to_int k2)) ->
         *   let a:integer t = a in
         *   imod (ishr a b) (iconst @@ Z.(asr) k1 @@ Z.to_int k2) *)
        | _ -> old_ishr a b




      and imod: integer t -> integer t -> integer t = fun a b ->
        match (a,b) with
        (* (a mod b) mod c = a mod c (if b divides c). *)
        | Integer{term=T2{ tag=TC.Imod; a; b=Integer{term=T0{tag=TC.Iconst k1}}}},
          Integer{term=T0{tag=TC.Iconst k2}} when Z.equal Z.zero @@ Z.rem k1 k2 ->
          let a:integer t = a in
          imod a b
        | _ -> old_imod a b



      and iand: integer t -> integer t -> integer t = fun a b ->
        match (a,b) with
        | Integer{term=T2{ tag=TC.Iand; a; b=Integer{term=T0{tag=TC.Iconst k1}}}},
          Integer{term=T0{tag=TC.Iconst k2}} ->
          let k12 = Z.(land) k1 k2 in
          let a:integer t = a in
          iand a (iconst k12)
        | _ -> old_iand a b
      ;;
      
     let ar2_integer_integer_boolean tag a b =
       HashCons.boolean (T2 {tag;a;b;level = max (level a) (level b)}) Condition.var ;;

      let ar2_integer_integer_boolean_commutative tag a b =
        let (a,b) = if get_id a <= get_id b then (a,b) else (b,a) in
        ar2_integer_integer_boolean tag a b
      ;;
     
      let ieq = ar2_integer_integer_boolean_commutative TC.Ieq
      let ile = ar2_integer_integer_boolean TC.Ile

      (* Happens a lot due to validity constraints. *)
      let ile:  integer t -> integer t -> boolean t = fun a b ->
        match (a,b) with
        | Integer{term=T0{tag=TC.Iconst k1}}, Integer{term=T0{tag=TC.Iconst k2}} ->
          if Z.leq k1 k2 then Boolean.true_ else Boolean.false_
        | _,_ -> ile a b
      ;;

      (* Happens when a condition is stored in a temporary variable. *)
      let ieq_:  integer t -> integer t -> boolean t = fun left right ->
        match (left,right) with
        | (Integer{term=Tuple_get(i,Nondet{conda_bool;a;condb_bool;b})},
          Integer{term=T0{tag=TC.Iconst k}}
        | Integer{term=T0{tag=TC.Iconst k}},
          Integer{term=Tuple_get(i,Nondet{conda_bool;a;condb_bool;b})}) ->
          (match Immutable_array.get a i, Immutable_array.get b i with
          | Any(Integer{term=T0{tag=TC.Iconst ka}}), Any(Integer{term=T0{tag=TC.Iconst kb}})
            when Z.equal k ka && (not @@ Z.equal k kb) -> conda_bool
          | Any(Integer{term=T0{tag=TC.Iconst ka}}), Any(Integer{term=T0{tag=TC.Iconst kb}})
            when Z.equal k kb && (not @@ Z.equal k ka) -> condb_bool
          | _,_ -> ieq left right
          )
        | _,_ -> ieq left right
      ;;

      


      
      
      type boolean = TC.boolean t
      type integer = TC.integer t          
      let assume _ = assert false (* Not used. *)
      let iunknown () = assert false
      
    end

    module Binary = struct
      type boolean = TC.boolean t
      type binary = TC.binary t
      let unknown ~size ~level = Fresh.binary ~size (Unknown level)
      let empty = Fresh.binary Empty
      let buninit = empty

      let ar1_boolean_binary ~size tag a =
        HashCons.binary ~size @@ T1 {tag; a; level = (level a)};;
                  
      let ar1_binary_binary ~size tag a =
        HashCons.binary ~size @@ T1 {tag; a; level = (level a)};;

      let ar2_binary_binary_binary ~size tag a b =
        HashCons.binary ~size @@ T2 {tag; a; b; level = max (level a) (level b)};;

      let ar2_binary_binary_binary_commutative tag a b =
        let (a,b) = if get_id a <= get_id b then (a,b) else (b,a) in
        ar2_binary_binary_binary tag a b
      ;;

      let ar2_binary_binary_boolean tag a b =
        HashCons.boolean (T2 {tag;a;b;level = max (level a) (level b)}) Condition.var ;;

      let ar2_binary_binary_boolean_commutative tag a b =
        let (a,b) = if get_id a <= get_id b then (a,b) else (b,a) in
        ar2_binary_binary_boolean tag a b
      ;;

      module TCBB = TC.Build.Binary
      
      (* TODO: size is both in the constructor and in the constraint;
         it is probably useless to store it twice. *)
      let beq   ~size = ar2_binary_binary_boolean_commutative (TCBB.beq ~size)
      let biule ~size = ar2_binary_binary_boolean (TCBB.biule ~size)
      let bisle ~size = ar2_binary_binary_boolean (TCBB.bisle ~size)
      let biadd  ~size ~nsw ~nuw ~nusw =
        ar2_binary_binary_binary_commutative ~size (TCBB.biadd ~size ~nsw ~nuw ~nusw)
      let bisub  ~size ~nsw ~nuw ~nusw =
        ar2_binary_binary_binary ~size (TCBB.bisub ~size ~nsw ~nuw ~nusw)      
      let bimul  ~size ~nsw ~nuw =
        ar2_binary_binary_binary_commutative ~size (TCBB.bimul ~size ~nsw ~nuw)
      let bxor   ~size = ar2_binary_binary_binary_commutative ~size (TCBB.bxor ~size)

      let bshift ~size ~offset ~max _ = assert false
      let bindex ~size _ = assert false

      let old_bimul = bimul

      (* Optimized version; necessary for binary analysis of ARM. *)
      let bimul(* : size:int -> binary -> binary -> binary *) =
        fun ~size ~nsw ~nuw (Binary bx as x) (Binary by as y) ->        
          match bx.term,by.term with
          | (T0 {tag = TC.Biconst(_,k)}),_  when Z.equal k Z.zero -> x
          | _,(T0 {tag = TC.Biconst(_,k)})  when Z.equal k Z.zero -> y            
          | (T0 {tag = TC.Biconst(_,k)}),_  when Z.equal k Z.one -> y
          | _,(T0 {tag = TC.Biconst(_,k)})  when Z.equal k Z.one -> x
          | _ -> old_bimul ~size ~nsw ~nuw x y

      
      
      (* Optimized version; necessary for binary analysis of ARM. *)
      let bxor: size:int -> binary -> binary -> binary =
        fun ~size (Binary bx as x) (Binary by as y) ->
        (* Codex_log.feedback "bxor %a %a" pretty x pretty y; *)
        match by.term with
        | (T2 {tag = TC.Bxor _;a;b}) when a == x -> b
        (* | (T2 {tag = TC.Bxor _;a;b}) when b == x -> y           *)
        (* | (T0 {tag = TC.False}), _ -> a *)
        (* | _, (T0 {tag = TC.True}) -> a *)
        (* | _, (T0 {tag = TC.False}) -> b *)
        (* | _, _ when a == b -> a *)
        | _ -> (* Codex_log.feedback "no success";  *)bxor ~size x y
      ;;

      (* Note: this does not work, because conda_bool in nondet is a condition since the beginning
         of the program, it cannot be extracted to be evaluated. 

         I have added the "binofbool" term for a proper fix. *)
      let beq_:  size:int -> binary -> binary -> boolean = fun ~size left right ->
        match (left,right) with
        | (Binary{term=Tuple_get(i,Nondet{conda_bool;a;condb_bool;b})},
           Binary{term=T0{tag=TC.Biconst(_,k)}}
          | Binary{term=T0{tag=TC.Biconst(_,k)}},
            Binary{term=Tuple_get(i,Nondet{conda_bool;a;condb_bool;b})}) ->
          (match Immutable_array.get a i, Immutable_array.get b i with
           | Any(Binary{term=T0{tag=TC.Biconst(_,ka)}}), Any(Binary{term=T0{tag=TC.Biconst(_,kb)}})
             when Z.equal k ka && (not @@ Z.equal k kb) -> conda_bool
           | Any(Binary{term=T0{tag=TC.Biconst(_,ka)}}), Any(Binary{term=T0{tag=TC.Biconst(_,kb)}})
             when Z.equal k kb && (not @@ Z.equal k ka) -> condb_bool
           | _,_ -> beq ~size left right
          )
        | _,_ -> beq ~size left right
      ;;

      (* This optimization is very important for kcg, which creates a
         lot of boolean variables. XXX: en fait, non.  Le problème
         vient du fait qu'on passe le temps dans cudd à lui faire
         calculer des &&. Je pense qu'avec les assume, on n'avait pas
         le problème (on appendait des conditions à la fin comme à
         chaque fois), mais qu'il se produit là.

         Peut-être que je n'ai juste pas besoin de faire correspondre
         tous mes termes booléens à des trucs cudd, et de ne le faire
         que dans le domaine? Ca pourrait marcher, et ca serait aussi 
         utile pour constraint_dominator.

         => Rendre optionelle la presence de bdds dans mes contraintes booléennes.
         => Un domaine devrait être représenté par deux choses: une contrainte, et une bdd

Oui, je pense que ca marcherait?

        XXX: si l'autre est efficace, c'est parce que je passe toute la condition dans le "domaine"
        du terme.  Si d est la condition courante, on remplace "c ({0,1} -> d)" par
        nondet ... "0 -> !c && d; 1 -> c && d".


        Du coup si plus tard, je suis avec une condition courante e (qui inclus souvent d, donc e == d && f), et que je fais un assume de la condition calculée.

       dans le premier cas je calcule c && d && e
       dans le deuxième cas je calcule 1 && (c && d) && e

       on doit donc aller plus vite, parce que d && c est déjà calculé. 

       Pas tout a fait clair


         =>

*)
      let beq:  size:int -> binary -> binary -> boolean = fun ~size left right ->
        let doit k a =
          if Z.equal k Z.one then a
          else if Z.equal k Z.zero then
            (* XXX: Cree un nouveau terme: c'est pas bien *)
            Boolean.not a
          else assert false
        in
        match (left,right) with 
        | (Binary{term=T1{tag=TC.Bofbool(_);a}},
           Binary{term=T0{tag=TC.Biconst(_,k)}}) -> doit k a
        | (Binary{term=T0{tag=TC.Biconst(_,k)}},
           Binary{term=T1{tag=TC.Bofbool(_);a}}) -> doit k a
          
          (* | Binary{term=T0{tag=TC.Biconst(_,k)}},
           *   Binary{term=Tuple_get(i,Nondet{conda_bool;a;condb_bool;b})}) ->
           * (match Immutable_array.get a i, Immutable_array.get b i with
           *  | Any(Binary{term=T0{tag=TC.Biconst(_,ka)}}), Any(Binary{term=T0{tag=TC.Biconst(_,kb)}})
           *    when Z.equal k ka && (not @@ Z.equal k kb) -> conda_bool
           *  | Any(Binary{term=T0{tag=TC.Biconst(_,ka)}}), Any(Binary{term=T0{tag=TC.Biconst(_,kb)}})
           *    when Z.equal k kb && (not @@ Z.equal k ka) -> condb_bool
           *  | _,_ -> beq ~size left right
           * ) *)
        | _,_ -> beq ~size left right
      ;;

      let bextract ~size ~index ~oldsize =
        ar1_binary_binary ~size (TC.Bextract{size;index;oldsize});;

      let old_bextract ~size ~index ~oldsize = bextract;;
          
      let bextract ~size ~index ~oldsize ((Binary x) as bx)  =
        match x.term with
        | T1 {tag= TC.Bextract{size=size';index=index';oldsize=oldsize'}; a} ->
          assert(oldsize == size');
          bextract ~size ~index:(index' + index) ~oldsize:oldsize' a
        | _ -> bextract ~size ~index ~oldsize bx

      
      (* let beq_ ~size left right = *)
      (*   let res = beq ~size left right in *)
      (*   Codex_log.feedback "beq %a %a res %a" pretty left pretty right pretty res; *)
      (*   res *)
      (* ;; *)

      
      let band   ~size = ar2_binary_binary_binary_commutative ~size (TCBB.band ~size)
      let bor    ~size = ar2_binary_binary_binary_commutative ~size (TCBB.bor ~size)
      let bsext  ~size ~oldsize = ar1_binary_binary ~size (TCBB.bsext ~size ~oldsize)
      let buext  ~size ~oldsize = ar1_binary_binary ~size (TCBB.buext ~size ~oldsize)
      let bofbool  ~size = ar1_boolean_binary ~size (TCBB.bofbool ~size)
      let bchoose  ~size cond = ar1_binary_binary ~size (TCBB.bchoose ~size cond)
      let bshl   ~size ~nsw ~nuw = ar2_binary_binary_binary ~size (TCBB.bshl ~size ~nsw ~nuw)
      let blshr  ~size = ar2_binary_binary_binary ~size (TCBB.blshr ~size)
      let bashr  ~size = ar2_binary_binary_binary ~size (TCBB.bashr ~size)
      let bisdiv ~size = ar2_binary_binary_binary ~size (TCBB.bisdiv ~size)
      let biudiv ~size = ar2_binary_binary_binary ~size (TCBB.biudiv ~size)
      let bismod ~size = ar2_binary_binary_binary ~size (TCBB.bismod ~size)
      let biumod ~size = ar2_binary_binary_binary ~size (TCBB.biumod ~size)          
      let bconcat ~size1 ~size2 =
        ar2_binary_binary_binary ~size:(size1 + size2) (TCBB.bconcat ~size1 ~size2);;
      let valid ~size _ = assert false
      let valid_ptr_arith ~size _ = assert false
      let biconst ~size k = HashCons.binary ~size @@ T0 { tag=TC.Biconst(size,k)};;        
      let assume ~size _cond _store = assert false

      let bunion ~size cond = ar2_binary_binary_binary ~size (TCBB.bunion ~size cond)
    end

    module Mu_Formal = struct
      let intro: type a. level:int -> actual:a t -> actual_cond:TC.boolean t ->  a TC.typ -> a t = fun ~level ~actual ~actual_cond typ ->
        (* Codex_log.feedback "Calling intro"; *)
        match typ with
        | TC.Integer -> Fresh.integer (Mu_formal {level; actual = (actual,actual_cond)})
        | TC.Boolean -> Fresh.boolean (Mu_formal {level; actual = (actual,actual_cond)}) @@ Condition.var ()
        | TC.Binary size -> Fresh.binary ~size (Mu_formal {level; actual = (actual,actual_cond)})
        | TC.Memory -> assert false
      ;;

    end

    module Tuple = struct


      (* The model is just to get the type. *)
      let tuple_from_tuple_ model tuple_ =
        model |> Immutable_array.mapi (fun i a ->
            let Any a = a in
            let f:type a. a t -> any = function
                | (Binary {size}) -> Any(Fresh.binary ~size (Tuple_get(i,tuple_)))
                | (Integer _) -> Any(Fresh.integer (Tuple_get(i,tuple_)))
                | (Bool _) -> Any(Fresh.boolean (Tuple_get(i,tuple_)) @@ Condition.var ())
            in f a
              )
           ;;

       let level_tuple x = Immutable_array.fold_left (fun a (Any b) -> max a @@ level b) (-1) x;;

           
       (* Note: if two constraints are the same (especially when all
          true), simplify.  Else this creates huge BDDs. But this
          should have been already done by the caller, so we do
          nothing here any more. *)
       let nondet ~level ~conda_bool ~a:tup1 ~condb_bool ~b:tup2 =
         (* Codex_log.feedback  "nondet level %d %d %d@\n tup1 %a@\n tup2 %a" level (level_tuple tup1) (level_tuple tup2) pretty_tuple tup1 pretty_tuple tup2; *)
        assert(level >= (level_tuple tup1) && level >= (level_tuple tup2));
        let tuple_ = Nondet({id=Id.fresh();conda_bool;a=tup1;condb_bool;b=tup2;level}) in
        let orig = tuple_from_tuple_ tup1 tuple_ in
        orig 
      ;;
        
      let mu ~level ~init ~var ~body ~body_cond =
        (* Codex_log.feedback "%d %d %d %a %a" level (level_tuple init) (level_tuple body)
         *   Pretty.Pretty.pretty_tuple init           Pretty.Pretty.pretty_tuple body; *)

        let tuple_ = Mu({id=Id.fresh();init;var;body;level;body_cond}) in
        let orig = tuple_from_tuple_ init tuple_ in

        if(Immutable_array.length init != 0) then begin
          assert (level >= (level_tuple init));
          assert ((level_tuple body) <= level + 1)
        end;

        orig
      ;;
      
      (* Optimize the creation of terms in the case where nothing changes. Yields
         a small improvement in some benchmarks. *)
      let nondet ~level ~conda_bool ~a:tup1 ~condb_bool ~b:tup2 =
        if Immutable_array.length tup1 == 0 then tup1
        else nondet ~level ~conda_bool ~a:tup1 ~condb_bool ~b:tup2
      
      let mu ~level ~init ~var ~body ~body_cond =
        if Immutable_array.length init == 0 then init
        else mu ~level ~init ~var ~body ~body_cond;;

      (* Downcast with dynamic check. *)
      let get_boolean: int -> tuple -> boolean t = fun idx tup ->
        let Any x = Immutable_array.get tup idx in
        match x with
        | Bool _ -> x
        | _ -> assert false

      let get_integer: int -> tuple -> integer t = fun idx tup ->
        let Any x = Immutable_array.get tup idx in
        match x with
        | Integer _ -> x
        | _ -> assert false

      let get_binary: size:int -> int -> tuple -> binary t = fun ~size idx tup ->
        let Any x = Immutable_array.get tup idx in
        match x with
        | Binary _ -> x
        | _ -> assert false
      
    end
  end

  module Utils = struct
    let get_term = get_term
  end

end

