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

module TC = Operator.Function_symbol
module In_bits = Units.In_bits

let nb_binary_terms = ref 0
let nb_unions = ref 0

let sizes = Hashtbl.create 1000


module type SUPER_TERMS = sig
  type 'a superterms
  module Make(Param:sig
      type elt
      val get_superterms: elt -> elt superterms
      val set_superterms: elt -> elt superterms -> unit
    end):sig

    (** An empty set of superterms. *)
    val create: unit -> Param.elt superterms

    (** Add one parent to one child. *)
    val add_one_superterm: subterm:Param.elt -> superterm:Param.elt -> unit

    (** Iterate on all superterms of a child. *)
    val iter_on_superterms: Param.elt -> (Param.elt -> unit) -> unit
  end
end


module SUPER_TERMS_WeakArray : SUPER_TERMS = struct

  (** WeakSet takes too much memory, we use weak array instead. *)
  type 'a superterms = 'a Weak.t

  module Make(P : sig
      type elt
      val get_superterms: elt -> elt superterms
      val set_superterms: elt -> elt superterms -> unit
  end) = struct

    (* We assume that most constraints will have at least one parent.  *)
    let create() = Weak.create 1;;

    (* Linear search *)
    let add_one_superterm ~subterm ~superterm =
      try
        let parents = P.get_superterms subterm in
        let length = Weak.length parents in
        assert (length > 0);
        for i = 0 to length - 1 do
          if not (Weak.check parents i)
          then begin
            Weak.set parents i (Some superterm);
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
           (* Kernel.warning "subterm %a superterm %a" *)
           (*  length Pretty.Pretty.pretty subterm Pretty.Pretty.pretty superterm *)
          );

        let newparents = Weak.create (2 * length) in
        Weak.blit parents 0 newparents 0 length;
        P.set_superterms  subterm newparents;
        Weak.set newparents length (Some superterm)
      with Exit -> ()

    let iter_on_superterms subterm f =
      let parents = P.get_superterms subterm in
      for i = 0 to (Weak.length parents) - 1 do
        match Weak.get parents i with
        | None -> ()
        | Some x -> f @@ x
      done
  end

    (* MAYBE: also add the index of the last superterm added, and start
       from here. This should allow amortized O(1) insertion if we
       explore from the start when the array is full, and O(1) if
       we do not try to reuse de-allocated array cells.

       We could also have a bitmap with the free elements in an int,
       and find the correct one with find_first_set. The bitmap would
       be updated on iter_on_superterms. More complex, and probably not
       as useful.

       To have small bitmaps unboxed and large boxed, we could just
       use Zarith; there are popcount and trailing_zeros functions.

       For now, just warn when we have a large number of superterms,
       which is the main problem anyway. The goal is to have a low
       memory consumption, so an extra index is useless. *)
end

(** Does not store superterms *)
module SUPER_TERMS_Unit : SUPER_TERMS = struct
  type 'a superterms = unit

  module Make(P:sig
      type elt
      [@@@ocaml.warning "-32"]
      val get_superterms: elt -> elt superterms
      val set_superterms: elt -> elt superterms -> unit
      [@@@ocaml.warning "+32"]
    end) = struct

    let create () = ()
    let add_one_superterm ~subterm ~superterm = ()
    let iter_on_superterms _ _ = ()
  end
end

let superterm_maker : (module SUPER_TERMS) =
  if Codex_config.terms_register_parents()
  then (module SUPER_TERMS_WeakArray)
  else (module SUPER_TERMS_Unit)

module SupertermMaker = (val superterm_maker)

module Make
    (Condition : Condition_map.CONDITION)
    (Relation : Union_Find.Parameters.GENERIC_GROUP)() =
struct
  type boolean = TC.boolean
  type integer = TC.integer
  type binary = TC.binary
  type enum = TC.enum

  module Log = Tracelog.Make(struct let category = "Terms.Builder" end);;

  (* Install the buttons to be understood by tracelog. *)
  Log.info (fun p -> p "Defines clickable buttons for Emacs tracelog-mode: %s%s%s"
                     "<(define:B:search-before:let B%1 =)>"
                     "<(define:i:search-before:let i%1 =)>"
                     "<(define:b:search-before:let b%1 =)>");;

  module Condition = Condition

  module Relation = Relation

  type ('arg,'res) function_symbol = ('arg,'res) TC.function_symbol

  module Id:sig
    type 'a t = Id: int -> 'a t  [@@unboxed];;
    val fresh: unit -> 'a t
    val _equal: 'a t -> 'b t -> bool
    val _hash: 'a t -> int
    val to_int: 'a t -> int
  end = struct

    type 'a t = Id: int -> 'a t [@@unboxed];;

    let count = ref 0;;
    let fresh() = incr count; Id !count;;

    let _equal (Id a) (Id b) = a == b;;

    let _hash (Id a) = a;;

    let to_int = _hash

  end

  type widening_id = int
  type level = int

  module T = struct
      (* MAYBE: Completely replace booleans by bdds. Alternatively: hide
         the presence of bdd, we only manipulate boolean constraints. *)
    type 'res complete_term =
      | Tuple_get: int * cfg_node -> 'res complete_term
      | Empty: 'res complete_term
      | Unknown: level -> 'res complete_term
      | Mu_formal:{ level:level; actual: ('res t * TC.boolean t) } -> 'res complete_term
      (* Note: maybe the level is a less fragile way to give name to inductive variables
        (where desambiguation can be done by the scope). For now we store both. *)

      (* Note: we have to choose between getting rid of the tuples and
        having [Inductive_var] here, or keeping them and having
        [Inductive_vars] in tuple_. *)
      | Inductive_var: {widening_id:widening_id; level:level; mutable definition: 'res t} -> 'res complete_term
      | T0: { tag: (TC.ar0,'res) function_symbol } -> 'res complete_term
      | T1: { tag: ('a TC.ar1,'res) function_symbol; a: 'a t; level:level} -> 'res complete_term
      | T2: { tag: (('a,'b) TC.ar2,'res) function_symbol; a: 'a t; b:'b t; level:level } -> 'res complete_term

    and 'a t =
      | Bool: {
          mutable superterms: superterms;
          id: boolean Id.t;
          mutable parent: boolean parent;
          term: boolean complete_term;
          bdd:Condition.t
        } -> boolean t
      | Integer: {
          mutable superterms: superterms;
          id: integer Id.t;
          mutable parent: integer parent;
          term: integer complete_term;
        } -> integer t
      | Binary: {
          mutable superterms: superterms;
          id: binary Id.t;
          mutable parent: binary parent;
          term: binary complete_term;
          size: In_bits.t
        } -> binary t
      | Enum: {
          mutable superterms: superterms;
          id: enum Id.t;
          mutable parent: enum parent;
          term: enum complete_term
        } -> enum t

      and 'a parent =
        | Node : 'b t * ('a, 'b) Relation.t -> 'a parent
        | Root

    and any = Any: 'res t -> any [@@unboxed]

    (** Note: Cannot use any (because the boxing clashes with using Weak),
        and we need to make the type disappear: so we use Obj.t instead. *)
    and superterms = Obj.t SupertermMaker.superterms

    (** The tuple that we manipulate *)
    and tuple = any Immutable_array.t

    (** Node in the SSA graph. *)
    and cfg_node =
      (* TODO:
         - View tuples as a CFG node, as in Lemerre's HDR.
         - Compute the domination tree, use this to replace the notion of level
         - When joining: compute the condition that separates the two from the dominator.
           Use this for the path-sensitive analysis. *)
      | Nondet of {id: tuple Id.t; level: level;
                  a:tuple; conda_bool: boolean t;
                  b:tuple; condb_bool: boolean t}
      | Mu of { id: tuple Id.t; level:level; init:tuple;var:tuple;body:tuple;body_cond:boolean t}
      (* Maybe: add a condition for the path predicate. *)
      | Inductive_vars of { id: tuple Id.t; widening_id:widening_id; level:int; mutable def:tuple }
  end
  include T
  type 'a constraints = 'a t

  let get_id: type a. a t -> a Id.t = function
    | Bool {id} -> id
    | Integer {id} -> id
    | Binary {id} -> id
    | Enum {id} -> id

  let get_term: type a. a t -> a complete_term = function
    | Bool {term} -> term
    | Integer {term} -> term
    | Binary {term} -> term
    | Enum {term} -> term

  let get_id_int: type a. a t -> int = function
    | Bool {id=Id.Id id} -> id
    | Integer {id=Id.Id id} -> id
    | Binary {id=Id.Id id} -> id
    | Enum {id=Id.Id id} -> id

  let polyeq (type a) (type b) (x: a t) (y: b t) : (a,b) PatriciaTree.cmp =
    match x, y with
    | Bool {id=Id.Id il; _}, Bool {id=Id.Id ir; _} ->
        if il == ir then Eq else Diff
    | Integer {id=Id.Id il; _}, Integer {id=Id.Id ir; _} ->
        if il == ir then Eq else Diff
    | Binary {id=Id.Id il; _}, Binary {id=Id.Id ir; _} ->
        if il == ir then Eq else Diff
    | _ -> Diff

  let compare: type a b. a t -> b t -> int = fun a b ->
    Int.compare (get_id_int a) (get_id_int b)

  let equal: type a b. a t -> b t -> bool = fun a b ->
    (get_id_int a) == (get_id_int b)

  let hash x = get_id_int x;;

  let level_term: type a. a complete_term -> level = function
    | Tuple_get(_,Nondet{level}) -> level
    | Tuple_get(_,Inductive_vars{level}) -> level
    | Tuple_get(_,Mu{level}) -> level
    | Empty -> -1
    | Unknown level -> level
    | Mu_formal {level} -> level
    | Inductive_var {level} -> level
    | T0 _ -> -1
    | T1 {level} -> level
    | T2 {level} -> level
  ;;

  let level x = level_term @@ get_term x;;

  let size_of: binary t -> In_bits.t = function Binary{size} -> size


  module Any = struct

    type t = any

    let hash (Any x) = get_id_int x

    let equal (Any a) (Any b) = (get_id_int a) == (get_id_int b);;

    let get_id_int (Any a) = get_id_int a;;

    let compare a b = Int.compare (get_id_int a) (get_id_int b)
  end

  module CFG_Node = struct
    type t = cfg_node
    let equal a b = match (a,b) with
      | Mu{id=Id.Id(id1)}, Mu{id=Id.Id(id2)} -> id1 == id2
      | Nondet{id=Id.Id(id1)}, Nondet{id=Id.Id(id2)} -> id1 == id2
      | Inductive_vars{id=Id.Id(id1)}, Inductive_vars{id=Id.Id(id2)} -> id1 == id2
      | _ -> false
    let hash = function
      | Mu{id=Id.Id(id)} -> id
      | Nondet{id=Id.Id(id)} -> id
      | Inductive_vars{id=Id.Id(id)} -> id

    let compare a b = Int.compare (hash a) (hash b)
  end

  module Pretty = struct
    type 'a pack =
      | Unit: TC.ar0 pack
      | Single: 'a t -> 'a TC.ar1 pack
      | Pair: 'a t * 'b t -> ('a,'b) TC.ar2 pack

    module rec PrettyArg:TC.PRETTY_ARG
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
    and PrettyApply:TC.PRETTY_RESULT
      with type 'a pack = 'a pack
       and type 'a t = 'a t
    = TC.Pretty(PrettyArg)
    and Pretty:sig
      val pretty: Format.formatter -> 'a t -> unit
      val pretty_definition: Format.formatter -> 'a t -> unit
      (* val pretty_tuple_deps: Format.formatter -> tuple -> unit *)
      val pretty_any: Format.formatter -> any -> unit
    end
    = struct

      let rec pretty_definition: type a. Format.formatter -> a t -> unit = fun fmt cons ->
        match cons with
        | Bool{id=Id.Id id;term} -> pretty_definition_complete_term "b" id fmt term;
        | Integer{id=Id.Id id;term} -> pretty_definition_complete_term "i" id fmt term;
        | Binary{id=Id.Id id;term} -> pretty_definition_complete_term "B" id fmt term;
        | Enum{id=Id.Id id;term} -> pretty_definition_complete_term "e" id fmt term;

      and pretty_definition_complete_term:
        type a. string -> int -> Format.formatter -> a complete_term -> unit =
        fun str id fmt cons ->

      let print_def tag x =
        Format.fprintf fmt "let %s%d = " str id;
        PrettyApply.pretty_destruct fmt tag x;
      in match cons with
        | T0{tag=(TC.Biconst _) as tag} -> Format.fprintf fmt "("; print_def tag Unit; Format.fprintf fmt ")"
        | T0{tag} -> print_def tag Unit
        | T1{tag;a} -> print_def tag (Single a)
        | T2{tag;a;b} -> print_def tag (Pair(a,b))
        | Tuple_get(i,tup) ->
          Format.fprintf fmt "let %s%d = tuple_get(%d,%a)@\n" str id i pretty_tuple tup
        | Empty -> Format.fprintf fmt "let %s%d = empty@\n" str id
        | Unknown _level -> Format.fprintf fmt "let %s%d = unknown(%d)@\n" str id _level
        | Mu_formal {level} -> Format.fprintf fmt "let %s%d = var(%d)@\n" str id level
        | Inductive_var {widening_id} -> Format.fprintf fmt "let %s%d = ivar(%d)@\n" str id widening_id
      and pretty: type a. Format.formatter -> a t -> unit = fun fmt cons ->
        match cons with
        | Bool{id=Id.Id id;term;parent} -> Format.fprintf fmt "<(b:%d%a)>" id pretty_parent parent
        | Integer{id=Id.Id id;term;parent} -> Format.fprintf fmt "<(i:%d%a)>" id pretty_parent parent
        | Binary{term=T0{tag=(TC.Biconst(size,k)) as tag}} -> PrettyApply.pretty_destruct fmt tag Unit
        | Binary{id=Id.Id id;term;parent} -> Format.fprintf fmt "<(B:%d%a)>" id pretty_parent parent
        | Enum{id=Id.Id id;term} -> Format.fprintf fmt "<(e:%d)>" id
      and pretty_parent: type a. Format.formatter -> a parent -> unit = fun fmt parent ->
        match parent with
        | Root -> ()
        | Node _ -> ()
      and pretty_tuple fmt tup =
        match tup with
        | Nondet({id=Id.Id id}) -> Format.fprintf fmt "T%d" id
        | Inductive_vars({id=Id.Id id}) -> Format.fprintf fmt "T%d" id
        | Mu {id=Id.Id id} -> Format.fprintf fmt "T%d" id

      and pretty_any fmt = function
        | Any x -> pretty fmt x

      and pretty_definition_any fmt = function
        | Any x -> pretty_definition fmt x

      and pretty_tuple_ fmt x =
        Immutable_array.pp_array ~pre:"<" ~suf:">" ~sep:"," pretty_any fmt x

      and pretty_definition_tuple_ fmt x =
        Immutable_array.iter (pretty_definition_any fmt) x
      ;;

      let _pretty_tuple_deps = pretty_definition_tuple_
      let _pretty_tuple = pretty_tuple_
    end
  end

  include Pretty.Pretty


  module SUPER_TERMS = struct

    module Param = struct

      type elt = Obj.t

      let get_superterms: type a. a constraints -> superterms = function
        | Bool{superterms; _} -> superterms
        | Integer{superterms; _} -> superterms
        | Binary{superterms; _} -> superterms
        | Enum{superterms; _} -> superterms

      let get_superterms x = get_superterms @@ Obj.obj x

      let set_superterms: type a. a constraints -> superterms -> unit = fun x superterms ->
        match x with
        | Bool r -> r.superterms <- superterms
        | Integer r  -> r.superterms <- superterms
        | Binary r -> r.superterms <- superterms
        | Enum r -> r.superterms <- superterms
      ;;

      let set_superterms x par = set_superterms (Obj.obj x) par

    end

    module Made = SupertermMaker.Make(Param)

    let create = Made.create

    let add_one_superterm ~child ~parent =
      (* Constants cannot be improved, and have lots of superterms.  Do
         not register superterms for constants.  *)
      let is_constant: type a. a constraints -> 'b = function
        | Bool{term=(T0 _ |Empty) } -> true
        | Integer{term=(T0 _ |Empty) } -> true
        | Binary{term=(T0 _ |Empty) } -> true
        | _ -> false
       in
       if is_constant child then ()
       else Made.add_one_superterm ~subterm:(Obj.repr child) ~superterm:(Obj.repr parent)
    ;;

    let add_to_childs: type a. a constraints -> unit = fun parent ->
      let add_parent_term: type res. res constraints -> res complete_term -> unit = fun parent term ->
        match term with
        | Empty | Unknown _ | Mu_formal _ | T0 _ | Inductive_var _ -> ()
        | Tuple_get(i,(Mu _  | Inductive_vars _)) -> ()       (* Do not redo fixpoints. *)
        | Tuple_get(i,Nondet{a;b})  ->
          let Any xa = Immutable_array.get a i in
          add_one_superterm ~child:xa ~parent;
          let Any xb = Immutable_array.get b i in
          add_one_superterm ~child:xb ~parent
        | T1 {a} -> add_one_superterm ~child:a ~parent
        | T2 {a;b} ->
          add_one_superterm ~child:a ~parent;
          add_one_superterm ~child:b ~parent
      in
      let term = get_term parent in
      add_parent_term parent term
    ;;

    (* With this we can remove the set of superterms altogether (thereby
       allowing for no forward constraint propagation), if it takes
       too long (e.g. for now, on huge arrays which should be dealt
       with.) *)
    (* let add_to_childs _ = ();; *)

    let iter_on_superterms child f =
      let child = Obj.repr child in
      let f x = f (Any (Obj.obj x)) in
      Made.iter_on_superterms child f


  end

  (* Note: my benchs showed that, at least for the way I generate
     constraints, hashconsing is very important. *)
  module HashCons = struct

    (* Note: we use different hashtables according to the type of the
       complete term. This should reduce the amount of collisions? *)


    module Param = struct

      let equal: type a. a complete_term -> a complete_term -> bool = fun a b ->
        (a == b) ||
        match (a,b) with
        | Tuple_get(i,x),Tuple_get(j,y) -> i == j && (assert false)
        | Empty,_ -> assert false
        | Unknown _,_ -> assert false
        | Mu_formal _ ,_ -> assert false
        | Inductive_var _ ,_ -> assert false
        | T0 {tag=tag1},T0{tag=tag2} -> TC.equal tag1 tag2
        | T1 {tag=tag1;a=a1},T1{tag=tag2;a=a2} ->
          TC.equal tag1 tag2 && (get_id_int a1) == (get_id_int a2)
        | T2 {tag=tag1;a=a1;b=b1},T2{tag=tag2;a=a2;b=b2} ->
          TC.equal tag1 tag2 && (get_id_int a1) == (get_id_int a2) && (get_id_int b1) == (get_id_int b2)
        | _ -> false

      let hash: type a. a complete_term -> int = function
        | Tuple_get(i,_) -> assert false (* TODO: create an id for tuples. *)
        | Empty -> assert false     (* Should not be hashed *)
        | Unknown _ -> assert false        (* Should not be hashed: created every time. *)
        | Mu_formal _ -> assert false            (* Should not be hashed: fresh every time. *)
        | Inductive_var _ -> assert false            (* Should not be hashed: fresh every time. *)
        | T0 {tag } -> Hashing.hash_sum ~nb:0 ~total:2 (TC.hash tag)
        | T1 {tag; a} -> Hashing.hash_sum ~nb:1 ~total:2 (Hashing.hash2 (TC.hash tag) (get_id_int a))
        | T2 {tag; a; b} -> Hashing.hash_sum ~nb:2 ~total:2 (Hashing.hash3 (TC.hash tag) (get_id_int a) (get_id_int b))


    end


    module type EphemeronHashCommon = sig
      type key
      type 'a t
      val create: int -> 'a t
      val find: 'a t -> key -> 'a
      val replace: 'a t -> key -> 'a -> unit
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
      (val if Codex_config.hash_cons_terms_with_weak_table()
        then (module Ephemeron.K1.Make(BoolParam):EphemeronHashCommon with type key = boolean complete_term)
        else (module Hashtbl.Make(BoolParam):EphemeronHashCommon with type key = boolean complete_term))
    ;;

    module IntParam = struct type t = integer complete_term include Param end
    module HashInt =
      (val if Codex_config.hash_cons_terms_with_weak_table()
        then (module Ephemeron.K1.Make(IntParam):EphemeronHashCommon with type key = integer complete_term)
        else (module Hashtbl.Make(IntParam):EphemeronHashCommon with type key = integer complete_term))

    module EnumParam = struct type t = enum complete_term include Param end
    module HashEnum =
      (val if Codex_config.hash_cons_terms_with_weak_table()
        then (module Ephemeron.K1.Make(EnumParam):EphemeronHashCommon with type key = enum complete_term)
        else (module Hashtbl.Make(EnumParam):EphemeronHashCommon with type key = enum complete_term))

    module BinParam = struct type t = binary complete_term include Param end
    module HashBin =
      (val if Codex_config.hash_cons_terms_with_weak_table()
        then (module Ephemeron.K1.Make(BinParam):EphemeronHashCommon with type key = binary complete_term)
        else (module Hashtbl.Make(BinParam):EphemeronHashCommon with type key = binary complete_term))
    ;;

    let bool_hash = HashBool.create 117;;
    let int_hash = HashInt.create 117;;
    let enum_hash = HashEnum.create 117;;
    let bin_hash = HashBin.create 117;;

    module BDDHash = Ephemeron.K1.Make(Condition);;
    let bddhash = BDDHash.create 117;;

    module X = Hashtbl.Make(Condition);;

    let boolean term bdd =
      try HashBool.find bool_hash term
      with Not_found ->
        let bdd = bdd() in
        try let x =  BDDHash.find bddhash bdd in
          let newer = (Bool{
                        superterms = SUPER_TERMS.create();
                        parent = Root;
                        id = Id.fresh();
                        term; bdd }) in
          Log.info (fun p -> p "%a" pretty_definition newer);
          x
        with Not_found ->
          let res = Bool {
                      superterms = SUPER_TERMS.create();
                      id = Id.fresh();
                      parent = Root;
                      term; bdd } in
          Log.info (fun p -> p "%a" pretty_definition res);
          (* Codex_log.feedback "Creating boolean %a" pretty res; *)
          HashBool.replace bool_hash term res;
          BDDHash.replace bddhash bdd res;
      (*   Kernel.feedback "bdd: %a term %a" Condition.pretty bdd *)
      (* pretty res; *)
        SUPER_TERMS.add_to_childs res;
        res

    let integer term =
      try HashInt.find int_hash term
      with Not_found ->
        let res = Integer {
                    superterms = SUPER_TERMS.create();
                    id = Id.fresh();
                    parent = Root;
                    term } in
        Log.info (fun p -> p "%a" pretty_definition res);
        HashInt.replace int_hash term res;
        SUPER_TERMS.add_to_childs res;
        res

    let enum term =
      try HashEnum.find enum_hash term
      with Not_found ->
        let res = Enum {
                    superterms = SUPER_TERMS.create();
                    id = Id.fresh();
                    parent = Root;
                    term } in
        Log.info (fun p -> p "%a" pretty_definition res);
        HashEnum.replace enum_hash term res;
        SUPER_TERMS.add_to_childs res;
        res



    let binary ~(size:In_bits.t) term =
      try HashBin.find bin_hash term
      with Not_found ->
        incr nb_binary_terms;
        let res = Binary {
                    superterms = SUPER_TERMS.create();
                    id = Id.fresh();
                    parent = Root;
                    term; size } in
        Log.info (fun p -> p "%a" pretty_definition res);
        HashBin.replace bin_hash term res;
        SUPER_TERMS.add_to_childs res;
        res
  end

  (** Similar to Hashcons, except that we do not need to hashcons because
      what we create is always fresh. *)
  module Fresh = struct

    (* Note: possibly no need to call add_to_childs here. *)
    let boolean term bdd =
      let res = Bool {
                  superterms = SUPER_TERMS.create();
                  id = Id.fresh();
                  parent = Root;
                  term; bdd } in
      HashCons.BDDHash.replace HashCons.bddhash bdd res;
      (match term with Tuple_get _ -> () | _ ->  Log.info (fun p -> p "%a" pretty_definition res));
      SUPER_TERMS.add_to_childs res;
      res

    let integer term =
      let res = Integer {
        superterms = SUPER_TERMS.create();
        id = Id.fresh();
        parent = Root;
        term } in
      (match term with Tuple_get _ -> () | _ ->  Log.info (fun p -> p "%a" pretty_definition res));
      SUPER_TERMS.add_to_childs res; res

    let binary ~size term =
      incr nb_binary_terms;
      let res = Binary {
        superterms = SUPER_TERMS.create();
        id = Id.fresh();
        parent = Root;
        term; size } in(match term with Tuple_get _  -> () | _ ->  Log.info (fun p -> p "%a" pretty_definition res));
      SUPER_TERMS.add_to_childs res; res

    let enum term =
      let res = Enum {
        superterms = SUPER_TERMS.create();
        id = Id.fresh();
        parent = Root;
        term } in
      (match term with Tuple_get _ -> () | _ ->  Log.info (fun p -> p "%a" pretty_definition res));
      SUPER_TERMS.add_to_childs res; res

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
          (T2 { tag = TC.BoolUnion; a; b; level = max 0 @@ max (level a) (level b)})
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
      let _ieq:  integer t -> integer t -> boolean t = fun left right ->
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
    end

    module Binary = struct
      type boolean = TC.boolean t
      type binary = TC.binary t
      let unknown ~size ~level = Fresh.binary ~size (Unknown level)
      let empty =
        let emptyHash = Hashtbl.create 10 in
        fun ~size ->
          match Hashtbl.find emptyHash size with
          | exception Not_found ->
            let res = Fresh.binary ~size Empty in
            Hashtbl.add emptyHash size res ;
            res

          | res -> res

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
      let biadd  ~size ~flags =
        ar2_binary_binary_binary_commutative ~size (TCBB.biadd ~size ~flags)
      let bisub  ~size ~flags =
        ar2_binary_binary_binary ~size (TCBB.bisub ~size ~flags)
      let bimul  ~size ~flags =
        ar2_binary_binary_binary_commutative ~size (TCBB.bimul ~size ~flags)
      let bxor   ~size = ar2_binary_binary_binary_commutative ~size (TCBB.bxor ~size)

      let bshift ~size ~offset ~max _ = assert false
      let bindex ~size _ = assert false

      let old_bimul = bimul

      (* Optimized version; necessary for binary analysis of ARM. *)
      let bimul(* : size:int -> binary -> binary -> binary *) =
        fun ~size ~flags (Binary bx as x) (Binary by as y) ->
          match bx.term,by.term with
          | (T0 {tag = TC.Biconst(_,k)}),_  when Z.equal k Z.zero -> x
          | _,(T0 {tag = TC.Biconst(_,k)})  when Z.equal k Z.zero -> y
          | (T0 {tag = TC.Biconst(_,k)}),_  when Z.equal k Z.one -> y
          | _,(T0 {tag = TC.Biconst(_,k)})  when Z.equal k Z.one -> x
          | _ -> old_bimul ~size ~flags x y



      (* Optimized version; necessary for binary analysis of ARM. *)
      let bxor: size:In_bits.t -> binary -> binary -> binary =
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
      let _beq:  size:In_bits.t -> binary -> binary -> boolean = fun ~size left right ->
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
      let beq:  size:In_bits.t -> binary -> binary -> boolean = fun ~size left right ->
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

      let bextract ~size ~index ~oldsize ((Binary x) as bx)  =
        match x.term with
        | T1 {tag= TC.Bextract{size=size';index=index';oldsize=oldsize'}; a} ->
          assert(oldsize == size');
          bextract ~size ~index:In_bits.(index' + index) ~oldsize:oldsize' a
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
      let bshl   ~size ~flags = ar2_binary_binary_binary ~size (TCBB.bshl ~size ~flags)
      let blshr  ~size = ar2_binary_binary_binary ~size (TCBB.blshr ~size)
      let bashr  ~size = ar2_binary_binary_binary ~size (TCBB.bashr ~size)
      let bisdiv ~size = ar2_binary_binary_binary ~size (TCBB.bisdiv ~size)
      let biudiv ~size = ar2_binary_binary_binary ~size (TCBB.biudiv ~size)
      let bismod ~size = ar2_binary_binary_binary ~size (TCBB.bismod ~size)
      let biumod ~size = ar2_binary_binary_binary ~size (TCBB.biumod ~size)
      let bconcat ~size1 ~size2 =
        ar2_binary_binary_binary ~size:In_bits.(size1 + size2) (TCBB.bconcat ~size1 ~size2);;
      let valid ~size _ = assert false
      let valid_ptr_arith ~size _ = assert false
      let biconst ~size k = HashCons.binary ~size @@ T0 { tag=TC.Biconst(size,k)};;

      (* We do not want union to be considered a constant, so the min level is 0 for it.
         Because of this, we inline ar2_binary_binary_binary and ar2_binary_binary_binary_commutative.
      *)
      let bunion ~size cond a b =
        (* ar2_binary_binary_binary_commutative ~size (TCBB.bunion ~size cond) *)
        let tag = (TCBB.bunion ~size cond) in
        let (a,b) = if get_id a <= get_id b then (a,b) else (b,a) in
        HashCons.binary ~size @@ T2 {tag; a; b; level = max 0 @@ max (level a) (level b)};;


    end

    module Enum = struct
      type boolean = TC.boolean t
      type enum = TC.enum t

      let unknown ~level = Fresh.enum (Unknown level)

      let empty = Fresh.enum Empty

      let ar1_enum_boolean tag a =
        HashCons.boolean (T1 {tag; a; level = (level a)}) Condition.var

      let caseof ~case =
        ar1_enum_boolean (TC.CaseOf(case));;

      let enum_const ~case = HashCons.enum @@ T0 { tag=TC.EnumConst case};;

    end

    module Mu_Formal = struct
      let intro: type a. level:int -> actual:a t -> actual_cond:TC.boolean t ->  a TC.typ -> a t = fun ~level ~actual ~actual_cond typ ->
        (* Codex_log.feedback "Calling intro"; *)
        match typ with
        | TC.Integer -> Fresh.integer (Mu_formal {level; actual = (actual,actual_cond)})
        | TC.Boolean -> Fresh.boolean (Mu_formal {level; actual = (actual,actual_cond)}) @@ Condition.var ()
        | TC.Binary size -> Fresh.binary ~size (Mu_formal {level; actual = (actual,actual_cond)})
        | TC.Memory -> assert false
        | TC.Enum -> Fresh.enum (Mu_formal {level; actual = (actual,actual_cond)})
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
                | (Enum _) -> Any(Fresh.enum (Tuple_get(i,tuple_)))
            in f a
              )
           ;;

       let level_tuple x = Immutable_array.fold_left (fun a (Any b) -> max a @@ level b) (-1) x;;


       (* Print a nondet constraint by separating each component. *)
       let pretty_nondet_tuple fmt ((tuple:any Immutable_array.t),conda_bool,a,condb_bool,b) =
         let len = Immutable_array.length tuple in
         for i = 0 to len-1 do
           Format.fprintf fmt "let %a = phi_nondet(%a when %a,%a when %a)@\n"
             pretty_any (Immutable_array.get tuple i)
             pretty_any (Immutable_array.get a i)
             pretty conda_bool
             pretty_any (Immutable_array.get b i)
             pretty condb_bool
         done
       ;;


       (* Note: if two constraints are the same (especially when all
          true), simplify.  Else this creates huge BDDs. But this
          should have been already done by the caller, so we do
          nothing here any more. *)
       let nondet ~level ~conda_bool ~a:tup1 ~condb_bool ~b:tup2 =
         (* Codex_log.feedback  "nondet level %d %d %d@\n tup1 %a@\n tup2 %a" level (level_tuple tup1) (level_tuple tup2) pretty_tuple tup1 pretty_tuple tup2; *)
        assert(level >= (level_tuple tup1) && level >= (level_tuple tup2));
        let tuple_ = Nondet({id=Id.fresh();conda_bool;a=tup1;condb_bool;b=tup2;level}) in
        (* Log.info (fun p -> p "tutu %a" Pretty.Pretty.pretty_tuple_ tuple_); *)
        let orig = tuple_from_tuple_ tup1 tuple_ in
        Log.info (fun p -> p "%a" pretty_nondet_tuple (orig,conda_bool,tup1,condb_bool,tup2));
        orig
      ;;

      let inductive_vars_individual ~widening_id ~level ~def:left =
        (* We fill the definition arbitrarily with left so make the typer happy. *)
        Immutable_array.map (fun left ->
            let Any left = left in
            let f:type a. a t -> any = fun left -> match left  with
              | (Binary {size}) -> Any(Fresh.binary ~size (Inductive_var {widening_id;level;definition=left}))
              | (Integer _) -> Any(Fresh.integer (Inductive_var {widening_id;level;definition=left}))
              | (Bool _) -> Any(Fresh.boolean (Inductive_var {widening_id;level;definition=left}) @@ Condition.var ())
              | (Enum _) -> Any(Fresh.enum (Inductive_var {widening_id;level;definition=left}))
            in f left
          ) left

      let inductive_vars_grouped ~widening_id ~level ~def =
        let tuple_:cfg_node = Inductive_vars({id=Id.fresh();widening_id;level;def}) in
        let orig = tuple_from_tuple_ def tuple_ in
        orig

      let inductive_vars =
        if Codex_config.term_group_inductive_variable_by_tuple
        then inductive_vars_grouped
        else inductive_vars_individual





      (* Print a nondet constraint by separating each component. *)
      let pretty_mu_tuple fmt ((tuple:any Immutable_array.t),init,var,body,body_cond) =
        let len = Immutable_array.length tuple in
        for i = 0 to len-1 do
          Format.fprintf fmt "let %a = phi_mu(init:%a,var:%a,body:%a with %a)@\n"
            pretty_any (Immutable_array.get tuple i)
            pretty_any (Immutable_array.get init i)
            pretty_any (Immutable_array.get var i)
            pretty_any (Immutable_array.get body i)
            pretty body_cond
        done
      ;;


      let mu ~level ~init ~var ~body ~body_cond =
        (* Codex_log.feedback "%d %d %d %a %a" level (level_tuple init) (level_tuple body)
         *   Pretty.Pretty.pretty_tuple init           Pretty.Pretty.pretty_tuple body; *)

        let tuple_ = Mu({id=Id.fresh();init;var;body;level;body_cond}) in
        let orig = tuple_from_tuple_ init tuple_ in
        Log.info (fun p -> p "%a" pretty_mu_tuple (orig,init,var,body,body_cond));
        if(Immutable_array.length init != 0) then begin
          assert (level >= (level_tuple init));
          assert ((level_tuple body) <= level + 1)
        end;
        orig

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

      let get_binary: size:In_bits.t -> int -> tuple -> binary t = fun ~size idx tup ->
        let Any x = Immutable_array.get tup idx in
        match x with
        | Binary _ -> x
        | _ -> assert false

      let get_enum: int -> tuple -> enum t = fun idx tup ->
        let Any x = Immutable_array.get tup idx in
        match x with
        | Enum _ -> x
        | _ -> assert false

    end
  end

  module Utils = struct
    let get_term = get_term
  end

  let get_parent (type a) : a t -> a parent = function
    | Bool { parent; _ } -> parent
    | Integer { parent; _ } -> parent
    | Binary { parent; _ } -> parent
    | Enum { parent; _ } -> parent

  module UnionFind = Union_Find.Imperative.GenericRelational(struct
    include T
    let polyeq = polyeq

    let get_parent = get_parent

    let set_parent (type a) (child: a t) (parent : a parent) =
      SUPER_TERMS.add_one_superterm ~child ~parent;
      match child with
      | Bool x -> x.parent <- parent;
      | Integer x -> x.parent <- parent
      | Binary x -> x.parent <- parent
      | Enum x -> x.parent <- parent

    module Relation = Relation
  end)
end
