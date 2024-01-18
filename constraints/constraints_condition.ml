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

(* Possible implementations of conditions for the constraint domain. *)


module Bed = Bdd.Make(struct
    type t = int

    let compare a b = (a - b)
    let pretty = Format.pp_print_int
    let hash x = x
    let equal a b = a == b
    
 end)

module ConditionMy = struct
  include Bed.BDD;;

  let var_count = ref 0;;
  let fresh() = incr var_count; !var_count;;
  
  let all = one
  let disjoint a b = ((&&~) a b) == zero
  let empty = zero
  let is_included a b = (==>~) a b == one

  let inter = (&&~)
  let union = (||~)              

  let is_empty x = x == zero
  let is_zero x= x == zero
  let is_one x = x == one

  let complement x = (!~) x

  let var () = Bed.BDD.var @@ fresh()

end


(* https://c-cube.github.io/ocaml-minisat/0.1/Minisat.html *)
(* https://en.wikipedia.org/wiki/Tseytin_transformation *)
(* https://github.com/c-cube/ocaml-minisat *)
(* module ConditionMinisat = struct *)

(*   (\* Not very efficient. *)

(*      I should maybe try with SateLite or Minisat2, or techniques to minimize  *)
(*      clauses in http://minisat.se/downloads/synth_in_sat.pdf, *)
(*      or do some BDD sweeping... *)

(*      + The minisat bindings I use are not very good; for instance they *)
(*      did not know that some literals were used that had the value true *)
(*      and false. *)
(*   *\) *)

(*   module Base = struct *)

(*     let state = Minisat.create();; *)
(*     (\* Minisat.set_verbose state 33;; *\) *)

(*     let add_clause state c = *)
(*       (\* Kernel.feedback "adding clause %a" Minisat.pp_clause (Array.to_list c); *\) *)
(*       Minisat.add_clause_a state c *)
(*     ;; *)

(*     let solve from state ~assumptions = *)
(*       (\* (match assumptions with *\) *)
(*       (\* | [| a;b|] -> Kernel.feedback "assumptions %s %a %a" from Minisat.Lit.pp a Minisat.Lit.pp b *\) *)
(*       (\* | [| a|] -> Kernel.feedback "assumptions %s %a" from Minisat.Lit.pp a  *\) *)
(*       (\* | _ -> assert false); *\) *)
(*       Minisat.solve state ~assumptions *)
(*     ;; *)

    
(*     type t = Minisat.Lit.t *)

(*     (\* The first are actually reserved... *\) *)
(*     let count = ref 2;; *)
(*     let fresh() = incr count; Minisat.Raw.set_nvars state (10 + !count); Minisat.Lit.make @@ !count;; *)

(*     let true_ = *)
(*       let v = fresh() in *)
(*       add_clause state [| v |]; *)
(*       Minisat.simplify state; *)
(*       (\* Kernel.feedback "After true: value = %s" @@ (match Minisat.value state v with *\) *)
(*       (\* | Minisat.V_true -> "true" *\) *)
(*       (\* | Minisat.V_false -> "false" *\) *)
(*       (\* | Minisat.V_undef -> "undef"); *\) *)

(*       (\* assert (is_true v); *\) *)
(*       v *)
(*     ;; *)

(*     let false_ = *)
(*       let v = fresh() in *)
(*       add_clause state [| Minisat.Lit.neg v |]; *)
(*       (\* assert (is_false v); *\) *)
(*       v *)
(*     ;; *)

(*     let not_ x  = Minisat.Lit.neg x;; *)

(*     let (&&~) a b = *)
(*       let c = fresh() in *)
(*       (\* Kernel.feedback "%a = %a || %a" Minisat.Lit.pp c Minisat.Lit.pp a Minisat.Lit.pp b; *\) *)
(*       let nc = Minisat.Lit.neg c in *)
(*       add_clause state [| Minisat.Lit.neg a; Minisat.Lit.neg b; c |]; *)
(*       add_clause state [| a; nc |]; *)
(*       add_clause state [| b; nc |]; *)
(*       c *)
(*     ;; *)

(*     let (||~) a b = *)
(*       let c = fresh() in *)
(*       (\* Kernel.feedback "%a = %a || %a" Minisat.Lit.pp c Minisat.Lit.pp a Minisat.Lit.pp b; *\) *)
(*       add_clause state [| a; b; Minisat.Lit.neg c |]; *)
(*       add_clause state [| Minisat.Lit.neg a; c |]; *)
(*       add_clause state [| Minisat.Lit.neg b; c |]; *)
(*       c *)
(*     ;; *)

(*     (\* TODO: We can simplify true/false directly here. *\) *)

    
(*     module Hashcons = Hashtbl.Make(struct *)
(*         type t = Minisat.Lit.t * Minisat.Lit.t *)
(*         let hash (a,b) = (lxor) (Obj.magic a) (Obj.magic b) ;; *)
(*         let equal (a,b) (c,d) = (a == c && b == d) || (a == d) && (b == c);; *)
(*       end) *)

(*     let and_h = Hashtbl.create 170;; *)
(*     let or_h = Hashtbl.create 170;; *)
    
(*     let (&&~) a b = *)
(*       try let c = Hashtbl.find and_h (a,b) in (\* Kernel.feedback "hit for %a" Minisat.Lit.pp c; *\) c *)
(*       with Not_found -> *)
(*         let r = (&&~) a b in *)
(*         Hashtbl.replace and_h (a,b) r; *)
(*         r *)
(*     ;; *)
      
(*     let (||~) a b = *)
(*       try  let c = Hashtbl.find or_h (a,b) in (\* Kernel.feedback "hit for %a" Minisat.Lit.pp c; *\) c *)
(*       with Not_found -> *)
(*         let r = (||~) a b in *)
(*         Hashtbl.replace or_h (a,b) r; *)
(*         r *)
(*     ;; *)

(*     (\* Some simplifications. *\) *)
(*     let (&&~) a b = *)
(*       if a == true_ then b *)
(*       else if a == false_ then false_ *)
(*       else if b == true_ then a *)
(*       else if b == false_ then false_ *)
(*       else (&&~) a b *)
(*     ;; *)

(*     let (||~) a b = *)
(*       if a == true_ then true_ *)
(*       else if a == false_ then b *)
(*       else if b == true_ then true_ *)
(*       else if b == false_ then a *)
(*       else (||~) a b *)
(*     ;; *)

(*     let (&&~) a b = *)
(*       if a == b then a else (&&~) a b *)
    
(*     let (||~) a b = *)
(*       if a == b then a else (||~) a b *)

(*     let is_false x = (Minisat.value state x == Minisat.V_false);; *)
(*     let is_true x = (Minisat.value state x == Minisat.V_true);; *)

(*     let is_true x = *)
(*       let assumptions = [| Minisat.Lit.neg x |] in *)
(*       try solve "is_true" state ~assumptions; false *)
(*       with Minisat.Unsat -> true *)
(*     ;; *)

(*     let is_false x = *)
(*       let assumptions = [| x |] in *)
(*       try solve "is_false" state ~assumptions; false *)
(*       with Minisat.Unsat -> true *)
(*     ;; *)

(*     let is_false_h = Hashtbl.create 170;; *)

(*     let is_false x = *)
(*       try Hashtbl.find is_false_h x *)
(*       with Not_found -> *)
(*         let r = is_false x in *)
(*         Hashtbl.replace is_false_h x r; *)
(*         r *)
(*     ;; *)

(*     let is_true_h = Hashtbl.create 170;; *)

(*     let is_true x = *)
(*       try Hashtbl.find is_true_h x *)
(*       with Not_found -> *)
(*         let r = is_true x in *)
(*         Hashtbl.replace is_true_h x r; *)
(*         r *)
(*     ;; *)
    
    

(*     (\* TODO: cache the results of the && in disjoint? *\) *)
(*     (\* Or maybe, just try to see if solving a and b is exact. *\) *)
(*     let disjoint a b = is_false (a &&~ b) *)
(*     (\* let disjoint a b = *\) *)
(*     (\*   let assumptions = [|a;b|] in *\) *)
(*     (\*   try solve state ~assumptions; false *\) *)
(*     (\*   with Minisat.Unsat -> true *\) *)
(*     (\* ;; *\) *)


(*     let implies a b = *)
(*       let assumptions = [|a;Minisat.Lit.neg b |] in *)
(*       try solve "implies" state ~assumptions; false *)
(*       with Minisat.Unsat -> true *)
(*     ;; *)

(*     let pretty = Minisat.Lit.pp *)

(*     let equal a b = a == b *)

(*   end *)


(*   (\* Note: no need to have t = ...: I could compare to the values of true_ and false_. *\) *)
(*   module Expr = struct *)

(*     type t = True | False | Var of Base.t *)

(*     let pretty fmt x = match x with *)
(*       | True -> Format.fprintf fmt "true" *)
(*       | False -> Format.fprintf fmt "false" *)
(*       | Var x -> Base.pretty fmt x *)
    
(*     let false_ = False *)
(*     let true_ = True *)

(*     let is_true = function *)
(*       | True -> true *)
(*       | False -> false *)
(*       | Var x -> Base.is_true x *)

(*     let is_false = function *)
(*       | True -> false *)
(*       | False -> true *)
(*       | Var x -> Base.is_false x *)
    
(*     let not_ = function *)
(*       | True -> False *)
(*       | False -> True *)
(*       | Var x -> Var (Base.not_ x) *)

(*     let (||~) a b = match (a,b) with *)
(*       | True, _ | _, True -> True *)
(*       | False, x | x, False -> x *)
(*       | Var a, Var b -> Var (Base.(||~) a b) *)

(*     let (&&~) a b = match (a,b) with *)
(*       | False, _ | _, False -> False *)
(*       | True, x | x, True -> x *)
(*       | Var a, Var b -> Var (Base.(&&~) a b) *)

(*     let implies a b = match (a,b) with *)
(*       | False, _ -> true *)
(*       | _, True -> true *)
(*       | True, False -> false *)
(*       | Var a, False -> Base.is_false a *)
(*       | True, Var b -> Base.is_true b *)
(*       | Var a, Var b -> Base.implies a b *)
(*     ;; *)

(*     let disjoint a b = match (a,b) with *)
(*       | False, _ -> true *)
(*       | _, False -> true *)
(*       | True, True -> false *)
(*       | True, Var a | Var a, True -> Base.is_false a *)
(*       | Var a, Var b -> Base.disjoint a b *)
    
    
(*     let fresh() = Var(Base.fresh()) *)

(*     let equal = (=) *)
    
(*   end *)

(*   (\* include Base *\) *)
(*   include Expr *)
  
(*   let is_included = implies;; *)

(*   let complement = not_ *)
(*   let union = (||~) *)
(*   let inter = (&&~) *)
(*   let empty = false_ *)
(*   let zero = false_ *)
(*   let all = true_ *)
(*   let one = true_ *)
(*   let is_empty = is_false *)
(*   let is_zero = is_false *)
(*   let is_one = is_true *)
(*   let (!~) = not_ *)


(*   let equal a b = *)
(*     implies a b && implies b a *)
(*   ;; *)

(*   let var = fresh;; *)

(*   let hash = Hashtbl.hash *)

(* end *)







(* This uses the structure of the dominator tree as conditions. It
   observes the fact that we never perform intersection on arbitrary
   conditions, as conditions represent set of paths, we either assume
   a new condition or join existing ones.

   This works together with skiplist.ml (fast implementation of useful
   algorithms like online nearest_common_ancestor) and treemap.ml
   (mapping from tree relations to lattices).

   It is a good candidate for introducing scopes and getting rid of
   Cudd.  Still, when I tested these conditions with
   Domain_non_relational, it worked for most benchmarks, but the
   performance gains were not fantastic compared to cudd (sometimes
   less time, some times more; sometimes a lot less memory, but
   sometimes more). *)
module ConditionDom = struct

  (* least significant bit is 0 if positive, 1 otherwise. *)
  module Literal = struct
    type t = int
    let var_count = ref 0;;
    let fresh() =
      var_count := !var_count + 2;
      !var_count
    ;;
    let pretty fmt x =
      if x land 1 == 1
      then Format.fprintf fmt "%d" (x/2)
      else Format.fprintf fmt "!%d" (x/2)
    let equal = (==)
    let complement x =
      if x land 1 == 1
      then x land (lnot 1)
      else x lor 1

    let hash x = x
  end;;

  (* A path is a path in a domination tree. *)
  module Path = struct
    type t' =
      | Root
      | Append of t * Literal.t  (* Append a path with a literal. *)
      (* Join node of a and b; the immediate dominator of the join
         node is the common ancestor of a and b. *)
      | Join of { a: t; b:t; ancestor:t }


    and t = {id:int; content:t';depth:int} (* Hash-consed version. *)
    ;;

    let root = {id=0;content=Root;depth=0};;

    let rec pretty fmt = function
      | x when x == root -> Format.fprintf fmt "root"
      | {content=Append(p,l)} -> Format.fprintf fmt "%a::%a" pretty p Literal.pretty l
      | {content=Join{a;b;ancestor}} -> Format.fprintf fmt "join(%a,%a,%a)" pretty a pretty b pretty ancestor
      | _ -> assert false
    ;;

    let equal a b = a.id == b.id;;

    let count = ref 0;;

    let tail x = match x.content with
      | Root -> assert false
      | Append(x,_) -> x
      | Join{ancestor} -> ancestor

    let rec nearest_common_ancestor a b =
      if a.depth == b.depth
      then
        if a == b
        then a
        else nearest_common_ancestor (tail a) (tail b)
      else if a.depth < b.depth
      then nearest_common_ancestor a (tail b)
      else nearest_common_ancestor (tail a) b
    ;;

    let is_prefix a b = equal (nearest_common_ancestor a b) a (* Not the most efficient. *)

    (* We should just check that one is the ancestor of the other, and
       take the longuest one. *)
    let inter a b =
      let ab = nearest_common_ancestor a b in
      if ab == a then b
      else if ab == b then a
      else Codex_log.fatal "inter %a %a" pretty a pretty b
      (* match  a.content,b.content with
       * | Root, x -> b
       * | x, Root -> a
       * | _ -> assert false *)
    ;;

    (* XXX: Not completely true for join nodes, but should work
       anyway, because we always consider then differently.
 *)
    let disjoint a b =
      let ab = nearest_common_ancestor a b in
      if ab == a || ab == b
      then false
      else true                 (* On different branches. *)
    ;;

    
    [@@@warning "-8"]

    let sdbm x y = y + (x lsl 16) + (x lsl 6) - x;;
    module AppendHash = Weak.Make(struct
        type nonrec t = t

        let equal ({content=Append(pa,la)}) ({content=Append(pb,lb)}) =
          pa == pb && Literal.equal la lb;;
        let hash ({content=Append (pa,la)}) = sdbm pa.id @@ Literal.hash la
      end)
    ;;

    module JoinHash = Weak.Make(struct
        type nonrec t = t
        let equal ({content=Join{a=a1;b=b1}}) ({content=Join{a=a2;b=b2}}) =
          a1 == a2 && b1 == b2
        let hash ({content=Join{a;b}}) = sdbm a.id b.id
      end)
    ;;

    

    [@@@warning "+8"]

    let weakhash_default_size = 2000;;

    let tag_ref = ref 1 ;;
    let append_table = AppendHash.create weakhash_default_size;;
    let join_table = JoinHash.create weakhash_default_size;;    

    let append p x =
      let tentative =
        {id = !tag_ref; content = Append(p,x); depth = p.depth + 1} in
      let ret = AppendHash.merge append_table tentative in
      (if ret == tentative then incr tag_ref);
      ret
    ;;

    let join a b =
      let ancestor = nearest_common_ancestor a b in
      let tentative =
        {id = !tag_ref; content = Join{a;b;ancestor}; depth = ancestor.depth + 1} in
      let ret = JoinHash.merge join_table tentative in
      (if ret == tentative then incr tag_ref);
      ret
    ;;

    let union = join

    let of_literal x = append root x;;    


    
  end

  type t =
    | False
    | Literal of Literal.t   (* positive (if x&0) of negative (if x&1) literal *)
    | Path of Path.t
    | Complement of t                  (* lazy computation *)

  (* Ensures absence of clashes. *)
  let hash = function
    | False -> 0
    | Literal x -> (Path.of_literal x).id 
    | Path {id} -> id
    | Complement _ -> assert false
    (* | Unused -> -1 *)
  ;;

  let pretty fmt x =
    match x with
    | False -> Format.fprintf fmt "False"
    (* | Unused -> Format.fprintf fmt "Unused"                  *)
    | Literal x -> Format.fprintf fmt "Literal(%a)" Literal.pretty x
    | Path x -> Format.fprintf fmt "Path(%a)" Path.pretty x
    | Complement _ -> assert false

  let equal a b =
    (* Codex_log.feedback "equal %a %a" pretty a pretty b; *)
    match a,b with
    | False, False -> true
    | Literal x, Literal y -> Literal.equal x y
    | Path x, Path y -> Path.equal x y
    | Literal x, Path y -> Path.equal y (Path.of_literal x)
    | Path x, Literal y -> Path.equal x (Path.of_literal y)                             
    | _ -> false
  ;;

  let _equal a b =
    let res = equal a b in 
    Codex_log.feedback "equal %a %a res %b" pretty a pretty b res;
    res
  ;;

  
  let var() = Literal (Literal.fresh());;
  let empty = False;;
  let all = Path Path.root;;
  let one = all;;
  
  let complement x =
    (* Codex_log.feedback "Complement %a" pretty x; *)
    match x with
    | Path x when x == Path.root -> False
    | Literal x -> Literal (Literal.complement x)
    | False -> Path(Path.root)
    | Complement x -> x
    | x -> Complement x
  ;;
  
  let (!~) = complement;;

  (* Check that we are in the same path of the tree *)
  let is_included a b =
    match a,b with
    | _ when equal a b -> true
    | False, _ -> true
    | _, False -> false
    | _, Path{content=Root} -> true
    | Path{content=Root}, _ -> false
    | _ -> Codex_log.feedback "is_included %a %a" pretty a pretty b;
    assert false;;

  (* let leq a b = is_included b a *)

  (* Find the common ancestore, and create a new one (except maybe for simple cases). *)
  let union a b = match a,b with
    | False, x | x, False -> x
    | Literal a, Literal b when Literal.equal a (Literal.complement b) -> one
    | Literal a, Literal b -> Path(Path.union (Path.of_literal a) (Path.of_literal b))
    | Path a, Path b -> Path(Path.union a b)
    | Path a, Literal b -> Path(Path.union a (Path.of_literal b))
    | Literal a, Path b -> Path(Path.union (Path.of_literal a) b)                          
    | _ -> assert false

  
  let (||~) = union

  let nearest_common_ancestor a b = match a,b with
    | Path {content=Root}, _ -> a
    | _, Path {content=Root} -> b
    | (Literal a as res), Literal b when Literal.equal a b -> res
    | (Literal a as res), Literal b -> all

    | Path a, Literal b -> Path(Path.nearest_common_ancestor a (Path.of_literal b))
    | Literal a, Path b -> Path(Path.nearest_common_ancestor (Path.of_literal a) b)
    | Path a, Path b -> Path(Path.nearest_common_ancestor a b)
    | _ -> Codex_log.fatal "nearest_common_ancestor %a %a" pretty a pretty b

  let is_prefix a b = match a,b with
    | Path {content=Root}, _ -> true
    | _, Path {content=Root} -> false
    | (Literal a), Literal b when Literal.equal a b -> true
    | (Literal a), Literal b -> false
    | Path a, Literal b -> Path.is_prefix a (Path.of_literal b)
    | Literal a, Path b -> Path.is_prefix (Path.of_literal a) b
    | Path a, Path b -> Path.is_prefix a b
    | _ -> Codex_log.fatal "is_prefix %a %a" pretty a pretty b

  


  
  (* Check that we are on the same branch, and take the lower one. *)
  (* MAYBE: when on disjoint branches, we know that inter is empty? 
     Not always, but sometime we do (when diverges from the same path)
  *)
  let inter a b =
    (* Codex_log.feedback "inter %a %a" pretty a pretty b; *)
    match a,b with
    | False, _ | _, False -> False
    | x, y when x == y -> x
    | Literal x, Path {content=Append(p,l)} when Literal.equal x l -> b
    | Literal x, Path {content=Append(p,l)} when Literal.equal x (Literal.complement l) -> False
    | Path {content=Append(p,l)}, Literal x when Literal.equal x l -> a
    | Path {content=Append(p,l)}, Literal x when Literal.equal x (Literal.complement l) -> False
    (* Why does this happens? *)
    | Literal a, Literal b when Literal.equal a (Literal.complement b) -> False
    | Literal a, Literal b ->
      (* In this case, the path is on the right. *)
      let b = Path.append Path.root b in
      Path(Path.append b a)
    | Literal x, Path p -> Path (Path.append p x)
    | Path p, Literal x -> Path (Path.append p x)
    | Path a, Path b -> Path(Path.inter a b)
    | _ -> 
      Codex_log.fatal "inter %a %a" pretty a pretty b
  ;;

  let (&&~) = inter
  
  let is_empty x = x = False
  ;;
  let disjoint a b =
    (* Codex_log.feedback "disjoint %a %a" pretty a pretty b; *)
    let res = is_empty @@ inter a b  in
    (* Codex_log.feedback "disjoint %a %a res %b" pretty a pretty b res; *)
    res
  ;;

  let disjoint a b = match a,b with
    | False, _ | _, False -> true
    | (Literal a), Literal b when Literal.equal a b -> false
    | (Literal a), Literal b when Literal.equal a (Literal.complement b) -> true
    | Path a, Literal b -> Path.disjoint a (Path.of_literal b)
    | Literal a, Path b -> Path.disjoint (Path.of_literal a) b
    | Path a, Path b -> Path.disjoint a b
    | _ -> Codex_log.fatal "disjoint %a %a" pretty a pretty b
  ;;

  module Log = struct
    let disjoint a b =
      let res = disjoint a b in
      Codex_log.feedback "disjoint %a %a res %b" pretty a pretty b res;
      res;;

    let inter a b =
      let res = inter a b in
      Codex_log.feedback "inter %a %a res %a" pretty a pretty b pretty res;
      res;;

    let union a b =
      let res = union a b in
      Codex_log.feedback "union %a %a res %a" pretty a pretty b pretty res;
      res;;

    let is_prefix a b =
      let res = is_prefix a b in
      Codex_log.feedback "is_prefix %a %a res %b" pretty a pretty b res;
      res;;

    let nearest_common_ancestor a b =
      let res = nearest_common_ancestor a b in
      Codex_log.feedback "nearest_common_ancestor %a %a res %a" pretty a pretty b pretty res;
      res
    ;;
  end
  (* include Log *)
  
end

(* A dummy Condition, which creates a new int each time. *)
module ConditionInt = struct
  type t = int
  let count = ref 0 ;;
  let unique() = incr count; !count;;
  let pretty _ _ = ()
  let all = unique ()
  let equal a b =  a == b
  let empty = unique ()
  let is_empty x =x == empty
  let inter _ _ = unique()
  let (&&~) _ _ = unique()
  let union _ _ = unique()
  let (||~) _ _ = unique()
  let disjoint _ _ = assert false
  let is_included _ _ = assert false
  let complement _ = unique()
  let var () = unique()
  let hash x = x
end

module ConditionCudd = struct

  type t = Cudd.Man.d Cudd.Bdd.t
  let man = Cudd.Man.make_d ();;

  (* Reordering makes the analysis longer. *)
  (* Cudd.Man.(enable_autodyn man REORDER_LAZY_SIFT);; *)
  
  let all = Cudd.Bdd.dtrue man;;
  let empty = Cudd.Bdd.dfalse man;;
  let inter = Cudd.Bdd.dand
  let union = Cudd.Bdd.dor
  let disjoint = Cudd.Bdd.is_inter_empty
  let is_empty = Cudd.Bdd.is_false
  let complement = Cudd.Bdd.dnot

  let equal = Cudd.Bdd.is_equal
  let is_included = Cudd.Bdd.is_included_in

  let (&&~) = inter
  let (||~) = union
  let (!~) = complement
  let one = all
  let zero = empty

  let is_zero = Cudd.Bdd.is_false
  let is_one = Cudd.Bdd.is_true

  (* let var_to_constraint = Hashtbl.create 17;; *)

  (* MAYBE: link newvar id to the id in b%d, for printing.
     But only when debugging. *)


  
  (* let term_to_var = Term.Hashtbl.create 17;;
   * let var_to_term = Hashtbl.create 17;;
   * let var t =
   *   try Term.Hashtbl.find term_to_var t
   *   with Not_found ->
   *     let x = Cudd.Bdd.newvar man in
   *     Term.Hashtbl.replace term_to_var t x;
   *     Hashtbl.replace var_to_term (Cudd.Bdd.topvar x) t;
   *     x
   * ;; *)

  (* Note: new vars are added at the bottom. *)
  let var () = Cudd.Bdd.newvar man;;

  let pp_print_var fmt v = Format.fprintf fmt "var%d" v
  ;;
  
  let pretty fmt x = Cudd.Bdd.print pp_print_var(* Format.pp_print_int *) fmt x

  let hash (x:t) = Hashtbl.hash x
  
end

module MakeConditionCudd(T:sig
    type t
    val compare: t -> t -> int
    val pretty: Format.formatter -> t -> unit
  end):Condition_map.Condition = struct
  include ConditionCudd
end

(**************** MakeConditionMap implementations. ****************)

module MakeConditionMapMTBDD(Lattice:sig
    include Condition_map.L
    val equal: t -> t -> bool
    val hash: t -> int
    val compare: t -> t -> int
  end)(* :Condition_map.LConditionMap with module L = Lattice =  *) = struct


  module Maybe_Lattice = struct
    include Datatype_sig.Option(struct include Lattice let pretty _ = assert false end)
    (* let pretty fmt = function
     *   | Some x -> Lattice.pretty fmt x
     *   | None -> Format.fprintf fmt "<none>"
     * ;; *)
    let some x = Some x
    let none = None
    (* let bottom = Some (Lattice.bottom) *)
  end


  module MTBDD = struct
    module Terminal = Maybe_Lattice
    include Bed.MTBDD_Make(Maybe_Lattice);;      
    include With_Set(struct
        type t = Lattice.t
        (* let empty = Lattice.bottom *)
        let empty = assert false
        let singleton x = Maybe_Lattice.the x
        let union = (* Lattice.join *) assert false
        let add a b = (* Lattice.join *)(assert false) (Maybe_Lattice.the a) b
        (* let pretty = Lattice.pretty *)
        let pretty _ = assert false
      end)
  end

  type value = Lattice.t 
  module L = Lattice
  module Cond = ConditionMy
  type t = MTBDD.t(* value Bmtbdd *)


  let find = MTBDD.find
  let pretty = MTBDD.pretty


  let refine mtbdd ~cond ?(notcond=(Cond.complement cond)) value =
    let value = Maybe_Lattice.some value in
    MTBDD.update mtbdd cond (fun old_lattice ->
        match old_lattice,value with
        | None, value -> value     (* XXX: Ca c'est une difference aussi avec ce que je faisait avant: ici c'etait un assert false. *)
        | _, None -> assert false (* Cannot put <notevaluated> in the lattice. *)
        | Some a, Some b -> Some ((* Lattice.inter *)(assert false) a b))
  ;;

  let create x = assert false
  let create_partial = MTBDD.terminal None

end

module type SConditionMapMTBDD = sig

  type value 

  include Condition_map.LConditionMap with type Cond.t = ConditionMy.t
                                       and type L.t = value
                                       and type t = value option Bed.mtbdd
  module MTBDD: Bed.MTBDD
    with type Terminal.t = L.t option

end


module ConditionMapMTBDD = struct
  include Condition_map.MakePathInsensitive(ConditionMy)(struct
      type 'a t = 'a option Bed.mtbdd
    end)
  (* Replace previous value. *)
  (* let ar1 = Bed.ar1 *)
  (* let ar2 = Bed.ar2 *)

  (* MAYBE: Faire une implementation directe de ar1 ar2? map3 n'est
     pas optimal, parce que dans certain cas on pourrait garder le
     sous-arbre existant tel quel. Ou alors on devrait avoir une
     fonction "special", comme dans Cudd.  *)


  (* MAYBE: We also should be using cond. Or maybe not; it suffice to
     propagate None as being "not defined"

  *)

  let ar1
      (type a) (module Ma:SConditionMapMTBDD with type value = a)
      (type res) (module Mres:SConditionMapMTBDD with type value = res)
      cond f ma mold  =
    let f a old = match a,old with
      | None, x -> x
      | Some a, None -> Some(f a)
      | Some a, Some old -> Some ((assert false)(* Mres.L.inter *) (f a) old)
    in
    Bed.map2 (module Ma.MTBDD) (module Mres.MTBDD) (module Mres.MTBDD) f ma mold
  ;;

  let ar2
      (type a) (module Ma:SConditionMapMTBDD with type value = a)
      (type b) (module Mb:SConditionMapMTBDD with type value = b)        
      (type res) (module Mres:SConditionMapMTBDD with type value = res)
      cond f ma mb mold  =
    let f a b old = match a,b,old with
      | None, _, x | _, None, x -> x
      | Some a, Some b, None -> Some(f a b)
      | Some a, Some b, Some old -> Some ((* Mres.L.inter *)(assert false) (f a b) old)
    in
    Bed.map3 (module Ma.MTBDD) (module Mb.MTBDD) (module Mres.MTBDD) (module Mres.MTBDD)
      f ma mb mold
  ;;

end


module MakeConditionMapPartitionPI(Condition:Condition_map.Condition) = struct

  module ConditionMapPartition = Condition_map.ConditionMapPartition(Condition)
  module type SConditionMap = sig
    type value
    include Condition_map.LConditionMapFold with type Cond.t = Condition.t
                                           and type L.t = value
                                           and type t = value ConditionMapPartition.t
  end
  module MakeConditionMap(L:Condition_map.L) =
  struct
    include ConditionMapPartition.Make(L)
    type value = L.t
  end

  module ConditionMap = struct
    module PI =  Condition_map.MakePathInsensitive(Condition)(struct type 'a t = 'a ConditionMapPartition.t end)
    include PI
  end

  
end

(* module MakeConditionMapPartitionPS(Condition:Condition_map.Condition) = struct
 * 
 *   module ConditionMapPartition = Condition_map.ConditionMapPartition(Condition)
 *   module type SConditionMap = sig
 *     type value
 *     include Condition_map.LConditionMapFold with type Cond.t = Condition.t
 *                                            and type L.t = value
 *                                            and type t = value ConditionMapPartition.t
 *   end
 *   module MakeConditionMap(L:Condition_map.L) =
 *   struct
 *     include ConditionMapPartition.Make(L)
 *     type value = L.t
 *   end
 * 
 *   module ConditionMap = struct
 *     module PS =  Condition_map.MakePathSensitive(Condition)(struct type 'a t = 'a ConditionMapPartition.t end)
 *     include PS
 *   end
 *   
 * end *)

(* module MakeConditionMapTreeMapPI(Condition:Treemap.Key) = struct
 * 
 *   module M = Treemap.Make(Condition)
 *   (\* module ConditionMapTree = Condition_map.ConditionMapTree(Condition) *\)
 *   module type SConditionMap = sig
 *     type value
 *     include Condition_map.LConditionMap with type Cond.t = Condition.t
 *                                            and type L.t = value
 *                                            and type t = value M.t
 *   end
 *   module MakeConditionMap(L:Condition_map.L) =
 *   struct
 *     (\* include ConditionMapTree.Make(L) *\)
 *     type value = L.t
 *   end
 * 
 *   module ConditionMap = struct
 *     module PI =  Condition_map.MakePathInsensitive(Condition)(struct type 'a t = 'a M.t end)
 *     include PI
 *   end
 * 
 *   
 * end *)



module MakeConditionMapTreePI(Condition:Condition_map.Condition) = struct

  module ConditionMapTree = Condition_map.ConditionMapTree(Condition)
  module type SConditionMap = sig
    type value
    include Condition_map.LConditionMap with type Cond.t = Condition.t
                                           and type L.t = value
                                           and type t = value ConditionMapTree.t
  end
  module MakeConditionMap(L:Condition_map.L) =
  struct
    include ConditionMapTree.Make(L)
    type value = L.t
  end

  module ConditionMap = struct
    module PI =  Condition_map.MakePathInsensitive(Condition)(struct type 'a t = 'a ConditionMapTree.t end)
    include PI
  end

  
end

(* module MakeConditionMapTreePS(Condition:Condition_map.Condition) = struct
 * 
 *   module ConditionMapTree = Condition_map.ConditionMapTree(Condition)
 *   module type SConditionMap = sig
 *     type value
 *     include Condition_map.LConditionMap with type Cond.t = Condition.t
 *                                            and type L.t = value
 *                                            and type t = value ConditionMapTree.t
 *   end
 *   module MakeConditionMap(L:Condition_map.L) =
 *   struct
 *     include ConditionMapTree.Make(L)
 *     type value = L.t
 *   end
 * 
 *   module ConditionMap = struct
 *     module PS =  Condition_map.MakePathSensitive(Condition)(struct type 'a t = 'a ConditionMapTree.t end)
 *     include PS
 *   end
 *   
 * end *)

(* Special MTBDD for Cudd. *)

(* Actually, it could work on non-cudd conditions too. But this does
   not work well on KCG.*)

(* XXX: je pense qu'on peut faire mieux: toujours avoir Ite qui soit un ConditionCudd,  *)
(* pas seulement dans le cas le plus bas. Par exemple, on peut avoir deux mtbdd qui conduisent *)
(* a des trucs differents; mais tous les chemins menent vers ces deux la. *)

module CuddMTBDD = struct

  (* Like a MTBDD, except that when we have to choose between two
     options, we encode this using a Cudd BDD. This avoids the need to
     traverse a long tail of BDDs. *)

  (* Note: this is buggy and slow. *)
  
  type id = int
  type var = int
  type 'a t =
    | TerminalNone
    | Terminal of id * 'a
    | Ite of id * ConditionCudd.t * 'a t * 'a t
end

module type SConditionMapCuddMTBDD = sig
  type value
  include Condition_map.LConditionMapFold with type Cond.t = ConditionCudd.t
                                           and type L.t = value
                                           and type t = value CuddMTBDD.t
end

module MakeConditionMapCuddMTBDD(L:Condition_map.L) = struct

  type t = L.t CuddMTBDD.t

  let fresh_id =
    let count = ref 0 in
    fun () ->
      incr count;
      !count
  ;;


  open CuddMTBDD
  
  let hash = function
    | TerminalNone -> 0
    | Terminal(id,_) ->  id
    | Ite(id,_,_,_) -> id
  ;;

  let equal = (==)

  let rec pretty pp fmt = function
    | TerminalNone -> Format.fprintf fmt "<none>"
    | Terminal(id,a) -> Format.fprintf fmt "%d:%a" id pp a
    | Ite(id,c,a,b) -> Format.fprintf fmt "%d:ite(@[<hv>%a@,,%a@,,%a@])" id ConditionCudd.pretty c (pretty pp) a (pretty pp) b


  [@@@warning "-8"]
  
  (* We use 2 different hashes for terminal and non-terminal
     terms; this avoids spurious collisions in the table. MAYBE:
     Use GADT to avoid testing the cases of the Sum. *)
  module TerminalHash = Weak.Make(struct
      type t = L.t CuddMTBDD.t

      let equal (Terminal (_,a)) (Terminal (_,b)) = L.equal a b;;
      let hash (Terminal (_,a)) = L.hash a
    end)

  let sdbm x y = y + (x lsl 16) + (x lsl 6) - x;;
  

  module IfHash = Weak.Make(struct
      type t = L.t CuddMTBDD.t
      let equal (Ite(_,c1,then1,else1)) (Ite(_,c2,then2,else2)) =
        ConditionCudd.equal c1 c2 && then1 == then2 && else1 == else2

      let hash (Ite(_,c,then_,else_)) = sdbm (ConditionCudd.hash c) @@ sdbm (hash then_) (hash else_);;
    end)

  [@@@warning "+8"]
  
  let weakhash_default_size = 2000;;
  
  let tag_ref = ref 1 ;;
  let terminal_table = TerminalHash.create weakhash_default_size;;
  let if_table = IfHash.create weakhash_default_size;;

  let terminal x =
    let tentative = Terminal (!tag_ref, x) in
    let ret = TerminalHash.merge terminal_table tentative in
    (if ret == tentative then (tag_ref := !tag_ref + 1));
    ret
  ;;

  (* let base_variable = function *)
  (*   | TerminalNone | Terminal _ -> 1000000000 *)
  (*   | Ite(_,c,_,_) -> Cudd.Bdd.topvar c *)
  (* ;; *)

  let base_mk cond then_ else_ =
    (* assert (Cudd.Bdd.topvar cond < base_variable then_); *)
    (* assert (Cudd.Bdd.topvar cond < base_variable else_);     *)
    
            let tentative =
          (* Normalize that a < b; else take the complement. *)
          if hash then_ < hash else_
          then Ite(!tag_ref, cond, then_,else_)
          else Ite(!tag_ref, ConditionCudd.complement cond, else_,then_)
        in

        let ret = IfHash.merge if_table tentative in
        (if ret == tentative then (tag_ref := !tag_ref + 1));
        ret
  ;;
  
  let mk cond then_ else_ =
    (* Kernel.feedback "mk %a %a %a" ConditionCudd.pretty cond pretty then_ pretty else_; *)
    if ConditionCudd.is_one cond then then_
    else if ConditionCudd.is_zero cond then else_
    else if equal then_ else_ then then_
    else
      match then_, else_ with
      (* Cases where we have only two possible choices at the end -> extend a bdd. *)
      | _, Ite(_,c,a,b) when then_ == a -> base_mk (ConditionCudd.union cond c) a b
      | _, Ite(_,c,a,b) when then_ == b -> base_mk (ConditionCudd.inter c (ConditionCudd.complement cond)) a b
      | Ite(_,c,a,b), _ when else_ == a -> base_mk (ConditionCudd.inter cond (ConditionCudd.complement c)) b a
      | Ite(_,c,a,b), _ when else_ == b -> base_mk (ConditionCudd.inter cond c) a else_
      | Ite(_,c1,a1,b1), Ite(_,c2,a2,b2) when a1 == a2 && b1 == b2 ->
        base_mk (ConditionCudd.union
                     (ConditionCudd.inter cond c1)
                     (ConditionCudd.inter (ConditionCudd.complement cond) c2)) a1 b1
      | _ -> base_mk cond then_ else_
  ;;

  let create_partial = TerminalNone

  module Cond = ConditionCudd
  module L = L
  type value = L.t

  module T = struct
    type t = L.t CuddMTBDD.t
    let hash = hash
    let equal = (==)
  end

  module Find_cache = Ephemeron.K2.Make(T)(ConditionCudd);;
  let find_cache = Find_cache.create 117;;

  module Find_all_cache = Ephemeron.K1.Make(T);;
  let find_all_cache = Find_all_cache.create 117;;  

  let rec find t cond cur =
    (* Kernel.feedback "find %a  %a " pretty t ConditionCudd.pretty cond;     *)
    try Find_cache.find find_cache (t,cond)
    with Not_found ->
    let res = match Cudd.Bdd.inspect cond with
    | Cudd.Bdd.Bool false -> (* L.bottom *) assert false
    | Cudd.Bdd.Bool true ->
      let rec find_all x =
        try Find_all_cache.find find_all_cache x
        with Not_found ->
        let res = match x with
        | TerminalNone -> assert false
        | Terminal(_,v) -> v
        | Ite(_,_,t1,t2) -> (* L.join *)(assert false) (find_all t1) (find_all t2)
        in Find_all_cache.replace find_all_cache x res;
        res
      in find_all t
    | Cudd.Bdd.Ite(var1,then1,else1) -> begin
        match t with
        | TerminalNone -> assert false
        | Terminal(_,a) -> a
        | Ite(_,c,then_,else_) -> begin


         let curthen = ConditionCudd.inter cur c in
         let curelse = ConditionCudd.inter cur (ConditionCudd.complement c) in
            
          match (Cudd.Bdd.is_inter_empty cond curthen, Cudd.Bdd.is_inter_empty cond curelse) with
          | false, false -> (* L.join *)(assert false) (find then_ cond curthen) (find else_ cond curelse)
          | true, false -> find else_ cond curelse
          | false, true -> find then_ cond curthen
          | true, true -> assert false
          end
      end
    in Find_cache.replace find_cache (t,cond) res;
    res
  ;;

  let find t cond = find t cond ConditionCudd.one;;

  module Refine_cache = Find_cache;;

  module Loop_cache = Ephemeron.K2.Make(ConditionCudd)(ConditionCudd);;

  module T2_cache = Ephemeron.K2.Make(T)(T);;


  (* MAYBE: Memoize this. In the same cache than refine? *)
  let rec replace_all value = function
    | TerminalNone -> terminal value
    | Terminal(_,a) -> terminal ((assert false)(* L.inter *) a value)
    | Ite(_,c,then_,else_) ->
      mk c (replace_all value then_) (replace_all value else_)
  ;;

  module Value_cache = Ephemeron.K1.Make(L);;

  let refine_cache = Value_cache.create 30;;

  
  (* Note: a simple choice is to make mk do all the re-arrangement
     work. But of course it is better to optimize this, i.e. stop early when feasible. *)
  let refine ~inter (t:L.t CuddMTBDD.t) ~(cond:ConditionCudd.t) ?notcond (value:L.t) =
    (* Attempt to share the cache acrosse invokations. *)
    let refine_cache,loop_cache =
      try Value_cache.find refine_cache value
      with Not_found ->
        let refine_cache = Refine_cache.create 3 in
        let loop_cache = T2_cache.create 3 in
        (refine_cache,loop_cache)
    in
    let rec refine t ~cond =
      (* Kernel.feedback "refine %a cond %a v %a" pretty t ConditionCudd.pretty cond L.pretty value;     *)
      try Refine_cache.find refine_cache (t,cond)
      with Not_found ->
      let res =     
        match Cudd.Bdd.inspect cond with
        | Cudd.Bdd.Bool true ->     (* Replace all. *)
          replace_all value t
        | Cudd.Bdd.Bool false -> t (* replace none *)
        | Cudd.Bdd.Ite(var1,then1,else1) -> begin (* Replace some *)
            match t with
            | TerminalNone -> mk cond (terminal value) t
            | Terminal(_,a) -> mk cond (terminal @@ inter a value) t
            | Ite(_,c2,then2,else2) ->
              let c1 = cond in
              (* Importang shortcuts. TODO: Also check leq. *)
              if ConditionCudd.equal c1 c2 then
                mk c2 (replace_all value then2) else2 
              else if ConditionCudd.equal (ConditionCudd.complement c1) c2 then
                mk c2 then2 (replace_all value else2)
              else begin
                (* Note that loop_cache is shared between all instances of refine. *)
                let loop_cache =
                  try T2_cache.find loop_cache (then2,else2)
                  with Not_found ->
                    let res = Loop_cache.create 3 in
                    T2_cache.replace loop_cache (then2,else2) res;
                    res
                in
                (* XXX: Is it possible to do it better than manual iteration? *)
                (* let loop_cache = Loop_cache.create 12 in *)
                let rec loop c1 c2 =
                  (* Kernel.feedback "loop c1 %a c2 %a" ConditionCudd.pretty c1 ConditionCudd.pretty c2; *)
                  try Loop_cache.find loop_cache (c1,c2)
                  with Not_found ->
                    (* Kernel.feedback "not found"; *)
                    let res =
                      (* Important? shortcuts: catch the tail. *)
                      if ConditionCudd.equal c1 c2
                      then
                        mk c2 (replace_all value then2) else2
                      else if ConditionCudd.equal c1 (ConditionCudd.complement c2)
                      then
                        mk c2 then2 (replace_all value else2)
                      else match Cudd.Bdd.inspect c1 with
                        | Cudd.Bdd.Bool false -> mk c2 then2 else2
                        | Cudd.Bdd.Bool true -> mk c2 (replace_all value then2) (replace_all value else2)
                        | Cudd.Bdd.Ite(var1,then1,else1) -> begin
                            match Cudd.Bdd.inspect c2 with
                            | Cudd.Bdd.Bool true -> refine then2 ~cond:c1(* mk c1 (replace_all value then2) then2  *)
                            | Cudd.Bdd.Bool false -> refine else2 ~cond:c1(* mk c1 (replace_all value else2) else2 *)
                            | Cudd.Bdd.Ite(var2,condthen2,condelse2) ->
                              if var1 == var2 then
                                mk (Cudd.Bdd.ithvar ConditionCudd.man var1)
                                  (loop then1 condthen2)
                                  (loop else1 condelse2)
                              else if var1 < var2 then
                                mk (Cudd.Bdd.ithvar ConditionCudd.man var1)
                                  (loop then1 c2)
                                  (loop else1 c2)
                              else
                                mk (Cudd.Bdd.ithvar ConditionCudd.man var2)
                                  (loop c1 condthen2)
                                  (loop c1 condelse2)
                          end
                    in Loop_cache.replace loop_cache (c1,c2) res;
                    (* Kernel.feedback "loop c1 %a c2 %a res %a" ConditionCudd.pretty c1 ConditionCudd.pretty c2 pretty res;                     *)
                    res
                in loop c1 c2
                (* Kernel.feedback "c1 %a c2 %a" ConditionCudd.pretty c1 ConditionCudd.pretty c2; *)

              end
          end
      in Refine_cache.replace refine_cache (t,cond) res;
      res
    in refine t ~cond
  ;;

  let refine t ~cond ?notcond v =
    let res = refine t ~cond ?notcond v in
    (* Kernel.feedback "refine %a cond %a v %a res %a" pretty t ConditionCudd.pretty cond L.pretty v pretty res; *)
    res;;

  
  let fold_with_cond _ = assert false

end


(**************** Special Tree for Cudd. ****************)

(* Actually, it could work on non-cudd conditions too. *)

module CuddTree = struct

  type id = int
  
  type 'a t =
    | TerminalNone
    | Terminal of id * 'a
    | Ite of id * ConditionCudd.t * 'a t * 'a t

  
end

module type SConditionMapCuddTree = sig
  type value 
  include Condition_map.LConditionMapFold with type Cond.t = ConditionCudd.t
                                           and type L.t = value
                                           and type t = value CuddTree.t
end

module MakeConditionMapCuddTree(L:Condition_map.L) = struct

  type t = L.t CuddTree.t

  let fresh_id =
    let count = ref 0 in
    fun () ->
      incr count;
      !count
  ;;


  open CuddTree
  
  let hash = function
    | TerminalNone -> 0
    | Terminal(id,_) ->  id
    | Ite(id,_,_,_) -> id
  ;;

  let equal = (==)

  [@@@warning "-8"]
  
  (* We use 2 different hashes for terminal and non-terminal
     terms; this avoids spurious collisions in the table. MAYBE:
     Use GADT to avoid testing the cases of the Sum. *)
  module TerminalHash = Weak.Make(struct
      type t = L.t CuddTree.t

      let equal (Terminal (_,a)) (Terminal (_,b)) = L.equal a b;;
      let hash (Terminal (_,a)) = L.hash a
    end)

  let sdbm x y = y + (x lsl 16) + (x lsl 6) - x;;
  
  module IfHash = Weak.Make(struct
      type t = L.t CuddTree.t
      let equal (Ite(_,c1,then1,else1)) (Ite(_,c2,then2,else2)) = 
        ConditionCudd.equal c1 c2 && then1 == then2 && else1 == else2

      let hash (Ite(_,c,then_,else_)) = sdbm (ConditionCudd.hash c) @@ sdbm (hash then_) (hash else_);;
    end)

  [@@@warning "+8"]
  
  let weakhash_default_size = 2000;;
  
  let tag_ref = ref 1 ;;
  let terminal_table = TerminalHash.create weakhash_default_size;;
  let if_table = IfHash.create weakhash_default_size;;

  let terminal x =
    let tentative = Terminal (!tag_ref, x) in
    let ret = TerminalHash.merge terminal_table tentative in
    (if ret == tentative then (tag_ref := !tag_ref + 1));
    ret
  ;;

  let rec mk var then_ else_ =
    assert (var != ConditionCudd.one); (* MAYBE: Handle these cases? *)
    assert (var != ConditionCudd.zero);
    
    (* TODO: Normalize that a < b; else take the complement. *)
    if equal then_ else_ then then_
    else match then_, else_ with
      (* Simplify the tree. *)
      | _, Ite(_,c,a,b) when then_ == a -> mk (ConditionCudd.union var c) a b
      | _, Ite(_,c,a,b) when then_ == b -> mk (ConditionCudd.inter c (ConditionCudd.complement var)) a b
      | Ite(_,c,a,b), _ when else_ == a -> mk (ConditionCudd.inter var (ConditionCudd.complement c)) b a
      | Ite(_,c,a,b), _ when else_ == b -> mk (ConditionCudd.inter var c) a else_
      | Ite(_,c1,a1,b1), Ite(_,c2,a2,b2) when a1 == a2 && b1 == b2 ->
        mk (ConditionCudd.union
              (ConditionCudd.inter var c1)
              (ConditionCudd.inter (ConditionCudd.complement var) c2)) a1 b1
      | _ ->
        let tentative =
          if hash then_ < hash else_
          then Ite(!tag_ref, var, then_,else_)
          else Ite(!tag_ref, ConditionCudd.complement var, else_,then_)
        in
      let ret = IfHash.merge if_table tentative in
      (if ret == tentative then (tag_ref := !tag_ref + 1));
      ret
  ;;

  let rec pretty pp fmt = function
    | TerminalNone -> Format.fprintf fmt "<none>"
    | Terminal(id,a) -> Format.fprintf fmt "%d:%a" id pp a
    | Ite(id,c,a,b) -> Format.fprintf fmt "%d:ite(@[<v>%a@ ,%a@ ,%a@])" id ConditionCudd.pretty c (pretty pp) a (pretty pp) b;;

  let pretty fmt _ = ();;
  
  let create_partial = TerminalNone

  (* Cur is the condition for the partition t. *)
  (* MAYBE: memoize. *)
  let rec refine ~inter (t:L.t CuddTree.t) ~cond cur (value:L.t) =
    if Cudd.Bdd.is_inter_empty cond cur
    then t
    else if Cudd.Bdd.is_leq cur cond
    then
      let rec replace_all = function
        | TerminalNone -> terminal value
        | Terminal(_,a) -> terminal (inter a value)
        | Ite(_,c,then_,else_) ->
          mk c (replace_all then_) (replace_all else_)
      in replace_all t
    else match t with
      | TerminalNone -> mk cond (terminal value) TerminalNone
      | Terminal(_,a) -> mk cond (terminal (inter a value)) t
      | Ite(_,c,then_,else_) ->
        let curthen = ConditionCudd.inter cur c in
        let curelse = ConditionCudd.inter cur (ConditionCudd.complement c) in
        mk c
          (refine ~inter then_ ~cond curthen value)
          (refine ~inter else_ ~cond curelse value)
  ;;

  let refine t ~cond ?notcond v = refine t ~cond ConditionCudd.one v;;

  let refine t ~cond ?notcond v =
    let res = refine t ~cond ?notcond v in
    (* Kernel.feedback "refine %a cond %a v %a res %a" pretty t ConditionCudd.pretty cond L.pretty v pretty res; *)
    res;;
  
  let rec find t cur cond = match t with
    | TerminalNone -> assert false
    | Terminal(_,v) -> v
    | Ite(_,c,then_,else_) ->
      (* MAYBE: minimize cond here too. *)
      (* Kernel.feedback "inter result: %a %a %a %b %b" ConditionCudd.pretty c ConditionCudd.pretty (ConditionCudd.complement c) ConditionCudd.pretty cond (Cudd.Bdd.is_inter_empty cond c) (Cudd.Bdd.is_inter_empty (ConditionCudd.complement c) cond); *)
      let curthen =  ConditionCudd.inter cur c in
      let curelse =  ConditionCudd.inter cur (ConditionCudd.complement c) in      
      match (Cudd.Bdd.is_inter_empty cond curthen, Cudd.Bdd.is_inter_empty cond curelse)
      with
      | false, false -> (* L.join *)(assert false) (find then_ curthen cond) (find else_ curelse cond)
      | true, false -> find else_ curelse cond
      | false, true -> find then_ curthen cond
      | true, true -> assert false
  ;;

  let find t cond =
    (* Kernel.feedback "find %a  %a" pretty t ConditionCudd.pretty cond; *)
    if ConditionCudd.is_empty cond
    then (* L.bottom *)assert false
    else find t ConditionCudd.one cond 
  ;;

  module Cond = ConditionCudd
  module L = L
  type value = L.t

  let rec fold_with_cond t cond acc f cur =
    if Cudd.Bdd.is_inter_empty cur cond then acc
    else match t with
    | TerminalNone -> assert false
    | Terminal(_,v) -> f v (ConditionCudd.inter cur cond) acc
    | Ite(_,c,then_,else_) ->
      let curthen =  ConditionCudd.inter cur c in
      let curelse =  ConditionCudd.inter cur (ConditionCudd.complement c) in
      let acc = fold_with_cond then_ cond acc f curthen in
      let acc = fold_with_cond else_ cond acc f curelse in
      acc
  ;;
      

  let fold_with_cond t cond acc f = fold_with_cond t cond acc f ConditionCudd.one;;

  
end



(**************** Final configurations. ****************)

(* Things to export: Condition; ConditionMap; SConditionMap; and MakeConditionMap. 

   Probably they are not all necessary. *)


(* This one is relatively fast. *)
module CuddPIPartition = struct
  module Condition = ConditionCudd
  include MakeConditionMapPartitionPI(Condition)
end


(* Our implementation of ConditionDom, based on treemaps. Works for
   most tests, but there is still implementation work to do to improve
   this. *)
module DomPIPartition = struct
  module Condition = ConditionDom

  module M = Treemap.Make(Condition)
  
  module type SConditionMap = sig
    type value
  end

  module ConditionMap = struct
    type 'a t = 'a M.t

    exception Never_refined

    let find cond a =
      try M.find cond a
      with Not_found -> raise Never_refined

    let join _ _ = assert false
    
    let ar0
        (type res) (type mres) (module Lres:SConditionMap) ~interres
        cond f old =
      M.refine cond ~inter:interres ~join f old
    ;;


    let ar1
        (type a) (type ma) (module La:SConditionMap) ~joina ~bottoma
        (type res) (type mres) (module Lres:SConditionMap) ~interres
        cond f a old =
      let av = find cond a in
      M.refine cond ~inter:interres ~join (f av) old
    ;;

    

    let ar2
        (type a) (type ma) (module La:SConditionMap) ~joina ~bottoma
        (type b) (type mb) (module Lb:SConditionMap) ~joinb ~bottomb        
        (type res) (type mres) (module Lres:SConditionMap) ~interres
        cond f a b old =
      let av = find cond a in
      let bv = find cond b in      
      M.refine cond ~inter:interres ~join (f av bv) old
    ;;

    
    let nondet_disjoint
        (type res) (type mres) (module L:SConditionMap)
        ~conda ~notconda ~cma ~condb ~notcondb ~cmb ~join ~bottom ~inter ~old =
      let av = find conda cma in
      let bv = find condb cmb in
      let res = M.refine conda ~inter ~join av old in
      let res = M.refine condb ~inter ~join bv res in
      res
    ;;

    let nondet_non_disjoint
        (type res) (type mres) (module L:SConditionMap)
        ~conda ~cma ~condb ~cmb ~condaorb ~notcondaorb ~join ~bottom ~inter ~old =        
      let av = find conda cma in
      let bv = find condb cmb in
      let abv = join av bv in
      let res = M.refine condaorb ~inter ~join abv old in
      res
    ;;

    let changed inter cond r = function
      | None -> Condition.empty, r
      | Some x -> cond, M.refine cond ~inter ~join x r
    ;;

    let ar1_bwd
        (type a) (type ma) (module La:SConditionMap) ~joina ~bottoma ~intera
        (type res) (type mres) (module Lres:SConditionMap) ~joinres ~bottomres
        cond f a res =
      let va = find cond a in
      let v = find cond res in 
      let newva = f va v in
      changed intera cond a newva
    ;;    


    let ar2_bwd
        (type a) (type ma) (module La:SConditionMap) ~joina ~bottoma ~intera
        (type b) (type mb) (module Lb:SConditionMap) ~joinb ~bottomb ~interb      
        (type res) (type mres) (module Lres:SConditionMap) ~joinres ~bottomres
        cond f a b res =
      let va = find cond a in
      let vb = find cond b in
      let v = find cond res in
      let newva, newvb = f va vb v in
      (changed intera cond a newva,
       changed interb cond b newvb)
    ;;    

    

    
  end

  
    (* module type of Treemap.Make(Condition) *)
  module MakeConditionMap(L:sig
      include Condition_map.L
      val pretty: Format.formatter -> t -> unit                
    end
)= struct
    type value = L.t
    type t = L.t ConditionMap.t
    let create_partial = M.empty
    let find ~join ~bottom x key = ConditionMap.find key x;;

    let refine ~inter map ~cond value = M.refine cond ~inter ~join:(fun _ _ -> assert false) value map

    let pretty = M.make_pretty L.pretty

    let find ~join ~bottom x key =
      let res = find ~join ~bottom x key in
      Codex_log.feedback "Finding %a %a res %a" Condition.pretty key pretty x L.pretty res;
      res
    ;;

    let refine ~inter map ~cond value =
      let res = refine ~inter map ~cond value in
      Codex_log.feedback "Refining %a %a %a res %a" Condition.pretty cond L.pretty value pretty map pretty res;
      res
    ;;
    
  end

end


(* module CuddPSPartition = struct
 *   module Condition = ConditionCudd
 *   include MakeConditionMapPartitionPS(Condition)
 * end *)

(* These are  awfully slow *)
(* module CuddPITree = struct *)
(*   module Condition = ConditionCudd *)
(*   include MakeConditionMapTreePI(Condition) *)
(* end *)

(* module CuddPSTree = struct *)
(*   module Condition = ConditionCudd *)
(*   include MakeConditionMapTreePS(Condition) *)
(* end *)


(* This is the fastest on kcg. *)
module CuddPITree = struct
  module Condition = ConditionCudd
  module ConditionMap = struct
      include Condition_map.MakePathInsensitive(ConditionCudd)(struct
          type 'a t = 'a CuddTree.t
        end)
  end
  module MakeConditionMap = MakeConditionMapCuddTree
  module type SConditionMap = SConditionMapCuddTree
end


(* module CuddPSTree = struct
 *   module Condition = ConditionCudd
 *   module ConditionMap = struct
 *       include Condition_map.MakePathSensitive(ConditionCudd)(struct
 *           type 'a t = 'a CuddTree.t
 *         end)
 *   end
 *   module MakeConditionMap = MakeConditionMapCuddTree
 *   module type SConditionMap = SConditionMapCuddTree
 * end *)


module CuddPIMTBDD = struct
  module Condition = ConditionCudd
  module ConditionMap = struct
      include Condition_map.MakePathInsensitive(ConditionCudd)(struct
          type 'a t = 'a CuddMTBDD.t
        end)
  end
  module MakeConditionMap = MakeConditionMapCuddMTBDD
  module type SConditionMap = SConditionMapCuddMTBDD
end


(* module CuddPSMTBDD = struct
 *   module Condition = ConditionCudd
 *   module ConditionMap = struct
 *       include Condition_map.MakePathSensitive(ConditionCudd)(struct
 *           type 'a t = 'a CuddMTBDD.t
 *         end)
 *   end
 *   module MakeConditionMap = MakeConditionMapCuddMTBDD
 *   module type SConditionMap = SConditionMapCuddMTBDD
 * end *)




module HomeMadeBDDPartitionPI = struct
  module Condition = ConditionMy
  include MakeConditionMapPartitionPI(Condition)  
end

module HomeMadeMTBDD = struct
  module Condition = ConditionMy
  module MakeConditionMap = MakeConditionMapMTBDD
  module type SConditionMap = SConditionMapMTBDD
  module ConditionMap = ConditionMapMTBDD

end

(* module MinisatPI = struct *)
(*   module Condition = ConditionMinisat *)
(*   include MakeConditionMapPartitionPI(Condition) *)
(* end *)
