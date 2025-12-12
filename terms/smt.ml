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

module Log = Tracelog.Make(struct let category = "Terms.SMT" end);;

(* This is sound only if we check that binaries cannot overflow. *)
let option_translate_binary_to_integer = Codex_config.translation_to_smt_use_integer();;

open Operator.Function_symbol
module In_bits = Units.In_bits

(* Generic: translation of operators to SMTLIB. It can create new
   definitions; then the result of the translation is the freshly
   created variable. *)
module Make
    (T: Sig.TERMS)
    (S: Smtbackend.Smtlib_sig.UNTYPED_S) = struct

  (* Group pairs of Terms. *)
  type 'a t =
    | Unit: ar0 t
    | Single: 'a T.t -> 'a ar1 t
    | Pair: 'a T.t * 'b T.t -> ('a,'b) ar2 t

  let varname = function
    | T.Any T.Bool{id} -> "b" ^  (string_of_int @@ T.Id.to_int id) ^ "__"
    | T.Any T.Integer{id} -> "i" ^  (string_of_int @@ T.Id.to_int id) ^ "__"
    | T.Any T.Binary{id} -> "B" ^  (string_of_int @@ T.Id.to_int id) ^ "__"
    | T.Any T.Enum{id} -> "e" ^  (string_of_int @@ T.Id.to_int id) ^ "__"
  ;;

  module Make(M:sig
      val translate: 'a T.t -> S.value

      (* Create definitions for constr. *)
      val define_var: constr:T.any -> S.sort -> S.value -> S.value
      val declare_var: constr:T.any -> S.sort -> S.value
    end) =
  struct

    let translate: type res arg. res T.t -> (arg,res) function_symbol -> arg t -> S.value =
      fun constr function_symbol arg ->
        let ar2 op a b = op (M.translate a) (M.translate b)
        in
        let ar1 op arg = op (M.translate arg) in
        let defvar = M.define_var ~constr:(T.Any constr) in
        let defbool = defvar S.bool in
        let defbin ~(size:In_bits.t) = defvar (S.bitvec (size:>int)) in
        let defint = defvar S.int in
        (* Translate the arguments, but replace with unknown. *)
        let ar2_unknown sort a b  =
          let _ = M.translate a in let _ = M.translate  b in
          M.declare_var ~constr:(T.Any constr) sort
        in
        let ar1_unknown sort a  =
          let _ = M.translate a in
          M.declare_var ~constr:(T.Any constr) sort
        in

        (* Normal translation. *)
        let [@warning "-21"] normal: (arg,res) function_symbol * arg t -> S.value = function
          | True,Unit -> defbool @@ S.true_
          | False,Unit -> defbool @@ S.false_
          | And,Pair(a,b) -> defbool @@ ar2 S.(&&) a b
          | Or,Pair(a,b) -> defbool @@ ar2 S.(||) a b
          | Not,Single a -> defbool @@ ar1 S.not a
          | BoolUnion, Pair(a,b) -> assert false
          | Biconst(size,k),Unit -> defbin ~size @@ S.bvlit ~size:(size:>int) k
          | Beq(size), Pair(a,b) -> defbool @@ ar2 S.(=) a b
          | Bisle(size), Pair(a,b) -> defbool @@ ar2 S.bvsle a b
          | Biule(size), Pair(a,b) -> defbool @@ ar2 S.bvule a b
          | Biadd{size}, Pair(a,b) -> defbin ~size @@ ar2 S.bvadd a b
          | Bisub{size}, Pair(a,b) -> defbin ~size @@ ar2 (fun a b -> S.bvadd a (S.bvneg b)) a b
          | Bimul{size}, Pair(a,b) -> defbin ~size @@ ar2 S.bvmul a b
          | Bismod(size), Pair(a,b) -> defbin ~size @@ ar2 S.bvsrem a b
          | Bisdiv(size), Pair(a,b) -> defbin ~size @@ ar2 S.bvsdiv a b
          | Biumod(size), Pair(a,b) -> defbin ~size @@ ar2 S.bvurem a b
          | Biudiv(size), Pair(a,b) -> defbin ~size @@ ar2 S.bvudiv a b
          | Band(size), Pair(a,b) -> defbin ~size @@ ar2 S.bvand a b
          | Bor(size), Pair(a,b) -> defbin ~size @@ ar2 S.bvor a b
          | Bxor(size), Pair(a,b) -> defbin ~size @@ ar2 S.bvxor a b
          | Bshl{size}, Pair(a,b) -> defbin ~size @@ ar2 S.bvshl a b
          | Blshr(size), Pair(a,b) -> defbin ~size @@ ar2 S.bvlshr a b
          | Bashr(size), Pair(a,b) -> defbin ~size @@ ar2 S.bvashr a b
          | Bconcat(size1,size2), Pair(a,b) -> defbin ~size:In_bits.(size1 + size2) @@ ar2 S.concat a b
          | Bextract{size;index;oldsize},Single(a) ->
            let first = (index:>int) in
            let last = (index:>int) + (size:>int) - 1 in
            defbin ~size @@ ar1 (S.extract ~first ~last) a
          | Bofbool(size),Single a -> assert false
          | Bchoose(_,size), Single a -> assert false; defint @@ M.translate a (* XXX: TODO. *)
          | Bunion(cond,size), Pair(a,b)-> assert false; defint @@ M.translate a (* XXX: TODO *)
          | Buext(size),Single a -> defbin ~size @@ ar1 (S.zero_extend (In_bits.(size - T.size_of a):>int)) a
          | Bsext(size),Single a -> defbin ~size @@ ar1 (S.sign_extend (In_bits.(size - T.size_of a):>int)) a

          | Iconst k,Unit -> defint @@ S.numeralz k
          | Idiv , Pair(a,b) -> defint @@ ar2 S.div a b
          | Imod , Pair(a,b) -> defint @@ ar2 S.modu a b
          | Iadd , Pair(a,b) -> defint @@ ar2 S.(+) a b
          | Isub , Pair(a,b) -> defint @@ ar2 S.(-) a b
          | Ieq , Pair(a,b) -> defbool @@ ar2 S.(=) a b
          | Ile , Pair(a,b) -> defbool @@ ar2 S.(<=) a b
          | Imul , Pair(a,b) -> defint @@ ar2 S.( * ) a b

          (* Note: the constraint should have been translated to * or /
             when feasible. *)
          | Ishl , Pair(a,b) -> ar2_unknown S.int a b
          | Ishr , Pair(a,b) -> ar2_unknown S.int a b
          | Iand , Pair(a,b) -> ar2_unknown S.int a b
          | Ior , Pair(a,b) ->  ar2_unknown S.int a b
          | Ixor , Pair(a,b) -> ar2_unknown S.int a b
          | Itimes k, Single a -> defint @@ ar1 (S.( * ) (S.numeralz k)) a

          (* TODO: Bitblasting translation to booleans might be more effective. *)
          | EnumConst(case), Unit -> defint @@ S.numeralz (Z.of_int case)
          | CaseOf(case), Single a -> defbool @@ ar1 (S.(=) @@ S.numeralz @@ Z.of_int case) a

          | _ -> .
        in

        (* Unsound translation; sound only if we check that no binary
           overflow is possible. Overloads the normal translation. *)
        let binary_to_integer:  (arg,res) function_symbol * arg t -> S.value = function
          | Biconst(size,k),Unit -> defint @@ S.numeralz k
          | Beq(size), Pair(a,b) -> defbool @@ ar2 S.(=) a b
          | Bisle(size), Pair(a,b) -> defbool @@ ar2 S.(<=) a b
          | Biule(size), Pair(a,b) -> defbool @@ ar2 S.(<=) a b
          | Biadd{size}, Pair(a,b) -> defint @@ ar2 S.(+) a b
          | Bisub{size}, Pair(a,b) -> defint @@ ar2 S.(-) a b
          | Bimul{size}, Pair(a,b) -> defint @@ ar2 S.( * ) a b
          | Bismod(size), Pair(a,b) -> defint @@ ar2 S.modu a b
          | Bisdiv(size), Pair(a,b) -> defint @@ ar2 S.div a b
          | Biumod(size), Pair(a,b) -> defint @@ ar2 S.modu a b
          | Biudiv(size), Pair(a,b) -> defint @@ ar2 S.div a b
          | Band(size), Pair(a,b) ->  ar2_unknown S.int a b
          | Bor(size), Pair(a,b) ->   ar2_unknown S.int a b
          | Bxor(size), Pair(a,b) ->  ar2_unknown S.int a b
          | Bshl{size}, Pair(a,b) ->  ar2_unknown S.int a b
          | Blshr(size), Pair(a,b) -> ar2_unknown S.int a b
          | Bashr(size), Pair(a,b) -> ar2_unknown S.int a b
          | Bconcat(size1,size2), Pair(a,b) -> ar2_unknown S.int a b

          | Bextract{size;index;oldsize},Single(a) when index == In_bits.zero ->
             defint @@ S.modu (M.translate a) (S.numeralz @@ Z.pred @@ Z.shift_left Z.one (size:>int) )
          | Bextract{size;index;oldsize},Single(a) ->
             let _first = index in
             let _last = (index:>int) + (size:>int) - 1 in
             ar1_unknown S.int a

          | Bofbool(size),Single a -> assert false
          | Buext(size),Single a -> defint @@ M.translate a
          | Bsext(size),Single a -> defint @@ M.translate a
          | x -> normal x
        in

        if option_translate_binary_to_integer
        then binary_to_integer (function_symbol,arg)
        else normal (function_symbol,arg)


  end
end

(* Translation to first-order, quantifier-free, SMT formula *)
module MakeFirstOrder
    (T: Sig.TERMS)
    (S: Smtbackend.Smtlib_sig.UNTYPED_S)

= struct

  module M = Make(T)(S)

  module rec MakeArg:sig
    val translate: 'a T.t -> S.value
    val define_var: constr:T.any -> S.sort -> S.value -> S.value
    val declare_var: constr:T.any -> S.sort -> S.value
  end = struct

    open T

    let ar0 ~constr tag = MakeApply.translate constr tag M.Unit
    let ar1 ~constr tag a = MakeApply.translate constr tag (M.Single a)
    let ar2 ~constr tag a b = MakeApply.translate constr tag (M.Pair(a,b))

    module AnyHash = Hashtbl.Make(T.Any);;
    let tr_memo = AnyHash.create 17;;

    let rec translate_: type a. a T.t -> S.value = fun constr ->
      match constr with
      | Bool{term=(Mu_formal _)} -> S.declare_var S.bool
      | Bool{term=Tuple_get _} -> assert false
      | Bool{term=Unknown _level} -> S.declare_var S.bool
      | Bool{term=Empty} -> assert false
      | Bool{term=T2{tag;a;b}} -> ar2 ~constr tag a b
      | Bool{term=T1{tag;a}} -> ar1 ~constr tag a
      | Bool{term=T0{tag}} -> ar0 ~constr tag
      | Integer{term=(Mu_formal _)} -> S.declare_var S.int
      | Integer{term=Tuple_get(i,Nondet{conda_bool;condb_bool;a;b})} ->
        let Any ai = Immutable_array.get a i in
        let Any bi = Immutable_array.get b i in
        let v = S.declare_var S.int in
        let trconda = (translate conda_bool) in
        let trcondb = (translate condb_bool) in
        S.assert_ @@
        S.(=>)
          (* Temporary builds a constrain: often this creates a simplified constrain. *)
          (* (S.(||) trconda trcondb) *)
          (translate @@ T.Build.Boolean.(||) conda_bool condb_bool)
          (S.(||)
             (S.(&&) trconda (S.(=) v (translate ai)))
             (S.(&&) trcondb (S.(=) v (translate bi))));
        v
      | Integer{term=Tuple_get(i,Mu _)} -> S.declare_var S.int
      | Integer{term=Unknown _level} -> S.declare_var S.int
      | Integer{term=Empty} -> assert false
      | Integer{term=T2{tag;a;b}} -> ar2 ~constr tag a b
      | Integer{term=T1{tag;a}} -> ar1 ~constr tag a
      | Integer{term=T0{tag}} -> ar0 ~constr tag
      | Binary{term = _ } -> assert false
      | Bool{term = _} -> assert false
      | Integer{term = _} -> assert false
      | Enum{term = _ } -> assert false

    and translate: type a. a T.t -> S.value = fun x ->
      let any = Any x in
      try AnyHash.find tr_memo any
      with Not_found ->
        let res = translate_ x in
        AnyHash.replace tr_memo any res;
        res
    ;;

    let define_var ~constr = S.define_var ~name:(M.varname constr)
    let declare_var ~constr = S.declare_var ~name:(M.varname constr)

  end

  and MakeApply:sig
    val translate: 'res T.t -> ('arg,'res) Operator.Function_symbol.function_symbol -> 'arg M.t -> S.value
  end = M.Make(MakeArg)

   let translate assertion =
     let assertion = MakeArg.translate assertion in
     S.assert_ assertion;
     match S.check_sat () with
     | S.Sat -> Smtbackend.Smtlib_sig.Sat ()
     | S.Unsat -> Smtbackend.Smtlib_sig.Unsat
     | S.Unknown -> Smtbackend.Smtlib_sig.Unknown
end

(* Translation to Horn clauses, using z3 extensions for doing so
   (declared variables are implicitly universally quantified). *)
module MakeHorn
    (T: Sig.TERMS)
    (S:Smtbackend.Smtlib_sig.UNTYPED_MUZ)

= struct

  module M = Make(T)(S)

  module CapturedSet = Set.Make(T.Any)

  module Slicing = Slicing.Make(T)

  (* The hashes etc. depend on the slicing, since we don't produce the
     same thing depending on the assertion we want to prove. *)
  module Make(Slicing:sig val slicing: T.cfg_node -> int list end) = struct

  module rec MakeArg:sig
    val translate: 'a T.t -> S.value
    val get_rels_and_captured_vars: T.level -> T.any list -> CapturedSet.t * S.value list
    val define_var: constr:T.any -> S.sort -> S.value -> S.value
    val declare_var: constr:T.any -> S.sort -> S.value
  end = struct

    open T

    (* We do not use Ephemeron here; especially, some Terms are
       created temporarily during the conversion to SMT, but we still
       need their translation. *)
    module AnyHash = Hashtbl.Make(T.Any);;
    module CFG_Node_Hash = Hashtbl.Make(T.CFG_Node);;

    (* Map from each constrain/tuple to the corresponding variable. *)
    let tr_memo = AnyHash.create 17;;
    let tr_tuple_memo = CFG_Node_Hash.create 17;;

    (* Map from each constrain/tuple to a relation that defines this constrain, if any. *)
    let tr_memo_rel = AnyHash.create 17;;
    let tr_tuple_rel = CFG_Node_Hash.create 17;;

    (* Map from each mu to the set of variables that it captures. *)
    let captured_hash = CFG_Node_Hash.create 17;;

    (* let string_of_id id = string_of_int @@ T.Id.to_int id *)

    let ar0 ~constr tag = MakeApply.translate constr tag M.Unit
    let ar1 ~constr tag a = MakeApply.translate constr tag (M.Single a)
    let ar2 ~constr tag a b = MakeApply.translate constr tag (M.Pair(a,b))

    let [@warning "-11"] rec translate_: type a. a T.t -> S.value = fun constr ->
      match constr with
      | Bool{term=(Mu_formal _) | Unknown _ | Inductive_var _} ->
        AnyHash.replace tr_memo_rel (Any constr) None;
        S.declare_muz_var ~name:(M.varname @@ Any constr) S.bool
      | Integer{term=(Mu_formal _) | Unknown _ | Inductive_var _} ->
        AnyHash.replace tr_memo_rel (Any constr) None;
        S.declare_muz_var ~name:(M.varname @@ Any constr) S.int
      | Binary{size;term=(Mu_formal _) | Unknown _ | Inductive_var _} ->
         let tr_typ =
           if option_translate_binary_to_integer then S.int
           else S.bitvec (size:>int)
         in
         AnyHash.replace tr_memo_rel (Any constr) None;
         S.declare_muz_var ~name:(M.varname @@ Any constr) tr_typ
      | Enum{term=(Mu_formal _) | Unknown _ | Inductive_var _} ->
        AnyHash.replace tr_memo_rel (Any constr) None;
        S.declare_muz_var ~name:(M.varname @@ Any constr) S.int

      (* Not handled by SMTlib; we over-approximate by choosing a fixed constant. *)
      | Bool{term=Empty} ->
        AnyHash.replace tr_memo_rel (Any constr) None;
        S.false_
      | Integer{term=Empty} ->
        AnyHash.replace tr_memo_rel (Any constr) None;
        S.numeral 0
      | Binary{size;term=Empty} ->
         AnyHash.replace tr_memo_rel (Any constr) None;
         if option_translate_binary_to_integer
         then S.numeral 0
         else S.bvlit ~size:(size:>int) Z.zero
      | Enum{term=Empty} ->
        AnyHash.replace tr_memo_rel (Any constr) None;
        S.numeral 0

      | Bool{term=T2{tag;a;b}} -> ar2 ~constr tag a b
      | Bool{term=T1{tag;a}} -> ar1 ~constr tag a
      | Bool{term=T0{tag}} -> ar0 ~constr tag
      | Integer{term=T2{tag;a;b}} -> ar2 ~constr tag a b
      | Integer{term=T1{tag;a}} -> ar1 ~constr tag a
      | Integer{term=T0{tag}} -> ar0 ~constr tag
      | Binary{term=T2{tag;a;b}} -> ar2 ~constr tag a b
      | Binary{term=T1{tag;a}} -> ar1 ~constr tag a
      | Binary{term=T0{tag}} -> ar0 ~constr tag
      | Enum{term=T2{tag;a;b}} -> ar2 ~constr tag a b
      | Enum{term=T1{tag;a}} -> ar1 ~constr tag a
      | Enum{term=T0{tag}} -> ar0 ~constr tag


      | Binary{term=Tuple_get(i,tup)} ->
        AnyHash.replace tr_memo_rel (Any constr) None;
        Immutable_array.get (translate_tuple tup) i
      | Integer{term=Tuple_get(i,tup)} ->
        AnyHash.replace tr_memo_rel (Any constr) None;
        Immutable_array.get (translate_tuple tup) i
      | Bool{term=Tuple_get(i,tup)} ->
        AnyHash.replace tr_memo_rel (Any constr) None;
        Immutable_array.get (translate_tuple tup) i
      | Enum{term=Tuple_get(i,tup)} ->
        AnyHash.replace tr_memo_rel (Any constr) None;
        Immutable_array.get (translate_tuple tup) i


      (* Alternate translation for nondet: define each element of a
         nondet individually. MAYBE: compare if it makes a difference.
         TODO: This version should eliminate the elements in the
         nondet that are not needed. *)
      | Integer{term=Tuple_get(i,Nondet{conda_bool;condb_bool;a;b})} ->
        let Any ai = Immutable_array.get a i in
        let Any bi = Immutable_array.get b i in
        let v = S.declare_muz_var ~name:(M.varname @@ Any constr) S.int in
        let trconda = (translate conda_bool) in
        let trcondb = (translate condb_bool) in
        let rel =
          S.(=>)
            (* May build a constrain temporarily, which can be garbage-collected. *)
            (translate @@ T.Build.Boolean.(||) conda_bool condb_bool)
            (S.(||)
               (S.(&&) trconda (S.(=) v (translate ai)))
               (S.(&&) trcondb (S.(=) v (translate bi)))) in
        AnyHash.replace tr_memo_rel (Any constr) (Some rel);
        v



    and translate: type a. a T.t -> S.value = fun x ->
      let any = Any x in
      try AnyHash.find tr_memo any
      with Not_found ->
        let res = translate_ x in
        AnyHash.replace tr_memo any res;
        res


    (* Translate to prefix and sort  *)
    and any_to_prefix_sort (Any x) = match x with
      | (Bool _) -> "b", S.bool
      | (Integer _) -> "i", S.int
      | (Binary {size}) -> "B", if option_translate_binary_to_integer
                                then S.int else S.bitvec (size:>int)

      | (Enum _) -> assert false

    and any_to_sort x = snd @@ any_to_prefix_sort x

    and dummy_var = S.declare_muz_var ~name:"unused" S.bool

    and translate_tuple_ tup =
      let used_indices = Slicing.slicing tup in

      (* When the indice is used, declare a variable, else fill with dummy. *)
      let declare_vars length name model =
        let rec loop indices m = match indices,m with
          | [], m when m == length -> [],[]
          | (i::rest, m) when i > m ->
            let x,y = loop indices (m+1) in
            x,dummy_var::y
          | [],m ->
            let x,y = loop indices (m+1) in
            x,dummy_var::y
          | i::rest, m when i == m ->
            let prefix,sort = any_to_prefix_sort @@ Immutable_array.get model i in
            let v = S.declare_muz_var ~name:(prefix ^ name) sort in
            let x,y = loop rest (m+1) in
            v::x,v::y
          | i::rest, m -> assert false (* Impossible *)
        in
        let used,all = loop used_indices 0 in
        assert(List.length all == Immutable_array.length model);
        assert(List.length used == List.length used_indices);
        used,all
      in

      match tup with
      | T.Inductive_vars _ -> assert false
      | T.Nondet {a;conda_bool;b;condb_bool} as tup ->
        let trconda = (translate conda_bool) in
        let trcondb = (translate condb_bool) in
        let both = (translate @@ T.Build.Boolean.(||) conda_bool condb_bool) in
        let v,ret = declare_vars (Immutable_array.length a) "nondet" a in

        let v_eq_a = S.and_list @@ trconda :: List.fold_left2 (fun acc vi i ->
            let (Any ai) = Immutable_array.get a i in
            S.(=) vi (translate ai)::acc) [] v used_indices in
        let v_eq_b = S.and_list @@ trcondb :: List.fold_left2 (fun acc vi i ->
            let (Any bi) = Immutable_array.get b i in
            S.(=) vi (translate bi)::acc) [] v used_indices in
        let rel = S.(=>) both (S.(||) v_eq_a v_eq_b) in
       CFG_Node_Hash.replace tr_tuple_rel tup rel;
       ret

      | T.Mu{level;init;var;body;body_cond} as mu ->
         (* First pass: output the declarations and compute translations. *)
         (* Note: we convert everything to a _reversed_ list. *)
         let (bodyt,initt,vart) =
           let translate_array_to_list a =
             List.fold_left (fun acc i ->
                 let Any x = Immutable_array.get a i in
                 (translate x)::acc
               ) [] used_indices
           in
           translate_array_to_list body,
           translate_array_to_list init,
           translate_array_to_list var
         in
         let mu_arg_sort =
           List.fold_left (fun acc i ->
               let vi = Immutable_array.get init i in
               (any_to_sort vi)::acc
             ) [] used_indices
         in
         let body_cond_t = translate body_cond in
         let v,ret = declare_vars (Immutable_array.length init) "mu" init in
         let v_list = List.rev v in


        (* Second pass: compute the definition of the loop body, and
           the set of captured variables, i.e. variables used inside
           the mu, that have been defined before the mu, and which must be
           explicitely passed as an (input) argument in the Horn formalism. *)
        let (captured,acc_rel) =
          let filtered_body = used_indices |> List.map (fun i -> Immutable_array.get body i) in
          get_rels_and_captured_vars level @@ (Any body_cond)::filtered_body in
        (* Captured is also used to compute the dependencies of a mu body. *)
        CFG_Node_Hash.replace captured_hash mu captured;

        let captured_var_sort = CapturedSet.fold (fun x acc -> (any_to_sort x)::acc) captured [] in
        let captured_var_tr = CapturedSet.fold (fun x acc -> (x |> function Any x -> x |> translate)::acc) captured [] in

        (* Output the definitions for mu. *)
        (* Order: mu(captured_vars,input,output). *)
        let (query,name) = S.declare_rel ~name:"mu" (captured_var_sort @ mu_arg_sort @ mu_arg_sort) in

        (* mu(captured_var,init,init). Note: init may be replaced by a constant, this is OK. *)
        (* S.rule [] @@ query @@ captured_var_tr @ vart @ vart; *)
        S.rule [] @@ query @@ captured_var_tr @ initt @ initt;


        (* mu(captured,var,init) && rels => mu(captured,body,init). *)
        let body_start = query @@ captured_var_tr @ vart @ initt in
        S.rule (body_start::body_cond_t::acc_rel) (query @@ captured_var_tr @ bodyt @ initt);

        (* Note that unknown() variables appear nowhere, because they
           are implicitely universally quantified. *)

        let rel = query @@ captured_var_tr @ v_list @ initt in
        CFG_Node_Hash.replace tr_tuple_rel mu rel;

        ret

    and translate_tuple tup =
      try CFG_Node_Hash.find tr_tuple_memo tup
      with Not_found ->
        let res = translate_tuple_ tup in
        let res = Immutable_array.of_list res in
        CFG_Node_Hash.replace tr_tuple_memo tup res;
        res

    (* DFS iteration, starting from root nodes, for all elements with
       the same level.  Computes the set of captured vars (i.e. found
       elements whose level is < level), and fold on the relations for
       every element traversed. *)
    and get_rels_and_captured_vars: level -> T.any list -> (CapturedSet.t * S.value list) = fun level term_list ->
        let visited = AnyHash.create 17 in
        let visited_tuple = CFG_Node_Hash.create 17 in

        (* DFS iteration, but only for constrains in the loop
           (i.e. whose level correspond to the one in the loop). *)
        let rec loop acc node =
          if AnyHash.mem visited node
          then acc
          else begin
            AnyHash.add visited node ();
            let T.Any n = node in
            let nlevel = T.level n in
            if nlevel != level + 1 then (* Capture a variable, which is not visited. *)
              if nlevel == -1 then acc (* Do not capture constants. *)
              else
                let acc_cset,acc_rel = acc in
                CapturedSet.add node acc_cset, acc_rel
            else
              let acc = begin match T.Utils.get_term n with
                | T.T2{a;b} ->
                  let acc = loop acc (T.Any a) in
                  let acc = loop acc (T.Any b) in
                  acc
                | T.T1{a} -> loop acc (T.Any a)
                | T.T0 _ | T.Mu_formal _ | T.Unknown _ | T.Empty | T.Inductive_var _ -> acc
                | T.Tuple_get(i,tup)  -> loop_tuple acc tup
              end in
              match AnyHash.find tr_memo_rel node with
              | None -> acc
              | Some rel -> let acc_cset,acc_rel = acc in acc_cset, rel::acc_rel end
        and loop_tuple acc tup =
          if CFG_Node_Hash.mem visited_tuple tup
          then acc
          else begin
            CFG_Node_Hash.add visited_tuple tup ();
            (* Get the dependencies of the tuple. *)
            let acc = match tup with
            | Inductive_vars _ -> assert false
            | Nondet{conda_bool;condb_bool;a;b} ->
              let acc = loop acc @@ T.Any conda_bool in
              let acc = loop acc @@ T.Any condb_bool in
              let used_indices = Slicing.slicing tup in
              let acc =
                List.fold_left (fun acc i -> loop acc @@ Immutable_array.get a i) acc used_indices
              in
              let acc =
                List.fold_left (fun acc i -> loop acc @@ Immutable_array.get b i) acc used_indices
              in

              (* let acc = Immutable_array.fold_left loop acc a in
               * let acc = Immutable_array.fold_left loop acc b in *)
              acc
            | Mu{init} as mu ->
              let captured = CFG_Node_Hash.find captured_hash mu in
              let acc = CapturedSet.fold (fun x acc -> loop acc x) captured acc in
              let used_indices = Slicing.slicing tup in
              let acc =
                List.fold_left (fun acc i -> loop acc @@ Immutable_array.get init i) acc used_indices
              in
              acc
            in
            (* Add the tuple relation itself. *)
            let rel = CFG_Node_Hash.find tr_tuple_rel tup in
            let acc_cset,acc_rel = acc in
            acc_cset,rel::acc_rel
          end
        in
        List.fold_left loop (CapturedSet.empty,[]) term_list
    ;;

    (* Define the variables, and fill [tr_memo_rel]. *)
    let define_var ~constr sort expr =
      let Any(q) = constr in
      (* Do not define relations for constants. *)
      match constr with
      | Any(Bool{term=T0 _}) ->
          AnyHash.replace tr_memo_rel constr None;
          expr
      | Any(Integer{term=T0 _}) ->
        AnyHash.replace tr_memo_rel constr None;
        expr
      | Any(Binary{term=T0 _}) ->
        AnyHash.replace tr_memo_rel constr None;
        expr
      | _ ->

      let var = S.declare_muz_var ~name:(M.varname constr) sort in
      let rel = S.(=) var expr in
      AnyHash.replace tr_memo_rel constr (Some rel);
      var
    ;;

    let declare_var ~constr sort =
      let var = S.declare_muz_var ~name:(M.varname constr) sort in
      AnyHash.replace tr_memo_rel constr None;
      var
    ;;

  end

  and MakeApply:sig
    val translate: 'res T.t -> ('arg,'res) Operator.Function_symbol.function_symbol -> 'arg M.t -> S.value
  end = M.Make(MakeArg)

  end

  let translate assertion =

    let module Real_Slicing() = struct
      (* Codex_log.feedback "Slicing on %a" T.pretty assertion;; *)
      let slicing = Slicing.deps assertion;;
    end in

    let module Dummy_Slicing() = struct
      [@@@ocaml.warning "-32"]
      let slicing t =
        let length = match t with
          | T.Inductive_vars _ -> assert false
          | T.Mu{init} -> Immutable_array.length init
          | T.Nondet{a} -> Immutable_array.length a
        in let rec loop acc = function
            | x when x < 0 -> acc
            | n -> loop (n::acc) (n-1)
        in loop [] (length - 1)
      [@@@ocaml.warning "+32"]
    end in



    let module M = Make(Real_Slicing()) in
    (* let module M = Make(Dummy_Slicing()) in *)

    (* First pass: generation all the variable declarations; compute
       the definitions. *)
    let assertiont = (M.MakeArg.translate assertion) in

    (* Second pass: compute the query. *)
    let (_,acc) = M.MakeArg.get_rels_and_captured_vars (-1) [T.Any assertion] in

    (* We use an implicit quantification over free variables, and we
       do not need any parameter for the query. *)
    let (query,name) = S.declare_rel ~name:"qu" [] in
    S.rule (assertiont::acc) (query []);

    (* We had to set this to false in older z3 versions. Works fine at
       least since 4.4.1 *)
    let is_master = true in
    let res = match is_master with
      | true -> S.query2 name
      | false -> S.query (query [](* used_horn_variables *))
    in
    (match res with
     | S.Sat ->
       Log.debug (fun p -> p "Result is sat");
     Smtbackend.Smtlib_sig.Sat ()
     | S.Unsat ->
       Log.debug (fun p -> p "Result is unsat");
       Smtbackend.Smtlib_sig.Unsat
     | S.Unknown -> Smtbackend.Smtlib_sig.Unknown)
  ;;

end
