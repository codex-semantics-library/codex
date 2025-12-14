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

(* Translate an address into something which is either an address or a
   numeric value.

   XXX: This is a disjoint union semantics. Maybe we could have the
   intersection semantics instead, but intersection semantics can be
   easily added.

   Disjoint union semantics: we consider that "normal"addresses and
   numeric values are separated. Still, it is possible to assign
   memory locations to numeric values.

   One of the benefits of this semantics is that in operations like
   add, we can reconstruct and find which is the address and which is
   the offset.

   It would only require that Address say what to do with biconst
   (does it know how to create an address from this? Region_separation
   does, for instance.)

   XXX: Ideally this would be a functor that creates *)
(* TODO: At the top level, we should have a non-relational domain for C values,
   which we could use instead of the is_address boolean here. This would allow
   gaining precision when doing != with a singleton on one side, any anything else
   in the other side (currently we can learn only in limited cases).
*)

module Log = Tracelog.Make(struct let category = "Domains.Value_union_concatenation" end);;
module TypedC = Types.TypedC
module Type_check_tree = Types.Type_check_tree
module In_bits = Units.In_bits
module In_bytes = Units.In_bytes

let option_merge merge left right =
  match left, right with
  | None, None -> None
  | Some v, None
  | None, Some v -> Some v
  | Some l, Some r -> Some (merge l r)

let option_map2 merge left right =
  match left, right with
  | Some l, Some r -> Some (merge l r)
  | _ -> None

module MakeAddressOnly(Sub:Memory_sig.FIXED_SIZE_VALUE_DOMAIN) = struct

    module Scalar = Sub.Scalar

    (** binary operators: =, signed <=, unsigned <= and their negation *)
    type binpred = B_EQ | B_SLE | B_ULE | B_NEQ | B_NSLE | B_NULE

    type arith = PLUS | MINUS

    type enum = Scalar.enum

    type binary =
      | Val of Sub.binary
      | Concat of (In_bits.t * binary) list (* size of the field * field value (sorted in ascending order) *)
      | Union of enum * (binary * boolean) list (* values of each case along with its condition *)
      | BofBool of In_bits.t * Sub.binary * boolean (* size and boolean to convert *)

    (** Elementary boolean types: quadrivalent or binpred *)
    and boolean_simple =
      | BVal of Sub.boolean
      | BBinpred of {pred:binpred; size:In_bits.t; left : binary; right : binary }
      | BValid of { size:In_bits.t; arg:binary}
      | BValidArith of {size:In_bits.t; op:arith; left: binary; right : binary}
      | BCaseOf of {arg: enum; case : int}

    (** Simple boolean AST, not is cast down on operator or quadrivalent *)
    and boolean_tree =
      | BSimple of boolean_simple
      | BAnd of boolean_tree * boolean_tree
      | BOr of boolean_tree * boolean_tree

    and boolean = boolean_tree

    let fresh_id =
      let counter = ref 0 in
      fun () -> incr counter; !counter

    let boolean_empty ctx = BSimple (BVal (Sub.boolean_empty ctx))
    let boolean_unknown ctx = BSimple (BVal (Sub.boolean_unknown ctx))

    let binary_empty ~size ctx = Val (Sub.binary_empty ~size ctx)

    let enum_empty = Scalar.enum_empty

    let join_values ~size ctx list =
      match list with
      | [] -> Sub.binary_empty ~size ctx
      | [x]-> x
      | a::b ->
        let nondet_binary v1 v2 =
          let open Scalar in 
          let Context.Result(_,tup,deserialize2) = Sub.serialize ~widens:false ~size ctx v1 ctx v2
              (true, Context.empty_tuple ()) in
          let res_tup = Scalar.nondet_same_context ctx tup in
          fst @@ deserialize2 ctx res_tup
        in List.fold_left nondet_binary a b
    ;;

    let binary_unknown ~size ctx = Val (Sub.binary_unknown ~size ctx)

    let enum_unknown = Scalar.enum_unknown


    (** Negate a binary operator *)
    let not_binpred = function
      | B_EQ -> B_NEQ
      | B_SLE -> B_NSLE
      | B_ULE -> B_NULE
      | B_NEQ -> B_EQ
      | B_NSLE -> B_SLE
      | B_NULE -> B_ULE

    (** Value of a cross operation (scalar <op> address)*)
    let binpred_cross_comp =
      let open Lattices.Quadrivalent in
      function
      | B_EQ -> False
      | B_NEQ -> True
      | B_SLE
      | B_ULE
      | B_NSLE
      | B_NULE -> Top

    (** True not in binpred_cross_comp *)
    let binpred_is_positive op =
      match binpred_cross_comp op with
      | Bottom | False -> true
      | Top | True -> false

    (** Convert a quadrivalent value to a scalar.boolean *)
    let quadri2scalar_bool ctx quadri =
      let open Lattices.Quadrivalent in
      match quadri with
      | True -> Scalar.Boolean_Forward.true_ ctx
      | False -> Scalar.Boolean_Forward.false_ ctx
      | Bottom -> Scalar.boolean_empty ctx
      | Top -> Scalar.boolean_unknown ctx

    (** Convert a quadrivalent value to a subaddress.boolean *)
    let quadri2addr_bool ctx quadri =
      let open Lattices.Quadrivalent in
      match quadri with
      | True -> Sub.Boolean_Forward.true_ ctx
      | False -> Sub.Boolean_Forward.false_ ctx
      | Bottom -> Sub.boolean_empty ctx
      | Top -> Sub.boolean_unknown ctx

    (** Downcast binary operator to SubAddress booleans *)
    let rec sub_predicate ~size ctx = function
      | B_EQ -> Sub.Binary_Forward.beq ~size ctx
      | B_SLE -> Sub.Binary_Forward.bisle ~size ctx
      | B_ULE -> Sub.Binary_Forward.biule ~size ctx
      | not_op -> fun l r -> Sub.Boolean_Forward.not ctx
          (sub_predicate ~size ctx (not_binpred not_op) l r)

    (** Downcast binary operator to Scalar booleans *)
    let rec scal_predicate ~size ctx = function
      | B_EQ -> Scalar.Binary_Forward.beq ~size ctx
      | B_SLE -> Scalar.Binary_Forward.bisle ~size ctx
      | B_ULE -> Scalar.Binary_Forward.biule ~size ctx
      | not_op -> fun l r -> Scalar.Boolean_Forward.not ctx
          (scal_predicate ~size ctx (not_binpred not_op) l r)


    let binpred_pretty fmt op =
      let open Format in
      match op with
      | B_EQ -> fprintf fmt "EQ"
      | B_SLE -> fprintf fmt "SLE"
      | B_ULE -> fprintf fmt "ULE"
      | B_NEQ -> fprintf fmt "NEQ"
      | B_NSLE -> fprintf fmt "NSLE"
      | B_NULE -> fprintf fmt "NULE"

    let arith_pretty fmt op =
      let open Format in
      match op with PLUS -> fprintf fmt "PLUS" | MINUS -> fprintf fmt "MINUS"


    (* Used to keep track of intersection between between instantiated
       and non instantiated existential types for better precision
    *)
    let get_type, add_type =
      let (typemap : (binary, TypedC.typ) Hashtbl.t) = Hashtbl.create 123 in
      (fun bin -> Hashtbl.find_opt typemap bin),
      (fun bin typ -> Hashtbl.add typemap  bin typ) ;;

    module rec Binary : Datatype_sig.S with type t=binary = struct
      type t = binary

      let rec pretty fmt x =
        let open Format in
        match x with
        | Val v -> Sub.Binary.pretty fmt v
        | Concat s ->
          fprintf fmt "@[<hov 2>Struct {@[<v 2>[@ %a@ ]@];@ }@]"
            (pp_print_list
              (fun fmt (sz,v) -> fprintf fmt "(@[<hov 2>%d,@ %a@]);" (In_bits.to_int sz) pretty v)) s

        | Union (discrim,cases) ->
          fprintf fmt "@[<hov 2>Union {@[<v 2>[@ %a : %a@ ]@];@ }@]"
            Scalar.Enum.pretty discrim
            (pp_print_list
              (fun fmt (v,cond) ->
                fprintf fmt "(@[<hov 2>%a,@ %a@]);"
                  pretty v
                  Boolean.pretty cond
              )
            ) cases

        | BofBool (size,v,b) -> fprintf fmt "@[<hov 2>Bofbool {@[<v 2>[@ %a@ : %a of size %d@]@];@ }@]" Sub.Binary.pretty v Boolean.pretty b (size:>int)


      let rec hash x =
        let total = 4 in
        match x with
        | Val value -> Hashing.hash_sum ~nb:0 ~total (Sub.Binary.hash value)
        | Concat values -> Hashing.hash_sum ~nb:1 ~total (Hashing.hash_list (fun (sz, value) -> hash value) values)
        | Union (enum,values) -> Hashing.hash_sum ~nb:2 ~total (Hashing.hash2
            (Scalar.Enum.hash enum)
            (Hashing.hash_list (fun (value, _) -> hash value) values))
        | BofBool (sz, bin, cond) -> Hashing.hash_sum ~nb:3 ~total @@ Hashing.hash2  (Boolean.hash cond) (Sub.Binary.hash bin)


      let compare_list cmp_elt l1 l2 =
        let rec cmp = function
          | [],[] -> 0
          | [],_ -> -1
          | _,[] -> 1
          | (h1 :: t1), (h2 :: t2) ->
            let c = cmp_elt h1 h2 in
            if c = 0 then cmp (t1, t2) else c
        in
        cmp (l1,l2)

      let rec compare x y =
        match x,y with
        | Val vx, Val vy -> Sub.Binary.compare vx vy
        | Val _, _ -> -1
        | _, Val _ -> 1

        | Concat cx, Concat cy ->
            compare_list (fun (_,v1) (_,v2) -> compare v1 v2) cx cy
        | Concat _, _ -> -1
        | _, Concat _ -> 1

        | Union (dx,lsx), Union (dy,lsy) ->
            compare_list (fun (v1,c1) (v2,c2) ->
              let c = compare v1 v2 in
              if c <> 0 then c
              else Boolean.compare c1 c2
            ) lsx lsy
        | Union _, _ -> -1
        | _, Union _ -> 1

        | BofBool (szx, vx, cx), BofBool (szy, vy, cy) ->
            let c = In_bits.compare szx szy in
            if c <> 0 then c
            else
              let c = Sub.Binary.compare vx vy in
              if c <> 0 then c
              else Boolean.compare cx cy

      let equal x y = compare x y = 0
    end

    and Boolean : Datatype_sig.S with type t=boolean = struct
      type t = boolean

      let pretty_simple fmt = function
        | BVal x -> Format.fprintf fmt "BVal(%a)" Sub.Boolean.pretty x
        | BValid{size;arg} -> Format.fprintf fmt "BValid{size=%d;arg=%a}" (size:>int) Binary.pretty arg
        | BBinpred{ pred; size; left; right } ->
            Format.fprintf fmt "BBinpred{pred:%a;left:%a;right:%a}"
              binpred_pretty pred
              Binary.pretty left
              Binary.pretty right
        | BValidArith {size; op; left; right} ->
            Format.fprintf fmt "BBinpred{size:%d;pred:%a;left:%a;right:%a}"
              (size:>int)
              arith_pretty op
              Binary.pretty left
              Binary.pretty right
        | BCaseOf {arg;case} -> Format.fprintf fmt "BCaseof{arg=%a;case=%d}" Scalar.Enum.pretty arg case
      let rec pretty_tree fmt x = match x with
        | BSimple b -> pretty_simple fmt b
        | BAnd(l,r) -> Format.fprintf fmt "BAnd(@[%a@,@ %a@])" pretty_tree l pretty_tree r
        | BOr(l,r) -> Format.fprintf fmt "BOr(@[%a@,@ %a@])" pretty_tree l pretty_tree r
      let pretty fmt x =
        Format.fprintf fmt "%a" pretty_tree x

      let compare_binpred a b =
        if a = b then 0
        else
          match a,b with
          | B_EQ, _ -> -1
          | _, B_EQ -> 1
          | B_SLE, _ -> -1
          | _, B_SLE -> 1
          | B_ULE, _ -> -1
          | _, B_ULE -> 1
          | B_NEQ, _ -> -1
          | _, B_NEQ -> 1
          | B_NSLE, _ -> -1
          | _, B_NSLE -> 1
          | B_NULE, _ -> -1

      (* No real notion of order on booleans... *)
      let compare_simple a b =
        match a,b with
        | BVal x, BVal y -> Sub.Boolean.compare x y
        | BVal _, _ -> -1
        | _, BVal _ -> 1
        | BValid{size=sza;arg=x}, BValid{size=szb;arg=y} ->
          let c = In_bits.compare sza szb in
          if c <> 0 then c else Binary.compare x y
        | BValid _, _ -> -1
        | _, BValid _ -> 1
        | BBinpred {pred=pa;left=la;right=ra}, BBinpred{pred=pb;left=lb;right=rb} ->
          let c = compare_binpred pa pb in
          if c <> 0 then c
          else
            let c = Binary.compare la lb in
            if c <> 0 then c
            else Binary.compare la lb
        | BBinpred _, _ -> -1
        | _, BBinpred _ -> 1
        | BValidArith {size=sza;op=opa;left=la;right=ra}, BValidArith {size=szb;op=opb;left=lb;right=rb} ->
          let c = (Stdlib.compare : arith -> arith -> int) opa opb in
          if c <> 0 then c
          else
            let c = Binary.compare la lb in
            if c <> 0 then c else Binary.compare ra rb

        | BCaseOf {arg=x;case=ca}, BCaseOf {arg=y;case=cb} ->
          let c = (Stdlib.compare : int -> int -> int) ca cb in
          if c <> 0 then c
          else Scalar.Enum.compare x y
        | BCaseOf _, _ -> -1
        | _, BCaseOf _ -> 1

      let rec compare a b =
        match a,b with
        | BSimple x, BSimple y -> compare_simple x y
        | BSimple _, _ -> -1
        | _, BSimple _ -> 1
        | BOr(la,ra), BOr(lb,rb) ->
          let c = compare la lb in
          if c <> 0 then c
          else compare ra rb
        | BOr _, _ -> -1
        | _, BOr _ -> 1
        | BAnd(la,ra), BAnd(lb,rb) ->
          let c = compare la lb in
          if c <> 0 then c
          else compare ra rb

      let equal a b = 0 = (compare a b)

      let hash_simple n =
        let total = 5 in
        match n with
        | BVal x -> Hashing.hash_sum ~nb:0 ~total (Sub.Boolean.hash x)
        | BValid{size;arg} -> Hashing.hash_sum ~nb:1 ~total (Binary.hash arg)
        | BBinpred{pred;left;right} ->
              Hashing.hash_sum ~nb:2 ~total @@
              Hashing.hash3 (Hashtbl.hash pred) (Binary.hash left) (Binary.hash right)
        | BValidArith {size; op; left; right} ->
              Hashing.hash_sum ~nb:3 ~total @@
              Hashing.hash4 4 (Hashtbl.hash pred) (Binary.hash left) (Binary.hash right)
        | BCaseOf {arg; case} ->
              Hashing.hash_sum ~nb:4 ~total @@
              Hashing.hash2 case (Scalar.Enum.hash arg)
      let rec hash = function
        | BSimple x -> Hashing.hash_sum ~nb:0 ~total:3 (hash_simple x)
        | BAnd(l,r) -> Hashing.hash_sum ~nb:1 ~total:3 @@ Hashing.hash2 (hash l) (hash r)
        | BOr (l,r )-> Hashing.hash_sum ~nb:2 ~total:3 @@ Hashing.hash2 (hash l) (hash r)
    end

    module Enum = Scalar.Enum


    (* ------------------------- Global symbols ------------------------- **)

    exception Global_symbol_not_found


    let rec global_constants =
      let table = ref ([] : (string * (In_bits.t * binary)) list) in
      let initialized = ref false in
      fun ctx ->
        if !initialized then table
        else begin
          (* table := List.map (fun (name,size,pred) ->
            let b = Sub.binary_unknown ~size ctx in
            (name, (size, Val (Sub.imperative_use_invariant ~size ctx pred b)))
          ) TS.global_symbols; *)
          initialized := true;
          table
        end

    and global_symbol ctx s =
      try
        (* List.assoc s (!(global_constants ctx)) *)
        let (size, bin) = List.assoc s (!(global_constants ctx)) in
        size, bin
      with Not_found ->
        Format.printf "Cannot find global symbolic variable : %s\n" s ;
        raise Global_symbol_not_found

    and add_global_symbol ~size ctx name binary =
      Log.debug (fun p -> p "Adding global symbol %s of value %a" name Binary.pretty binary);
       let table = global_constants ctx in
        table := (name, (size, binary))::(!table)


    module type BOOLEAN_FORWARD_TYPE = sig
      include Operator.BOOLEAN_FORWARD
        with module Arity := Sig.Context_Arity_Forward(Scalar.Context)
          and type boolean := boolean

      val equiv : Scalar.Context.t -> boolean -> boolean -> boolean
    end

    module Boolean_Forward : BOOLEAN_FORWARD_TYPE = struct

      module Arity = Sig.Context_Arity_Forward(Scalar.Context)

      let true_ ctx = BSimple (BVal (Sub.Boolean_Forward.true_ ctx))
      let false_ ctx = BSimple (BVal (Sub.Boolean_Forward.false_ ctx))

      let not_simple ctx = function
          | BVal x -> BVal (Sub.Boolean_Forward.not ctx x)
          | BBinpred{pred;size;left;right} -> BBinpred{pred=not_binpred pred;size;left;right}
          | BValid _ -> assert false
          | BValidArith _ -> assert false
          | BCaseOf _ -> assert false

      let rec not_tree ctx = function
          | BSimple b -> BSimple (not_simple ctx b)
          | BAnd(l,r) -> or_tree ctx (not_tree ctx l) (not_tree ctx r)
          | BOr(l,r) -> and_tree ctx (not_tree ctx l) (not_tree ctx r)

      and and_tree ctx l r = match l,r with
        | BSimple(BVal e), a
        | a, BSimple(BVal e) ->
          begin
            match Sub.query_boolean ctx e with
            | False | Bottom -> BSimple(BVal e)
            | True -> a
            | _ -> if Boolean.equal l r then l else BAnd(l,r)
          end
        | _ ->
          if Boolean.equal l r then l
          else if Boolean.equal l (not_tree ctx r) then false_ ctx
          else BAnd(l,r)

      and or_tree ctx l r = match l,r with
        | BSimple(BVal e), a
        | a, BSimple(BVal e) ->
          begin
            match Sub.query_boolean ctx e with
            | True | Bottom -> BSimple(BVal e)
            | False -> a
            | _ -> if Boolean.equal l r then l else BOr(l,r)
          end
        | _ ->
          if Boolean.equal l r then l
          else if Boolean.equal l (not_tree ctx r) then true_ ctx
          else BOr(l,r)

      let not ctx x = not_tree ctx x

      let (&&) ctx l r = and_tree ctx l r
      let (||) ctx l r = or_tree ctx l r

      (* Boolean equivalence operation *)
      let equiv ctx l r = (||) ctx ((&&) ctx l r) ((&&) ctx (not ctx l) (not ctx r))
    end

    (*--------------- Transfer functions for enumeration values ---------------*)

    module Enum_Forward (* : Enum_Forward_type *) = struct

      (* module Arity = Domain_sig.Context_Arity_Forward(Context) *)

      let caseof ~case ctx x = BSimple (BCaseOf {arg=x; case})
      let enum_const ~case = assert false

    end

    (*--------------- Utility functions to call "assume" with the correct sizes ---------------*)

    type expr_size = Const of In_bits.t | Min of In_bits.t

    let eq_size sz1 sz2 =
      match sz1, sz2 with
      | Const i, Const j when i = j -> sz1
      | Min i, Const j when In_bits.(i <= j) -> sz2
      | Const j, Min i when In_bits.(i <= j) -> sz1
      | Min i, Min j -> Min (max i j)
      | Const i, Min j ->
          raise (Failure ("invalid expr size : Const " ^ (string_of_int (i:>int)) ^ " and Min " ^ (string_of_int (j:>int))))
      | Min i, Const j ->
          raise (Failure ("invalid expr size : Min " ^ (string_of_int (i:>int)) ^ " and Const " ^ (string_of_int (j:>int))))
      | Const i, Const j ->
          raise (Failure ("invalid expr size : Const " ^ (string_of_int (i:>int)) ^ " and Const " ^ (string_of_int (j:>int))))

    let rec expr_size ~size ctx expr =
      let open TypedC.Pred in
      match expr with
      | Const c -> Min (In_bits.of_int (Z.numbits c))
      | Self -> Const size
      | Val(Sym s) -> let sz = fst @@ global_symbol ctx s in Const sz
      | Binop(op, e1, e2) ->
          let sz1 = expr_size ~size ctx e1
          and sz2 = expr_size ~size ctx e2
          in eq_size sz1 sz2
      | Unop(Extract (index, len), e) ->
          let sz = expr_size ~size ctx e
          in eq_size sz (Const len)
      (* | Unop(op, e) -> *)

    let cmp_size ~size ctx expr1 expr2 =
      let size1 = expr_size ~size ctx expr1
      and size2 = expr_size ~size ctx expr2 in
      match eq_size size1 size2 with
      | Const sz | Min sz -> sz

    (* Conversion operation for flags of operation "valid_ptr_arith" *)
    let arith_to_type = function
      | PLUS -> Operator.Plus
      | MINUS -> Operator.Minus

    module Context = Scalar.Context
    module type PRETTY_TYPE = sig
      val binary_pretty : size:In_bits.t -> Context.t -> Format.formatter -> binary -> unit
      val boolean_simple_pretty : Context.t -> Format.formatter -> boolean_simple -> unit
      val boolean_pretty : Context.t -> Format.formatter -> boolean -> unit
      val enum_pretty : Context.t -> Format.formatter -> enum -> unit
    end

    module type CONVERSION_TYPE = sig
      val force: size:In_bits.t -> Context.t -> binary -> Sub.binary
      val forceb_simple: Context.t -> boolean_simple -> Sub.boolean
      val forceb: Context.t -> boolean -> Sub.boolean
    end

    module type QUERY_TYPE = sig
      val quadri2sub_bool: Context.t -> Lattices.Quadrivalent.boolean -> Sub.boolean
      val boolean: Scalar.Context.t -> boolean -> Lattices.Quadrivalent.t

      module Binary_Lattice: Sig.Binary_Lattice
      val binary: size:In_bits.t -> Context.t -> binary -> Binary_Lattice.t

      module Enum_Lattice: Sig.Enum_Lattice
      val enum: Context.t -> enum -> Enum_Lattice.t
    end

    module type BINARY_FORWARD_TYPE = sig
      include Operator.BINARY_FORWARD
        with module Arity := Sig.Context_Arity_Forward(Context)
          and type boolean := boolean
          and type binary := binary

      val binary_of_expr : size:In_bits.t -> Context.t -> self:binary -> Types.TypedC.Pred.expr -> binary
      val cond_of_pred : size:In_bits.t -> Context.t -> Types.TypedC.Pred.t -> self:binary -> boolean
      val discriminated : size:In_bits.t -> Context.t -> binary -> binary
      val evaluate_cases : size:In_bits.t -> Context.t -> enum -> (binary * boolean) list -> (binary * boolean) list
      val binpred_to_operation : size:In_bits.t -> Context.t -> binpred -> binary -> binary -> boolean
    end

    module type ASSUME_TYPE = sig

      val assume_simple : Context.t -> boolean_simple -> Context.t option
      val assume_tree : Context.t -> boolean_tree -> Context.t option

    end


    module rec Pretty : PRETTY_TYPE = struct
      let rec binary_union_pretty ~size ctx fmt (discrim,cases) =
        let open Format in
        let folder (case,lst) (value,cond) =
          let b1 = Scalar.query_boolean ctx @@ Scalar.Enum_Forward.caseof ~case ctx discrim in
          let b2 = Query.boolean ctx cond in
          match b1,b2 with
          | Lattices.Quadrivalent.(False | Bottom), _
          | _, Lattices.Quadrivalent.(False | Bottom) -> (case + 1, lst)
          | _ ->  (case + 1, (value,cond) :: lst)
        in
        let ncase,lst = List.fold_left folder (0,[]) cases in
        let lst = List.rev lst in
        (* assert (ncase = List.length cases); *)
        match lst with
        | [] -> fprintf fmt "<Empty Union>"
        | [(value, _)] -> binary_pretty ~size ctx fmt value
        | _ ->
          fprintf fmt "Union {%a}"
            (pp_print_list
              (fun fmt (value, cond) ->
                  fprintf fmt "@[<hov 2>{ %a with %a }@];" (binary_pretty ~size ctx) value (boolean_pretty ctx) cond)) lst

      and binary_pretty ~size ctx fmt x =
        let open Format in
        match x with
        | Val v -> Sub.binary_pretty ~size ctx fmt v (* (force ~size ctx x) *)
        | Concat ls ->
          let _,ls = List.fold_right (fun (sz,part) (ofs,acc) -> (In_bits.(ofs+sz), (ofs, sz, part)::acc)) ls (In_bits.zero,[]) in
          let pp_sep fmt () = Format.fprintf fmt ";@;" in
          fprintf fmt "Concat [@[<hov 2>%a@]]"
              (pp_print_list ~pp_sep
                 (fun fmt (ofs, sz, part) ->
                    fprintf fmt "@[<hov 2>{ %d -> %a of size %d }@]"
                      (In_bits.to_int ofs) (binary_pretty ~size:sz ctx) part (sz:>int))) ls

        | Union (discrim,cases) -> binary_union_pretty ~size ctx fmt (discrim,cases)

        | BofBool (sz,value,cond) ->
          fprintf fmt "@[<hov 2>Bofbool { %a of size %d }@]" (Sub.binary_pretty ~size ctx) value (* (boolean_pretty ctx) cond *) (sz:>int)

      (*
      and binary_pretty ~size ctx fmt x =
        let open Format in
        match x with
        | Val v -> Sub.binary_pretty ~size ctx fmt v (* (force ~size ctx x) *)
        | Concat ls ->
          let _,ls = List.fold_right (fun (sz,part) (ofs,acc) -> (ofs+sz, (ofs, sz, part)::acc)) ls (0,[]) in
          let pp_sep fmt () = Format.fprintf fmt ";@;" in
          fprintf fmt "Concat [@[<hov 2>%a@]]"
              (pp_print_list ~pp_sep
                (fun fmt (ofs, sz, part) -> fprintf fmt "@[<hov 2>{ %d -> %a of size %d }@]" ofs (binary_pretty ~size:sz ctx) part sz)) ls

        | Union cases ->
          let pp_sep fmt () = Format.fprintf fmt ";@;" in
          fprintf fmt "Union [@[<hov 2>%a@]]"
              (pp_print_list ~pp_sep
                (fun fmt (value, cond, discrim) -> fprintf fmt "@[<hov 2>{ %a with cond %a and discrim %a }@]"
                    (binary_pretty ~size ctx) value
                    (boolean_pretty ctx) cond
                    (Scalar.boolean_pretty ctx) discrim)) cases

        | BofBool (sz,value,cond) ->
          fprintf fmt "@[<hov 2>Bofbool { %a of size %d }@]" (Sub.binary_pretty ~size ctx) value (* (boolean_pretty ctx) cond *) sz
      *)
      and boolean_simple_pretty ctx fmt x =
        let open Format in
        match x with
        | BVal b -> fprintf fmt "BVal %a" Lattices.Quadrivalent.pretty @@ Sub.query_boolean ctx b
        | BBinpred {pred; size; left; right} ->
            fprintf fmt "BBinpred {pred:%a; size:%d; left:%a; right:%a}"
                binpred_pretty pred
                (size:>int)
                (binary_pretty ~size ctx) left
                (binary_pretty ~size ctx) right

        | BValid {size; arg} ->
          fprintf fmt "BValid {size:%d; arg:%a}" (size:>int) (binary_pretty ~size ctx) arg
        | BValidArith {size; op; left; right} ->
          fprintf fmt "BValid {size:%d; op:%a; left:%a; right:%a}"
            (size:>int) arith_pretty op
            (binary_pretty ~size ctx) left
            (binary_pretty ~size ctx) right
        | BCaseOf {arg;case} -> fprintf fmt "BCaseof {arg=%a;case=%d}" (Scalar.enum_pretty ctx) arg case

      and boolean_pretty ctx fmt x =
        let open Format in
        match x with
        | BSimple bs -> boolean_simple_pretty ctx fmt bs
        | BAnd (bl,br) ->
            fprintf fmt "(%a) and (%a)" (boolean_pretty ctx) bl (boolean_pretty ctx) br
        | BOr (bl,br) ->
            fprintf fmt "(%a) or (%a)" (boolean_pretty ctx) bl (boolean_pretty ctx) br

      let enum_pretty = Scalar.enum_pretty
    end

    and Conversion : CONVERSION_TYPE = struct

      let rec force_union ~size ctx (discrim,cases) =
        let folder (case,lst) (value, cond) =
          let b1 = Scalar.query_boolean ctx @@ Scalar.Enum_Forward.caseof ~case ctx discrim in
          let b2 = Query.boolean ctx cond in
          match b1,b2 with
          | Lattices.Quadrivalent.(False | Bottom), _
          | _, Lattices.Quadrivalent.(False | Bottom) -> (case + 1, lst)
          | _ -> (case + 1, (value,cond):: lst)
        in
        let ncase,lst = List.fold_left folder (0,[]) cases in
        let lst = List.rev lst in
        (* assert (ncase = List.length cases); *)
        match lst with
        | [] -> Sub.binary_empty ~size ctx
        | [(value, _ )] -> force ~size ctx value
        | lst ->
          let res = join_values ~size ctx @@ List.map (fun (value,_) -> force ~size ctx value) lst in
          Log.debug (fun p -> p "in VUC.force_union, joined union gives %a" (Sub.binary_pretty ~size ctx) res);
          res

      and force ~size ctx binary =
        match binary with
        | Val v -> v
        | Concat [] -> assert false (* Cannot concatenate an empty list. *)
        | Concat [(sz, v)] -> force ~size ctx v
        | Concat ((sz, v) :: tl) ->
          let folder (acc_value, acc_sz) (sz, value) =
            let value = force ~size:sz ctx value in
            let concat_value = Sub.Binary_Forward.bconcat ~size1:acc_sz ~size2:sz ctx acc_value value in
            concat_value, In_bits.(acc_sz + sz)
          in
          let first_value = force ~size:sz ctx v in
          let value, sz = List.fold_left folder (first_value, sz) tl in
          value

        | Union (discrim,cases) -> force_union ~size ctx (discrim,cases)

        | BofBool (sz, v, b) -> assert (sz = size) ; v (* Sub.Binary_Forward.bofbool ~size ctx @@ forceb ctx b *)

      and forceb_simple ctx boolean_simple =
        match boolean_simple with
        | BVal b -> b
        | BBinpred {pred; size; left; right} ->
            sub_predicate ~size ctx pred (force ~size ctx left) (force ~size ctx right)
        | BValid { size; arg:binary} ->
            Sub.Binary_Forward.valid ~size Operator.Read ctx (force ~size ctx arg)
        | BValidArith {size; op; left; right} ->
            Sub.Binary_Forward.valid_ptr_arith ~size (arith_to_type op) ctx (force ~size ctx left) (force ~size ctx right)
        | BCaseOf {arg;case} ->
            Query.quadri2sub_bool ctx @@ Scalar.query_boolean ctx @@ Scalar.Enum_Forward.caseof ~case ctx arg

      and forceb' ctx boolean =
        match boolean with
        | BSimple simple -> forceb_simple ctx simple
        | BAnd (b1,b2) -> Sub.Boolean_Forward.(&&) ctx (forceb ctx b1) (forceb ctx b2)
        | BOr (b1,b2) -> Sub.Boolean_Forward.(||) ctx (forceb ctx b1) (forceb ctx b2)

      and forceb ctx boolean =
        Log.trace (fun p -> p "forceb %a" (Pretty.boolean_pretty ctx) boolean)
        (*~pp_ret:(Sub.boolean_pretty ctx)*)
        @@ fun () -> forceb' ctx boolean

    end

    and Query : QUERY_TYPE = struct

      (* TODO : Precision could be improved by keeping an actual boolean expression (rather than its lattice value) longer *)
      let quadri2sub_bool ctx quadri =
        let open Lattices.Quadrivalent in
        match quadri with
        | True -> Sub.Boolean_Forward.true_ ctx
        | False -> Sub.Boolean_Forward.false_ ctx
        | Bottom -> Sub.boolean_empty ctx
        | Top -> Sub.boolean_unknown ctx

      let rec eval_assumed_cases ~case initctx pred discrim cases  =
        match cases with
        | [] -> Lattices.Quadrivalent.Bottom
        | (value, condition) :: tl ->
          let case_cond = Scalar.Enum_Forward.caseof ~case initctx discrim in
          let ctxopt = Scalar.assume initctx case_cond in
          let ctxopt = Option.bind ctxopt (fun ctx -> Assume.assume_tree ctx condition) in
          let res = begin match ctxopt with
              | None -> Lattices.Quadrivalent.Bottom
              | Some ctx -> eval_boolean ctx @@ pred value
            end
          in
          let res2 = (eval_assumed_cases ~case:(case + 1) initctx pred discrim tl) in
          let res3 = Single_value_abstraction.Quadrivalent.Boolean_Lattice.join res res2 in
          res3

      and eval_cases ctx pred discrim cases =
        match cases with
        | [] -> Lattices.Quadrivalent.Bottom
        | _ -> eval_assumed_cases ~case:0 ctx pred discrim cases

      and eval_boolean_simple ctx = function
        | BVal x -> Sub.query_boolean ctx x
        | BValid{size; arg = Union (discrim,cases)} ->
          eval_cases ctx (Binary_Forward.valid ~size Operator.Read ctx) discrim cases

        | BBinpred {pred; size; left = Union (discrim,cases); right} ->
          eval_cases ctx (fun x -> Binary_Forward.binpred_to_operation ~size ctx pred x right) discrim cases
        | BBinpred {pred; size; left; right = Union (discrim,cases)} ->
          eval_cases ctx (Binary_Forward.binpred_to_operation ~size ctx pred left) discrim cases

        | BValidArith {size; op; left = Union (discrim,cases); right} ->
          eval_cases ctx (fun x -> Binary_Forward.valid_ptr_arith ~size (arith_to_type op) ctx x right) discrim cases

        | BValidArith {size; op; left; right = Union (discrim,cases)} ->
          eval_cases ctx (fun x -> Binary_Forward.valid_ptr_arith ~size (arith_to_type op) ctx left x) discrim cases

        | BCaseOf {arg;case} -> Scalar.query_boolean ctx @@ Scalar.Enum_Forward.caseof ~case ctx arg

        | x -> Sub.query_boolean ctx @@ Conversion.forceb_simple ctx x

      and eval_boolean ctx b =
        match b with
        | BSimple x -> eval_boolean_simple ctx x
        | BOr (b1,b2) -> (* TODO : improve the precision by using quadri2bool rather than operating on the lattice *)
          Single_value_abstraction.Quadrivalent.Boolean_Forward.(||)
            (eval_boolean ctx b1)
            (eval_boolean ctx b2)
        | BAnd (b1,b2) -> (* TODO : improve the precision by using quadri2bool rather than operating on the lattice *)
          Single_value_abstraction.Quadrivalent.Boolean_Forward.(&&)
            (eval_boolean ctx b1)
            (eval_boolean ctx b2)

      let boolean ctx b =
        Log.trace (fun p -> p "boolean %a" (Pretty.boolean_pretty ctx) b)
        ~pp_ret:Lattices.Quadrivalent.pretty
        (fun () -> eval_boolean ctx b)

      module Binary_Lattice = Sub.Query.Binary_Lattice
      let binary ~size ctx x = Sub.Query.binary ~size ctx @@ Conversion.force ~size ctx x

      module Enum_Lattice = Scalar.Query.Enum_Lattice
      let enum ctx x = Scalar.Query.enum ctx x
    end

    and Binary_Forward : BINARY_FORWARD_TYPE = struct

      let rec apply_all ~size op1 ctx a b =
        match a,b with
        | Union (da,lsa), Union (db,lsb) when Scalar.Enum.equal da db ->
            (* Log.debug (fun p -> p "Applying operation to matching cases"); *)
            let ls = List.map2 (fun (v1,cond) (v2,_) -> apply_all ~size op1 ctx v1 v2, cond) lsa lsb in
            Union (da,ls)
        | Val _, Union (db,lsb) ->
            let ls = List.map (fun (value, cond) -> apply_all ~size op1 ctx a value, cond) lsb in
            Union (db,ls)
        | Union (da,lsa), Val _ ->
            let ls = List.map (fun (value, cond) -> apply_all ~size op1 ctx value b, cond) lsa in
            Union (da,ls)

        | _ -> Val (op1 ~size ctx (Conversion.force ~size ctx a) (Conversion.force ~size ctx b))

      let rec apply_all1 ~size op1 ctx a =
        match a with
        | Union (d,ls) ->
            let ls = List.map (fun (value, cond) -> apply_all1 ~size op1 ctx value, cond) ls in
            Union (d,ls)

        | _ -> Val (op1 ~size ctx (Conversion.force ~size ctx a))

      let bool_binpred op ~size (_ctx:Context.t) left right =
        (BSimple (BBinpred{pred=op; size; left; right }))

      let beq ~size ctx x y =
        Log.trace (fun p -> p "beq (boolean operation) %a and %a" (Pretty.binary_pretty ~size ctx) x (Pretty.binary_pretty ~size ctx) y)
      (*~pp_ret:(Pretty.boolean_pretty ctx)*)
      @@ fun () ->
        match x,y with
        | BofBool(sz, u, b), Val v
        | Val v, BofBool(sz, u, b) ->
            begin
              let is_zero = Sub.Binary_Forward.(beq ~size ctx v @@ biconst ~size Z.zero ctx) in
              match Sub.query_boolean ctx is_zero with
              | Lattices.Quadrivalent.True -> Boolean_Forward.not ctx b
              | Lattices.Quadrivalent.False -> b
              | Lattices.Quadrivalent.Top -> bool_binpred B_EQ ~size ctx x y
              | Lattices.Quadrivalent.Bottom -> boolean_empty ctx
            end

        | _ -> bool_binpred B_EQ ~size ctx x y

      let biule ~size ctx x y =
        Log.trace (fun p -> p "biule")
        @@ fun () ->
        bool_binpred B_ULE ~size ctx x y

      let bisle ~size ctx x y =
        Log.trace (fun p -> p "bisle")
        @@ fun () ->
        bool_binpred B_SLE ~size ctx x y

      let biadd ~size ~flags ctx x y =
        Log.trace (fun p -> p "biadd %a %a" (Pretty.binary_pretty ~size ctx) x (Pretty.binary_pretty ~size ctx) y)
        @@ fun () ->
        apply_all ~size (fun ~size ctx a b -> Sub.Binary_Forward.biadd ~size ~flags ctx a b) ctx x y

      let bimul ~size ~flags ctx x y = Val (Sub.Binary_Forward.bimul ~size ~flags ctx (Conversion.force ~size ctx x) (Conversion.force ~size ctx y))
      let bxor ~size ctx x y =
        Val (Sub.Binary_Forward.bxor ~size ctx (Conversion.force ~size ctx x) (Conversion.force ~size ctx y))
      (* let band ~size ctx x y = Val (Sub.Binary_Forward.band ~size ctx (force ctx x) (force ctx y)) *)

      let band ~size ctx x y =
        Log.trace (fun p -> p "band %a %a" (Pretty.binary_pretty ~size ctx) x (Pretty.binary_pretty ~size ctx) y )
        @@ fun () ->
        apply_all ~size Sub.Binary_Forward.band ctx x y


      let bor ~size ctx x y =
        Log.trace (fun p -> p "bor %a %a" (Pretty.binary_pretty ~size ctx) x (Pretty.binary_pretty ~size ctx) y)
        @@ fun () ->
        apply_all ~size Sub.Binary_Forward.bor ctx x y

      let rec bsext ~size ~oldsize ctx x =
        Log.trace (fun p -> p "bsext %a" (Pretty.binary_pretty ~size:oldsize ctx) x)
        @@ fun () ->
        match x with
        | Union (d,ls) ->
            let ls = List.map (fun (value, cond) -> bsext ~size ~oldsize ctx value, cond) ls in
            Union (d,ls)

        | _ -> Val (Sub.Binary_Forward.bsext ~size ~oldsize ctx @@ Conversion.force ~size:oldsize ctx x)

      let rec buext ~size ~oldsize ctx x =
        Log.trace (fun p -> p "buext %a" (Pretty.binary_pretty ~size:oldsize ctx) x )
        @@ fun () ->
        match x with
        | Union (d,ls) ->
            let ls = List.map (fun (value, cond) -> buext ~size ~oldsize ctx value, cond) ls in
            Union (d,ls)

        | _ -> Val (Sub.Binary_Forward.buext ~size ~oldsize ctx @@ Conversion.force ~size:oldsize ctx x)

      let bofbool ~size ctx b =
        match Query.boolean ctx b with
        | Lattices.Quadrivalent.True -> Val Sub.(Binary_Forward.bofbool ~size ctx @@ Boolean_Forward.true_ ctx)
        | Lattices.Quadrivalent.False -> Val Sub.(Binary_Forward.bofbool ~size ctx @@ Boolean_Forward.false_ ctx)
        | Lattices.Quadrivalent.Bottom -> Val (Sub.binary_empty ~size ctx)
        | _ -> BofBool (size, Sub.Binary_Forward.bofbool ~size ctx @@ Conversion.forceb ctx b, b)

      let rec bchoose ~size cond ctx x =
        match x with
        | Concat lst ->
          let lst = List.map (fun (size,a) -> (size, bchoose ~size cond ctx a)) lst in
          Concat lst

        | _ -> Val (Sub.Binary_Forward.bchoose ~size cond ctx @@ Conversion.force ~size ctx x)

      let bashr ~size ctx x y =
        apply_all ~size Sub.Binary_Forward.bashr ctx x y

      let blshr ~size ctx x y =
        apply_all ~size Sub.Binary_Forward.blshr ctx x y

      let bshl ~size ~flags ctx x y =
        apply_all ~size (fun ~size ctx a b ->Sub.Binary_Forward.bshl ~size ~flags ctx a b) ctx x y

      let bisdiv ~size ctx x y = Val (Sub.Binary_Forward.bisdiv ~size ctx (Conversion.force ~size ctx x) (Conversion.force ~size ctx y))
      let biudiv ~size ctx x y = Val (Sub.Binary_Forward.biudiv ~size ctx (Conversion.force ~size ctx x) (Conversion.force ~size ctx y))

      let bconcat ~(size1:In_bits.t) ~(size2:In_bits.t) ctx x y =
        (* let x_is_bottom = Query.binary_is_empty ~size:size1 @@ Query.binary ~size:size1 ctx x in
        let y_is_bottom = Query.binary_is_empty ~size:size2 @@ Query.binary ~size:size2 ctx y in
        if x_is_bottom || y_is_bottom then binary_empty ~size:(size1 + size2) ctx
        else *)
          match x,y with (* TODO : improve this *)
          | Concat l1, Concat l2 -> Concat (l1 @ l2)
          | Concat l1, _ when (size2:>int) mod 8 = 0 -> Concat (l1 @ [(size2,y)])
          | _, Concat l2 when (size1:>int) mod 8 = 0 -> Concat ((size1,x) :: l2)
          | _ when (size1:>int) mod 8 = 0 && (size2:>int) mod 8 = 0 -> Concat [(size1, x);(size2, y)]
          | _ -> Val (Sub.Binary_Forward.bconcat ~size1 ~size2 ctx (Conversion.force ~size:size1 ctx x) (Conversion.force ~size:size2 ctx y))

      let bismod ~size ctx x y = Val (Sub.Binary_Forward.bismod ~size ctx (Conversion.force ~size ctx x) (Conversion.force ~size ctx y))
      let biumod ~size ctx x y = Val (Sub.Binary_Forward.biumod ~size ctx (Conversion.force ~size ctx x) (Conversion.force ~size ctx y))

      (* let rec extract_from_concat ~size ~index ~oldsize ctx concat =
        let rec aux lst = match lst with
          | [] -> assert false
          | [(min, max, last)] -> let sz = max-min in [(sz, bextract ~size:(max-index) ~index:(index-min) ~oldsize:sz ctx last)]
          | (min, max, value) :: tl -> let sz = max-min in (sz, value) :: aux tl
        in
        let folder (sz, value) (acc, lst) = acc + sz, (acc, acc + sz, value)::lst in
        let acc,lst = List.fold_right folder concat (0,[]) in
        let filterer (min, max, value) = (max > index) && (min < index + size) in
        let lst = List.filter filterer lst in
        match lst with
        | [] -> assert false
        | [(min, max, value)] -> bextract ~size ~index:(index-min) ~oldsize:(max-min) ctx value
        | (min, max, first) :: tl ->
          let sz = max-min in Concat ((sz, bextract ~size:(index+size-min) ~index:0 ~oldsize:sz ctx first) :: aux tl)
      *)
      let rec extract_from_concat ~size ~index ~oldsize ctx concat =
        let concat = List.rev concat in
        let rec discard (discarded_size:In_bits.t) = function
          | ((sz, v) :: tl) as lst ->
            if discarded_size < index then
              if In_bits.(discarded_size + sz <= index) then discard In_bits.(discarded_size + sz) tl
              else
                let i = In_bits.(index - discarded_size) in
                let v = bextract ~size:In_bits.(sz-i) ~index:i ~oldsize:sz ctx v in
                (In_bits.(sz-i), v) :: lst
            else if discarded_size = index then lst
            else assert false

          | [] -> assert false
        in
        let lst = discard In_bits.zero concat in
        let rec take (taken_size:In_bits.t) acc = function
          | ((sz, v) as hd) :: tl ->
            if taken_size < size then
              if In_bits.(taken_size + sz <= size) then take In_bits.(taken_size + sz) (hd :: acc) tl
              else
                let s = In_bits.(size - taken_size) in
                let v' = bextract ~size:s ~index:In_bits.zero ~oldsize:sz ctx v in
                (s,v') :: acc
            else if taken_size = size then acc
            else assert false

          | [] ->
            if taken_size = size then acc
            else assert false
        in
        match take In_bits.zero [] lst with
        | [(sz,v)] -> assert (sz = size) ; v
        | lst -> Concat lst


      and bextract ~(size:In_bits.t) ~(index:In_bits.t) ~(oldsize:In_bits.t) ctx x =
        if In_bits.(index < zero || oldsize < index + size) then (
          Emit_alarm.emit_alarm Operator.Alarm.Extract_offset ;
          Codex_log.error "Extraction of size %d at offset %d outside the value %a" (size:>int) (index:>int) (Pretty.binary_pretty ~size:oldsize ctx) x ;
          binary_empty ~size ctx
        )
        else if size = oldsize then x
        else
          match x with
          | Val v -> Val (Sub.Binary_Forward.bextract ~size ~index ~oldsize ctx v)
          | Concat concat -> extract_from_concat ~size ~index ~oldsize ctx concat
          | Union (d,ls) ->
            let ls = List.map (fun (value, cond) -> bextract ~size ~index ~oldsize ctx value, cond) ls in
            Union (d,ls)
          | BofBool (sz, v, c) -> assert (oldsize = sz) ; Val (Sub.Binary_Forward.bextract ~size ~index ~oldsize ctx v)
              (* Val (Sub.Binary_Forward.bofbool ~size ctx @@ Conversion.forceb ctx c) *)

      let biconst ~size k ctx = Val (Sub.Binary_Forward.biconst ~size k ctx)

      let buninit ~size ctx = Val (Sub.Binary_Forward.buninit ~size ctx)

      let bshift ~size ~offset ~max ctx x =
        Log.trace (fun p -> p "bshift ~offset:%d %a" offset (Pretty.binary_pretty ~size ctx) x )
        @@ fun () ->
        apply_all1 ~size (fun ~size ctx a -> Sub.Binary_Forward.bshift ~size ~offset ~max ctx a) ctx x

      (* let bshift ~size ~offset ~max ctx x = Val (Sub.Binary_Forward.bshift ~size ~offset ~max ctx (force ctx x)) *)

      let bindex ~size k ctx x e =
        Log.trace (fun p -> p "bindex offset:%d %a %a" k (Pretty.binary_pretty ~size ctx) x (Pretty.binary_pretty ~size ctx) e )
        (*~pp_ret:(Pretty.binary_pretty ~size ctx)*)
        (fun () ->
          apply_all ~size (fun ~size ctx a b -> Sub.Binary_Forward.bindex ~size k ctx a b) ctx x e)

      let bisub ~(size:In_bits.t) ~flags ctx x y =
        Log.trace (fun p -> p "bisub ~size:%d %a %a" (size:>int) (Pretty.binary_pretty ~size ctx) x (Pretty.binary_pretty ~size ctx) y )
        @@ fun () ->
        apply_all ~size (fun ~size ctx a b -> Sub.Binary_Forward.bisub ~size ~flags ctx a b) ctx x y

      let rec binary_of_value ~size ctx v =
        let open TypedC in
        match v with
        | Sym s -> snd @@ global_symbol ctx s

      and cond_of_cmp ~size ctx cmpop v1 v2 =
        let open TypedC.Pred in
        match cmpop with
        | Equal ->
            beq ~size ctx v1 v2
        | NotEqual ->
            Boolean_Forward.not ctx @@ beq ~size ctx v1 v2
        | ULt ->
            Boolean_Forward.not ctx @@ biule ~size ctx v2 v1
        | SLt ->
            Boolean_Forward.not ctx @@ bisle ~size ctx v2 v1
        | ULeq ->
            biule ~size ctx v1 v2
        | SLeq ->
            bisle ~size ctx v1 v2
        | UGeq ->
            biule ~size ctx v2 v1
        | SGeq ->
            bisle ~size ctx v2 v1
        | UGt ->
            Boolean_Forward.not ctx @@ biule ~size ctx v1 v2
        | SGt ->
            Boolean_Forward.not ctx @@ bisle ~size ctx v1 v2

      and lift_unop ~size ctx op =
        let open TypedC.Pred in
        match op with
        | Extract (index,len) -> fun x ->
          bextract ~size:len ~index ~oldsize:size ctx x
          |> buext ~size ~oldsize:len ctx

      and lift_binop ~size ctx op =
        let open TypedC.Pred in
        match op with
        | Add -> biadd ~size ctx ~flags:(Operator.Flags.Biadd.pack ~nsw:false ~nuw:false ~nusw:false)
        | Sub -> bisub ~size ctx ~flags:(Operator.Flags.Bisub.pack ~nsw:false ~nuw:false ~nusw:false)
        | Mul -> bimul ~size ctx ~flags:(Operator.Flags.Bimul.pack ~nsw:false ~nuw:false)
        | Div -> bisdiv ~size ctx
        | And -> band ~size ctx
        | Or -> bor ~size ctx
        | Concat (size1,size2) -> bconcat ~size1 ~size2 ctx
        | Mod -> bismod ~size ctx

      and binary_of_expr ~size ctx ~self e =
        let open TypedC.Pred in
        match e with
        | Const x -> biconst ~size x ctx
        | Self -> self
        | Val v -> binary_of_value ~size ctx v
        | Unop (op, e) -> lift_unop ~size ctx op (binary_of_expr ~size ctx ~self e)
        | Binop (op, e1, e2) -> lift_binop ~size ctx op
            (binary_of_expr ~size ctx ~self e1) (binary_of_expr ~size ctx ~self e2)

      and cond_of_pred ~size ctx pred ~self =
        let open TypedC.Pred in
        match pred with
        | True -> Boolean_Forward.true_ ctx
        | Cmp (op, e1, e2) ->
            (* let size = cmp_size ~size ctx e1 e2 in *)
            cond_of_cmp ~size ctx op (binary_of_expr ~size ctx ~self e1)
            (binary_of_expr ~size ctx ~self e2)
            (* let cond1 = binary_of_expr ~size ctx ~self e1 in
            let cond2 = binary_of_expr ~size ctx ~self e2 in
            cond_of_cmp ~size ctx op cond1 cond2 *)
        | And (p,q) ->
            let c1 = cond_of_pred ~size ctx p ~self in
            let c2 = cond_of_pred ~size ctx q ~self in
            Boolean_Forward.(&&) ctx c1 c2
        |Mutval (mut,p) -> cond_of_pred ~size ctx (evaluate_mutval mut p) ~self

      and check_invariant_complex ~(size:In_bits.t) ctx typ value =
        let open TypedC in
        let typ = inlined typ in
        match typ.descr with
        | Structure {st_byte_size; st_members} ->
            let folder res (ofs,_,typ) =
              let sz = TypedC.sizeof typ |> In_bytes.in_bits in
              let index = In_bytes.in_bits ofs in
              Boolean_Forward.(&&) ctx res @@
                check_invariant_complex ~size:sz ctx typ @@
                  bextract ~size:sz ~index ~oldsize:size ctx value
            in
            List.fold_left folder (Boolean_Forward.true_ ctx) st_members

        | _ -> cond_of_pred ~size ctx typ.pred ~self:value

      let discriminated ~size ctx binary =
        match binary with
        | Union (d,ls) -> (
            let folder (case,lst) (value, cond) =
              let b1 = Scalar.query_boolean ctx @@ Scalar.Enum_Forward.caseof ~case ctx d in
              let b2 = Query.boolean ctx cond in
              match b1,b2 with
              | Lattices.Quadrivalent.(False | Bottom), _
              | _, Lattices.Quadrivalent.(False | Bottom) -> (case + 1, lst)
              | _ ->  (case + 1, (value,cond):: lst)
            in
            let ncase,ls = List.fold_left folder (0,[]) ls in
            let ls = List.rev ls in
            match ls with
            | [] -> binary_empty ~size ctx
            | [(value,_)] -> value
            | cases -> Union (d,ls)
          )
        | _ -> binary

      let evaluate_cases ~size ctx discrim cases =
        let folder (case,lst) (value,cond) =
          let b1 = Scalar.query_boolean ctx @@ Scalar.Enum_Forward.caseof ~case ctx discrim in
          let b2 = Query.boolean ctx cond in
          match b1,b2 with
          | Lattices.Quadrivalent.(False | Bottom), _
          | _, Lattices.Quadrivalent.(False | Bottom) -> (case + 1, (binary_empty ~size ctx, cond) :: lst)
          | _ -> (case + 1, (value,cond) :: lst)
        in
        let ncase,ls = List.fold_left folder (0,[]) cases in List.rev ls

      (* let valid ~size _acc_type ctx ptr = Sub.Binary_Forward.valid ~size _acc_type ctx (force ctx ptr) *)
      let valid ~size _acc_type ctx ptr =
        Log.trace (fun p -> p "valid %a" (Pretty.binary_pretty ~size ctx) ptr)
        (*~pp_ret:(Pretty.boolean_pretty ctx)*)
        (fun () -> (BSimple (BValid{size;arg=ptr})))

      let valid_ptr_arith ~size arith_typ ctx x y =
        Log.trace (fun p -> p "valid_ptr_arith %a %a" (Pretty.binary_pretty ~size ctx) x (Pretty.binary_pretty ~size ctx) y )
      @@ fun () ->
        let op = Operator.(match arith_typ with Plus -> PLUS | Minus -> MINUS) in
        (BSimple (BValidArith{size;op;left=x;right=y}))

      let rec binpred_to_operation ~size ctx = function
        | B_EQ -> Binary_Forward.beq ~size ctx
        | B_SLE -> Binary_Forward.bisle ~size ctx
        | B_ULE -> Binary_Forward.biule ~size ctx
        | not_op -> fun l r -> Boolean_Forward.not ctx
            (binpred_to_operation ~size ctx (not_binpred not_op) l r)

    end

    and Assume : ASSUME_TYPE = struct

      (* Assume functions *)
      let join_context =
        option_merge (fun ctxa ctxb -> fst @@ Scalar.typed_nondet2 ctxa ctxb @@ Context.empty_tuple ())

      let rec joined_assume_cases ~case initctx pred discrim cases =
        match cases with
        | [] -> None
        | (value, condition) :: tl ->
          let case_cond = Scalar.Enum_Forward.caseof ~case initctx discrim in
          let ctxopt = Scalar.assume initctx case_cond in
          let ctxopt = Option.bind ctxopt (fun ctx -> assume_tree ctx (pred value)) in
          let ctxopt = Option.bind ctxopt (fun ctx -> assume_tree ctx condition) in
          join_context ctxopt (joined_assume_cases ~case:(case + 1) initctx pred discrim tl)

      and assume_cases ctx pred discrim cases =
        match cases with
        | [] -> assert false
        | _ -> joined_assume_cases ~case:0 ctx pred discrim cases

      and assume_simple ctx = function
        | BVal v -> Sub.assume ctx v
        | BValid{size; arg = Union (discrim,cases)} ->
            assume_cases ctx (Binary_Forward.valid ~size Operator.Read ctx) discrim cases

        | BBinpred {pred; size; left = Union (discrim,cases); right} ->
            assume_cases ctx (fun x -> Binary_Forward.binpred_to_operation ~size ctx pred x right) discrim cases
        | BBinpred {pred; size; left; right = Union (discrim,cases)} ->
            assume_cases ctx (Binary_Forward.binpred_to_operation ~size ctx pred left) discrim cases

        | BValidArith {size; op; left = Union (discrim,cases); right} ->
            assume_cases ctx (fun x -> Binary_Forward.valid_ptr_arith ~size (arith_to_type op) ctx x right) discrim cases
        | BValidArith {size; op; left; right = Union (discrim,cases)} ->
            assume_cases ctx (fun x -> Binary_Forward.valid_ptr_arith ~size (arith_to_type op) ctx left x) discrim cases

        | BCaseOf {arg; case} -> Scalar.assume ctx @@ Scalar.Enum_Forward.caseof ~case ctx arg

        | x -> Sub.assume ctx @@ Conversion.forceb_simple ctx x

      and assume_tree ctx btree =
        match btree with
        | BSimple b -> assume_simple ctx b
        | BAnd (b1, b2) ->
            let ctxopt = assume_tree ctx b1 in
            Option.bind ctxopt (fun ctx -> assume_tree ctx b2)
        | BOr (b1, b2) -> Sub.assume ctx (Conversion.forceb ctx btree)

    end

    include Pretty

    let discriminated = Binary_Forward.discriminated
    let evaluate_cases = Binary_Forward.evaluate_cases

    let assume ctx cond =
      Log.trace (fun p -> p "assume %a" (boolean_pretty ctx) cond )
      (fun () -> Assume.assume_tree ctx cond)

    let imperative_assume ctx cond =
      let module Ext = Extend.MakeForAADT(Sub) in
      Ext.imperative_assume ctx @@ Conversion.forceb ctx cond
    let satisfiable ctx b = Sub.satisfiable ctx @@ Conversion.forceb ctx b

    (* Extract the global and invariants from a type and returns a "local" type (with predicates only on self) and a "global" invariant *)

    let rec is_expression_local expr =
      let open TypedC.Pred in
      match expr with
      | Const _ -> false        (* XXX: The analysis fails on bisort if we put true, that seems more correct.  *)
      | Self -> true
      | Val _ -> false
      | Binop(_, e1, e2) -> is_expression_local e1 || is_expression_local e2
      | Unop(_, e) -> is_expression_local e

    let rec split_predicate pred =
      let open TypedC.Pred in
      match pred with
      | True -> true_ , true_
      | Cmp(op, e1, e2) ->
        if is_expression_local e1 || is_expression_local e2 then pred, true_ else true_, pred
      | And(p1, p2) ->
          let localpred1, globalpred1 = split_predicate p1 in
          let localpred2, globalpred2 = split_predicate p2 in
          let conjunction = Types.TypedC.Pred.conjunction in
          conjunction localpred1 localpred2,
          conjunction globalpred1 globalpred2
      | Mutval (mut,p) -> let p1,p2 = split_predicate p in Mutval(mut,p1),Mutval(mut,p2)

    let rec split_type typ =
      let open TypedC in
      let folder (ofs,name,typ) (members, gpred) =
        let typ, pred = split_type typ in
        (ofs,name,typ) :: members, Pred.conjunction gpred pred
      in
      let localpred,globalpred = split_predicate typ.pred in
      let typ = inlined typ in
      match typ.descr with
      | Structure ({st_members; _} as s) ->
        let st_members, pred = List.fold_right folder st_members ([],globalpred) in
        {descr = Structure {s with st_members}; pred = localpred}, pred

      | _ -> {typ with pred = localpred}, globalpred


    let binary_of_expr = Binary_Forward.binary_of_expr
    let cond_of_pred = Binary_Forward.cond_of_pred

    let imperative_use_invariant ~size ctx pred value =
      let open TypedC.Pred in
        match pred with
        | Cmp(Equal, Self, e)
        | Cmp(Equal, e, Self) -> binary_of_expr ~size ctx ~self:value e
        | And(p1, p2) ->
          let cond1 = (cond_of_pred ~size ctx p1 ~self:value) in
          imperative_assume ctx cond1 ;
          let cond2 = (cond_of_pred ~size ctx p2 ~self:value) in
          imperative_assume ctx cond2 ;
          value
        | pred when is_true pred -> value
        | _ -> imperative_assume ctx (cond_of_pred ~size ctx pred ~self:value) ; value

    (* Serialize functions for boolean constraints and binary constructs *)

    let counter = ref 0 ;;

    (* Serialize operation for joining two boolean constraints *)
    let rec serialize_boolean_simple
      : 'a. Context.t -> boolean_simple -> Context.t -> boolean_simple -> (bool * 'a Context.in_tuple) -> (boolean_simple, 'a) Context.result
      = fun ctxa a ctxb b ((inc,intup) as acc) ->
      Log.trace (fun p -> p "serialize_boolean_simple %a %a" (boolean_simple_pretty ctxa) a (boolean_simple_pretty ctxb) b )
      @@ fun () ->
      match a,b with
      | BVal va, BVal vb ->
          let Sub.Context.Result(inc,intup,des) = Sub.serialize_boolean ctxa va ctxb vb acc in
          Context.Result(inc, intup, fun ctx out -> let v,out = des ctx out in BVal v,out)
      | BValid {size=sza; arg=va}, BValid {size=szb; arg=vb} when sza = szb ->
          let Context.Result (inc, intup, des) = serialize ~widens:false ~size:sza ctxa va ctxb vb acc in
          Context.Result(inc, intup, fun ctx out -> let v,out = des ctx out in BValid {size=sza; arg=v}, out)
      | BBinpred {pred=pa;size=sza;left=la;right=ra}, BBinpred {pred=pb;size=szb;left=lb;right=rb}
          when sza = szb && pa = pb
        ->
          let Context.Result (inc, intup, desl) = serialize ~widens:false ~size:sza ctxa la ctxb lb acc in
          let Context.Result (inc, intup, desr) = serialize ~widens:false ~size:szb ctxa ra ctxb rb (inc,intup) in
          Context.Result(inc, intup, fun ctx out ->
            let right,out = desr ctx out in
            let left,out = desl ctx out in
            BBinpred {pred=pa; size=sza; left; right}, out
          )

      | BValidArith {size=sza;op=pa;left=la;right=ra}, BValidArith {size=szb;op=pb;left=lb;right=rb}
          when sza = szb && pa = pb
        ->
          let Context.Result (inc, intup, desl) = serialize ~widens:false ~size:sza ctxa la ctxb lb acc in
          let Context.Result (inc, intup, desr) = serialize ~widens:false ~size:szb ctxa ra ctxb rb (inc,intup) in
          Context.Result(inc, intup, fun ctx out ->
            let r,out = desr ctx out in
            let l,out = desl ctx out in
            BValidArith {size=sza;op=pa;left=l;right=r},out
          )

      | BCaseOf {arg=ea;case=ca}, BCaseOf {arg=eb;case=cb} when ca = cb ->
          let Context.Result (inc, intup, des) = Scalar.serialize_enum ctxa ea ctxb eb acc in
          Context.Result (inc, intup, fun ctx out ->
            let arg,out = des ctx out in
            BCaseOf {arg; case=ca}, out
          )


      | _ -> Context.Result(inc, intup, fun ctx out -> BVal (Sub.boolean_unknown ctx), out)

    and serialize_boolean
      : 'a. Context.t -> boolean -> Context.t -> boolean -> (bool * 'a Context.in_tuple) -> (boolean, 'a) Context.result
      = fun ctxa a ctxb b (inc,intup) ->
      incr counter ;
      let num = !counter in
      Log.debug (fun p -> p "VUC.serialize_boolean no.%d, %a %a" num (boolean_pretty ctxa) a (boolean_pretty ctxb) b );
      match a,b with
      | BSimple ba, BSimple bb ->
        let Context.Result(inc,intup,des) = serialize_boolean_simple ctxa ba ctxb bb (inc,intup) in
        Context.Result (inc, intup, fun ctx out ->
          let b,out = des ctx out in
          let res,out = BSimple b, out in
          Log.debug (fun p -> p "VUC.serialize_boolean no.%d, returning %a" num (boolean_pretty ctx) res );
          res, out
        )

      | BOr (la,ra), BOr (lb,rb) ->
        let Context.Result(inc, intup, desl) = serialize_boolean ctxa la ctxb lb (inc,intup) in
        let Context.Result(inc, intup, desr) = serialize_boolean ctxa ra ctxb rb (inc,intup) in
        Context.Result (inc, intup, fun ctx out ->
          let r,out = desr ctx out in
          let l,out = desl ctx out in
          BOr (l,r), out
        )
      | BAnd (la,ra), BAnd (lb,rb) ->
        let Context.Result(inc,intup,desl) = serialize_boolean ctxa la ctxb lb (inc,intup) in
        let Context.Result(inc,intup,desr) = serialize_boolean ctxa ra ctxb rb (inc,intup) in
        Context.Result (inc, intup, fun ctx out ->
          let r,out = desr ctx out in
          let l,out = desl ctx out in
          BAnd (l,r), out
        )

      | _ -> Context.Result(inc, intup, fun ctx out -> boolean_unknown ctx, out)

      (* Serialize operation for joining a boolean constraints and bottom *)
      and serialize_boolean_simple_with_bottom_right
      : 'a. Context.t -> Context.t -> boolean_simple -> (bool * 'a Context.in_tuple) -> (boolean_simple, 'a) Context.result
      = fun ctxa ctxdummy b ((inc,intup) as acc) ->
      Log.debug (fun p -> p "VUC.serialize_boolean_simple_with_bottom_right" );
      match b with
      | BVal v ->
          let Sub.Context.Result(inc,intup,des) = Sub.serialize_boolean ctxa v ctxdummy (Sub.boolean_empty ctxdummy) acc in
          Context.Result(inc, intup, fun ctx out -> let v,out = des ctx out in BVal v,out)
      | BValid {size; arg} ->
          let Context.Result (inc, intup, des) = serialize ~widens:false ~size ctxa arg ctxdummy (binary_empty ~size ctxdummy) acc in
          Context.Result(inc, intup, fun ctx out -> let arg,out = des ctx out in BValid {size; arg}, out)
      | BBinpred {pred;size;left;right} ->
          let Context.Result (inc, intup, desl) = serialize ~widens:false ~size ctxa left ctxdummy (binary_empty ~size ctxdummy) acc in
          let Context.Result (inc, intup, desr) = serialize ~widens:false ~size ctxa right ctxdummy (binary_empty ~size ctxdummy) (inc,intup) in
          Context.Result(inc, intup, fun ctx out ->
            let right,out = desr ctx out in
            let left,out = desl ctx out in
            BBinpred {pred; size; left; right}, out
          )
      | BValidArith {size;op;left;right} ->
          let Context.Result (inc, intup, desl) = serialize ~widens:false ~size ctxa left ctxdummy (binary_empty ~size ctxdummy) acc in
          let Context.Result (inc, intup, desr) = serialize ~widens:false ~size ctxa right ctxdummy (binary_empty ~size ctxdummy) (inc,intup) in
          Context.Result(inc, intup, fun ctx out ->
            let right,out = desr ctx out in
            let left,out = desl ctx out in
            BValidArith {size;op;left;right}, out
          )
      | BCaseOf {arg;case} ->
        let Context.Result (inc, tup, des) = Scalar.serialize_enum ctxa arg ctxdummy (Scalar.enum_empty ctxdummy) acc in
        Context.Result (inc, tup, fun ctx out ->
          let arg,out = des ctx out in
          BCaseOf {arg;case}, out
        )

    and serialize_boolean_with_bottom_right
      : 'a. Context.t -> Context.t -> boolean -> (bool * 'a Context.in_tuple) -> (boolean, 'a) Context.result
      = fun ctxa ctxdummy b (inc,intup) ->
      Log.debug (fun p -> p "VUC.serialize_boolean_with_bottom_right" );
      match b with
      | BSimple s ->
        let Context.Result(inc,intup,des) = serialize_boolean_simple_with_bottom_right ctxa ctxdummy s (inc,intup) in
        Context.Result (inc, intup, fun ctx out -> let s,out = des ctx out in BSimple s, out)
      | BOr (left,right) ->
        let Context.Result(inc, intup, desl) = serialize_boolean_with_bottom_right ctxa ctxdummy left (inc,intup) in
        let Context.Result(inc, intup, desr) = serialize_boolean_with_bottom_right ctxa ctxdummy right (inc,intup) in
        Context.Result (inc, intup, fun ctx out ->
          let right,out = desr ctx out in
          let left,out = desl ctx out in
          BOr (left,right), out
        )
      | BAnd (left,right) ->
        let Context.Result(inc,intup,desl) = serialize_boolean_with_bottom_right ctxa ctxdummy left (inc,intup) in
        let Context.Result(inc,intup,desr) = serialize_boolean_with_bottom_right ctxa ctxdummy right (inc,intup) in
        Context.Result (inc, intup, fun ctx out ->
          let right,out = desr ctx out in
          let left,out = desl ctx out in
          BAnd (left,right), out
        )

    (* Serialize operation for joining bottom and a boolean constraints *)
    and serialize_boolean_simple_with_bottom_left
    : 'a. Context.t -> Context.t -> boolean_simple -> (bool * 'a Context.in_tuple) -> (boolean_simple, 'a) Context.result
    = fun ctxdummy ctxb b ((inc,intup) as acc) ->
    Log.debug (fun p -> p "VUC.serialize_boolean_simple_with_bottom_left" );
    match b with
    | BVal v ->
        let Sub.Context.Result(inc,intup,des) = Sub.serialize_boolean ctxdummy (Sub.boolean_empty ctxdummy) ctxb v acc in
        Context.Result(inc, intup, fun ctx out -> let v,out = des ctx out in BVal v,out)
    | BValid {size; arg} ->
        let Context.Result (inc, intup, des) = serialize ~widens:false ~size ctxdummy (binary_empty ~size ctxdummy) ctxb arg acc in
        Context.Result(inc, intup, fun ctx out -> let arg,out = des ctx out in BValid {size; arg}, out)
    | BBinpred {pred;size;left;right} ->
        let Context.Result (inc, intup, desl) = serialize ~widens:false ~size ctxdummy (binary_empty ~size ctxdummy) ctxb left acc in
        let Context.Result (inc, intup, desr) = serialize ~widens:false ~size ctxdummy (binary_empty ~size ctxdummy) ctxb right (inc,intup) in
        Context.Result(inc, intup, fun ctx out ->
          let right,out = desr ctx out in
          let left,out = desl ctx out in
          BBinpred {pred; size; left; right}, out
        )
    | BValidArith {size;op;left;right} ->
        let Context.Result (inc, intup, desl) = serialize ~widens:false ~size ctxdummy (binary_empty ~size ctxdummy) ctxb left acc in
        let Context.Result (inc, intup, desr) = serialize ~widens:false ~size ctxdummy (binary_empty ~size ctxdummy) ctxb right (inc,intup) in
        Context.Result(inc, intup, fun ctx out ->
          let right,out = desr ctx out in
          let left,out = desl ctx out in
          BValidArith {size;op;left;right}, out
        )
    | BCaseOf {arg;case} ->
        let Context.Result (inc, tup, des) = Scalar.serialize_enum ctxdummy (Scalar.enum_empty ctxdummy) ctxb arg acc in
        Context.Result (inc, tup, fun ctx out ->
          let arg,out = des ctx out in
          BCaseOf {arg;case}, out
        )

  and serialize_boolean_with_bottom_left
    : 'a. Context.t -> Context.t -> boolean -> (bool * 'a Context.in_tuple) -> (boolean, 'a) Context.result
    = fun ctxdummy ctxb b (inc,intup) ->
    Log.debug (fun p -> p "VUC.serialize_boolean_with_bottom_left" );
    match b with
    | BSimple s ->
      let Context.Result(inc,intup,des) = serialize_boolean_simple_with_bottom_left ctxdummy ctxb s (inc,intup) in
      Context.Result (inc, intup, fun ctx out -> let s,out = des ctx out in BSimple s, out)
    | BOr (left,right) ->
      let Context.Result(inc, intup, desl) = serialize_boolean_with_bottom_left ctxdummy ctxb left (inc,intup) in
      let Context.Result(inc, intup, desr) = serialize_boolean_with_bottom_left ctxdummy ctxb right (inc,intup) in
      Context.Result (inc, intup, fun ctx out ->
        let right,out = desr ctx out in
        let left,out = desl ctx out in
        BOr (left,right), out
      )
    | BAnd (left,right) ->
      let Context.Result(inc,intup,desl) = serialize_boolean_with_bottom_left ctxdummy ctxb left (inc,intup) in
      let Context.Result(inc,intup,desr) = serialize_boolean_with_bottom_left ctxdummy ctxb right (inc,intup) in
      Context.Result (inc, intup, fun ctx out ->
        let right,out = desr ctx out in
        let left,out = desl ctx out in
        BAnd (left,right), out
      )

    (* Serialize operation for joining two fixed size concatenation constructs *)
    and serialize_concat
      : 'a. widens:bool -> Context.t -> (In_bits.t * binary) list ->
          Context.t -> (In_bits.t * binary) list -> ('a Context.in_acc) -> ((In_bits.t * binary) list, 'a) Context.result
      = fun ~widens ctxa la ctxb lb (included,in_tup) ->
      Log.debug (fun p -> p "VUC.serialize_struct" );
      match la,lb with
      | [],[] -> Context.Result(included, in_tup, fun ctx out_tup -> [], out_tup)
      | [], _ | _, [] -> assert false (* Should not happen as long as structures are the the same size *)
      | (sza,va) :: tla, (szb,vb) :: tlb when sza = szb ->
        begin
          let Context.Result (included, in_tup, d_struct) = serialize_concat ~widens ctxa tla ctxb tlb (included, in_tup) in
          let Context.Result (included, in_tup, deserialize) = serialize ~widens ~size:sza ctxa va ctxb vb (included, in_tup) in
          Context.Result(included, in_tup, fun ctx out_tup -> (
            let v, out_tup = deserialize ctx out_tup in
            let tl, out_tup = d_struct ctx out_tup in
            (sza, v) :: tl , out_tup
          ))
        end

      | (sza,va) :: tla, (szb,vb) :: tlb when sza < szb ->
        begin
          let part_b = Binary_Forward.bextract ~size:sza ~index:In_bits.zero ~oldsize:szb ctxb vb in
          let rest_b = Binary_Forward.bextract ~size:In_bits.(szb-sza) ~index:sza ~oldsize:szb ctxb vb in
          let Context.Result (included, in_tup, d_struct) = serialize_concat ~widens ctxa tla ctxb ((In_bits.(szb-sza), rest_b) :: tlb) (included, in_tup) in
          let Context.Result (included, in_tup, deserialize) = serialize ~widens ~size:sza ctxa va ctxb part_b (included, in_tup) in
          Context.Result(included, in_tup, fun ctx out_tup -> (
            let v, out_tup = deserialize ctx out_tup in
            let tl, out_tup = d_struct ctx out_tup in
            (sza, v) :: tl , out_tup
          ))
        end
      | (sza,va) :: tla, (szb,vb) :: tlb ->
        begin
          let part_a = Binary_Forward.bextract ~size:szb ~index:In_bits.zero ~oldsize:sza ctxa va in
          let rest_a = Binary_Forward.bextract ~size:In_bits.(sza-szb) ~index:szb ~oldsize:sza ctxa va in
          let Context.Result (included, in_tup, d_struct) = serialize_concat ~widens ctxa ((In_bits.(sza-szb), rest_a) :: tla) ctxb tlb (included, in_tup) in
          let Context.Result (included, in_tup, deserialize) = serialize ~widens ~size:szb ctxa part_a ctxb vb (included, in_tup) in
          Context.Result(included, in_tup, fun ctx out_tup -> (
            let v, out_tup = deserialize ctx out_tup in
            let tl, out_tup = d_struct ctx out_tup in
            (szb, v) :: tl , out_tup
          ))
        end

    and serialize_concat_with_right
      : 'a. widens:bool -> index:In_bits.t -> oldsize:In_bits.t -> Context.t -> (In_bits.t * binary) list ->
          Context.t -> binary -> ('a Context.in_acc) -> ((In_bits.t * binary) list, 'a) Context.result
      = fun ~widens ~index ~oldsize ctxa la ctxb b (inc,tup) ->
      Log.debug (fun p -> p "VUC.serialize_struct_with_right" );
      match la with
      | [] -> Context.Result(inc, tup, fun ctx out_tup -> [], out_tup)
      | (sz,va) :: tla ->
        begin
          let part_b = Binary_Forward.bextract ~size:sz ~index ~oldsize ctxb b in
          let Context.Result (inc, tup, d_struct) = serialize_concat_with_right ~widens ~index:In_bits.(index+sz) ~oldsize ctxa tla ctxb b (inc,tup) in
          let Context.Result (inc, tup, deserialize) = serialize ~widens ~size:sz ctxa va ctxb part_b (inc, tup) in
          Context.Result(inc, tup, fun ctx out_tup -> (
            let v, out_tup = deserialize ctx out_tup in
            let tl, out_tup = d_struct ctx out_tup in
            (sz, v) :: tl , out_tup
          ))
        end

    and serialize_concat_with_left
      : 'a. widens:bool -> index:In_bits.t -> oldsize:In_bits.t -> Context.t -> binary ->
          Context.t -> (In_bits.t * binary) list -> ('a Context.in_acc) -> ((In_bits.t * binary) list, 'a) Context.result
      = fun ~widens ~index ~oldsize ctxa a ctxb lb (inc,tup) ->
      Log.debug (fun p -> p "VUC.serialize_struct_with_left" );
      match lb with
      | [] -> Context.Result(inc, tup, fun ctx out_tup -> [], out_tup)
      | (sz,vb) :: tlb ->
        begin
          let part_a = Binary_Forward.bextract ~size:sz ~index ~oldsize ctxa a in
          let Context.Result (inc, tup, d_struct) = serialize_concat_with_left ~widens ~index:In_bits.(index+sz) ~oldsize ctxa a ctxb tlb (inc,tup) in
          let Context.Result (inc, tup, deserialize) = serialize ~widens ~size:sz ctxa part_a ctxb vb (inc, tup) in
          Context.Result(inc, tup, fun ctx out_tup -> (
            let v, out_tup = deserialize ctx out_tup in
            let tl, out_tup = d_struct ctx out_tup in
            (sz, v) :: tl , out_tup
          ))
        end

    (* Serialize operation for joining two union constructs (when they use the same case identifier variables) *)
    and serialize_union
      : 'a. widens:bool -> size:In_bits.t -> Context.t -> (binary * boolean) list -> Context.t -> (binary * boolean) list ->
              'a Context.in_acc -> ((binary * boolean) list option, 'a) Context.result
      = fun ~widens ~size ctxa la ctxb lb (included,in_tup) ->
      Log.debug (fun p -> p "VUC.serialize_union" );
      match la,lb with
      | [],[] -> Context.Result(included, in_tup, fun ctx out_tup -> Some [], out_tup)

      | (va,ca) :: tla, (vb,cb) :: tlb ->
        begin
          let Context.Result (inc, tup, deserialize_union) = serialize_union ~widens ~size ctxa tla ctxb tlb (included, in_tup) in
          let Context.Result (inc, tup, deserialize_value) = serialize ~widens ~size ctxa va ctxb vb (inc,tup) in
          let Context.Result (inc, tup, deserialize_cond) = serialize_boolean ctxa ca ctxb cb (inc,tup) in
          Context.Result(included, tup, fun ctx out_tup -> (
            let cond, out_tup = deserialize_cond ctx out_tup in
            let value, out_tup = deserialize_value ctx out_tup in
            let list, out_tup = deserialize_union ctx out_tup in
            Option.map (fun ls -> (value, cond) :: ls) list, out_tup
          ))
        end

      | _ -> Context.Result(included, in_tup, fun ctx out_tup -> None, out_tup)

    (* Serialize operation for joining two union constructs by first doing a cross product of all cases (which is rather inefficient) *)
    and serialize_union_cases
      : 'a. widens:bool -> size:In_bits.t -> Context.t -> Context.t -> ((binary * boolean) * (binary * boolean)) list ->  'a Context.in_acc -> ((binary * boolean) list, 'a) Context.result
      = fun ~widens ~size ctxa ctxb cases (inc,tup) ->
        Log.debug (fun p -> p "VUC.serialize_union_cases" );
        match cases with
        | [] -> Context.Result (inc, tup, fun ctx out -> [], out)
        | ((va,ca), (vb,cb)) :: tl ->
          let Context.Result (inc, tup, deserialize_union) = serialize_union_cases ~widens ~size ctxa ctxb tl (inc,tup) in
          let Context.Result (inc, tup, deserialize_value) = serialize ~widens ~size ctxa va ctxb vb (inc,tup) in
          let Context.Result (inc, tup, deserialize_cond) = serialize_boolean ctxa ca ctxb cb (inc,tup) in
          Context.Result (inc, tup, fun ctx out ->
            let cond, out = deserialize_cond ctx out in
            let value, out = deserialize_value ctx out in
            let tl, out = deserialize_union ctx out in
            (value, cond) :: tl, out
          )

    (* Serialize operation between an union construct and non union constructs (a value) *)
    and serialize_union_with_right
      : 'a. widens:bool -> size:In_bits.t -> Context.t -> (binary * boolean) list ->
            Context.t -> binary -> 'a Context.in_acc -> ((binary * boolean) list, 'a) Context.result
      = fun ~widens ~size ctxa lst ctxb vb (inc,tup) ->
      Log.debug (fun p -> p "VUC.serialize_union_with_right" );
      match lst with
      | [] -> Context.Result(inc, tup, fun ctx out_tup -> [], out_tup)
      | (value, cond) :: tl ->
        let Context.Result (inc, tup, deserialize_union) = serialize_union_with_right ~widens ~size ctxa tl ctxb vb (inc,tup) in
        let Context.Result (inc, tup, deserialize_value) = serialize ~widens ~size ctxa value ctxb vb (inc,tup) in
        let Context.Result (inc, tup, deserialize_cond) = serialize_boolean_with_bottom_right ctxa ctxb cond (inc,tup) in
        Context.Result (inc, tup, fun ctx out ->
          let cond, out = deserialize_cond ctx out in
          let value, out = deserialize_value ctx out in
          let tl, out = deserialize_union ctx out in
          (value, cond) :: tl, out
        )

    (* Serialize operation between a non union constructs (a value) and an union construct *)
    and serialize_union_with_left
      : 'a. widens:bool -> size:In_bits.t -> Context.t -> binary -> Context.t ->
            (binary * boolean) list -> 'a Context.in_acc -> ((binary * boolean) list, 'a) Context.result
      = fun ~widens ~size ctxa va ctxb lst (inc,tup) ->
      Log.debug (fun p -> p "VUC.serialize_union_with_left" );
      match lst with
      | [] -> Context.Result(inc, tup, fun ctx out_tup -> [], out_tup)
      | (value, cond) :: tl ->
        let Context.Result (inc, tup, deserialize_union) = serialize_union_with_left ~widens ~size ctxa va ctxb tl (inc,tup) in
        let Context.Result (inc, tup, deserialize_value) = serialize ~widens ~size ctxa va ctxb value (inc,tup) in
        let Context.Result (inc, tup, deserialize_cond) = serialize_boolean_with_bottom_left ctxa ctxb cond (inc,tup) in
        Context.Result (inc, tup, fun ctx out ->
          let cond, out = deserialize_cond ctx out in
          let value, out = deserialize_value ctx out in
          let tl, out = deserialize_union ctx out in
          (value, cond) :: tl, out
        )

    (* Serialize operation between two binary constructs *)
    and serialize
      : 'a. widens:bool -> size:In_bits.t -> Context.t -> binary -> Context.t -> binary -> ' a Context.in_acc -> (binary, 'a) Context.result
      = fun ~widens ~size ctxa a ctxb b in_tup ->
      incr counter ;
      let num = !counter in
      Log.debug (fun p -> p "VUC.serialize no.%d %a %a" num (binary_pretty ~size ctxa) a (binary_pretty ~size ctxb) b );
      (* let a = discriminated ~size ctxa a in
      let b = discriminated ~size ctxb b in *)
      match a,b with
      | Val va, Val vb ->
        let Context.Result(included, acc, d_single) = Sub.serialize ~widens ~size ctxa va ctxb vb in_tup in
        Context.Result(included, acc, fun ctx out ->
          let v,out = d_single ctx out in
          let res, out = Val v, out in
          (* Codex_log.debug "VUC.serialize no.%d, returning %a" num (binary_pretty ~size ctx) res ; *)
          res, out)

      | Concat la, Concat lb ->
        begin
          let Context.Result(included, acc, d_struct) = serialize_concat ~widens ctxa la ctxb lb in_tup in
          Context.Result(included, acc, fun ctx out ->
            match d_struct ctx out with
            | [(_,v)], out -> v, out
            | lst, out -> Concat lst, out
          )
        end

      | Concat la, _
          when Query.(Binary_Lattice.is_empty ~size @@ binary ~size ctxb b)
        ->
          begin
            let Context.Result(included, acc, d_struct) = serialize_concat_with_right ~widens ~index:In_bits.zero ~oldsize:size ctxa la ctxb b in_tup in
            Context.Result(included, acc, fun ctx out ->
              let lst, out = d_struct ctx out in
              Concat lst, out
            )
          end

      | _, Concat lb
          when Query.(Binary_Lattice.is_empty ~size @@ binary ~size ctxa a)
        ->
          begin
            let Context.Result(included, acc, d_struct) = serialize_concat_with_left ~widens ~index:In_bits.zero ~oldsize:size ctxa a ctxb lb in_tup in
            Context.Result(included, acc, fun ctx out ->
              let lst, out = d_struct ctx out in
              Concat lst, out
            )
          end

      | BofBool (sza,_,ba), BofBool (szb,_,bb) when sza = szb ->
        let Context.Result(included, in_tup, d_boolean) = serialize_boolean ctxa ba ctxb bb in_tup in
        Context.Result(included, in_tup, fun ctx out ->
          let b,out = d_boolean ctx out in
          let v = Sub.Binary_Forward.bofbool ~size ctx @@ Conversion.forceb ctx b in
          BofBool (sza, v, b), out)

      (* TODO : use the criteron otherwise it could be unsound *)
      | Union (da,lsa), Union (db,lsb) when List.length lsa = List.length lsb (* da = db *) ->
        let lsa = evaluate_cases ~size ctxa da lsa in
        let lsb = evaluate_cases ~size ctxb db lsb in
        let Context.Result (inc, tup, deserialize_discrim) = Scalar.serialize_enum ctxa da ctxb db in_tup in
        let Context.Result (inc, tup, deserialize_union) = serialize_union ~widens ~size ctxa lsa ctxb lsb (inc,tup) in
        Context.Result (inc, tup, fun ctx out ->
          let cases, out = deserialize_union ctx out in
          let d, out = deserialize_discrim ctx out in
          match cases with
          | None -> assert false
          | Some lst -> Union (d,lst), out
        )

      | Union (da,lsa), _ ->
        let lsa = evaluate_cases ~size ctxa da lsa in
        let vb = Conversion.force ~size ctxb b in
        let Context.Result (inc, acc, deserialize_discrim) = Scalar.serialize_enum ctxa da ctxb (Scalar.enum_empty ctxb) in_tup in
        let Context.Result(inc, acc, deserialize_union) = serialize_union_with_right ~widens ~size ctxa lsa ctxb (Val vb) (inc,acc) in
        Context.Result (inc, acc, fun ctx out ->
          let lst, out = deserialize_union ctx out in
          let d, out = deserialize_discrim ctx out in
          let res,out = Union (d,lst), out in
          (* Codex_log.debug "VUC.serialize no.%d, returning %a" num (binary_pretty ~size ctx) res ; *)
          res, out
        )
      (*
      | _, Union lsb ->
        Log.debug (fun p -> p "in VUC.serialize_union_left a = %a and b = %a" (binary_pretty ~size ctxa) a (binary_pretty ~size ctxb) b );
        let va = Conversion.force ~size ctxa a in
        let Context.Result(inc, acc, deserialize_union) = serialize_union_with_left ~widens ~size ctxa (Val va) ctxb lsb in_tup in
        Context.Result (inc, acc, fun ctx out ->
          let lst, out = deserialize_union ctx out in
          let res,out = Union lst, out in
          Log.debug (fun p -> p "in VUC.serialize_union_left, returning %a" (binary_pretty ~size ctx) res );
          res, out
        )
      *)
      | _ ->
        let va = Conversion.force ~size ctxa a in
        let vb = Conversion.force ~size ctxb b in
        let Context.Result(inc, tup, des) = Sub.serialize ~widens ~size ctxa va ctxb vb in_tup in
        Context.Result (inc, tup, fun ctx out ->
          let v,out = des ctx out in
          let res,out = Val v, out in
          Log.debug (fun p -> p "VUC.serialize no.%d, returning %a" num (binary_pretty ~size ctx) res );
          res, out
        )

    let serialize_enum = Scalar.serialize_enum

    let fresh_int =
      let fresh_counter = ref (0 : int) in
      fun () ->
        incr fresh_counter ;
        !fresh_counter

    let fresh_symbol () = Format.sprintf "#%d" (fresh_int ())


    let rec binary_unknown_typed_from_record ~size ctx list =
      let folder (acc_value, acc_sz) typ =
        let sz = TypedC.sizeof typ |> In_bytes.in_bits in
        let value = binary_unknown_typed ~size:sz ctx typ in
        let concat_value = Binary_Forward.bconcat ~size1:sz ~size2:acc_sz ctx value acc_value in
        concat_value, In_bits.(acc_sz + sz)
      in
      let typ = List.hd list in
      let sz = TypedC.sizeof typ |> In_bytes.in_bits in
      let first_value = binary_unknown_typed ~size:sz ctx typ in
      let value, sz = List.fold_left folder (first_value, sz) (List.tl list) in
      assert (size = sz);
      value

    (** Create a new binary unknown with given type.
        @raise [Failure] if [~size] doesn't match the type's size
        /!\ [~size] should be the bit size, not the byte size *)
    and binary_unknown_typed ~size ctx typ =
      Log.trace (fun p -> p "binary_unknown_typed ~size:%d %a" (size:>int) TypedC.pp typ )
      ~pp_ret:Binary.pretty
      @@ fun () ->
      assert ((size:>int) mod 8 = 0);
      let open TypedC in
      let new_unknown () = Val (Sub.binary_unknown_typed ~size ctx typ) in
      let bin = match typ.descr with
      | Application _ -> binary_unknown_typed ~size ctx (inlined typ)
      | Void -> new_unknown ()
      | Base (byte_size, _) ->
          assert (size = In_bytes.in_bits byte_size);
          new_unknown ()
      | Structure struc ->
          binary_unknown_typed_from_record ~size ctx
          (List.map (fun (_offset, _name, typ) -> typ) struc.st_members)
      | Ptr{pointed} ->
          assert (size = Codex_config.ptr_size ());
          (* TODO change this...
              this should create a new pointer value which is either null
              or points to something of type pointed
              (whose value can be computed recursively) *)
          (* old_load_from_typed_region ~size ctx
          Type.(PtrT{ptyp=typ; idx = index_zero ctx; fe = index_minus_one ctx;
                      ppred=TypedC.Pred.True; ofs=0}) *)
          new_unknown ()
      | Array (tp, Fixed_length t) ->
          let array_size = Z.to_int t in
          assert (array_size > 0);
          binary_unknown_typed_from_record ~size ctx
          (List.init array_size (function _ -> tp))
      | Array (tp, _) ->
          failwith "Not implemented binary unknown of non-constant array"
      | Function _ ->
          failwith "Not implemented binary unknown of function"

      | Union {un_byte_size; un_types} ->
          let d = Scalar.enum_unknown ~enumsize:(List.length un_types) ctx in
          let lst = List.map (fun (sz,typ) ->
            let localtyp, globalpred = split_type typ in
            let value = binary_unknown_typed ~size ctx @@ localtyp in
            let cond = cond_of_pred ~size ctx globalpred ~self:value in
            (value, cond)
          ) un_types in
          Union (d,lst)

      | Existential {bound_typ;bound_var;body}  ->
          let te_sz = TypedC.sizeof bound_typ |> In_bytes.in_bits in
          let res = binary_unknown_typed ~size:te_sz ctx bound_typ in
          let new_symb = fresh_symbol () in
          add_global_symbol ~size:te_sz ctx new_symb res ;
          let new_t = TypedC.substitute_symbol body bound_var new_symb in
          let res = binary_unknown_typed ~size ctx new_t in
          add_type res typ ;
          res

      | Weak t ->
          assert (TypedC.sizeof t |> In_bytes.in_bits = size) ;
          binary_unknown_typed ~size ctx t

      | _ -> assert false
      in
      (* Add constraints if any *)
      let bin = imperative_use_invariant ~size ctx typ.pred bin in
      (* assume_type ~size ctx bin typ ; *) bin



    (** Unification for existential types *)
    (* Note: we call this unification, but this is actually checking
       that some fixed-sized value can be given a type which is an
       existential type. This requires instantiating the existential
       qualifiers, i.e., filling an environment, which is a binding
       from variables (string) to binary (values). Thus, unification
       is just a part of checking that a value has some type.
    *)
    module Unification = struct

      exception Non_unifiable_type of string

      (* We use environments which are maps from the variables
         appearing in the types (represented as strings) to binary
         option.

         TODO: What does None mean?
      *)
      module StringMap = struct
        include Map.Make(String)

        (** Updates the environment [map] with a binding from variable
            [key] to [value]. If [key] was previously bound to a value
            [prev], then [prev] and [value] should be equal, otherwise
            unification fails. *)
        let replace ~size ctx key value map =
          (* Note: shouldn't it be [find_opt] here? *)
          let prev = find key map in
          match prev, value with
          | Some p, Some v ->
            begin
              let comp = Binary_Forward.beq ~size ctx p v in
              let is_true = Query.boolean ctx comp in
              match is_true with
              | Lattices.Quadrivalent.True -> map
              | _ -> raise (Non_unifiable_type "Unification attempt results in two different predicates")
            end
          | _ -> add key value map
      end

      let rec contains_local_var expr env =
        let open TypedC.Pred in
        match expr with
        | Val (Sym s) when StringMap.mem s env -> true
        | Binop(_, e1, e2) -> contains_local_var e1 env || contains_local_var e2 env
        | Unop(_, e) -> contains_local_var e env
        | _ -> false

      (* TODO : Improve this and add more cases *)
      (** [unify_pred] codes updates [env] by finding values of
          variables in [pred] so that [pred] satisfies [self]. *)
      let rec unify_pred ~size ctx pred ~self env =
        let is_exist_var s = StringMap.mem s env in
        let open TypedC.Pred in
        match pred with
        | Cmp (Equal, Self, Val(Sym s))
        | Cmp (Equal, Val(Sym s), Self) ->
          if is_exist_var s then StringMap.replace ~size ctx s (Some self) env
          else env

        | Cmp (Equal, Self, Binop (Add, Val(Sym s), e))
        | Cmp (Equal, Self, Binop (Add, e, Val(Sym s)))
        | Cmp (Equal, Binop (Add, Val(Sym s), e), Self)
        | Cmp (Equal, Binop (Add, e, Val(Sym s)), Self) ->
          if contains_local_var e env && is_exist_var s then env
          else
            StringMap.replace ~size ctx s (Some
              (Binary_Forward.bisub ~size ~flags:(Operator.Flags.Bisub.pack ~nsw:false ~nuw:false ~nusw:false) ctx self (binary_of_expr ~size ctx ~self e))
            ) env

        | Cmp (Equal, Self, Binop (Sub, Val(Sym s), e))
        | Cmp (Equal, Binop (Sub, Val(Sym s), e), Self) ->
          if contains_local_var e env && is_exist_var s then env
          else
            StringMap.replace ~size ctx s (Some
              (Binary_Forward.biadd ~size
                 ~flags:(Operator.Flags.Biadd.pack ~nsw:false ~nuw:false ~nusw:false)
                 ctx self (binary_of_expr ~size ctx ~self e))
            ) env

        | And (p1, p2) ->
            let env = unify_pred ~size ctx p1 ~self env in
            unify_pred ~size ctx p2 ~self env
        | Cmp(_, e1, e2) when contains_local_var e1 env || contains_local_var e2 env -> env
        | _ -> env


      let unify_expr expr value env =
        let open TypedC.Pred in
        match expr with
        | Val (Sym s) when StringMap.mem s env -> StringMap.add s (Some value) env
        | _ -> env


      (* Note: unify_type is actually unification between type names. *)
      let unify_names ~size ctx typ1 typ2 env =
        let open TypedC in
        Log.trace (fun p -> p "unify_names %a %a" pp typ1 pp typ2 )
        @@ fun () ->
        let bottom = binary_empty ~size ctx in
        match typ1.descr, typ2.descr with
          | Application {constr = c1;args = exprs1},
            Application {constr = c2;args = exprs2}
            when List.length exprs1 = List.length exprs2 ->
              let env =
                List.combine exprs1 exprs2
                |> List.fold_left (fun env (e1,e2) ->
                  unify_expr e1 (binary_of_expr ~size ctx ~self:bottom e2) env) env in
              {typ1 with descr = Application {constr = c1;args = exprs1}}, env

          | Array (t1, Variable_length s1), Array (t2, Variable_length s2) when StringMap.mem s1 env ->
            let ptr_size = Codex_config.ptr_size () in
            let sz, value = global_symbol ctx s2 in
            assert (ptr_size = sz) ;
            t1, StringMap.add s1 (Some value) env (* TODO : check unify_type on t1 as well *)

          | _ -> typ1, env

      (* This checks whethere ptr_typ can be casted into typ. *)
      let unify_type ~size ctx typ ptr_typ env =
        let open TypedC in
        Log.trace (fun p -> p "unify_type %a %a" pp typ (Fmt.option pp) ptr_typ) (fun () ->
        match typ, ptr_typ with
        | {descr = Ptr {pointed=ptyp1}}, Some {descr = Ptr {pointed=ptyp2}} ->
          (* if TypedC.equiv ptyp1 ptyp2 then ptyp1, env *)
          (* else raise (Non_unifiable_type "Cannot unify different pointers") *)
          unify_names ~size ctx ptyp1 ptyp2 env

        | {descr = Ptr {pointed}; pred}, None -> pointed, env

        | _ -> assert false)


      (** Given the list [lst] of fields, checks that every field in
          [value] corresponds. *)
      let rec unify_record ~size ctx lst value env =
        let open TypedC in
        let members, env = List.fold_right (fun (ofs,name,t) (lst, env) ->
          let size_bytes = sizeof t in
          let size_bits = In_bytes.in_bits size_bytes in
          let index = In_bytes.in_bits ofs in
          let part = Binary_Forward.bextract ~size:size_bits ~oldsize:size ~index ctx value in
          let typ, env = unify ~size:size_bits ctx t part env in
          (ofs,name, typ) :: lst, env
        ) lst ([],env)
        in members, env

      (* TODO : Improve this *)
      and unify ~size ctx typ (value : binary) env =
        let ptrsize = Codex_config.ptr_size () in
        let open TypedC in
        Log.trace (fun p -> p "unify %a %a" pp typ (binary_pretty ~size ctx) value )
        @@ fun () ->
        let typ = inlined typ in
        let env = unify_pred ~size ctx typ.pred ~self:value env in
        match typ.descr with
          | Void -> typ, env
          | Base (sz, _) -> typ, env
          | Structure ({st_byte_size = sz; st_members} as st) ->
              assert (In_bytes.in_bits sz = size);
              let st_members, env = unify_record ~size ctx st_members value env in
              {typ with descr = Structure {st with st_members}}, env

          | StructureFAM _ -> assert false (* TODO *)

          | Ptr ptr ->
            let ptr_typ = Sub.type_of ~size ctx @@ Conversion.force ~size ctx value in
            let pointed,env = unify_type ~size ctx typ ptr_typ env in
            let typ = Build.ptr pointed typ.pred in
            typ, env

          | Array (tp, ((Fixed_length t) as sz)) ->
              let elem_size = sizeof tp |> In_bytes.in_bits in
              let t,env = unify ~size:elem_size ctx tp value env in
              {typ with descr = Array (t, sz)}, env

          | Array (tp, (Variable_length s)) when StringMap.mem s env ->
            let elem_byte_size = sizeof tp in
            let elem_size = elem_byte_size |> In_bytes.in_bits in
            if (size:>int) mod (elem_size:>int) = 0 then
              let arr_size = (size:>int) / (elem_size:>int) in
              let arr_size_b = Binary_Forward.biconst ~size:ptrsize (Z.of_int arr_size) ctx in
              let env = StringMap.add s (Some arr_size_b) env in
              let typs = List.init arr_size (fun i -> (In_bytes.times i elem_byte_size, "anon", tp)) in
              let st_members,env = unify_record ~size ctx typs value env in
              {typ with descr = Structure {st_byte_size = In_bits.in_bytes size; st_members}}, env

            else raise (Non_unifiable_type "Unification attempt with a variable size array of the wrong size")

      | Existential {bound_typ;bound_var;body}  ->
            begin
              let env' = StringMap.add bound_var None env in
              let t, new_env = unify ~size ctx body value env' in
              let v =
                match StringMap.find bound_var new_env with None -> binary_unknown ~size ctx | Some v -> v
              in
               (* raise Not_found ; *) (* TODO : could be omitted if the existential variable is not used anywhere *)
              let exist_size = sizeof bound_typ |> In_bytes.in_bits in
              let new_var = fresh_symbol () in
              add_global_symbol ~size:exist_size ctx new_var v ;
              substitute_symbol t bound_var new_var, new_env
            end

          | Union ({un_types;} as un) ->
                let new_lst, env' =
                  List.fold_left (fun (ls,map) (sz,typ) -> let t,m = unify ~size ctx typ value map in (sz,t):: ls,m) ([],env) un_types
                in
                let new_lst = List.rev new_lst in
                {typ with descr = Union {un with un_types = new_lst}}, env'

          | Weak _ -> raise (Non_unifiable_type "Unification attempt with weak type")

          | Array _ -> raise (Non_unifiable_type "Unification attempt with arbitrary array")

          | Function _ -> raise (Non_unifiable_type "Unification attempt with a function")

          | Application _ -> raise (Non_unifiable_type "Unification attemp with non-applied constructor type")

      let rec unify_function ctx typ args env =
        let open TypedC in
        match (inlined typ).descr with
        | Existential {bound_typ;bound_var;body}  ->
          begin
            let env' = StringMap.add bound_var None env in
            let t, new_env = unify_function ctx body args env' in

            let v =
              match StringMap.find bound_var new_env with
              | None ->
                (* raise Not_found ; *) (* TODO : could be omitted if the existential variable is not used anywhere *)
                Codex_log.warning "Existential variable could not be identified by unification" ;
                binary_unknown_typed ~size:(sizeof bound_typ |> In_bytes.in_bits) ctx bound_typ
              | Some v -> v
            in
            let exist_size = sizeof bound_typ |> In_bytes.in_bits in
            let new_var = fresh_symbol () in
            add_global_symbol ~size:exist_size ctx new_var v ;
            substitute_symbol t bound_var new_var, new_env
          end

        | Union ({un_types;} as un) ->
          let new_lst, env' =
            List.fold_left (fun (ls,map) (sz,typ) -> let t,m = unify_function ctx typ args map in (sz,t):: ls,m) ([],env) un_types
          in
          let new_lst = List.rev new_lst in
          {typ with descr = Union {un with un_types = new_lst}}, env'

        | Function funtyp ->
          let new_lst, env' =
            List.fold_left (fun (ls,map) (typ,(size,arg)) -> let t,m = unify ~size ctx typ arg map in t :: ls,m) ([],env) @@ List.combine funtyp.args args
          in
          let new_args = List.rev new_lst in
          {typ with descr = Function {funtyp with args = new_args}}, env'

        | _ -> assert false

      let unify ~size ctx typ value = fst @@ unify ~size ctx typ value StringMap.empty

      let unify_function ctx typ args = fst @@ unify_function ctx typ args StringMap.empty

    end

    (* ------------------------------------------------------------------------------------------------------*)

    let rec check_type ~(size:In_bits.t) ctx typ value =
      Log.trace (fun p ->
          p "check_type size:%d %a %a" (size:>int) TypedC.pp typ (binary_pretty ~size ctx)
            value)
      @@ fun () ->
      let open Type_check_tree in
      let block_data = create_typing_node value (binary_pretty ~size ctx) typ in
      let typ = TypedC.inlined typ in
      let typing_store () =
        Codex_log.error "Type %a possibly does not hold for value %a" TypedC.pp typ
          (binary_pretty ~size ctx) value;
        type_error block_data Operator.Alarm.Typing_store
      in
      let open TypedC in
      let b1 =
        match typ.descr with
        | Void -> assert false
        | Base base -> no_error block_data
        | Structure s ->
            check_type_and_predicate_record ~size ctx s.st_members value block_data
        | Ptr ptr ->
            Sub.check_type ~size ctx typ (Conversion.force ~size ctx value)
            |> sub_check block_data
        | Array (elem_typ, Fixed_length sz) ->
            let elem_sz = sizeof elem_typ in
            let members =
              List.init (Z.to_int sz) (fun i -> (In_bytes.times i elem_sz, "anon", elem_typ))
            in
            check_type_and_predicate_record ~size ctx members value block_data
        | StructureFAM _ -> assert false
        | Array _ -> assert false
        | Function _ -> assert false
        | Application _ -> assert false
        | Existential _   -> (
            if Option.equal TypedC.equal (Some typ) (get_type value) then
              no_error block_data
            else
              try
                let newtyp = Unification.unify ~size ctx typ value in
                check_type ~size ctx newtyp value
              with _ ->
                (Codex_log.error
                  "Storing value@ %a to region of type@ %a: no unification is made"
                (binary_pretty ctx ~size) value TypedC.pp typ);
                type_error block_data Operator.Alarm.Typing_store)
        | Union { un_types } ->
            check_type_and_predicate_union ~size ctx un_types value block_data
        | Weak _ ->
            Codex_log.error "Reaching non unified weak type.";
            type_error block_data Operator.Alarm.Weak_type_use
      in
      let b2 =
        let c = cond_of_pred ~size ctx typ.pred ~self:value in
        let is_true = Query.boolean ctx c in
        match is_true with
        | Lattices.Quadrivalent.True -> no_error block_data
        | Lattices.Quadrivalent.Bottom -> typing_store () (* true *)
        | _ -> typing_store () (* Possibly false *)
      in
      and_ block_data [b1;b2]

    and check_type_and_predicate_record ~size ctx lst value block_data =
      (*Log.trace (fun p -> p "check_type_and_predicate_record %a of oldsize %d and %a"*)
      (*  TypedC.pp typ*)
      (*  size*)
      (*  (binary_pretty ~size ctx) value) *)
      (*@@ fun () -> *)
      let open Type_check_tree in
      let mapper (ofs,_,typ) =
        let part_size = TypedC.sizeof typ |> In_bytes.in_bits in
        let index = In_bytes.in_bits ofs in
        let part = Binary_Forward.bextract ~size:part_size ~oldsize:size ~index ctx value in
        check_type ~size:part_size ctx typ part
      in
      tlist_map mapper lst |> and_ block_data


    and check_type_and_predicate_union ~size ctx typs value block_data =
    Log.trace (fun p ->
        p "check_type_and_predicate_union %a and %a" TypedC.pp
          TypedC.
            {
              descr = Union { un_byte_size = Some (In_bits.in_bytes size); un_types = typs };
              pred = Pred.true_;
            }
          (binary_pretty ~size ctx) value)
    @@ fun () ->
    let open Type_check_tree in
     let typing_store () =
      Format.(Codex_log.error "Type %a possibly does not hold in the union {%a}"
          (pp_print_list (fun p (s,typ) -> fprintf p "%s:%a" s TypedC.pp typ)) typs
          (binary_pretty ~size ctx) value);
        type_error block_data Operator.Alarm.Typing_store in
    let checker (typ,(value, cond)) =
      Log.debug (fun p ->
          p "Checking if value %a is of type %a" (binary_pretty ~size ctx) value
            TypedC.pp typ);
      let typ, pred = split_type typ in
      Log.debug (fun p ->
          p "Type is made from a local type %a and some global predicates %a"
            TypedC.pp typ TypedC.Pred.pp pred);
      let bottom = binary_empty ~size ctx in
      (* Should not be used in actuality *)
      let global_cond = cond_of_pred ~size ctx pred ~self:bottom in
      Log.debug (fun p -> p "global_cond : %a" (boolean_pretty ctx) global_cond);
      let equiv = Boolean_Forward.equiv ctx cond global_cond in
      Log.debug (fun p -> p "equiv is %a" (boolean_pretty ctx) equiv);
      match Query.boolean ctx equiv with
      | False | Top -> typing_store ()
      | Bottom -> assert false
      | True ->
          if Query.Binary_Lattice.is_empty ~size @@ Query.binary ~size ctx value
          then no_error block_data
          else check_type ~size ctx typ value
    in
    match value with
    | Union (discrim, cases) -> (
        Log.debug (fun p -> p "in multiple cases union case");
        try
          List.map2 (fun (_,typ) case ->(typ,case)) typs cases
          |> tlist_map checker
          |> and_ block_data
       with Invalid_argument _ -> typing_store ())
    | _ ->
        Log.debug (fun p -> p "in single case union case");
        or_ block_data (fun (_,typ) -> check_type ~size ctx typ value) typs



    let analyze_summary ctx typ args =
      let open TypedC in
      let pp_args fmt = (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
        (fun fmt (sz,arg) -> (binary_pretty ~size:sz ctx fmt) arg)) fmt in
      Log.trace (fun p -> p "analyze_summary %a %a"
        pp typ pp_args args)
      @@ fun () ->
      let newtyp = Unification.unify_function ctx typ args in
      Log.debug (fun p -> p "In analyze_summary, unified function type is %a" pp newtyp );
      match newtyp.descr with
      | Function {ret = rtyp; args = typs} ->
        let open Type_check_tree in
        let res =
          let type_check_tree =
            List.combine typs args
            |> tlist_map (fun (typ,(size,arg)) -> check_type ~size ctx typ arg)
            |> and_ (create_typing_node args pp_args newtyp)
           in
          save type_check_tree;
          is_valid type_check_tree
         in
        (*TODO: this is not ideal*)
        if not res then begin
            Codex_log.error "Function type %a possibly does not hold for arguments %a" TypedC.pp typ
              (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
                  (fun fmt (sz,arg) -> (binary_pretty ~size:sz ctx fmt) arg)) args ;
          type_error (create_typing_node args pp_args newtyp) Operator.Alarm.Typing_store
          |> save
        end;
        let ret =
          begin match rtyp with
          | {descr = Void;} -> None
          | _ ->
            let retsize = sizeof rtyp |> In_bytes.in_bits in
            Some (retsize, binary_unknown_typed ~size:retsize ctx rtyp)
          end
        in res, ret
      | _ -> assert false

    let binary2scalar_binary ~size ctx value =
      Sub.binary2scalar_binary ~size ctx @@ Conversion.force ~size ctx value

    let assume_type ~size ctx value typ = assert false

    let type_of ~size ctx value = Sub.type_of ~size ctx @@ Conversion.force ~size ctx value

    let force ~size ctx value = Conversion.force ~size ctx value

    let make_value value = Val value

    let rec addresses_in_concat ctxa la ctxb lb =
      match la,lb with
      | [],[] -> []
      | [], _ | _, [] -> assert false
      | (sza,va) :: tla, (szb,vb) :: tlb when sza = szb ->
        begin
          let lst = addresses_in_concat ctxa tla ctxb tlb in
          let res = addresses_in_binary ~size:sza ctxa va ctxb vb in
          res @ lst
        end

      | (sza,va) :: tla, (szb,vb) :: tlb when sza < szb ->
        begin
          let part = Binary_Forward.bextract ~size:sza ~index:In_bits.zero ~oldsize:szb ctxb vb in
          let rest = Binary_Forward.bextract ~size:In_bits.(szb-sza) ~index:sza ~oldsize:szb ctxb vb in
          let lst = addresses_in_concat ctxa tla ctxb (In_bits.(szb-sza, rest) :: tlb) in
          let res = addresses_in_binary ~size:sza ctxa va ctxb part in
          res @ lst
        end
      | (sza,va) :: tla, (szb,vb) :: tlb ->
        begin
          let part = Binary_Forward.bextract ~size:szb ~index:In_bits.zero ~oldsize:sza ctxa va in
          let rest = Binary_Forward.bextract ~size:In_bits.(sza-szb) ~index:szb ~oldsize:sza ctxa va in
          let lst = addresses_in_concat ctxa ((In_bits.(sza-szb), rest) :: tla) ctxb tlb in
          let res = addresses_in_binary ~size:szb ctxa part ctxb vb in
          res @ lst
        end

    and addresses_in_binary ~size ctxa va ctxb vb =
      Log.trace (fun p -> p "addresses_in_value ~size:%d %a %a" (size:>int) (binary_pretty ~size ctxa) va (binary_pretty ~size ctxb) vb )
      @@ fun () ->
      match va,vb with
      | Val va, Val vb -> List.map (fun (a,b) -> (Val a, Val b)) @@ Sub.addresses_in_binary ~size ctxa va ctxb vb
      | Concat lsa, Val _ -> addresses_in_concat ctxa lsa ctxb [(size,vb)]
      | Val _, Concat lsb -> addresses_in_concat ctxa [(size,va)] ctxb lsb
      | Concat lsa, Concat lsb -> addresses_in_concat ctxa lsa ctxb lsb

      | _ -> []


    (* Useless, but needed *)
    module Address = struct
      let beq = Binary_Forward.beq
      let ble = Binary_Forward.biule
      let bshift = Binary_Forward.bshift
      let bisub ~size _ = assert false (* Binary_Forward.bisub *)
      let bindex ~size  = assert false
      let bchoose = Binary_Forward.bchoose
      let within_bounds ~size _ = assert false
      let zero_offset ~size ~max ctx = assert false
      let query_boolean = Query.boolean
    end
    include Address
    let serialize_binary = serialize
    let union = Scalar.union




end

module Make (Sub:Memory_sig.WHOLE_MEMORY_DOMAIN)
  : Memory_sig.WHOLE_MEMORY_DOMAIN
    with module Scalar = Sub.Scalar
     and module Offset = Sub.Offset
     and module Address.Context = Sub.Address.Context
     and module Address.Scalar = Sub.Address.Scalar
= struct
  module Scalar = Sub.Address.Scalar
  module Offset = Sub.Offset
  module Address = MakeAddressOnly(Sub.Address)
  type address = Address.binary

  module Make_Memory
      (Block:Memory_sig.BLOCK with module Scalar = Scalar and module Offset = Offset)
    :Memory_sig.MEMORY
      with module Scalar = Scalar
       and module Address = Address
       and module Block = Block
     (* and type boolean = Sub.Memory(Block)(Lift).boolean *)
  = struct
    module Scalar = Scalar
    module Address = Address
    module Block = Block
    module SubMemory = Sub.Make_Memory(Block)

    module M:Memory_sig.WITH_BOOLEAN_REDEFINITION
      with module Context = Scalar.Context
       and type boolean = SubMemory.boolean =
      SubMemory
    include M

    module Value = Block.Value

    type address = Address.binary

    type memory = SubMemory.memory

    type offset = Block.Offset.offset

    let pretty ctx fmt x = SubMemory.pretty ctx fmt x

    let memory_empty ctx = SubMemory.memory_empty ctx

    let rec map_assume_cases
    : case:int -> Context.t -> (Context.t -> address -> 'a) -> Scalar.enum -> (address * Address.boolean) list -> ('a * Context.t) option list
    = fun ~case ctx operation discrim cases ->
      match cases with
      | [] -> []
      | (value, cond) :: tl ->
        let case_cond = Scalar.Enum_Forward.caseof ~case ctx discrim in
        let ctxopt = Scalar.assume ctx case_cond in
        let ctxopt = Option.bind ctxopt (fun ctx -> Address.assume ctx cond) in
        let res = Option.map (fun ctx -> (operation ctx value), ctx) ctxopt in
        res :: (map_assume_cases ~case:(case + 1) ctx operation discrim tl)

    let map_cases ctx operation discrim cases =
      (* let ctx = address_ctx ctx in *)
      match cases with
      | [] -> assert false
      | _ -> map_assume_cases ~case:0 ctx operation discrim cases

    (*
    let load ~size (ctx:Value.Context.t) mem list : (Value.binary * Context.t) option =
      let ctx, to_value_ctx = Lift.ctx ctx in
      let mapper (addr, cond, discrim) =
        let ctxopt = Scalar.assume ctx discrim in
        let ctxopt = Option.bind ctxopt (fun ctx -> Address.assume ctx cond) in
        begin match ctxopt with
          | None -> None
          | Some ctx ->
            let ctx = to_value_ctx ctx in
            Some (SubMemory.load ~size ctx mem @@ Address.Conversion.force ~size ctx addr, ctx)
        end
      in
      let loaded = List.map mapper list in
      match loaded with
      | [] -> None
      | [x]-> x
      | a::tl ->
        let nondet_binary a b =
          begin match a,b with
            | None, _ -> b
            | _, None -> a
            | Some (v1,ctx1), Some (v2,ctx2) ->
              let Context.Result(_,tup,deserialize2) = Value.serialize ~widens:false ~size ctx1 v1 ctx2 v2
                (true, Context.empty_tuple ()) in
              let ctx,res_tup = Value.typed_nondet2 ctx1 ctx2 tup in
              Some (fst @@ deserialize2 ctx res_tup, ctx)
          end
        in List.fold_left nondet_binary a tl

    let load ~size (ctx:Value.Context.t) mem (x : address) =
      match x with
      | Val addr -> SubMemory.load ~size ctx mem addr
      | Union lst ->
        begin match load ~size ctx mem lst with
          | None -> assert false
          | Some (loaded, newctx) -> imperative_assign_context ctx newctx ; loaded
        end
      | _ -> assert false
    *)

    let load ~size (ctx:Value.Context.t) mem (x : address) =
      let ptr_size = Codex_config.ptr_size () in
      Log.trace (fun p -> p "load %a" (Address.binary_pretty ~size:ptr_size ctx) x )
      @@ fun () ->
      SubMemory.load ~size ctx mem @@ Address.force ~size:ptr_size ctx x


    let typed_load ~size ctx mem x typ = assert false

    let store ~size (ctx : Value.Context.t) mem (x : address) value =
      (* match x with
      | Val addr -> SubMemory.store ~size ctx mem addr value
      | Union _ -> SubMemory.store ~size ctx mem (Address.Conversion.force ~size ctx x) value
      | _ -> assert false
      *)
      SubMemory.store ~size ctx mem (Address.force ~size ctx x) value

    let typed_store ~size ctx mem x typ value = assert false

    let load_block (ctx:Value.Context.t) mem (x : address) =
      let ptr_size = Codex_config.ptr_size () in
      Log.trace (fun p -> p "VUC.load_block %a" (Address.binary_pretty ~size:ptr_size ctx) x)
      @@ fun () ->
      SubMemory.load_block ctx mem @@ Address.force ~size:ptr_size ctx x

    let store_block (ctx : Value.Context.t) mem (x : address) value =
    let ptr_size = Codex_config.ptr_size () in
    SubMemory.store_block ctx mem (Address.force ~size:ptr_size ctx x) value

    let serialize ~widens ctxa mema ctxb memb acc =
      SubMemory.serialize ~widens ctxa mema ctxb memb acc

    let malloc ~id ~malloc_size ctx mem =
      let address,mem = SubMemory.malloc ~id ~malloc_size ctx mem in
      Address.make_value address, mem

    let free ctx mem (x : address) =
      let size = Codex_config.ptr_size () in
      (* match x with
      | Val addr -> SubMemory.free ctx mem addr
      | Union _ -> SubMemory.free ctx mem @@ Address.Conversion.force ~size ctx x
      | _ -> assert false
      *)
      SubMemory.free ctx mem @@ Address.force ~size ctx x

    let unknown ~level ctx = SubMemory.unknown ~level ctx

    let should_focus ~size (ctx:Value.Context.t) mem discrim list : ((address * offset) option * Context.t) option =
      let ptr_size = Codex_config.ptr_size () in
      let operation ctx addr =
        Option.map (fun (base,off) -> ((Address.Val base), off)) @@
          SubMemory.should_focus ~size ctx mem @@ Address.force ~size:ptr_size ctx addr
      in
      let res = map_cases ctx operation discrim list in
      match res with
      | [] -> assert false
      | [x]-> x
      | a::tl ->
        let nondet_binary a b =
          begin match a,b with
            | None, _ -> b
            | _, None -> a
            | Some (Some (base1,off1), ctx1), Some (Some (base2,off2), ctx2)
                when off1 = off2
               ->
              (* TODO : Keep the union structure rather than joining all bases *)
              let Address.Context.Result(_,tup,deserialize2) = Address.serialize ~widens:false ~size ctx1 base1 ctx2 base2
                (true, Address.Context.empty_tuple ()) in
              let ctx,res_tup = Scalar.typed_nondet2 ctx1 ctx2 tup in
              Some (Some (fst @@ deserialize2 ctx res_tup, off1), ctx)

            | _ -> None
          end
        in List.fold_left nondet_binary a tl

    let should_focus ~size ctx (mem:memory) (x:address) : (address * offset) option =
      let ptr_size = Codex_config.ptr_size () in
      Log.trace (fun p -> p "VUC.should_focus %a" (Address.binary_pretty ~size:ptr_size ctx) x )
      @@ fun () ->
      match x with
      | Union (d,lst) ->
        begin
          let res = should_focus ~size ctx mem d lst in
          match res with
          | None -> None
          | Some (None, _) -> None
          | Some (Some (base,off), newctx) ->
            Value.Context.assign ctx newctx ;
            Some (base,off)
        end
      | _ ->
        let addr = Address.force ~size:ptr_size ctx x in
        Option.map (fun (base,off) -> ((Address.Val base), off)) @@ SubMemory.should_focus ~size ctx mem addr

    (*
    let should_focus ~size ctx (mem:memory) (x:address) =
      let ctx = address_ctx ctx in
      Log.debug (fun p -> p "VUC.should_focus ~size:%d %a" size (Address.binary_pretty ~size ctx) x );
      let ptr_size = Codex_config.ptr_size () in
      let res = SubMemory.should_focus ~size ctx mem @@ Address.force ~size:ptr_size ctx x in
      Option.map (fun (addr,size,off) -> (Address.make_value addr, size, off)) res
    *)

    let may_alias ~ptr_size ctx ~size1 ~size2 x y =
      let addr_x = Address.force ~size:ptr_size ctx x in
      let addr_y = Address.force ~size:ptr_size ctx y in
      SubMemory.may_alias ~ptr_size ctx ~size1 ~size2 addr_x addr_y
      (*
      match x,y with
      | Val addrx, Val addry -> SubMemory.may_alias ~ptr_size ctx ~size1 ~size2 addrx addry
      | _ -> assert false
      *)

    let is_weak ~size ctx addr =
      SubMemory.is_weak ~size ctx @@ Address.force ~size ctx addr

    let addresses_in_memory ctxa mema ctxb memb =
      SubMemory.addresses_in_memory ctxa mema ctxb memb

  end

end
