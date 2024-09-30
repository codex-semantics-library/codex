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

module Log = Tracelog.Make(struct let category = "Domains.Wholify" end);;
module Ctypes = Types.Ctypes

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

module MakeAddressOnly
    (SubAddress:Memory_sig.Address)
    (Address2Scalar: Memory_sig.Address_to_Scalar with module Scalar := SubAddress.Scalar
                                                   and module Address := SubAddress)
(* :Memory_sig.Operable_Value_Whole *) = struct

  let address2scalar x = fst @@ Address2Scalar.ctx x

    
  (* module Scalar = SubAddress.Scalar *)

  (* module Operable_Value(\* :Memory_sig.Address *\) = struct *)

    module Domain:Domain_sig.Minimal_No_Boolean with module Context = SubAddress.Context
      = SubAddress
    include Domain      
    
    module Scalar = SubAddress.Scalar

    (* A binary is either a numerical value, or an address. *)
    type binary =
    {
      (* true if an address, false if a scalar, top if both, bottom if none.   *)
      (* Note: probably this should be a  SubAddress.boolean, which is higher in the stack.
         we would thus not need to go from SubAddress.boolean to boolean. *)
      is_address: Scalar.boolean;
      (* Note: we don't use nullE and addressE directly,
        and use decomp (below) instead. *)
      nullE: Scalar.binary option;
      addressE: SubAddress.binary option;
      invalid:bool;
      id: int; (** Unique identifier for comparisons. *)
    }

    (* Refine null and address using the is_address boolean. *)
    let decomp ctx binary =
      let res = Scalar.query_boolean ctx binary.is_address in
      match res, binary.nullE, binary.addressE with
      | Lattices.Quadrivalent.Bottom, _, _ -> None,None
      | Lattices.Quadrivalent.True, _, None -> None,None
      | Lattices.Quadrivalent.True, _, (Some _ as addr) -> None, addr
      | Lattices.Quadrivalent.False, None, _ -> None,None
      | Lattices.Quadrivalent.False, (Some _ as null), _  -> null, None
      | Lattices.Quadrivalent.Top, null, addr -> null, addr
      | _ -> .

    let fresh_id =
      let counter = ref 0 in
      fun () -> incr counter; !counter

    let pp scalar_pretty binary_pretty fmt x =
      let or_invalid = match x.invalid with
        | false -> ""
        | true -> " or invalid"
      in
      begin match x.nullE, x.addressE with
        | None, None -> Format.fprintf fmt "<bottom>"
        | Some x, None -> Format.fprintf fmt "%a%s" scalar_pretty x or_invalid
        | None, Some x -> Format.fprintf fmt "%a%s" binary_pretty x or_invalid
        | Some n, Some adr ->
          Format.fprintf fmt "%a or %a%s" 
            scalar_pretty n
            binary_pretty adr 
            or_invalid          
      end

    let ppctx ctx scalar_pretty binary_pretty fmt x =
      let or_invalid = match x.invalid with
        | false -> ""
        | true -> " or invalid"
      in
      (* Codex_log.feedback "is_address %a" (Scalar.boolean_pretty ctx) x.is_address; *)
      begin match decomp ctx x with
        | None, None -> Format.fprintf fmt "<bottom>"   
        | Some scal, None -> Format.fprintf fmt "%a%s" scalar_pretty scal or_invalid
        | None, Some adr -> Format.fprintf fmt "%a%s"   binary_pretty adr or_invalid
        | Some n, Some adr ->
          Format.fprintf fmt "%a or %a%s"
            scalar_pretty n
            binary_pretty adr 
            or_invalid          
      end

    
    let binary_pretty ~size ctx =
      ppctx ctx (Scalar.binary_pretty ~size ctx) (SubAddress.binary_pretty ~size ctx)

    (** binary operators: =, signed <=, unsigned <= and their negation *)
    type binpred = B_EQ | B_SLE | B_ULE | B_NEQ | B_NSLE | B_NULE

    (** Elementary boolean types: quadrivalent or binpred *)
    type boolean_simple =
      | BVal of Lattices.Quadrivalent.boolean
      | BBinpred of {pred:binpred; size : int; left : binary; right : binary }
      (* Note: we should normally not encounter BUnvalid, as it is difficult to handle precisely. *)
      (* But we could handle it by adding a "pos" argument here saying whether we mean valid or invalid. *)
      | BValid of { size:int; arg:binary}

    (** Simple boolean AST, not is cast down on operator or quadrivalent *)
    type boolean_tree =
      | BSimple of boolean_simple
      | BAnd of boolean_tree * boolean_tree
      | BOr of boolean_tree * boolean_tree

    type boolean = boolean_tree

    let bottom = (BSimple(BVal(Lattices.Quadrivalent.Bottom)))
    let top = (BSimple(BVal(Lattices.Quadrivalent.Top)))

    let boolean_empty _ctx = bottom
    let boolean_unknown _ctx = top

    let binpred_pretty fmt op =
      let open Format in
      match op with
      | B_EQ -> fprintf fmt "EQ"
      | B_SLE -> fprintf fmt "SLE"
      | B_ULE -> fprintf fmt "ULE"
      | B_NEQ -> fprintf fmt "NEQ"
      | B_NSLE -> fprintf fmt "NSLE"
        | B_NULE -> fprintf fmt "NULE"
  
    let boolean_simple_pretty ctx fmt x =
      let open Format in
      match x with
      | BVal b -> fprintf fmt "BVal %a" Lattices.Quadrivalent.pretty b
      (* | BVal b -> fprintf fmt "BVal %a" Scalar.Boolean.pretty b *)
      | BBinpred {pred; size; left; right} ->
          fprintf fmt "BBinpred {pred:%a; size:%d; left:%a; right:%a}"
              binpred_pretty pred
              size
              (binary_pretty ~size ctx) left
              (binary_pretty ~size ctx) right
  
      | BValid {size; arg} -> fprintf fmt "BValid {size:%d; arg:%a}" size (binary_pretty ~size ctx) arg
  
    let rec boolean_pretty ctx fmt x =
      let open Format in
      match x with
      | BSimple bs -> boolean_simple_pretty ctx fmt bs
      | BAnd (bl,br) ->
          fprintf fmt "(%a) and (%a)" (boolean_pretty ctx) bl (boolean_pretty ctx) br
      | BOr (bl,br) ->
          fprintf fmt "(%a) or (%a)" (boolean_pretty ctx) bl (boolean_pretty ctx) br

    let boolean2scalar_bool _ = assert false
    let scalar_bool2boolean _ = assert false      

    module Binary = struct
      type t = binary

      let pretty =
        pp (Scalar.Binary.pretty) (SubAddress.Binary.pretty)

      let hash {id; _} = Hashtbl.hash id

      (* let compare x y = Pervasives.compare x.id y.id *)

      (* let hash {id;nullE;addressE} =
        Hashtbl.hash (
          (Option.map Scalar.Binary.hash nullE),
          (Option.map SubAddress.Binary.hash addressE)
        )
      *)

      let compare x y =
        let c = Option.compare Scalar.Binary.compare x.nullE y.nullE in
        if c <> 0 then c
        else Option.compare SubAddress.Binary.compare x.addressE y.addressE


      let equal x y = compare x y = 0
    end

    module Boolean:Datatype_sig.S with type t=boolean = struct
      type t = boolean

      (* No real notion of order on booleans... *)

      let compare_simple a b =
        match a,b with
        | BVal va, BVal vb -> Single_value_abstraction.Quadrivalent.Boolean_Lattice.compare va vb
        | _ -> assert false

      let compare (a:t) (b:t) : int =
        match a,b with 
        | BSimple a, BSimple b -> compare_simple a b
        | _ -> assert false

      let equal a b = compare a b = 0

      let hash (x:t) : int = assert false

      let pretty_binpred fmt operator =
        Format.fprintf fmt "%s" (match operator with
          | B_EQ -> "B_EQ"
          | B_SLE -> "B_SLE"
          | B_ULE -> "B_ULE"
          | B_NEQ -> "B_NEQ"
          | B_NSLE -> "B_NSLE"
          | B_NULE -> "B_NULE"
        )
      let pretty_simple fmt = function
        | BVal x -> Format.fprintf fmt "BVal(%a)" Lattices.Quadrivalent.pretty x
        | BValid{size;arg} -> Format.fprintf fmt "BValid{size=%d;arg=%a}" size Binary.pretty arg
        | BBinpred{ pred; size; left; right } ->
            Format.fprintf fmt "%a(@[%a@,@ %a@])"
              pretty_binpred pred
              Binary.pretty left Binary.pretty right
      let rec pretty_tree fmt x = match x with
        | BSimple b -> pretty_simple fmt b
        | BAnd(l,r) -> Format.fprintf fmt "BAnd(@[%a@,@ %a@])" pretty_tree l pretty_tree r
        | BOr(l,r) -> Format.fprintf fmt "BOr(@[%a@,@ %a@])" pretty_tree l pretty_tree r
      let pretty fmt x =
        Format.fprintf fmt "%a" pretty_tree x
    end

    let global_symbol ctx s = assert false
    let add_global_symbol ~size ctx name binary = assert false

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

    let and_tree l r = match l,r with
      | BSimple(BVal(False|Bottom as e)), _
      | _, BSimple(BVal(False|Bottom as e)) -> BSimple(BVal(e))
      | BSimple(BVal(True)), a
      | a, BSimple(BVal(True)) -> a
      | _ -> if l = r then l else BAnd(l,r)

    let or_tree l r = match l,r with
      | BSimple(BVal(True|Bottom as e)), _
      | _, BSimple(BVal(True|Bottom as e)) -> BSimple(BVal(e))
      | BSimple(BVal(False)), a
      | a, BSimple(BVal(False)) -> a
      | _ -> if l = r then l else BOr(l,r)

    module Boolean_to_Binary = struct

      (* Common to binary comparison operations. *)
      let ar2 op1 ~size ctx a b =
        (* Codex_log.debug "Wholifiy.Boolean_to_Binary.ar2 a:(%a) b:(%a)" (binary_pretty ~size ctx) a (binary_pretty ~size ctx) b ; *)
        let a_null, a_address = decomp ctx a in
        let b_null, b_address = decomp ctx b in
        (* let invalid = a.invalid || b.invalid || (a_address <> None) || (b_address <> None) in
        let nullE = match a_null, b_null with
          | Some a, Some b -> Some(op1 ~size ctx a b)
          | _ -> None
        in *)
        let invalid = a.invalid || b.invalid in
        let res =
          match (a_address, a_null), (b_address, b_null) with
          | (None, Some a_null), (None, Some b_null) -> op1 ~size ctx a_null b_null
          | (Some a_addr, None), (None, Some b_null) -> op1 ~size ctx (SubAddress.binary2scalar_binary ~size ctx a_addr) b_null (* TODO : use fonction from SubAddress *)
          | (None, Some a_null), (Some b_addr, None) -> op1 ~size ctx a_null (SubAddress.binary2scalar_binary ~size ctx b_addr)
          | (Some a_addr, None), (Some b_addr, None) -> 
              op1 ~size ctx (SubAddress.binary2scalar_binary ~size ctx a_addr) (SubAddress.binary2scalar_binary ~size ctx b_addr)

          | (Some a_addr, Some a_null), (None, Some b_null) ->
            let scalar_and = Scalar.Boolean_Forward.(&&) in
            let scalar_or = Scalar.Boolean_Forward.(||) in
            let scalar_not = Scalar.Boolean_Forward.not in
            
            scalar_or ctx 
              (scalar_and ctx (scalar_not ctx a.is_address) (op1 ~size ctx a_null b_null))
              (scalar_and ctx a.is_address (op1 ~size ctx (SubAddress.binary2scalar_binary ~size ctx a_addr) b_null))

          | (None, Some a_null), (Some b_addr, Some b_null) ->
            let scalar_and = Scalar.Boolean_Forward.(&&) in
            let scalar_or = Scalar.Boolean_Forward.(||) in
            let scalar_not = Scalar.Boolean_Forward.not in

            scalar_or ctx 
              (scalar_and ctx (scalar_not ctx b.is_address) (op1 ~size ctx a_null b_null))
              (scalar_and ctx b.is_address (op1 ~size ctx a_null (SubAddress.binary2scalar_binary ~size ctx b_addr)))

          | (Some a_addr, Some a_null), (Some b_addr, None)  ->
            let scalar_and = Scalar.Boolean_Forward.(&&) in
            let scalar_or = Scalar.Boolean_Forward.(||) in
            let scalar_not = Scalar.Boolean_Forward.not in

            scalar_or ctx 
              (scalar_and ctx (scalar_not ctx a.is_address)
                (op1 ~size ctx a_null (SubAddress.binary2scalar_binary ~size ctx b_addr)))
              (scalar_and ctx a.is_address
                (op1 ~size ctx (SubAddress.binary2scalar_binary ~size ctx a_addr) (SubAddress.binary2scalar_binary ~size ctx b_addr)))

          | (Some a_addr, None), (Some b_addr, Some b_null) ->
            let scalar_and = Scalar.Boolean_Forward.(&&) in
            let scalar_or = Scalar.Boolean_Forward.(||) in
            let scalar_not = Scalar.Boolean_Forward.not in

            scalar_or ctx 
              (scalar_and ctx (scalar_not ctx b.is_address)
                (op1 ~size ctx (SubAddress.binary2scalar_binary ~size ctx a_addr) b_null))
              (scalar_and ctx b.is_address
                (op1 ~size ctx (SubAddress.binary2scalar_binary ~size ctx a_addr) (SubAddress.binary2scalar_binary ~size ctx b_addr)))

          | (None, None), _ 
          | _, (None, None) -> Scalar.boolean_empty ctx

          | (Some a_addr, Some a_null), (Some b_addr, Some b_null) -> Scalar.boolean_unknown ctx (* TODO : check if this is correct *)
          
          (*  let scalar_and = Scalar.Boolean_Forward.(&&) in
            let scalar_or = Scalar.Boolean_Forward.(||) in
            let scalar_not = Scalar.Boolean_Forward.not in
            let scalar_bool_eq ctx x y = Scalar.Binary_Forward.(beq ~size:1 ctx (bofbool ~size:1 ctx x) (bofbool ~size:1 ctx y)) in
            let scalar_eq = Scalar.Binary_Forward.beq in

            scalar_or ctx
              (scalar_and ctx
                (scalar_and ctx
                  (scalar_bool_eq ctx a.is_address b.is_address)
                  (scalar_eq ~size ctx a_null b_null)
                )
                (scalar_eq ~size ctx (SubAddress.binary2scalar_binary ~size ctx a_addr) (SubAddress.binary2scalar_binary ~size ctx b_addr))
              )
              (scalar_and ctx
                (scalar_and ctx
                  (scalar_not ctx (scalar_bool_eq ctx a.is_address b.is_address))
                  (scalar_eq ~size ctx (SubAddress.binary2scalar_binary ~size ctx a_addr) b_null)
                )
                (scalar_eq ~size ctx a_null (SubAddress.binary2scalar_binary ~size ctx b_addr))
              )
          *)
        in
        if invalid then Log.warning (fun p -> p "invalid operation");
        res

      ;;

      let quadri2scalar_bool ctx quadri =
      let open Lattices.Quadrivalent in
      match quadri with
      | True -> Scalar.Boolean_Forward.true_ ctx
      | False -> Scalar.Boolean_Forward.false_ ctx
      | Bottom -> Scalar.boolean_empty ctx
      | Top -> Scalar.boolean_unknown ctx

      let binpred_op op =
        match op with
        | B_EQ -> Scalar.Binary_Forward.beq
        | B_SLE -> Scalar.Binary_Forward.bisle
        | B_ULE -> Scalar.Binary_Forward.biule
        | B_NEQ ->
            fun ~size ctx x y ->
              Scalar.(Boolean_Forward.not ctx @@ Binary_Forward.beq ~size ctx x y)
        | B_NSLE ->
            fun ~size ctx x y ->
              Scalar.(Boolean_Forward.not ctx @@ Binary_Forward.bisle ~size ctx x y)
        | B_NULE ->
            fun ~size ctx x y ->
              Scalar.(Boolean_Forward.not ctx @@ Binary_Forward.biule ~size ctx x y)

      (** Elementary boolean types: quadrivalent or binpred *)
      let boolean_simple_to_binary ctx b =
        match b with
        | BVal q -> quadri2scalar_bool ctx q
        | BBinpred {pred; size; left; right } ->
            let op = binpred_op pred in
            ar2 ~size op ctx left right
        | BValid _ -> assert false

      (** Simple boolean AST, not is cast down on operator or quadrivalent *)
      let rec boolean_tree_to_binary ctx b =
        match b with
        | BSimple v -> boolean_simple_to_binary ctx v
        | BAnd (b1,b2) -> 
          Scalar.Boolean_Forward.(&&) ctx
            (boolean_tree_to_binary ctx b1)
            (boolean_tree_to_binary ctx b2)
        | BOr (b1,b2) -> 
          Scalar.Boolean_Forward.(||) ctx
            (boolean_tree_to_binary ctx b1)
            (boolean_tree_to_binary ctx b2)

    end

    module Boolean_Forward:Transfer_functions.Boolean_Forward
      with type boolean := boolean
      and module Arity = Domain_sig.Context_Arity_Forward(Scalar.Context)
    = struct

      module Arity = Domain_sig.Context_Arity_Forward(Scalar.Context)

      let not_simple = function
        | BVal x -> BVal (Single_value_abstraction.Quadrivalent.Boolean_Forward.not x)
        | BBinpred{pred;size;left;right} -> BBinpred{pred=not_binpred pred;size;left;right}
        | BValid _ -> assert false
      let rec not_tree ctx = function
        | BSimple b -> BSimple (not_simple b)
        | BAnd(l,r) -> or_tree (not_tree ctx l) (not_tree ctx r)
        | BOr(l,r) -> and_tree (not_tree ctx l) (not_tree ctx r)
      let not ctx x = not_tree ctx x

      let true_ _ctx = (BSimple (BVal (Lattices.Quadrivalent.True)))
      let false_ _ctx = (BSimple (BVal (Lattices.Quadrivalent.False)))

      let (&&) _ctx l r = and_tree l r
      let (||) _ctx l r = or_tree l r
    end

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
      | True -> SubAddress.Boolean_Forward.true_ ctx
      | False -> SubAddress.Boolean_Forward.false_ ctx
      | Bottom -> SubAddress.boolean_empty ctx
      | Top -> SubAddress.boolean_unknown ctx

    let option_map x f = match x with
      | None -> None
      | Some a -> Some (f a)
    ;;

    let binary_empty ~size ctx = 
      { is_address = Scalar.boolean_empty ctx;
        id = fresh_id (); 
        nullE = None;
        addressE = None;
        invalid = false }

    (** Downcast binary operator to SubAddress booleans *)
    let rec addr_predicate ~size ctx = function
      | B_EQ -> SubAddress.beq ~size ctx
      (* Maybe: we could compare the bitvector representation of pointers here. *)
      | B_SLE -> fun _ _ -> SubAddress.boolean_unknown ctx
      | B_ULE -> SubAddress.ble ~size ctx
      | not_op -> fun l r -> SubAddress.Boolean_Forward.not ctx
          (addr_predicate size ctx (not_binpred not_op) l r)

    (** Downcast binary operator to Scalar booleans *)
    let rec scal_predicate ~size ctx = function
      | B_EQ -> Scalar.Binary_Forward.beq ~size ctx
      | B_SLE -> Scalar.Binary_Forward.bisle ~size ctx
      | B_ULE -> Scalar.Binary_Forward.biule ~size ctx
      | not_op -> fun l r -> Scalar.Boolean_Forward.not ctx
          (scal_predicate size ctx (not_binpred not_op) l r)


    let scal_is_singleton ~size ctx addr =
      Scalar.Query.binary_is_singleton ~size (Scalar.Query.binary ~size ctx addr)

    let addr_is_singleton ~size ctx addr =
      SubAddress.Query.binary_is_singleton ~size (SubAddress.Query.binary ~size ctx addr)



    module Query = struct
      module Boolean_Lattice = Lattices.Quadrivalent

      (** Boolean query response *)
      type bool_response =
        | BR_Known of Lattices.Quadrivalent.boolean (** Known quantity *)
        | BR_Scal of Scalar.boolean (** Query to perform on scalars*)
        | BR_Addr of SubAddress.boolean (** Query to perform on address *)

      (** Evaluate the response to a quadrivalent value *)
      let eval_response ctx = function
        | BR_Known k -> k
        | BR_Addr a -> SubAddress.query_boolean ctx a
        | BR_Scal s -> Scalar.query_boolean ctx s

      (** Performs binary operation op on a response *)
      let op_response ctx op_known op_scal op_addr l r =
        match l,r with
        | BR_Known Bottom, _
        | _, BR_Known Bottom -> BR_Known Bottom
        | BR_Known l, BR_Known r -> BR_Known (op_known l r)
        | BR_Scal a, BR_Scal b -> BR_Scal (op_scal ctx a b)
        | BR_Addr a, BR_Addr b -> BR_Addr (op_addr ctx a b)
        | BR_Known k, BR_Scal s
        | BR_Scal s, BR_Known k -> BR_Scal (op_scal ctx s (quadri2scalar_bool ctx k))
        | BR_Known k, BR_Addr a
        | BR_Addr a, BR_Known k -> BR_Addr (op_addr ctx a (quadri2addr_bool ctx k))
        | _ ->
          let l = eval_response ctx l in
          let r = eval_response ctx r in
          BR_Known (op_known l r)

      let join_scalar_bool ctx l r =
        let Context.Result(_,tup,deserialize2) = Scalar.serialize_boolean ctx l ctx r
            (true, Context.empty_tuple ()) in
        let res_tup = Scalar.nondet_same_context ctx tup in
        fst @@ deserialize2 ctx res_tup

      let join_addr_bool ctx l r =
        let Context.Result(_,tup,deserialize2) = SubAddress.serialize_boolean ctx l ctx r
            (true, Context.empty_tuple ()) in
        let res_tup = Scalar.nondet_same_context ctx tup in
        fst @@ deserialize2 ctx res_tup

      (** Joins two responses (boolean lattice union) *)
      let join_responses ctx l r = match l,r with
        | BR_Known Top, _
        | _, BR_Known Top -> BR_Known Top
        | BR_Known Bottom, s
        | s, BR_Known Bottom -> s
        | _ -> op_response ctx
            Lattices.Quadrivalent.join
            join_scalar_bool
            join_addr_bool l r

      let and_response ctx l r = match l,r with
        | BR_Known True, s
        | s, BR_Known True -> s
        | _ -> op_response ctx
            Single_value_abstraction.Quadrivalent.Boolean_Forward.(&&)
            Scalar.Boolean_Forward.(&&)
            SubAddress.Boolean_Forward.(&&) l r

      let or_response  ctx l r = match l,r with
        | BR_Known False, s
        | s, BR_Known False -> s
        | _ -> op_response ctx
            Single_value_abstraction.Quadrivalent.Boolean_Forward.(||)
            Scalar.Boolean_Forward.(||)
            SubAddress.Boolean_Forward.(||) l r

      (** Returns a triplet (boolean on null, boolean on address, explicit value) 
          which is to be joined to get the final evaluation *)
      (* Note: this is somewhat redundant with assume, as we cann join between 
        assume true and assume false to get this. *)
      let boolean_simple ctx boolean =
        (* Codex_log.feedback "booleansimple"; *)
        match boolean with
        | BVal x -> BR_Known x
        | BValid{size;arg} ->
          let arg_null, arg_address = decomp ctx arg in          
          (* Codex_log.feedback "boolean_simple BValid %b %b" (arg_address = None) (arg_null = None); *)
          let cond_null = 
            match arg_null, Codex_config.valid_absolute_addresses() with
            | None, _ -> None
            | Some _, None -> Some(Scalar.Boolean_Forward.false_ ctx)
            | Some null, Some (min,max) ->
              let ptr_size = Codex_config.ptr_size() in
              assert(size mod 8 == 0);
              let bytesize = size / 8 in
              let max = (Z.sub max @@ Z.of_int bytesize) in
              let cond = 
                Scalar.Boolean_Forward.(&&) ctx
                  (Scalar.Binary_Forward.biule ~size:ptr_size ctx (Scalar.Binary_Forward.biconst ~size:ptr_size min ctx) null)
                  (Scalar.Binary_Forward.biule ~size:ptr_size ctx null @@ Scalar.Binary_Forward.biconst ~size:ptr_size max ctx)
              in
              (* Codex_log.feedback "Boolean simple size %d cond = %a %s %s %a %a" size Scalar.Binary.pretty null (Z.to_string min) (Z.to_string max) Scalar.Boolean.pretty cond (Scalar.boolean_pretty ctx) cond; *)
              Some(cond)
          in
          let cond_address = match arg_address with
            | None -> None
            | Some x -> Some(SubAddress.within_bounds ~size ctx x)
          in
          begin match cond_null, cond_address with
            | None, None -> BR_Known (Lattices.Quadrivalent.Bottom)
            | Some null, None -> BR_Scal null
            | None, Some addr -> BR_Addr addr
            | Some null, Some addr -> join_responses ctx (BR_Scal null) (BR_Addr addr)
          end
        | BBinpred{pred=binpred; size; left; right } ->
          (* Codex_log.feedback "booleansimple.bbinpred"; *)
          let open Lattices.Quadrivalent in
          if left.invalid || right.invalid then BR_Known Top else
          let lscal_empty, laddr_empty = decomp ctx left in
          let rscal_empty, raddr_empty = decomp ctx right in
          let cast_scal l r = BR_Scal (scal_predicate ~size ctx binpred l r) in
          let cast_addr l r = BR_Addr (addr_predicate ~size ctx binpred l r) in
          let join_mixed = BR_Known (binpred_cross_comp binpred) in
          match lscal_empty, laddr_empty, rscal_empty, raddr_empty with
          | None, None, _, _
          | _, _, None, None -> BR_Known Bottom
          | None, _, _, None
          | _, None, None, _ -> join_mixed
          | Some lscal, Some laddr, Some rscal, Some raddr ->
              join_responses ctx
                (join_responses ctx (cast_scal lscal rscal) join_mixed)
                (join_responses ctx (cast_addr laddr raddr) join_mixed)
          | Some lscal, None, Some rscal, None ->
              cast_scal lscal rscal
          | Some lscal, _, Some rscal, _ ->
              join_responses  ctx (cast_scal lscal rscal) join_mixed
          | None, Some laddr, None, Some raddr ->
              cast_addr laddr raddr
          | _, Some laddr, _, Some raddr ->
              join_responses  ctx (cast_addr laddr raddr) join_mixed

      let rec boolean_tree ctx = function
        | BSimple b -> boolean_simple ctx b
        | BAnd(l,r) -> and_response ctx (boolean_tree ctx l) (boolean_tree ctx r)
        | BOr(l,r) -> or_response  ctx (boolean_tree  ctx l) (boolean_tree ctx r)

      let boolean ctx bool =
        let res = eval_response ctx (boolean_tree ctx bool) in
        res

      let boolean_tree ctx bool = boolean_tree ctx bool

      let convert_to_quadrivalent (x : Lattices.Quadrivalent.boolean) = x

      module Binary_Lattice = struct

        type t = {
          null: Scalar.Query.Binary_Lattice.t;
          address: bool;        (* false = is not an address. *)
          invalid: bool;
        }

        let top ~size = assert false
        let singleton ~size = assert false
        let hash _ = assert false
        let equal _ _ = assert false
        let compare _ _ = assert false
        let is_included _ _ = assert false
        let inter ~size _ _ = assert false
        let includes ~size a b = is_included b a
        let includes_or_widen ~size ~previous _ = assert false
        let widen ~size ~previous _ = assert false
        let is_bottom ~size x = x.invalid = false && x.address = false
                                && Scalar.Query.Binary_Lattice.is_bottom ~size x.null
                                (* && SubAddress.Query.Binary_Lattice.is_bottom ~size x.address;; *)
        let bottom ~size = { address = false;
                            null=Scalar.Query.Binary_Lattice.bottom ~size;
                            invalid = false }
        ;;

        let pretty ~size fmt x = assert false

        let join ~size a b =
          { null = Scalar.Query.Binary_Lattice.join ~size a.null b.null;
            address = a.address || b.address;
            invalid = a.invalid || b.invalid
          }

      end

      let binary ~size ctx x =
        let x_null, x_address = decomp ctx x in        
        let null = match x_null with
          | None -> Scalar.Query.Binary_Lattice.bottom ~size
          | Some off -> Scalar.Query.binary ~size ctx off
        in
        let address = match x_address with
          | None -> false
          | Some _ -> true
        in
        (*   | None -> SubAddress.Query.Binary_Lattice.bottom ~size *)
        (*   | Some off -> SubAddress.Query.binary ~size ctx off *)
        (* in *)
        (Binary_Lattice.{invalid=x.invalid;null;address})

      let binary_to_ival ~signed ~size Binary_Lattice.{null;address;invalid} =
        Scalar.Query.binary_to_ival ~signed ~size null

      let binary_to_known_bits  ~size x = assert false
      let binary_fold_crop ~size (x:Binary_Lattice.t) ~inf ~sup acc f =
        (* let x_null, x_address = decomp ctx x in *)
        match x.null, x.address with
        | _, true -> assert false (* Not handled *)
        | null, false ->
          Scalar.Query.binary_fold_crop ~size null ~inf ~sup acc f
      ;;

      let binary_is_empty ~size b = Binary_Lattice.is_bottom ~size b

      let binary_is_singleton ~size (x:Binary_Lattice.t) =
        (* Focus on the most interesting case *)
        if x.invalid || x.address
        then None
        else
          Scalar.Query.binary_is_singleton ~size x.null
    end

    
    let null_addr_overlaps ~size ctxa null ctxb addr = 
      let null_val = Scalar.Query.(binary_to_ival ~signed:true ~size @@ binary ~size ctxa null) in
      let addr_val = Scalar.Query.(binary_to_ival ~signed:false ~size @@
            binary ~size ctxb @@ SubAddress.binary2scalar_binary ~size ctxb addr) in

      Framac_ival.Ival.intersects null_val addr_val
    
    let rec serialize_boolean_simple
      : 'a. Context.t -> boolean_simple -> Context.t -> boolean_simple -> (bool * 'a Context.in_tuple) -> (boolean_simple, 'a) Context.result
      = fun ctxa a ctxb b ((inc,intup) as acc) ->
      match a,b with
      | BVal va, BVal vb ->
          Context.Result(inc, intup, fun ctx out -> BVal (Lattices.Quadrivalent.join va vb), out)
      | BValid {size=sza; arg=va}, BValid {size=szb; arg=vb} when sza = szb ->
          let Context.Result (inc, intup, des) = serialize ~size:sza ctxa va ctxb vb acc in
          Context.Result(inc, intup, fun ctx out -> let v,out = des ctx out in BValid {size=sza; arg=v}, out)
      | BBinpred {pred=pa;size=sza;left=la;right=ra}, BBinpred {pred=pb;size=szb;left=lb;right=rb}
          when sza = szb && pa = pb
        ->
          let Context.Result (inc, intup, desl) = serialize ~size:sza ctxa la ctxb lb acc in
          let Context.Result (inc, intup, desr) = serialize ~size:szb ctxa ra ctxb rb (inc,intup) in
          Context.Result(inc, intup, fun ctx out ->
            let r,out = desr ctx out in
            let l,out = desl ctx out in
            BBinpred {pred=pa; size=sza; left=l; right=r}, out
          ) 
      
      | _ -> assert false (* Context.Result(inc, intup, fun ctx out -> BVal (Sub.boolean_unknown ctx), out) *)
          
    and serialize_both_boolean
      : 'a. Context.t -> boolean -> Context.t -> boolean -> (bool * 'a Context.in_tuple) -> (boolean, 'a) Context.result
      = fun ctxa a ctxb b (inc,intup) ->
      match a,b with
      | BSimple ba, BSimple bb ->
        let Context.Result(inc,intup,des) = serialize_boolean_simple ctxa ba ctxb bb (inc,intup) in
        Context.Result (inc, intup, fun ctx out -> let b,out = des ctx out in BSimple b, out)
      | BOr (la,ra), BOr (lb,rb) -> 
        let Context.Result(inc, intup, desl) = serialize_both_boolean ctxa la ctxb lb (inc,intup) in
        let Context.Result(inc, intup, desr) = serialize_both_boolean ctxa ra ctxb rb (inc,intup) in
        Context.Result (inc, intup, fun ctx out ->
          let r,out = desr ctx out in
          let l,out = desl ctx out in 
          BOr (l,r), out
        )
      | BAnd (la,ra), BAnd (lb,rb) -> 
        let Context.Result(inc,intup,desl) = serialize_both_boolean ctxa la ctxb lb (inc,intup) in
        let Context.Result(inc,intup,desr) = serialize_both_boolean ctxa ra ctxb rb (inc,intup) in
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
      match b with
      | BVal v -> Context.Result(inc, intup, fun ctx out -> BVal v,out)
      | BValid {size; arg} ->
          let Context.Result (inc, intup, des) = serialize ~size ctxa arg ctxdummy (binary_empty ~size ctxdummy) acc in
          Context.Result(inc, intup, fun ctx out -> let arg,out = des ctx out in BValid {size; arg}, out)
      | BBinpred {pred;size;left;right} ->
          let Context.Result (inc, intup, desl) = serialize ~size ctxa left ctxdummy (binary_empty ~size ctxdummy) acc in
          let Context.Result (inc, intup, desr) = serialize ~size ctxa right ctxdummy (binary_empty ~size ctxdummy) (inc,intup) in
          Context.Result(inc, intup, fun ctx out ->
            let right,out = desr ctx out in
            let left,out = desl ctx out in
            BBinpred {pred; size; left; right}, out
          )
          
    and serialize_boolean_with_bottom_right
      : 'a. Context.t -> Context.t -> boolean -> (bool * 'a Context.in_tuple) -> (boolean, 'a) Context.result
      = fun ctxa ctxdummy b (inc,intup) ->
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
    match b with
    | BVal v -> Context.Result(inc, intup, fun ctx out -> BVal v,out)
    | BValid {size; arg} ->
        let Context.Result (inc, intup, des) = serialize ~size ctxdummy (binary_empty ~size ctxdummy) ctxb arg acc in
        Context.Result(inc, intup, fun ctx out -> let arg,out = des ctx out in BValid {size; arg}, out)
    | BBinpred {pred;size;left;right} ->
        let Context.Result (inc, intup, desl) = serialize ~size ctxdummy (binary_empty ~size ctxdummy) ctxb left acc in
        let Context.Result (inc, intup, desr) = serialize ~size ctxdummy (binary_empty ~size ctxdummy) ctxb right (inc,intup) in
        Context.Result(inc, intup, fun ctx out ->
          let right,out = desr ctx out in
          let left,out = desl ctx out in
          BBinpred {pred; size; left; right}, out
        )
        
  and serialize_boolean_with_bottom_left
    : 'a. Context.t -> Context.t -> boolean -> (bool * 'a Context.in_tuple) -> (boolean, 'a) Context.result
    = fun ctxdummy ctxb b (inc,intup) ->
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

    and serialize_boolean
      : Context.t -> boolean -> Context.t -> boolean -> (bool * 'a Context.in_tuple) -> (boolean, 'a) Context.result
      = fun ctxa a ctxb b acc ->
      let is_bottom ctx x = 
        match Query.boolean ctx x with Lattices.Quadrivalent.Bottom -> true | _ -> false
      in
      if is_bottom ctxa a then serialize_boolean_with_bottom_left ctxa ctxb b acc
      else if is_bottom ctxb b then serialize_boolean_with_bottom_right ctxa ctxb a acc
      else serialize_both_boolean ctxa a ctxb b acc
      

    and serialize
      : 'a. size:int -> Context.t -> binary -> Context.t -> binary -> (bool * 'a Context.in_tuple) -> (binary, 'a) Context.result
      = fun ~size ctxa a ctxb b (included,acc) ->
      (* Codex_log.feedback "WHolify.OV.serialize2 %a %a "
         (binary_pretty ~size ctxa) a  (binary_pretty ~size ctxb) b; *)  
      let f_addr ctx = function
        | None -> SubAddress.binary_empty ~size ctx
        | Some a -> a
      in
      let f_null ctx = function
        | None -> Scalar.binary_empty ~size ctx
        | Some a -> a
      in
      let a_null,a_addr = decomp ctxa a in
      let b_null,b_addr = decomp ctxb b in
      (* let a_null = a.nullE and a_address = a.addressE in
      let b_null = b.nullE and b_address = b.addressE in *)

      let invalid = ref (a.invalid || b.invalid) in

      let Context.Result(included,acc,d_is_address) = Scalar.serialize_boolean ctxa a.is_address ctxb b.is_address (included,acc) in
    
      let Context.Result(included, acc, des) =
        match (a_null, a_addr), (b_null, b_addr) with
        | (None, None), (None, None) -> Context.Result(included, acc, (fun ctx out -> (None,None), out))
        
        | (Some nulla, None), (Some nullb, None) ->
            let Context.Result(included, acc, d_null) = Scalar.serialize_binary ~size ctxa nulla ctxb nullb (included,acc) in
            Context.Result(included, acc, fun ctx out ->
              let null, out = d_null ctx out in
              (Some null, None), out
            )

        | (None, Some addra), (None, Some addrb) ->
            let Context.Result(included, acc, d_address) = SubAddress.serialize ~size ctxa addra ctxb addrb (included,acc) in
            Context.Result(included, acc, fun ctx out ->
              let addr, out = d_address ctx out in
              (None, Some addr), out
            )

        (*
        | (None, Some addr), (Some null, None) ->
            if null_addr_overlaps ~size ctxb null ctxa addr
            then
              let null_a = SubAddress.binary2scalar_binary ~size ctxa addr in
              let Context.Result(included, acc, d_null) = Scalar.serialize_binary ~size ctxa null_a ctxb null (included,acc) in
              Context.Result(included, acc, fun ctx out ->
                let null, out = d_null ctx out in
                (Some null, None), out
              )
            else
              let Context.Result(included, acc, d_null) = Scalar.serialize_binary ~size ctxa (f_null ctxa None) ctxb (f_null ctxb b_null) (included,acc) in
              let Context.Result(included, acc, d_address) = SubAddress.serialize ~size ctxa (f_addr ctxa a_addr) ctxb (f_addr ctxb None) (included,acc) in
              Context.Result(included, acc, fun ctx out ->
                let addr, out = d_address ctx out in
                let null, out = d_null ctx out in
                (Some null, Some addr), out
              )

        | (Some null, None), (None, Some addr) ->
            if null_addr_overlaps ~size ctxa null ctxb addr
            then
              let null_b = SubAddress.binary2scalar_binary ~size ctxb addr in
              let Context.Result(included, acc, d_null) = Scalar.serialize_binary ~size ctxa null ctxb null_b (included,acc) in
              Context.Result(included, acc, fun ctx out ->
                let null, out = d_null ctx out in
                (Some null, None), out
              )
            else
              let Context.Result(included, acc, d_null) = Scalar.serialize_binary ~size ctxa null ctxb (f_null ctxb None) (included,acc) in
              let Context.Result(included, acc, d_address) = SubAddress.serialize ~size ctxa (f_addr ctxa None) ctxb addr (included,acc) in
              Context.Result(included, acc, fun ctx out ->
                let addr, out = d_address ctx out in
                let null, out = d_null ctx out in
                (Some null, Some addr), out
              )

        | a,b when not @@ overlaps ctxa a ctxb b -> 
            let Context.Result(included, acc, d_null) = Scalar.serialize_binary ~size ctxa (f_null ctxa a_null) ctxb (f_null ctxb b_null) (included,acc) in
            let Context.Result(included, acc, d_address) = SubAddress.serialize ~size ctxa (f_addr ctxa a_addr) ctxb (f_addr ctxb b_addr) (included,acc) in
            Context.Result(included, acc, fun ctx out ->
              let addr, out = d_address ctx out in
              let null, out = d_null ctx out in
              (Some null, Some addr), out
            )
        *)
        | _ ->
            (* TODO : improve this part as it is not totally sound right now *)
            (* Codex_log.debug "Passing in this case with a = %a and b = %a" (binary_pretty ~size ctxa) a (binary_pretty ~size ctxb) b ; *)
            let Context.Result(included, acc, d_null) = Scalar.serialize_binary ~size ctxa (f_null ctxa a_null) ctxb (f_null ctxb b_null) (included,acc) in
            let Context.Result(included, acc, d_address) = SubAddress.serialize ~size ctxa (f_addr ctxa a_addr) ctxb (f_addr ctxb b_addr) (included,acc) in
            Context.Result(included, acc, fun ctx out ->
              let addr, out = d_address ctx out in
              let null, out = d_null ctx out in
              (Some null, Some addr), out
            )

      in Context.Result(included, acc, fun ctx tup ->
          (* let addressE,tup = d_address ctx tup in          
          let nullE,tup = d_null ctx tup in *)
          let (nullE, addressE), tup = des ctx tup in
          let is_address, tup = d_is_address ctx tup in
          let res = {is_address; addressE;nullE; id = fresh_id ();
                    invalid = !invalid} in
          (* Codex_log.feedback "OV.deserialize2 @\na:%a@\nb:%a@\nres:%a" *)
          (* (binary_pretty ~size:32 ctx) a (binary_pretty ~size:32 ctx) b (binary_pretty ~size:32 ctx) res; *)
          (* Codex_log.debug "Wholify.Single.serialize no.%d returns %a" cur_counter (binary_pretty ~size ctx) res ; *)
          res,tup)
    ;;

    (** [assume_simple ctx boolean] returns a pair [bscalar, baddr]
        of booleans to assume on scalar and address components *)
    (* assume pred return pred' such that pred ==> pred', so that we can assume pred'.
      None means return true.  *)
    (* Note: we should probably not be returning two conditions at the
      same time (unless they are both just false) *)
    let assume_simple ctx = function
      | BVal x -> None, None
      | BValid{size;arg} -> begin
          let arg_null, arg_address = decomp ctx arg in
          (* None if we won't be an address afterwards. Bools say if this is new. *)
          let learn_not_address, cond_address = 
            match arg_address with
            | None -> false, None
            | Some x -> begin
              let cond_address = SubAddress.within_bounds ~size ctx x in
              match SubAddress.query_boolean ctx cond_address with
              | Lattices.Quadrivalent.(False | Bottom) -> true, None
              | _ -> false, Some cond_address
            end
          in
          let learn_not_scalar, cond_null = 
            match arg_null, Codex_config.valid_absolute_addresses() with
            | None, _ -> false, None
            | _, None -> (* Codex_log.feedback "learn not scalar"; *) true, None
            | Some null, Some (min,max) -> begin
                (* Codex_log.feedback "Some null and addreses"; *)
                let ptr_size = Codex_config.ptr_size() in
                assert(size mod 8 == 0);
                let bytesize = size / 8 in
                let max = Z.sub max @@ Z.of_int bytesize in
                let cond_null =
                  Scalar.Boolean_Forward.(&&) ctx
                    (Scalar.Binary_Forward.biule ~size:ptr_size ctx (Scalar.Binary_Forward.biconst ~size:ptr_size min ctx) null)
                    (Scalar.Binary_Forward.biule ~size:ptr_size ctx null @@ Scalar.Binary_Forward.biconst ~size:ptr_size max ctx)
                in
                begin match Scalar.query_boolean ctx cond_null with
                  | Lattices.Quadrivalent.(False | Bottom) -> true, None
                  | Lattices.Quadrivalent.(True | Top) -> false,Some(cond_null)
                end
              end
          in
          match cond_address, cond_null with
          | None, None -> Some (Scalar.Boolean_Forward.false_ ctx), Some(SubAddress.Boolean_Forward.false_ ctx)
          | Some _, Some _ -> None, None (* Cannot learn anything. *)
          | None, Some cond_null ->
            if learn_not_address
            then Some(Scalar.Boolean_Forward.(&&) ctx (Scalar.Boolean_Forward.not ctx arg.is_address) cond_null), None
            else Some(cond_null), None
          | Some cond_address, None -> 
            if learn_not_scalar
            then Some(arg.is_address), Some(cond_address)
            else None, Some(cond_address)
        end
      | BBinpred({pred=op;left; right; size}) ->

        (* Address of the one that is both a scalar and address. *)
        let scal__scal_addr is_address ascal bscal baddr =
          match op with
          | B_EQ ->
            let scalcond = scal_predicate ~size ctx op ascal bscal in
            Some(Scalar.Boolean_Forward.(&&) ctx scalcond @@
                Scalar.Boolean_Forward.not ctx is_address), None
          | B_NEQ -> begin
              (* If we compare with a singleton, we can remove it. *)
              match Scalar.Query.binary_is_singleton ~size @@ Scalar.Query.binary ~size ctx ascal,
                    Scalar.Query.binary_is_singleton ~size @@ Scalar.Query.binary ~size ctx bscal
              with
              (* We know that a must be some address. *)
              | Some a, Some b when Z.equal a b -> Some(is_address), None
              (* Note: in the Some a, None case we cannot deduce anything,
                but nonrelaional domains can. *)
              | _ -> None, None
            end
          | B_SLE | B_ULE -> begin
              (* If we compare with a singleton, we can remove it. *)
              match Scalar.Query.binary_is_singleton ~size @@ Scalar.Query.binary ~size ctx ascal,
                    Scalar.Query.binary_is_singleton ~size @@ Scalar.Query.binary ~size ctx bscal
              with
              | Some a, Some b when Z.gt a b -> Some(is_address), None
              | _ -> None, None
            end
          | B_NSLE | B_NULE -> begin
              (* If we compare with a singleton, we can remove it. *)
              match Scalar.Query.binary_is_singleton ~size @@ Scalar.Query.binary ~size ctx ascal,
                    Scalar.Query.binary_is_singleton ~size @@ Scalar.Query.binary ~size ctx bscal
              with
              | Some a, Some b when Z.leq a b -> Some(is_address), None
              | _ -> None, None
            end
        in

        let addr__scal_addr is_address aaddr bscal baddr =
          match op with
          | B_EQ -> Some(is_address), Some(addr_predicate ~size ctx op aaddr baddr)
          (* We could try to learn something from NEQ, but probably not worth it. *)
          | _ -> None, None
        in

        
        if left.invalid || right.invalid then None, None else
        let lscal, laddr = decomp ctx left in
        let rscal, raddr = decomp ctx right in
        match lscal, laddr, rscal, raddr with
        | None, None, _, _
        | _, _, None, None ->
          Some(Scalar.Boolean_Forward.false_ ctx), Some(SubAddress.Boolean_Forward.false_ ctx)
        | None, _, _, None
        | _, None, None, _ when (binpred_is_positive op) ->
          Some(Scalar.Boolean_Forward.false_ ctx), Some(SubAddress.Boolean_Forward.false_ ctx)
        | None, _, _, None
        | _, None, None, _ ->
          None, None
        | Some ls, None, Some rs, None -> Some (scal_predicate ~size ctx op ls rs), None
        | None, Some la, None, Some ra -> None, Some (addr_predicate ~size ctx op la ra)

        (* Note: for eq, we can deduce that both booleans are equal. *)
        | Some ls, Some la, Some rs, Some ra -> None, None

        | Some ls, Some la, Some rs, None ->
          scal__scal_addr left.is_address rs ls la
        | Some ls, None, Some rs, Some ra ->
          scal__scal_addr right.is_address ls rs ra
    

        | Some ls, Some la, None, Some ra ->
          addr__scal_addr left.is_address ra ls la
        | None, Some la, Some rs, Some ra ->
          addr__scal_addr right.is_address la rs ra
    ;;   


    let rec assume_tree ctx b =
      match b with
      | BSimple b -> assume_simple ctx b
      | BAnd(l,r) ->
          let s_l, a_l = assume_tree  ctx l in
          let s_r, a_r = assume_tree  ctx r in
          option_merge (Scalar.Boolean_Forward.(&&) ctx) s_l s_r,
          option_merge (SubAddress.Boolean_Forward.(&&) ctx) a_l a_r
      | BOr(l,r) ->
          let s_l, a_l = assume_tree  ctx l in
          let s_r, a_r = assume_tree  ctx r in
          (* eturn None if one is none (assume (cond || true)) *)
          option_map2 (Scalar.Boolean_Forward.(||) ctx) s_l s_r,
          option_map2 (SubAddress.Boolean_Forward.(||) ctx) a_l a_r

    let assume ctx cond =
      let a,b = assume_tree  ctx cond in
      let ctx =
        match a with
        | None -> Some ctx
        | Some a -> (* Codex_log.feedback "Assumed condition: %a" (Scalar.boolean_pretty ctx) a; *)
          Scalar.assume ctx a
      in
      match ctx with
      | None -> None
      | Some ctx -> 
        begin match b with
          | None -> Some ctx
          | Some b ->
            (* Codex_log.feedback "Assumed condition2:"; *)
            SubAddress.assume ctx b
        end
    ;;

    let imperative_assume ctx cond =
      let a,b = assume_tree  ctx cond in
      match a with None -> () | Some a -> Scalar.imperative_assume ctx a;
      match b with None -> () | Some b -> SubAddress.imperative_assume ctx b
    ;;      

    let assume ctx cond =
      (* Codex_log.feedback "assume: evaluation of cond %a: res = %a" Boolean.pretty cond Lattices.Quadrivalent.pretty (Query.boolean ctx cond); *)
      assume ctx cond
    ;;

    let imperative_assume ctx cond =
      (* Codex_log.feedback "imperative_assume: evaluation of cond %a: res = %a" Boolean.pretty cond Lattices.Quadrivalent.pretty (Query.boolean ctx cond); *)
      imperative_assume ctx cond

    
    let satisfiable_quadri ctx x =
      let open Lattices.Quadrivalent in
      match x with
      | Bottom | False -> Smtbackend.Smtlib_sig.Unsat
      | Top | True -> Scalar.satisfiable ctx (Scalar.Boolean_Forward.true_ ctx)

    let satisfiable ctx b =
      match Query.boolean_tree ctx b with
      | BR_Known x -> satisfiable_quadri ctx x
      | BR_Scal s -> Scalar.satisfiable ctx s
      | BR_Addr a -> SubAddress.satisfiable ctx a

    let join_values ~size ctx list =
        match list with
        | [] -> Scalar.binary_empty ~size ctx
        | [x]-> x
        | a::b ->
          let nondet_binary v1 v2 = 
            let Context.Result(_,tup,deserialize2) = Scalar.serialize_binary ~size ctx v1 ctx v2
                (true, Context.empty_tuple ()) in
            let res_tup = Scalar.nondet_same_context ctx tup in
            fst @@ deserialize2 ctx res_tup
          in List.fold_left nondet_binary a b
      ;;

    (* What to put in invalid here is unclear. This operation can be
        used for completely unknon values. *)
    let binary_unknown ~size ctx =
      { is_address = Scalar.Boolean_Forward.false_ ctx;
        nullE = Some(Scalar.binary_unknown ~size ctx);
        id = fresh_id ();
        addressE = None;
        invalid = false;
      }

    module Binary_Forward = struct

      let buninit ~size ctx = binary_empty ~size ctx (* binary_unknown ~size ctx *)

      (* Operations on pointers. *)

      let numeric_operation ~size ctx null =
        {
          is_address = Scalar.Boolean_Forward.false_ ctx;
          id = fresh_id();
          nullE = Some null;
          addressE = None;
          invalid = false
        }
      ;;

      let biconst ~size k ctx = numeric_operation ~size ctx @@ Scalar.Binary_Forward.biconst ~size k ctx

      let bofbool ~size ctx x =
        (* Codex_log.debug "Wholify.bofbool %a" (boolean_pretty ctx) x ; *)
        let scalar_bool = quadri2scalar_bool ctx @@ Query.boolean ctx x in
        (* let scalar_bool = Boolean_to_Binary.boolean_tree_to_binary ctx x in *)
        let res =
        numeric_operation ~size ctx @@ Scalar.Binary_Forward.bofbool ~size ctx scalar_bool
        in res

      let bchoose ~size cond ctx x =
        let null, addr = decomp ctx x in
        let x = {x with nullE = null; addressE = addr} in
        {
          is_address = x.is_address; (* XXX: Should probably use bchoose on booleans. *)
          id = fresh_id ();
          nullE = option_map x.nullE @@ (fun x -> Scalar.Binary_Forward.bchoose ~size cond ctx x);
          addressE = option_map x.addressE @@ (fun x -> SubAddress.bchoose ~size cond ctx x);
          invalid = x.invalid }
      ;;

      let bindex ~size k ctx x e =
        (* Codex_log.debug "Wholify.Single.bindex k:%d %a %a" k (binary_pretty ~size ctx) x (binary_pretty ~size ctx) e ; *)
        let e_null, _ = decomp ctx e in
        let x_null, x_address = decomp ctx x in        
        match e_null with
        | None -> binary_empty ~size ctx 
        | Some e ->
          {
            is_address = x.is_address;
            id = fresh_id ();
            nullE = option_map x_null (fun x -> 
                let k = Scalar.Binary_Forward.biconst ~size (Z.of_int k) ctx in
                let off =Scalar.Binary_Forward.bimul ~size ~nsw:false ~nuw:false ctx k e in
                Scalar.Binary_Forward.biadd ~size ~nsw:false ~nuw:false ~nusw:false ctx x off);
            addressE = option_map x_address (fun x -> SubAddress.bindex ~size k ctx x e);
            invalid = x.invalid;
          }
      ;;


      let valid ~size _acc_type ctx (ptr:binary) =
        (BSimple (BValid{size;arg=ptr}))

      (* Common to arithmetical unary operations. *)
      let ar1 op1 ~size ctx x =
        let x_null, x_address = decomp ctx x in
        let invalid = x.invalid || (x_address <> None && x_null <> None) (* || (x_address <> None) *) in
        let nullE =
          match x_null, x_address with
          | Some x, None -> Some (op1 ~size ctx x)
          | None, Some x -> Some (op1 ~size ctx @@ SubAddress.binary2scalar_binary ~size ctx x)
          | Some x, Some y -> Some (Scalar.binary_unknown ~size ctx) (* assert false *)
          | _ -> None
        in
        {is_address = Scalar.Boolean_Forward.false_ ctx;
        id = fresh_id (); nullE;addressE=None;invalid}

        (* let nullE = match x_null with None -> None | Some x -> Some(op1 ~size ctx x) in
        {is_address = Scalar.Boolean_Forward.false_ ctx;
        id = fresh_id (); nullE;addressE=None;invalid} *)
      ;;

      let buext ~size ~oldsize ctx x =
        if size == oldsize then x
        else ar1 (Scalar.Binary_Forward.buext ~oldsize) ~size ctx x;;

      let bsext ~size ~oldsize ctx x =
        if size == oldsize then x
        else ar1 (Scalar.Binary_Forward.bsext ~oldsize) ~size ctx x;;

      let bextract ~size ~index ~oldsize ctx x =
        (* Codex_log.feedback "region_separation.bextract"; *)
        if size == oldsize then x
        else ar1 (Scalar.Binary_Forward.bextract ~index ~oldsize) ~size ctx x
      ;;


      (* Common to arithmetical binary operations. *)
      let ar2 op1 ~size ctx a b =
        (* Codex_log.debug "Wholifiy.ar2 a:(%a) b:(%a)" (binary_pretty ~size ctx) a (binary_pretty ~size ctx) b ; *)
        let a_null, a_address = decomp ctx a in
        let b_null, b_address = decomp ctx b in
        (* let invalid = a.invalid || b.invalid || (a_address <> None) || (b_address <> None) in
        let nullE = match a_null, b_null with
          | Some a, Some b -> Some(op1 ~size ctx a b)
          | _ -> None
        in *)
        let invalid = a.invalid || b.invalid in
        let nullE =
        match (a_address, a_null), (b_address, b_null) with
        | (None, Some a), (None, Some b) -> Some (op1 ~size ctx a b)
        | (Some a, None), (None, Some b) -> Some (op1 ~size ctx (SubAddress.binary2scalar_binary ~size ctx a) b) 
        | (None, Some a), (Some b, None) -> Some (op1 ~size ctx a (SubAddress.binary2scalar_binary ~size ctx b))
        | (Some a, None), (Some b, None) -> 
            Some (op1 ~size ctx (SubAddress.binary2scalar_binary ~size ctx a) (SubAddress.binary2scalar_binary ~size ctx b))

        | (Some addr, Some null), (None, Some b) ->
            let v1 = op1 ~size ctx null b in
            let v2 = op1 ~size ctx (SubAddress.binary2scalar_binary ~size ctx addr) b in
            Some (join_values ~size ctx [v1;v2])

        | (None, Some a), (Some addr, Some null) ->
            let v1 = op1 ~size ctx a null in
            let v2 = op1 ~size ctx a (SubAddress.binary2scalar_binary ~size ctx addr) in
            Some (join_values ~size ctx [v1;v2])

        | (None, None), _ -> None
        | _, (None, None) -> None

        | _ -> assert false
        in
        if invalid && not (a.invalid || b.invalid)
        then Log.warning (fun p -> p "invalid operation");
        { is_address = Scalar.Boolean_Forward.false_ ctx;
          nullE;addressE=None;invalid; id = fresh_id () }

      ;;

      let bimul ~size ~nsw ~nuw ctx a b = ar2 (Scalar.Binary_Forward.bimul ~nsw ~nuw) ~size ctx a b
      let bxor ~size ctx a b = ar2 (Scalar.Binary_Forward.bxor) ~size ctx a b
      let band ~size ctx a b = ar2 (Scalar.Binary_Forward.band) ~size ctx a b

      let bor ~size ctx a b = ar2 (Scalar.Binary_Forward.bor) ~size ctx a b

      let bashr ~size ctx a b = ar2 (Scalar.Binary_Forward.bashr) ~size ctx a b
      let blshr ~size ctx a b = ar2 (Scalar.Binary_Forward.blshr) ~size ctx a b
      let bshl ~size ~nsw ~nuw ctx a b = ar2 (Scalar.Binary_Forward.bshl ~nsw ~nuw)   ~size ctx a b                    

      let bisdiv ~size ctx a b = ar2 (Scalar.Binary_Forward.bisdiv) ~size ctx a b
      let bismod ~size ctx a b = ar2 (Scalar.Binary_Forward.bismod) ~size ctx a b        
      let biudiv ~size ctx a b = ar2 (Scalar.Binary_Forward.biudiv) ~size ctx a b
      let biumod ~size ctx a b = ar2 (Scalar.Binary_Forward.biumod) ~size ctx a b        

      
      let bconcat ~size1 ~size2 ctx a b =
        if size1 = 0 then b
        else if size2 = 0 then a
        else
          let a_null, a_address = decomp ctx a in
          let b_null, b_address = decomp ctx b in

          let nulla = match a_null, a_address with
            | Some null, Some addr -> 
              let v = SubAddress.binary2scalar_binary ~size:size1 ctx addr in
              Some (join_values ~size:size1 ctx [null; v])
            | None, Some addr -> Some (SubAddress.binary2scalar_binary ~size:size1 ctx addr)
            | _ -> a_null
          in

          let nullb = match b_null, b_address with
            | Some null, Some addr ->
              let v = SubAddress.binary2scalar_binary ~size:size2 ctx addr in
              Some (join_values ~size:size2 ctx [null; v])
            | None, Some addr -> Some (SubAddress.binary2scalar_binary ~size:size2 ctx addr)
            | _ -> b_null
          in

          let invalid = a.invalid || b.invalid
              || (a_address <> None && a_null <> None) || (b_address <> None && b_null <> None) in

          let nullE =
            match nulla, nullb with
            | Some a, Some b -> Some (Scalar.Binary_Forward.bconcat ~size1 ~size2 ctx a b)
            | _ -> None
          in
          { is_address = Scalar.Boolean_Forward.false_ ctx;
              nullE;addressE=None;invalid; id = fresh_id ()}
      ;;

      let bool_binpred op ~size (_ctx:Scalar.Context.t) left right =
        (BSimple (BBinpred{pred=op; size; left; right }))

      let biule = bool_binpred B_ULE
      let bisle = bool_binpred B_SLE
      let beq = bool_binpred B_EQ

      let valid_ptr_arith ~size arith_typ ctx x y =
        Log.debug (fun p -> p "Wholify.valid_ptr_arith %a %a" (binary_pretty ~size ctx) x (binary_pretty ~size ctx) y);
        let x_null, x_address = decomp ctx x in
        let y_null, y_address = decomp ctx y in

        match (x_null,x_address),(y_null,y_address) with
        | (None, None), _
        | _, (None, None) -> boolean_empty ctx
        | (_, Some _),(_, Some _) when arith_typ = Transfer_functions.Plus ->
            Log.warning (fun p -> p "@[<hov 2>Addition of pointers %a and %a is probably an error@]" (binary_pretty ~size ctx) x (binary_pretty ~size ctx) y);
            Boolean_Forward.false_ ctx
        | (_, Some _),(_, Some _) (* when arith_typ = Transfer_functions.Minus *) ->
            Boolean_Forward.(&&) ctx (valid ~size Transfer_functions.Read ctx x) (valid ~size Transfer_functions.Read ctx y)
        | (Some _, None),(Some _, None) -> Boolean_Forward.true_ ctx
        | (_, Some _), _ -> valid ~size Transfer_functions.Read ctx x
        | _, (_, Some _) -> valid ~size Transfer_functions.Read ctx y

      (* TODO: Maybe we should raise alarm, even if we count them only
        after the fixpoint is reached, or we could perform a pass to
        collect the alarms. That would be simpler. *)

      (* MAYBE: Si l'un etait un pointeur et l'autre pas, transformer en bshift ou bindex si on peut. *)
      let biadd ~size ~nsw ~nuw ~nusw ctx a b =
        (* Codex_log.debug "Wholify.biadd %a %a" (binary_pretty ~size ctx) a (binary_pretty ~size ctx) b ; *)
        let a_null, a_address = decomp ctx a in
        let b_null, b_address = decomp ctx b in                
        let nullE = match a_null,b_null with
          | Some a, Some b -> Some(Scalar.Binary_Forward.biadd ~size ~nsw ~nuw ~nusw ctx a b)
          | _ -> None
        in
        (* Translate to index calls *)
        let is_address, addressE = 
          match (a_address, a_null), (b_address, b_null) with
          | (Some a, None), (None, Some b) -> Scalar.Boolean_Forward.true_ ctx, Some (SubAddress.bindex ~size 1 ctx a b)
          | (None, Some a), (Some b, None) -> Scalar.Boolean_Forward.true_ ctx, Some (SubAddress.bindex ~size 1 ctx b a)
          | (None, Some _), (None, Some _) -> Scalar.Boolean_Forward.false_ ctx, None
          | (None, None), (None, None) -> Scalar.Boolean_Forward.false_ ctx, None
          | (Some addr, Some null), (None, Some b) -> Scalar.boolean_unknown ctx, Some (SubAddress.bindex ~size 1 ctx addr b)
          | (None, Some a), (Some addr, Some null) -> Scalar.boolean_unknown ctx, Some (SubAddress.bindex ~size 1 ctx addr a)
          
          | (None, None), _ -> Scalar.boolean_empty ctx, None
          | _, (None, None) -> Scalar.boolean_empty ctx, None

          | _ -> assert false
        in
        (* if (a_null <> None && b_address <> None) || (a_address <> None && b_null <> None) then assert false *)
        (* else *)
        let res =
          let invalid = a.invalid || b.invalid || (b_address <> None && a_address <> None) in
          { is_address; nullE; addressE; invalid; id = fresh_id ()}

        in (* Codex_log.debug "Wholify.Single.biadd returning %a" (binary_pretty ~size ctx) res ;  *)res
      ;;

      
      let bisub ~size ~nsw ~nuw ~nusw ctx a b =
        (* Codex_log.debug "Wholify.Single.bisub %a %a" (binary_pretty ~size ctx) a (binary_pretty ~size ctx) b ; *)
        let invalid = a.invalid || b.invalid in
        let a_null, a_address = decomp ctx a in
        let b_null, b_address = decomp ctx b in                
        (* let l = [] in *)
        let a_null, a_address =
          match a_null, a_address with
          | Some null, Some addr -> (* Some (join_values ~size ctx [null; (SubAddress.binary2scalar_binary ~size ctx addr)]) , None *)
              Some (Scalar.binary_unknown ~size ctx), None 
          | _ -> a_null, a_address
        in
        let b_null, b_address =
          match b_null, b_address with
          | Some null, Some addr -> (* Some (join_values ~size ctx [null; (SubAddress.binary2scalar_binary ~size ctx addr)]), None *)
              Some (Scalar.binary_unknown ~size ctx), None 
          | _ -> b_null, b_address
        in  
        (* let l = match a_null,b_null with
          | Some a, Some b -> (Scalar.Binary_Forward.bisub ~size ~nsw ~nuw ctx a b)::l
          | _ -> l
        in
        let l = match a_address, b_address with
          | Some a, Some b -> (SubAddress.bisub ~size ctx a b)::l
          | _ -> l
        in *)
        (* XXX: handle this case. Translate to shift/index calls? *)
        let is_address, addressE, nullE = 
          match (a_address, a_null), (b_address, b_null) with
          | (Some a, None), (None, Some b) -> Scalar.Boolean_Forward.true_ ctx, Some (SubAddress.bindex ~size (-1) ctx a b), None
          | (None, Some a), (Some b, None) -> Scalar.Boolean_Forward.true_ ctx, Some (SubAddress.bindex ~size (-1) ctx b a), None
          | (None, Some a), (None, Some b) -> Scalar.Boolean_Forward.false_ ctx, None, Some (Scalar.Binary_Forward.bisub ~size ~nsw ~nuw ~nusw ctx a b)
          | (Some a, None), (Some b, None) -> Scalar.Boolean_Forward.false_ ctx, None, Some (SubAddress.bisub ~size ctx a b)
          
          | (None, None), _ -> Scalar.boolean_empty ctx, None, None
          | _, (None, None) -> Scalar.boolean_empty ctx, None, None

          | _ -> assert false
        in
        let res = 
        (* if (a_null <> None && b_address <> None) || (a_address <> None && b_null <> None) then assert false
        else *)
          (*let scalar = join_values ~size ctx l in *)
          { is_address;
            id = fresh_id(); nullE = nullE(* Some scalar *); addressE; invalid }          
        in
        (* Codex_log.feedback "bisub: %a %a res %a" Binary.pretty a Binary.pretty b Binary.pretty res; *)
        (* Codex_log.feedback "Wholify.Single.bisub returning %a" (binary_pretty ~size ctx) res; *)
        res
      ;;

      let bshift ~size ~offset ~max ctx x =
        (* Codex_log.debug "Wholifiy.Single.bshift offset:%d %a" offset (binary_pretty ~size ctx) x ; *)
        let x_null, x_address = decomp ctx x in
        {
        is_address = x.is_address;
        nullE = (match x_null with
            | None -> None
            | Some x -> Some(
                let k = Scalar.Binary_Forward.biconst ~size (Z.of_int offset) ctx in
                Scalar.Binary_Forward.biadd ~size ~nsw:false ~nuw:false ~nusw:false ctx x k));
        addressE = option_map x_address (fun x -> SubAddress.bshift ~size ~offset ~max ctx x);
        invalid = x.invalid;
        id = fresh_id ();
      }

      let bchoose ~size _ = assert false
    end

    (* Here, we assume that the value is not invalid, as this
      operation is only created for initial values of variables. *)
    let binary_unknown_typed ~size ctx typ =
      let open Ctypes in
      let typ = inlined typ in
      match typ.descr with
      | Ptr _->
        let addr = SubAddress.binary_unknown_typed ~size ctx typ in
        let bin = SubAddress.binary2scalar_binary ~size ctx addr in
        let zero = Scalar.Binary_Forward.biconst ~size Z.zero ctx in
        let cond = Scalar.Boolean_Forward.not ctx @@ Scalar.Binary_Forward.beq ~size ctx bin zero in
        (* 0 is never a valid addr. *)
        Scalar.imperative_assume ctx cond ;
        { is_address = Scalar.boolean_unknown ctx ;
          nullE = Some zero ;
          id = fresh_id ();
          addressE = Some addr;
          invalid = false
        }

      | _ -> (* imperative_use_invariant ~size ctx typ.pred @@ *) binary_unknown ~size ctx

    let binary2scalar_binary ~size ctx value =
      let null,addr = decomp ctx value in
      match null,addr with
      | Some n, None -> n
      | _ -> assert false

    let assume_type ~size ctx value typ = assert false

    let type_of ~size ctx value =
      let _, addr = decomp ctx value in
      Option.bind addr (fun a -> SubAddress.type_of ~size ctx a)

    let has_type ~size ctx typ x = 
      let null, addr = decomp ctx x in
      match addr with
      | None -> true
      | Some addr -> SubAddress.has_type ~size ctx typ addr

    let analyze_summary _ctx funtyp args = assert false

    let contained_addresses ~size ctx value =
      let _,addr = decomp ctx value in
      match addr with
      | Some _ -> [(0, value)]
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
    (* end *)

end

(* Functor to help the instanciation. *)
module Make
    (Sub:Memory_sig.Memory_domain)
    (Address2Scalar: Memory_sig.Address_to_Scalar with module Scalar := Sub.Address.Scalar
                                                   and module Address := Sub.Address) = 
struct
  module Scalar = Sub.Address.Scalar
  module Address = MakeAddressOnly(Sub.Address)(Address2Scalar)
  (* module Address = M.Operable_Value *)
  type address = Address.binary
  let address2scalar x = fst @@ Address2Scalar.ctx x

  module Memory
      (Block:Memory_sig.Block)
      (Lift:Memory_sig.Value_to_address with module Address := Sub.Address and module Value := Block.Value)
    :Memory_sig.Memory
    with module Address = Address
     and module Block = Block
     and type boolean = Sub.Memory(Block)(Lift).boolean
  = struct
    module Address = Address
    module Block = Block
    module Region = Sub.Memory(Block)(Lift)
    module Domain:Domain_sig.Minimal
      with module Context = Region.Context
       and type boolean = Region.boolean
      = Region
    include Domain      


    let address_ctx ctx = fst (Lift.ctx ctx)
    module Value = Block.Value

    type address = Address.binary
    module Region_numeric_offset = Region_numeric_offset.Make(Scalar);;
    module ConstantRegion = Region_numeric_offset.Memory(Value)(Lift);;    

    type memory = {
      sub:Region.memory;               (* Region when not a constant address. *)
      constant: ConstantRegion.block; (* Region for constant addresses. *)
    }

    let pretty ctx fmt x =
      match Codex_config.valid_absolute_addresses() with
      | None ->
        Format.fprintf fmt "{ constant = <none>; sub = %a; }"
          (Region.pretty ctx) x.sub
      | Some _ ->
        Format.fprintf fmt "{ constant = %a; sub = %a; }"
          (ConstantRegion.pretty ctx) x.constant
          (Region.pretty ctx) x.sub
    ;;        


    let join_values ~size ctx list =
      match list with
      | [] -> Value.binary_empty ~size ctx
      | [x]-> x
      | a::b ->
        let nondet_binary v1 v2 = 
          let Context.Result(_,tup,deserialize2) = Value.serialize ~size ctx v1 ctx v2
              (true, Context.empty_tuple ()) in
          let res_tup = Value.nondet_same_context ctx tup in
          fst @@ deserialize2 ctx res_tup
        in List.fold_left nondet_binary a b
    ;;


    
    let memory_empty ctx = { sub = Region.memory_empty ctx ;
                             constant = ConstantRegion.block_empty ctx };;

    let load ~size (ctx:Value.Context.t) mem x =
      let addr_ctx = address_ctx ctx in
      Log.debug (fun p -> p "Wholify.load %a" (Address.binary_pretty ~size:32 addr_ctx) x);
      (*
      let ptr_size = Codex_config.ptr_size () in
      let x_null, x_address = Address.decomp ~size:ptr_size ctx x in *)
      let x_null, x_address = Address.decomp addr_ctx x in

      let l = [] in
      let l = match x_null with
        | None -> l
        | Some addr ->
          let res = ConstantRegion.load ~size ctx mem.constant addr in 
          Log.debug (fun p -> p "result of my constant load : %a" (Value.binary_pretty ~size ctx) res);
          [res] (* [ConstantRegion.load ~size ctx mem.constant addr] *)
      in
      let l = match x_address with
        | None -> l
        | Some addr ->
            Log.debug (fun p -> p "Loading from address %a" (Region.Address.binary_pretty ~size:32 addr_ctx) addr);
            let res = (Region.load ~size ctx mem.sub addr) in
            (* Codex_log.debug "result of region load : %a" (Value.binary_pretty ~size ctx) res ; *)
            res::l
      in
      (* if List.length l <= 0 then assert false ; *)
      let res =
      join_values ~size ctx l
      (* in Codex_log.debug "Wholify.load returning %a" (Value.binary_pretty ~size ctx) res ; res *)
      in res
    ;;

    let typed_load ~size ctx mem x typ = assert false ;;

    let store ~size ctx mem x value =
      let addr_ctx = address_ctx ctx in
      (* let ptr_size = Codex_config.ptr_size () in
      Codex_log.debug "Storing %a at address %a" (Value.binary_pretty ~size ctx) value (Address.binary_pretty ~size:ptr_size addr_ctx) x; *)
      (*
      let x = Address.discriminated ~size:ptr_size ctx x in
      let x_null, x_address = Address.decomp ~size:ptr_size ctx x in *)
      let x_null, x_address = Address.decomp addr_ctx x in

      match x_null, x_address with
      | None, None -> memory_empty ctx
      | _ -> 
        let constant = match x_null with
          | None -> mem.constant
          | Some addr ->
            ConstantRegion.store ~size ctx mem.constant addr value
        in
        let sub = match x_address with
          | None -> mem.sub
          | Some address -> Region.store ~size ctx mem.sub address value
        in
        { constant; sub}
    ;;

    let typed_store ~size ctx mem x typ value = assert false ;;

    let load_block ~size:_ _ = assert false
    let store_block ~size:_ _ = assert false

    let serialize ctxa mema ctxb memb acc =
      let Context.Result(included,acc,d_sub) = Region.serialize ctxa mema.sub ctxb memb.sub acc in
      let Context.Result(included,acc,d_const) = ConstantRegion.serialize ctxa mema.constant ctxb memb.constant (included,acc) in      
      Context.Result(included,acc,fun ctx tup ->
          let constant,tup = d_const ctx tup in          
          let sub,tup = d_sub ctx tup in
          {sub;constant},tup
        )
    ;;
    let serialize_binary = serialize

    let malloc ~id ~malloc_size ctx mem =
      let address_ctx = address_ctx ctx in
      let address,sub = Region.malloc ~id ~malloc_size ctx mem.sub in
      Address.({ is_address = Scalar.Boolean_Forward.true_ address_ctx;
                id = fresh_id(); addressE = Some address; nullE = None; invalid = false}),
      { sub; constant = mem.constant }

    let free ctx mem (x:Address.binary) =
      let addr_ctx = address_ctx ctx in
      (* let x_null, x_address = Address.decomp ~size:(Codex_config.ptr_size()) ctx x in *)
      let x_null, x_address = Address.decomp addr_ctx x in
      (match x_null with
      | None -> ()
      | Some x_null ->
        Codex_log.error "Free on null address: %a"
          (Address.Scalar.binary_pretty ~size:(Codex_config.ptr_size()) addr_ctx) x_null;
        Codex_log.alarm "free-on-null-address");
      match (x_address) with
      | None -> memory_empty ctx
      | Some address ->
        let sub = Region.free ctx mem.sub address in
        { sub; constant = mem.constant}

    let unknown ~level ctx =
      { sub = Region.unknown ~level ctx;
        constant =
          match Codex_config.valid_absolute_addresses() with
          | None -> ConstantRegion.initial ctx 0
          | Some (_,max) ->
            ConstantRegion.initial ctx @@ Z.to_int max        
      }

    let should_focus ~size ctx (mem:memory) (x:address) =
      let address_ctx = address_ctx ctx in
      let scalar_ctx = address2scalar address_ctx in
      Log.debug (fun p -> p "Wholify.should_focus ~size:%d %a" size (Address.binary_pretty ~size address_ctx) x);
      let ptr_size = Codex_config.ptr_size () in
      (*
      let x = Address.discriminated ~size:ptr_size ctx x in
      let x_null, x_address = Address.decomp ~size:ptr_size ctx x in *)
      let x_null, x_address = Address.decomp address_ctx x in

      let is_address =
        match x_null, x_address with
        | Some _, Some _ -> Scalar.boolean_unknown scalar_ctx
        | Some _, _ -> Scalar.Boolean_Forward.false_ scalar_ctx
        | _, Some _ -> Scalar.Boolean_Forward.true_ scalar_ctx
        | _ -> Scalar.boolean_empty scalar_ctx
      in
      
      match x_address with
        | None -> None
        | Some address -> begin
            match Region.should_focus ~size ctx mem.sub address with
            | None -> None
            | Some (base,size,off) ->
              let offset = Z.of_int (off / 8) in
              let null =
                Option.map (fun x -> Scalar.Binary_Forward.(bisub ~size:ptr_size ~nsw:false ~nuw:false ~nusw:false address_ctx x @@ biconst ~size:ptr_size offset address_ctx)) x_null in
              Some(Address.({is_address = is_address;
                            id=fresh_id(); nullE=null; addressE=Some base; invalid=x.invalid}),size,off)
          end
    ;;

    let may_alias ~ptr_size ctx ~size1 ~size2 x y =
      let address_ctx = address_ctx ctx in
      let scalar_ctx = address2scalar address_ctx in
      (*
      let x_null, x_address = Address.decomp ~size:ptr_size ctx x in
      let y_null, y_address = Address.decomp ~size:ptr_size ctx y in *)
      let x_null, x_address = Address.decomp address_ctx x in
      let y_null, y_address = Address.decomp address_ctx y in
      let may_scalar_alias = 
        match(x_null, y_null) with
        | None, _ | _, None -> false
        | Some(x_null), Some(y_null) ->
          let res = Address.Scalar.Binary_Forward.beq ~size:ptr_size scalar_ctx x_null y_null in
          let res = Address.Scalar.query_boolean scalar_ctx res in
          match res with
          | Lattices.Quadrivalent.(Bottom | False) -> false
          | _ -> true
      in
      let may_address_alias = 
        match(x_address, y_address) with
        | None, _ | _, None -> false
        | Some(x_address), Some y_address ->
          Region.may_alias ~ptr_size ctx ~size1 ~size2 x_address y_address
      in
      may_scalar_alias || may_address_alias
    ;;

    let is_weak ~size ctx value =
      let addr_ctx = address_ctx ctx in 
      let _,addr = Address.decomp addr_ctx value in
      match addr with
      | None -> false
      | Some a -> Region.is_weak ~size ctx a
    
    let shared_addresses ctxa mema ctxb memb =
      Region.shared_addresses ctxa mema.sub ctxb memb.sub

  end

end
