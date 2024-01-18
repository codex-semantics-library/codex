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

module MakeOld
    (SubAddress:Memory_sig.Address)
    (Address2Scalar: Memory_sig.Address_to_Scalar with module Scalar := SubAddress.Scalar
                                                   and module Address := SubAddress)
(* :Memory_sig.Operable_Value_Whole *) = struct

  let address2scalar x = fst @@ Address2Scalar.ctx x

    
  module Scalar = SubAddress.Scalar

  module Operable_Value(* :Memory_sig.Address *) = struct

    module Domain:Domain_sig.Minimal_No_Boolean with module Context = SubAddress.Context
      = SubAddress
    include Domain      
    
    module Scalar = SubAddress.Scalar

    module Single_Value = struct

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

      let boolean2scalar_bool _ = assert false
      let scalar_bool2boolean _ = assert false      
      let serialize_boolean _ = assert false

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

        let equal = ( = )

        (* No real notion of order on booleans... *)
        let compare (a:t) (b:t) : int = assert false

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

            | (Some a_addr, Some a_null), (Some b_addr, Some b_null) -> (* TODO : check if this is correct *)
              assert false
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
          if invalid then Codex_log.warning "invalid operation";
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
        (** Maybe: we could compare the bitvector representation of pointers here. *)
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


      let counter = ref 0 ;;

      let serialize ~size ctxa a ctxb b (included,acc) =
        (* Codex_log.feedback "WHolify.OV.serialize2 %a %a " *)
        (*   (binary_pretty ~size ctx) a  (binary_pretty ~size ctx) b;       *)
        incr counter ;
        let cur_counter = !counter in
        Codex_log.debug "Wholify.Single.serialize no.%d %a %a" cur_counter (binary_pretty ~size ctxa) a  (binary_pretty ~size ctxb) b ;
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

        let Context.Result(included,acc,d_is_address) = Scalar.serialize_boolean ctxa a.is_address ctxb b.is_address (included,acc) in
        (*
        let Scalar.Result(included,acc,d_null) = match a_null,b_null with
          | None, None -> Scalar.Result(included, acc, (fun ctx tup -> None,tup))
          | _ ->
            let Scalar.Result(inc,acc,d) =
              Scalar.serialize_binary ~size ctxa (f_null ctxa a_null) ctxb (f_null ctxb  b_null)
                (included,acc)
            in Scalar.Result(inc,acc,fun ctx tup -> let res,tup = d ctx tup in Some res,tup)
        in
        let Scalar.Result(included,acc,d_address) = match a_addr,b_addr with
          | None, None -> Scalar.Result(included, acc, (fun ctx tup -> None,tup))
          | _ ->
            let Scalar.Result(inc,acc,d) =
              SubAddress.serialize ~size ctxa (f_addr ctxa a_addr) ctxb (f_addr ctxb b_addr)
                (included,acc)
            in Scalar.Result(inc,acc,fun ctx tup -> let res,tup = d ctx tup in Some res,tup)
        in 

        Scalar.Result(included,acc,fun ctx tup ->
            let addressE,tup = d_address ctx tup in          
            let nullE,tup = d_null ctx tup in
            let is_address, tup = d_is_address ctx tup in
            let res = {is_address; addressE;nullE; id = fresh_id ();
                      invalid = a.invalid || b.invalid} in
            (* Codex_log.feedback "OV.deserialize2 @\na:%a@\nb:%a@\nres:%a" *)
            (* (binary_pretty ~size:32 ctx) a (binary_pretty ~size:32 ctx) b (binary_pretty ~size:32 ctx) res; *)
            Codex_log.debug "Wholify.Single.serialize no.%d returns %a" cur_counter (binary_pretty ~size ctx) res ;
            res,tup)
        *)
      
        let Context.Result(included, acc, des) =
          match (a_null, a_addr), (b_null, b_addr) with
          | (None, None), (None, None) -> Context.Result(included, acc, (fun ctx out -> (None,None), out))
          | (Some null, None), (None, Some addr)
                when Option.equal Z.equal (Some Z.zero) @@ (scal_is_singleton ~size ctxa null)
            ->
              let Context.Result(included, acc, d_null) = Scalar.serialize_binary ~size ctxa null ctxb (Scalar.binary_empty ~size ctxb) (included,acc) in
              let Context.Result(included, acc, d_address) = SubAddress.serialize ~size ctxa (SubAddress.binary_empty ~size ctxa) ctxb addr (included,acc) in
              Context.Result(included, acc, fun ctx out -> 
                let addr, out = d_address ctx out in
                let null, out = d_null ctx out in
                (Some null, Some addr), out
              )
          | (None, Some addr), (Some null, None)
                when Option.equal Z.equal (Some Z.zero) @@ (scal_is_singleton ~size ctxb null)
            ->
              let Context.Result(included, acc, d_null) = Scalar.serialize_binary ~size ctxa (Scalar.binary_empty ~size ctxa) ctxb null (included,acc) in
              let Context.Result(included, acc, d_address) = SubAddress.serialize ~size ctxa addr ctxb (SubAddress.binary_empty ~size ctxb) (included,acc) in
              Context.Result(included, acc, fun ctx out -> 
                let addr, out = d_address ctx out in
                let null, out = d_null ctx out in
                (Some null, Some addr), out
              )

          | (None, _), (None, _) ->
              let Context.Result(included, acc, d_address) = SubAddress.serialize ~size ctxa (f_addr ctxa a_addr) ctxb (f_addr ctxb b_addr) (included,acc) in
              Context.Result(included, acc, fun ctx out -> 
                let addr, out = d_address ctx out in
                (None, Some addr), out
              )

          | (_, None), (_, None) ->
              let Context.Result(included, acc, d_null) = Scalar.serialize_binary ~size ctxa (f_null ctxa a_null) ctxb (f_null ctxb b_null) (included,acc) in
              Context.Result(included, acc, fun ctx out -> 
                let null, out = d_null ctx out in
                (Some null, None), out
              )

          | _ ->
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
                      invalid = a.invalid || b.invalid} in
            (* Codex_log.feedback "OV.deserialize2 @\na:%a@\nb:%a@\nres:%a" *)
            (* (binary_pretty ~size:32 ctx) a (binary_pretty ~size:32 ctx) b (binary_pretty ~size:32 ctx) res; *)
            (* Codex_log.debug "Wholify.Single.serialize no.%d returns %a" cur_counter (binary_pretty ~size ctx) res ; *)
            res,tup)
      ;;
      let serialize_binary = serialize

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
            | _ -> None, None
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
              (true, Context.empty_tuple) in
          let res_tup = Scalar.nondet_same_context ctx tup in
          fst @@ deserialize2 ctx res_tup

        let join_addr_bool ctx l r =
          let Context.Result(_,tup,deserialize2) = SubAddress.serialize_boolean ctx l ctx r
              (true, Context.empty_tuple) in
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
                  (true, Context.empty_tuple) in
              let res_tup = Scalar.nondet_same_context ctx tup in
              fst @@ deserialize2 ctx res_tup
            in List.fold_left nondet_binary a b
        ;;

      module Binary_Forward = struct

        let buninit = binary_empty;;
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
          (* let scalar_bool = quadri2scalar_bool ctx @@ Query.boolean ctx x in *)
          let scalar_bool = Boolean_to_Binary.boolean_tree_to_binary ctx x in
          (* Codex_log.debug "in bofbool, scalar_bool : %a" (Scalar.boolean_pretty ctx) scalar_bool ; *)
          let res =
          numeric_operation ~size ctx @@ Scalar.Binary_Forward.bofbool ~size ctx scalar_bool
          in (* Codex_log.debug "bofboof returning %a" (binary_pretty ~size ctx) res ; *) res

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
          then Codex_log.warning "invalid operation";
          { is_address = Scalar.Boolean_Forward.false_ ctx;
            nullE;addressE=None;invalid; id = fresh_id () }

        ;;

        let bimul ~size ~nsw ~nuw ctx a b = ar2 (Scalar.Binary_Forward.bimul ~nsw ~nuw) ~size ctx a b
        let bxor ~size ctx a b = ar2 (Scalar.Binary_Forward.bxor) ~size ctx a b
        let band ~size ctx a b = Codex_log.debug "Wholify.band" ; ar2 (Scalar.Binary_Forward.band) ~size ctx a b
        let bor ~size ctx a b = ar2 (Scalar.Binary_Forward.bor) ~size ctx a b

        let bashr ~size ctx a b = ar2 (Scalar.Binary_Forward.bashr) ~size ctx a b
        let blshr ~size ctx a b = ar2 (Scalar.Binary_Forward.blshr) ~size ctx a b
        let bshl ~size ~nsw ~nuw ctx a b = ar2 (Scalar.Binary_Forward.bshl ~nsw ~nuw)   ~size ctx a b                    

        let bisdiv ~size ctx a b = ar2 (Scalar.Binary_Forward.bisdiv) ~size ctx a b
        let bismod ~size ctx a b = ar2 (Scalar.Binary_Forward.bismod) ~size ctx a b        
        let biudiv ~size ctx a b = ar2 (Scalar.Binary_Forward.biudiv) ~size ctx a b
        let biumod ~size ctx a b = ar2 (Scalar.Binary_Forward.biumod) ~size ctx a b        

        (* let bconcat ~size1 ~size2 ctx a b =
          if size1 = 0 then b
          else if size2 = 0 then a
          else
            begin
              let a_null, a_address = decomp ctx a in
              let b_null, b_address = decomp ctx b in
              let invalid = a.invalid || b.invalid
                  || (a_address <> None && a_null <> None) || (b_address <> None && b_null <> None) in
              let nullE = match (a_null, a_address), (b_null, b_address) with
                | (Some a, None), (Some b, None) -> Some(Scalar.Binary_Forward.bconcat ~size1 ~size2 ctx a b)
                | (None, Some a), (Some b, None) ->
                    Some(Scalar.Binary_Forward.bconcat ~size1 ~size2 ctx (SubAddress.binary2scalar_binary ~size:size1 ctx a) b)
                | (Some a, None), (None, Some b) ->
                    Some(Scalar.Binary_Forward.bconcat ~size1 ~size2 ctx a (SubAddress.binary2scalar_binary ~size:size2 ctx b))
                | (None, Some a), (None, Some b) ->
                    let a = SubAddress.binary2scalar_binary ~size:size1 ctx a in
                    let b = SubAddress.binary2scalar_binary ~size:size2 ctx b in
                    Some(Scalar.Binary_Forward.bconcat ~size1 ~size2 ctx a b)
                
                | (None, None), _ -> None
                | _, (None, None) -> None

                | _ -> assert false
              in
              { is_address = Scalar.Boolean_Forward.false_ ctx;
                  nullE;addressE=None;invalid; id = fresh_id ()}
            end
        ;; *)
        
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

        (* let valid_ptr_arith ~size:_ ctx _x _y = (* Scalar.Boolean_Forward.true_ ctx *) assert false *)

        let valid_ptr_arith ~size ctx x y =
          (* Codex_log.debug "Wholify.valid_ptr_arith %a %a" (binary_pretty ~size ctx) x (binary_pretty ~size ctx) y ; *)
          let x_null, x_address = decomp ctx x in
          let y_null, y_address = decomp ctx y in

          (* IMPORTANT : we mostly check null pointer here, we do not look prevent the addition of two pointers for instance *)
          
          let res2 = 
            match x_null, x_address with
            | (None, None) -> boolean_empty ctx
            | (Some _, Some _) -> valid ~size Transfer_functions.Read ctx x (* Boolean_Forward.not ctx @@ beq ~size ctx x (biconst ~size Z.zero ctx) *)
            | _ -> Boolean_Forward.true_ ctx
          in

          let res3 = 
            match y_null, y_address with
            | (None, None) -> boolean_empty ctx
            | (Some _, Some _) -> valid ~size Transfer_functions.Read ctx y (* Boolean_Forward.not ctx @@ beq ~size ctx y (biconst ~size Z.zero ctx) *)
            | _ -> Boolean_Forward.true_ ctx
          in

          Boolean_Forward.(&&) ctx res2 res3

        (* TODO: Maybe we should raise alarm, even if we count them only
          after the fixpoint is reached, or we could perform a pass to
          collect the alarms. That would be simpler. *)

        (* MAYBE: Si l'un etait un pointeur et l'autre pas, transformer en bshift ou bindex si on peut. *)
        let biadd ~size ~nsw ~nuw ~nusw ctx a b =
          (* Codex_log.debug "Wholify.Single.biadd %a %a" (binary_pretty ~size ctx) a (binary_pretty ~size ctx) b ; *)
          let a_null, a_address = decomp ctx a in
          let b_null, b_address = decomp ctx b in                
          let nullE = match a_null,b_null with
            | Some a, Some b -> Some(Scalar.Binary_Forward.biadd ~size ~nsw ~nuw ~nusw ctx a b)
            | _ -> None
          in
          (* XXX: handle this case. Translate to shift/index callls? *)
          let level = Scalar.Context.level ctx in

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


      (*--------------- Utility functions to call "assume" with the correct sizes ---------------*)

      (* type expr_size = Const of int | Min of int

      let eq_size sz1 sz2 =
        match sz1, sz2 with
        | Const i, Const j when i = j -> sz1
        | Min i, Const j when i < j -> sz2
        | Const j, Min i when i < j -> sz1
        | Min i, Min j -> Min (max i j)
        | Const i, Min j ->
            raise (Failure ("invalid expr size : Const " ^ (string_of_int i) ^ " and Min " ^ (string_of_int j)))
        | Min i, Const j ->
            raise (Failure ("invalid expr size : Min " ^ (string_of_int i) ^ " and Const " ^ (string_of_int j)))
        | Const i, Const j ->
            raise (Failure ("invalid expr size : Const " ^ (string_of_int i) ^ " and Const " ^ (string_of_int j)))

      let rec expr_size ~size ctx expr =
        let open Ctypes.Pred in
        match expr with
        | Self -> Const size
        | Val(Const c) -> Min (Z.numbits c)
        | Val(Sym s) -> assert false (* let sz = fst @@ global_symbol ctx s in Const sz *)
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
      *)

      (* ------------------------------ Assume operations -------------------------------*)

      (* let binary_of_value ~size ctx v =
        let open Ctypes in
        match v with
        | Const x -> Binary_Forward.biconst ~size x ctx
        | Sym s -> assert false

      let cond_of_cmp ~size ctx cmpop v1 v2 =
        let open Ctypes.Pred in
        match cmpop with
        | Equal ->
            Binary_Forward.beq ~size ctx v1 v2
        | NotEqual ->
            Boolean_Forward.not ctx @@ Binary_Forward.beq ~size ctx v1 v2
        | ULt ->
            Boolean_Forward.not ctx @@ Binary_Forward.biule ~size ctx v2 v1
        | SLt ->
            Boolean_Forward.not ctx @@ Binary_Forward.bisle ~size ctx v2 v1
        | ULeq ->
            Binary_Forward.biule ~size ctx v1 v2
        | SLeq ->
            Binary_Forward.bisle ~size ctx v1 v2
        | UGeq ->
            Binary_Forward.biule ~size ctx v2 v1
        | SGeq ->
            Binary_Forward.bisle ~size ctx v2 v1
        | UGt ->
            Boolean_Forward.not ctx @@ Binary_Forward.biule ~size ctx v1 v2
        | SGt ->
            Boolean_Forward.not ctx @@ Binary_Forward.bisle ~size ctx v1 v2

      let lift_unop ~size ctx op =
        let open Ctypes.Pred in
        match op with
        | Extract (index,len) -> fun x ->
          Binary_Forward.bextract ~size:len ~index ~oldsize:size ctx x
          |> Binary_Forward.buext ~size ~oldsize:len ctx

      let rec lift_binop ~size ctx op =
        let open Ctypes.Pred in
        match op with
        | Add -> Binary_Forward.biadd ~size ctx ~nsw:false ~nuw:false ~nusw:false
        | Sub -> Binary_Forward.bisub ~size ctx ~nsw:false ~nuw:false ~nusw:false
        | Mul -> Binary_Forward.bimul ~size ctx ~nsw:false ~nuw:false
        | And -> Binary_Forward.band ~size ctx
        | Or -> Binary_Forward.bor ~size ctx
        | Concat (size1,size2) -> Binary_Forward.bconcat ~size1 ~size2 ctx

      and binary_of_expr ~size ctx ~self e =
        let open Ctypes.Pred in
        match e with
        | Self -> self
        | Val v -> binary_of_value ~size ctx v
        | Unop (op, e) -> lift_unop ~size ctx op (binary_of_expr ~size ctx ~self e)
        | Binop (op, e1, e2) -> lift_binop ~size ctx op
            (binary_of_expr ~size ctx ~self e1) (binary_of_expr ~size ctx ~self e2)

      and cond_of_pred ~size ctx pred ~self =
        let open Ctypes.Pred in
        match pred with
        | True -> Boolean_Forward.true_ ctx
        | Cmp (op, e1, e2) ->
            let size = cmp_size ~size ctx e1 e2 in
            cond_of_cmp ~size ctx op (binary_of_expr ~size ctx ~self e1)
            (binary_of_expr ~size ctx ~self e2)
            (* let cond1 = binary_of_expr ~size ctx ~self e1 in
            let cond2 = binary_of_expr ~size ctx ~self e2 in
            cond_of_cmp ~size ctx op cond1 cond2 *)
        | And (p,q) ->
            let c1 = cond_of_pred ~size ctx p ~self in
            let c2 = cond_of_pred ~size ctx q ~self in
            Boolean_Forward.(&&) ctx c1 c2

      and imperative_use_invariant ~size ctx pred value =
          Codex_log.debug "use_invariant %a" Ctypes.Pred.pp pred;
          let open Ctypes.Pred in
          match pred with
          | True -> value
          | Cmp(Equal, Self, e)
          | Cmp(Equal, e, Self) -> binary_of_expr ~size ctx ~self:value e
          | _ -> imperative_assume ctx (cond_of_pred ~size ctx pred ~self:value) ; value
      *)
          
          
      (*--------------- Utility functions to call "assume" with the correct sizes ---------------*)

      (* type expr_size = Const of int | Min of int

      let eq_size sz1 sz2 =
        match sz1, sz2 with
        | Const i, Const j when i = j -> sz1
        | Min i, Const j when i < j -> sz2
        | Const j, Min i when i < j -> sz1
        | Min i, Min j -> Min (max i j)
        | Const i, Min j ->
            raise (Failure ("invalid expr size : Const " ^ (string_of_int i) ^ " and Min " ^ (string_of_int j)))
        | Min i, Const j ->
            raise (Failure ("invalid expr size : Min " ^ (string_of_int i) ^ " and Const " ^ (string_of_int j)))
        | Const i, Const j ->
            raise (Failure ("invalid expr size : Const " ^ (string_of_int i) ^ " and Const " ^ (string_of_int j)))

      let rec expr_size ~size ctx expr =
        let open Ctypes.Pred in
        match expr with
        | Self -> Const size
        | Val(Const c) -> Min (Z.numbits c)
        | Val(Sym s) -> assert false (* let sz = fst @@ global_symbol ctx s in Const sz *)
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
      *)

      (* ------------------------------ Assume operations -------------------------------*)

      (* let binary_of_value ~size ctx v =
        let open Ctypes in
        match v with
        | Const x -> Binary_Forward.biconst ~size x ctx
        | Sym s -> assert false

      let cond_of_cmp ~size ctx cmpop v1 v2 =
        let open Ctypes.Pred in
        match cmpop with
        | Equal ->
            Binary_Forward.beq ~size ctx v1 v2
        | NotEqual ->
            Boolean_Forward.not ctx @@ Binary_Forward.beq ~size ctx v1 v2
        | ULt ->
            Boolean_Forward.not ctx @@ Binary_Forward.biule ~size ctx v2 v1
        | SLt ->
            Boolean_Forward.not ctx @@ Binary_Forward.bisle ~size ctx v2 v1
        | ULeq ->
            Binary_Forward.biule ~size ctx v1 v2
        | SLeq ->
            Binary_Forward.bisle ~size ctx v1 v2
        | UGeq ->
            Binary_Forward.biule ~size ctx v2 v1
        | SGeq ->
            Binary_Forward.bisle ~size ctx v2 v1
        | UGt ->
            Boolean_Forward.not ctx @@ Binary_Forward.biule ~size ctx v1 v2
        | SGt ->
            Boolean_Forward.not ctx @@ Binary_Forward.bisle ~size ctx v1 v2

      let lift_unop ~size ctx op =
        let open Ctypes.Pred in
        match op with
        | Extract (index,len) -> fun x ->
          Binary_Forward.bextract ~size:len ~index ~oldsize:size ctx x
          |> Binary_Forward.buext ~size ~oldsize:len ctx

      let rec lift_binop ~size ctx op =
        let open Ctypes.Pred in
        match op with
        | Add -> Binary_Forward.biadd ~size ctx ~nsw:false ~nuw:false ~nusw:false
        | Sub -> Binary_Forward.bisub ~size ctx ~nsw:false ~nuw:false ~nusw:false
        | Mul -> Binary_Forward.bimul ~size ctx ~nsw:false ~nuw:false
        | And -> Binary_Forward.band ~size ctx
        | Or -> Binary_Forward.bor ~size ctx
        | Concat (size1,size2) -> Binary_Forward.bconcat ~size1 ~size2 ctx

      and binary_of_expr ~size ctx ~self e =
        let open Ctypes.Pred in
        match e with
        | Self -> self
        | Val v -> binary_of_value ~size ctx v
        | Unop (op, e) -> lift_unop ~size ctx op (binary_of_expr ~size ctx ~self e)
        | Binop (op, e1, e2) -> lift_binop ~size ctx op
            (binary_of_expr ~size ctx ~self e1) (binary_of_expr ~size ctx ~self e2)

      and cond_of_pred ~size ctx pred ~self =
        let open Ctypes.Pred in
        match pred with
        | True -> Boolean_Forward.true_ ctx
        | Cmp (op, e1, e2) ->
            let size = cmp_size ~size ctx e1 e2 in
            cond_of_cmp ~size ctx op (binary_of_expr ~size ctx ~self e1)
            (binary_of_expr ~size ctx ~self e2)
            (* let cond1 = binary_of_expr ~size ctx ~self e1 in
            let cond2 = binary_of_expr ~size ctx ~self e2 in
            cond_of_cmp ~size ctx op cond1 cond2 *)
        | And (p,q) ->
            let c1 = cond_of_pred ~size ctx p ~self in
            let c2 = cond_of_pred ~size ctx q ~self in
            Boolean_Forward.(&&) ctx c1 c2

      and imperative_use_invariant ~size ctx pred value =
          Codex_log.debug "use_invariant %a" Ctypes.Pred.pp pred;
          let open Ctypes.Pred in
          match pred with
          | True -> value
          | Cmp(Equal, Self, e)
          | Cmp(Equal, e, Self) -> binary_of_expr ~size ctx ~self:value e
          | _ -> imperative_assume ctx (cond_of_pred ~size ctx pred ~self:value) ; value
      *)
          
      (* What to put in invalid here is unclear. This operation can be
        used for completely unknon values. *)
      let binary_unknown ~size ctx =
        { is_address = Scalar.Boolean_Forward.false_ ctx;
          nullE = Some(Scalar.binary_unknown ~size ctx);
          id = fresh_id ();
          addressE = None;
          invalid = false;
        }

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

          (* let value = Scalar.binary_unknown ~size ~level ctx in
          let value = imperative_use_invariant ~size ctx typ.pred value in
          let is_empty = Scalar.binary_is_empty ~size ctx value in
          let nullE, is_address =
            if is_empty then None, Scalar.boolean_empty ctx
            else Some value, Scalar.Boolean_Forward.false_ ctx
          in

          { is_address ;
            nullE ;
            id = fresh_id ();
            addressE = None;
            invalid = false;
          } *)
        (* | _ -> binary_unknown ~size ~level ctx *)

      (* let binary_unknown_typed ~size ~level ctx typ = binary_unknown ~size ~level ctx ;; *)

      let binary2scalar_binary ~size ctx value = assert false

      (* let assume_type ~size ctx value typ = 
        let open Ctypes in
        match value with
         { nullE; addressE; _} ->
          Option.iter (imperative_use_invariant ~size ctx typ.pred) nullE ;
          Option.iter (fun addr -> SubAddress.assume_type ~size ctx addr typ) addressE
      *)

      let assume_type ~size ctx value typ = assert false

      let has_type ~size ctx typ x = 
        let null, addr = decomp ctx x in
        match addr with
        | None -> true
        | Some addr -> SubAddress.has_type ~size ctx typ addr

    end

    module Single = Single_Value ;;

    type binary =
      | Val of Single.binary
      | Concat of (int * binary) list (* size of the field * field value (sorted in ascending order) *)
      | Union of (binary * Ctypes.typ * binary) list


    let fresh_id = Single.fresh_id

    let binary_empty ~size ctx = Val (Single.binary_empty ~size ctx)

    let join_values ~size ctx list =
      match list with
      | [] -> Single.binary_empty ~size ctx
      | [x]-> x
      | a::b ->
        let nondet_binary v1 v2 = 
          let Context.Result(_,tup,deserialize2) = Single.serialize ~size ctx v1 ctx v2
              (true, Context.empty_tuple) in
          let res_tup = Scalar.nondet_same_context ctx tup in
          fst @@ deserialize2 ctx res_tup
        in List.fold_left nondet_binary a b
    ;;

    let binary_unknown ~size ctx = Val (Single.binary_unknown ~size ctx)

    (* let folder (acc_value, acc_sz) typ =
      let sz = 8 * Ctypes.sizeof typ in
      let value = binary_unknown_typed ~size:sz ~level ctx typ in
      let concat_value = Binary_Forward.bconcat ~size1:acc_sz ~size2:sz ctx acc_value value in
      concat_value, acc_sz + sz
    in
    let typ = List.hd list in
    let sz = 8 * Ctypes.sizeof typ in
    let first_value = binary_unknown_typed ~size:sz ~level ctx typ in
    let value, sz = List.fold_left folder (first_value, sz) (List.tl list) in *)

    let rec simplified ~size ctx binary = 
      match binary with
      | Val v -> v

      | Concat [] -> Single.binary_empty ~size ctx
      | Concat [(sz, v)] -> simplified ~size ctx v
      | Concat ((sz, v) :: tl) ->
        let folder (acc_value, acc_sz) (sz, value) =
          let value = simplified ~size:sz ctx value in
          let concat_value = Single.Binary_Forward.bconcat ~size1:acc_sz ~size2:sz ctx acc_value value in
          concat_value, acc_sz + sz
        in
        let first_value = simplified ~size:sz ctx v in
        let value, sz = List.fold_left folder (first_value, sz) tl in
        value

      | Union [] -> assert false
      | Union lst -> Single.binary_unknown ~size ctx
          (* join_values ~size ctx @@ List.map (fun (v,t) -> simplified ~size ctx v) lst *)
          (* Single.binary_unknown ~size ~level:(Scalar.Context.level ctx) ctx *)

    let decomp ~size ctx binary =
      match binary with
      | Val v -> Single.decomp ctx v
      | _ -> assert false (* Single.decomp ctx (simplified ~size ctx binary) *)

    
    (** ------------------------- Global symbols ------------------------- **)

    exception Global_symbol_not_found

    let add_global_scalar_symbol ~size ctx name binary = assert false 

    let rec global_constants =
      let table = ref ([] : (string * (int * binary)) list) in
      let initialized = ref false in
      fun ctx ->
        if !initialized then table
        else begin
          (* table := List.map (fun (name,size,pred) ->
            let b = Single.binary_unknown ~size ~level:(Scalar.Context.level ctx) ctx in
            (name, (size, Val (Single.imperative_use_invariant ~size ctx pred b)))
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
        Format.printf "cannot find global symbolic variable : %s\n" s ;
        raise Global_symbol_not_found
  
    and add_global_symbol ~size ctx name binary =
      let table = global_constants ctx in
        table := (name, (size, binary))::(!table) ;

      match binary with
      | Val {nullE = Some v; _} -> SubAddress.add_global_scalar_symbol ~size ctx name v
      | _ -> ()

    
    let get_type, add_type =
        let (typemap : (binary, Ctypes.typ) Hashtbl.t) = Hashtbl.create 123 in
        (fun bin -> Hashtbl.find_opt typemap bin),
        (fun bin typ -> Hashtbl.add typemap  bin typ) ;;

    let rec binary_pretty ~size ctx fmt x =
      let open Format in
      match x with
      | Val v -> Single.binary_pretty ~size ctx fmt v (* (simplified ~size ctx x) *)
      | Concat ls ->
        let _,ls = List.fold_right (fun (sz,part) (ofs,acc) -> (ofs+sz, (ofs, sz, part)::acc)) ls (0,[]) in
        let pp_sep fmt () = Format.fprintf fmt ";@;" in
        fprintf fmt "Concat [@[<hov 2>%a@]]"
            (pp_print_list ~pp_sep
              (fun fmt (ofs, sz, part) -> fprintf fmt "@[<hov 2>{ %d -> %a of size %d }@]" ofs (binary_pretty ~size:sz ctx) part sz)) ls
      
      | Union [] -> assert false
      | Union [(value, typ, _)] -> fprintf fmt "@[<hov 2>{ %a of type %a }@]" (binary_pretty ~size ctx) value Ctypes.pp typ 
      | Union ls ->
        fprintf fmt "Union {%a}"
            (pp_print_list
              (fun fmt (value, typ, whole) ->
                fprintf fmt "@[<hov 2>{ %a of type %a }@];" (binary_pretty ~size ctx) value (Ctypes.pp) typ)) ls

    
    let binpred_pretty fmt op =
      let open Format in
      match op with
      | Single.B_EQ -> fprintf fmt "EQ"
      | Single.B_SLE -> fprintf fmt "SLE"
      | Single.B_ULE -> fprintf fmt "ULE"
      | Single.B_NEQ -> fprintf fmt "NEQ"
      | Single.B_NSLE -> fprintf fmt "NSLE"
      | Single.B_NULE -> fprintf fmt "NULE"

    let boolean_simple_pretty ctx fmt x =
      let open Format in
      match x with
      | Single.BVal b -> fprintf fmt "BVal %a" Lattices.Quadrivalent.pretty b
      | Single.BBinpred {pred; size; left; right} ->
          fprintf fmt "BBinpred {pred:%a; size:%d; left%a; right:%a}"
              binpred_pretty pred
              size
              (Single.binary_pretty ~size ctx) left
              (Single.binary_pretty ~size ctx) right

      | Single.BValid {size; arg} -> fprintf fmt "BValid {size:%d; arg:%a}" size (Single.binary_pretty ~size ctx) arg

    let rec boolean_pretty ctx fmt x =
      let open Format in
      match x with
      | Single.BSimple bs -> boolean_simple_pretty ctx fmt bs
      | Single.BAnd (bl,br) ->
          fprintf fmt "(%a) and (%a)" (boolean_pretty ctx) bl (boolean_pretty ctx) br
      | Single.BOr (bl,br) ->
          fprintf fmt "(%a) or (%a)" (boolean_pretty ctx) bl (boolean_pretty ctx) br

    (* let rec serialize_struct
      : 'a. size:int -> Scalar.Context.t -> (int * binary) list ->
          Scalar.Context.t -> (int * binary) list -> ('a Scalar.in_acc) -> ((int * binary) list, 'a) Scalar.result
      = fun ~size ctxa la ctxb lb (included,in_tup) ->
      match la,lb with
      | [],[] -> Scalar.Result(included, in_tup, fun ctx out_tup -> [], out_tup)
      | [], _ -> assert false
      | _, [] -> assert false
      | (sza,va) :: tla, (szb,vb) :: tlb when sza = szb -> 
        begin
          let Scalar.Result (included, in_tup, deserialize_struct) = serialize_struct ~size:(size - sza) ctxa tla ctxb tlb (included, in_tup) in
          (* let Scalar.Result (included, in_tup, deserialize) = Single.serialize ~size:sza ctxa (simplified ~size:sza ctxa va) ctxb (simplified ~size:szb ctxb vb) (included, in_tup) in *)
          let Scalar.Result (included, in_tup, deserialize) = serialize ~size:sza ctxa va ctxb vb (included, in_tup) in
          Scalar.Result(included, in_tup, fun ctx out_tup -> (
            let v, out_tup = deserialize ctx out_tup in
            let tl, out_tup = deserialize_struct ctx out_tup in
            (* (sza, Val v) :: tl , out_tup *)
            (sza, v) :: tl , out_tup
          ))
        end

      | _, _ -> 
          let va = simplified ~size ctxa (Struct la) in
          let vb = simplified ~size ctxb (Struct lb) in
          let Scalar.Result (included, in_tup, deserialize) = Single.serialize ~size ctxa va ctxb vb (included, in_tup) in
          Scalar.Result(included, in_tup, fun ctx out_tup -> let res,out = deserialize ctx out_tup in [(size, Val res)], out)

    and serialize_union
      : 'c. size:int -> Scalar.Context.t -> (binary * Ctypes.typ) list ->
          Scalar.Context.t -> (binary * Ctypes.typ) list -> 'b Scalar.in_acc -> ((binary * Ctypes.typ) list option, 'b) Scalar.result
      = fun ~size ctxa la ctxb lb (included,in_tup) ->
      match la,lb with
      | [],[] -> Scalar.Result(included, in_tup, fun ctx out_tup -> Some [], out_tup)

      | (va,ta) :: tla, (vb,tb) :: tlb when Ctypes.equal ~only_descr:false ta tb ->
        begin
          let sz = 8 * Ctypes.sizeof ta in
          let Scalar.Result (included, in_tup, deserialize_union) = serialize_union ~size:(size - sz) ctxa tla ctxb tlb (included, in_tup) in
          let Scalar.Result (included, in_tup, deserialize) = serialize ~size:sz ctxa va ctxb vb (included, in_tup) in
          Scalar.Result(included, in_tup, fun ctx out_tup -> (
            let v, out_tup = deserialize ctx out_tup in
            let res, out_tup = deserialize_union ctx out_tup in
            Option.map (fun ls -> (v,ta)::ls) res, out_tup
          ))
        end

      | _ -> Scalar.Result(included, in_tup, fun ctx out_tup -> None, out_tup)

    and serialize
      : 'b. size:int -> Scalar.Context.t -> binary -> Scalar.Context.t -> binary -> 'b Scalar.in_acc -> (binary, 'b) Scalar.result
      = fun ~size ctxa a ctxb b acc ->
      Codex_log.debug "Wholify.serialize %a %a" (binary_pretty ~size ctxa) a (binary_pretty ~size ctxb) b ;
      match a,b with
      | Val va, Val vb -> 
        let Scalar.Result(included,in_tup,deserialize) = Single.serialize ~size ctxa va ctxb vb acc in
        Scalar.Result(included,in_tup, fun ctx out -> let v,out = deserialize ctx out in Val v, out)

      | Struct la, Struct lb -> 
        begin
          let Scalar.Result(included, in_tup, deserialize_struct) = serialize_struct ~size ctxa la ctxb lb acc in
          Scalar.Result(included, in_tup, fun ctx out -> 
            match deserialize_struct ctx out with
            | [(_,v)], out -> v, out
            | lst, out -> Struct lst, out
          )
        end

      | Union la, Union lb ->
          let a = discriminated ~size ctxa a in
          let va = simplified ~size ctxa a in
          let vb = simplified ~size ctxb b in
          let Scalar.Result(included, in_tup, deserialize_union) = serialize_union ~size ctxa la ctxb lb acc in
          Codex_log.debug "va : %a" (Single.binary_pretty ~size ctxa) va ;
          Codex_log.debug "vb : %a" (Single.binary_pretty ~size ctxb) vb ; 
          (* let Scalar.Result(included, in_tup, deserialize) =
            Single.serialize ~size ctxa (simplified ~size ctxa a) ctxb (simplified ~size ctxb b) (included, in_tup) in *)
          let Scalar.Result(included, in_tup, deserialize) = Single.serialize ~size ctxa va ctxb vb (included, in_tup) in  
          Scalar.Result(included, in_tup, fun ctx out_tup ->
            let v,out_tup = deserialize ctx out_tup in
            let res,out_tup = deserialize_union ctx out_tup in
            (match res with None -> Val v | Some ls -> Union ls), out_tup

          )

      | _ -> 
        let va = simplified ~size ctxa a in
        let vb = simplified ~size ctxb b in 
        let Scalar.Result(included, in_tup, deserialize) = Single.serialize ~size ctxa va ctxb vb acc in
        Scalar.Result(included, in_tup, fun ctx out -> let v,out = deserialize ctx out in Val v, out)
    *)

    module Binary = struct
      type t = binary

      let rec pretty fmt x =
        let open Format in
        match x with
        | Val v -> Single.Binary.pretty fmt v
        | Concat s ->
          fprintf fmt "@[<hov 2>struct {@[<v 2>[@ %a@ ]@];@ }@]"
            (pp_print_list
              (fun fmt (sz,v) -> fprintf fmt "(@[<hov 2>%d,@ %a@]);" sz pretty v)) s

        | union -> assert false
          
      let rec hash x =
        match x with
        | Val value -> Hashtbl.hash (1, Single.Binary.hash value)
        | Concat values -> Hashtbl.hash @@ (2, List.map (fun (sz, value) -> hash value) values)
        | Union values -> Hashtbl.hash @@ (3, List.map (fun (value, typ, whole) -> hash value) values)

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
        | Val vx, Val vy -> Single.Binary.compare vx vy
        | Val _, _ -> -1
        | _, Val _ -> 1

        | Concat cx, Concat cy ->
            compare_list (fun (_,v1) (_,v2) -> compare v1 v2) cx cy
        | Concat _, _ -> -1
        | _, Concat _ -> 1

        | Union ux, Union uy -> 
            compare_list (fun (v1,_,_) (v2,_,_) -> compare v1 v2) ux uy

      let equal x y = compare x y = 0
    end

    type boolean = Single.boolean
    module Boolean = Single.Boolean

    module Boolean_Forward = Single.Boolean_Forward       
      

    let rec apply_all ~size op1 ctx a b =
      match a,b with
      | Val _, Union ls ->
          let ls = List.map (fun (value, typ, whole) -> apply_all ~size op1 ctx a value, typ, whole) ls in
          Union ls
      | Union ls, Val _ ->
          let ls = List.map (fun (value, typ, whole) -> apply_all ~size op1 ctx value b, typ, whole) ls in
          Union ls

      | _ -> Val (op1 ~size ctx (simplified ~size ctx a) (simplified ~size ctx b))

    let rec apply_all1 ~size op1 ctx a =
      match a with
      | Union ls ->
          let ls = List.map (fun (value, typ, whole) -> apply_all1 ~size op1 ctx value, typ, whole) ls in
          Union ls

      | _ -> Val (op1 ~size ctx (simplified ~size ctx a))

    let rec apply_all_comp ~size op1 ctx a b =
      Codex_log.debug "apply_all_comp %a %a" (binary_pretty ~size ctx) a (binary_pretty ~size ctx) b ;
      match a,b with
      | Val _, Union ls ->
          let true_b = Boolean_Forward.true_ ctx in
          List.fold_left (fun res (value, typ, whole) -> Boolean_Forward.(&&) ctx res @@ (apply_all_comp ~size op1 ctx a value)) true_b ls

      | Union ls, Val _->
          let true_b = Boolean_Forward.true_ ctx in
          List.fold_left (fun res (value, typ, whole) -> Boolean_Forward.(&&) ctx res @@ (apply_all_comp ~size op1 ctx value b)) true_b ls

      | _ -> op1 ~size ctx (simplified ~size ctx a) (simplified ~size ctx b)

    let rec apply_all_comp1 ~size op1 ctx a =
      match a with
      | Union ls ->
          let true_b = Boolean_Forward.true_ ctx in
          List.fold_left (fun res (value, typ, whole) -> Boolean_Forward.(&&) ctx res @@ (apply_all_comp1 ~size op1 ctx value)) true_b ls

      | _ -> op1 ~size ctx (simplified ~size ctx a)


    module Query = struct
      module Boolean_Lattice = Single.Query.Boolean_Lattice
      let boolean ctx bool = Single.Query.boolean ctx bool
      let convert_to_quadrivalent (x : Lattices.Quadrivalent.boolean) = Single.Query.convert_to_quadrivalent x

      module Binary_Lattice = Single.Query.Binary_Lattice
      let binary ~size ctx x = Single.Query.binary ~size ctx @@ simplified ~size ctx x
      let binary_to_ival ~signed ~size x = Single.Query.binary_to_ival ~signed ~size x
      let binary_to_known_bits  ~size x = assert false
      let binary_fold_crop ~size (x:Binary_Lattice.t) ~inf ~sup acc f = Single.Query.binary_fold_crop ~size x ~inf ~sup acc f

      let binary_is_empty ~size b = Single.Query.binary_is_empty ~size b
      let binary_is_singleton ~size (x:Binary_Lattice.t) = Single.Query.binary_is_singleton ~size x
    end

    (*--------------- Utility functions to call "assume" with the correct sizes ---------------*)

    type expr_size = Const of int | Min of int

    let eq_size sz1 sz2 =
      match sz1, sz2 with
      | Const i, Const j when i = j -> sz1
      | Min i, Const j when i <= j -> sz2
      | Const j, Min i when i <= j -> sz1
      | Min i, Min j -> Min (max i j)
      | Const i, Min j ->
          raise (Failure ("invalid expr size : Const " ^ (string_of_int i) ^ " and Min " ^ (string_of_int j)))
      | Min i, Const j ->
          raise (Failure ("invalid expr size : Min " ^ (string_of_int i) ^ " and Const " ^ (string_of_int j)))
      | Const i, Const j ->
          raise (Failure ("invalid expr size : Const " ^ (string_of_int i) ^ " and Const " ^ (string_of_int j)))

    let rec expr_size ~size ctx expr =
      let open Ctypes.Pred in
      match expr with
      | Self -> Const size
      | Val(Const c) -> Min (Z.numbits c)
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


    module Binary_Forward = struct
      (* let beq ~size ctx x y = Single.Binary_Forward.beq ~size ctx (simplified ctx x) (simplified ctx y) *)
      let beq ~size ctx x y = 
        Codex_log.debug "Wholify.beq" ;
        apply_all_comp ~size Single.Binary_Forward.beq ctx x y

      (* let biule ~size ctx x y = Single.Binary_Forward.biule ~size ctx (simplified ctx x) (simplified ctx y) *)
      let biule ~size ctx x y = 
        Codex_log.debug "Wholify.biule" ;
        apply_all_comp ~size Single.Binary_Forward.biule ctx x y
      
      (* let bisle ~size ctx x y = Single.Binary_Forward.bisle ~size ctx (simplified ctx x) (simplified ctx y) *)
      let bisle ~size ctx x y = 
        Codex_log.debug "Wholify.bisle" ;
        let res = apply_all_comp ~size Single.Binary_Forward.bisle ctx x y in
        (* Codex_log.debug "Wholify.bisle returning %a" (boolean_pretty ctx) res ; *)
        res

      (* let bitimes ~size ctx x y = Single.Binary_Forward. ~size ctx (simplified ~size ctx x) (simplified ~size ctx y) *)
      
      let biadd ~size ~nsw ~nuw ~nusw ctx x y = 
        Codex_log.debug "Wholify.biadd %a %a" (binary_pretty ~size ctx) x (binary_pretty ~size ctx) y ;
        apply_all ~size (fun ~size ctx a b -> Single.Binary_Forward.biadd ~size ~nsw ~nuw ~nusw ctx a b) ctx x y

      (* let biadd ~size ~nsw ~nuw ctx x y = Val (Single.Binary_Forward.biadd ~size ~nsw ~nuw ~nusw ctx (simplified ctx x) (simplified ctx y)) *)

      (* let bisub ~size ~nsw ~nuw ctx x y = Val (Single.Binary_Forward.bisub ~size ~nsw ~nuw ~nusw ctx (simplified ctx x) (simplified ctx y)) *)
      (* let bisub ~size ~nsw ~nuw ~nusw ctx x y = 
        apply_all ~size (fun ~size ctx a b -> Single.Binary_Forward.bisub ~size ~nsw ~nuw ~nusw ctx a b) ctx x y *)

      let bimul ~size ~nsw ~nuw ctx x y = Val (Single.Binary_Forward.bimul ~size ~nsw ~nuw ctx (simplified ~size ctx x) (simplified ~size ctx y))
      let bxor ~size ctx x y =
        Val (Single.Binary_Forward.bxor ~size ctx (simplified ~size ctx x) (simplified ~size ctx y))
      (* let band ~size ctx x y = Val (Single.Binary_Forward.band ~size ctx (simplified ctx x) (simplified ctx y)) *)

      let band ~size ctx x y = apply_all ~size Single.Binary_Forward.band ctx x y


      let bor ~size ctx x y = Val (Single.Binary_Forward.bor ~size ctx (simplified ~size ctx x) (simplified ~size ctx y))
      let bsext ~size ~oldsize ctx x = Val (Single.Binary_Forward.bsext ~size ~oldsize ctx (simplified ~size ctx x))
      let buext ~size ~oldsize ctx x = Val (Single.Binary_Forward.buext ~size ~oldsize ctx (simplified ~size ctx x))
      let bofbool ~size ctx b =
        Codex_log.debug "Wholify.bofbool %a" (boolean_pretty ctx) b ;
        Val (Single.Binary_Forward.bofbool ~size ctx b)
      let bchoose = Single.Binary_Forward.bchoose
      
      let bashr ~size ctx x y =
        apply_all ~size Single.Binary_Forward.bashr ctx x y

      let blshr ~size ctx x y =
        apply_all ~size Single.Binary_Forward.blshr ctx x y

      let bshl ~size ~nsw ~nuw ctx x y = 
        apply_all ~size (fun ~size ctx a b ->Single.Binary_Forward.bshl ~size ~nsw ~nuw ctx a b) ctx x y

      let bisdiv ~size ctx x y = Val (Single.Binary_Forward.bisdiv ~size ctx (simplified ~size ctx x) (simplified ~size ctx y))
      let biudiv ~size ctx x y = Val (Single.Binary_Forward.biudiv ~size ctx (simplified ~size ctx x) (simplified ~size ctx y))
      (* let bconcat ~size1 ~size2 ctx b1 b2 = 
        match b1, b2 with
        | Val v1, _ when size2 = 0 -> b1
        | _, Val v2 when size1 = 0 -> b2
        | Val v1, Val v2 when size1 mod 8 = 0 && size2 mod 8 = 0 -> Struct [(size1, v1); (size2, v2)] 
        | Val v1, Struct s2 when size1 mod 8 = 0 && size2 mod 8 = 0 -> Struct ((size1, v1) :: s2)
        | Struct s1, Val v2 when size2 mod 8 = 0 && size2 mod 8 = 0 -> Struct (s1 @ [(size2, v2)])
        | _ -> Val (Single.Binary_Forward.bconcat ~size1 ~size2 ctx (simplified ~size ctx b1) (simplified ~size ctx b2)) *)

      let bconcat ~size1 ~size2 ctx x y =
        let x_is_bottom = Query.binary_is_empty ~size:size1 @@ Query.binary ~size:size1 ctx x in
        let y_is_bottom = Query.binary_is_empty ~size:size2 @@ Query.binary ~size:size2 ctx y in
        if x_is_bottom || y_is_bottom then binary_empty ~size:(size1 + size2) ctx
        else 
          match x,y with (* TODO : improve this *)
          | Concat l1, Concat l2 -> Concat (l1 @ l2)
          | Concat l1, _ when size2 mod 8 = 0 -> Concat (l1 @ [(size2,y)])
          | _, Concat l2 when size1 mod 8 = 0 -> Concat ((size1,x) :: l2)
          | _ when size1 mod 8 = 0 && size2 mod 8 = 0 -> Concat [(size1, x);(size2, y)]
          | _ -> Val (Single.Binary_Forward.bconcat ~size1 ~size2 ctx (simplified ~size:size1 ctx x) (simplified ~size:size2 ctx y))
      
      let bconcat ~size1 ~size2 ctx x y =
        Codex_log.debug "bconcat size1:%d size2:%d %a %a" size1 size2 (binary_pretty ~size:size1 ctx) x (binary_pretty ~size:size2 ctx) y ;
        let res = bconcat ~size1 ~size2 ctx x y in
        Codex_log.debug "bconcat returning %a" (binary_pretty ~size:(size1 + size2) ctx) res ;
        res

      let bismod ~size ctx x y = Val (Single.Binary_Forward.bismod ~size ctx (simplified ~size ctx x) (simplified ~size ctx y))
      let biumod ~size ctx x y = Val (Single.Binary_Forward.biumod ~size ctx (simplified ~size ctx x) (simplified ~size ctx y))
      
      (*
      let bextract_from_record ~size ~index ~oldsize st =
        let end_ = index + size in
        let rec left_extract idx lst =
          match lst with
          | [] -> Some []
          | (sz,v) :: tl ->
              if idx = index then Some []
              else if idx < index then None
              else Option.map (fun res -> (sz,v) :: res) @@ left_extract (idx - sz) tl
        in
        let rec right_extract idx lst =
          match lst with
          | [] -> None
          | (sz, v)::tl ->
              if idx = end_ then left_extract idx lst
              else if idx < sz then None
              else right_extract (idx - sz) tl

        in right_extract oldsize st
      *)
      let rec extract_from_concat ~size ~index ~oldsize ctx concat =
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

      and bextract ~size ~index ~oldsize ctx x = 
        Codex_log.debug "Wholify.bextract ~size:%d ~index:%d ~oldsize:%d %a" size index oldsize (binary_pretty ~size:oldsize ctx) x ;
        if size = oldsize then x
        else if Query.binary_is_empty ~size:oldsize @@ Query.binary ~size:oldsize ctx x then binary_empty ~size ctx 
        else
        match x with
          | Val {nullE=None; addressE=None; _} -> binary_empty ~size ctx
          | Concat concat -> extract_from_concat ~size ~index ~oldsize ctx concat
          | Union ls -> Union (List.map (fun (value, typ, whole) -> bextract ~size ~index ~oldsize ctx value, typ, whole) ls)
          | Val v -> Val (Single.Binary_Forward.bextract ~size ~index ~oldsize ctx v)

      (* let bextract ~size ~index ~oldsize ctx x =
        if size mod 8 = 0 then 
          let word = Ctypes.word ~byte_size:size in
          let ls = List.map (fun a -> (Single.Binary_Forward.bextract ~size ~index ~oldsize ctx a, word)) ls in
        
        else bextract ~size ~index ~oldsize ctx x *)
      
      (* let valid_ptr_arith ~size ctx x y = Single.Binary_Forward.valid_ptr_arith ~size ctx (simplified ctx x) (simplified ctx y) *)
      let biconst ~size k ctx = Val (Single.Binary_Forward.biconst ~size k ctx)

      let buninit ~size ctx = Val (Single.Binary_Forward.buninit ~size ctx)

      let bshift ~size ~offset ~max ctx x =
        apply_all1 ~size (fun ~size ctx a -> Single.Binary_Forward.bshift ~size ~offset ~max ctx a) ctx x
        
      (* let bshift ~size ~offset ~max ctx x = Val (Single.Binary_Forward.bshift ~size ~offset ~max ctx (simplified ctx x)) *)

      let bindex ~size k ctx x e =
        apply_all ~size (fun ~size ctx a b -> Single.Binary_Forward.bindex ~size k ctx a b) ctx x e

      let bisub ~size ~nsw ~nuw ~nusw ctx x y =
        apply_all ~size (fun ~size ctx a b -> Single.Binary_Forward.bisub ~size ~nsw ~nuw ~nusw ctx a b) ctx x y

      let rec binary_of_value ~size ctx v =
        let open Ctypes in
        match v with
        | Const x -> biconst ~size x ctx
        | Sym s -> snd @@ global_symbol ctx s

      and cond_of_cmp ~size ctx cmpop v1 v2 =
        let open Ctypes.Pred in
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
        let open Ctypes.Pred in
        match op with
        | Extract (index,len) -> fun x ->
          bextract ~size:len ~index ~oldsize:size ctx x
          |> buext ~size ~oldsize:len ctx

      and lift_binop ~size ctx op =
        let open Ctypes.Pred in
        match op with
        | Add -> biadd ~size ctx ~nsw:false ~nuw:false ~nusw:false
        | Sub -> bisub ~size ctx ~nsw:false ~nuw:false ~nusw:false
        | Mul -> bimul ~size ctx ~nsw:false ~nuw:false
        | And -> band ~size ctx
        | Or -> bor ~size ctx
        | Concat (size1,size2) -> bconcat ~size1 ~size2 ctx

      and binary_of_expr ~size ctx ~self e =
        let open Ctypes.Pred in
        match e with
        | Self -> self
        | Val v -> binary_of_value ~size ctx v
        | Unop (op, e) -> lift_unop ~size ctx op (binary_of_expr ~size ctx ~self e)
        | Binop (op, e1, e2) -> lift_binop ~size ctx op
            (binary_of_expr ~size ctx ~self e1) (binary_of_expr ~size ctx ~self e2)

      and cond_of_pred ~size ctx pred ~self =
        let open Ctypes.Pred in
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

      and check_invariant_complex ~size ctx typ value =
        let open Ctypes in
        let typ = inlined typ in
        match typ.descr with
        | Structure {st_byte_size; st_members} -> 
            let folder res (ofs,_,typ) =
              let sz = 8 * Ctypes.sizeof typ in
              let index = 8 * ofs in
              Boolean_Forward.(&&) ctx res @@
                check_invariant_complex ~size:sz ctx typ @@
                  bextract ~size:sz ~index ~oldsize:size ctx value
            in
            List.fold_left folder (Boolean_Forward.true_ ctx) st_members
  
        | _ -> cond_of_pred ~size ctx typ.pred ~self:value

      (** ---------------------------------------------------------------------------------------------  **)

      and discriminated ~size ctx binary =
        match binary with
        | Union lst -> (
            let filter (value, typ, whole) =
              let whole_size = 8 * Ctypes.sizeof typ in
              let c = check_invariant_complex ~size:whole_size ctx typ whole in
              match Query.convert_to_quadrivalent @@ Query.boolean ctx c with
              | Lattices.Quadrivalent.True | Lattices.Quadrivalent.Top -> true
              | _ -> false
            in
            match List.filter filter lst with
            | [] -> binary_empty ~size ctx
            | [(value, typ, whole)] -> value
            | values -> Union values
          )
        | _ -> binary

      (* let valid ~size _acc_type ctx ptr = Single.Binary_Forward.valid ~size _acc_type ctx (simplified ctx ptr) *)
      let valid ~size _acc_type ctx ptr =
        Codex_log.debug "Wholify.valid %a" (binary_pretty ~size:32 ctx) ptr ;
        let x = discriminated ~size ctx ptr in
        let _ = decomp ~size ctx ptr in
        Single.Binary_Forward.valid ~size _acc_type ctx (simplified ~size ctx x)
        (*
        let res =
        apply_all_comp1 ~size (fun ~size ctx a -> Single.Binary_Forward.valid ~size _acc_type ctx a) ctx ptr
        in Codex_log.feedback "Wholify.valid, returning %a" (boolean_pretty ctx) res ; res
        *)

      let valid_ptr_arith ~size ctx x y =
        Codex_log.debug "Wholify.valid_ptr_arith" ;
        apply_all_comp ~size Single.Binary_Forward.valid_ptr_arith ctx x y
      
    end

    let boolean_empty = Single.boolean_empty
    
    let boolean_unknown = Single.boolean_unknown


    let boolean2scalar_bool = Single.boolean2scalar_bool
    let scalar_bool2boolean = Single.scalar_bool2boolean 
    let serialize_boolean = Single.serialize_boolean

    let assume = Single.assume

    let imperative_assume = Single.imperative_assume

    let satisfiable = Single.satisfiable


    (*--------------- Utility functions to call "assume" with the correct sizes ---------------*)

    (*
    type expr_size = Const of int | Min of int

    let eq_size sz1 sz2 =
      match sz1, sz2 with
      | Const i, Const j when i = j -> sz1
      | Min i, Const j when i <= j -> sz2
      | Const j, Min i when i <= j -> sz1
      | Min i, Min j -> Min (max i j)
      | Const i, Min j ->
          raise (Failure ("invalid expr size : Const " ^ (string_of_int i) ^ " and Min " ^ (string_of_int j)))
      | Min i, Const j ->
          raise (Failure ("invalid expr size : Min " ^ (string_of_int i) ^ " and Const " ^ (string_of_int j)))
      | Const i, Const j ->
          raise (Failure ("invalid expr size : Const " ^ (string_of_int i) ^ " and Const " ^ (string_of_int j)))

    let rec expr_size ~size ctx expr =
      let open Ctypes.Pred in
      match expr with
      | Self -> Const size
      | Val(Const c) -> Min (Z.numbits c)
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
    *)

    let rec contains_self expr =
      let open Ctypes.Pred in
      match expr with
      | Self -> true
      | Val _ -> false
      | Binop(_, e1, e2) -> contains_self e1 || contains_self e2
      | Unop(_, e) -> contains_self e

    let rec local_invariant pred =
      let open Ctypes.Pred in
      match pred with
      | True -> pred
      | Cmp(op, e1, e2) when contains_self e1 || contains_self e2 -> pred
      | Cmp _ -> True
      | And(p1, p2) -> And(local_invariant p1, local_invariant p2)

    let rec local_type typ =
      let open Ctypes in
      let pred = local_invariant typ.pred in
      let typ = inlined typ in
      match typ.descr with
      | Structure ({st_members; _} as s) ->
        let st_members = List.map (fun (ofs,name,typ) -> (ofs, name, local_type typ)) st_members in
        {descr = Structure {s with st_members}; pred}
        
      | _ -> {typ with pred}

    (* ------------------------------ Assume operations -------------------------------*)

    (*
    let binary_of_value ~size ctx v =
      let open Ctypes in
      match v with
      | Const x -> Binary_Forward.biconst ~size x ctx
      | Sym s -> snd @@ global_symbol ctx s

    let cond_of_cmp ~size ctx cmpop v1 v2 =
      let open Ctypes.Pred in
      match cmpop with
      | Equal ->
          Binary_Forward.beq ~size ctx v1 v2
      | NotEqual ->
          Boolean_Forward.not ctx @@ Binary_Forward.beq ~size ctx v1 v2
      | ULt ->
          Boolean_Forward.not ctx @@ Binary_Forward.biule ~size ctx v2 v1
      | SLt ->
          Boolean_Forward.not ctx @@ Binary_Forward.bisle ~size ctx v2 v1
      | ULeq ->
          Binary_Forward.biule ~size ctx v1 v2
      | SLeq ->
          Binary_Forward.bisle ~size ctx v1 v2
      | UGeq ->
          Binary_Forward.biule ~size ctx v2 v1
      | SGeq ->
          Binary_Forward.bisle ~size ctx v2 v1
      | UGt ->
          Boolean_Forward.not ctx @@ Binary_Forward.biule ~size ctx v1 v2
      | SGt ->
          Boolean_Forward.not ctx @@ Binary_Forward.bisle ~size ctx v1 v2

    let lift_unop ~size ctx op =
      let open Ctypes.Pred in
      match op with
      | Extract (index,len) -> fun x ->
        Binary_Forward.bextract ~size:len ~index ~oldsize:size ctx x
        |> Binary_Forward.buext ~size ~oldsize:len ctx

    let rec lift_binop ~size ctx op =
      let open Ctypes.Pred in
      match op with
      | Add -> Binary_Forward.biadd ~size ctx ~nsw:false ~nuw:false ~nusw:false
      | Sub -> Binary_Forward.bisub ~size ctx ~nsw:false ~nuw:false ~nusw:false
      | Mul -> Binary_Forward.bimul ~size ctx ~nsw:false ~nuw:false
      | And -> Binary_Forward.band ~size ctx
      | Or -> Binary_Forward.bor ~size ctx
      | Concat (size1,size2) -> Binary_Forward.bconcat ~size1 ~size2 ctx

    and binary_of_expr ~size ctx ~self e =
      let open Ctypes.Pred in
      match e with
      | Self -> self
      | Val v -> binary_of_value ~size ctx v
      | Unop (op, e) -> lift_unop ~size ctx op (binary_of_expr ~size ctx ~self e)
      | Binop (op, e1, e2) -> lift_binop ~size ctx op
          (binary_of_expr ~size ctx ~self e1) (binary_of_expr ~size ctx ~self e2)

    and cond_of_pred ~size ctx pred ~self =
      let open Ctypes.Pred in
      match pred with
      | True -> Boolean_Forward.true_ ctx
      | Cmp (op, e1, e2) ->
          let size = cmp_size ~size ctx e1 e2 in
          cond_of_cmp ~size ctx op (binary_of_expr ~size ctx ~self e1)
          (binary_of_expr ~size ctx ~self e2)
          (* let cond1 = binary_of_expr ~size ctx ~self e1 in
          let cond2 = binary_of_expr ~size ctx ~self e2 in
          cond_of_cmp ~size ctx op cond1 cond2 *)
      | And (p,q) ->
          let c1 = cond_of_pred ~size ctx p ~self in
          let c2 = cond_of_pred ~size ctx q ~self in
          Boolean_Forward.(&&) ctx c1 c2

    and check_invariant ~size ctx pred v =
        Codex_log.debug "check_invariant %a" Ctypes.Pred.pp pred;
        let c = cond_of_pred ~size ctx pred ~self:v in
        let is_true =
          Query.convert_to_quadrivalent (Query.boolean ctx c) in
        match is_true with
        | Lattices.Quadrivalent.True -> true
        | _ -> false (* Possibly false *)
    *)

    (*
    let rec check_invariant_complex ~size ctx typ value =
      let open Ctypes in
      let typ = inlined typ in
      match typ.descr with
      | Structure {st_byte_size; st_members} -> 
          let folder res (ofs,_,typ) =
            let sz = 8 * Ctypes.sizeof typ in
            let index = 8 * ofs in
            Boolean_Forward.(&&) ctx res @@
              check_invariant_complex ~size:sz ctx typ @@
                Binary_Forward.bextract ~size:sz ~index ~oldsize:size ctx value
          in
          List.fold_left folder (Boolean_Forward.true_ ctx) st_members

      | _ -> cond_of_pred ~size ctx typ.pred ~self:value
    *)

    let binary_of_expr = Binary_Forward.binary_of_expr
    let cond_of_pred = Binary_Forward.cond_of_pred

    let imperative_use_invariant ~size ctx pred value =
      Codex_log.debug "Wholify.imperative_use_invariant %a" Ctypes.Pred.pp pred;
      let open Ctypes.Pred in
        match pred with
        | True -> value
        | Cmp(Equal, Self, e)
        | Cmp(Equal, e, Self) -> binary_of_expr ~size ctx ~self:value e
        | _ -> imperative_assume ctx (cond_of_pred ~size ctx pred ~self:value) ; value

    (*
    let rec check_invariant_complex ~size ctx typ value =
        let open Ctypes in
        let typ = inlined typ in
        match typ.descr with
        | Structure {st_byte_size; st_members} -> 
            let folder res (ofs,_,typ) =
              let sz = 8 * Ctypes.sizeof typ in
              let index = 8 * ofs in
              Boolean_Forward.(&&) ctx res @@
                check_invariant_complex ~size:sz ctx typ @@
                  Binary_Forward.bextract ~size:sz ~index ~oldsize:size ctx value
            in
            List.fold_left folder (Boolean_Forward.true_ ctx) st_members
  
        | _ -> cond_of_pred ~size ctx typ.pred ~self:value


    let discriminated ~size ctx binary =
      match binary with
      | Union lst -> 
        begin
          let filter (value,typ) =
            let c = check_invariant_complex ~size ctx typ value in
            match Query.convert_to_quadrivalent @@ Query.boolean ctx c with
            | Lattices.Quadrivalent.True | Lattices.Quadrivalent.Top -> true
            | _ -> false
          in
          match List.filter filter lst with
          | [] -> binary_empty ~size ctx
          | [(v,t)] -> v
          | ls -> Union ls
        end
      | _ -> binary
    *)

    let check_invariant ~size ctx pred v =
        Codex_log.debug "check_invariant %a" Ctypes.Pred.pp pred;
        let c = cond_of_pred ~size ctx pred ~self:v in
        let is_true =
          Query.convert_to_quadrivalent (Query.boolean ctx c) in
        match is_true with
        | Lattices.Quadrivalent.True -> true
        | Lattices.Quadrivalent.Bottom -> false (* true *)
        | _ -> false (* Possibly false *)

    let discriminated = Binary_Forward.discriminated

    let rec serialize_struct
      : 'a. size:int -> Scalar.Context.t -> (int * binary) list ->
          Scalar.Context.t -> (int * binary) list -> ('a Context.in_acc) -> ((int * binary) list, 'a) Context.result
      = fun ~size ctxa la ctxb lb (included,in_tup) ->
      Codex_log.debug "Wholify.serialize_struct" ;
      match la,lb with
      | [],[] -> Context.Result(included, in_tup, fun ctx out_tup -> [], out_tup)
      | [], _ -> assert false
      | _, [] -> assert false
      | (sza,va) :: tla, (szb,vb) :: tlb when sza = szb -> 
        begin
          let Context.Result (included, in_tup, d_struct) = serialize_struct ~size:(size - sza) ctxa tla ctxb tlb (included, in_tup) in
          (* let Context.Result (included, in_tup, deserialize) = Single.serialize ~size:sza ctxa (simplified ~size:sza ctxa va) ctxb (simplified ~size:szb ctxb vb) (included, in_tup) in *)
          let Context.Result (included, in_tup, deserialize) = serialize ~size:sza ctxa va ctxb vb (included, in_tup) in
          Context.Result(included, in_tup, fun ctx out_tup -> (
            let v, out_tup = deserialize ctx out_tup in
            let tl, out_tup = d_struct ctx out_tup in
            (* (sza, Val v) :: tl , out_tup *)
            (sza, v) :: tl , out_tup
          ))
        end

      | _, _ -> 
          let va = simplified ~size ctxa (Concat la) in
          let vb = simplified ~size ctxb (Concat lb) in
          let Context.Result (included, in_tup, d_single) = Single.serialize ~size ctxa va ctxb vb (included, in_tup) in
          Context.Result(included, in_tup, fun ctx out_tup -> let res,out = d_single ctx out_tup in [(size, Val res)], out)

    (* and serialize_union
      : 'c. size:int -> Context.Context.t -> (binary * Ctypes.typ) list ->
          Context.Context.t -> (binary * Ctypes.typ) list -> 'b Context.in_acc -> ((binary * Ctypes.typ) list option, 'b) Context.result
      = fun ~size ctxa la ctxb lb (included,in_tup) ->
      Codex_log.debug "Wholify.serialize_union" ;
      match la,lb with
      | [],[] -> Context.Result(included, in_tup, fun ctx out_tup -> Some [], out_tup)

      | (va,ta) :: tla, (vb,tb) :: tlb when Ctypes.equal ~only_descr:false ta tb ->
        begin
          let sz = 8 * Ctypes.sizeof ta in
          let Context.Result (included, in_tup, deserialize_union) = serialize_union ~size:(size - sz) ctxa tla ctxb tlb (included, in_tup) in
          let Context.Result (included, in_tup, deserialize) = serialize ~size:sz ctxa va ctxb vb (included, in_tup) in
          Context.Result(included, in_tup, fun ctx out_tup -> (
            let v, out_tup = deserialize ctx out_tup in
            let res, out_tup = deserialize_union ctx out_tup in
            Option.map (fun ls -> (v,ta)::ls) res, out_tup
          ))
        end

      | _ -> Context.Result(included, in_tup, fun ctx out_tup -> None, out_tup)
    *)

    and serialize
      : 'b. size:int -> Context.t -> binary -> Context.t -> binary -> 'b Context.in_acc -> (binary, 'b) Context.result
      = fun ~size ctxa a ctxb b in_tup ->
      Codex_log.debug "Wholify.serialize %a %a" (binary_pretty ~size ctxa) a (binary_pretty ~size ctxb) b ;
      let a = discriminated ~size ctxa a in
      let b = discriminated ~size ctxb b in
      match a,b with
      | Val va, Val vb -> 
        let Context.Result(included, acc, d_single) = Single.serialize ~size ctxa va ctxb vb in_tup in
        Context.Result(included, acc, fun ctx out -> let v,out = d_single ctx out in Val v, out)

      | Concat la, Concat lb -> 
        begin
          let Context.Result(included, acc, d_struct) = serialize_struct ~size ctxa la ctxb lb in_tup in
          Context.Result(included, acc, fun ctx out -> 
            match d_struct ctx out with
            | [(_,v)], out -> v, out
            | lst, out -> Concat lst, out
          )
        end

      (* | Union la, Union lb ->
          let a = discriminated ~size ctxa a in
          let b = discriminated ~size ctxb b in
          let va = simplified ~size ctxa a in   (* improve this part *)
          let vb = simplified ~size ctxb b in
          let Context.Result(included, in_tup, deserialize_union) = serialize_union ~size ctxa la ctxb lb acc in
          Codex_log.debug "va : %a" (Single.binary_pretty ~size ctxa) va ;
          Codex_log.debug "vb : %a" (Single.binary_pretty ~size ctxb) vb ; 
          (* let Context.Result(included, in_tup, deserialize) =
            Single.serialize ~size ctxa (simplified ~size ctxa a) ctxb (simplified ~size ctxb b) (included, in_tup) in *)
          let Context.Result(included, in_tup, deserialize) = Single.serialize ~size ctxa va ctxb vb (included, in_tup) in  
          Context.Result(included, in_tup, fun ctx out_tup ->
            let v,out_tup = deserialize ctx out_tup in
            let res,out_tup = deserialize_union ctx out_tup in
            (match res with None -> Val v | Some ls -> Union ls), out_tup

          )
      *)
      | _ -> 
        let va = simplified ~size ctxa a in
        let vb = simplified ~size ctxb b in 
        let Context.Result(included, in_tup, d_single) = Single.serialize ~size ctxa va ctxb vb in_tup in
        Context.Result(included, in_tup, fun ctx out -> let v,out = d_single ctx out in Val v, out)


    let fresh_int =
      let fresh_counter = ref (0 : int) in
      fun () ->
        incr fresh_counter ;
        !fresh_counter

    let fresh_symbol () = Format.sprintf "#%d" (fresh_int ())


    let rec binary_unknown_typed_from_record ~size ctx list =
      let folder (acc_value, acc_sz) typ =
        let sz = 8 * Ctypes.sizeof typ in
        let value = binary_unknown_typed ~size:sz ctx typ in
        let concat_value = Binary_Forward.bconcat ~size1:sz ~size2:acc_sz ctx value acc_value in
        concat_value, acc_sz + sz
      in
      let typ = List.hd list in
      let sz = 8 * Ctypes.sizeof typ in
      let first_value = binary_unknown_typed ~size:sz ctx typ in
      let value, sz = List.fold_left folder (first_value, sz) (List.tl list) in
      assert (size = sz);
      value

    (** Create a new binary unknown with given type.
        @raise [Failure] if [~size] doesn't match the type's size
        /!\ [~size] should be the bit size, not the byte size *)
    and binary_unknown_typed ~size ctx typ =
      Codex_log.debug "Wholifiy.binary_unknown_typed ~size:%d %a" size Ctypes.pp typ ;
      assert (size mod 8 = 0);
      let open Ctypes in
      let new_unknown () = Val (Single.binary_unknown_typed ~size ctx typ) in
      let bin = match typ.descr with
        | Name n -> binary_unknown_typed ~size ctx (inlined typ)
        | Void -> new_unknown ()
        | Base (byte_size, _) ->
            assert (size  = 8*byte_size);
            new_unknown ()
        | Enum enum ->
            assert (size = 8*enum.en_byte_size);
            new_unknown ()
        | Structure struc ->
            binary_unknown_typed_from_record ~size ctx
            (List.map (fun (_offset, _name, typ) -> typ) struc.st_members)
        | Ptr{pointed; index} ->
            assert (size = Codex_config.ptr_size ());
            (* TODO change this...
               this should create a new pointer value which is either null
               or points to something of type pointed
               (whose value can be computed recursively) *)
            (* old_load_from_typed_region ~size ctx
            Type.(PtrT{ptyp=typ; idx = index_zero ctx; fe = index_minus_one ctx;
                        ppred=Ctypes.Pred.True; ofs=0}) *)
            new_unknown ()
        | Array (tp, Some (Const t)) ->
            let array_size = Z.to_int t in
            assert (array_size > 0);
            binary_unknown_typed_from_record ~size ctx
            (List.init array_size (function _ -> tp))
        | Array (tp, _) ->
            failwith "Not implemented binary unknown of non-constant array"
        | Function (res, args) ->
            failwith "Not implemented binary unknown of function"

        | Union {un_byte_size; un_types} -> 
            (* let lst = List.map (fun (sz,typ) -> (binary_unknown_typed ~size ~level ctx @@ local_type typ), typ) un_types in *)
            let lst = List.map (fun (sz,typ) ->
              let value = binary_unknown_typed ~size ctx @@ local_type typ in (value, typ, value)) un_types in
            
            begin
              match lst with
              | [(v1, _,_); (v2,_,_)] ->
                let null1, addr1 = decomp ~size ctx v1 in
                let null2, addr2 = decomp ~size ctx v2 in
                begin match (null1,addr1), (null2,addr2) with
                | (None, Some a), (Some n, None)
                | (Some n, None), (None, Some a) -> (
                    let const = Scalar.Query.binary_is_singleton ~size @@ Scalar.Query.binary ~size ctx n in
                    match const with
                    | Some num when Z.equal Z.zero num ->
                      let res = Val {is_address=Scalar.boolean_unknown ctx; nullE=Some n; addressE=Some a; invalid=false; id=fresh_id ()} in
                      res
                    | _ -> Union lst
                  )
                | _ -> Union lst 
                end
              | _ -> Union lst
            end

        | Existential (t, se, te) ->
            let te_sz = 8 * Ctypes.sizeof te in
            let res = binary_unknown_typed ~size:te_sz ctx te in
            let new_symb = fresh_symbol () in
            add_global_symbol ~size:te_sz ctx new_symb res ;
            let new_t = Ctypes.substitute_symbol t se new_symb in
            let res = binary_unknown_typed ~size ctx new_t in
            add_type res typ ;
            res

        | Weak t ->
            assert (8 * Ctypes.sizeof t = size) ;
            Binary_Forward.buninit ~size ctx

        | _ -> assert false
      in
      (* Add constraints if any *)
      let bin = imperative_use_invariant ~size ctx typ.pred bin in
      Codex_log.debug "Wholify.binary_unknown_typed returning %a" (binary_pretty ~size ctx) bin ;
      (* assume_type ~size ctx bin typ ; *) bin

    
      
    (* --------------------------------- Unification for existential types --------------------------------- *)


    module StringMap = struct
      include Map.Make(String)

      (* type value = Type of Ctypes.typ | Value of Binary.t *)

      let replace ~size ctx key value map =
        let prev = find key map in
        match prev, value with
        | Some p, Some v ->
          begin
            let comp = Binary_Forward.beq ~size ctx p v in
            let is_true = Query.convert_to_quadrivalent (Query.boolean ctx comp) in
            match is_true with
            | Lattices.Quadrivalent.True -> map
            | _ -> raise Not_found
          end
        | _ -> add key value map 
    end

    let rec contains_local_var expr env =
      let open Ctypes.Pred in
      match expr with
      | Val (Sym s) when StringMap.mem s env -> true
      | Binop(_, e1, e2) -> contains_local_var e1 env || contains_local_var e2 env
      | Unop(_, e) -> contains_local_var e env
      | _ -> false

    (* TODO : Improve this and add more cases *)
    let rec unify_pred ~size ctx pred ~self env =
      let is_exist_var s = StringMap.mem s env in
      let open Ctypes.Pred in
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
            (Binary_Forward.bisub ~size ~nsw:false ~nuw:false ~nusw:false ctx self (binary_of_expr ~size ctx ~self e))
          ) env

      | Cmp (Equal, Self, Binop (Sub, Val(Sym s), e))
      | Cmp (Equal, Binop (Sub, Val(Sym s), e), Self) -> 
        if contains_local_var e env && is_exist_var s then env
        else 
          StringMap.replace ~size ctx s (Some
            (Binary_Forward.biadd ~size ~nsw:false ~nuw:false ~nusw:false ctx self (binary_of_expr ~size ctx ~self e))
          ) env
      
      | And (p1, p2) ->
          let env = unify_pred ~size ctx p1 ~self env in
          unify_pred ~size ctx p2 ~self env
      | Cmp(_, e1, e2) when contains_local_var e1 env || contains_local_var e2 env -> env
      | _ -> env

    
    let rec unify_record ~size ctx lst value env =
      let open Ctypes in
      let members, env = List.fold_right (fun (ofs,name,t) (lst, env) ->
        let size_bytes = sizeof t in
        let size_bits = 8 * size_bytes in
        let part = Binary_Forward.bextract ~size:size_bits ~oldsize:size ~index:(8 * ofs) ctx value in
        let typ, env = unify ~size:size_bits ctx t part env in
        (ofs,name, typ) :: lst, env
      ) lst ([],env)
      in members, env

    (* TODO : Improve this *)
    and unify ~size ctx typ (value : binary) env = 
      let open Ctypes in
      Codex_log.debug "Wholify.unify" ;
      let typ = inlined typ in
      let env = unify_pred ~size ctx typ.pred ~self:value env in
      match typ.descr with
        | Void -> typ, env
        | Base (sz, _)
        | Enum {en_byte_size = sz; _} -> typ, env
        | Structure ({st_byte_size = Some sz; st_members} as st) ->
            assert (8 * sz = size);
            let st_members, env = unify_record ~size ctx st_members value env in
            {typ with descr = Structure {st with st_members}}, env
        
        | Structure {st_byte_size = None; _} -> assert false (* TODO *)
        
        | Ptr ptr -> typ, env
        
        | Array (tp, (Some (Const t) as sz)) ->
            let elem_size = 8 * sizeof tp in
            let t,env = unify ~size:elem_size ctx tp value env in
            {typ with descr = Array (t, sz)}, env  

        | Array _ -> failwith "Unification attempt with arbitrary array"
        
        | Function (res, args) ->
            failwith "Unification attempt with a function"

        | Name name -> failwith ("Unification attempt with non-inlined named type " ^ name)
        
        | Application _ -> failwith ("Unification attemp with non-applied constructor type")

        | Existential (t, se, te) ->
          begin
            let env' = StringMap.add se None env in
            let t, new_env = unify ~size ctx t value env' in
            match StringMap.find se new_env with
            | None -> raise Not_found ; (* TODO : could be omitted if the existential variable is not used anywhere *)
            | Some v -> 
              let exist_size = sizeof te in
              let new_var = fresh_symbol () in
              add_global_symbol ~size:exist_size ctx new_var v ;
              substitute_symbol t se new_var, new_env
          end
      
        | Union ({un_types;} as un) -> 
              let new_lst, env' =
                List.fold_left (fun (ls,map) (sz,typ) -> let t,m = unify ~size ctx typ value map in (sz,t):: ls,m) ([],env) un_types
              in {typ with descr = Union {un with un_types = new_lst}}, env'
          
        | Weak _ -> assert false

    (* ------------------------------------------------------------------------------------------------------*)

    exception Typing_store_alarm


    (* let binary_unknown_typed ~size ~level ctx typ = binary_unknown ~size ~level ctx ;; *) (* For now. *)
    let rec check_type_and_predicate_record ~size ctx lst value =
      (* Codex_log.feedback "check_record_type %a of oldsize %d"
        Ctypes.pp Ctypes.{descr = Structure {st_byte_size = Some (size / 8); st_members = lst}; pred = Pred.True}
        size ;
      *)
      let folder acc (ofs,_,typ) =
        let part_size = 8 * Ctypes.sizeof typ in
        let part = Binary_Forward.bextract ~size:part_size ~oldsize:size ~index:(ofs * 8) ctx value in
        acc && check_type_and_predicate ~size:part_size ctx typ part
      in
      List.fold_left folder true lst

    and check_type_and_predicate ~size ctx typ value =
      (* TODO : correct typing store !!! *)
      (* Codex_log.feedback "Wholify.check_type_and_predicate size:%d %a %a" size Ctypes.pp typ (binary_pretty ~size ctx) value ; *)
      Codex_log.check "typing_store";
      let open Ctypes in
      let typ = inlined typ in
      let res =
        match typ.descr with
        | Void -> assert false
        | Base base -> true
        | Structure s -> check_type_and_predicate_record ~size ctx s.st_members value
        | Ptr ptr -> (* if Single.has_type ~size ctx typ (simplified ~size ctx value) then true else raise Typing_store_alarm *)
              Single.has_type ~size ctx typ (simplified ~size ctx value)
        | Enum _ -> assert false
        | Array (elem_typ, Some Const sz) ->
          let elem_sz = Ctypes.sizeof elem_typ in
          let members = List.init (Z.to_int sz) (fun i -> (i * elem_sz, "anon", elem_typ)) in
          check_type_and_predicate_record ~size ctx members value
        | Array _ -> assert false
        | Function _ -> assert false
        | Name _ -> assert false
        | Application _ -> assert false
        | Existential (t, se, te) -> (
            if Option.equal (Ctypes.equal ~only_descr:false) (Some typ) (get_type value) then true
            else
              begin
                try 
                  let t,_ = unify ~size ctx typ value StringMap.empty in
                  (* Codex_log.feedback "t (after unification) : %a" Ctypes.pp t ; *)
                  if check_type_and_predicate ~size ctx t value then true
                  else raise Typing_store_alarm
                with _ ->
                  Codex_log.alarm "typing_store_existential" ;
                  Codex_log.error "Storing value@ %a to region of type@ %a: no unification is made"
                  (binary_pretty ctx ~size) value
                  Ctypes.pp typ ;
                  (* raise Typing_store_alarm *)
                  false
              end
        )
        | Union {un_types;} -> 
          List.exists (fun (sz,typ) -> try check_type_and_predicate ~size ctx typ value with _ -> false) un_types
          (* List.exists (fun (sz,typ) -> check_type_and_predicate ~size ctx typ value) un_types *)
        | _ -> assert false
        in res && check_invariant ~size ctx typ.pred value
      
      

    let has_type ~size ctx typ value =
      Codex_log.check "predicate";
      (* try *)
      (* let typ,_ = unify ~size ctx typ value StringMap.empty in *)
      let res = check_type_and_predicate ~size ctx typ value in
      if not res then begin
        Codex_log.alarm "predicate";
        Codex_log.error "Predicate possibly does not hold."
      end;
      res
      (* with Typing_store_alarm -> () ; false (* prevents calling the alarm two times *) *)
      

    let binary2scalar_binary ~size ctx value = assert false

    let assume_type ~size ctx value typ = assert false


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


end


(* Functor to help the instanciation. *)
module Make
    (Sub:Memory_sig.Memory_domain)
    (Address2Scalar: Memory_sig.Address_to_Scalar with module Scalar := Sub.Address.Scalar
                                                   and module Address := Sub.Address) = 
struct
  module Scalar = Sub.Address.Scalar
  module M = MakeOld(Sub.Address)(Address2Scalar)
  module Address = M.Operable_Value

  type address = Address.binary

  module Memory
      (Value:Memory_sig.Value
       with module Context = Scalar.Context
      )
      (Lift:Memory_sig.Value_to_address with module Address := Sub.Address and module Value := Value)      
    :Memory_sig.Memory
    with module Value = Value
     and module Address = Address
     and type boolean = Sub.Memory(Value)(Lift).boolean
  = struct
    module Address = Address
    module Region = Sub.Memory(Value)(Lift)
    module Domain:Domain_sig.Minimal
      with module Context = Region.Context
       and type boolean = Region.boolean
      = Region
    include Domain      

    
    module Value = Region.Value
    type address = Address.binary
    module Region_numeric_offset = Region_numeric_offset.Make(Scalar);;
    module ConstantRegion = Region_numeric_offset.Memory(Value)(struct let ctx x = x,fun x -> x end);;    

    type memory = {
      sub:Region.memory;               (* Region when not a constant address. *)
      constant: ConstantRegion.memory; (* Region for constant addresses. *)
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
              (true, Context.empty_tuple) in
          let res_tup = Scalar.nondet_same_context ctx tup in
          fst @@ deserialize2 ctx res_tup
        in List.fold_left nondet_binary a b
    ;;


    
    let memory_empty ctx = { sub = Region.memory_empty ctx ;
                             constant = ConstantRegion.memory_empty ctx };;

    let load ~size ctx mem x =
      Codex_log.debug "Wholify.load %a" (Address.binary_pretty ~size:32 ctx) x ;
      let ptr_size = Codex_config.ptr_size () in
      let x = Address.discriminated ~size:ptr_size ctx x in
      let x_null, x_address = Address.decomp ~size:ptr_size ctx x in

      let l = [] in
      let l = match x_null with
        | None -> l
        | Some addr ->
          let res = (ConstantRegion.load ~size ctx mem.constant addr) in 
          Codex_log.debug "result of my constant load : %a" (Value.binary_pretty ~size ctx) res ;
          [res] (* [ConstantRegion.load ~size ctx mem.constant addr] *)
      in
      let l = match x_address with
        | None -> l
        | Some addr ->
            let res = (Region.load ~size ctx mem.sub addr) in
            Codex_log.debug "result of region load : %a" (Value.binary_pretty ~size ctx) res ;
            res::l
      in
      (* if List.length l <= 0 then assert false ; *)
      let res =
      join_values ~size ctx l
      in Codex_log.debug "Wholify.load returning %a" (Value.binary_pretty ~size ctx) res ; res
    ;;

    let typed_load ~size ctx mem x typ = assert false ;;

    let store ~size ctx mem x value =
      let ptr_size = Codex_config.ptr_size () in
      Codex_log.debug "Storing %a at address %a" (Value.binary_pretty ~size ctx) value (Address.binary_pretty ~size:ptr_size ctx) x;
      let x = Address.discriminated ~size:ptr_size ctx x in
      let x_null, x_address = Address.decomp ~size:ptr_size ctx x in

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
      let address,sub = Region.malloc ~id ~malloc_size ctx mem.sub in
      Address.(Val { is_address = Scalar.Boolean_Forward.true_ ctx;
                id = fresh_id(); addressE = Some address; nullE = None; invalid = false}),
      { sub; constant = mem.constant }

    let free ctx mem (x:Address.binary) =
      let x_null, x_address = Address.decomp ~size:(Codex_config.ptr_size()) ctx x in
      (match x_null with
      | None -> ()
      | Some x_null ->
        Codex_log.error "Free on null address: %a"
          (Address.Scalar.binary_pretty ~size:(Codex_config.ptr_size()) ctx) x_null;
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

    let should_focus ~size (ctx:Scalar.Context.t) (mem:memory) (x:address) =
      Codex_log.debug "Wholify.should_focus ~size:%d %a" size (Address.binary_pretty ~size ctx) x ;
      let ptr_size = Codex_config.ptr_size () in
      let x = Address.discriminated ~size:ptr_size ctx x in
      let x_null, x_address = Address.decomp ~size:ptr_size ctx x in

      let is_address =
        match x_null, x_address with
        | Some _, Some _ -> Scalar.boolean_unknown ctx
        | Some _, _ -> Scalar.Boolean_Forward.false_ ctx
        | _, Some _ -> Scalar.Boolean_Forward.true_ ctx
        | _ -> Scalar.boolean_empty ctx
      in
      
      match x_address with
        | None -> None
        | Some address -> begin
            match Region.should_focus ~size ctx mem.sub address with
            | None -> None
            | Some (base,size,off) ->
              let offset = Z.of_int (off / 8) in
              let null =
                Option.map (fun x -> Scalar.Binary_Forward.(bisub ~size:ptr_size ~nsw:false ~nuw:false ~nusw:false ctx x @@ biconst ~size:ptr_size offset ctx)) x_null in
              Some(Address.(Val {is_address = is_address;
                            id=fresh_id(); nullE=null; addressE=Some base; invalid=false}),size,off)
          end
    ;;

    let may_alias ~ptr_size ctx ~size1 ~size2 x y =
      let x_null, x_address = Address.decomp ~size:ptr_size ctx x in
      let y_null, y_address = Address.decomp ~size:ptr_size ctx y in
      let may_scalar_alias = 
        match(x_null, y_null) with
        | None, _ | _, None -> false
        | Some(x_null), Some(y_null) ->
          let res = Address.Scalar.Binary_Forward.beq ~size:ptr_size ctx x_null y_null in
          let res = Address.Scalar.query_boolean ctx res in
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

  end

end
