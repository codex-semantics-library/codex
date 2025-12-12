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

module Log = Tracelog.Make (struct
  let category = "Domains.Wholify"
end)

module TypedC = Types.TypedC
module Type_check_tree = Types.Type_check_tree
module In_bits = Units.In_bits
module In_bytes = Units.In_bytes

let option_merge merge left right =
  match (left, right) with
  | None, None -> None
  | Some v, None | None, Some v -> Some v
  | Some l, Some r -> Some (merge l r)

let option_map2 merge left right =
  match (left, right) with Some l, Some r -> Some (merge l r) | _ -> None

module MakeAddressOnly
    (SubAddress : Memory_sig.ADDRESS)
     =
struct
  let address2scalar x = x

  module Domain : Sig.Minimal_No_Boolean with module Context = SubAddress.Scalar.Context
                = SubAddress.Scalar

  include Domain
  module Scalar = SubAddress.Scalar

  (* A binary is either a numerical value, or an address. *)
  type binary = {
    (* true if an address, false if a scalar, top if both, bottom if none.   *)
    (* Note: probably this should be a  SubAddress.boolean, which is higher in the stack.
         we would thus not need to go from SubAddress.boolean to boolean. *)
    is_address : Scalar.boolean;
    (* Note: we don't use nullE and addressE directly,
        and use decomp (below) instead. *)
    nullE : Scalar.binary option;
    addressE : SubAddress.binary option;
    invalid : bool;
    id : int;  (** Unique identifier for comparisons. *)
  }

  (* Refine null and address using the is_address boolean. *)
  let decomp ctx binary =
    let res = Scalar.query_boolean ctx binary.is_address in
    match (res, binary.nullE, binary.addressE) with
    | Lattices.Quadrivalent.Bottom, _, _ -> (None, None)
    | Lattices.Quadrivalent.True, _, None -> (None, None)
    | Lattices.Quadrivalent.True, _, (Some _ as addr) -> (None, addr)
    | Lattices.Quadrivalent.False, None, _ -> (None, None)
    | Lattices.Quadrivalent.False, (Some _ as null), _ -> (null, None)
    | Lattices.Quadrivalent.Top, null, addr -> (null, addr)
    | _ -> .

  let fresh_id =
    let counter = ref 0 in
    fun () ->
      incr counter;
      !counter

  let pp scalar_pretty binary_pretty fmt x =
    let or_invalid =
      match x.invalid with false -> "" | true -> " or invalid"
    in
    match (x.nullE, x.addressE) with
    | None, None -> Format.fprintf fmt "<bottom>"
    | Some x, None -> Format.fprintf fmt "%a%s" scalar_pretty x or_invalid
    | None, Some x -> Format.fprintf fmt "%a%s" binary_pretty x or_invalid
    | Some n, Some adr ->
        Format.fprintf fmt "%a or %a%s" scalar_pretty n binary_pretty adr
          or_invalid

  let ppctx ctx scalar_pretty binary_pretty fmt x =
    let or_invalid =
      match x.invalid with false -> "" | true -> " or invalid"
    in
    (* Codex_log.feedback "is_address %a" (Scalar.boolean_pretty ctx) x.is_address; *)
    match decomp ctx x with
    | None, None -> Format.fprintf fmt "<bottom>"
    | Some scal, None -> Format.fprintf fmt "%a%s" scalar_pretty scal or_invalid
    | None, Some adr -> Format.fprintf fmt "%a%s" binary_pretty adr or_invalid
    | Some n, Some adr ->
        Format.fprintf fmt "%a or %a%s" scalar_pretty n binary_pretty adr
          or_invalid

  let binary_pretty ~size ctx =
    ppctx ctx
      (Scalar.binary_pretty ~size ctx)
      (SubAddress.binary_pretty ~size ctx)

  module Binary = struct
    type t = binary

    let pretty = pp Scalar.Binary.pretty SubAddress.Binary.pretty
    let hash { id; _ } = Hashtbl.hash id

    let compare x y =
      let c = Option.compare Scalar.Binary.compare x.nullE y.nullE in
      if c <> 0 then c
      else Option.compare SubAddress.Binary.compare x.addressE y.addressE

    let equal x y = compare x y = 0
  end

  module Binpred = struct
    (** binary operators: =, signed <=, unsigned <= and their negation *)
    type t =
      | B_EQ
      | B_SLE
      | B_ULE
      | B_NEQ
      | B_NSLE
      | B_NULE  (** Negate a binary operator *)

    let not = function
      | B_EQ -> B_NEQ
      | B_SLE -> B_NSLE
      | B_ULE -> B_NULE
      | B_NEQ -> B_EQ
      | B_NSLE -> B_SLE
      | B_NULE -> B_ULE

    let pretty fmt op =
      let open Format in
      match op with
      | B_EQ -> fprintf fmt "EQ"
      | B_SLE -> fprintf fmt "SLE"
      | B_ULE -> fprintf fmt "ULE"
      | B_NEQ -> fprintf fmt "NEQ"
      | B_NSLE -> fprintf fmt "NSLE"
      | B_NULE -> fprintf fmt "NULE"

    (** Value of a cross operation (scalar <op> address)*)
    let cross_comp =
      let open Lattices.Quadrivalent in
      function
      | B_EQ -> False
      | B_NEQ -> True
      | B_SLE | B_ULE | B_NSLE | B_NULE -> Top

    (** True not in binpred_cross_comp *)
    let is_positive op =
      match cross_comp op with Bottom | False -> true | Top | True -> false

    let op op =
      match op with
      | B_EQ -> Scalar.Binary_Forward.beq
      | B_SLE -> Scalar.Binary_Forward.bisle
      | B_ULE -> Scalar.Binary_Forward.biule
      | B_NEQ ->
          fun ~size ctx x y ->
            Scalar.(Boolean_Forward.not ctx @@ Binary_Forward.beq ~size ctx x y)
      | B_NSLE ->
          fun ~size ctx x y ->
            Scalar.(
              Boolean_Forward.not ctx @@ Binary_Forward.bisle ~size ctx x y)
      | B_NULE ->
          fun ~size ctx x y ->
            Scalar.(
              Boolean_Forward.not ctx @@ Binary_Forward.biule ~size ctx x y)

    (** Downcast binary operator to SubAddress booleans *)
    let rec addr_predicate ~size ctx = function
      | B_EQ -> SubAddress.beq ~size ctx
      (* Maybe: we could compare the bitvector representation of pointers here. *)
      | B_SLE -> fun _ _ -> SubAddress.boolean_unknown ctx
      | B_ULE -> SubAddress.ble ~size ctx
      | not_op ->
          fun l r ->
            SubAddress.Boolean_Forward.not ctx
              (addr_predicate size ctx (not not_op) l r)

    (** Downcast binary operator to Scalar booleans *)
    let rec scal_predicate ~size ctx = function
      | B_EQ -> Scalar.Binary_Forward.beq ~size ctx
      | B_SLE -> Scalar.Binary_Forward.bisle ~size ctx
      | B_ULE -> Scalar.Binary_Forward.biule ~size ctx
      | not_op ->
          fun l r ->
            Scalar.Boolean_Forward.not ctx
              (scal_predicate size ctx (not not_op) l r)
  end

  module AbstractBoolean = struct
    module Scalar = Scalar

    (** Elementary boolean types: quadrivalent or binpred *)
    type boolean =
      | BVal of Lattices.Quadrivalent.boolean
      | BBinpred of {
          pred : Binpred.t;
          size : In_bits.t;
          left : binary;
          right : binary;
        }
      (* Note: we should normally not encounter BUnvalid, as it is difficult to handle precisely. *)
      (* But we could handle it by adding a "pos" argument here saying whether we mean valid or invalid. *)
      | BValid of { size:In_bits.t; arg : binary }

    let compare a b =
      match (a, b) with
      | BVal va, BVal vb ->
          Single_value_abstraction.Quadrivalent.Boolean_Lattice.compare va vb
      | _ -> assert false

    let pretty fmt = function
      | BVal x -> Format.fprintf fmt "BVal(%a)" Lattices.Quadrivalent.pretty x
      | BValid { size; arg } ->
          Format.fprintf fmt "BValid{size=%d;arg=%a}" (size:>int) Binary.pretty arg
      | BBinpred { pred; size; left; right } ->
          Format.fprintf fmt "%a(@[%a@,@ %a@])" Binpred.pretty pred
            Binary.pretty left Binary.pretty right

    module Utils = struct
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

      (** Boolean query response *)
      type bool_response =
        | BR_Known of Lattices.Quadrivalent.boolean  (** Known quantity *)
        | BR_Scal of Scalar.boolean  (** Query to perform on scalars*)
        | BR_Addr of SubAddress.boolean  (** Query to perform on address *)

      (** Evaluate the response to a quadrivalent value *)
      let eval_response ctx = function
        | BR_Known k -> k
        | BR_Addr a -> SubAddress.query_boolean ctx a
        | BR_Scal s -> Scalar.query_boolean ctx s

      (** Performs binary operation op on a response *)
      let op_response ctx op_known l r =
        let l = eval_response ctx l in
        let r = eval_response ctx r in
        BR_Known (op_known l r)

      let join_scalar_bool ctx l r =
        let (Context.Result (_, tup, deserialize2)) =
          Scalar.serialize_boolean ctx l ctx r (true, Context.empty_tuple ())
        in
        let res_tup = Scalar.nondet_same_context ctx tup in
        fst @@ deserialize2 ctx res_tup

      let join_addr_bool ctx l r =
        let (Context.Result (_, tup, deserialize2)) =
          SubAddress.serialize_boolean ctx l ctx r (true, Context.empty_tuple ())
        in
        let res_tup = Scalar.nondet_same_context ctx tup in
        fst @@ deserialize2 ctx res_tup

      (** Joins two responses (boolean lattice union) *)
      let join_responses ctx = op_response ctx Lattices.Quadrivalent.join

      let and_response ctx =
        op_response ctx
          Single_value_abstraction.Quadrivalent.Boolean_Forward.( && )

      let or_response ctx =
        op_response ctx
          Single_value_abstraction.Quadrivalent.Boolean_Forward.( || )
    end

    (* Note: this is somewhat redundant with assume, as we cann join between
        assume true and assume false to get this. *)
    let query_boolean_response ctx boolean =
      let open Utils in
      match boolean with
      | BVal x -> BR_Known x
      | BValid { size; arg } -> (
          let arg_null, arg_address = decomp ctx arg in
          let cond_null =
            match (arg_null, Codex_config.valid_absolute_addresses ()) with
            | None, _ -> None
            | Some _, None -> Some (Scalar.Boolean_Forward.false_ ctx)
            | Some null, Some (min, max) ->
                let ptr_size = Codex_config.ptr_size () in
                let bytesize = In_bits.in_bytes size in
                let max = Z.sub max @@ Z.of_int (bytesize:>int) in
                let cond =
                  Scalar.Boolean_Forward.( && ) ctx
                    (Scalar.Binary_Forward.biule ~size:ptr_size ctx
                       (Scalar.Binary_Forward.biconst ~size:ptr_size min ctx)
                       null)
                    (Scalar.Binary_Forward.biule ~size:ptr_size ctx null
                    @@ Scalar.Binary_Forward.biconst ~size:ptr_size max ctx)
                in
                Some cond
          in
          let cond_address =
            match arg_address with
            | None -> None
            | Some x -> Some (SubAddress.within_bounds ~size ctx x)
          in
          match (cond_null, cond_address) with
          | None, None -> BR_Known Lattices.Quadrivalent.Bottom
          | Some null, None -> BR_Scal null
          | None, Some addr -> BR_Addr addr
          | Some null, Some addr ->
              join_responses ctx (BR_Scal null) (BR_Addr addr))
      | BBinpred { pred = binpred; size; left; right } -> (
          let open Lattices.Quadrivalent in
          if left.invalid || right.invalid then BR_Known Top
          else
            let lscal_empty, laddr_empty = decomp ctx left in
            let rscal_empty, raddr_empty = decomp ctx right in
            let cast_scal l r =
              BR_Scal (Binpred.scal_predicate ~size ctx binpred l r)
            in
            let cast_addr l r =
              BR_Addr (Binpred.addr_predicate ~size ctx binpred l r)
            in
            let join_mixed = BR_Known (Binpred.cross_comp binpred) in
            match (lscal_empty, laddr_empty, rscal_empty, raddr_empty) with
            | None, None, _, _ | _, _, None, None -> BR_Known Bottom
            | None, _, _, None | _, None, None, _ -> join_mixed
            | Some lscal, Some laddr, Some rscal, Some raddr ->
                join_responses ctx
                  (join_responses ctx (cast_scal lscal rscal) join_mixed)
                  (join_responses ctx (cast_addr laddr raddr) join_mixed)
            | Some lscal, None, Some rscal, None -> cast_scal lscal rscal
            | Some lscal, _, Some rscal, _ ->
                join_responses ctx (cast_scal lscal rscal) join_mixed
            | None, Some laddr, None, Some raddr -> cast_addr laddr raddr
            | _, Some laddr, _, Some raddr ->
                join_responses ctx (cast_addr laddr raddr) join_mixed)

    let boolean_empty _ = BVal Lattices.Quadrivalent.Bottom
    let boolean_unknown _ = BVal Lattices.Quadrivalent.Top

    let not ctx = function
      | BVal x ->
          BVal (Single_value_abstraction.Quadrivalent.Boolean_Forward.not x)
      | BBinpred { pred; size; left; right } ->
          BBinpred { pred = Binpred.not pred; size; left; right }
      | _ -> assert false

    let assume_aux ctx = function
      | BVal x -> (
          match x with
          | Sig.Quadrivalent.Bottom ->
              (Some (Scalar.boolean_empty ctx), None)
          | Sig.Quadrivalent.True -> (None, None)
          | Sig.Quadrivalent.False ->
              (Some (Scalar.Boolean_Forward.false_ ctx), None)
          | Sig.Quadrivalent.Top -> (None, None))
      | BValid { size; arg } -> (
          let arg_null, arg_address = decomp ctx arg in
          (* None if we won't be an address afterwards. Bools say if this is new. *)
          let learn_not_address, cond_address =
            match arg_address with
            | None -> (false, None)
            | Some x -> (
                let cond_address = SubAddress.within_bounds ~size ctx x in
                match SubAddress.query_boolean ctx cond_address with
                | Lattices.Quadrivalent.(False | Bottom) -> (true, None)
                | _ -> (false, Some cond_address))
          in
          let learn_not_scalar, cond_null =
            match (arg_null, Codex_config.valid_absolute_addresses ()) with
            | None, _ -> (false, None)
            | _, None -> (true, None)
            | Some null, Some (min, max) -> (
                let ptr_size = Codex_config.ptr_size () in
                let bytesize = In_bits.in_bytes size in
                let max = Z.sub max @@ Z.of_int (bytesize:>int) in
                let cond_null =
                  Scalar.Boolean_Forward.( && ) ctx
                    (Scalar.Binary_Forward.biule ~size:ptr_size ctx
                       (Scalar.Binary_Forward.biconst ~size:ptr_size min ctx)
                       null)
                    (Scalar.Binary_Forward.biule ~size:ptr_size ctx null
                    @@ Scalar.Binary_Forward.biconst ~size:ptr_size max ctx)
                in
                match Scalar.query_boolean ctx cond_null with
                | Lattices.Quadrivalent.(False | Bottom) -> (true, None)
                | Lattices.Quadrivalent.(True | Top) -> (false, Some cond_null))
          in
          match (cond_address, cond_null) with
          | None, None ->
              ( Some (Scalar.Boolean_Forward.false_ ctx),
                Some (SubAddress.Boolean_Forward.false_ ctx) )
          | Some _, Some _ -> (None, None (* Cannot learn anything. *))
          | None, Some cond_null ->
              if learn_not_address then
                ( Some
                    (Scalar.Boolean_Forward.( && ) ctx
                       (Scalar.Boolean_Forward.not ctx arg.is_address)
                       cond_null),
                  None )
              else (Some cond_null, None)
          | Some cond_address, None ->
              if learn_not_scalar then (Some arg.is_address, Some cond_address)
              else (None, Some cond_address))
      | BBinpred { pred = op; left; right; size } -> (
          (* Address of the one that is both a scalar and address. *)
          let scal__scal_addr is_address ascal bscal baddr =
            match op with
            | B_EQ ->
                let scalcond =
                  Binpred.scal_predicate ~size ctx op ascal bscal
                in
                ( Some
                    (Scalar.Boolean_Forward.( && ) ctx scalcond
                    @@ Scalar.Boolean_Forward.not ctx is_address),
                  None )
            | B_NEQ -> (
                (* If we compare with a singleton, we can remove it. *)
                match
                  ( Scalar.Query.Binary_Lattice.is_singleton ~size
                    @@ Scalar.Query.binary ~size ctx ascal,
                    Scalar.Query.Binary_Lattice.is_singleton ~size
                    @@ Scalar.Query.binary ~size ctx bscal )
                with
                (* We know that a must be some address. *)
                | Some a, Some b when Z.equal a b -> (Some is_address, None)
                (* Note: in the Some a, None case we cannot deduce anything,
                but nonrelaional domains can. *)
                | _ -> (None, None))
            | B_SLE | B_ULE -> (
                (* If we compare with a singleton, we can remove it. *)
                match
                  ( Scalar.Query.Binary_Lattice.is_singleton ~size
                    @@ Scalar.Query.binary ~size ctx ascal,
                    Scalar.Query.Binary_Lattice.is_singleton ~size
                    @@ Scalar.Query.binary ~size ctx bscal )
                with
                | Some a, Some b when Z.gt a b -> (Some is_address, None)
                | _ -> (None, None))
            | B_NSLE | B_NULE -> (
                (* If we compare with a singleton, we can remove it. *)
                match
                  ( Scalar.Query.Binary_Lattice.is_singleton ~size
                    @@ Scalar.Query.binary ~size ctx ascal,
                    Scalar.Query.Binary_Lattice.is_singleton ~size
                    @@ Scalar.Query.binary ~size ctx bscal )
                with
                | Some a, Some b when Z.leq a b -> (Some is_address, None)
                | _ -> (None, None))
          in

          let addr__scal_addr is_address aaddr bscal baddr =
            match op with
            | B_EQ ->
                ( Some is_address,
                  Some (Binpred.addr_predicate ~size ctx op aaddr baddr) )
            (* We could try to learn something from NEQ, but probably not worth it. *)
            | _ -> (None, None)
          in

          if left.invalid || right.invalid then (None, None)
          else
            let lscal, laddr = decomp ctx left in
            let rscal, raddr = decomp ctx right in
            match (lscal, laddr, rscal, raddr) with
            | None, None, _, _ | _, _, None, None ->
                ( Some (Scalar.Boolean_Forward.false_ ctx),
                  Some (SubAddress.Boolean_Forward.false_ ctx) )
            | (None, _, _, None | _, None, None, _) when Binpred.is_positive op
              ->
                ( Some (Scalar.Boolean_Forward.false_ ctx),
                  Some (SubAddress.Boolean_Forward.false_ ctx) )
            | None, _, _, None | _, None, None, _ -> (None, None)
            | Some ls, None, Some rs, None ->
                (Some (Binpred.scal_predicate ~size ctx op ls rs), None)
            | None, Some la, None, Some ra ->
                (None, Some (Binpred.addr_predicate ~size ctx op la ra))
            (* Note: for eq, we can deduce that both booleans are equal. *)
            | Some ls, Some la, Some rs, Some ra -> (None, None)
            | Some ls, Some la, Some rs, None ->
                scal__scal_addr left.is_address rs ls la
            | Some ls, None, Some rs, Some ra ->
                scal__scal_addr right.is_address ls rs ra
            | Some ls, Some la, None, Some ra ->
                addr__scal_addr left.is_address ra ls la
            | None, Some la, Some rs, Some ra ->
                addr__scal_addr right.is_address la rs ra)

    let assume ctx cond =
      match assume_aux ctx cond with
      | None, None -> Some ctx
      | Some a, None -> Scalar.assume ctx a
      | None, Some b -> SubAddress.assume ctx b
      | Some a, Some b ->
          Option.bind (Scalar.assume ctx a) (fun ctx -> SubAddress.assume ctx b)

    (** Returns a triplet (boolean on null, boolean on address, explicit value)
        which is to be joined to get the final evaluation *)
    let query_boolean ctx b =
      Utils.eval_response ctx (query_boolean_response ctx b)

    let satisfiable_quadri ctx x =
      let open Lattices.Quadrivalent in
      match x with
      | Bottom | False -> Smtbackend.Smtlib_sig.Unsat
      | Top | True -> Scalar.satisfiable ctx (Scalar.Boolean_Forward.true_ ctx)

    let satisfiable ctx b =
      match query_boolean_response ctx b with
      | BR_Known x -> satisfiable_quadri ctx x
      | BR_Scal s -> Scalar.satisfiable ctx s
      | BR_Addr a -> SubAddress.satisfiable ctx a

    let rec serialize_boolean :
        'a.
        Context.t ->
        boolean ->
        Context.t ->
        boolean ->
        bool * 'a Context.in_tuple ->
        (boolean, 'a) Context.result =
     fun ctxa a ctxb b ((inc, intup) as acc) ->
      match (a, b) with
      | BVal va, BVal vb ->
          Context.Result
            ( inc,
              intup,
              fun ctx out -> (BVal (Lattices.Quadrivalent.join va vb), out) )
      | BValid { size = sza; arg = va }, BValid { size = szb; arg = vb }
        when sza = szb ->
          let (Context.Result (inc, intup, des)) =
            serialize ~widens:false ~size:sza ctxa va ctxb vb acc
          in
          Context.Result
            ( inc,
              intup,
              fun ctx out ->
                let v, out = des ctx out in
                (BValid { size = sza; arg = v }, out) )
      | ( BBinpred { pred = pa; size = sza; left = la; right = ra },
          BBinpred { pred = pb; size = szb; left = lb; right = rb } )
        when sza = szb && pa = pb ->
          let (Context.Result (inc, intup, desl)) =
            serialize ~widens:false ~size:sza ctxa la ctxb lb acc
          in
          let (Context.Result (inc, intup, desr)) =
            serialize ~widens:false ~size:szb ctxa ra ctxb rb (inc, intup)
          in
          Context.Result
            ( inc,
              intup,
              fun ctx out ->
                let r, out = desr ctx out in
                let l, out = desl ctx out in
                (BBinpred { pred = pa; size = sza; left = l; right = r }, out)
            )
      | _ -> assert false
    (* Context.Result(inc, intup, fun ctx out -> BVal (Sub.boolean_unknown ctx), out) *)

    and serialize :
        'a.
        widens:bool ->
        size:In_bits.t ->
        Context.t ->
        binary ->
        Context.t ->
        binary ->
        bool * 'a Context.in_tuple ->
        (binary, 'a) Context.result =
     fun ~widens ~size ctxa a ctxb b (included, acc) ->
      let f_addr ctx = function
        | None -> SubAddress.binary_empty ~size ctx
        | Some a -> a
      in
      let f_null ctx = function
        | None -> Scalar.binary_empty ~size ctx
        | Some a -> a
      in
      let a_null, a_addr = decomp ctxa a in
      let b_null, b_addr = decomp ctxb b in
      let invalid = ref (a.invalid || b.invalid) in
      let (Context.Result (included, acc, d_is_address)) =
        Scalar.serialize_boolean ctxa a.is_address ctxb b.is_address
          (included, acc)
      in
      let (Context.Result (included, acc, des)) =
        match ((a_null, a_addr), (b_null, b_addr)) with
        | (None, None), (None, None) ->
            Context.Result (included, acc, fun ctx out -> ((None, None), out))
        | (Some nulla, None), (Some nullb, None) ->
            let (Context.Result (included, acc, d_null)) =
              Scalar.serialize_binary ~widens ~size ctxa nulla ctxb nullb
                (included, acc)
            in
            Context.Result
              ( included,
                acc,
                fun ctx out ->
                  let null, out = d_null ctx out in
                  ((Some null, None), out) )
        | (None, Some addra), (None, Some addrb) ->
            let (Context.Result (included, acc, d_address)) =
              SubAddress.serialize ~widens ~size ctxa addra ctxb addrb
                (included, acc)
            in
            Context.Result
              ( included,
                acc,
                fun ctx out ->
                  let addr, out = d_address ctx out in
                  ((None, Some addr), out) )
        | _ ->
            (* TODO : improve this part as it is not totally sound right now *)
            let (Context.Result (included, acc, d_null)) =
              Scalar.serialize_binary ~widens ~size ctxa (f_null ctxa a_null)
                ctxb (f_null ctxb b_null) (included, acc)
            in
            let (Context.Result (included, acc, d_address)) =
              SubAddress.serialize ~widens ~size ctxa (f_addr ctxa a_addr) ctxb
                (f_addr ctxb b_addr) (included, acc)
            in
            Context.Result
              ( included,
                acc,
                fun ctx out ->
                  let addr, out = d_address ctx out in
                  let null, out = d_null ctx out in
                  ((Some null, Some addr), out) )
      in
      Context.Result
        ( included,
          acc,
          fun ctx tup ->
            let (nullE, addressE), tup = des ctx tup in
            let is_address, tup = d_is_address ctx tup in
            let res =
              {
                is_address;
                addressE;
                nullE;
                id = fresh_id ();
                invalid = !invalid;
              }
            in
            (res, tup) )
  end

  module SymbBool = Symbolic_boolean.Make (AbstractBoolean)
  include SymbBool

  let boolean2scalar_bool _ = assert false
  let scalar_bool2boolean _ = assert false

  type enum = Scalar.enum

  let enum_empty = Scalar.enum_empty
  let enum_unknown = Scalar.enum_unknown
  let enum_pretty = Scalar.enum_pretty

  module Enum = Scalar.Enum

  let global_symbol ctx s = assert false
  let add_global_symbol ~size ctx name binary = assert false

  module Boolean_to_Binary = struct
    (* Common to binary comparison operations. *)
    let ar2 op1 ~size ctx a b =
      let a_null, a_address = decomp ctx a in
      let b_null, b_address = decomp ctx b in
      let invalid = a.invalid || b.invalid in
      let res =
        match ((a_address, a_null), (b_address, b_null)) with
        | (None, Some a_null), (None, Some b_null) ->
            op1 ~size ctx a_null b_null
        | (Some a_addr, None), (None, Some b_null) ->
            op1 ~size ctx
              (SubAddress.binary2scalar_binary ~size ctx a_addr)
              b_null (* TODO : use fonction from SubAddress *)
        | (None, Some a_null), (Some b_addr, None) ->
            op1 ~size ctx a_null
              (SubAddress.binary2scalar_binary ~size ctx b_addr)
        | (Some a_addr, None), (Some b_addr, None) ->
            op1 ~size ctx
              (SubAddress.binary2scalar_binary ~size ctx a_addr)
              (SubAddress.binary2scalar_binary ~size ctx b_addr)
        | (Some a_addr, Some a_null), (None, Some b_null) ->
            let scalar_and = Scalar.Boolean_Forward.( && ) in
            let scalar_or = Scalar.Boolean_Forward.( || ) in
            let scalar_not = Scalar.Boolean_Forward.not in
            scalar_or ctx
              (scalar_and ctx
                 (scalar_not ctx a.is_address)
                 (op1 ~size ctx a_null b_null))
              (scalar_and ctx a.is_address
                 (op1 ~size ctx
                    (SubAddress.binary2scalar_binary ~size ctx a_addr)
                    b_null))
        | (None, Some a_null), (Some b_addr, Some b_null) ->
            let scalar_and = Scalar.Boolean_Forward.( && ) in
            let scalar_or = Scalar.Boolean_Forward.( || ) in
            let scalar_not = Scalar.Boolean_Forward.not in

            scalar_or ctx
              (scalar_and ctx
                 (scalar_not ctx b.is_address)
                 (op1 ~size ctx a_null b_null))
              (scalar_and ctx b.is_address
                 (op1 ~size ctx a_null
                    (SubAddress.binary2scalar_binary ~size ctx b_addr)))
        | (Some a_addr, Some a_null), (Some b_addr, None) ->
            let scalar_and = Scalar.Boolean_Forward.( && ) in
            let scalar_or = Scalar.Boolean_Forward.( || ) in
            let scalar_not = Scalar.Boolean_Forward.not in

            scalar_or ctx
              (scalar_and ctx
                 (scalar_not ctx a.is_address)
                 (op1 ~size ctx a_null
                    (SubAddress.binary2scalar_binary ~size ctx b_addr)))
              (scalar_and ctx a.is_address
                 (op1 ~size ctx
                    (SubAddress.binary2scalar_binary ~size ctx a_addr)
                    (SubAddress.binary2scalar_binary ~size ctx b_addr)))
        | (Some a_addr, None), (Some b_addr, Some b_null) ->
            let scalar_and = Scalar.Boolean_Forward.( && ) in
            let scalar_or = Scalar.Boolean_Forward.( || ) in
            let scalar_not = Scalar.Boolean_Forward.not in

            scalar_or ctx
              (scalar_and ctx
                 (scalar_not ctx b.is_address)
                 (op1 ~size ctx
                    (SubAddress.binary2scalar_binary ~size ctx a_addr)
                    b_null))
              (scalar_and ctx b.is_address
                 (op1 ~size ctx
                    (SubAddress.binary2scalar_binary ~size ctx a_addr)
                    (SubAddress.binary2scalar_binary ~size ctx b_addr)))
        | (None, None), _ | _, (None, None) -> Scalar.boolean_empty ctx
        | (Some a_addr, Some a_null), (Some b_addr, Some b_null) ->
            Scalar.boolean_unknown ctx (* TODO : check if this is correct *)
      in
      if invalid then Log.warning (fun p -> p "invalid operation");
      res

    let quadri2scalar_bool ctx quadri =
      let open Lattices.Quadrivalent in
      match quadri with
      | True -> Scalar.Boolean_Forward.true_ ctx
      | False -> Scalar.Boolean_Forward.false_ ctx
      | Bottom -> Scalar.boolean_empty ctx
      | Top -> Scalar.boolean_unknown ctx
  end

  let binary_empty ~size ctx =
    {
      is_address = Scalar.boolean_empty ctx;
      id = fresh_id ();
      nullE = None;
      addressE = None;
      invalid = false;
    }

  let scal_is_singleton ~size ctx addr =
    Scalar.Query.Binary_Lattice.is_singleton ~size
      (Scalar.Query.binary ~size ctx addr)

  let addr_is_singleton ~size ctx addr =
    SubAddress.Query.Binary_Lattice.is_singleton ~size
      (SubAddress.Query.binary ~size ctx addr)

  module Enum_Forward = struct
    let caseof ~case ctx x = assert false
    let enum_const ~case ctx = assert false
  end

  module Query = struct
    module Boolean_Lattice = Lattices.Quadrivalent

    let convert_to_quadrivalent (x : Lattices.Quadrivalent.boolean) = x

    module Binary_Lattice = struct
      type t = {
        null : Scalar.Query.Binary_Lattice.t;
        address : bool; (* false = is not an address. *)
        invalid : bool;
      }

      module L = Lattices.Unimplemented.Bitvector_Lattice (struct
        type nonrec t = t

        let loc = __LOC__
      end)

      include (L : module type of L with type t := t)

      let is_bottom ~size x =
        x.invalid = false && x.address = false
        && Scalar.Query.Binary_Lattice.is_bottom ~size x.null

      let bottom ~size =
        {
          address = false;
          null = Scalar.Query.Binary_Lattice.bottom ~size;
          invalid = false;
        }

      let join ~size a b =
        {
          null = Scalar.Query.Binary_Lattice.join ~size a.null b.null;
          address = a.address || b.address;
          invalid = a.invalid || b.invalid;
        }

      let is_empty = is_bottom

      let is_singleton ~size x =
        (* Focus on the most interesting case *)
        if x.invalid || x.address then None
        else Scalar.Query.Binary_Lattice.is_singleton ~size x.null

      let fold_crop_unsigned ~size (x : t) ~inf ~sup acc f =
        assert (x.invalid = false);
        assert (x.address = false);
        Scalar.Query.Binary_Lattice.fold_crop_unsigned ~size x.null ~inf ~sup
          acc f
    end

    let binary ~size ctx x =
      let x_null, x_address = decomp ctx x in
      let null =
        match x_null with
        | None -> Scalar.Query.Binary_Lattice.bottom ~size
        | Some off -> Scalar.Query.binary ~size ctx off
      in
      let address = match x_address with None -> false | Some _ -> true in
      Binary_Lattice.{ invalid = x.invalid; null; address }

    module Enum_Lattice = Scalar.Query.Enum_Lattice

    let enum = Scalar.Query.enum
  end

  let serialize_enum = Scalar.serialize_enum

  let join_values ~size ctx list =
    match list with
    | [] -> Scalar.binary_empty ~size ctx
    | [ x ] -> x
    | a :: b ->
        let nondet_binary v1 v2 =
          let (Context.Result (_, tup, deserialize2)) =
            Scalar.serialize_binary ~widens:false ~size ctx v1 ctx v2
              (true, Context.empty_tuple ())
          in
          let res_tup = Scalar.nondet_same_context ctx tup in
          fst @@ deserialize2 ctx res_tup
        in
        List.fold_left nondet_binary a b

  (* What to put in invalid here is unclear. This operation can be
        used for completely unknon values. *)
  let binary_unknown ~size ctx =
    {
      is_address = Scalar.Boolean_Forward.false_ ctx;
      nullE = Some (Scalar.binary_unknown ~size ctx);
      id = fresh_id ();
      addressE = None;
      invalid = false;
    }

  module Binary_Forward = struct
    let buninit ~size ctx = binary_empty ~size ctx

    (* Operations on pointers. *)

    let numeric_operation ~size ctx null =
      {
        is_address = Scalar.Boolean_Forward.false_ ctx;
        id = fresh_id ();
        nullE = Some null;
        addressE = None;
        invalid = false;
      }

    let biconst ~size k ctx =
      numeric_operation ~size ctx @@ Scalar.Binary_Forward.biconst ~size k ctx

    let bofbool ~size ctx x =
      let scalar_bool =
        AbstractBoolean.Utils.quadri2scalar_bool ctx @@ query_boolean ctx x
      in
      numeric_operation ~size ctx
      @@ Scalar.Binary_Forward.bofbool ~size ctx scalar_bool

    let bchoose ~size cond ctx x =
      let null, addr = decomp ctx x in
      let x = { x with nullE = null; addressE = addr } in
      {
        is_address = x.is_address;
        (* XXX: Should probably use bchoose on booleans. *)
        id = fresh_id ();
        nullE =
          Option.map
            (fun x -> Scalar.Binary_Forward.bchoose ~size cond ctx x)
            x.nullE;
        addressE =
          Option.map (fun x -> SubAddress.bchoose ~size cond ctx x) x.addressE;
        invalid = x.invalid;
      }

    let bchoose ~size cond ctx x =
      let null, addr = decomp ctx x in
      let x = { x with nullE = null; addressE = addr } in
      {
        is_address = x.is_address;
        (* XXX: Should probably use bchoose on booleans. *)
        id = fresh_id ();
        nullE =
          (x.nullE
          |> Option.map @@ fun x ->
             Scalar.Binary_Forward.bchoose ~size cond ctx x);
        addressE =
          (x.addressE
          |> Option.map @@ fun x -> SubAddress.bchoose ~size cond ctx x);
        invalid = x.invalid;
      }

    let bindex ~size k ctx x e =
      let e_null, _ = decomp ctx e in
      let x_null, x_address = decomp ctx x in
      match e_null with
      | None -> binary_empty ~size ctx
      | Some e ->
          {
            is_address = x.is_address;
            id = fresh_id ();
            nullE =
              (x_null
              |> Option.map @@ fun x ->
                 if k < 0 then
                   let k =
                     Scalar.Binary_Forward.biconst ~size (Z.of_int (-k)) ctx
                   in
                   let off =
                     Scalar.Binary_Forward.bimul ~size ~flags:(Operator.Flags.Bimul.pack ~nsw:false ~nuw:false) ctx
                       k e
                   in
                   Scalar.Binary_Forward.bisub ~size
                     ~flags:(Operator.Flags.Bisub.pack ~nsw:false ~nuw:false ~nusw:false) ctx x off
                 else
                   let k =
                     Scalar.Binary_Forward.biconst ~size (Z.of_int k) ctx
                   in
                   let off =
                     Scalar.Binary_Forward.bimul ~size ~flags:(Operator.Flags.Bimul.pack ~nsw:false ~nuw:false) ctx
                       k e
                   in
                   Scalar.Binary_Forward.biadd ~size ~flags:(Operator.Flags.Biadd.pack ~nsw:false ~nuw:false ~nusw:false) ctx x off);
            addressE =
              (x_address
              |> Option.map @@ fun x -> SubAddress.bindex ~size k ctx x e);
            invalid = x.invalid;
          }

    let valid ~size _acc_type ctx (ptr : binary) =
      lift @@ BValid { size; arg = ptr }

    (* Common to arithmetical unary operations. *)
    let ar1 op1 ~size ctx x =
      let x_null, x_address = decomp ctx x in
      let invalid = x.invalid || (x_address <> None && x_null <> None) in
      let nullE =
        match (x_null, x_address) with
        | Some x, None -> Some (op1 ~size ctx x)
        | None, Some x ->
            Some (op1 ~size ctx @@ SubAddress.binary2scalar_binary ~size ctx x)
        | Some x, Some y -> Some (Scalar.binary_unknown ~size ctx)
        | _ -> None
      in
      {
        is_address = Scalar.Boolean_Forward.false_ ctx;
        id = fresh_id ();
        nullE;
        addressE = None;
        invalid;
      }

    let buext ~size ~oldsize ctx x =
      if size == oldsize then x
      else ar1 (Scalar.Binary_Forward.buext ~oldsize) ~size ctx x

    let bsext ~size ~oldsize ctx x =
      if size == oldsize then x
      else ar1 (Scalar.Binary_Forward.bsext ~oldsize) ~size ctx x

    let bextract ~size ~index ~oldsize ctx x =
      if size == oldsize then x
      else ar1 (Scalar.Binary_Forward.bextract ~index ~oldsize) ~size ctx x

    (* Common to arithmetical binary operations. *)
    let ar2 op1 ~size ctx a b =
      let a_null, a_address = decomp ctx a in
      let b_null, b_address = decomp ctx b in
      let invalid = a.invalid || b.invalid in
      let nullE =
        match ((a_address, a_null), (b_address, b_null)) with
        | (None, Some a), (None, Some b) -> Some (op1 ~size ctx a b)
        | (Some a, None), (None, Some b) ->
            Some (op1 ~size ctx (SubAddress.binary2scalar_binary ~size ctx a) b)
        | (None, Some a), (Some b, None) ->
            Some (op1 ~size ctx a (SubAddress.binary2scalar_binary ~size ctx b))
        | (Some a, None), (Some b, None) ->
            Some
              (op1 ~size ctx
                 (SubAddress.binary2scalar_binary ~size ctx a)
                 (SubAddress.binary2scalar_binary ~size ctx b))
        | (Some addr, Some null), (None, Some b) ->
            let v1 = op1 ~size ctx null b in
            let v2 =
              op1 ~size ctx (SubAddress.binary2scalar_binary ~size ctx addr) b
            in
            Some (join_values ~size ctx [ v1; v2 ])
        | (None, Some a), (Some addr, Some null) ->
            let v1 = op1 ~size ctx a null in
            let v2 =
              op1 ~size ctx a (SubAddress.binary2scalar_binary ~size ctx addr)
            in
            Some (join_values ~size ctx [ v1; v2 ])
        | (None, None), _ -> None
        | _, (None, None) -> None
        | _ -> assert false
      in
      if invalid && not (a.invalid || b.invalid) then
        Log.warning (fun p -> p "invalid operation");
      {
        is_address = Scalar.Boolean_Forward.false_ ctx;
        nullE;
        addressE = None;
        invalid;
        id = fresh_id ();
      }

    let bimul ~size ~flags ctx a b =
      ar2 (Scalar.Binary_Forward.bimul ~flags) ~size ctx a b

    let bxor ~size ctx a b = ar2 Scalar.Binary_Forward.bxor ~size ctx a b
    let band ~size ctx a b = ar2 Scalar.Binary_Forward.band ~size ctx a b
    let bor ~size ctx a b = ar2 Scalar.Binary_Forward.bor ~size ctx a b
    let bashr ~size ctx a b = ar2 Scalar.Binary_Forward.bashr ~size ctx a b
    let blshr ~size ctx a b = ar2 Scalar.Binary_Forward.blshr ~size ctx a b

    let bshl ~size ~flags ctx a b =
      ar2 (Scalar.Binary_Forward.bshl ~flags) ~size ctx a b

    let bisdiv ~size ctx a b = ar2 Scalar.Binary_Forward.bisdiv ~size ctx a b
    let bismod ~size ctx a b = ar2 Scalar.Binary_Forward.bismod ~size ctx a b
    let biudiv ~size ctx a b = ar2 Scalar.Binary_Forward.biudiv ~size ctx a b
    let biumod ~size ctx a b = ar2 Scalar.Binary_Forward.biumod ~size ctx a b

    let bconcat ~size1 ~size2 ctx a b =
      if size1 = In_bits.zero then b
      else if size2 = In_bits.zero then a
      else
        let a_null, a_address = decomp ctx a in
        let b_null, b_address = decomp ctx b in

        let nulla =
          match (a_null, a_address) with
          | Some null, Some addr ->
              let v = SubAddress.binary2scalar_binary ~size:size1 ctx addr in
              Some (join_values ~size:size1 ctx [ null; v ])
          | None, Some addr ->
              Some (SubAddress.binary2scalar_binary ~size:size1 ctx addr)
          | _ -> a_null
        in

        let nullb =
          match (b_null, b_address) with
          | Some null, Some addr ->
              let v = SubAddress.binary2scalar_binary ~size:size2 ctx addr in
              Some (join_values ~size:size2 ctx [ null; v ])
          | None, Some addr ->
              Some (SubAddress.binary2scalar_binary ~size:size2 ctx addr)
          | _ -> b_null
        in

        let invalid =
          a.invalid || b.invalid
          || (a_address <> None && a_null <> None)
          || (b_address <> None && b_null <> None)
        in

        let nullE =
          match (nulla, nullb) with
          | Some a, Some b ->
              Some (Scalar.Binary_Forward.bconcat ~size1 ~size2 ctx a b)
          | _ -> None
        in
        {
          is_address = Scalar.Boolean_Forward.false_ ctx;
          nullE;
          addressE = None;
          invalid;
          id = fresh_id ();
        }

    let bool_binpred op ~size (_ctx : Scalar.Context.t) left right =
      lift @@ BBinpred { pred = op; size; left; right }

    let biule = bool_binpred Binpred.B_ULE
    let bisle = bool_binpred Binpred.B_SLE
    let beq = bool_binpred Binpred.B_EQ

    let valid_ptr_arith ~size arith_typ ctx x y =
      Log.trace
        (fun p ->
          p "valid_ptr_arith %a %a" (binary_pretty ~size ctx) x
            (binary_pretty ~size ctx) y)
        ~pp_ret:Boolean.pretty
      @@ fun () ->
      let x_null, x_address = decomp ctx x in
      let y_null, y_address = decomp ctx y in

      match ((x_null, x_address), (y_null, y_address)) with
      | (None, None), _ | _, (None, None) -> boolean_empty ctx
      | (_, Some _), (_, Some _) when arith_typ = Operator.Plus ->
          Log.warning (fun p ->
              p "@[<hov 2>Addition of pointers %a and %a is probably an error@]"
                (binary_pretty ~size ctx) x (binary_pretty ~size ctx) y);
          Boolean_Forward.false_ ctx
      | (_, Some _), (_, Some _) (* when arith_typ = Operator.Minus *)
        ->
          Boolean_Forward.( && ) ctx
            (valid ~size Operator.Read ctx x)
            (valid ~size Operator.Read ctx y)
      | (Some _, None), (Some _, None) -> Boolean_Forward.true_ ctx
      | (_, Some _), _ -> valid ~size Operator.Read ctx x
      | _, (_, Some _) -> valid ~size Operator.Read ctx y

    (* TODO: Maybe we should raise alarm, even if we count them only
        after the fixpoint is reached, or we could perform a pass to
        collect the alarms. That would be simpler. *)

    (* MAYBE: Si l'un etait un pointeur et l'autre pas, transformer en bshift ou bindex si on peut. *)
    let biadd ~size ~flags ctx a b =
      Log.trace (fun p ->
          p "biadd %a %a" (binary_pretty ~size ctx) a (binary_pretty ~size ctx)
            b)
      @@ fun () ->
      let a_null, a_address = decomp ctx a in
      let b_null, b_address = decomp ctx b in
      let nullE =
        match (a_null, b_null) with
        | Some a, Some b ->
            Some (Scalar.Binary_Forward.biadd ~size ~flags ctx a b)
        | _ -> None
      in
      (* Translate to index calls *)
      let is_address, addressE =
        match ((a_address, a_null), (b_address, b_null)) with
        | (Some a, None), (None, Some b) ->
            ( Scalar.Boolean_Forward.true_ ctx,
              Some (SubAddress.bindex ~size 1 ctx a b) )
        | (None, Some a), (Some b, None) ->
            ( Scalar.Boolean_Forward.true_ ctx,
              Some (SubAddress.bindex ~size 1 ctx b a) )
        | (None, Some _), (None, Some _) ->
            (Scalar.Boolean_Forward.false_ ctx, None)
        | (None, None), (None, None) -> (Scalar.Boolean_Forward.false_ ctx, None)
        | (Some addr, Some null), (None, Some b) ->
            ( Scalar.boolean_unknown ctx,
              Some (SubAddress.bindex ~size 1 ctx addr b) )
        | (None, Some a), (Some addr, Some null) ->
            ( Scalar.boolean_unknown ctx,
              Some (SubAddress.bindex ~size 1 ctx addr a) )
        | (None, None), _ -> (Scalar.boolean_empty ctx, None)
        | _, (None, None) -> (Scalar.boolean_empty ctx, None)
        | _ -> assert false
      in
      let invalid =
        a.invalid || b.invalid || (b_address <> None && a_address <> None)
      in
      { is_address; nullE; addressE; invalid; id = fresh_id () }

    let bisub ~size ~flags ctx a b =
      let invalid = a.invalid || b.invalid in
      let a_null, a_address = decomp ctx a in
      let b_null, b_address = decomp ctx b in
      let a_null, a_address =
        match (a_null, a_address) with
        | Some null, Some addr -> (Some (Scalar.binary_unknown ~size ctx), None)
        | _ -> (a_null, a_address)
      in
      let b_null, b_address =
        match (b_null, b_address) with
        | Some null, Some addr -> (Some (Scalar.binary_unknown ~size ctx), None)
        | _ -> (b_null, b_address)
      in
      (* XXX: handle this case. Translate to shift/index calls? *)
      let is_address, addressE, nullE =
        match ((a_address, a_null), (b_address, b_null)) with
        | (Some a, None), (None, Some b) ->
            ( Scalar.Boolean_Forward.true_ ctx,
              Some (SubAddress.bindex ~size (-1) ctx a b),
              None )
        | (None, Some a), (Some b, None) ->
            ( Scalar.Boolean_Forward.true_ ctx,
              Some (SubAddress.bindex ~size (-1) ctx b a),
              None )
        | (None, Some a), (None, Some b) ->
            ( Scalar.Boolean_Forward.false_ ctx,
              None,
              Some (Scalar.Binary_Forward.bisub ~size ~flags ctx a b)
            )
        | (Some a, None), (Some b, None) ->
            ( Scalar.Boolean_Forward.false_ ctx,
              None,
              Some (SubAddress.bisub ~size ctx a b) )
        | (None, None), _ -> (Scalar.boolean_empty ctx, None, None)
        | _, (None, None) -> (Scalar.boolean_empty ctx, None, None)
        | _ -> assert false
      in
      { is_address; id = fresh_id (); nullE; addressE; invalid }

    let bshift ~size ~offset ~max ctx x =
      let x_null, x_address = decomp ctx x in
      {
        is_address = x.is_address;
        nullE =
          (match x_null with
          | None -> None
          | Some x ->
              Some
                (let k =
                   Scalar.Binary_Forward.biconst ~size (Z.of_int offset) ctx
                 in
                 Scalar.Binary_Forward.biadd ~size
                   ~flags:(Operator.Flags.Biadd.pack ~nsw:false ~nuw:false ~nusw:false) ctx x k));
        addressE =
          x_address
          |> Option.map (fun x -> SubAddress.bshift ~size ~offset ~max ctx x);
        invalid = x.invalid;
        id = fresh_id ();
      }

    let bchoose ~size choice ctx x =
      let x_null, x_address = decomp ctx x in
      {
        is_address = x.is_address;
        nullE =
          Option.map
            (fun x -> Scalar.Binary_Forward.bchoose ~size choice ctx x)
            x_null;
        addressE =
          Option.map (fun x -> SubAddress.bchoose ~size choice ctx x) x_address;
        invalid = x.invalid;
        id = fresh_id ();
      }
  end

  (* Here, we assume that the value is not invalid, as this
      operation is only created for initial values of variables. *)
  let binary_unknown_typed ~size ctx typ =
    let open TypedC in
    let typ = inlined typ in
    match typ.descr with
    | Ptr _ ->
        let addr = SubAddress.binary_unknown_typed ~size ctx typ in
        let bin = SubAddress.binary2scalar_binary ~size ctx addr in
        let zero = Scalar.Binary_Forward.biconst ~size Z.zero ctx in
        let cond =
          Scalar.Boolean_Forward.not ctx
          @@ Scalar.Binary_Forward.beq ~size ctx bin zero
        in
        let module Ext = Extend.Make (Scalar) in
        (* 0 is never a valid addr. *)
        Ext.imperative_assume ctx cond;
        {
          is_address = Scalar.boolean_unknown ctx;
          nullE = Some zero;
          id = fresh_id ();
          addressE = Some addr;
          invalid = false;
        }
    | _ -> binary_unknown ~size ctx

  let binary2scalar_binary ~size ctx value =
    let null, addr = decomp ctx value in
    match (null, addr) with Some n, None -> n | _ -> assert false

  let assume_type ~size ctx value typ = assert false

  let type_of ~size ctx value =
    let _, addr = decomp ctx value in
    Option.bind addr (fun a -> SubAddress.type_of ~size ctx a)

  let check_type ~size ctx typ x =
    let nul, addr = decomp ctx x in
    let block_data =
      Type_check_tree.create_typing_node x (binary_pretty ~size ctx) typ
    in
    match addr with
    | None -> Type_check_tree.no_error block_data
    | Some addr ->
        Type_check_tree.sub_check block_data
        @@ SubAddress.check_type ~size ctx typ addr

  let analyze_summary _ctx funtyp args = assert false

  let addresses_in_binary ~size ctxa a ctxb b =
    let null_a, addr_a = decomp ctxa a in
    let null_b, addr_b = decomp ctxb b in
    match (addr_a, addr_b) with Some _, Some _ -> [ (a, b) ] | _ -> []

  let serialize = AbstractBoolean.serialize

  (* Useless, but needed *)
  module Address = struct
    let beq = Binary_Forward.beq
    let ble = Binary_Forward.biule
    let bshift = Binary_Forward.bshift
    let bisub ~size _ = assert false
    let bindex ~size = assert false
    let bchoose = Binary_Forward.bchoose
    let within_bounds ~size _ = assert false
    let zero_offset ~size ~max ctx = assert false
    let query_boolean = query_boolean
  end

  include Address

  let serialize_binary = serialize
  let union = Scalar.union
end

(* Functor to help the instanciation. *)
module Make(Sub : Memory_sig.ADDRESS_AND_MAKE_MEMORY) =
struct
  module Scalar = Sub.Address.Scalar
  module Address = MakeAddressOnly (Sub.Address) 

  type address = Address.binary

  module Make_Memory
      (Block : Memory_sig.BLOCK with module Scalar = Scalar):
    Memory_sig.MEMORY
    with module Scalar = Scalar
     and module Address = Address
     and module Block = Block =
  struct
    module Scalar = Scalar
    module Address = Address
    module Block = Block
    module Region = Sub.Make_Memory(Block)

    module M:Memory_sig.WITH_BOOLEAN_REDEFINITION
      with module Context = Scalar.Context
       and type boolean = Region.boolean =
      Region
    include M

    module Value = Block.Value

    type address = Address.binary

    module Region_numeric_offset = Region_numeric_offset.Make (Scalar)
    module ConstantRegion = Region_numeric_offset.Make_Block (Value)

    type memory = {
      sub : Region.memory; (* Region when not a constant address. *)
      constant : ConstantRegion.block; (* Region for constant addresses. *)
    }

    let pretty ctx fmt x =
      match Codex_config.valid_absolute_addresses () with
      | None ->
          Format.fprintf fmt "{ constant = <none>; sub = %a; }"
            (Region.pretty ctx) x.sub
      | Some _ ->
          Format.fprintf fmt "{ constant = %a; sub = %a; }"
            (ConstantRegion.pretty ctx)
            x.constant (Region.pretty ctx) x.sub

    module Context = Scalar.Context
    let join_values ~size ctx list =
      match list with
      | [] -> Value.binary_empty ~size ctx
      | [ x ] -> x
      | a :: b ->
          let nondet_binary v1 v2 =
            let (Context.Result (_, tup, deserialize2)) =
              Value.serialize ~widens:false ~size ctx v1 ctx v2
                (true, Context.empty_tuple ())
            in
            let res_tup = Scalar.nondet_same_context ctx tup in
            fst @@ deserialize2 ctx res_tup
          in
          List.fold_left nondet_binary a b

    let memory_empty ctx =
      {
        sub = Region.memory_empty ctx;
        constant = ConstantRegion.block_empty ctx;
      }

    let load ~size (ctx : Value.Context.t) mem x =
      Log.trace
        (fun p -> p "load %a" (Address.binary_pretty ~size:(Codex_config.ptr_size()) ctx) x)
        ~pp_ret:(fun fmt x -> Value.binary_pretty ~size ctx fmt x)
      @@ fun () ->
      let x_null, x_address = Address.decomp ctx x in
      let l = [] in
      let l =
        match x_null with
        | None -> l
        | Some addr ->
            let res = ConstantRegion.load ~size ctx mem.constant addr in
            Log.debug (fun p ->
                p "result of my constant load : %a"
                  (Value.binary_pretty ~size ctx)
                  res);
            [ res ]
      in
      let l =
        match x_address with
        | None -> l
        | Some addr ->
            Log.debug (fun p ->
                p "Loading from address %a"
                  (Sub.Address.binary_pretty ~size:(Codex_config.ptr_size()) ctx)
                  addr);
            let res = Region.load ~size ctx mem.sub addr in
            res :: l
      in
      join_values ~size ctx l

    let typed_load ~size ctx mem x typ = assert false

    let store ~size ctx mem x value =
      let x_null, x_address = Address.decomp ctx x in
      match (x_null, x_address) with
      | None, None -> memory_empty ctx
      | _ ->
          let constant =
            match x_null with
            | None -> mem.constant
            | Some addr ->
                ConstantRegion.store ~size ctx mem.constant addr value
          in
          let sub =
            match x_address with
            | None -> mem.sub
            | Some address -> Region.store ~size ctx mem.sub address value
          in
          { constant; sub }

    let typed_store ~size ctx mem x typ value = assert false

    let join_blocks ctx list =
      match list with
      | [] -> Block.block_empty ctx
      | [ x ] -> x
      | a :: b ->
          let nondet_array b1 b2 =
            let (Context.Result (_, tup, deserialize2)) =
              Block.serialize ~widens:false ctx b1 ctx b2
                (true, Context.empty_tuple ())
            in
            let res_tup = Scalar.nondet_same_context ctx tup in
            fst @@ deserialize2 ctx res_tup
          in
          List.fold_left nondet_array a b

    let load_block (ctx : Value.Context.t) mem x : Block.block =
      let ptr_size = Codex_config.ptr_size () in
      Log.trace (fun p ->
          p "load_block %a" (Address.binary_pretty ~size:ptr_size ctx) x)
      @@ fun () ->
      let x_null, x_address = Address.decomp ctx x in

      let l = [] in
      let l = match x_null with None -> l | Some addr -> assert false in
      let l =
        match x_address with
        | None -> l
        | Some addr ->
            Log.debug (fun p ->
                p "Loading region from address %a"
                  (Sub.Address.binary_pretty ~size:ptr_size ctx)
                  addr);
            let res = Region.load_block ctx mem.sub addr in
            res :: l
      in
      join_blocks ctx l

    let store_block (ctx : Value.Context.t) mem x region =
      let x_null, x_address = Address.decomp ctx x in

      match (x_null, x_address) with
      | None, None -> memory_empty ctx
      | _ ->
          let constant =
            match x_null with None -> mem.constant | Some addr -> assert false
          in
          let sub =
            match x_address with
            | None -> mem.sub
            | Some address -> Region.store_block ctx mem.sub address region
          in
          { constant; sub }

    let serialize ~widens ctxa mema ctxb memb acc =
      let (Context.Result (included, acc, d_sub)) =
        Region.serialize ~widens ctxa mema.sub ctxb memb.sub acc
      in
      let (Context.Result (included, acc, d_const)) =
        ConstantRegion.serialize ~widens ctxa mema.constant ctxb memb.constant
          (included, acc)
      in
      Context.Result
        ( included,
          acc,
          fun ctx tup ->
            let constant, tup = d_const ctx tup in
            let sub, tup = d_sub ctx tup in
            ({ sub; constant }, tup) )

    let serialize_binary = serialize

    let malloc ~id ~malloc_size ctx mem =
      let address, sub = Region.malloc ~id ~malloc_size ctx mem.sub in
      ( Address.
          {
            is_address = Scalar.Boolean_Forward.true_ ctx;
            id = fresh_id ();
            addressE = Some address;
            nullE = None;
            invalid = false;
          },
        { sub; constant = mem.constant } )

    let free ctx mem (x : Address.binary) =
      let x_null, x_address = Address.decomp ctx x in
      (match x_null with
      | None -> ()
      | Some x_null ->
          Codex_log.error "Free on null address: %a"
            (Address.Scalar.binary_pretty ~size:(Codex_config.ptr_size ())
               ctx)
            x_null;
          Emit_alarm.emit_alarm Operator.Alarm.Free_on_null_address);
      match x_address with
      | None -> memory_empty ctx
      | Some address ->
          let sub = Region.free ctx mem.sub address in
          { sub; constant = mem.constant }

    let unknown ~level ctx =
      {
        sub = Region.unknown ~level ctx;
        constant =
          (match Codex_config.valid_absolute_addresses () with
          | None -> ConstantRegion.initial ctx In_bytes.zero
          | Some (_, max) -> ConstantRegion.initial ctx @@ In_bytes.of_int @@ Z.to_int max);
      }

    let should_focus ~(size:In_bits.t) ctx (mem : memory) (x : address) =
      Log.trace (fun p ->
          p "should_focus ~size:%d %a" (size:>int)
            (Address.binary_pretty ~size ctx)
            x)
      @@ fun () ->
      let x_null, x_address = Address.decomp ctx x in
      let is_address =
        match (x_null, x_address) with
        | Some _, Some _ -> Scalar.boolean_unknown ctx
        | Some _, _ -> Scalar.Boolean_Forward.false_ ctx
        | _, Some _ -> Scalar.Boolean_Forward.true_ ctx
        | _ -> Scalar.boolean_empty ctx
      in

      match x_address with
      | None -> None
      | Some address -> (
          match Region.should_focus ~size ctx mem.sub address with
          | None -> None
          | Some (base, off) ->
              (* TODO : substracts the offset *)
              let null = None in
              Some
                ( Address.
                    {
                      is_address;
                      id = fresh_id ();
                      nullE = null;
                      addressE = Some base;
                      invalid = x.invalid;
                    },
                  off ))

    let may_alias ~ptr_size ctx ~size1 ~size2 x y =
      let x_null, x_address = Address.decomp ctx x in
      let y_null, y_address = Address.decomp ctx y in
      let may_scalar_alias =
        match (x_null, y_null) with
        | None, _ | _, None -> false
        | Some x_null, Some y_null -> (
            let res =
              Address.Scalar.Binary_Forward.beq ~size:ptr_size ctx x_null
                y_null
            in
            let res = Address.Scalar.query_boolean ctx res in
            match res with
            | Lattices.Quadrivalent.(Bottom | False) -> false
            | _ -> true)
      in
      let may_address_alias =
        match (x_address, y_address) with
        | None, _ | _, None -> false
        | Some x_address, Some y_address ->
            Region.may_alias ~ptr_size ctx ~size1 ~size2 x_address y_address
      in
      may_scalar_alias || may_address_alias

    let is_weak ~size ctx value =
      let _, addr = Address.decomp ctx value in
      match addr with None -> false | Some a -> Region.is_weak ~size ctx a

    let addresses_in_memory ctxa mema ctxb memb =
      Region.addresses_in_memory ctxa mema.sub ctxb memb.sub
  end
end
