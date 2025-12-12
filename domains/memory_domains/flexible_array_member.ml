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

module Log = Tracelog.Make(struct let category = "Domains.Flexible_array_member" end);;
module TypedC = Types.TypedC
module Type_check_tree = Types.Type_check_tree
module In_bits = Units.In_bits
module In_bytes = Units.In_bytes
let in_bits = In_bytes.in_bits


module Make 
    (Value : Memory_sig.FIXED_SIZE_VALUE_DOMAIN)
    (Sub : Memory_sig.BLOCK
     with module Scalar = Value.Scalar
      and module Value = Value)
= struct

  include Sub.Value
  
  module Scalar = Scalar
  module Offset = Sub.Offset
  module Value = Sub.Value

  type offset = Offset.offset


  type block = {sized : (In_bits.t * Value.binary) option; unsized : Sub.block option}


  let index_zero (ctx : Scalar.Context.t) = 
    Offset.offset_zero ~max:None ctx

  let pretty (ctx : Context.t) fmt {sized;unsized} = 
    let open Format in
    fprintf fmt "{sized:%a; unsized:%a}"
      (fun fmt sized -> match sized with
        | None -> fprintf fmt "None"
        | Some (size,v) -> fprintf fmt "%a of size %d" (Value.binary_pretty ~size ctx) v (size:>int)
      ) sized
      (fun fmt unsized -> match unsized with
        | None -> fprintf fmt "None"
        | Some arr -> (Sub.pretty ctx fmt) arr
      ) unsized

  let block_empty ctx = {sized = None; unsized = None}

  let unknown ~level ctx = {sized = None; unsized = Some (Sub.unknown ~level ctx)}

  let initial ctx byte_size = 
    let size = byte_size |> in_bits in
    {sized = Some (size, Value.binary_unknown ~size ctx); unsized = None}

  (* Serialize functions for partitioned blocks *)
  
  
  let serialize
    : 'a. widens:bool -> Context.t -> block -> Context.t -> block -> ('a Context.in_acc) -> (block, 'a) Context.result
    = fun ~widens ctxa a ctxb b (included,in_tup) ->
    Log.trace (fun p -> p "serialize %a %a" (pretty ctxa) a (pretty ctxb) b) 
        @@ fun () -> 
    match a,b with
    | {sized = Some (sza, va); unsized = None}, {sized = Some (szb,vb); unsized = None} when sza = szb -> 
      let Context.Result (included, in_tup, d_value) = Value.serialize ~widens ~size:sza ctxa va ctxb vb (included, in_tup) in
      Context.Result(included, in_tup, fun ctx out -> 
        let v,out = d_value ctx out in
        {sized = Some (sza, v); unsized = None}, out
      )

    | {sized = None; unsized = Some arra}, {sized = None; unsized = Some arrb} -> 
      let Context.Result (included, in_tup, d_array) = Sub.serialize ~widens ctxa arra ctxb arrb (included, in_tup) in
      Context.Result(included, in_tup, fun ctx out ->
        let arr, out = d_array ctx out in
        {sized = None; unsized = Some arr}, out
      )

    | {sized = Some (sza, va); unsized = Some arra}, {sized = Some (szb, vb); unsized = Some arrb} when sza = szb -> 
      let Context.Result (included, in_tup, d_value) = Value.serialize ~widens ~size:sza ctxa va ctxb vb (included, in_tup) in
      let Context.Result (included, in_tup, d_array) = Sub.serialize ~widens ctxa arra ctxb arrb (included, in_tup) in
      Context.Result (included, in_tup, fun ctx out ->
          let arr, out = d_array ctx out in
          let value, out = d_value ctx out in
          {sized = Some (sza,value); unsized = Some arr}, out
        )

    | _ -> assert false


  let join_binary_values ~size ctx list =
    match list with
    | [] -> Value.binary_empty ~size ctx
    | [x]-> x
    | a::b ->
      let nondet_binary v1 v2 = 
        let Context.Result(_,tup,deserialize2) = Value.serialize ~widens:false ~size ctx v1 ctx v2
            (true, Context.empty_tuple ()) in
        let res_tup = Scalar.nondet_same_context ctx tup in
        fst @@ deserialize2 ctx res_tup
      in List.fold_left nondet_binary a b

  let join_block_values ctx list : block =
    match list with
    | [] -> block_empty ctx
    | [x]-> x
    | a::b ->
      let nondet_block arr1 arr2 = 
        let Context.Result(_,tup,deserialize2) = serialize ~widens:false ctx arr1 ctx arr2
            (true, Context.empty_tuple ()) in
        let res_tup = Scalar.nondet_same_context ctx tup in
        fst @@ deserialize2 ctx res_tup
      in List.fold_left nondet_block a b

  let to_sub ctx {sized;unsized} = 
    match sized, unsized with
    | None, None -> Sub.block_empty ctx
    | Some (size,v), None -> Sub.binary_to_block ~size ctx v
    | None, Some arr -> arr
    | Some (size,v), Some arr -> Sub.concat ctx (Sub.binary_to_block ~size ctx v) arr

  module Block_Forward = struct

    let sizeof (ctx : Context.t) {sized;unsized} : Offset.offset =
      let res = match unsized with 
        | None -> index_zero ctx
        | Some arr -> Sub.sizeof ctx arr
      in
      let res = match sized with
        | None -> res
        | Some (sz,_) -> 
          let offset = In_bits.in_bytes sz |> Units.In_bytes.to_int in
          Offset.offset_shift ~offset ~max:None ctx res
      in res

    let concat (ctx : Context.t) a b =
      Log.trace (fun p -> p "concat %a %a" (pretty ctx) a (pretty ctx) b) 
        @@ fun () -> 
      match a,b with
      | {sized = None; unsized = None}, _-> a
      | _,{sized = None; unsized = None} -> b
      | {sized = Some (sza,va); unsized = None}, {sized = Some (szb,vb); unsized} ->
          {sized = Some (In_bits.(sza + szb), Value.Binary_Forward.bconcat ~size1:sza ~size2:szb ctx va vb); unsized}
      | {sized; unsized = Some arra}, {sized = None; unsized = Some arrb} -> 
          {sized; unsized = Some (Sub.concat ctx arra arrb)}
      | {sized; unsized = None}, {sized = None; unsized} -> {sized; unsized}
      | {sized; unsized = Some arra}, _ ->
          {sized; unsized = Some (Sub.concat ctx arra @@ to_sub ctx b)}


    let binary_to_block ~(size:In_bits.t) ctx value =
      assert ((size:>int) mod 8 = 0) ;
      {sized = Some (size, value); unsized = None}

    let binary_is_zero (ctx : Scalar.Context.t) x =
      let ptr_size = Codex_config.ptr_size () in  
      match Scalar.Query.(Binary_Lattice.is_singleton ~size:ptr_size @@ binary ~size:ptr_size ctx x) with
      | None -> false | Some x -> Z.equal x Z.zero
    ;;

    let index_lt (ctx : Scalar.Context.t) x y = 
      let res = Offset.Scalar.Boolean_Forward.not ctx @@ Offset.offset_le ctx y x in
      match Scalar.query_boolean ctx @@ Offset.boolean2scalar_bool ctx res with
      | Lattices.Quadrivalent.True -> true
      | _ -> false
    ;;

    let index_le (ctx : Scalar.Context.t) x y = 
      let res = Offset.offset_le ctx x y in
      match Scalar.query_boolean ctx @@ Offset.boolean2scalar_bool ctx res with
      | Lattices.Quadrivalent.True -> true
      | _ -> false
    ;;

    let index_gt (ctx : Scalar.Context.t) x y = 
      let res = Offset.offset_le ctx x y in
      let res = Offset.Scalar.Boolean_Forward.not ctx @@ Offset.boolean2scalar_bool ctx res in
      match Scalar.query_boolean ctx res with
      | Lattices.Quadrivalent.True -> true
      | _ -> false
    ;;


    let index_ge (ctx : Scalar.Context.t) x y = 
      let res = Offset.offset_le ctx y x in
      match Scalar.query_boolean ctx @@ Offset.boolean2scalar_bool ctx res with
      | Lattices.Quadrivalent.True -> true
      | _ -> false
    ;;

    let offset_to_singleton (ctx : Scalar.Context.t) x =
      let ptr_size = Codex_config.ptr_size () in  
      let zero = index_zero ctx in
      let res = Offset.offset_sub ctx x zero in
      match Scalar.Query.(Binary_Lattice.is_singleton ~size:ptr_size @@ binary ~size:ptr_size ctx res) with
      | Some v -> Z.to_int v
      | None -> assert false
    ;;

    (* Load operations (for enumerable index and not) *)
    let load_from_sized ~(size:In_bits.t) ~oldsize (ctx : Context.t) block index : Value.binary =
      let ptr_size = Codex_config.ptr_size () in  
      Log.trace (fun p -> p "load_from_sized ~size:%d ~oldsize:%a arr:%a index:%a"
        (size:>int)
        Z.pp_print oldsize
        (pretty ctx) block
        (Offset.offset_pretty ctx) index
      ) 
        ~pp_ret:Binary.pretty
        @@ fun () -> 
      let zero = index_zero ctx in
      let offset = Offset.offset_sub ctx index zero in
      let res = Scalar.Query.Binary_Lattice.fold_crop_unsigned ~size:ptr_size (Scalar.Query.binary ~size:ptr_size ctx offset) ~inf:Z.zero ~sup:oldsize []
        (fun i acc -> 
          let idx = Z.to_int i in
          let index = In_bytes.(in_bits @@ of_int idx) in
          match block with
          | {sized = Some (block_size, v); unsized = None} -> 
            let loaded = Value.Binary_Forward.bextract ~size ~index ~oldsize:block_size ctx v in
            loaded :: acc

          | {sized = None; unsized = Some arr} ->
            let offset = Offset.offset_shift ~offset:idx ~max:None ctx zero in
            let loaded = Sub.load ~size ctx arr offset in
            loaded :: acc

          | _ -> raise Not_found (* assert false *)
          
        ) in join_binary_values ~size:size ctx res

    let load ~(size:In_bits.t) (ctx : Context.t) block index : Value.binary =
      let ptr_size = Codex_config.ptr_size () in  
      Log.trace (fun p -> p "load ~size:%d %a %a"
        (size:>int) (pretty ctx) block (Offset.offset_pretty ctx) index
      ) 
        ~pp_ret:Binary.pretty
        @@ fun () -> 
      assert ((size:>int) mod 8 = 0) ;
      let zero = index_zero ctx in
      (* let index_end = Offset.offset_shift ~size:ptr_size ~offset:(size / 8) ~max:None ctx index in *)
      let block_size = sizeof ctx block in

      (* let index_in_bounds = index_le ctx zero index && index_le ctx index_end block_size in *)
      let total_size = Offset.offset_sub ctx block_size zero in
      Log.debug (fun p -> p "in load, total_size = %a" (Scalar.binary_pretty ~size:ptr_size ctx) total_size) ;
      match Scalar.Query.(Binary_Lattice.is_singleton ~size:ptr_size @@ binary ~size:ptr_size ctx total_size) with
      | Some oldsize -> 
        (* case 1 : size of arr is constant *)
        load_from_sized ~size ~oldsize ctx block index

      | None ->
        (* case 2 : size of arr is not a constant *)
        (* let byte_size = size / 8 in *)

        let zero = Offset.offset_zero ~max:None ctx in
        begin
          match block with
          | {sized = None; unsized = Some arr} -> Sub.load ~size ctx arr index

          | {sized = Some (prefix_size, v); unsized = Some arr} ->
            let prefix_size_in_bytes = prefix_size |> In_bits.in_bytes |> In_bytes.to_int in
            let prefix_offset = Offset.offset_shift ~offset:prefix_size_in_bytes ~max:None ctx zero in
            if index_ge ctx index prefix_offset then
              (* TODO: This is wrong. Instead, the offset should already be structured in
                 the "either we are in the prefix, or we are in the suffix" case, and
                 we should be taking the suffix part of this structured offset. *)
              let offset = -(prefix_size_in_bytes) in
              let new_index = Offset.offset_shift ~offset ~max:None ctx index in
              Sub.load ~size ctx arr new_index
            else if index_lt ctx index prefix_offset then 
              let oldsize = Z.of_int (prefix_size_in_bytes) in
              load_from_sized ~size ~oldsize ctx {sized = Some (prefix_size, v); unsized = None} index

            else raise Not_found

          | _ -> raise Not_found
        end

    (* Store operations (for enumerable index and not) *)
    let store_from_sized ~(size:In_bits.t) ~oldsize (ctx : Context.t) block index value : block =
      let ptr_size = Codex_config.ptr_size () in  
      Log.trace (fun p -> p "store_from_sized ~size:%d ~oldsize:%a arr:%a index:%a value:%a"
        (size:>int)
        Z.pp_print oldsize
        (pretty ctx) block
        (Offset.offset_pretty ctx) index
        (Value.binary_pretty ~size ctx) value
      ) @@ fun () -> 
      let zero = index_zero ctx in
      let offset = Offset.offset_sub ctx index zero in
      let res = Scalar.Query.Binary_Lattice.fold_crop_unsigned ~size:ptr_size (Scalar.Query.binary ~size:ptr_size ctx offset) ~inf:Z.zero ~sup:(Z.pred oldsize) []
          (fun i acc -> 
            let index = Z.to_int i in
            let index = index * 8 |> In_bits.of_int in
            Log.debug (fun p -> p "in store_from_sized, index = %d" (index:>int)) ;
            match block with
            | {sized = Some (block_size, v); unsized = None} ->
              let res =
                if index > In_bits.zero then
                  let v1 = Value.Binary_Forward.bextract ~size:index ~index:In_bits.zero ~oldsize:block_size ctx v in
                  Value.Binary_Forward.bconcat ~size1:size ~size2:index ctx value v1
                else value
              in
              let res =
                if In_bits.(index + size < block_size) then
                  let v2 = Value.Binary_Forward.bextract ~size:In_bits.(block_size - index - size) ~index:In_bits.(index + size) ~oldsize:block_size ctx v in
                  Value.Binary_Forward.bconcat ~size1:In_bits.(block_size - index - size) ~size2:In_bits.(index + size) ctx v2 res
                else res
              in
              Log.debug (fun p -> p "in store_from_sized, res = %a" (binary_pretty ~size:In_bits.(of_int @@ 8 * Z.to_int oldsize) ctx) res) ;
              res :: acc

            | _ -> raise Not_found
            
          )  in
      let size = Z.to_int oldsize |> In_bytes.of_int |> in_bits in
      let value = join_binary_values ~size ctx res in
      {sized = Some (size, value); unsized = None}


    let store ~(size:In_bits.t) (ctx : Context.t) block index value : block =
      let ptr_size = Codex_config.ptr_size () in  
      Log.trace (fun p -> p "store ~size:%d %a %a %a" (size:>int)
        (Offset.offset_pretty ctx) index
        (pretty ctx) block
        (Value.binary_pretty ~size ctx) value 
      ) @@ fun () -> 
      assert ((size:>int) mod 8 = 0) ;
      let zero = index_zero ctx in
      (* let index_end = Offset.offset_shift ~size:ptr_size ~offset:(size / 8) ~max:None ctx index in *)
      let block_size = sizeof ctx block in

      let total_size = Offset.offset_sub ctx block_size zero in
      match Scalar.Query.(Binary_Lattice.is_singleton ~size:ptr_size @@ binary ~size:ptr_size ctx total_size) with
      | Some oldsize -> 
        (* case 1 : size of arr is constant *)
        store_from_sized ~size ~oldsize ctx block index value

      | None ->
        begin
          let zero = Offset.offset_zero ~max:None ctx in
          match block with
          | {sized = None; unsized = Some arr} -> {sized = None; unsized = Some (Sub.store ~size ctx arr index value)}

          | {sized = Some (prefix_size, v); unsized = Some arr} ->
            let prefix_offset = Offset.offset_shift ~offset:(In_bits.(in_bytes prefix_size):>int) ~max:None ctx zero in
            if index_ge ctx index prefix_offset then 
              let offset = - (In_bits.(in_bytes prefix_size):>int) in
              let new_index = Offset.offset_shift ~offset ~max:None ctx index in
              let arr = Sub.store ~size ctx arr new_index value in
              {sized = Some (prefix_size, v); unsized = Some arr}
            else if index_lt ctx index prefix_offset then 
              let oldsize = Z.of_int (In_bits.(in_bytes prefix_size):>int) in
              let res = store_from_sized ~size ~oldsize ctx {sized = Some (prefix_size, v); unsized = None} index value in
              {res with unsized = Some arr}

            else raise Not_found

          | _ -> raise Not_found
        end

  end

  include Block_Forward


  let fresh_int =
    let fresh_counter = ref (0 : int) in
    fun () ->
      incr fresh_counter ;
      !fresh_counter

  let fresh_symbol () = Format.sprintf "a#%d" (fresh_int ())


  let rec block_unknown_typed_from_record ctx list =
    let folder acc_value typ =
      let value = block_unknown_typed ctx typ in
      concat ctx value acc_value
    in
    let typ = List.hd list in
    let first_value = block_unknown_typed ctx typ in
    let value = List.fold_left folder first_value (List.tl list) in
    value

  and block_unknown_typed ctx typ : block =
    let open TypedC in
    Log.trace
      (fun p -> p "block_unknown_typed %a" pp typ)
      @@ fun () ->
        let typ = inlined typ in
        match typ.descr with
        | StructureFAM {structure; array } ->
            block_unknown_typed_from_record ctx [array;structure]
        | Array (tp, Variable_length s) ->
            { sized = None; unsized = Some (Sub.block_unknown_typed ctx typ) }
        | Existential {bound_typ;bound_var;body}  ->
            let exists_sz = TypedC.sizeof bound_typ |> in_bits in
            let res = binary_unknown_typed ~size:exists_sz ctx bound_typ in
            let new_symb = fresh_symbol () in
            Value.add_global_symbol ~size:exists_sz ctx new_symb res;
            let new_t = substitute_symbol body bound_var new_symb in
            block_unknown_typed ctx new_t
        | Union { un_byte_size = None } -> assert false
         | _ -> 
           let size = TypedC.sizeof typ |> in_bits in
            binary_to_block ~size ctx @@ binary_unknown_typed ~size ctx typ

  module Pred_to_Value = struct

    module BF = Value.Binary_Forward

    let rec binary_of_value ~size ctx v =
      let open TypedC in
      match v with
      | Sym s -> snd @@ Value.global_symbol ctx s

    and cond_of_cmp ~size ctx cmpop v1 v2 =
      let open TypedC.Pred in
      match cmpop with
      | Equal ->
          BF.beq ~size ctx v1 v2
      | NotEqual ->
          Boolean_Forward.not ctx @@ BF.beq ~size ctx v1 v2
      | ULt ->
          Boolean_Forward.not ctx @@ BF.biule ~size ctx v2 v1
      | SLt ->
          Boolean_Forward.not ctx @@ BF.bisle ~size ctx v2 v1
      | ULeq ->
          BF.biule ~size ctx v1 v2
      | SLeq ->
          BF.bisle ~size ctx v1 v2
      | UGeq ->
          BF.biule ~size ctx v2 v1
      | SGeq ->
          BF.bisle ~size ctx v2 v1
      | UGt ->
          Boolean_Forward.not ctx @@ BF.biule ~size ctx v1 v2
      | SGt ->
          Boolean_Forward.not ctx @@ BF.bisle ~size ctx v1 v2

    and lift_unop ~size ctx op =
      let open TypedC.Pred in
      match op with
      | Extract (index,len) -> fun x ->
        BF.bextract ~size:len ~index ~oldsize:size ctx x
        |> BF.buext ~size ~oldsize:len ctx

    and lift_binop ~size ctx op =
      let open TypedC.Pred in
      match op with
      | Add -> BF.biadd ~size ctx ~flags:(Operator.Flags.Biadd.pack ~nsw:false ~nuw:false ~nusw:false)
      | Sub -> BF.bisub ~size ctx ~flags:(Operator.Flags.Bisub.pack ~nsw:false ~nuw:false ~nusw:false)
      | Mul -> BF.bimul ~size ctx ~flags:(Operator.Flags.Bimul.pack ~nsw:false ~nuw:false)
      | Div -> BF.bisdiv ~size ctx
      | And -> BF.band ~size ctx
      | Or -> BF.bor ~size ctx
      | Concat (size1,size2) -> BF.bconcat ~size1 ~size2 ctx
      | Mod -> BF.bismod ~size ctx

    and binary_of_expr ~size ctx ~self e =
      let open TypedC.Pred in
      match e with
      | Const x -> BF.biconst ~size x ctx
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
      | Mutval (mut,p) -> cond_of_pred ~size ctx (evaluate_mutval mut p) ~self

  end


  (** Unification for existential types *)
  module Unification = struct

    module BF = Value.Binary_Forward

    module StringMap = struct
      include Map.Make(String)

      let replace ~size ctx key value map =
        let prev = find key map in
        match prev, value with
        | Some p, Some v ->
          begin
            let comp = BF.beq ~size ctx p v in
            let is_true = query_boolean ctx comp in
            match is_true with
            | Lattices.Quadrivalent.True -> map
            | _ -> raise Not_found
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
            (BF.bisub ~size ~flags:(Operator.Flags.Bisub.pack ~nsw:false ~nuw:false ~nusw:false) ctx self (Pred_to_Value.binary_of_expr ~size ctx ~self e))
          ) env

      | Cmp (Equal, Self, Binop (Sub, Val(Sym s), e))
      | Cmp (Equal, Binop (Sub, Val(Sym s), e), Self) -> 
        if contains_local_var e env && is_exist_var s then env
        else 
          StringMap.replace ~size ctx s (Some
            (BF.biadd ~size ~flags:(Operator.Flags.Biadd.pack ~nsw:false ~nuw:false ~nusw:false) ctx self (Pred_to_Value.binary_of_expr ~size ctx ~self e))
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

    let unify_type ~size ctx typ1 typ2 env = 
      let open TypedC in
      Log.trace (fun p -> p "unify_type %a %a" pp typ1 pp typ2) 
      @@ fun () -> 
      let bottom = binary_empty ~size ctx in
      match typ1.descr, typ2.descr with
      | Application {constr = c1;args =  exprs1}, Application {constr = c2;args = exprs2} ->
          let env = List.fold_left (fun env (e1,e2) -> unify_expr e1 (Pred_to_Value.binary_of_expr ~size ctx ~self:bottom e2) env) env @@ List.combine exprs1 exprs2 in
        {typ1 with descr = Application {constr = c1;args = exprs1}}, env

        | Array (t1, Variable_length s1), Array (t2, Variable_length s2) when StringMap.mem s1 env ->
          let ptr_size = Codex_config.ptr_size () in
          let sz, value = Value.global_symbol ctx s2 in
          assert (ptr_size = sz) ;
          t1, StringMap.add s1 (Some value) env (* TODO : check unify_type on t1 as well *)
          
        | _ -> typ1, env

    let unify_type ~size ctx typ ptr_typ env =
      let open TypedC in
      match typ, ptr_typ with
      | {descr = Ptr {pointed=ptyp1}}, Some {descr = Ptr {pointed=ptyp2}} -> 
        unify_type ~size ctx ptyp1 ptyp2 env

      | {descr = Ptr {pointed}; pred}, None -> pointed, env

      | _ -> assert false

    
    let rec unify_record ~size ctx lst value env =
      let open TypedC in
      let members, env = List.fold_right (fun (ofs,name,t) (lst, env) ->
        let size_bytes = sizeof t in
        let size_bits = size_bytes |> in_bits in
        let index = ofs |> in_bits in
        let part = Binary_Forward.bextract ~size:size_bits ~oldsize:size ~index ctx value in
        let typ, env = unify ~size:size_bits ctx t part env in
        (ofs,name, typ) :: lst, env
      ) lst ([],env)
      in members, env

    (* TODO : Improve this *)
    and unify ~size ctx typ (value : binary) env = 
      let open TypedC in
      Log.trace (fun p -> p "VUC.unify %a %a" pp typ (binary_pretty ~size ctx) value) 
      @@ fun () -> 
      let typ = inlined typ in
      let env = unify_pred ~size ctx typ.pred ~self:value env in
      match typ.descr with
        | Void -> typ, env
        | Base (sz, _) -> typ, env
        | Structure ({st_byte_size = sz; st_members} as st) ->
            assert (8 * (sz:>int) = (size:>int));
            let st_members, env = unify_record ~size ctx st_members value env in
            {typ with descr = Structure {st with st_members}}, env
        
        | StructureFAM _ -> assert false (* TODO *)
        
        | Ptr ptr -> 
          let ptr_typ = Value.type_of ~size ctx value in
          let pointed,env = unify_type ~size ctx typ ptr_typ env in
          let typ = Build.ptr pointed typ.pred in
          typ, env
        
        | Array (tp, ((Fixed_length t) as sz)) ->
            let elem_size = sizeof tp |> in_bits in
            let t,env = unify ~size:elem_size ctx tp value env in
            {typ with descr = Array (t, sz)}, env  

        | Array _ -> failwith "Unification attempt with arbitrary array"
        
        | Function _ ->
            failwith "Unification attempt with a function"

        | Application _ -> failwith ("Unification attemp with non-applied constructor type")

        | Existential {bound_typ;bound_var;body}  ->
          begin
            let env' = StringMap.add bound_var None env in
            let t, new_env = unify ~size ctx body value env' in
            let v =
              match StringMap.find bound_var new_env with None -> binary_unknown ~size ctx | Some v -> v
            in
             (* raise Not_found ; *) (* TODO : could be omitted if the existential variable is not used anywhere *)
            let exist_size = sizeof bound_typ |> in_bits in
            let new_var = fresh_symbol () in
            Value.add_global_symbol ~size:exist_size ctx new_var v ;
            substitute_symbol t bound_var new_var, new_env
          end
      
        | Union ({un_types;} as un) -> 
              let new_lst, env' =
                List.fold_left (fun (ls,map) (sz,typ) -> let t,m = unify ~size ctx typ value map in (sz,t):: ls,m) ([],env) un_types
              in
              let new_lst = List.rev new_lst in
              {typ with descr = Union {un with un_types = new_lst}}, env'
          
        | Weak _ -> assert false

    (* let unify ctx typ value = fst @@ unify ctx typ value StringMap.empty *)
    
  end


  let check_type ctx typ block =
    Log.trace (fun p -> p "check_type %a %a" TypedC.pp typ (pretty ctx) block) 
    @@ fun () -> 
    let open Type_check_tree in
    let block_data = create_typing_node block ~printer:(pretty ctx) typ in 
    let typing_store () =
      Codex_log.error "Type %a possibly does not hold for block %a" TypedC.pp
        typ (pretty ctx) block;
      type_error block_data Operator.Alarm.Typing_store in
    let open TypedC in
    let typ = inlined typ in
    match typ.descr with
    | StructureFAM { structure; array } -> (
        match block with
        | { sized = None; _ } -> assert false
        | { unsized = None; _ } -> assert false (* Not a FAM. *)
        | { sized = Some (size, value); unsized = Some ar_block } ->
            assert (TypedC.sizeof structure |> in_bits = size);
            and_ block_data
              [
                Value.check_type ~size ctx structure value;
                Sub.check_type ctx array ar_block;
              ]
    )
    | Array (elem_typ, Variable_length s) -> (
        match block with
        | { sized = None; unsized = Some arr } -> 
            Sub.check_type ctx typ arr
            |> sub_check block_data
        | _ -> assert false)
    | Existential _  -> (
        match block with
        | { sized = Some (size, v); unsized = None } ->
            Value.check_type ~size ctx typ v
            |> sub_check block_data
        | _ ->
            Codex_log.error
              "Storing value@ %a to region of type@ %a: no unification is made"
              (pretty ctx) block pp typ;
            typing_store ())
    | Union { un_byte_size = None } -> assert false
    | Weak _ ->
      Codex_log.error "Reaching non unified weak type.";
      type_error block_data Operator.Alarm.Weak_type_use;
    | _ ->
        Log.debug (fun p ->
            p "in check_type, comparing type %a with block %a" TypedC.pp typ
              (pretty ctx) block);
        let size = offset_to_singleton ctx @@ Block_Forward.sizeof ctx block in
        (* assert (size = TypedC.sizeof typ) ; *)
        if size = (TypedC.sizeof typ:>int) then
          let size = TypedC.sizeof typ |> in_bits in
          let zero = index_zero ctx in
          let value = load ~size ctx block zero in
          Value.check_type ~size ctx typ value
          |> sub_check block_data
        else typing_store ()

  let addresses_in_block ctxa a ctxb b =
    let lst1 =
      match a.sized, b.sized with
      | Some (sza,va), Some (szb,vb) when sza = szb -> Value.addresses_in_binary ~size:sza ctxa va ctxb vb
      | _ -> []
    in
    let lst2 =
      match a.unsized, b.unsized with
      | Some arra, Some arrb -> Sub.addresses_in_block ctxa arra ctxb arrb
      | _ -> []
    in
    lst1 @ lst2

end

module MakeComplete
    (Sub : Memory_sig.WHOLE_MEMORY_DOMAIN)
    (Block : Memory_sig.BLOCK
     with module Scalar = Sub.Scalar
      and module Value = Sub.Address)
: Memory_sig.COMPLETE_DOMAIN
  with module Scalar = Sub.Scalar
   and  module Value.Context = Sub.Address.Scalar.Context and module Value.Scalar = Sub.Address.Scalar
= struct
  module Scalar = Sub.Scalar
  module Address = Sub.Address
  module Value = Sub.Address
  module Block : Memory_sig.BLOCK
    with module Scalar = Sub.Scalar
     and module Value = Value
  = (* Block *) Make (Value)(Block)

  module Memory : Memory_sig.MEMORY
    with module Scalar = Scalar
     and module Address := Value
     and module Block := Block 
  = Sub.Make_Memory (Block)
end
