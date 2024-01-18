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


(* The heart of the domain is a mapping from constraint variables to
   their basis. If a variable is absent from the mapping, it means
   that the variable is mapped to the "top" basis. Thus, the empty map
   denotes top, union of domains is intersection of mappings, and
   intersection of domains is union of the mappings. We do not need to
   represent the bottom domain because it is handled by
   constraint_domain. *)
module type Map_S = sig
  type t
  type key
  type value
  val top: t

  (* We use add when you don't know if the new value is going to be
     smaller than the previous one. This is necessary in forward
     evaluation because the constraints can already exist. *)
  val add: key -> value -> t -> t

  (* Use replace when you know that the new value is smaller than the
     previous one. Used when propagating assume conditions. *)
  val replace: key -> value -> t -> t
  val inter: t -> t -> t
  val join: t -> t -> t
  val pretty: Format.formatter -> t -> unit
  val find: key -> t -> value
end


module type Key = sig
  include Map.OrderedType
  val pretty: Format.formatter -> t -> unit
  (* Note: For Okasaki maps, to_int must be injective. *)
  val to_int: t -> int
  val equal: t -> t -> bool
end

module type Value = Lattices.Sig.Join_Semi_Lattice_With_Inter_Bottom

module MakeMap1(Key:Key)(Value:Value)(*:Map_S with type key = Key.t
                                        and type value = Value.t *) =
struct

  module M1 = struct
    include Extstdlib.Map.Make(Key)

    let [@inline always] union f a b = union (fun key a b -> Some (f a b)) a b
    let [@inline always] inter f a b = merge (fun key a b -> match a,b with
        | None, _ | _, None -> None
        | Some a, Some b -> Some (f a b)) a b
  end
  (* A version with Hashamt. *)
  (* module M2 = struct
   *   include Hashamt.Make(Key)
   *   let singleton x value = add x value @@ empty
   *   let [@inline always] union f a b = union (fun key a b -> Some (f a b)) a b
   *   let [@inline always] inter f a b = join (fun key a b -> Some (f a b)) a b          
   * end *)
  (* Okasaki maps are the fastest version. *)
  module M3 = struct
    include Okasakimap.Make(* Pretty *)(struct
        include Key
        let pp = pretty
      end)
    let mk_pretty fkey fvalue fmt x =
      iter (fun key value ->
          Format.fprintf fmt "%x -> %a@\n" key fvalue value
        ) x
    ;;

  end

  module M = M3;;

  type t = Value.t M.t
  type key = Key.t
  type value = Value.t

  let pretty fmt x = M.mk_pretty Key.pretty Value.pretty fmt x
  (* let pretty fmt x = M.pp Value.pretty fmt x *)

  let top = M.empty
  let inter a b = M.union Value.inter a b

  (* Since absence of information is top, we use inter here. *)
  let join a b = M.inter Value.join a b

  let find key store = M.find key store

  let add key value map =
    match find key map with
    | exception Not_found -> M.add key value map
    | old ->
      M.add key (Value.inter old value) map
  ;;

  let replace key value map =
    match find key map with
    | exception Not_found -> M.add key value map
    | old ->
      assert (Value.includes old value); M.add key value map
  ;;

  (* Sligthtly more optimized versions when we use M3. *)
  let add key value map =
    M3.insert (fun ~old ~value -> Value.inter old value) key value map

  let replace key value map =
    M3.insert (fun ~old ~value -> assert (Value.includes old value); value) key value map

end

(* A variation of MakeMap where we do not store constant contraints in
   the map. We allow constraint rewriting to create new constant
   constraints, so this is necessary. *)
module MakeMap2
    (Map:Map_S)    
    (Key:sig
       include Key with type t = Map.key
       val get_level: t -> int   (* -1 < level < 7 *)
     end)
    (GetCst:sig val get_cst: Key.t -> Map.value end):Map_S
  with type key = Map.key
   and type value = Map.value
   and type t = Map.t
= struct

  let replace key value map =
    if Key.get_level key == -1
    then map
    else Map.replace key value map
  ;;

  let add key value map =
    if Key.get_level key == -1
    then map
    else Map.add key value map
  ;;

  let find key map =
    if Key.get_level key == -1
    then GetCst.get_cst key
    else Map.find key map
  ;;

  let inter = Map.inter
  let join = Map.join
  let pretty = Map.pretty
  let top = Map.top
  type t = Map.t
  type key = Map.key
  type value = Map.value

end

module TC = Transfer_functions.Term;;

module Const_eval = Constraints.Const_eval

module Make
    (Constraints:Constraints.Constraints_sig.Constraints)
    (B:Single_value_abstraction.Sig.Binary_Integer_Basis)
= struct

  module Const_eval = Const_eval.Make(Constraints);;
  module IntegerGetCst = struct
    let get_cst x =
      match Const_eval.integer x with
      | exception Const_eval.Empty -> B.Integer_Lattice.bottom
      | exception Const_eval.Not_a_constant -> assert false
      | x -> B.Integer_Lattice.singleton x
  end
  module BinaryGetCst = struct
    let get_cst x =
      let Constraints.Binary{size} = x in      
      match Const_eval.binary x with
      | exception Const_eval.Empty -> B.Binary_Lattice.bottom ~size
      | exception Const_eval.Not_a_constant -> assert false
      | k -> B.Binary_Lattice.singleton ~size k
  end  
  module BooleanGetCst = struct
    let get_cst x =
      match Const_eval.boolean x with
      | exception Const_eval.Empty -> B.Boolean_Lattice.bottom
      | exception Const_eval.Not_a_constant -> B.Boolean_Lattice.bottom
      | x -> B.Boolean_Lattice.singleton x
  end

  module BinaryConstraint = struct
    type kind = TC.binary
    type t = TC.binary Constraints.t
    let compare = Constraints.compare
    let equal = Constraints.equal
    let hash = Constraints.hash
    let pretty = Constraints.pretty
    let get_level = Constraints.level

    (* We store the size in the least significant bits. *)
    let size_reserved_bits = 20;;
    let mask_size = (1 lsl size_reserved_bits) - 1;;
    let to_int (Constraints.Binary{size;id}) =
      assert(size lsr size_reserved_bits == 0); (* Size should fit in 16 bits. *)
      let id = Constraints.Id.to_int id in
      let shift_id = (id lsl size_reserved_bits) in
      assert(shift_id lsr size_reserved_bits == id); (* We should have enough room for the id *)
      shift_id lor size
    (* let to_int = Constraints.hash *)
  end

  module IntegerConstraint = struct
    type kind = TC.integer
    type t = TC.integer Constraints.t
    let compare = Constraints.compare
    let equal = Constraints.equal
    let hash = Constraints.hash
    let pretty = Constraints.pretty
    let get_level = Constraints.level
    let to_int = Constraints.hash
  end

  module BooleanConstraint = struct
    type kind = TC.boolean    
    type t = TC.boolean Constraints.t
    let compare = Constraints.compare
    let equal = Constraints.equal
    let hash = Constraints.hash
    let pretty = Constraints.pretty
    let get_level = Constraints.level
    let to_int = Constraints.hash
  end


  module BinaryMap = struct

    (* We do not actually use these arguments. *)
    module FL = (struct
      include B.Binary_Lattice
      let includes _ = assert false
      let is_bottom _ = assert false
      let bottom = bottom ~size:1
        
      let inter _ = assert false
      let join _  = assert false
      let pretty _ _ = (* assert false *)()
      let includes_or_widen ~previous _x = assert false
      let widen ~previous _x = assert false
    end)

    module Map1 = struct
      include MakeMap1(BinaryConstraint)(FL)

      let mask_size = BinaryConstraint.mask_size
                        
      let join a b = M3.interi (fun key a b ->
          let size = (key land mask_size) in
          (* Codex_log.feedback "join key %x mask_size %x size %d" key mask_size size; *)
          B.Binary_Lattice.join ~size a b) a b

      let inter a b = M3.unioni (fun key a b ->
          let size = (key land mask_size) in
          B.Binary_Lattice.inter ~size a b) a b

      let add key value map =
        let Constraints.Binary{size} = key in
        M3.insert (fun ~old ~value -> B.Binary_Lattice.inter ~size old value) key value map

      let pretty fmt x =
        M3.iter (fun key value ->
            let size = key land BinaryConstraint.mask_size in
            Format.fprintf fmt "%d -> %a@\n" (key lsr BinaryConstraint.size_reserved_bits) (B.Binary_Lattice.pretty ~size) value
          ) x

      let replace key value map =
        (* let size = key land mask_size in *)
        M3.insert (fun ~old ~value -> (* assert (B.Binary_Lattice.includes ~size old value); *) value) key value map
      

    end
    
    module M = MakeMap2(Map1)(BinaryConstraint)(BinaryGetCst);;

    type t = M.t
    let top = M.top
    let inter = M.inter
    let join  = M.join
    let pretty ~size = M.pretty
    let find = M.find
    let add = M.add
    let replace = M.replace
    let pretty = Map1.pretty

    ;;


  end
  module IntegerMap = MakeMap2(MakeMap1(IntegerConstraint)(B.Integer_Lattice))(IntegerConstraint)(IntegerGetCst);;
  module BooleanMap = MakeMap2(MakeMap1(BooleanConstraint)(B.Boolean_Lattice))(BooleanConstraint)(BooleanGetCst);;  

  let name = "Base_Domain(" ^ B.name ^ ")"

  module Constraints = Constraints;;
  module B = B;;

  type binary = TC.binary Constraints.t
  type integer = TC.integer Constraints.t
  type boolean = TC.boolean Constraints.t


  type t = {
    boolean_map: BooleanMap.t;
    integer_map: IntegerMap.t;
    binary_map: BinaryMap.t;
  }

  let pretty fmt t =
    Format.fprintf fmt "@[<v>%a@\n%a@\n%a@]"
      BooleanMap.pretty t.boolean_map
      IntegerMap.pretty t.integer_map
      BinaryMap.pretty t.binary_map      
  ;;

  let equal = (==);;

  let top = {
    boolean_map = BooleanMap.top;
    integer_map = IntegerMap.top;
    binary_map = BinaryMap.top
  };;

  let inter a b = {
    boolean_map = BooleanMap.inter a.boolean_map b.boolean_map;
    integer_map = IntegerMap.inter a.integer_map b.integer_map;
    binary_map = BinaryMap.inter a.binary_map b.binary_map;
  }

  let join a b = {
    boolean_map = BooleanMap.join a.boolean_map b.boolean_map;
    integer_map = IntegerMap.join a.integer_map b.integer_map;
    binary_map = BinaryMap.join a.binary_map b.binary_map;
  }

  let query_boolean x map = BooleanMap.find x map.boolean_map
  let query_integer x map = IntegerMap.find x map.integer_map
  let query_binary x map = BinaryMap.find x map.binary_map


  module Query = struct
    include B
    let boolean t x = query_boolean x t
    let integer t x = query_integer x t
    let binary ~size t x = query_binary x t
  end

  let add_boolean x value map = {map with boolean_map = BooleanMap.add x value map.boolean_map}
  let add_integer x value map = {map with integer_map = IntegerMap.add x value map.integer_map}
  let add_binary x value map = {map with binary_map = BinaryMap.add x value map.binary_map}


  (* Transfer functions. *)
  let copy_boolean ~src ~dst dom =
    let boolean_map = dom.boolean_map in
    let value = BooleanMap.find src boolean_map in
    let boolean_map = BooleanMap.add dst value boolean_map in
    { dom with boolean_map }, value
  ;;

  let copy_integer ~src ~dst dom =
    let integer_map = dom.integer_map in
    let value = IntegerMap.find src integer_map in
    let integer_map = IntegerMap.add dst value integer_map in
    { dom with integer_map }, value
  ;;

  let copy_binary ~src ~dst dom =
    let binary_map = dom.binary_map in
    let value = BinaryMap.find src binary_map in
    let binary_map = BinaryMap.add dst value binary_map in
    { dom with binary_map }, value
  ;;
  
  let boolean_pretty dom fmt x =
    let res = BooleanMap.find x dom.boolean_map in
    B.Boolean_Lattice.pretty fmt res

  let integer_pretty dom fmt x =
    let res = IntegerMap.find x dom.integer_map in
    B.Integer_Lattice.pretty fmt res

  let binary_pretty ~size dom fmt x =
    let res = BinaryMap.find x dom.binary_map in
    (* Format.fprintf fmt "%a -> %a" Constraints.pretty x *)
    (B.Binary_Lattice.pretty ~size) fmt res

  let nondet ~doma ~tupa ~domb ~tupb ~tupres =

    let copy_constraints tup dom =
      Immutable_array.fold_left2 (fun dom (Constraints.Any nondet) (Constraints.Any orig) ->
          match nondet,orig with
          | Constraints.((Binary _ as nondet)), Constraints.((Binary _ as orig)) ->
            fst @@ copy_binary ~src:orig ~dst:nondet dom          
          | Constraints.((Integer _ as nondet)), Constraints.((Integer _ as orig)) ->
            fst @@ copy_integer ~src:orig ~dst:nondet dom
          | Constraints.((Bool _ as nondet)), Constraints.((Bool _ as orig)) ->
            fst @@ copy_boolean ~src:orig ~dst:nondet dom            
          | _ -> assert false
        ) dom tupres tup
    in

    let doma = copy_constraints tupa doma in
    let domb = copy_constraints tupb domb in    
    let dom = join doma domb in
    dom

  let fixpoint_open() = ()

  module Sum = struct
    type ('a,'b,'c) t =
      | Boolean of 'a
      | Integer of 'b
      | Binary of 'c
  end;;


  let fixpoint_step ~lvl _actual_dom  ~actuals arg_dom ~args final_dom ~finals =
    let n = Immutable_array.length actuals in
    assert (n == Immutable_array.length args);
    assert (n == Immutable_array.length finals);

    let fixpoint_reached = ref true in
    let widened_values = Immutable_array.init n (fun i ->
        let Constraints.Any actual = Immutable_array.get actuals i in
        let Constraints.Any arg = Immutable_array.get args i in
        let Constraints.Any final = Immutable_array.get finals i in
        (* Codex_log.feedback "index %d: @[<v> actual :%a@ arg: %a@ final: %a@]" i Constraints.pretty actual Constraints.pretty arg Constraints.pretty final; *)
        match actual,arg,final with
        | (Constraints.Binary {size=sizeactual} as actual), (Constraints.Binary {size=sizearg} as arg), (Constraints.Binary {size=sizefinal} as final) ->
          assert(sizeactual == sizearg && sizearg == sizefinal);
          let size = sizeactual in
          let actualv = query_binary actual arg_dom in
          let argv = query_binary arg arg_dom in
          let finalv = query_binary final final_dom in
          let joined = B.Binary_Lattice.join ~size actualv finalv in
          let (bool,widened) = B.Binary_Lattice.includes_or_widen ~size ~previous:argv joined in
          fixpoint_reached := !fixpoint_reached && bool;
          Sum.Binary widened        
        | (Constraints.Integer _ as actual), (Constraints.Integer _ as arg), (Constraints.Integer _ as final) ->
          let actualv = query_integer actual arg_dom in
          let argv = query_integer arg arg_dom in
          let finalv = query_integer final final_dom in
          let joined = B.Integer_Lattice.join actualv finalv in
          let (bool,widened) = B.Integer_Lattice.includes_or_widen ~previous:argv joined in
          fixpoint_reached := !fixpoint_reached && bool;
          Sum.Integer widened
        | (Constraints.Bool _ as actual), (Constraints.Bool _ as arg), (Constraints.Bool _ as final) ->
          let actualv = query_boolean actual arg_dom in
          let argv = query_boolean arg arg_dom in
          let finalv = query_boolean final final_dom in
          let joined = B.Boolean_Lattice.join actualv finalv in
          let (bool,widened) = B.Boolean_Lattice.includes_or_widen ~previous:argv joined in
          fixpoint_reached := !fixpoint_reached && bool;
          Sum.Boolean widened
        | _ -> failwith  "Typing issue"
      ) in
    !fixpoint_reached, (fun ~close res ->
        let res = Immutable_array.fold_left2 (fun dom (Constraints.Any constrain) value ->
            match constrain,value with
            | Constraints.Binary _ as constrain, Sum.Binary value ->
              add_binary constrain value dom            
            | Constraints.Integer _ as constrain, Sum.Integer value ->
              add_integer constrain value dom
            | Constraints.Bool _ as constrain, Sum.Boolean value ->
              add_boolean constrain value dom
            | _ -> failwith "Typing issue"
          ) arg_dom res widened_values in
        (* Codex_log.feedback "fpstep bool %b @\nres %a @\narg_dom%a@\nfinal_dom%a" close pretty res pretty arg_dom pretty final_dom; *)
        res
      )
  ;;


  (**************** Forward propagation. ****************)

  module Domain_Arity = struct
    type 'r ar0 = t -> 'r -> t
    type ('a,'r) ar1 = t -> 'a -> 'r -> t
    type ('a,'b,'r) ar2 = t -> 'a -> 'b -> 'r -> t
    type ('a,'b,'c,'r) ar3 = t -> 'a -> 'b -> 'c -> 'r -> t
    type ('a,'r) variadic = t -> 'a list -> t
  end

  (* Defines transfer functions of the form dom[res <- f(a,b)]. *)

  let ar2_boolean_boolean_boolean str f dom a b res =
    let va = BooleanMap.find a dom.boolean_map in
    let vb = BooleanMap.find b dom.boolean_map in    
    let boolean_map = BooleanMap.add res (f va vb) dom.boolean_map in
    { dom with boolean_map }
  ;;

  let ar1_boolean_boolean str f dom a res =
    let va = BooleanMap.find a dom.boolean_map in
    let boolean_map = BooleanMap.add res (f va) dom.boolean_map in
    { dom with boolean_map }
  ;;

  let ar0_boolean f dom res =
    let boolean_map = BooleanMap.add res f dom.boolean_map in
    { dom with boolean_map }
  ;;

  let ar2_integer_integer_boolean f dom a b res = 
    let va = IntegerMap.find a dom.integer_map in
    let vb = IntegerMap.find b dom.integer_map in    
    let boolean_map = BooleanMap.add res (f va vb) dom.boolean_map in
    { dom with boolean_map }
  ;;

  let ar2_integer_integer_integer f dom a b res = 
    let va = IntegerMap.find a dom.integer_map in
    let vb = IntegerMap.find b dom.integer_map in    
    let integer_map = IntegerMap.add res (f va vb) dom.integer_map in
    { dom with integer_map }
  ;;

  let ar1_integer_integer f dom a res = 
    let va = IntegerMap.find a dom.integer_map in
    let integer_map = IntegerMap.add res (f va) dom.integer_map in
    { dom with integer_map }
  ;;

  let ar0_integer f dom res = 
    let integer_map = IntegerMap.add res f dom.integer_map in
    { dom with integer_map }
  ;;

  let ar0_binary str f dom res =
    let binary_map = BinaryMap.add res f dom.binary_map in
    { dom with binary_map }
  ;;

  let ar2_binary_binary_binary str f dom a b res =
    let va = BinaryMap.find a dom.binary_map in
    let vb = BinaryMap.find b dom.binary_map in
    let binary_map = BinaryMap.add res (f va vb) dom.binary_map in
    { dom with binary_map }
  ;;

  let ar2_binary_binary_boolean str f dom a b res =
    let va = BinaryMap.find a dom.binary_map in
    let vb = BinaryMap.find b dom.binary_map in
    let boolean_map = BooleanMap.add res (f va vb) dom.boolean_map in
    { dom with boolean_map }
  ;;

  let ar1_binary_binary str f dom a res =
    let va = BinaryMap.find a dom.binary_map in
    let binary_map = BinaryMap.add res (f va) dom.binary_map in
    { dom with binary_map }
  ;;

  let ar1_boolean_binary f dom a res =
    let va = BooleanMap.find a dom.boolean_map in
    let binary_map = BinaryMap.add res (f va) dom.binary_map in
    { dom with binary_map }
  ;;

  
  module Boolean_Forward = struct
    let (||) = ar2_boolean_boolean_boolean "||" B.Boolean_Forward.(||)
    let (&&) = ar2_boolean_boolean_boolean "&&" B.Boolean_Forward.(&&)
    let (not) = ar1_boolean_boolean "not" B.Boolean_Forward.not
    let true_ = ar0_boolean B.Boolean_Forward.true_
    let false_  = ar0_boolean B.Boolean_Forward.false_
  end

  module Integer_Forward = struct
    let ile = ar2_integer_integer_boolean B.Integer_Forward.ile      
    let ieq = ar2_integer_integer_boolean B.Integer_Forward.ieq
    let iconst k = ar0_integer (B.Integer_Forward.iconst k)
    let one  = iconst Z.one
    let zero = iconst Z.zero

    let ixor = ar2_integer_integer_integer B.Integer_Forward.ixor
    let ior  = ar2_integer_integer_integer B.Integer_Forward.ior 
    let iand = ar2_integer_integer_integer B.Integer_Forward.iand
    let ishr = ar2_integer_integer_integer B.Integer_Forward.ishr
    let ishl = ar2_integer_integer_integer B.Integer_Forward.ishl
    let imod = ar2_integer_integer_integer B.Integer_Forward.imod
    let idiv = ar2_integer_integer_integer B.Integer_Forward.idiv
    let imul = ar2_integer_integer_integer B.Integer_Forward.imul
    let iadd = ar2_integer_integer_integer B.Integer_Forward.iadd
    let isub = ar2_integer_integer_integer B.Integer_Forward.isub
    let itimes k = ar1_integer_integer (B.Integer_Forward.itimes k)
  end

  module Binary_Forward = struct
    let biconst ~size k = ar0_binary (Z.to_string k) (B.Binary_Forward.biconst ~size k)
    let beq    ~size = ar2_binary_binary_boolean "beq" @@ B.Binary_Forward.beq ~size
    let biule  ~size = ar2_binary_binary_boolean "biule" @@ B.Binary_Forward.biule ~size
    let bisle  ~size = ar2_binary_binary_boolean "bisle"  @@ B.Binary_Forward.bisle ~size
    let biadd  ~size ~nsw ~nuw ~nusw = ar2_binary_binary_binary "biadd" @@ B.Binary_Forward.biadd ~size ~nsw ~nuw ~nusw
    let bisub  ~size ~nsw ~nuw ~nusw = ar2_binary_binary_binary "bisub" @@ B.Binary_Forward.bisub ~size ~nsw ~nuw ~nusw
    let bimul  ~size ~nsw ~nuw = ar2_binary_binary_binary "bimul" @@ B.Binary_Forward.bimul ~size ~nsw ~nuw
    let bshl   ~size ~nsw ~nuw = ar2_binary_binary_binary "bshl" @@ B.Binary_Forward.bshl ~size ~nsw ~nuw
    let bxor   ~size = ar2_binary_binary_binary "bxor" @@ B.Binary_Forward.bxor ~size
    let bor    ~size = ar2_binary_binary_binary "bor" @@ B.Binary_Forward.bor ~size
    let band   ~size = ar2_binary_binary_binary "band" @@ B.Binary_Forward.band ~size
    let bashr  ~size = ar2_binary_binary_binary "bashr" @@ B.Binary_Forward.bashr ~size
    let blshr  ~size = ar2_binary_binary_binary "blshr" @@ B.Binary_Forward.blshr ~size
    let bisdiv ~size = ar2_binary_binary_binary "bisdiv" @@ B.Binary_Forward.bisdiv ~size
    let biudiv ~size = ar2_binary_binary_binary "biudiv" @@ B.Binary_Forward.biudiv ~size
    let bismod ~size = ar2_binary_binary_binary "bismod" @@ B.Binary_Forward.bismod ~size
    let biumod ~size = ar2_binary_binary_binary "biumod" @@ B.Binary_Forward.biumod ~size

    let bsext ~size ~oldsize = ar1_binary_binary "bsext" @@ B.Binary_Forward.bsext ~size ~oldsize
    let buext ~size ~oldsize = ar1_binary_binary "buext" @@ B.Binary_Forward.buext ~size ~oldsize
    let bchoose ~size cond = ar1_binary_binary "bchoose" @@ B.Binary_Forward.bchoose ~size cond
    let bofbool ~size = ar1_boolean_binary @@ B.Binary_Forward.bofbool ~size
    let bconcat ~size1 ~size2 = ar2_binary_binary_binary "bconcat" @@ B.Binary_Forward.bconcat ~size1 ~size2
    let bextract ~size ~index ~oldsize = ar1_binary_binary "bextract" @@ B.Binary_Forward.bextract ~size ~index ~oldsize
    let valid ~size _ = assert false
    let valid_ptr_arith ~size _ = assert false
    let baddr ~size _  = assert false
    (* let biconst ~size k = ar0_binary (Z.to_string k) (B.Binary_Forward.biconst ~size k) *)
    let buninit ~size _ = assert false
    let bshift ~size ~offset ~max _ = assert false
    let bindex ~size _ = assert false
  end

  let binary_empty ~size = ar0_binary "empty" @@ B.Binary_Lattice.bottom ~size
  let integer_empty = ar0_integer B.Integer_Lattice.bottom
  let boolean_empty = ar0_boolean B.Boolean_Lattice.bottom        

  let binary_unknown ~size = ar0_binary "unknown" (B.Binary_Lattice.top ~size)
  let integer_unknown = ar0_integer B.Integer_Lattice.top
  let boolean_unknown = ar0_boolean B.Boolean_Lattice.top

  

  (**************** Backward propagation. ****************)
  module Backward_Interp = struct

    let ar2_boolean_boolean_boolean dom a b res f =
      let va = BooleanMap.find a dom.boolean_map in
      let vb = BooleanMap.find b dom.boolean_map in
      let vres = BooleanMap.find res dom.boolean_map in
      let va',vb' = f va vb vres in
      let acc,boolean_map = match va' with
        | None -> [], dom.boolean_map
        | Some v -> [Constraints.Any a], BooleanMap.replace a v dom.boolean_map
      in
      let acc,boolean_map = match vb' with
        | None -> acc, boolean_map
        | Some v -> (Constraints.Any b)::acc, BooleanMap.replace b v boolean_map
      in
      acc,{dom with boolean_map}
    ;;


    let ar2_integer_integer_boolean dom a b res f =
      let va = IntegerMap.find a dom.integer_map in
      let vb = IntegerMap.find b dom.integer_map in
      let vres = BooleanMap.find res dom.boolean_map in
      let va',vb' = f va vb vres in
      let acc,integer_map = match va' with
        | None -> [], dom.integer_map
        | Some v ->
          [Constraints.Any a], IntegerMap.replace a v dom.integer_map
      in
      let acc,integer_map = match vb' with
        | None -> acc, integer_map
        | Some v ->
          (Constraints.Any b)::acc, IntegerMap.replace b v integer_map
      in
      acc,{dom with integer_map}
    ;;

    let ar2_integer_integer_integer dom a b res f =
      let va = IntegerMap.find a dom.integer_map in
      let vb = IntegerMap.find b dom.integer_map in
      let vres = IntegerMap.find res dom.integer_map in
      let va',vb' = f va vb vres in
      let acc,integer_map = match va' with
        | None -> [], dom.integer_map
        | Some v -> [Constraints.Any a], IntegerMap.replace a v dom.integer_map
      in
      let acc,integer_map = match vb' with
        | None -> acc, integer_map
        | Some v -> (Constraints.Any b)::acc, IntegerMap.replace b v integer_map
      in
      acc,{dom with integer_map}
    ;;

    let ar1_boolean_boolean dom a res f =
      let va = BooleanMap.find a dom.boolean_map in
      let vres = BooleanMap.find res dom.boolean_map in
      let va' = f va vres in
      let acc,boolean_map = match va' with
        | None -> [], dom.boolean_map
        | Some v -> [Constraints.Any a], BooleanMap.replace a v dom.boolean_map
      in
      acc,{dom with boolean_map}
    ;;

    let ar1_integer_integer dom a res f =
      let va = IntegerMap.find a dom.integer_map in
      let vres = IntegerMap.find res dom.integer_map in
      let va' = f va vres in
      let acc,integer_map = match va' with
        | None -> [], dom.integer_map
        | Some v -> [Constraints.Any a], IntegerMap.replace a v dom.integer_map
      in
      acc,{dom with integer_map}
    ;;


    let ar2_binary_binary_boolean dom a b res f =
      let va = BinaryMap.find a dom.binary_map in
      let vb = BinaryMap.find b dom.binary_map in
      let vres = BooleanMap.find res dom.boolean_map in
      let va',vb' = f va vb vres in
      let acc,binary_map = match va' with
        | None -> [], dom.binary_map
        | Some v ->
          [Constraints.Any a], BinaryMap.replace a v dom.binary_map
      in
      let acc,binary_map = match vb' with
        | None -> acc, binary_map
        | Some v ->
          (Constraints.Any b)::acc, BinaryMap.replace b v binary_map
      in
      acc,{dom with binary_map}
    ;;

    let ar2_binary_binary_binary dom a b res f =
      let va = BinaryMap.find a dom.binary_map in
      let vb = BinaryMap.find b dom.binary_map in
      let vres = BinaryMap.find res dom.binary_map in
      let va',vb' = f va vb vres in
      let acc,binary_map = match va' with
        | None -> [], dom.binary_map
        | Some v -> [Constraints.Any a], BinaryMap.replace a v dom.binary_map
      in
      let acc,binary_map = match vb' with
        | None -> acc, binary_map
        | Some v -> (Constraints.Any b)::acc, BinaryMap.replace b v binary_map
      in
      acc,{dom with binary_map}
    ;;

    
    let ar1_binary_binary dom a res f =
      let va = BinaryMap.find a dom.binary_map in
      let vres = BinaryMap.find res dom.binary_map in
      let va' = f va vres in
      let acc,binary_map = match va' with
        | None -> [], dom.binary_map
        | Some v -> [Constraints.Any a], BinaryMap.replace a v dom.binary_map
      in
      acc,{dom with binary_map}
    ;;

    
    (* Refinement of one term. *)
    let backward_interp dom (Constraints.Any x)=
      let open Constraints in
      let open TC in
      match x with
      | Bool{term=T2{tag=Or;a;b}} -> ar2_boolean_boolean_boolean dom a b x B.Boolean_Backward.(||)
      | Bool{term=T2{tag=And;a;b}} -> ar2_boolean_boolean_boolean dom a b x B.Boolean_Backward.(&&)
      | Bool{term=T2{tag=Ieq;a;b}} -> ar2_integer_integer_boolean dom a b x B.Integer_Backward.ieq
      | Bool{term=T2{tag=Ile;a;b}} -> ar2_integer_integer_boolean dom a b x B.Integer_Backward.ile
      | Bool{term=T1{tag=Not;a}} -> ar1_boolean_boolean dom a x B.Boolean_Backward.not
      | Bool{term=T0 _} -> [], dom

      | Integer{term=T2{tag=Iadd;a;b}} -> ar2_integer_integer_integer dom a b x B.Integer_Backward.iadd
      | Integer{term=T2{tag=Isub;a;b}} -> ar2_integer_integer_integer dom a b x B.Integer_Backward.isub
      | Integer{term=T2{tag=Imul;a;b}} -> ar2_integer_integer_integer dom a b x B.Integer_Backward.imul
      | Integer{term=T2{tag=Idiv;a;b}} -> ar2_integer_integer_integer dom a b x B.Integer_Backward.idiv
      | Integer{term=T2{tag=Imod;a;b}} -> ar2_integer_integer_integer dom a b x B.Integer_Backward.imod
      | Integer{term=T2{tag=Ishl;a;b}} -> ar2_integer_integer_integer dom a b x B.Integer_Backward.ishl
      | Integer{term=T2{tag=Ishr;a;b}} -> ar2_integer_integer_integer dom a b x B.Integer_Backward.ishr
      | Integer{term=T2{tag=Iand;a;b}} -> ar2_integer_integer_integer dom a b x B.Integer_Backward.iand
      | Integer{term=T2{tag=Ior;a;b}}  -> ar2_integer_integer_integer dom a b x B.Integer_Backward.ior 
      | Integer{term=T2{tag=Ixor;a;b}} -> ar2_integer_integer_integer dom a b x B.Integer_Backward.ixor
      | Integer{term=T1{tag=Itimes k;a}} -> ar1_integer_integer dom a x (B.Integer_Backward.itimes k)                                          
      | Integer{term=T0 _} -> [], dom


      | Bool{term=T2{tag=Beq size;a;b}} ->   ar2_binary_binary_boolean dom a b x @@ B.Binary_Backward.beq   ~size
      | Bool{term=T2{tag=Bisle size;a;b}} -> ar2_binary_binary_boolean dom a b x @@ B.Binary_Backward.bisle ~size
      | Bool{term=T2{tag=Biule size;a;b}} -> ar2_binary_binary_boolean dom a b x @@ B.Binary_Backward.biule ~size
      | Binary{term=T2{tag=Biadd{size;nsw;nuw;nusw};a;b}} ->  ar2_binary_binary_binary dom a b x @@ B.Binary_Backward.biadd  ~size ~nsw ~nuw ~nusw
      | Binary{term=T2{tag=Bisub{size;nsw;nuw;nusw};a;b}} ->  ar2_binary_binary_binary dom a b x @@ B.Binary_Backward.bisub  ~size ~nsw ~nuw ~nusw
      | Binary{term=T2{tag=Bimul{size;nsw;nuw};a;b}} ->  ar2_binary_binary_binary dom a b x @@ B.Binary_Backward.bimul  ~size ~nsw ~nuw
      | Binary{term=T2{tag=Bshl{size;nsw;nuw};a;b}} ->   ar2_binary_binary_binary dom a b x @@ B.Binary_Backward.bshl   ~size ~nsw ~nuw
      | Binary{term=T2{tag=Bisdiv(size);a;b}} ->         ar2_binary_binary_binary dom a b x @@ B.Binary_Backward.bisdiv ~size
      | Binary{term=T2{tag=Bismod(size);a;b}} ->         ar2_binary_binary_binary dom a b x @@ B.Binary_Backward.bismod ~size
      | Binary{term=T2{tag=Biudiv(size);a;b}} ->         ar2_binary_binary_binary dom a b x @@ B.Binary_Backward.biudiv ~size
      | Binary{term=T2{tag=Biumod(size);a;b}} ->         ar2_binary_binary_binary dom a b x @@ B.Binary_Backward.biumod ~size
      | Binary{term=T2{tag=Bashr(size);a;b}} ->          ar2_binary_binary_binary dom a b x @@ B.Binary_Backward.bashr  ~size
      | Binary{term=T2{tag=Blshr(size);a;b}} ->          ar2_binary_binary_binary dom a b x @@ B.Binary_Backward.blshr  ~size
      | Binary{term=T2{tag=Band(size);a;b}} ->           ar2_binary_binary_binary dom a b x @@ B.Binary_Backward.band   ~size
      | Binary{term=T2{tag=Bor (size);a;b}} ->           ar2_binary_binary_binary dom a b x @@ B.Binary_Backward.bor    ~size
      | Binary{term=T2{tag=Bxor(size);a;b}} ->           ar2_binary_binary_binary dom a b x @@ B.Binary_Backward.bxor   ~size
      | Binary{term=T2{tag=Bconcat(size1,size2);a;b}} -> ar2_binary_binary_binary dom a b x @@ B.Binary_Backward.bconcat ~size1 ~size2
      | Binary{term=T1{tag=Bsext(size);a=Binary{size=oldsize} as a}} -> ar1_binary_binary dom a x @@ B.Binary_Backward.bsext ~size ~oldsize
      | Binary{term=T1{tag=Buext(size);a=Binary{size=oldsize} as a}} -> ar1_binary_binary dom a x @@ B.Binary_Backward.buext ~size ~oldsize
      | Binary{term=T1{tag=Bextract{size;index;oldsize};a}} ->          ar1_binary_binary dom a x @@ B.Binary_Backward.bextract ~size ~index ~oldsize
      | Binary{term=T0 _} -> [], dom


    
      | Bool{term=(Mu_formal _|Unknown _) } -> [], dom                                       
      | Integer{term=(Mu_formal _|Unknown _) } -> [], dom
      | Binary{term=(Mu_formal _|Unknown _) } -> [], dom                                                  

      (* We can, but do not, propagate across mu. *)
      | Binary{term=Tuple_get(_,Mu _)} -> [], dom
      | Integer{term=Tuple_get(_,Mu _)} -> [], dom
      | Bool{term=Tuple_get(_,Mu _)} -> [], dom

      (* We could also propagate across nondet. *)
      | Bool{term=Tuple_get(_,Nondet _)} -> [], dom
      | Binary{term=Tuple_get(_,Nondet _)} -> [], dom
      | Integer{term=Tuple_get(_,Nondet _)} -> [], dom


      | t -> Codex_log.fatal "backward_interp on %a" Constraints.pretty t

  end

  let backward_interp = Backward_Interp.backward_interp


  (**************** Forward propagation ****************)

  let forward_interp dom (Constraints.Any x)=
    let open Constraints in
    let open TC in
    match x with
    | Bool{term=T0 _} -> dom
    | Binary{term=T0 _} -> dom
    | Integer{term=T0 _} -> dom

    | Bool{term=T2{tag=Or;a;b}} ->  Boolean_Forward.(||) dom a b x
    | Bool{term=T2{tag=And;a;b}} -> Boolean_Forward.(&&) dom a b x
    | Bool{term=T1{tag=Not;a}} ->   Boolean_Forward.not dom a x


    | Bool{term=T2{tag=Ieq;a;b}} -> Integer_Forward.ieq dom a b x
    | Bool{term=T2{tag=Ile;a;b}} -> Integer_Forward.ile dom a b x
    | Integer{term=T2{tag=Iadd;a;b}} -> Integer_Forward.iadd dom a b x
    | Integer{term=T2{tag=Isub;a;b}} -> Integer_Forward.isub dom a b x
    | Integer{term=T2{tag=Imul;a;b}} -> Integer_Forward.imul dom a b x
    | Integer{term=T2{tag=Idiv;a;b}} -> Integer_Forward.idiv dom a b x
    | Integer{term=T2{tag=Imod;a;b}} -> Integer_Forward.imod dom a b x
    | Integer{term=T2{tag=Ishl;a;b}} -> Integer_Forward.ishl dom a b x
    | Integer{term=T2{tag=Ishr;a;b}} -> Integer_Forward.ishr dom a b x
    | Integer{term=T2{tag=Iand;a;b}} -> Integer_Forward.iand dom a b x
    | Integer{term=T2{tag=Ior;a;b}}  -> Integer_Forward.ior  dom a b x
    | Integer{term=T2{tag=Ixor;a;b}} -> Integer_Forward.ixor dom a b x
    | Integer{term=T1{tag=Itimes k;a}} -> Integer_Forward.itimes k dom a x

    | Bool{term=T2{tag=Beq size;a;b}} ->   Binary_Forward.beq   ~size dom a b x
    | Bool{term=T2{tag=Bisle size;a;b}} -> Binary_Forward.bisle ~size dom a b x
    | Bool{term=T2{tag=Biule size;a;b}} -> Binary_Forward.biule ~size dom a b x
    | Binary{term=T2{tag=Biadd{size;nsw;nuw;nusw};a;b}} ->  Binary_Forward.biadd  ~size ~nsw ~nuw ~nusw dom a b x
    | Binary{term=T2{tag=Bisub{size;nsw;nuw;nusw};a;b}} ->  Binary_Forward.bisub  ~size ~nsw ~nuw ~nusw dom a b x
    | Binary{term=T2{tag=Bimul{size;nsw;nuw};a;b}} ->  Binary_Forward.bimul  ~size ~nsw ~nuw dom a b x
    | Binary{term=T2{tag=Bshl{size;nsw;nuw};a;b}} ->   Binary_Forward.bshl   ~size ~nsw ~nuw dom a b x
    | Binary{term=T2{tag=Bisdiv(size);a;b}} ->         Binary_Forward.bisdiv ~size           dom a b x
    | Binary{term=T2{tag=Bismod(size);a;b}} ->         Binary_Forward.bismod ~size           dom a b x
    | Binary{term=T2{tag=Biudiv(size);a;b}} ->         Binary_Forward.biudiv ~size           dom a b x
    | Binary{term=T2{tag=Biumod(size);a;b}} ->         Binary_Forward.biumod ~size           dom a b x
    | Binary{term=T2{tag=Bashr(size);a;b}} ->          Binary_Forward.bashr  ~size           dom a b x
    | Binary{term=T2{tag=Blshr(size);a;b}} ->          Binary_Forward.blshr  ~size           dom a b x
    | Binary{term=T2{tag=Band(size);a;b}} ->           Binary_Forward.band   ~size           dom a b x
    | Binary{term=T2{tag=Bor (size);a;b}} ->           Binary_Forward.bor    ~size           dom a b x
    | Binary{term=T2{tag=Bxor(size);a;b}} ->           Binary_Forward.bxor   ~size           dom a b x
    | Binary{term=T2{tag=Bconcat(size1,size2);a;b}} -> Binary_Forward.bconcat ~size1 ~size2  dom a b x
    | Binary{term=T1{tag=Bsext(size);a=Binary{size=oldsize} as a}} -> Binary_Forward.bsext ~size ~oldsize           dom a x
    | Binary{term=T1{tag=Buext(size);a=Binary{size=oldsize} as a}} -> Binary_Forward.buext ~size ~oldsize           dom a x
    | Binary{term=T1{tag=Bextract{size;index;oldsize};a}} ->          Binary_Forward.bextract ~size ~index ~oldsize dom a x

    | Bool{term=(Mu_formal _|Unknown _) } -> dom
    | Integer{term=(Mu_formal _|Unknown _) } -> dom
    | Binary{term=(Mu_formal _|Unknown _) } -> dom

    (* We can, but do not, propagate across mu. *)
    | Binary{term=Tuple_get(_,Mu _)} -> dom
    | Integer{term=Tuple_get(_,Mu _)} -> dom
    | Bool{term=Tuple_get(_,Mu _)} -> dom

    (* We could also propagate across nondet. *)
    (* Maybe the "Context" should have a term, and we should propagate when it becomes bottom.  *)
    | Bool{term=Tuple_get(_,Nondet _)} -> dom
    | Binary{term=Tuple_get(_,Nondet _)} -> dom
    | Integer{term=Tuple_get(_,Nondet _)} -> dom


    | t -> Codex_log.fatal "forward_interp on %a" Constraints.pretty t

  

  (**************** Backward propagation ****************)

  module MakeWorklist(Comp:sig 
      val compare: Constraints.any -> Constraints.any -> int 
    end) = struct

    module Heap = Binary_heap.Make(struct 
        type t = Constraints.any
        let compare = Comp.compare
      end)

    let create = Heap.create
    let add = Heap.add
    let pop_minimum = Heap.pop_minimum
  end

  module BackwardWorklist = MakeWorklist(struct 
      open Constraints
      let compare a b = (Any.get_id_int b) - (Any.get_id_int a)
    end)

  module ForwardWorklist = MakeWorklist(struct 
      open Constraints
      let compare a b = (Any.get_id_int a) - (Any.get_id_int b)
    end)

  let backward_propagate dom worklist fwd_worklist (* constrain *) =
    let limit_count = ref 0 in
    let limit = 1000 in
    let exception Stop in
    (* Backward propagate until the worklist is empty. *)
    let rec loop dom =
      try 
        let (Constraints.Any(max_elt) as max_any) = BackwardWorklist.pop_minimum worklist in
        ForwardWorklist.add fwd_worklist max_any;
        (* Possibly: also add the parents to the backward worklist;
           but make sure that elements are processed only once. *)
        (* Constraints.Parents.iter_on_parents max_elt (fun parent ->
           *     ForwardWorklist.add fwd_worklist parent cond
           *   ); *)
        (* TODO: If we find bottom, there is probably no need to
           propagate it further; by forward propagation, we should
           (probably? always?) find that the original condition is
           bottom. The only "legitimate" bottom should be the one
           bound to empty; backward propagation to bottom probably
           indicates an empty state. This may be important as backward
           propagation of bottom may propagate bottom up to the
           beginning of the program. *)
        let changed, newdom = backward_interp dom max_any in
        (* Codex_log.feedback "Backward propagation of %a:@\nold:%a@\nnew:%a" *)
        (*   Constraints.pretty max_elt pretty dom pretty newdom; *)
        let dom = newdom in
        changed |> List.iter (fun any -> 
            let Constraints.Any x = any in
            assert (Constraints.Any.get_id_int any < Constraints.Any.get_id_int max_any);
            (* Note: because we usse a binary heap, if an element is
               already in the list, we add it twice. We could pair
               with a Hash to avoid that if this was problematic (it
               probably is not). *)
            BackwardWorklist.add worklist any;
            incr limit_count;
            if !limit_count > limit
            then (Codex_log.warning "Propagation: limit found"; raise Stop)
          );
        loop dom 
      with Binary_heap.Empty | Stop -> dom
    in loop dom 
  ;;

  let forward_propagate dom fwd_worklist =
    let rec loop dom =
      try 
        let (Constraints.Any(max_elt) as max_any) = ForwardWorklist.pop_minimum fwd_worklist in
        let newdom = forward_interp dom max_any in
        (* Codex_log.feedback "Forward propagation of %a:@\nold:%a@\nnew:%a" *)
        (*   Constraints.pretty max_elt pretty dom pretty newdom; *)
        loop newdom
      with Binary_heap.Empty -> dom
    in loop dom
  ;;


  let assume =
    fun dom cond ->
      assert(BooleanMap.find cond dom.boolean_map = Lattices.Quadrivalent.Top);

    let dummy = Constraints.Any(Constraints.Build.Boolean.true_) in
    let worklist = (BackwardWorklist.create ~dummy 50) in
    let fwd_worklist = (ForwardWorklist.create ~dummy 50) in
      
    let boolean_map = BooleanMap.replace cond Lattices.Quadrivalent.True dom.boolean_map in
    let dom = { dom with boolean_map } in
    BackwardWorklist.add worklist (Constraints.Any cond);
    let dom = backward_propagate dom worklist fwd_worklist in
    let dom = forward_propagate dom fwd_worklist in
    (* Codex_log.feedback "after assume %a:@\nold %a@\nnew %a"
     *   Constraints.pretty cond pretty dom pretty dom'; *)
    Some dom
  
end
