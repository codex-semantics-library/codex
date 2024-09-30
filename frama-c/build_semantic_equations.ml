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

open Codex
open Frama_c_kernel

(* Build semantic equations: conversion/interpretation of CIL to/into
   a simpler internal language. *)

type size = int


module Case = struct
  type case =
    | Case of Z.t
    | Default

  include Datatype.Make_with_collections(struct
    type t = case
    let name = "Cases"
    let reprs = [Default; Case(Z.one)];;
    include Datatype.Undefined;;
    let pretty _ = assert false;;
    let equal a b = match a,b with
      | Case a, Case b -> Z.equal a b
      | Default, Default -> true
      | _ -> false
    let compare a b = match a,b with
      | Default, Default -> 0
      | Default, Case _ -> -1
      | Case _, Default -> 1
      | Case a, Case b -> Z.compare a b
    let hash = function
      | Default -> 37
      | Case a -> 37 + 281 * Z.hash a
  end)
end



(* Modules with this signature defines a language, to which Cil is
   translated. The terms of this language are given using a set of
   smart constructors; how it should be interpreted is given using the
   "return" and "bind" monadic operations.

   Exemples of modules with this signature include a compilation to
   3-address, SSA or logic programming (using the CPS monad to name
   the intermediary results of computation), or concrete or abstract
   interpretation (using the state monad to accumulate traces, alarms,
   or relations between variables). *)
module type Lang = sig

  (****************************************************************)

  (* A representation of the result of evaluation of an expression. *)
  type binary

  (* A representation of the result of evaluation of a predicate. *)
  type boolean

  (* A representation of a position where control can jump to. This
     encompasses function labels, return, exceptions, calls to
     exit()... *)
  type continuation

  (* A representation of the whole memory (at some point of the
     program). This includes both the stack and the heap. *)
  type memory

  module Monadic_Arity:sig
    (* Monadic operations. *)
    type 'a m
    val return: 'a -> 'a m
    val bind: 'a m -> ('a -> 'b m) -> 'b m
    type 'r ar0 = 'r m
    type ('a,'r) ar1 = 'a -> 'r m
    type ('a,'b,'r) ar2 = 'a -> 'b -> 'r m
    type ('a,'b,'c,'r) ar3 = 'a -> 'b -> 'c -> 'r m
    type ('a,'r) variadic = 'a list -> 'r m
  end
  type 'a m = 'a Monadic_Arity.m

  (****************************************************************)
  (* Operations on (binary) values, and basic expressions. Size is
     computed automacally in some cases. TODO: It should not; just
     pass the size everywhere. *)

  module Binary:sig
    include Transfer_functions.Binary_Forward with module Arity := Monadic_Arity
                                              and type binary = binary
                                              and type boolean = boolean
                                                                   (* and module Ref_Addr = Ref_addr_non_modular.Ref_Addr *)
    val binary_unknown: size:int -> binary m
  end


  (****************************************************************)
  (* Operations on truth values, and basic predicates. *)

  module Boolean:sig
    include Transfer_functions.Boolean_Forward with module Arity := Monadic_Arity
                                               and type boolean = boolean
  end

  (* Transforms a boolean value into an integer. Note that the inverse
     conversion does not exist: it depends on the type of the value
     (and generally consists in comparison with the zero of the
     type). *)
  val value_of_truth: size -> boolean -> binary m

  (****************************************************************)
  (* Operations on memory. *)

  module Memory: sig
    include Transfer_functions.Memory_Forward with module Arity := Monadic_Arity
                                              and type memory = memory
                                              and type address = binary
                                              and type boolean = boolean
                                              and type value = binary
   val assume: (boolean,memory,memory) Monadic_Arity.ar2
   val pp : Format.formatter -> memory -> unit
  end

  (****************************************************************)
  (* Operations on control flow. *)

  (* In concrete semantics, a statement is a continuation that takes a
     memory, and will call (i.e. emp to) to another continuation with
     an updated memory. The "answer" type represents the result of
     this call.

     The idea is that the builder of semantic equations is not
     standalone, but is used by a larger module, that will call it
     iteratively on each statement (e.g. dataflow analysis call the
     builder according to the worklist of statements; a compiler
     iteratively calls the semantic equations builder on each
     statement). "answer" is the result to be used by this larger
     module.

     This answer type is necessary because except for concrete
     interpretation, there is not a single thread of execution
     (e.g. dataflow is non-deterministic execution, and compilation is
     not an execution *)
  type answer

  val apply_cont: continuation -> memory -> answer m
  val choose_and_apply_cont: boolean -> continuation -> continuation -> memory -> answer m
  val select_and_apply_cont: binary -> (Case.Set.t * continuation) list -> memory -> answer m
  val no_continuation: memory -> answer m
end

(* This helper module is used to provide information about the
   conversion from Cil to Lang.

   Note that this makes Lang independent from Cil, and Lang be thus
   reused in other contexts; for instance Cil can be translated to an
   intermediate language (e.g. a reified version of Lang), on which we
   could reuse a Lang interpreter. *)
module type Conv = sig
  module L:Lang

  (* Contains static information about the translation from Cil to L. *)
  (* TODO: Currently not used. Is this useful? *)
  type env

  val loc_of_var: env -> Cil_types.varinfo -> L.binary;;
  val loc_of_string: env -> string -> L.binary;;
  val loc_of_wstring: env -> int64 list -> L.binary;;
  val continuation_of_stmt: env -> Cil_types.stmt -> Cil_types.stmt ->  L.continuation ;;
  val return_continuation: env -> Cil_types.stmt -> L.continuation;;

  val register_alarm: Alarms.alarm -> Cil_types.location -> L.boolean -> unit L.m
  val register_lvalue: Cil_types.lval -> L.binary -> unit L.m;;
  val register_expression: Cil_types.exp -> L.binary -> unit L.m;;
  val register_boolean_expression: Cil_types.exp -> L.boolean -> unit L.m;;

(* TODO: conversion des types C vers la representation dans Lang.
   Exemple of conversions: just the size of the type, or a
   description of the layout, etc. *)
end

(* Converts Cil terms to L terms. *)
module Make(L:Lang)(Conv:Conv with module L = L) = struct

  open Cil_types;;


  (**************** The state monad, used in expressions ****************)

  (* Transform the initial monad into a state monad, to thread other
     things during the compilation of expressions; here we thread the
     memory, which gets changed by the various alarms and assertions. *)


  (* Note: we use option types as there may be no mem, for
     instance when evaluating constant expressions during
     initialization.
     MAYBE: always require mem. *)
  type state = { mem: L.memory option; };;

  module M' = struct

    module Monadic_Arity:sig
      (* Monadic operations on the state monad. *)
      type 'a m
      val return: 'a -> 'a m
      val return2: 'a L.Monadic_Arity.m -> 'a m
      val bind: 'a m -> ('a -> 'b m) -> 'b m
      type 'r ar0 = 'r m
      type ('a,'r) ar1 = 'a -> 'r m
      type ('a,'b,'r) ar2 = 'a -> 'b -> 'r m
      type ('a,'b,'c,'r) ar3 = 'a -> 'b -> 'c -> 'r m
      type ('a,'r) variadic = 'a list -> 'r m
      val run: state -> 'a m -> ('a * state) L.Monadic_Arity.m

      val register_alarm: (Alarms.alarm * Cil_types.location) -> L.boolean -> unit m
      val register_lvalue: Cil_types.lval -> L.binary -> unit m
      val register_expression: Cil_types.exp -> L.binary -> unit m
      val register_boolean_expression: Cil_types.exp -> L.boolean -> unit m

      (* Return an unknown value, and update the world. *)
      val binary_unknown: size:int -> L.binary m

      (* Load with the memory hidden. *)
      val load: size:int -> L.binary -> L.binary m

    end = struct
      type 'a m = state -> ('a * state) L.Monadic_Arity.m

      let return x = fun mem -> L.Monadic_Arity.return (x,mem)
      let bind x f =
        fun s ->
          L.Monadic_Arity.bind (x s) @@ fun (v,s2) ->
            (f v) s2
      ;;

      (* Lift a value inside the L monad to the M monad. *)
      let return2 x = fun state ->
        L.Monadic_Arity.bind x (fun v -> L.Monadic_Arity.return (v,state));;

      type 'r ar0 = 'r m
      type ('a,'r) ar1 = 'a -> 'r m
      type ('a,'b,'r) ar2 = 'a -> 'b -> 'r m
      type ('a,'b,'c,'r) ar3 = 'a -> 'b -> 'c -> 'r m
      type ('a,'r) variadic = 'a list -> 'r m

      let run state (x:'a m) = x state

      let register_alarm_with_assume (alarm,loc) bool = fun st ->
        L.Monadic_Arity.bind (Conv.register_alarm alarm loc bool) @@ fun () ->
        let mem = match st.mem with Some x -> x | None -> assert false in
        L.Monadic_Arity.bind (L.Memory.assume bool mem) @@ fun mem ->
        L.Monadic_Arity.return ((),{mem = Some mem})

      let register_alarm_no_assume (alarm,loc) bool = fun st ->
        L.Monadic_Arity.bind (Conv.register_alarm alarm loc bool) @@ fun () ->
        L.Monadic_Arity.return ((),st)


      let register_alarm =
        if Codex_config.assume_alarms()
        then register_alarm_with_assume
        else register_alarm_no_assume
      ;;


      let register_lvalue exp bin = return2 (Conv.register_lvalue exp bin)
      let register_expression exp bin = return2 (Conv.register_expression exp bin)
      let register_boolean_expression exp bin =
        return2 (Conv.register_boolean_expression exp bin)


      let binary_unknown ~size = return2 (L.Binary.binary_unknown ~size)

      let load ~size address =
        fun state ->
        let mem = match state.mem with None -> assert false | Some mem -> mem in
        L.Monadic_Arity.bind
          (L.Memory.load ~size mem address)
          (fun (res,mem) ->
             let state = {mem=Some mem} in
             L.Monadic_Arity.return (res,state))


    end

    let value_of_truth size bool =
      Monadic_Arity.return2 (L.value_of_truth size bool)

    module Conversion = struct
      module From_Arity = L.Monadic_Arity
      module To_Arity = Monadic_Arity
      let ar0 x = Monadic_Arity.return2 x
      let ar1 f = fun x -> Monadic_Arity.return2 @@ f x
      let ar2 f = fun x y -> Monadic_Arity.return2 @@ f x y
      let ar3 f = fun x y z -> Monadic_Arity.return2 @@ f x y z
      let variadic f = fun l -> Monadic_Arity.return2 @@ f l
    end

    module Binary:Transfer_functions.Binary_Forward with module Arity := Monadic_Arity
                                                    and type binary = L.binary
                                                    and type boolean = L.boolean =
      Transfer_functions.Conversions.Convert_Binary_Forward(Conversion)(L.Binary)

    module Boolean:Transfer_functions.Boolean_Forward with module Arity := Monadic_Arity
                                                       and type boolean = L.boolean =
      Transfer_functions.Conversions.Convert_Boolean_Forward(Conversion)(L.Boolean)

    module Memory:Transfer_functions.Memory_Forward with module Arity := Monadic_Arity
                                                    and type address = L.binary
                                                    and type boolean = L.boolean
                                                    and type memory = L.memory
                                                    and type value = L.binary =
    struct
      include Transfer_functions.Conversions.Convert_Memory_Forward(Conversion)(L.Memory)
      (* Use Monadic_Arity.load instead. *)
      let load ~size = assert false
    end

  end


  module M = M'

  let return = M.Monadic_Arity.return
  let (>>=) = M.Monadic_Arity.bind

  (**************** Compilation of expressions ****************)

  let ptr_bit_size = Cil.bitsSizeOf Cil.voidPtrType;;

  let cast_size ~sext ~from_size ~to_size v =
    if from_size > to_size then M.Binary.bextract ~index:0 ~size:to_size ~oldsize:from_size v
    else if from_size < to_size then (if sext then M.Binary.bsext else M.Binary.buext) ~size:to_size ~oldsize:from_size v
    else return v
  ;;

  type bitfield = {
    bit_offset: int;
    bit_size: int;
  }

  type compiled_lvalue = {
    address: L.binary;
    size: int;
    bitfield: bitfield option;
  }

  let z_of_integer c = Z.of_string @@ Integer.to_string c;;

  let constant env ~size = function
    | CInt64(c,_,_) ->
       M.Binary.biconst ~size (z_of_integer c)
    | CChr c ->
       M.Binary.biconst ~size (z_of_integer (Cil.charConstToInt c))
    | CEnum {eival} ->
       (match Cil.isInteger eival with
       | None -> assert false   (* Should not happen *)
       | Some v -> M.Binary.biconst ~size (z_of_integer v))
    | CStr s -> return (Conv.loc_of_string env s)
    | CWStr ws -> return (Conv.loc_of_wstring env ws)
    | CReal _ -> M.Monadic_Arity.binary_unknown ~size
  ;;

  let rec expression' env exp =
    let exp_size = Cil.(bitsSizeOf (typeOf exp)) in
    match exp.enode with
    | Const c -> constant env ~size:exp_size c
    | Lval(lv) ->
      lvalue env lv >>= fun loc ->
      let size = loc.size in
      let address = loc.address in
      M.Binary.valid ~size Transfer_functions.Read address >>= fun valid ->
      M.Monadic_Arity.register_alarm (Alarms.Memory_access(lv,Alarms.For_reading),exp.eloc) valid  >>= fun () ->
      (* Translate volatile using bunknown. *)
      (if Cil.typeHasQualifier "volatile" @@ Cil.typeOfLval lv
      then M.Monadic_Arity.binary_unknown ~size
      else M.Monadic_Arity.load ~size address
      ) >>= fun v -> begin
        match loc.bitfield with
        | None -> return v
        | Some {bit_offset;bit_size} ->
          (* MAYBE: optimize this case. if bit_offset mod 8 == 0 && bit_size mod 8 == 0 -> *)
          (* Note: shouldn't this depend on some machdep?  *)
          M.Binary.bextract ~size:bit_size ~oldsize:loc.size ~index:bit_offset v >>= fun res ->
          if bit_size == exp_size then return res
          else let op = match Cil.unrollType @@ Cil.typeOf exp with
              | TInt(ikind,_) -> if Cil.isSigned ikind then M.Binary.bsext ~oldsize:bit_size else M.Binary.buext ~oldsize:bit_size
              | _ -> assert false in
            op ~size:exp_size res
      end
    | AddrOf lv (* | StartOf lv *) -> lvalue env lv >>= fun loc -> return loc.address

    (* Most of the time this allows to merge indice 0 with the others
       in our abstract domain for offsets. *)
    | StartOf lv -> lvalue env lv >>= fun loc ->
      M.Binary.biconst Z.zero ~size:ptr_bit_size >>= fun zero ->
      let (TArray(t,_,_)) = Cil.unrollType @@ Cil.typeOfLval lv in
      M.Binary.bindex ~size:ptr_bit_size (Cil.bytesSizeOf t) loc.address zero

    | BinOp(bop, e1, e2, _) -> binop env exp_size bop e1 e2
    | UnOp(uop, e1, _) -> unop env exp_size uop e1
    | SizeOf(typ) ->
       M.Binary.biconst ~size:exp_size (Z.of_int (Cil.bytesSizeOf typ))
    | SizeOfE(exp) ->
       M.Binary.biconst ~size:exp_size (Z.of_int (Cil.bytesSizeOf (Cil.typeOf exp)))
    | CastE(to_typ,subexp) ->
       let from_typ = Cil.typeOf subexp in
       expression env subexp >>= fun subexp ->
       (match Cil.unrollType from_typ, Cil.unrollType to_typ with
       | (TInt (from_kind,_) | TEnum({ekind = from_kind},_)),
         (TEnum({ekind = to_kind},_) | TInt (to_kind,_)) ->
         (match Cil.bitsSizeOfInt from_kind, Cil.bitsSizeOfInt to_kind with
         | from,to_ when from == to_ -> return subexp
         | from,to_ when from < to_ ->
           let from_size = Cil.bitsSizeOf from_typ in
           assert(from == from_size);
           if Cil.isSigned from_kind
           then M.Binary.bsext ~size:to_ ~oldsize:from_size subexp
           else M.Binary.buext ~size:to_ ~oldsize:from_size subexp
         | from,to_ -> M.Binary.bextract ~index:0 ~size:to_ ~oldsize:from subexp)
       | TFloat _, (TInt _ | TEnum _)
       | (TInt _ | TEnum _), TFloat _
       | TFloat _, TFloat _ -> M.Monadic_Arity.binary_unknown ~size:(Cil.bitsSizeOf to_typ)

       (* Conversion between pointers, and putting pointers in arrays *)
       | (TPtr _ | TArray _) , (TPtr _ | TArray _) -> return subexp
       | (TInt _ | TEnum _ | TPtr _), (TInt _ | TEnum _ | TPtr _)
         when Cil.bitsSizeOf from_typ == Cil.bitsSizeOf to_typ -> return subexp
       | _, _ -> Kernel.fatal "cast not handled: from %a to %a (sizes: %d and %d)"
                   Cil_datatype.Typ.pretty from_typ Cil_datatype.Typ.pretty to_typ
                   (Cil.bitsSizeOf from_typ) (Cil.bitsSizeOf to_typ))
    | _ -> Kernel.fatal "Expression not implemented: %a" Cil_datatype.Exp.pretty exp

  and expression env exp =
    expression' env exp >>= fun result ->
    M.Monadic_Arity.register_expression exp result >>= fun () ->
    M.Monadic_Arity.return result

  and apply_binpred bop typ v1 v2 =
    let signed = match (Cil.unrollType typ) with
      | Cil_types.TInt(ikind,_) | Cil_types.TEnum({Cil_types.ekind = ikind},_) -> Cil.isSigned ikind
      | Cil_types.TPtr _ | Cil_types.TArray _ -> false (* Pointer is seen as unsigned integers. *)
      | Cil_types.TFloat _ -> false (* And also floats, currently. *)
      | TFun _ -> assert false
      | TNamed _ -> assert false
      | TVoid _ -> assert false
      | TComp _ -> assert false
      | TBuiltin_va_list _ -> assert false
    in
    let size = Cil.bitsSizeOf typ in
    let res =
      match bop with
      | Lt -> (if signed then M.Binary.bisle ~size else M.Binary.biule ~size) v2 v1 >>= fun v -> M.Boolean.not v
      | Le -> (if signed then M.Binary.bisle ~size else M.Binary.biule ~size) v1 v2
      | Gt -> (if signed then M.Binary.bisle ~size else M.Binary.biule ~size) v1 v2 >>= fun v -> M.Boolean.not v
      | Ge -> (if signed then M.Binary.bisle ~size else M.Binary.biule ~size) v2 v1
      | Ne -> (M.Binary.beq ~size v1 v2) >>= fun v -> M.Boolean.not v
      | Eq -> M.Binary.beq ~size v1 v2
      | _ -> assert false
    in res


  and binop env exp_size bop e1 e2 =
    (* TODO:
       - Generate alarms (actually, do not do it here; use alarms
         generated by rte, so as to create a link to alarms on terms).
       - Actual operation depends on the type and sign of the expressions *)

    let add_shift ~sext ~index_size k v1 v2 =
      cast_size ~sext ~from_size:index_size ~to_size:ptr_bit_size v2 >>= fun off ->
      M.Binary.bindex ~size:ptr_bit_size k v1 off
    in

    (* Operations that depends on the sign. *)
    let arith_op typ op =
      assert (Cil.isIntegralType typ); (* For now *)
      let size = Cil.bitsSizeOf typ in
      match (Cil.isSignedInteger typ, op) with
      | (true,Div) -> M.Binary.bisdiv ~size
      | (true,Mod) -> M.Binary.bismod ~size
      | (false,Div) -> M.Binary.biudiv ~size
      | (false,Mod) -> M.Binary.biumod ~size
      | _ -> assert false
    in

    let typ_e1 = Cil.typeOf e1 in
    let typ_e2 = Cil.typeOf e2 in

    (* This is the C standard default: unsigned overflow may occur on
       all integer operations, and operations on signed integers
       cannot perform a signed overflow. Pointer operations are
       different.*)
    let nuw = false in
    let nsw = Cil.isSignedInteger typ_e1 in
    let nusw = false in (* TODO : to check *)

    (* v1 op v2 *)
    let apply bop = match bop with
      (* Filter arithmetical floating-point operations first. *)
      | PlusA | MinusA | Mult | Div | Mod when Cil.isFloatingType typ_e1 ->
         assert(Cil.isFloatingType typ_e2);
        fun _v1 _v2 -> M.Monadic_Arity.binary_unknown ~size:exp_size

      (* Arithmetical operations. *)
      | PlusA -> M.Binary.biadd ~size:exp_size ~nsw ~nuw ~nusw
      | MinusA -> M.Binary.bisub ~size:exp_size ~nsw ~nuw ~nusw
      | Mult -> M.Binary.bimul ~size:exp_size ~nsw ~nuw
      | Div | Mod -> fun v1 v2 ->
        M.Binary.biconst ~size:exp_size Z.zero >>= fun zero ->
        M.Binary.beq ~size:(exp_size) (* exp_size *) zero v2 >>= fun bool ->
        M.Boolean.not bool >>= fun bool ->
        M.Monadic_Arity.register_alarm (Alarms.Division_by_zero e2,e2.eloc) bool >>= fun () ->
        let typ = Cil.typeOf e1 in
        (arith_op typ bop) v1 v2

      (* Bitwise operations. *)
      | BAnd -> M.Binary.band ~size:exp_size
      | BXor -> M.Binary.bxor ~size:exp_size
      | BOr -> M.Binary.bor ~size:exp_size
      | Shiftlt | Shiftrt  as op ->
         let typ = Cil.typeOf e1 in
         assert (Cil.isIntegralType typ);
         assert (Cil.bitsSizeOf typ == exp_size);
         fun v1 v2 ->
           (* C operators do not mandate that operands have the same
              size, but Codex operand does. Cast the right operand to
              the correct size. *)
           cast_size ~sext:false ~from_size:(Cil.bitsSizeOf @@ Cil.typeOf e2) ~to_size:exp_size v2 >>= fun v2 ->
           (match (Cil.isSignedInteger typ,op) with
            (* Note: the behaviour of shifting negative values is actually unspecified. *)
           | (true,Shiftlt) -> M.Binary.bshl ~size:exp_size ~nsw ~nuw v1 v2
           | (true,Shiftrt) -> M.Binary.bashr ~size:exp_size v1 v2
           | (false,Shiftlt) -> M.Binary.bshl ~size:exp_size ~nsw ~nuw v1 v2
           | (false,Shiftrt) -> M.Binary.blshr ~size:exp_size v1 v2
           | _ -> assert false
           )

      (* Predicates on integers and pointers. *)
      | Lt | Gt | Le | Ge | Ne | Eq -> fun v1 v2 ->
        (apply_binpred bop Cil.(unrollType (typeOf e1))) v1 v2 >>= fun b ->
        M.value_of_truth exp_size b

      (* Pointer operations. *)
      | PlusPI (* | IndexPI *) ->
         let k = Cil.(bytesSizeOf (typeOf_pointed (typeOf e1))) in
         let index_size = Cil.bitsSizeOf typ_e2 in
         add_shift ~sext:(Cil.isSignedInteger typ_e2) ~index_size k
      | MinusPI -> fun v1 v2 ->
        let k = Cil.(bytesSizeOf (typeOf_pointed (typeOf e1))) in
        let index_size = Cil.bitsSizeOf typ_e2 in
        cast_size ~sext:(Cil.isSignedInteger typ_e2) ~from_size:index_size ~to_size:ptr_bit_size v2 >>= fun off ->
        M.Binary.biconst ~size:ptr_bit_size Z.zero >>= fun zero ->
        M.Binary.bisub ~size:ptr_bit_size ~nsw:false ~nuw:false ~nusw:false zero off >>= fun moff ->
        M.Binary.bindex ~size:ptr_bit_size k v1 moff
      | MinusPP -> fun v1 v2 ->
        let k = Cil.(bytesSizeOf (typeOf_pointed (typeOf e1))) in
        let k = Z.of_int k in
        M.Binary.bisub ~size:exp_size ~nsw:false ~nuw:false ~nusw:false v1 v2 >>= fun diff ->
        (M.Binary.biconst ~size:exp_size k) >>= fun k ->
        M.Binary.biudiv ~size:exp_size diff k

      (* Boolean specials. *)
      | LAnd | LOr -> assert false
    in
    expression env e1 >>= fun v1 ->
    expression env e2 >>= fun v2 ->
    apply bop v1 v2

  and unop env exp_size uop e1 =
    match uop with
    | Neg ->
       expression env e1 >>= fun v1 ->
       M.Binary.biconst ~size:exp_size Z.zero >>= fun zero ->
       M.Binary.bisub ~size:exp_size ~nsw:true ~nuw:false ~nusw:false zero v1
    | LNot -> cond_node env e1 >>= fun (v1:L.boolean) ->
      M.Boolean.not v1 >>= fun notv1 ->
      M.value_of_truth exp_size notv1
    | BNot ->
       expression env e1 >>= fun v1 ->
       M.Binary.biconst ~size:exp_size Z.minus_one >>= fun ffff ->
       M.Binary.bxor ~size:exp_size ffff v1

  and lhost env = function
    | Var(var) -> return (Conv.loc_of_var env var)
    | Mem exp -> expression env exp

  (* TODO: On renvoie (addresse,taille de la lvalue en bits,offset en bits,taille en bits) *)
  and loffset env typ (v:L.binary) offs =
    let size =
      try Cil.bitsSizeOf typ
      with Cil.SizeOfError _ ->
      match Cil.unrollType typ with
      | TFun _ ->
        Codex_config.function_size()
      | TArray(_,None,_) -> assert false
      | _ -> assert false
    in
    (* Note: max should be None for C99 flexible array members. But even then, pointer arithmetics
       cannot go before the array. *)
    let do_field fi ~byte_offset ~byte_size remaining_offset =
      M.Binary.bshift ~size:ptr_bit_size ~offset:byte_offset ~max:(Some (byte_size)) v >>= fun v ->
      loffset env fi.ftype v remaining_offset
    in

    match offs with
    | NoOffset -> return {address = v; size; bitfield = None}

    (* A bitfield. *)
    | Field(fi,NoOffset) when fi.fbitfield <> None ->
      let (bit_offset,bit_size) = Cil.bitsOffset typ offs in
      return {address = v; size; bitfield = Some({bit_offset;bit_size})}

    (* Cannot be a bitfield. *)
    | Field(fi,offs) -> begin
        let (bit_offset,bit_size) = Cil.bitsOffset typ (Field(fi,NoOffset)) in
        assert(bit_offset mod 8 == 0);
        assert(bit_size mod 8 == 0);
        do_field fi ~byte_offset:(bit_offset / 8) ~byte_size:(bit_size / 8) offs
      end

    | Index(exp,offs) ->
       let pointed_typ = Cil.typeOf_array_elem typ in
       expression env exp >>= fun off ->
       let typ_exp = Cil.typeOf exp in
       let size = Cil.(bitsSizeOf typ_exp) in
       let k = Cil.bytesSizeOf pointed_typ in
       cast_size ~sext:(Cil.isSignedInteger typ_exp)
         ~from_size:size ~to_size:ptr_bit_size off >>= fun off ->
       let next = fun () ->
         M.Binary.bindex ~size:ptr_bit_size k v off >>= fun v ->
         loffset env pointed_typ v offs
       in

       (* This is to help the analysis. We already have \valid, but
          these stricter conditions help reduce the abstract state. *)
       (match Cil.unrollType typ with
        | TArray(_elt_typ,length,_) -> begin
            match length, Kernel.SafeArrays.get() with
            | Some length, true ->
              let alarm1 = Alarms.Index_out_of_bound(exp,None) in
              let alarm2 = Alarms.Index_out_of_bound(exp,Some length) in
              let length = z_of_integer @@ match (Cil.constFoldToInt ~machdep:true length) with None -> assert false | Some x -> x in
              M.Binary.biconst ~size:ptr_bit_size Z.zero >>= fun zero ->
              M.Binary.bisle ~size:ptr_bit_size zero off >>= fun boolean1 ->
              M.Monadic_Arity.register_alarm (alarm1,exp.eloc) boolean1 >>= fun () ->
              M.Binary.biconst ~size:ptr_bit_size length >>= fun bound ->
              M.Binary.bisle ~size:ptr_bit_size bound off >>= fun boolean2 ->
              M.Boolean.not boolean2 >>= fun boolean2 ->
              M.Monadic_Arity.register_alarm (alarm2,exp.eloc) boolean2 >>= fun () ->
              next ()
            | _ -> next ()
          end
        | TPtr _ -> assert false
        | _ -> assert false
       );

  and lvalue' env (host,offs) =
    lhost env host >>= fun v ->
    loffset env (Cil.typeOfLhost host) v offs

  and lvalue:Conv.env -> Cil_types.lval -> compiled_lvalue M.Monadic_Arity.m = fun env lval ->
    lvalue' env lval >>= fun result ->
    M.Monadic_Arity.register_lvalue lval result.address >>= fun () ->
    M.Monadic_Arity.return result

  (* Evaluate an expression to obtain its truth value. *)
  and cond_node env e =
    match e.enode with


    (* If e.enode is already a boolean expression, we can return a
       truth value directly. This avoids a "double conversion" (from
       boolean to int and back), which results in translating the
       condition in "if(x <= 3)" as "value_of_truth(x <= 3) != 0". *)
    | BinOp((Eq|Ne|Gt|Lt|Ge|Le as op),e1,e2,_) ->
      expression env e1 >>= fun v1 ->
      expression env e2 >>= fun v2 ->
      apply_binpred op Cil.(unrollType (typeOf e1)) v1 v2 >>= fun res ->
      M.Monadic_Arity.register_boolean_expression e res >>= fun () ->
      M.Monadic_Arity.return res

    | UnOp(LNot,e1,_) ->
       cond_node env e1 >>= fun v ->
       M.Boolean.not v >>= fun res ->
       M.Monadic_Arity.register_boolean_expression e res >>= fun () ->
       M.Monadic_Arity.return res
    | _ ->

    (* TODO: We need to compile the type. *)

    (* The conversion to boolean depends on the static type of e. In
       every case we compare against 0, but 0 means "null pointer" for
       pointer types, and "0.0" in float types. *)
      (* Note: we do not register the boolean expression here; we prefer to register
        the non-boolean one.*)
    match (Cil.unrollType (Cil.typeOf e)) with
    | TInt _ | TEnum _ | TPtr _ as typ ->
      expression env e >>= fun cond ->
      let size = (Cil.bitsSizeOf typ) in
      M.Binary.biconst ~size Z.zero >>= fun izero ->
      M.Binary.beq ~size cond izero >>= fun eqzero ->
      M.Boolean.not eqzero
    | _ -> Codex_options.fatal "Not yet implemented cond_node %a" Cil_datatype.Exp.pretty e

  (**************** Compilation of statements (invoke the state monad) ****************)

  let make_state ~mem = {mem=Some mem;};;
  let dummy_state = {mem=None;};;
  let (>>=) = L.Monadic_Arity.bind
  let return = L.Monadic_Arity.return
  let the_mem state = (match state.mem with None -> assert false | Some x -> x)

  (* Insert the contents of a bitfield in a previous value. *)
  let bitfield_replace _env old oldsize {bit_offset;bit_size} newv newvsize =
    (* Newv may be too big to enter in the bitfield; take only the last bits. *)
    (if bit_size == newvsize then return newv
     else L.Binary.bextract ~oldsize:newvsize ~index:0 ~size:bit_size newv)
    >>= fun newv ->
    (if bit_offset == 0
     then return newv
     else
       L.Binary.bextract ~oldsize ~index:0 ~size:bit_offset old >>= fun prev ->
       L.Binary.bconcat ~size1:bit_size ~size2:bit_offset newv prev
    )
    >>= fun previous ->
    let end_index = bit_offset + bit_size in
    if end_index == oldsize
    then return previous
    else
      L.Binary.bextract ~oldsize ~index:end_index ~size:(oldsize - end_index) old >>= fun next ->
      (* Kernel.feedback "concat2 term %a size2 %d" Term.pretty (Obj.magic previous) end_index; *)
      L.Binary.bconcat ~size1:(oldsize - end_index) ~size2:end_index next previous
  ;;

  let pp_location fmt (pos_start, pos_end) =
    Format.fprintf fmt "(%a,%a)" Cil_types_debug.pp_filepath_position pos_start Cil_types_debug.pp_filepath_position pos_end

  (* Store a compiled lvalue. Handles the bitfield case. *)
  let store_lvalue ~instr_loc env lv newv newvsize mem =
    (M'.Monadic_Arity.run (make_state ~mem) @@
    let (>>=) = M.Monadic_Arity.bind in
    lvalue env lv >>= fun loc ->
    M.Binary.valid ~size:loc.size Transfer_functions.Write loc.address >>= fun valid ->
    M.Monadic_Arity.register_alarm
      (Alarms.Memory_access(lv,Alarms.For_writing),instr_loc) valid >>= fun () ->
    M.Monadic_Arity.return loc) >>= fun (loc,state) ->
    match loc.bitfield with
    | None ->
      (* Kernel.feedback "store at location %a made on term %a" pp_location instr_loc L.Memory.pp (the_mem state); *)
      L.Memory.store ~size:loc.size (the_mem state) loc.address newv
    | Some bitfield ->
      (* MAYBE: optimize the case bitfield.bit_offset mod 8 == 0 && bitfield.bit_size mod 8 == 0:  *)
      let mem = the_mem state in
      L.Memory.load ~size:loc.size mem loc.address >>= fun (oldv, mem) ->
      bitfield_replace env oldv loc.size bitfield newv newvsize >>= fun tostore ->
      L.Memory.store ~size:loc.size mem loc.address tostore

  ;;

  let init var (env:Conv.env) ~mem init =
    let rec doit lv init typ mem =
      let size = Cil.bitsSizeOf typ in
      match init with
      | SingleInit(e) ->
        (M'.Monadic_Arity.run (make_state ~mem) @@
         let (>>=) = M.Monadic_Arity.bind in
         expression env e >>= fun value ->
         lvalue env lv >>= fun address ->
         M.Monadic_Arity.return (value,address)) >>= fun ((value,address),state) ->
        assert(address.bitfield == None);
        let mem = the_mem state in
        L.Memory.store ~size mem address.address value
      | CompoundInit(ct,initl) ->
        let doinit off init typ acc =
          acc >>= fun mem ->
          doit (Cil.addOffsetLval off lv) init typ mem in
        (* Note: [implicit:true] handles incomplete initializers for us. *)
        Cil.foldLeftCompound
          ~implicit:true
          ~doinit ~ct ~initl ~acc:(return mem)
    in
    (* let initialize_padding ~size = Binary.bunknown_explicit ~size ~level:0 in *)
    (* let mem = Lang.allocate_var mem var initialize_padding in *)
    doit (Var var, NoOffset) init var.vtype (mem)
  ;;

  (**************** Builtins and calls to unknown functions. ****************)


  let call_to_unknown env mem ret f args instr_loc =
    Codex_options.warning ~once:true "Warning: no definition for %a" Cil_datatype.Varinfo.pretty f;
    M.Monadic_Arity.run (make_state ~mem) @@
    (* Compile the arguments of the function (they may be subject to runtime errors) *)
    List.fold_left (fun acc arg ->
        M.Monadic_Arity.bind acc @@ fun (acc:M.Binary.binary list) ->
        M.Monadic_Arity.bind (expression env arg) @@ fun arg ->
        M.Monadic_Arity.return (arg::acc)) (M.Monadic_Arity.return []) args >>= fun (_args,mem)  ->
    let mem = the_mem mem in

    (* XXX unsound: this considers that the function assigns
       nothing, and just returns a result. *)
    (match ret with
     | None -> return (mem)
     | Some lv ->
       let size = (Cil.bitsSizeOf (Cil.typeOfLval lv)) in
       L.Binary.binary_unknown ~size  >>= fun value ->
       store_lvalue ~instr_loc env lv value size mem)
  ;;

  let is_builtin x =  (* Put everything in compilation to term. *)
    List.mem x ["malloc";"free";"alloca";"__fc_vla_alloc";"__fc_vla_free";"exit";
                "Frama_C_bzero";"calloc";"realloc";
                "__VERIFIER_assert"; "__VERIFIER_error"; "__VERIFIER_assume";
                "__VERIFIER_nondet_int"]
  ;;


  let instruction env ~mem = function
    | Skip _ -> return mem
    | Asm _ ->
      Codex_options.warning "Skipping assembly instruction";
      return mem
    (* Optional optimisation. *)
    | Set(lvdst,({enode=Lval(lvsrc)} as exp),instr_loc) when false ->
      let f mem =
        M'.Monadic_Arity.run (make_state ~mem) @@
        let (>>=) = M.Monadic_Arity.bind in
        lvalue env lvdst >>= fun ptrdst ->
        lvalue env lvsrc >>= fun ptrsrc ->
        M.Monadic_Arity.return (ptrdst,ptrsrc)
      in
      let size = (Cil.bitsSizeOf @@ Cil.typeOf exp) in
      f mem >>= fun ((ptrdst,ptrsrc),state) ->
      let addrdst = ptrdst.address and addrsrc = ptrsrc.address in
      (* Does not work yet for memcopies in bitfields. *)
      assert (ptrdst.bitfield == None);
      assert (ptrsrc.bitfield == None);
      L.Memory.memcpy ~size (the_mem state) addrsrc addrdst

    (* Optimisation: when copying structures, copy each field.  Note
       that the analysis would be faster (and more precise, as it
       would delay the copies) if we would just call memcpy here, but
       I do not have it for now. *)
    (* Note: this is not an optimisation at all, and slows things down.
       I need to have memcpy instead. *)
    | Set(lvdst,exp,instr_loc) when false && Cil.isStructOrUnionType @@ Cil.unrollType @@ Cil.typeOf exp ->
      let open Cil_types in
      let lvsrc = match exp.enode with Lval lv -> lv | _ -> assert false in
      let rec f lvdst lvsrc mem =
        (* let return = L.Monadic_Arity.return in *)
        let (>>=) = L.Monadic_Arity.bind in
        let typ = Cil.unrollType @@ Cil.typeOfLval lvsrc in
        Codex_log.feedback "Type of %a is %a" Cil_datatype.Lval.pretty lvsrc Cil_datatype.Typ.pretty typ;
        if Cil.isArithmeticOrPointerType typ then
          mem >>= fun mem ->
          M'.Monadic_Arity.run (make_state ~mem) @@
          (* M.Memory.load ~size:(Cil.bitsSizeOf typ) addr >>= fun (value,state) -> *)
          expression env (Cil.dummy_exp @@  Lval lvsrc) >>= fun (value,state) ->
          store_lvalue ~instr_loc env lvdst value (Cil.bitsSizeOf typ) (the_mem state)
        else if Cil.isArrayType typ then begin
          (* The type should be known statically. *)
          let len = match typ with TArray(_,i,_) -> i | _ -> assert false in
          let len = match len with None -> assert false | Some len -> len in
          let len = Cil.constFoldToInt ~machdep:true len in
          let len = match len with None -> assert false | Some len -> Z.to_int len in
          let rec loop mem i =
            if i >= len then mem
            else
              let offs = Index(Cil.integer ~loc:instr_loc i,NoOffset) in
              let lvsrc = Cil.addOffsetLval offs lvsrc in
              let lvdst = Cil.addOffsetLval offs lvdst in
              let mem = f lvdst lvsrc mem in
              loop mem (i + 1)
          in loop mem 0
        end
        else begin
          assert(Cil.isStructOrUnionType typ);
          let compinfo = match typ with Cil_types.TComp(ci,_) -> ci | _ -> assert false in
          if compinfo.cstruct = false (* union *)
          then assert false           (* TODO. assign the largest field? *)
          else
            let open Cil_types in
            let mem = List.fold_left (fun mem fi ->
                let lvsrc = Cil.addOffsetLval (Field(fi,NoOffset)) lvsrc in
                let lvdst = Cil.addOffsetLval (Field(fi,NoOffset)) lvdst in
                f lvdst lvsrc mem
                ) mem (match compinfo.cfields with None-> assert false | Some x -> x)  in
            mem
        end
      in
      f lvdst lvsrc (return mem)
    | Set(lv,exp,instr_loc) ->
      if Cil.isStructOrUnionType @@ Cil.unrollType @@ Cil.typeOf exp
      then Codex_options.warning "Assignment of union or structure may\
      loose precision; we should use memcopy";
      M'.Monadic_Arity.run (make_state ~mem) @@
      expression env exp >>= fun (value,state) ->
      store_lvalue ~instr_loc env lv value (Cil.bitsSizeOf @@ Cil.typeOf exp) (the_mem state)
    | Call(ret,{enode=Lval(Var(f),NoOffset)},args,instr_loc)
      when not (Kernel_function.is_definition (Globals.Functions.get f)) ->
      assert(not @@ is_builtin f.vname);
      call_to_unknown env mem ret f args instr_loc
    | Call _ as i -> Kernel.fatal "instr %a" Cil_datatype.Instr.pretty i
    | Local_init(vi,AssignInit i,_) -> init vi env ~mem i
    | Local_init(vi,ConsInit(f,args,_),instr_loc)
      when not (Kernel_function.is_definition (Globals.Functions.get f)) ->
      assert(not @@ is_builtin f.vname);
      call_to_unknown env mem (Some(Var vi, NoOffset)) f args instr_loc
    | Local_init(_, ConsInit _, _) as i -> Kernel.fatal "instr %a" Cil_datatype.Instr.pretty i
    | _ -> failwith "Instruction not yet implemented"

  let switch env stmt =
    let (cases, default) = Cil.separate_switch_succs stmt in
    let found_default = ref false in
    let do_stmt acc succ =
      let do_label acc =
        function
        | Cil_types.Label _ -> acc
        | Cil_types.Default _ ->
           found_default := true;
           Case.Set.add Case.Default acc
        | Cil_types.Case(e,_) ->
           let case =
             (match (Cil.constFoldToInt e) with
             | None -> Kernel.fatal "costfoldto %a" Cil_datatype.Exp.pretty e
             | Some x -> Case.Case (z_of_integer x)) in
           Case.Set.add case acc
      in
      (List.fold_left do_label Case.Set.empty succ.labels,
       Conv.continuation_of_stmt env stmt succ)::acc
    in
    let result = List.fold_left do_stmt [] cases in
    (* If no default was found, the default is the syntactic successor
       of the switch, which is not one of the cases. *)
    if !found_default then result
    else (Case.Set.singleton Case.Default, Conv.continuation_of_stmt env stmt default)::result

  (* Statements can be seen as returning a continuation and a memory
     (but instead return an "answer" type, which is the result of applying
     that continuation to that memory) *)
  let statement env ~mem stmt =
    match stmt.skind with

    | Loop _ | Block _ | Break _ | Continue _ | Goto _ | UnspecifiedSequence _ ->
       (match stmt.succs with
       | [succ] -> L.apply_cont (Conv.continuation_of_stmt env stmt succ) mem
       | [] -> L.no_continuation mem
       | _ -> assert false )    (* Cannot have several successors. *)

    | Instr i ->
       instruction env ~mem i >>= fun changed_mem ->
      (match stmt.succs with
      | [succ] -> L.apply_cont (Conv.continuation_of_stmt env stmt succ) changed_mem
      | [] -> L.no_continuation changed_mem
      | _ -> assert false)      (* Cannot have several successors. *)

    | If(e,_,_,_) ->
      let state = make_state ~mem in
      M.Monadic_Arity.run state @@
        cond_node env e >>= fun (bcond,state) ->
      let (stmt_then, stmt_else) = Cil.separate_if_succs stmt in
      let kthen = Conv.continuation_of_stmt env stmt stmt_then in
      let kelse = Conv.continuation_of_stmt env stmt stmt_else in
      L.choose_and_apply_cont bcond kthen kelse (the_mem state)

    (* Note: which expression is returned is handled by the compilation of function. *)
    | Return(_,_) ->
      let continuation = Conv.return_continuation env stmt in
      L.apply_cont continuation mem

    | Switch(exp,_,_,_) ->
       let case_conds = switch env stmt in
       let state = make_state ~mem in
       M.Monadic_Arity.run state @@
         expression env exp >>= fun (v,state) ->
       L.select_and_apply_cont v case_conds (the_mem state)

    | TryFinally _ | TryExcept _ -> assert false
    | Throw _ | TryCatch _ -> assert false

  let term _env _mem t =
    match t.term_node with
    | TConst _ -> assert false
    | TLval _ -> assert false
    | TBinOp _ -> assert false
    | TUnOp _ -> assert false
    | Tcomprehension _ -> assert false
    | TCastE _ -> assert false
    | TLogic_coerce _ -> assert false
    | _ -> Kernel.fatal "Term is %a" Cil_datatype.Term.pretty t


  let predicate env mem predicate =
    match predicate.pred_content with
    | Pfalse -> L.Boolean.false_
    | Ptrue -> L.Boolean.true_
    | Prel(_,t1,t2) ->
      let _t1' = term env mem t1 in
      let _t2' = term env mem t2 in
      assert false
    | _ -> assert false


  let statement_unused_for_now env ~mem stmt =
    (* Handles ACSL assertions. *)
    let mem = Annotations.fold_code_annot (fun _ annot acc ->
        match annot.annot_content with
        | AAssert([],pred) ->
          (* let _tpred = predicate env mem pred in *)
          (* TODO: Add the assertion to the table of things to check;
             add the assume. *)
          assert false
        | _ -> Codex_options.warning "Code annotation not handled: %a"
                 Cil_datatype.Code_annotation.pretty annot; acc
      ) stmt mem in
    statement env ~mem stmt

end
