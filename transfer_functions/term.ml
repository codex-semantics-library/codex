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

(* We make the type concrete, so the type checker knows that they
   cannot be equal. *)
(* Should this go into transfer_function? *)
type boolean = private TypeBoolean
type integer = private TypeInteger
type binary  = private TypeBinary
type memory  = private TypeMemory


type size = int

type 'a typ =
  | Boolean: boolean typ
  | Integer: integer typ
  | Binary: size -> binary typ
  | Memory: memory typ      

type z_t = Z.t

type id = int                   (* A unique identifier for the term. *)

(* We define these dummy types, used to represent knowledge about
   arity in gadt. We need a definition and use constructors for this,
   but they are never used.  *)
type ar0 = private Ar0
type 'a ar1 = private Ar1
type ('a,'b) ar2 = private Ar2

module UniqueId:sig
  type t = private int
  val get: unit -> t
end = struct
  type t = int
  let count = ref 0;;
  let get () =
    incr count;
    !count
  ;;
end


type any_type =
  Any_type: 'a typ -> any_type [@@unboxed];;

type ('arg,'ret) term =
  | True: (ar0,boolean) term
  | False: (ar0,boolean) term
  | And: ((boolean,boolean) ar2,boolean) term
  | Or: ((boolean,boolean) ar2,boolean) term
  | Not: (boolean ar1,boolean) term
  | BoolUnion: ((boolean,boolean) ar2,boolean) term

  | Biconst: size * z_t -> (ar0,binary) term
  | Biadd: {size:size;nsw:bool;nuw:bool;nusw:bool} -> ((binary,binary) ar2,binary) term
  | Bisub: {size:size;nsw:bool;nuw:bool;nusw:bool} -> ((binary,binary) ar2,binary) term
  | Bimul: {size:size;nsw:bool;nuw:bool} -> ((binary,binary) ar2,binary) term
  | Biudiv:size -> ((binary,binary) ar2,binary) term
  | Bisdiv:size -> ((binary,binary) ar2,binary) term
  | Biumod:size -> ((binary,binary) ar2,binary) term
  | Bismod:size -> ((binary,binary) ar2,binary) term
  | Bshl:  {size:size;nsw:bool;nuw:bool} -> ((binary,binary) ar2,binary) term
  | Bashr: size -> ((binary,binary) ar2,binary) term
  | Blshr: size -> ((binary,binary) ar2,binary) term
  | Band:  size -> ((binary,binary) ar2,binary) term
  | Bor:   size -> ((binary,binary) ar2,binary) term
  | Bxor:  size -> ((binary,binary) ar2,binary) term

  | Beq:   size -> ((binary,binary) ar2,boolean) term
  | Bisle: size -> ((binary,binary) ar2,boolean) term
  | Biule: size -> ((binary,binary) ar2,boolean) term

  | Bconcat: size * size -> ((binary,binary) ar2,binary) term
  | Bextract: {size:size;index:int;oldsize:size} -> (binary ar1,binary) term
  | Bsext: size -> (binary ar1,binary) term
  | Buext: size -> (binary ar1,binary) term
  | Bofbool: size -> (boolean ar1,binary) term
  | Bunion: Transfer_functions_ids.Condition.t * size -> ((binary,binary) ar2,binary) term
  | Bchoose: Transfer_functions_ids.Choice.t * size -> (binary ar1,binary) term

  | Iconst: z_t -> (ar0,integer) term
  | Iadd: ((integer,integer) ar2, integer) term
  | Isub: ((integer,integer) ar2, integer) term
  | Imul: ((integer,integer) ar2, integer) term
  | Idiv: ((integer,integer) ar2, integer) term
  | Imod: ((integer,integer) ar2, integer) term
  | Ishl: ((integer,integer) ar2, integer) term
  | Ishr: ((integer,integer) ar2, integer) term
  | Ior:  ((integer,integer) ar2, integer) term
  | Ixor: ((integer,integer) ar2, integer) term
  | Iand: ((integer,integer) ar2, integer) term

  | Ieq:  ((integer,integer) ar2, boolean) term
  | Ile:  ((integer,integer) ar2, boolean) term

  | Itimes: z_t -> (integer ar1,integer) term
;;


let type_of: type a b. (a,b) term -> b typ = function
  | True-> Boolean
  | False-> Boolean
  | And-> Boolean
  | Or-> Boolean
  | Not-> Boolean
  | Biconst(size,_) -> Binary size
  | Biadd{size} -> Binary size
  | Bisub{size} -> Binary size
  | Bimul{size} -> Binary size
  | Biudiv(size) -> Binary size
  | Bisdiv(size) -> Binary size
  | Biumod(size) -> Binary size
  | Bismod(size) -> Binary size
  | Bshl{size} -> Binary size
  | Bashr(size) -> Binary size
  | Blshr(size) -> Binary size
  | Band(size) -> Binary size
  | Bor(size) -> Binary size
  | Bxor(size) -> Binary size

  | Beq(size) -> Boolean
  | Bisle(size) -> Boolean
  | Biule(size) -> Boolean

  | Bconcat(s1,s2) -> Binary(s1+s2)
  | Bextract{size;index;oldsize} -> Binary size
  | Bsext(size) -> Binary size
  | Buext(size) -> Binary size
  | Bofbool(size) -> Binary size
  | Bunion(_,size) -> Binary size
  | Bchoose(_,size) -> Binary size
                 
  | Iconst _ -> Integer
  | Iadd->  Integer
  | Isub->  Integer
  | Imul->  Integer
  | Idiv->  Integer
  | Imod->  Integer
  | Ishl->  Integer
  | Ishr->  Integer
  | Ior->  Integer
  | Ixor->  Integer
  | Iand->  Integer
  | Ieq ->  Boolean
  | Ile ->  Boolean
  | Itimes _ -> Integer
;;

(* Used to identify a problem when we do not have a pretty printer. *)
let _identify: type a b. (a,b) term -> int = function
  | True -> assert false
  | False -> assert false
  | And -> assert false
  | Or -> assert false
  | Not -> assert false
  | Biconst (_, _) -> assert false
  | Biadd _ -> assert false
  | Bisub _ -> assert false
  | Bimul _ -> assert false
  | Biudiv _ -> assert false
  | Bisdiv _ -> assert false
  | Biumod _ -> assert false
  | Bismod _ -> assert false
  | Bshl _ -> assert false
  | Bashr _ -> assert false
  | Blshr _ -> assert false
  | Band _ -> assert false
  | Bor _ -> assert false
  | Bxor _ -> assert false
  | Beq _ -> assert false
  | Bisle _ -> assert false
  | Biule _ -> assert false
  | Bconcat (_, _) -> assert false
  | Bextract _ -> assert false
  | Bsext _ -> assert false
  | Buext _ -> assert false
  | Bofbool _ -> assert false
  | Bunion (_, _) -> assert false
  | Bchoose (_, _) -> assert false
  | Iconst _ -> assert false
  | Iadd -> assert false
  | Isub -> assert false
  | Imul -> assert false
  | Idiv -> assert false
  | Imod -> assert false
  | Ishl -> assert false
  | Ishr -> assert false
  | Ior -> assert false
  | Ixor -> assert false
  | Iand -> assert false
  | Ieq -> assert false
  | Ile -> assert false
  | Itimes _ -> assert false
;;


let sdbm x y = y + (x lsl 16) + (x lsl 6) - x;;

(* We use the last 6 bits (0..63) as the type.  *)
(* We use bit 7 and 8 for flags. *)
(* We use 8 bits, from 9 to 15,  for the size1 (0..255) *)
(* We use 8 bits, from 16 to 24, for the size2 (0..255) *)
(* If there are other large elements: we sdbm with this. *)
let hash: type a b. (a,b) term -> int = function
  | True-> 1
  | False-> 2
  | And -> 3
  | Or-> 4
  | Not-> 5
  | Iadd-> 6
  | Isub-> 7
  | Imul-> 8
  | Idiv-> 9
  | Imod-> 10
  | Ishl-> 11
  | Ishr-> 12
  | Ior->  13
  | Ixor-> 14
  | Iand-> 15
  | Ieq -> 16
  | Ile -> 17
  | Biadd{size;nsw;nuw;nusw} -> 18 lor (if nsw then 64 else 0) lor (if nuw then 128 else 0) lor (size lsl 8)
  | Bisub{size;nsw;nuw;nusw} -> 19 lor (if nsw then 64 else 0) lor (if nuw then 128 else 0) lor (size lsl 8)
  | Bimul{size;nsw;nuw} -> 20 lor (if nsw then 64 else 0) lor (if nuw then 128 else 0) lor (size lsl 8)
  | Bshl{size;nsw;nuw} -> 21 lor (if nsw then 64 else 0) lor (if nuw then 128 else 0) lor (size lsl 8)
  | Biudiv(size) -> 22 lor (size lsl 8)
  | Bisdiv(size) -> 23 lor (size lsl 8)
  | Biumod(size) -> 24 lor (size lsl 8)
  | Bismod(size) -> 25 lor (size lsl 8)
  | Bashr(size) -> 26 lor (size lsl 8)
  | Blshr(size) -> 27 lor (size lsl 8)
  | Band(size) -> 28 lor (size lsl 8)
  | Bor(size) -> 29 lor (size lsl 8)
  | Bxor(size) -> 30 lor (size lsl 8)
  | Beq(size) -> 31 lor (size lsl 8)
  | Bisle(size) -> 32 lor (size lsl 8)
  | Biule(size) -> 33 lor (size lsl 8)
  | Bconcat(s1,s2) -> 34 lor (s1 lsl 8) lor (s2 lsl 16)
  | Bextract{size;index;oldsize} -> 35 lor (size lsl 8) lor (oldsize lsl 16) lor (index lsl 24)
  | Bsext(size) -> 36 lor (size lsl 8)
  | Buext(size) -> 37 lor (size lsl 8)
  | Bofbool(size) -> 38 lor (size lsl 8)
  | Bchoose(id,size) -> sdbm (39 lor (size lsl 8)) (id:>int)
  | Bunion(id,size) ->  sdbm (40 lor (size lsl 8)) (id:>int)
  | Itimes k -> sdbm 41 (Z.hash k)
  | Iconst k -> sdbm 42 (Z.hash k)
  | Biconst(size,k) -> sdbm (43 lor (size lsl 8)) (Z.hash k)
  | BoolUnion ->  44
;;

let equal: type a b c d. (a,b) term -> (c,d) term -> bool = fun a b ->
  match (a,b) with
  | True, True -> true
  | False,False -> true
  | And ,And  -> true
  | Or,Or -> true
  | Not,Not -> true
  | BoolUnion,BoolUnion -> true    
  | Iadd,Iadd -> true
  | Isub,Isub -> true
  | Imul,Imul -> true
  | Idiv,Idiv -> true
  | Imod,Imod -> true
  | Ishl,Ishl -> true
  | Ishr,Ishr -> true
  | Ior,Ior -> true
  | Ixor,Ixor -> true
  | Iand,Iand -> true
  | Ieq ,Ieq  -> true
  | Ile ,Ile  -> true
  | Biadd{size=s1;nsw=nsw1;nuw=nuw1;nusw=nusw1} ,Biadd{size=s2;nsw=nsw2;nuw=nuw2;nusw=nusw2}->
    s1 = s2 && nsw1 = nsw2 && nuw1 = nuw2 && nusw1 = nusw2
  | Bisub{size=s1;nsw=nsw1;nuw=nuw1;nusw=nusw1} ,Bisub{size=s2;nsw=nsw2;nuw=nuw2;nusw=nusw2}->
    s1 = s2 && nsw1 = nsw2 && nuw1 = nuw2 && nusw1 = nusw2                             
  | Bimul{size=s1;nsw=nsw1;nuw=nuw1} ,Bimul{size=s2;nsw=nsw2;nuw=nuw2}->
    s1 = s2 && nsw1 = nsw2 && nuw1 = nuw2
  | Bshl{size=s1;nsw=nsw1;nuw=nuw1} ,Bshl{size=s2;nsw=nsw2;nuw=nuw2}->
    s1 = s2 && nsw1 = nsw2 && nuw1 = nuw2                              
  | Biudiv(s1) ,Biudiv(s2)  -> s1 == s2
  | Bisdiv(s1) ,Bisdiv(s2)  -> s1 == s2
  | Biumod(s1) ,Biumod(s2)  -> s1 == s2
  | Bismod(s1) ,Bismod(s2)  -> s1 == s2
  | Bashr(s1) ,Bashr(s2)  -> s1 == s2
  | Blshr(s1) ,Blshr(s2)  -> s1 == s2
  | Band(s1) ,Band(s2)  -> s1 == s2
  | Bor(s1) ,Bor(s2)  -> s1 == s2
  | Bxor(s1) ,Bxor(s2)  -> s1 == s2
  | Beq(s1) ,Beq(s2)  -> s1 == s2
  | Bisle(s1) ,Bisle(s2)  -> s1 == s2
  | Biule(s1) ,Biule(s2)  -> s1 == s2
  | Bconcat(s1,s2) ,Bconcat(s3,s4)  -> s1 == s3 && s2 == s4
  | Bextract{size=s1;index=i1;oldsize=olds1},
    Bextract{size=s2;index=i2;oldsize=olds2} -> i1 == i2 && s1 == s2 && olds1 == olds2
  | Bsext(s1) ,Bsext(s2)  -> s1 == s2
  | Buext(s1) ,Buext(s2)  -> s1 == s2
  | Bofbool(s1) ,Bofbool(s2)  -> s1 == s2
  | Bchoose(id1,s1) ,Bchoose(id2,s2)  -> id1 == id2 && (assert (s1 == s2); true)
  | Bunion(id1,s1) ,Bunion(id2,s2)  -> id1 == id2 && (assert (s1 == s2); true)
  | Itimes k1 ,Itimes k2  -> Z.equal k1 k2
  | Iconst k1 ,Iconst k2  -> Z.equal k1 k2
  | Biconst(s1,k1),Biconst(s2,k2)  -> s1 == s2 && Z.equal k1 k2
  | _, _ ->
    if (hash a) == (hash b)
    then begin
      Codex_log.warning "Bad hash collision or mistake";
      (* let _ = identify b in () *)
    end;
    false
;;

(**************** Builders ****************)

module Build = struct

  module Arity = struct
    type nonrec 'r ar0 = (ar0,'r) term
    type nonrec ('a,'r) ar1 = ('a ar1,'r) term
    type nonrec  ('a,'b,'r) ar2 = (('a,'b) ar2,'r) term
    type nonrec ('a,'b,'c,'r) ar3 = unit
    type ('a,'r) variadic = unit
  end
  
  module Binary = struct

    (* module Arity = Arity *)

    type nonrec boolean = boolean
    type nonrec binary = binary
    
    let biconst ~size k = Biconst(size,k)

    let biadd ~size ~nsw ~nuw ~nusw = Biadd{size;nsw;nuw;nusw}
    let bisub ~size ~nsw ~nuw ~nusw = Bisub{size;nsw;nuw;nusw}        
    let bimul ~size ~nsw ~nuw = Bimul{size;nsw;nuw}
    let bor ~size = Bor(size)
    let band ~size = Band(size)
    let bxor ~size = Bxor(size)
    let bisdiv ~size = Bisdiv(size)
    let bismod ~size = Bismod(size)        
    let biudiv ~size = Biudiv(size)
    let biumod ~size = Biumod(size)

    let bshl ~size ~nsw ~nuw = Bshl{size;nsw;nuw}
    let bashr ~size = Bashr(size)
    let blshr ~size = Blshr(size)

    let bisle ~size = Bisle(size)
    let biule ~size = Biule(size)
    let beq ~size = Beq(size)                                                

    let bconcat ~size1 ~size2 = Bconcat(size1,size2)
    let bextract ~size ~index ~oldsize = Bextract {size;oldsize;index}
      
    let assume ~size = assert false
    let valid ~size = assert false
    let valid_ptr_arith ~size = assert false
    let bindex ~size = assert false
    let bshift ~size ~offset ~max = assert false
    let buninit ~size = assert false
    let bunknown ~size = assert false                              
    
    let bsext ~size ~oldsize = Bsext(size)
    let buext ~size ~oldsize = Buext(size)                                

    let bofbool ~size = Bofbool(size)
    let bchoose ~size choice = Bchoose(choice,size)
    let bunion  ~size cond = Bunion(cond,size)
  end

end


(**************** Generic pretty printers. ****************)


(* This allows to print something (a type 'at) that contains a term. *)
module type Pretty_Arg = sig
  type 'a t                     (* The term to print. *)
  type 'a pack                  (* Packing of the arguments. *)
  val pretty: Format.formatter -> 'a t -> unit
  (* Extraction of the subterms. *)
  val extract1: 'a ar1 pack -> 'a t
  val extract2: ('a,'b) ar2 pack -> 'a t * 'b t
end

module type Pretty_Result = sig
  type 'a t
  type 'a pack
  val pretty_destruct: Format.formatter -> ('arg,'a) term -> 'arg pack ->  unit
end

module Pretty(M:Pretty_Arg):Pretty_Result with type 'a t = 'a M.t and type 'a pack = 'a M.pack =
struct
  type 'a t = 'a M.t
  type 'a pack = 'a M.pack
  
  let pretty_destruct: type a arg. Format.formatter -> (arg,a) term -> arg M.pack ->  unit =
    fun fmt term arg ->
      let ar1 arg string =
        let x1 = M.extract1 arg in Format.fprintf fmt string M.pretty x1
      in
      let ar2 arg string =
        let x1,x2 = M.extract2 arg in Format.fprintf fmt string M.pretty x1 M.pretty x2
      in
      let open Format in
      match term with
      | True -> fprintf fmt "true"
      | False -> fprintf fmt "false"
      | And -> ar2 arg "%a && %a"
      | Or -> ar2 arg "%a || %a"
      | Not-> ar1 arg "!(%a)"
      | BoolUnion->  ar2 arg "boolunion(%a,%a)"
      | Biconst(size,k) -> 
        fprintf fmt "(bin%d " size;
        (if Z.numbits k < 8
         then (fprintf fmt "%s)" @@ Z.to_string k)
         else fprintf fmt "0x%s)" @@ Z.format "x" k)
      | Biadd(size) -> ar2 arg "(%a +b %a)"
      | Bisub(size) -> ar2 arg "(%a -b %a)"
      | Bimul(size) -> ar2 arg "(%a * %a)"                                                  
      | Biudiv(size) -> ar2 arg "(%a /bu %a)"
      | Bisdiv(size) -> ar2 arg "(%a /bs %a)"
      | Biumod(size) -> ar2 arg "(%a %%bs %a)"
      | Bismod(size) -> ar2 arg "(%a %%bu %a)"
      | Bshl(size) -> ar2 arg "(%a << %a)"
      | Blshr(size) -> ar2 arg "(%a >>l %a)"
      | Bashr(size) -> ar2 arg "(%a >>r %a)"
      | Band(size) -> ar2 arg "(%a & %a)"
      | Bor(size) -> ar2 arg "(%a | %a)"
      | Bxor(size) -> ar2 arg "(%a ^ %a)"
      | Beq(size) -> ar2 arg "(%a =b= %a)"
      | Bisle(size) -> ar2 arg "(%a <=bs %a)"
      | Biule(size) -> ar2 arg "(%a <=bu %a)"
      | Bconcat(s1,s2) -> ar2 arg "(%a::%a)"
      | Bunion(id,size) -> let x,y = M.extract2 arg in fprintf fmt "bunion%d_%d(%a,%a)" (id:>int) size M.pretty x M.pretty y                            
      | Bextract{size;index;oldsize} -> (ar1 arg "(%a)[%d:%d]")  index (index+size)
      | Buext(size) -> let x = M.extract1 arg in fprintf fmt "buext%d(%a)" size M.pretty x
      | Bsext(size) -> let x = M.extract1 arg in fprintf fmt "bsext%d(%a)" size M.pretty x
      | Bofbool(size) -> let x = M.extract1 arg in fprintf fmt "bofbool%d(%a)" size M.pretty x
      | Bchoose(id,size) -> let x = M.extract1 arg in fprintf fmt "bchoose%d_%d(%a)" (id:>int) size M.pretty x

      | Iconst k -> (if Z.numbits k < 8
                     then (fprintf fmt "%s" @@ Z.to_string k)
                     else fprintf fmt "0x%s" @@ Z.format "x" k)
      | Idiv -> ar2 arg "(%a / %a)"
      | Imod -> ar2 arg "(%a %% %a)"
      | Iadd -> ar2 arg "(%a + %a)"
      | Isub -> ar2 arg "(%a - %a)"
      | Ieq -> ar2 arg "(%a = %a)"
      | Ile -> ar2 arg "(%a <= %a)"
      | Imul -> ar2 arg "(%a * %a)"
      | Ishl ->  ar2 arg "(%a << %a)"
      | Ishr ->  ar2 arg "(%a >> %a)"
      | Iand ->  ar2 arg "(%a & %a)"
      | Ior ->  ar2 arg "(%a | %a)"
      | Ixor ->  ar2 arg "(%a ^ %a)"
      | Itimes k ->
        let x = M.extract1 arg in fprintf fmt "(%s * %a)" (Z.to_string k) M.pretty x
end

(**************** Generic evaluation functions. ****************)
(* (Currently not used) *)

(* Generic evaluators; probably the genericity is not worth it (and,
   for instance, we lack some additional arguments here). *)
module Unused_Eval(T:sig
    type ('arg,'res) t
    val true_: (ar0,boolean) t
    val false_: (ar0,boolean) t

    val (&&): ((boolean,boolean) ar2,boolean) t
    val (||): ((boolean,boolean) ar2,boolean) t        
    val not: (boolean ar1,boolean) t

    val biconst: size:size -> z_t -> (ar0,binary) t
    val bitimes: size:size -> z_t -> (binary ar1,binary) t
    val biadd: size:size -> ((binary,binary) ar2,binary) t
    val bisub: size:size -> ((binary,binary) ar2,binary) t
    val bimul: size:size -> ((binary,binary) ar2,binary) t
    val bisdiv: size:int -> ((binary,binary) ar2,binary) t
    val bismod: size:int -> ((binary,binary) ar2,binary) t
    val biudiv: size:int -> ((binary,binary) ar2,binary) t
    val biumod: size:int -> ((binary,binary) ar2,binary) t

    val bshl: size:int -> ((binary,binary) ar2,binary) t
    val bashr: size:int -> ((binary,binary) ar2,binary) t
    val blshr: size:int -> ((binary,binary) ar2,binary) t

    val band: size:int -> ((binary,binary) ar2,binary) t
    val bor: size:int -> ((binary,binary) ar2,binary) t
    val bxor: size:int -> ((binary,binary) ar2,binary) t

    val beq: size:size   -> ((binary,binary) ar2,boolean) t
    val bisle: size:size -> ((binary,binary) ar2,boolean) t
    val biule: size:size -> ((binary,binary) ar2,boolean) t

    (* First argument become most significant. *)
    val bconcat: size1:int -> size2:int -> ((binary,binary) ar2,binary) t
    val bextract: size:int -> index:int -> oldsize:int -> (binary ar1,binary) t

    val buext: size:int -> (binary ar1,binary) t
    val bsext: size:int -> (binary ar1,binary) t


    val iconst: z_t -> (ar0,integer) t
    val itimes: z_t -> (integer ar1,integer) t
    val iadd: ((integer,integer) ar2,integer) t
    val imul: ((integer,integer) ar2,integer) t
    val idiv: ((integer,integer) ar2,integer) t
    val imod: ((integer,integer) ar2,integer) t
    val ishl: ((integer,integer) ar2,integer) t
    val ishr: ((integer,integer) ar2,integer) t
    val iand: ((integer,integer) ar2,integer) t
    val ior: ((integer,integer) ar2,integer ) t
    val ixor: ((integer,integer) ar2,integer) t
    val isub: ((integer,integer) ar2,integer) t
    val ieq: ((integer,integer) ar2,boolean ) t
    val ile: ((integer,integer) ar2,boolean ) t

  end) = struct

  let eval:type arg res. (arg,res) term -> (arg,res) T.t = function
    | True -> T.true_
    | False -> T.false_
    | And -> T.(&&)
    | Or -> T.(||)
    | Not -> T.not
    | Biconst(size,k) -> T.biconst ~size k
    | Biadd{size;nsw;nuw;nusw} -> (* T.biadd ~size ~nsw ~nuw *) assert false
    | Bisub{size;nsw;nuw;nusw} -> (* T.bisub ~size ~nsw ~nuw *) assert false
    | Bimul{size;nsw;nuw} -> (* T.bimul ~size ~nsw ~nuw *) assert false      
    | Bisdiv(size) -> T.bisdiv ~size
    | Bismod(size) -> T.bismod ~size
    | Biudiv(size) -> T.biudiv ~size
    | Biumod(size) -> T.biumod ~size
    | Bshl{size;nsw;nuw} -> (* T.bshl ~size *) assert false
    | Bashr(size) -> T.bashr ~size
    | Blshr(size) -> T.blshr ~size
    | Band(size) -> T.band ~size
    | Bor(size) -> T.bor ~size
    | Bxor(size) -> T.bxor ~size
    | Beq(size) -> T.beq ~size
    | Bisle(size) -> T.bisle ~size
    | Biule(size) -> T.biule ~size
    | Buext(size) -> T.buext ~size
    | Bsext(size) -> T.bsext ~size
    | Bconcat(size1,size2) -> T.bconcat ~size1 ~size2
    | Bextract{size;index;oldsize} -> T.bextract ~size ~index ~oldsize
    | Bofbool _ -> assert false
    | Bchoose _ -> assert false
    | Bunion _ -> assert false
    | BoolUnion _ -> assert false            
    | Iconst k -> T.iconst k
    | Itimes k -> T.itimes k
    | Iadd -> T.iadd
    | Imul -> T.imul
    | Idiv -> T.idiv
    | Imod -> T.imod
    | Ishl -> T.ishl
    | Ishr -> T.ishr
    | Iand -> T.iand
    | Ior -> T.ior
    | Ixor -> T.ixor
    | Isub -> T.isub
    | Ieq -> T.ieq
    | Ile -> T.ile
  ;;
  
end

(* Idem; it is probably better if eval matches the term together with
   its argument, and directly known the type of the arguments. *)
module Unused_Eval_Forward(T:sig
    type 'a t
    type 'a pack
    
    val extract2: ('a,'b) ar2 pack -> 'a t * 'b t
    val extract1: ('a) ar1 pack -> 'a t

    val true_:  boolean t
    val false_:  boolean t
    val (&&): boolean t -> boolean t -> boolean t
    val (||): boolean t -> boolean t -> boolean t
    val not: boolean t -> boolean t

    val biconst: size:size -> z_t ->  binary t
    val bitimes: size:size -> z_t -> binary t -> binary t
    val biadd: size:size -> binary t -> binary t -> binary t
    val bisub: size:size -> binary t -> binary t -> binary t
    val bimul: size:size -> binary t -> binary t -> binary t
    val bisdiv: size:int -> binary t -> binary t -> binary t
    val bismod: size:int -> binary t -> binary t -> binary t
    val biudiv: size:int -> binary t -> binary t -> binary t
    val biumod: size:int -> binary t -> binary t -> binary t

    val bshl: size:int -> binary t -> binary t -> binary t
    val bashr: size:int -> binary t -> binary t -> binary t
    val blshr: size:int -> binary t -> binary t -> binary t

    val band: size:int -> binary t -> binary t -> binary t
    val bor: size:int -> binary t -> binary t -> binary t
    val bxor: size:int -> binary t -> binary t -> binary t

    val beq: size:size -> binary t -> binary t -> boolean t
    val bisle: size:size -> binary t -> binary t -> boolean t
    val biule: size:size -> binary t -> binary t -> boolean t

    val bconcat: size1:int -> size2:int -> binary t -> binary t -> binary t
    val bextract: size:int -> index:int -> oldsize:int -> binary t -> binary t

    val buext: size:int -> binary t -> binary t
    val bsext: size:int -> binary t -> binary t

    val iconst: z_t -> integer t
    val itimes: z_t -> integer t -> integer t
    val iadd: integer t -> integer t -> integer t
    val imul: integer t -> integer t -> integer t
    val idiv: integer t -> integer t -> integer t
    val imod: integer t -> integer t -> integer t
    val ishl: integer t -> integer t -> integer t
    val ishr: integer t -> integer t -> integer t
    val iand: integer t -> integer t -> integer t
    val ior: integer t -> integer t -> integer  t
    val ixor: integer t -> integer t -> integer t
    val isub: integer t -> integer t -> integer t
    val ieq: integer t -> integer t -> boolean  t
    val ile: integer t -> integer t -> boolean  t
  end) = struct

  let eval:type arg res. (arg,res) term -> arg T.pack -> res T.t = function
    | True -> fun _ -> T.true_
    | False -> fun _ -> T.false_
    | And -> fun arg -> let (b1,b2) = T.extract2 arg in T.(&&) b1 b2
    | Or -> fun arg -> let (b1,b2) = T.extract2 arg in T.(||) b1 b2
    | Not -> fun arg -> T.not @@ T.extract1 arg
    | Biconst(size,k) -> fun _ -> T.biconst ~size k
    | Biadd{size;nsw;nuw;nusw} -> fun arg -> let (b1,b2) = T.extract2 arg in (* T.biadd ~size b1 b2 *) assert false
    | Bisub{size;nsw;nuw;nusw} -> fun arg -> let (b1,b2) = T.extract2 arg in (* T.bisub ~size b1 b2 *) assert false
    | Bimul{size;nsw;nuw} -> fun arg -> let (b1,b2) = T.extract2 arg in (* T.bimul ~size b1 b2 *) assert false
    | Bisdiv(size) -> fun arg -> let (b1,b2) = T.extract2 arg in T.bisdiv ~size b1 b2
    | Bismod(size) -> fun arg -> let (b1,b2) = T.extract2 arg in T.bismod ~size b1 b2
    | Biudiv(size) -> fun arg -> let (b1,b2) = T.extract2 arg in T.biudiv ~size b1 b2
    | Biumod(size) -> fun arg -> let (b1,b2) = T.extract2 arg in T.biumod ~size b1 b2
    | Bshl{size;nsw;nuw} -> fun arg -> let (b1,b2) = T.extract2 arg in (* T.bshl ~size b1 b2 *) assert false
    | Bashr(size) -> fun arg -> let (b1,b2) = T.extract2 arg in T.bashr ~size b1 b2
    | Blshr(size) -> fun arg -> let (b1,b2) = T.extract2 arg in T.blshr ~size b1 b2
    | Band(size) -> fun arg -> let (b1,b2) = T.extract2 arg in T.band ~size b1 b2
    | Bor(size) -> fun arg -> let (b1,b2) = T.extract2 arg in T.bor ~size b1 b2
    | Bxor(size) -> fun arg -> let (b1,b2) = T.extract2 arg in T.bxor ~size b1 b2
    | Beq(size) -> fun arg -> let (b1,b2) = T.extract2 arg in T.beq ~size b1 b2
    | Bisle(size) -> fun arg -> let (b1,b2) = T.extract2 arg in T.bisle ~size b1 b2
    | Biule(size) -> fun arg -> let (b1,b2) = T.extract2 arg in T.biule ~size b1 b2
    | Buext(size) -> fun arg -> T.buext ~size @@ T.extract1 arg
    | Bsext(size) -> fun arg -> T.bsext ~size @@ T.extract1 arg
    | Bconcat(size1,size2) -> fun arg -> let (b1,b2) = T.extract2 arg in T.bconcat ~size1 ~size2 b1 b2
    | Bextract{size;index;oldsize} -> fun arg -> T.bextract ~size ~index ~oldsize @@ T.extract1 arg
    | Bofbool(size) -> assert false
    | Bchoose(id,size) -> assert false
    | Bunion(id,size) -> assert false      
    | Iconst k -> fun _ -> T.iconst k
    | Itimes k -> fun arg -> T.itimes k @@ T.extract1 arg
    | Iadd -> fun arg -> let (i1,i2) = T.extract2 arg in T.iadd i1 i2
    | Imul -> fun arg -> let (i1,i2) = T.extract2 arg in T.imul i1 i2
    | Idiv -> fun arg -> let (i1,i2) = T.extract2 arg in T.idiv i1 i2
    | Imod -> fun arg -> let (i1,i2) = T.extract2 arg in T.imod i1 i2
    | Ishl -> fun arg -> let (i1,i2) = T.extract2 arg in T.ishl i1 i2
    | Ishr -> fun arg -> let (i1,i2) = T.extract2 arg in T.ishr i1 i2
    | Iand -> fun arg -> let (i1,i2) = T.extract2 arg in T.iand i1 i2
    | Ior  -> fun arg -> let (i1,i2) = T.extract2 arg in T.ior  i1 i2
    | Ixor -> fun arg -> let (i1,i2) = T.extract2 arg in T.ixor i1 i2
    | Isub -> fun arg -> let (i1,i2) = T.extract2 arg in T.isub i1 i2
    | Ieq  -> fun arg -> let (i1,i2) = T.extract2 arg in T.ieq  i1 i2
    | Ile  -> fun arg -> let (i1,i2) = T.extract2 arg in T.ile  i1 i2
end

  
(* Smartcons could be the home to a generic implementation of Smart
   constructors, independent from the data attached to type t. 

   One issue is how to handle conditions; e.g. 0 * (1/x) can be
   rewritten to 0 only if x is non-zero. Maybe we should be combining
   the "conditions" attached to each argument, if there are some. *)
module Unused_SmartCons(M:sig
    type 'a t
    val destruct: 'b t -> ('a,'b) term * 'a t
    val construct: ('a,'b) term -> 'a t -> 'b t
    val split_boolean_boolean: (boolean, boolean) ar2 t -> boolean t * boolean t
    val combine_boolean_boolean: boolean t -> boolean t -> (boolean, boolean) ar2 t
  end) = struct

  let (&&): type a. boolean M.t -> boolean M.t -> boolean M.t = fun a b ->
    match M.destruct a with
    | (True,_) -> b
    | _ -> match M.destruct b with
      | (True,_) -> a
      | _ -> M.construct And @@ M.combine_boolean_boolean a b
  ;;

end

