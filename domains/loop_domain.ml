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

module Log = Tracelog.Make(struct let category = "Domains.Loop" end);;
module TC = Operator.Function_symbol

module In_bits = Units.In_bits

(* let index_size = 64 ;; *)
let index_size = In_bits.of_int 32

module Make
    (Terms: Terms.Sig.TERMS)
    (Sub: Sig.BASE
      with type binary = TC.binary Terms.t
       and type boolean = TC.boolean Terms.t
       and type enum = TC.enum Terms.t)
  : Sig.BASE = struct

  let name () = "Loop_Domain(" ^ Sub.name () ^ ")";;
  let unique_id () = Sig.Fresh_id.fresh @@ name ();;

  module Types = struct
    type binary = Sub.binary
    type boolean = Sub.boolean
    type enum = Sub.enum
  end

  include Types

  module Binary = Sub.Binary
  module Boolean = Sub.Boolean
  module Enum = Sub.Enum

  type context =
  {
    subcontext: Sub.Context.t ;
    index: binary option
  };;

  module Context = struct
    type t = context
    let copy x = { x with subcontext=Sub.Context.copy x.subcontext }
    let assign ctx1 ctx2 =
      Sub.Context.assign ctx1.subcontext ctx2.subcontext

    let level ctx = Sub.Context.level ctx.subcontext

    type 'a in_tuple = 'a Sub.Context.in_tuple
    type 'a in_acc = bool * 'a Sub.Context.in_tuple

    (* We reconstruct the identifiers on-demand. MAYBE: have the bottom case. *)
    type 'a out_tuple = 'a Sub.Context.out_tuple

    type ('a,'b) result =
      Result: bool * 'some in_tuple * (t -> 'some out_tuple -> 'a * 'b out_tuple) -> ('a,'b) result

    type empty_tuple = Sub.Context.empty_tuple
    let empty_tuple () = Sub.Context.empty_tuple ()

  end
  open Context

  let root_context () =
    {
      subcontext = Sub.root_context () ;
      index = None
    }
  ;;

  let context_pretty fmt ctx =
    match ctx.index with
    | None -> Format.fprintf fmt "Context{sub=%a}" Sub.context_pretty ctx.subcontext
    | Some idx -> Format.fprintf fmt "Context{sub=%a,index=%a}" Sub.context_pretty ctx.subcontext (Sub.binary_pretty ~size:index_size ctx.subcontext) idx

  (* include Operator.Builtin.Make(Types)(Context) *)

  let assume ctx cond =
    Option.map (fun subctx -> {ctx with subcontext = subctx}) (Sub.assume ctx.subcontext cond)

  module Boolean_Forward = struct
    let (||) ctx = Sub.Boolean_Forward.(||) ctx.subcontext
    let (&&) ctx = Sub.Boolean_Forward.(&&) ctx.subcontext
    let not ctx = Sub.Boolean_Forward.not ctx.subcontext
    (* Note: we avoid creating those every time. *)
    let true_ ctx = Sub.Boolean_Forward.true_ ctx.subcontext
    let false_ ctx = Sub.Boolean_Forward.false_ ctx.subcontext
  end

  module Binary_Forward = struct

    let biadd ~size ~flags ctx = Sub.Binary_Forward.biadd ~size ~flags ctx.subcontext
    let bisub ~size ~flags ctx = Sub.Binary_Forward.bisub ~size ~flags ctx.subcontext
    let bimul ~size ~flags ctx = Sub.Binary_Forward.bimul ~size ~flags ctx.subcontext
    let bxor ~size ctx = Sub.Binary_Forward.bxor ~size ctx.subcontext
    let band ~size ctx = Sub.Binary_Forward.band ~size ctx.subcontext
    let bor ~size ctx = Sub.Binary_Forward.bor ~size ctx.subcontext
    let bashr ~size ctx = Sub.Binary_Forward.bashr ~size ctx.subcontext
    let blshr ~size ctx = Sub.Binary_Forward.blshr ~size ctx.subcontext
    let bshl ~size ~flags ctx = Sub.Binary_Forward.bshl ~size ~flags ctx.subcontext

    let bisdiv ~size ctx = Sub.Binary_Forward.bisdiv ~size ctx.subcontext
    let biudiv ~size ctx = Sub.Binary_Forward.biudiv ~size ctx.subcontext
    let bismod ~size ctx = Sub.Binary_Forward.bismod ~size ctx.subcontext
    let biumod ~size ctx = Sub.Binary_Forward.biumod ~size ctx.subcontext

    let beq ~size ctx = Sub.Binary_Forward.beq ~size ctx.subcontext

    let biule ~size ctx = Sub.Binary_Forward.biule ~size ctx.subcontext
    let bisle ~size ctx = Sub.Binary_Forward.bisle ~size ctx.subcontext

    let bsext ~size ~oldsize ctx = Sub.Binary_Forward.bsext ~size ~oldsize ctx.subcontext
    let buext ~size ~oldsize ctx = Sub.Binary_Forward.buext ~size ~oldsize ctx.subcontext
    let bchoose ~size cond ctx = Sub.Binary_Forward.bchoose ~size cond ctx.subcontext
    let bofbool ~size ctx = Sub.Binary_Forward.bofbool ~size ctx.subcontext
    let bconcat ~size1 ~size2 ctx = Sub.Binary_Forward.bconcat ~size1 ~size2 ctx.subcontext
    let bextract ~size ~index ~oldsize ctx = Sub.Binary_Forward.bextract ~size ~index ~oldsize ctx.subcontext
    let biconst ~size k ctx = Sub.Binary_Forward.biconst ~size k ctx.subcontext
    let buninit ~size  = assert false
    let bshift ~size ~offset ~max _ = assert false
    let bindex ~size _ = assert false
    let valid ~size _ = assert false
    let valid_ptr_arith ~size _ = assert false
  end

  module Enum_Forward = struct
    let caseof ~case ctx = Sub.Enum_Forward.caseof ~case ctx.subcontext
    let enum_const ~case ctx = Sub.Enum_Forward.enum_const ~case ctx.subcontext
  end

  let boolean_empty ctx = Sub.boolean_empty ctx.subcontext
  let binary_empty ~size ctx = Sub.binary_empty ~size ctx.subcontext
  let enum_empty ctx = Sub.enum_empty ctx.subcontext

  let boolean_unknown ctx = Sub.boolean_unknown ctx.subcontext
  let binary_unknown ~size ctx = Sub.binary_unknown ~size ctx.subcontext
  let enum_unknown ~enumsize ctx = Sub.enum_unknown ~enumsize ctx.subcontext

  let boolean_pretty ctx fmt x = Sub.boolean_pretty ctx.subcontext fmt x
  let binary_pretty ~size ctx fmt x = Sub.binary_pretty ~size ctx.subcontext fmt x
  let enum_pretty ctx fmt x = Sub.enum_pretty ctx.subcontext fmt x



  let serialize_binary ~widens ~size ctxa a ctxb b acc =
    let Sub.Context.Result (included, in_tup, deserialize) = Sub.serialize_binary ~widens ~size ctxa.subcontext a ctxb.subcontext b acc in
    Context.Result (included, in_tup, (fun ctx out_tup -> deserialize ctx.subcontext out_tup))

  let pretty_index fmt ctx =
    match ctx.index with
    | None -> Format.fprintf fmt "None"
    | Some v -> binary_pretty ~size:index_size ctx fmt v

  (* TODO : look at the way arithmetic flags are used to unsure the absence of unsoundness *)
  let serialize_binary ~widens ~(size:In_bits.t) ctxa a ctxb b ((inc, tup) as acc) =
    Log.debug (fun p -> p "Loop_domain.serialize_binary ~widens:%b ~size:%d %a %a with index = %a" widens (size:>int)
      (binary_pretty ~size ctxa) a
      (binary_pretty ~size ctxb) b
      (fun fmt index -> match index with None -> Format.fprintf fmt "None" | Some idx -> (binary_pretty ~size ctxa fmt) idx) ctxa.index);
    if size <> index_size then serialize_binary ~widens ~size ctxa a ctxb b acc
    else if not widens then serialize_binary ~widens ~size ctxa a ctxb b acc
    else
    let cur_level = Context.level ctxa in
    match ctxa.index with
    (* Case 1 : First iteration of the loop *)
    | Some (Terms.Binary{term=T0{tag=TC.Biconst(_size,k)}}) when (* Z.equal Z.one k *) Z.equal Z.zero k ->
      begin
        match a,b with
        (* case 1 : with prev_index = 0, base \cup (offset + base) = ((offset * index) + base) *)
        | Terms.(Binary _ as x),
          Terms.(Binary{term=T2{tag=TC.Biadd{size=size';flags};
                                      a=Binary{term=T0{tag=TC.Biconst(_size2,k)}};
                                      b=Binary _ as y}})
            when Terms.equal x y
            && (Terms.level x) < cur_level ->
              Log.debug (fun p -> p "in Loop_domain.serialize, applying substitution 1");
              Result (inc, tup, (fun ctx out ->
                match ctx.index with
                | Some idx ->
                  let offset = Binary_Forward.biconst ~size:index_size k ctx in
                  let offset = Binary_Forward.bimul ~size:index_size
                      ~flags:(Operator.Flags.Bimul.pack ~nsw:false ~nuw:false) ctx offset idx in
                  let res = Binary_Forward.biadd ~size:index_size ~flags ctx offset x in
                  res, out

                | _ -> assert false
              ))

        (* case 2 : with prev_index = 0, base \cup (base - offset) = base - (offset * index) *)
        | Terms.(Binary _ as x),
          Terms.(Binary{term=T2{tag=TC.Bisub{size=size';flags=flagssub};
                                      a=Binary _ as y;
                                      b=Binary{term=T0{tag=TC.Biconst(_size2,k)}}}})
            when Terms.equal x y
            && (Terms.level x) < cur_level ->
              Log.debug (fun p -> p "in Loop_domain.serialize, applying substitution 2");
              Result (inc, tup, (fun ctx out ->
                match ctx.index with
                | Some idx ->
                  Log.debug (fun p -> p "while, applying substitution 2, index = %a" (binary_pretty ~size ctx) idx);
                  let flags = Operator.Flags.Bimul.pack ~nsw:false ~nuw:false in
                  let offset = Binary_Forward.biconst ~size:index_size k ctx in
                  let offset = Binary_Forward.bimul ~size:index_size ~flags ctx offset idx in
                  let res = Binary_Forward.bisub ~size:index_size ~flags:flagssub ctx x offset in
                  Log.debug (fun p -> p "Loop_domain.serialize_binary, returning %a" Binary.pretty res);
                  res, out

                | _ -> assert false
              ))

        (* case 7 & 8 : with constant values *)
        | Terms.(Binary{term=T0{tag=TC.Biconst(_size1, k)}}),
          Terms.(Binary{term=T0{tag=TC.Biconst(_size2, l)}})
          when not @@ Z.equal k l ->
            let l = Z.signed_extract l 0 (index_size:>int) in
            if Z.leq k l then (
              Log.debug (fun p -> p "in Loop_domain.serialize, applying substitution 7");
              Result (inc, tup, (fun ctx out ->
                match ctx.index with
                | Some idx ->
                  let base = Binary_Forward.biconst ~size:index_size k ctx in
                  let offset = Binary_Forward.biconst ~size:index_size (Z.sub l k) ctx in
                  let offset = Binary_Forward.bimul ~size:index_size
                      ~flags:(Operator.Flags.Bimul.pack ~nsw:false ~nuw:false) ctx idx offset in
                  let res = Binary_Forward.biadd ~size:index_size
                      ~flags:(Operator.Flags.Biadd.pack ~nsw:false ~nuw:false ~nusw:false) ctx base offset in
                  res, out

                | _ -> assert false
              )))

            else (
              Log.debug (fun p -> p "in Loop_domain.serialize, applying substitution 8");
              Result (inc, tup, (fun ctx out ->
                match ctx.index with
                | Some idx ->
                  let base = Binary_Forward.biconst ~size:index_size k ctx in
                  let offset = Binary_Forward.biconst ~size:index_size (Z.sub k l) ctx in
                  let offset = Binary_Forward.bimul ~size:index_size
                      ~flags:(Operator.Flags.Bimul.pack ~nsw:false ~nuw:false) ctx offset idx in
                  let res = Binary_Forward.bisub ~size:index_size ~flags:(Operator.Flags.Bisub.pack ~nsw:false ~nuw:false ~nusw:false) ctx base offset in
                  res, out

                | _ -> assert false
              )))

        | _ ->
          Log.debug (fun p -> p "in Loop_domain.serialize, applying no substituation") ;
          serialize_binary ~widens ~size ctxa a ctxb b acc

      end

    (* Case 2 : Subsequent iteration of the loop *)
    | Some previdx ->
      begin
        match a,b with

        (* case 3 : ((offset * prev_index) + base) \cup (offset + ((offset * prev_index) + base) = ((offset * index) + base) *)
        | Terms.(Binary{term=T2{tag=TC.Biadd{size=_size1;flags=flags1};
                                    a=Binary{term=T2{tag=TC.Bimul _size2;
                                        a=Binary{term=T0{tag=TC.Biconst(_size3, i)}};
                                        b=Binary _ as u
                                      }};
                                    b=Binary _ as x;
                                    }}),
          Terms.(Binary{term=T2{tag=TC.Biadd{size=_size4;flags=flags4};
                                    a=Binary{term=T0{tag=TC.Biconst(_size5, k)}};
                                    b=Binary{term=T2{tag=TC.Biadd{size=_size6;flags=flags6};
                                        a=Binary{term=T2{tag=TC.Bimul{size=_size7;flags=flags7};
                                            a=Binary{term=T0{tag=TC.Biconst(_size8, j)}};
                                            b=Binary _ as v
                                          }};
                                        b=Binary _ as y
                                      }};
                                  }})
          when Z.equal i j && Z.equal i k
          && Terms.equal x y && Terms.equal u v && Terms.equal u previdx
          && (Terms.level x) < cur_level
          && flags1 = flags4 ->
            Log.debug (fun p -> p "in Loop_domain.serialize, applying substitution 3");
            Result (inc, tup, (fun ctx out ->
              match ctx.index with
              | Some idx ->
                let offset = Binary_Forward.biconst ~size:index_size i ctx in
                let offset = Binary_Forward.bimul ~size:index_size ~flags:flags7 ctx offset idx in
                let res = Binary_Forward.biadd ~size:index_size ~flags:flags1 ctx offset x in
                res, out

              | _ -> assert false
            ))

        (* case 4 : (base - (offset * prev_index)) \cup ((base - (offset * prev_index)) - offset) = (base - (offset * index)) *)
        | Terms.(Binary{term=T2{tag=TC.Bisub {size=_size1;flags=flags1};
                                    a=Binary _ as x;
                                    b=Binary{term=T2{tag=TC.Bimul {size=_size2;flags=flags2};
                                        a=Binary{term=T0{tag=TC.Biconst(_size3, i)}};
                                        b=Binary _ as u}
                                      }}
                                  }),
          Terms.(Binary{term=T2{tag=TC.Bisub {size=_size4;flags=flags4};
                                    a=Binary{term=T2{tag=TC.Bisub {size=_size5;flags=flags5};
                                        a=Binary _ as y;
                                        b=Binary{term=T2{tag=TC.Bimul {size=_size6;flags=flags6};
                                            a=Binary{term=T0{tag=TC.Biconst(_size7, j)}};
                                            b=Binary _ as v
                                          }}
                                        }};
                                    b=Binary{term=T0{tag=TC.Biconst(_size8, k)}}}
                                  })
          when Z.equal i j && Z.equal i k
          && Terms.equal x y && Terms.equal u v && Terms.equal u previdx
          && (Terms.level x) < cur_level
          && flags1 = flags4 && flags2 = flags6 ->
            Log.debug (fun p -> p "in Loop_domain.serialize, applying substitution 4");
            Result (inc, tup, (fun ctx out ->
              match ctx.index with
              | Some idx ->
                let offset = Binary_Forward.biconst ~size:index_size i ctx in
                let offset = Binary_Forward.bimul ~size:index_size ~flags:flags2 ctx offset idx in
                let res = Binary_Forward.bisub ~size:index_size ~flags:flags1 ctx x offset in
                res, out

              | _ -> assert false
            ))

        (* case 5 : (prev_index + base) \cup (1 + (prev_index + base)) = (index + base) *)
        | Terms.(Binary{term=T2{tag=TC.Biadd {size=_size1;flags=flags1};
                                    a=Binary _ as u;
                                    b=Binary _ as x }}),
          Terms.(Binary{term=T2{tag=TC.Biadd {size=_size2;flags=flags2};
                                    a=Binary{term=T0{tag=TC.Biconst(_size3, k)}};
                                    b=Binary{term=T2{tag=TC.Biadd {size=_size4;flags=flags4};
                                        a=Binary _ as v;
                                        b=Binary _ as y
                                      }}
                                  }})
          when Z.equal k Z.one
          && Terms.equal x y && Terms.equal u v && Terms.equal u previdx
          && (Terms.level x) < cur_level
          && flags1 = flags2 && flags1 = flags4 ->
            Log.debug (fun p -> p "in Loop_domain.serialize, applying substitution 5");
            Result (inc, tup, (fun ctx out ->
              match ctx.index with
              | Some idx ->
                let res = Binary_Forward.biadd ~size:index_size ~flags:flags1 ctx idx x in
                res, out

              | _ -> assert false
            ))

        (* case 6 : (base - prev_index) \cup (base - prev_index - 1) = (base - index) *)
        | Terms.(Binary{term=T2{tag=TC.Bisub {size=_size1;flags=flags1};
                                    a=Binary _ as x;
                                    b=Binary _ as u }}),
          Terms.(Binary{term=T2{tag=TC.Bisub {size=_size2;flags=flags2};
                                    a=Binary{term=T2{tag=TC.Bisub {size=_size3;flags=flags3};
                                        a=Binary _ as y;
                                        b=Binary _ as v
                                      }};
                                    b=Binary{term=T0{tag=TC.Biconst(_size4, k)}}
                                  }})
          when Z.equal k Z.one
          && Terms.equal x y && Terms.equal u v && Terms.equal u previdx
          && (Terms.level x) < cur_level
          && flags1 = flags2 && flags1 = flags3 ->
            Log.debug (fun p -> p "in Loop_domain.serialize, applying substitution 6");
            Result (inc, tup, (fun ctx out ->
              match ctx.index with
              | Some idx ->
                let res = Binary_Forward.bisub ~size:index_size ~flags:flags1 ctx x idx in
                res, out

              | _ -> assert false
            ))

        (* case 9 : (offset * prev_index) \cup (offset + (offset * prev_index)) = (offset * index) (with base = 0) *)
        | Terms.(Binary{term=T2{tag=TC.Bimul {size=_size1;flags=flags1};
                                    a=Binary{term=T0{tag=TC.Biconst(_size2, i)}};
                                    b=Binary _ as x}}),
          Terms.(Binary{term=T2{tag=TC.Biadd {size=_size3;flags=flags3};
                                    a=Binary{term=T0{tag=TC.Biconst(_size4, j)}};
                                    b=Binary{term=T2{tag=TC.Bimul {size=_size5;flags=flags5};
                                        a=Binary{term=T0{tag=TC.Biconst(_size6, k)}};
                                        b=Binary _ as y
                                      }};
                                  }})
          when Terms.equal x y && Terms.equal x previdx && Z.equal i j && Z.equal i k
          && flags1 = flags5 ->
            Log.debug (fun p -> p "in Loop_domain.serialize, applying substitution 9");
            Result (inc, tup, (fun ctx out ->
              match ctx.index with
              | Some idx ->
                let offset = Binary_Forward.biconst ~size:index_size i ctx in
                let res = Binary_Forward.bimul ~size:index_size ~flags:flags5 ctx offset idx in
                res, out

              | _ -> assert false
            ))

        (* case 10 : 0 - (prev_index * offset) \cup 0 - (prev_index * offset) - offset = 0 - (index * offset) *)
        (* TODO : check if it is necessary *)

        (* case 11 : prev_index \cup 1 + prev_index = index (when offset = 1 & base = 0) *)
        | Terms.(Binary _ as x),
          Terms.(Binary{term=T2{tag=TC.Biadd {size=_size;flags};
                                    a=Binary{term=T0{tag=TC.Biconst(_size2, k)}};
                                    b=Binary _ as y
                                }})
          when Z.equal k Z.one
          && Terms.equal x y && Terms.equal x previdx ->
            Log.debug (fun p -> p "in Loop_domain.serialize, applying substitution 11");
            Result (inc, tup, (fun ctx out ->
              match ctx.index with
              | Some idx -> idx, out

              | _ -> assert false
            ))

        (* case 12 : 0 - prev_index \cup 0 - prev_index - 1 = 0 - index *)
        (* TODO : check if it is necessary *)

        (* generally for constant value bases *)
        (* case 13 : (base + (offset * prev_index)) \cup (offset + (base + (offset + base))) = ((offset * index) + base) *)
        | Terms.(Binary{term=T2{tag=TC.Biadd {size=_size1;flags=flags1};
                                    a=Binary _ as x;
                                    b=Binary{term=T2{tag=TC.Bimul {size=_size2;flags=flags2};
                                        a=Binary {term=T0{tag=TC.Biconst(_size3, i)}};
                                        b=Binary _ as u
                                      }}
                                    }}),
          Terms.(Binary{term=T2{tag=TC.Biadd {size=_size4;flags=flags4};
                                    a=Binary{term=T0{tag=TC.Biconst(_size5, k)}};
                                    b=Binary{term=T2{tag=TC.Biadd {size=_size6;flags=flags6};
                                        a=Binary _ as y;
                                        b=Binary{term=T2{tag=TC.Bimul {size=_size7;flags=flags7};
                                            a=Binary{term=T0{tag=TC.Biconst(_size8, j)}};
                                            b=Binary _ as v
                                          }}
                                      }};
                                  }})
          when Z.equal i j && Z.equal i k
          && Terms.equal x y && Terms.equal u v && Terms.equal u previdx
          && (Terms.level x) < cur_level
          && flags1 = flags4 && flags1 = flags6
          && flags2=flags7 ->
            Log.debug (fun p -> p "in Loop_domain.serialize, applying substitution 13");
            Result (inc, tup, (fun ctx out ->
              match ctx.index with
              | Some idx ->
                let offset = Binary_Forward.biconst ~size:index_size i ctx in
                let offset = Binary_Forward.bimul ~size:index_size ~flags:flags2 ctx offset idx in
                let res = Binary_Forward.biadd ~size:index_size ~flags:flags1 ctx offset x in
                res, out

              | _ -> assert false
            ))

        (* case 14 : (base + prev_index) \cup (1 + (base + prev_index)) = (base + index) *)
        | Terms.(Binary{term=T2{tag=TC.Biadd {size=_size1;flags=flags1};
                                    a=Binary _ as x;
                                    b=Binary _ as u }}),
          Terms.(Binary{term=T2{tag=TC.Biadd {size=_size2;flags=flags2};
                                    a=Binary{term=T0{tag=TC.Biconst(_size3, k)}};
                                    b=Binary{term=T2{tag=TC.Biadd {size=_size4;flags=flags4};
                                        a=Binary _ as y;
                                        b=Binary _ as v
                                      }}
                                  }})
          when Z.equal k Z.one
          && Terms.equal x y && Terms.equal u v && Terms.equal u previdx
          && (Terms.level x) < cur_level
          && flags1 = flags2 && flags1 = flags4 ->
            Log.debug (fun p -> p "in Loop_domain.serialize, applying substitution 14");
            Result (inc, tup, (fun ctx out ->
              match ctx.index with
              | Some idx ->
                let res = Binary_Forward.biadd ~size:index_size ~flags:(Operator.Flags.Biadd.pack ~nsw:false ~nuw:false ~nusw:false) ctx x idx in
                res, out

              | _ -> assert false
            ))


        | _ ->
          Log.debug (fun p -> p "in Loop_domain.serialize, applying no substituation") ;
          serialize_binary ~widens ~size ctxa a ctxb b acc

      end

    | _ ->
      Log.debug (fun p -> p "in Loop_domain.serialize, unable to substitute without an adequate loop index") ;
      serialize_binary ~widens ~size ctxa a ctxb b acc


  let serialize_boolean ctxa a ctxb b acc =
    let Sub.Context.Result (included, in_tup, deserialize) = Sub.serialize_boolean ctxa.subcontext a ctxb.subcontext b acc in
    Context.Result (included, in_tup, (fun ctx out_tup -> deserialize ctx.subcontext out_tup))

  let serialize_enum ctxa a ctxb b acc =
    let Sub.Context.Result (included, in_tup, deserialize) = Sub.serialize_enum ctxa.subcontext a ctxb.subcontext b acc in
    Context.Result (included, in_tup, (fun ctx out_tup -> deserialize ctx.subcontext out_tup))

  (**************** Nondet and union. ****************)

  let nondet_same_context ctx in_tup = Sub.nondet_same_context ctx.subcontext in_tup

  let union cond ctx in_tup = Sub.union cond ctx.subcontext in_tup

  let typed_nondet2 ctxa ctxb in_tup =
    match ctxa.index, ctxb.index with
    | Some ia, Some ib when Terms.equal ia ib ->
      let subctx, out = Sub.typed_nondet2 ctxa.subcontext ctxb.subcontext in_tup in
      {subcontext = subctx; index = Some ia}, out
    | _ ->
      let subctx, out = Sub.typed_nondet2 ctxa.subcontext ctxb.subcontext in_tup in
      {subcontext = subctx; index = None}, out

  let typed_fixpoint_step ~iteration ~init ~arg ~body (inc, tup) =
    let bool, continuef = Sub.typed_fixpoint_step ~iteration ~init:init.subcontext ~arg:arg.subcontext ~body:body.subcontext (inc, tup) in
    let continuef ~close =
      let out, ctx = continuef ~close in
      out, {subcontext = ctx; index = None}
    in bool, continuef

  let typed_fixpoint_step ~iteration ~init ~arg ~body ((inc, tup) : bool * 'a in_tuple) : bool * (close:bool -> 'a out_tuple * Context.t)  =
    match arg.index with
    | Some idx ->
        let one = Sub.Binary_Forward.biconst ~size:index_size Z.one init.subcontext in
        let next_idx = Sub.Binary_Forward.biadd ~size:index_size ~flags:(Operator.Flags.Biadd.pack ~nsw:false ~nuw:false ~nusw:false) body.subcontext idx one in (* TODO : we should probably use flag "nuw" *)
        let Sub.Context.Result(inc, tup, deserialize) = Sub.serialize_binary ~widens:true ~size:index_size arg.subcontext idx body.subcontext next_idx (inc, tup) in
        let bool, continuef = Sub.typed_fixpoint_step ~iteration ~init:init.subcontext ~arg:arg.subcontext ~body:body.subcontext (inc, tup) in
        let continuef ~close =
          let out, ctx = continuef ~close in
          let new_index, out = deserialize ctx out in
          out, {subcontext = ctx; index = Some new_index}
        in bool, continuef

    | _ -> typed_fixpoint_step ~iteration ~init ~arg ~body (inc, tup)


  let widened_fixpoint_step ~widening_id ~previous ~next = assert false


  let mu_context_open parent_ctx =
    let subctx = Sub.mu_context_open parent_ctx.subcontext in
    let zero = Sub.Binary_Forward.biconst ~size:index_size Z.zero parent_ctx.subcontext in
    {subcontext = subctx; index = Some zero}

  module Query = struct
    include Sub.Query

    let binary ~size ctx = Sub.Query.binary ~size ctx.subcontext
    let enum ctx = Sub.Query.enum ctx.subcontext
  end

  let query_boolean ctx b = Sub.query_boolean ctx.subcontext b

  let assume_binary ~size = assert false
  let satisfiable ctx cond = Sub.satisfiable ctx.subcontext cond
  let binary_unknown_typed ~size ctx typ = Sub.binary_unknown_typed ~size ctx.subcontext typ

end
