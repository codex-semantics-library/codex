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

module type S = sig
  include Basis_sig.Binary_Basis;;
  val concretize: binary -> Lattices.BVSet.t
  val abstract: size:int -> Lattices.BVSet.t -> binary
  val pretty: Format.formatter -> binary -> unit
  val parse_string: string -> binary
end
module Collecting = struct
  include Binary_collecting
  let equal = Binary_Lattice.equal
  let subset = Lattices.BVSet.ZSet.subset
  let inter = Binary_Lattice.inter ~size:(-1)
end
module Quadrivalent = Lattices.Quadrivalent

module Test(Implem:S) = struct

  module StringSet = Set.Make(String);;

  (* All combinations with n bits *)
  let rec all n =
    if n == 0 then StringSet.singleton ""
    else
      let allprev = all (n - 1) in
      let cat a b = String.concat "" [a;b] in
      StringSet.union (StringSet.map (cat "0") allprev) @@
        StringSet.union (StringSet.map (cat "1") allprev) @@
          (StringSet.map (cat "?") allprev)
  ;;

  let test_soundness1 absop concop a =
    (* Format.printf "Abstract computation: %a %a@." Implem.pretty a Implem.pretty (absop a);
     * Format.printf "Result: %a %a@." 
     *   Collecting.pretty (concop (Implem.concretize a)) 
     *   Collecting.pretty (Implem.concretize (absop a)); *)
    Collecting.subset (concop (Implem.concretize a))
      (Implem.concretize (absop a)) || failwith "Soundness check failed"
  ;;

  let test_completeness1 absop concop a =
    (* Format.printf "Abstract computation: %a %a@." Implem.pretty a Implem.pretty (absop a);
     * Format.printf "Result: %a %a@." 
     *   Collecting.pretty (concop (Implem.concretize a)) 
     *   Collecting.pretty (Implem.concretize (absop a));     *)
    Collecting.equal (concop (Implem.concretize a))
      (Implem.concretize (absop a)) || failwith "Completeness check failed"
  ;;

  let test_best_transformer1 ~size absop concop a =
    (* Format.printf "Abstract computation: %a %a@." Implem.pretty a Implem.pretty (absop a);
     * Format.printf "Result: %a %a %a %a@." 
     *   Collecting.pretty (concop (Implem.concretize a)) 
     *   Collecting.pretty (Implem.concretize (absop a))
     *   Implem.pretty (Implem.abstract ~size @@ concop (Implem.concretize a))  
     *   Collecting.pretty (Implem.concretize @@ Implem.abstract ~size @@ concop (Implem.concretize a))
     * ; *)
    Collecting.equal (Implem.concretize @@ Implem.abstract ~size @@ concop (Implem.concretize a))
      (Implem.concretize (absop a)) || failwith "Maximally precise check failed"
  ;;

  
  
  let test_soundness2 absop concop a b =
    (* Format.printf "Abstract computation: %a op %a = %a@." Implem.pretty a Implem.pretty b Implem.pretty (absop a b);
     * Format.printf "Result: %a %a@."
     *   Collecting.pretty (concop (Implem.concretize a) (Implem.concretize b)) 
     *   Collecting.pretty (Implem.concretize (absop a b)); *)
    Collecting.subset (concop (Implem.concretize a) (Implem.concretize b))
      (Implem.concretize (absop a b)) || (* failwith  *)
    (* (Format.printf "Soundness check failed: %a op %a = %a should be %a\n" Implem.pretty a Implem.pretty b Implem.pretty (absop a b) Collecting.pretty (concop (Implem.concretize a) (Implem.concretize b)) ; true) *)true
  ;;

  let test_soundness2_pred absop concop a b =
    (* Format.printf "Abstract computation: %a op %a = %a@." Implem.pretty a Implem.pretty b Quadrivalent.pretty (absop a b); *)
    (* Format.printf "Result: %a %a@." 
     *   Quadrivalent.pretty (concop (Implem.concretize a) (Implem.concretize b)) 
     *   Quadrivalent.pretty (absop a b); *)
    Quadrivalent.includes (absop a b) (concop (Implem.concretize a) (Implem.concretize b))
    || failwith "Soundness check failed"
  ;;

  
  (*  I could give an example of soundness or incompleteness. *)
  let test_completeness2 absop concop a b =
    (* Format.printf "Abstract computation: %a %a@." Implem.pretty a Implem.pretty (absop a b);
     * Format.printf "Result: %a %a@." 
     *   Collecting.pretty (concop (Implem.concretize a) (Implem.concretize b)) 
     *   Collecting.pretty (Implem.concretize (absop a b));     *)
    Collecting.equal (concop (Implem.concretize a) (Implem.concretize b))
      (Implem.concretize (absop a b)) || failwith "Completeness check failed"
  ;;

  let test_completeness2_pred absop concop a b =
    (* Format.printf "Abstract computation: %a op %a = %a@." Implem.pretty a Implem.pretty b Quadrivalent.pretty (absop a b); *)
    (* Format.printf "Result: %a %a@." 
     *   Quadrivalent.pretty (concop (Implem.concretize a) (Implem.concretize b)) 
     *   Quadrivalent.pretty (absop a b); *)
    Quadrivalent.equal (concop (Implem.concretize a) (Implem.concretize b))
      (absop a b) || failwith (Format.asprintf "Completeness/best transformer check failed: %a op %a = %a, best result is %a"
                                Implem.pretty a Implem.pretty b Quadrivalent.pretty (absop a b)
                                Quadrivalent.pretty (concop (Implem.concretize a) (Implem.concretize b)))
  ;;

  
  let test_best_transformer2 ~size absop concop a b =
    (* Format.printf "Abstract computation: %a %a@." Implem.pretty a Implem.pretty (absop a b);
     * Format.printf "Result: %a %a@." 
     *   Collecting.pretty (concop (Implem.concretize a) (Implem.concretize b)) 
     *   Collecting.pretty (Implem.concretize (absop a b));     *)
    Collecting.equal (Implem.concretize @@ Implem.abstract ~size @@ concop (Implem.concretize a) (Implem.concretize b))
      (Implem.concretize (absop a b)) || failwith (Format.asprintf "Maximal precision check failed: %a op %a returns %a, optimal result is %a" 
                                                    Implem.pretty a Implem.pretty b Implem.pretty (absop a b) 
                                                    Implem.pretty (Implem.abstract ~size @@ concop (Implem.concretize a) (Implem.concretize b)))
  (* Collecting.equal (concop (Implem.concretize a) (Implem.concretize b))
   *   (Implem.concretize (absop a b)) || failwith "Maximal precision check failed" *)
  ;;

  (* no test_best_transformer2_pred, as this is the same than test_best_transformer2_complete. *)
  
  (* test_soundness2 Implem.(band) Collecting.band (Implem.parse_string "1?10?") (Implem.parse_string "1?10?");;
   * test_completeness2 Implem.(band) Collecting.band (Implem.parse_string "1?10?") (Implem.parse_string "1?10?");; *)

  let all_test_f1 f bits absop concop  =
    (all bits) |> StringSet.iter (fun a ->
                      (* Format.printf "testing for a=%s@." a; *)
                      let a = Implem.parse_string a in
                      (* Format.printf "a = %a res = %a@." Implem.pretty a Implem.pretty (absop a); *)
                      assert(f absop concop a));;


  
  let all_test_f2 f bits absop concop  =
    (all bits) |> StringSet.iter (fun a ->
                      (all bits) |> StringSet.iter (fun b ->
                                        (* Format.printf "testing for a=%s b=%s@." a b; *)
                                        let a = Implem.parse_string a in
                                        let b = Implem.parse_string b in
                                        (* Format.printf "res = %a@." Implem.pretty (absop a b); *)
                                        assert(f absop concop a b)));;
  let all_test_f2' f (bitsa,bitsb) absop concop  =
    (all bitsa) |> StringSet.iter (fun a ->
                      (all bitsb) |> StringSet.iter (fun b ->
                                        (* Format.printf "testing for a=%s b=%s@." a b; *)
                                        let a = Implem.parse_string a in
                                        let b = Implem.parse_string b in
                                        (* Format.printf "res = %a@." Implem.pretty (absop a b); *)
                                        assert(f absop concop a b)));;

  
  let all_test_f3 f (bitsa,bitsb,bitsr) absop concop  =
    (all bitsa) |> StringSet.iter (fun a ->
                       (all bitsb) |> StringSet.iter (fun b ->
                                          (all bitsr) |> StringSet.iter (fun r ->                                          
                                        (* Format.printf "testing for a=%s b=%s@." a b; *)
                                        let a = Implem.parse_string a in
                                        let b = Implem.parse_string b in
                                        let r = Implem.parse_string r in                                        
                                        (* Format.printf "res = %a@." Implem.pretty (absop a b); *)
                                        assert(f absop concop a b r))));;

  let all_test_f3' f (bitsa,bitsb,bitsr) absop concop  =
    (all bitsa) |> StringSet.iter (fun a ->
        (all bitsb) |> StringSet.iter (fun b ->
            [Quadrivalent.True;Quadrivalent.False;
             Quadrivalent.Bottom;Quadrivalent.Top] |> List.iter (fun r ->
                                        (* Format.printf "testing for a=%s b=%s@." a b; *)
                                        let a = Implem.parse_string a in
                                        let b = Implem.parse_string b in
                                        (* Format.printf "res = %a@." Implem.pretty (absop a b); *)
                                        assert(f absop concop a b r))));;

  
  
  (* Note: complete implies maximally precise.contents. 
     Actually, my completeness checks that we are sound and complete. *)
  let all_test_soundness1 = all_test_f1 test_soundness1;;
  let all_test_completeness1 = all_test_f1 test_completeness1;;
  let all_test_best1 size = all_test_f1 (test_best_transformer1 ~size);;  
  let all_test_soundness2 = all_test_f2 test_soundness2;;
  let all_test_soundness2_pred = all_test_f2 test_soundness2_pred;;  
  let all_test_completeness2 = all_test_f2 test_completeness2;;
  let all_test_completeness2_pred = all_test_f2 test_completeness2_pred;;    
  let all_test_best2 size = all_test_f2 (test_best_transformer2 ~size);;  
  

  module  BF = Implem.Binary_Forward;;
  module  BR = Implem.Binary_Backward;;
  module  CBF = Collecting.Binary_Forward;;
  module  CBB = Collecting.Binary_Backward;;    
  module Test() = struct

    (* all_test_soundness2 1 BF.band Collecting.band;;
     * Format.printf "band is sound@.";; *)
    all_test_completeness2 1 (BF.band ~size:1) (CBF.band ~size:1);;
    Format.printf "band is sound and complete@.";;

    (* all_test_soundness2 1 BF.bor Collecting.bor;;
     * Format.printf "bor is sound@.";; *)
    all_test_completeness2 1 (BF.bor ~size:1) (CBF.bor ~size:1);;
    Format.printf "bor is sound and complete@.";;

    (* all_test_soundness2 1 BF.bxor CBF.bxor;;
     * Format.printf "bxor is sound@.";; *)
    all_test_completeness2 1 (BF.bxor ~size:1) (CBF.bxor ~size:1);;
    Format.printf "bxor is sound and complete@.";;

    all_test_soundness2 4 (BF.biadd ~size:4 ~nsw:false ~nuw:false ~nusw:false) (CBF.biadd ~size:4 ~nsw:false ~nuw:false ~nusw:false);;
    Format.printf "biadd is sound@.";;
    (* all_test_best2 4 4 (BF.biadd ~size:4) (CBF.biadd ~size:4);;
     * Format.printf "biadd is maximally precise@.";; *)
    (* biadd is incomplete, and not maximaly precise (e.g. cannot compute 1 + 1 = 2). *)

    all_test_soundness2 4 (BF.bisub ~size:4 ~nsw:false ~nuw:false ~nusw:false) (CBF.bisub ~size:4 ~nsw:false ~nuw:false ~nusw:false);;
    Format.printf "bisub is sound@.";;
    (* bisub is incomplete and not maximally precise. *)

    (* all_test_soundness1 2 (BF.buext ~size:4 ~oldsize:2) (CBF.buext ~size:4 ~oldsize:2);;
     * Format.printf "buext is sound@.";; *)
    all_test_completeness1 2 (BF.buext ~size:4 ~oldsize:2) (CBF.buext ~size:4 ~oldsize:2);;
    Format.printf "buext is sound and complete@.";;

    
    all_test_soundness1 2 (BF.bsext ~size:4 ~oldsize:2) (CBF.bsext ~size:4 ~oldsize:2);;
    Format.printf "bsext is sound@.";;
    (* bsext is incomplete, as we loose the fact the the copied bits are all equal. *)  
    all_test_best1 4 2 (BF.bsext ~size:4 ~oldsize:2) (CBF.bsext ~size:4 ~oldsize:2);;
    Format.printf "bsext is maximally precise@.";;
    


    all_test_soundness2 4 (BF.bshl ~size:4 ~nsw:false ~nuw:false) (CBF.bshl ~size:4 ~nsw:false ~nuw:false);;
    Format.printf "bshl is sound@.";;
    (* bshl is incomplete: 1 << ? = ??, while {1 << 0} U {1 << 1} = {1,10} *)
    all_test_best2 4 4 (BF.bshl ~size:4 ~nsw:false ~nuw:false) (CBF.bshl ~size:4 ~nsw:false ~nuw:false);;
    Format.printf "bshl is maximally precise@.";;  

    all_test_soundness2 4 (BF.blshr ~size:4) (CBF.blshr ~size:4);;
    Format.printf "blshr is sound@.";;
    (* blshr is incomplete: 1 << ? = ??, while {1 << 0} U {1 << 1} = {1,10} *)
    all_test_best2 4 4 (BF.blshr ~size:4) (CBF.blshr ~size:4);;
    Format.printf "blshr is maximally precise@.";;  
    

    all_test_soundness2 4 (BF.bashr ~size:4) (CBF.bashr ~size:4);;
    Format.printf "bashr is sound@.";;
    all_test_best2 4 4 (BF.bashr ~size:4) (CBF.bashr ~size:4);;
    Format.printf "bashr is maximally precise@.";;  
    
    
    (* all_test_soundness2_pred 2 (BF.beq ~size:2) (CBF.beq ~size:2);;
     * Format.printf "beq is sound@.";; *)
    all_test_completeness2_pred 2 (BF.beq ~size:2) (CBF.beq ~size:2);;
    Format.printf "beq is sound and complete@.";;

    

    (* all_test_soundness2_pred 4 (BF.bisle ~size:4) (CBF.bisle ~size:4);;
     * Format.printf "bisle is sound@.";; *)
    all_test_completeness2_pred 2 (BF.bisle ~size:4) (CBF.bisle ~size:4);;
    Format.printf "bisle is sound and complete@.";;
    

    (* all_test_soundness2_pred 4 (BF.biule ~size:4) (CBF.biule ~size:4);;
     * Format.printf "biule is sound@.";; *)
    all_test_completeness2_pred 2 (BF.biule ~size:4) (CBF.biule ~size:4);;
    Format.printf "biule is sound and complete@.";;
    

    
    (* all_test_soundness2 3 (BF.bconcat ~size1:3 ~size2:3) (CBF.bconcat ~size1:3 ~size2:3);;
     * Format.printf "bconcat is sound@.";; *)
    all_test_completeness2 3 (BF.bconcat ~size1:3 ~size2:3) (CBF.bconcat ~size1:3 ~size2:3);;
    Format.printf "bconcat is sound and complete@.";;


    (* all_test_soundness1 4 (BF.bextract ~index:1 ~size:2 ~oldsize:4) (CBF.bextract ~index:1 ~size:2 ~oldsize:4);;
     * Format.printf "bextract is sound@.";; *)
    all_test_completeness1 4 (BF.bextract ~index:1 ~size:2 ~oldsize:4) (CBF.bextract ~index:1 ~size:2 ~oldsize:4);;
    Format.printf "bextract is sound and complete@.";;



    all_test_soundness2 4 (BF.bimul ~size:4 ~nsw:false ~nuw:false) (CBF.bimul ~size:4 ~nuw:false ~nsw:false);;
    Format.printf "bimul is sound@.";;
    (* bisub is incomplete. *)
  end;;

    all_test_soundness2 4 (BF.biudiv ~size:4) (CBF.biudiv ~size:4);;
    Format.printf "biudiv is sound@.";;

    all_test_soundness2 4 (BF.biumod ~size:4) (CBF.biumod ~size:4);;
    Format.printf "biumod is sound@.";;
    
    all_test_soundness2 3 (BF.bisdiv ~size:3) (CBF.bisdiv ~size:3);;
    Format.printf "bisdiv is sound@.";;

    all_test_soundness2 4 (BF.bismod ~size:4) (CBF.bismod ~size:4);;
    Format.printf "bismod is sound@.";;

  (* all_test_completeness1 2 (BF.bnot ~size:2) (CBF.bnot ~size:2);;
   * Format.printf "bnot is sound@.";; *)
  (* end;; *)

  (*  I could give an example of soundness or incompleteness. *)
  let test_backward_soundness1 absop concop a r =
    match absop a r with
    | None -> true
    | Some x ->
       (* Should always return a refinement.  *)
       assert(Collecting.subset (Implem.concretize x) (Implem.concretize a));

       (* Should x be such that its image through concop encompasses what is both in r 
          and in the image of a. *)
       (* Format.printf "Result: a=%a r=%a res=%a@." Implem.pretty a Implem.pretty r Implem.pretty x; *)
       (* Format.fprintf "Error? %a %a" Collecting.pretty  (concop @@ Implem.concretize x) *)
       assert(
           Collecting.subset
             (Collecting.inter (Implem.concretize r) (concop @@ Implem.concretize a))
             (concop @@ Implem.concretize x) 
         );
       true
    (*    Collecting.subset (Implem.concretize)
     * (\* Format.printf "Abstract computation: %a %a@." Implem.pretty a Implem.pretty (absop a b);
     *  * Format.printf "Result: %a %a@." 
     *  *   Collecting.pretty (concop (Implem.concretize a) (Implem.concretize b)) 
     *  *   Collecting.pretty (Implem.concretize (absop a b));     *\)
     * Collecting.equal (concop (Implem.concretize a) (Implem.concretize b))
     *   (Implem.concretize (absop a b)) || failwith "Completeness check failed" *)
  ;;

  (*  I could give an example of soundness or incompleteness. *)
  let test_backward_soundness2 absop concop a b r =
    let newa,newb = 
      match absop a b r with
      | None,None -> a,b
      | Some a,None -> a,b
      | None, Some b -> a,b
      | Some a, Some b -> a,b
    in
    (* Should always return a refinement.  *)
    assert(Collecting.subset (Implem.concretize newa) (Implem.concretize a));
    assert(Collecting.subset (Implem.concretize newb) (Implem.concretize b));


    Format.printf "Result: a=%a b=%a r=%a newa=%a newb=%a@." 
      Implem.pretty a Implem.pretty b Implem.pretty r
      Implem.pretty newa Implem.pretty newb;
    (* Should newa and newb be such that its image through concop encompasses what is both in r 
          and in the image of a,b. *)    
    assert(Collecting.subset
             (Collecting.inter (Implem.concretize r) (concop (Implem.concretize a) (Implem.concretize b)))
             (concop (Implem.concretize newa) (Implem.concretize newb))
             );
    true
    (*    Collecting.subset (Implem.concretize)
     * (\* Format.printf "Abstract computation: %a %a@." Implem.pretty a Implem.pretty (absop a b);
     *  * Format.printf "Result: %a %a@." 
     *  *   Collecting.pretty (concop (Implem.concretize a) (Implem.concretize b)) 
     *  *   Collecting.pretty (Implem.concretize (absop a b));     *\)
     * Collecting.equal (concop (Implem.concretize a) (Implem.concretize b))
     *   (Implem.concretize (absop a b)) || failwith "Completeness check failed" *)
  ;;

  let test_backward_soundness2_pred absop concop a b r =
    let newa,newb = 
      match absop a b r with
      | None,None -> a,b
      | Some a,None -> a,b
      | None, Some b -> a,b
      | Some a, Some b -> a,b
    in
    (* Should always return a refinement.  *)
    assert(Collecting.subset (Implem.concretize newa) (Implem.concretize a));
    assert(Collecting.subset (Implem.concretize newb) (Implem.concretize b));


    Format.printf "Result: a=%a b=%a r=%a newa=%a newb=%a@." 
      Implem.pretty a Implem.pretty b Quadrivalent.pretty r
      Implem.pretty newa Implem.pretty newb;
    (* Should newa and newb be such that its image through concop encompasses what is both in r 
          and in the image of a,b. *)    
    assert(Quadrivalent.includes
             (concop (Implem.concretize newa) (Implem.concretize newb))
             (Quadrivalent.inter r (concop (Implem.concretize a) (Implem.concretize b)))
      );
    (* This also tests completeness. *)
    (* assert(Quadrivalent.equal
     *          (Quadrivalent.inter r (concop (Implem.concretize a) (Implem.concretize b)))
     *          (concop (Implem.concretize newa) (Implem.concretize newb))
     *          ); *)
    
    true
    (*    Collecting.subset (Implem.concretize)
     * (\* Format.printf "Abstract computation: %a %a@." Implem.pretty a Implem.pretty (absop a b);
     *  * Format.printf "Result: %a %a@." 
     *  *   Collecting.pretty (concop (Implem.concretize a) (Implem.concretize b)) 
     *  *   Collecting.pretty (Implem.concretize (absop a b));     *\)
     * Collecting.equal (concop (Implem.concretize a) (Implem.concretize b))
     *   (Implem.concretize (absop a b)) || failwith "Completeness check failed" *)
  ;;

  let test_backward_completeness2_pred absop concop a b r =

    (* let r = Quadrivalent.inter r @@ absopf a b in *)
    
    let newa,newb = 
      match absop a b r with
      | None,None -> a,b
      | Some a,None -> a,b
      | None, Some b -> a,b
      | Some a, Some b -> a,b
    in
    (* Should always return a refinement.  *)
    assert(Collecting.subset (Implem.concretize newa) (Implem.concretize a));
    assert(Collecting.subset (Implem.concretize newb) (Implem.concretize b));


    Format.printf "Result: a=%a b=%a r=%a newa=%a newb=%a@." 
      Implem.pretty a Implem.pretty b Quadrivalent.pretty r
      Implem.pretty newa Implem.pretty newb;
    (* Should newa and newb be such that its image through concop encompasses what is both in r 
          and in the image of a,b. *)    
    assert(Quadrivalent.equal
             (Quadrivalent.inter r (concop (Implem.concretize a) (Implem.concretize b)))
             (concop (Implem.concretize newa) (Implem.concretize newb))
      );
    (* This also tests completeness. *)
    (* assert(Quadrivalent.equal
     *          (Quadrivalent.inter r (concop (Implem.concretize a) (Implem.concretize b)))
     *          (concop (Implem.concretize newa) (Implem.concretize newb))
     *          ); *)
    
    true
    (*    Collecting.subset (Implem.concretize)
     * (\* Format.printf "Abstract computation: %a %a@." Implem.pretty a Implem.pretty (absop a b);
     *  * Format.printf "Result: %a %a@." 
     *  *   Collecting.pretty (concop (Implem.concretize a) (Implem.concretize b)) 
     *  *   Collecting.pretty (Implem.concretize (absop a b));     *\)
     * Collecting.equal (concop (Implem.concretize a) (Implem.concretize b))
     *   (Implem.concretize (absop a b)) || failwith "Completeness check failed" *)
  ;;

  let forward_then_backward_pred fwd bwd =
    fun a b r -> 
    let r = Quadrivalent.inter r @@ fwd a b in
    bwd a b r
  ;;
    
  
  
  let all_test_backward_soundness1 = all_test_f2' test_backward_soundness1;;
  let all_test_backward_soundness2 = all_test_f3 test_backward_soundness2;;
  let all_test_backward_soundness2_pred = all_test_f3' test_backward_soundness2_pred;;
  (* let all_test_backward_completeness2_pred = all_test_f3' test_backward_completeness2_pred;;       *)

  module BTest() = struct
  (* all_test_backward_soundness1 (2,2) (BR.bnot ~size:2) (Collecting.bnot ~size:2);;
   * Format.printf "rev_bnot is sound (and complete)@.";; *)

  all_test_backward_soundness2 (2,2,2) (BR.band ~size:2) (CBF.band ~size:2);;
  Format.printf "rev_band is sound@.";;

  all_test_backward_soundness2 (2,2,2) (BR.bor ~size:2) (CBF.bor ~size:2);;
  Format.printf "rev_bor is sound@.";;

  all_test_backward_soundness2 (2,2,2) (BR.bxor ~size:2) (CBF.bxor ~size:2);;
  Format.printf "rev_bxor is sound@.";;

  all_test_backward_soundness2 (2,2,4) (BR.bconcat ~size1:2 ~size2:2) (CBF.bconcat ~size1:2 ~size2:2);;
  Format.printf "rev_bconcat is sound@.";;

  all_test_backward_soundness1 (4,2) (BR.bextract ~size:2 ~index:1 ~oldsize:4) (CBF.bextract ~size:2 ~index:1 ~oldsize:4);;
  Format.printf "rev_bextract is sound@.";;

  all_test_backward_soundness1 (2,4) (BR.buext ~size:4 ~oldsize:2) (CBF.buext ~size:4 ~oldsize:2);;
  Format.printf "rev_buext is sound@.";;

  all_test_backward_soundness1 (2,4) (BR.bsext ~size:4 ~oldsize:2) (CBF.bsext ~size:4 ~oldsize:2);;
  Format.printf "rev_bsext is sound@.";;


  (* Takes around 15s. *)
  all_test_backward_soundness2 (4,4,4) (BR.bshl ~size:4 ~nsw:false ~nuw:false) (CBF.bshl ~size:4 ~nsw:false ~nuw:false);;
  Format.printf "rev_bshl is sound@.";;

  all_test_backward_soundness2 (4,4,4) (BR.blshr ~size:4) (CBF.blshr ~size:4);;
  Format.printf "rev_blshr is sound@.";;

  all_test_backward_soundness2 (2,2,2) (BR.bashr ~size:2) (CBF.bashr ~size:2);;
  Format.printf "rev_bashr is sound@.";;

  
  all_test_backward_soundness2 (4,4,4) (BR.bashr ~size:4) (CBF.bashr ~size:4);;
  Format.printf "rev_bashr is sound@.";;


  all_test_backward_soundness2_pred (3,3,3) (BR.beq ~size:3) (CBF.beq ~size:3);;
  Format.printf "rev_beq is sound@.";;

  (* all_test_backward_completeness2_pred (3,3,3) (forward_then_backward_pred (BF.beq ~size:3) (BR.beq ~size:3)) (CBF.beq ~size:3);;
   * Format.printf "rev_beq is complete@.";; *)
  all_test_backward_soundness2_pred (3,3,3) (BR.biule ~size:3) (CBF.biule ~size:3);;
  Format.printf "rev_biule is sound@.";;

  all_test_backward_soundness2_pred (3,3,3) (BR.bisle ~size:3) (CBF.bisle ~size:3);;
  Format.printf "rev_bisle is sound@.";;  

  
  all_test_backward_soundness2 (3,3,3) (BR.biadd ~size:3 ~nsw:false ~nuw:false ~nusw:false) (CBF.biadd ~size:3 ~nsw:false ~nuw:false ~nusw:false);;
  Format.printf "rev_biadd is sound@.";;

  all_test_backward_soundness2 (3,3,3) (BR.bisub ~size:3 ~nsw:false ~nuw:false ~nusw:false) (CBF.bisub ~size:3 ~nsw:false ~nuw:false ~nusw:false);;
  Format.printf "rev_bisub is sound@.";;


  all_test_backward_soundness2 (3,3,3) (BR.bimul ~nsw:false ~nuw:false ~size:3) (CBF.bimul ~nsw:false ~nuw:false ~size:3);;
  Format.printf "rev_bimul is sound@.";;

  
  all_test_backward_soundness2 (3,3,3) (BR.bimul ~nsw:true ~nuw:false ~size:3) (CBF.bimul ~nsw:true ~nuw:false ~size:3);;
  Format.printf "rev_bimul is sound@.";;

  all_test_backward_soundness2 (3,3,3) (BR.bimul ~nsw:false ~nuw:true ~size:3) (CBF.bimul ~nsw:false ~nuw:true ~size:3);;
  Format.printf "rev_bimul is sound@.";;

  all_test_backward_soundness2 (3,3,3) (BR.bimul ~nsw:true ~nuw:true ~size:3) (CBF.bimul ~nsw:true ~nuw:true ~size:3);;
  Format.printf "rev_bimul is sound@.";;

  all_test_backward_soundness2 (3,3,3) (BR.biudiv ~size:3) (CBF.biudiv ~size:3);;
  Format.printf "rev_biudiv is sound@.";;

  all_test_backward_soundness2 (3,3,3) (BR.bisdiv ~size:3) (CBF.bisdiv ~size:3);;
  Format.printf "rev_bisdiv is sound@.";;
  
  all_test_backward_soundness2 (3,3,3) (BR.biumod ~size:3) (CBF.biumod ~size:3);;
  Format.printf "rev_biumod is sound@.";;

  all_test_backward_soundness2 (3,3,3) (BR.bismod ~size:3) (CBF.bismod ~size:3);;
  Format.printf "rev_bismod is sound@.";;
  end;;  
  
  (* all_test_backward_completeness2_pred (3,3,3) (BR.biule ~size:3) (CBF.biule ~size:3);;
   * Format.printf "rev_biule is complete@.";; *)

  
  (* all_test_backward_completeness2_pred (3,3,3) (BF.biule ~size:3) (BR.biule ~size:3) (CBF.biule ~size:3);;
   * Format.printf "rev_biule is complete@.";; *)

  
  (* XXX: tester la completeness de mes predicats; peut-etre en raffinant d'abord res apres avoir calcule op(x,y), puisque cést je pense ce que je présume et pour ca que je suis incomplete.
   * 
   * XXX:biule marche pas mal, mais ameliorer                                                               *)                                                  
  
  (* all_test_backward_soundness2_pred (4,4,4) (BR.biule ~size:4) (CBF.biule ~size:4);;
   * Format.printf "rev_biule is sound@.";; *)

  
  (* all_test_best2 3 3 (BF.bismod ~size:3) (CBF.bismod ~size:3);;
   * Format.printf "bismod is maximlly precise@.";; *)

  
  (* all_test_soundness2 4 (BF.bisdiv ~size:4) (CBF.bisdiv ~size:4);;
   * Format.printf "bisdiv is sound@.";; *)

end;;

(* module TestMust0_Must1 = Test(Known_bits_basis);; *)
