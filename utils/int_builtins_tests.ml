module Int_builtins_zarith = struct

  (* Does not work for negative or null numbers. *)
  let log2 x = Z.log2 @@ Z.of_int x
  
  let highest_bit x =
    1 lsl (Z.log2 @@ Z.of_int x)

  let popcount x = Z.popcount @@ Z.extract (Z.of_int x) 0 (Sys.int_size)

  let ctz x = Z.trailing_zeros @@ Z.of_int x;;

  let ffs x = if x = 0 then 0 else 1 + (Z.trailing_zeros @@ Z.of_int x);;  
end;;

(* Log2 should be the position of the highest bit set,
   except log2 0 = -1. *)
let check_log2 x log2x =
  (* Printf.printf "CHECK_LOG2: %d %d\n%!" x log2x; *)
  if(x > 0) then assert(log2x = Int_builtins_zarith.log2 x);
  if x = 0 then assert(log2x = -1)
  else begin
  let u = (1 lsl log2x) in  
  assert(x lsr log2x = 1)
end


(* This really is a weird spec.  *)
let check_log2_untaggued x log2x =
  (* Printf.printf "CHECK_LOG2_UNTAGGUED: %d %d\n%!" x log2x; *)
  if(x > 0) then assert(log2x = Int_builtins_zarith.log2 x);
  if x = 0 then assert(log2x = 1)
  else begin
    let log2x = if x < 0 then log2x - 1  else log2x in
    let u = (1 lsl log2x) in  
    assert(x lsr log2x = 1)
  end
;;

let check_highest_bit x res  =
  (* Printf.printf "CHECK_HIGHEST_BIT: %x %x\n%!" x res;   *)
  if (x = 0)
  then (assert (res = 0))
  else begin
    assert(x != 0);
    (* The result is a single bit set. *)
    assert(Int_builtins.is_zero_or_a_power_of_2 res);
    (* The bit x is set. *)
    assert(x land res = res);
    (* It is the highest bit. *)
    assert(x land (lnot res) land (lnot (res - 1)) = 0)
  end
;;


(* No spec if x <= 0. *)
let check_highest_bit_unsafe x res =
  if(x <= 0) then ()
  else check_highest_bit x res
;;

let check_popcount x res =
  (* Printf.printf "CHECK_POPCOUNT: %x %d %d\n%!" x res (Int_builtins_zarith.popcount x);   *)
  assert(res = Int_builtins_zarith.popcount x)
;;

let check_ctz x res =
  (* Printf.printf "CHECK_CTZ: %x %d %d\n%!" x res (Int_builtins_zarith.ctz x); *)
  assert(x = 0 && res = Sys.word_size || res = Int_builtins_zarith.ctz x)
;;

let check_ffs x res =
  (* Printf.printf "CHECK_FFS: %x %d %d\n%!" x res (Int_builtins_zarith.ffs x); *)
  assert(res = Int_builtins_zarith.ffs x)
;;



let%test_module "Int_builtins" = (module struct

  let gen = QCheck.(frequency [1,QCheck.small_int_corners();1,int]);;

  QCheck.Test.check_exn @@ QCheck.Test.make ~count:1000 ~name:"log2" gen (fun x ->
      (* Printf.printf "LOG2: %d %d\n%!" x (Int_builtins.log2 x); *)
      check_log2 x (Int_builtins.log2 x); true) ;;

  QCheck.Test.check_exn @@ QCheck.Test.make ~count:1000 ~name:"log2_untagged_unsafe" gen (fun x ->
      (* Printf.printf "LOG2: %d %d %d\n%!" x (Int_builtins.log2_untagged_unsafe x) (Int_builtins.log2 x); *)
      check_log2_untaggued x (Int_builtins.log2_untagged_unsafe x); true) ;;

  QCheck.Test.check_exn @@ QCheck.Test.make ~count:1000 ~name:"highest_bit" gen (fun x ->
      (* Printf.printf "HIGHEST_BIT: %d %d\n%!" x (Int_builtins.highest_bit x); *)
      check_highest_bit x (Int_builtins.highest_bit x); true) ;;

  QCheck.Test.check_exn @@ QCheck.Test.make ~count:1000 ~name:"highest_bit_untagged" gen (fun x ->
      (* Printf.printf "HIGHEST_BIT_UNTAGGED: %x %x %x\n%!" x (Int_builtins.highest_bit_untagged_unsafe x) (Int_builtins.highest_bit x); *)
      check_highest_bit_unsafe x (Int_builtins.highest_bit_untagged_unsafe x); true) ;;

  QCheck.Test.check_exn @@ QCheck.Test.make ~count:1000 ~name:"popcount" gen (fun x ->
      check_popcount x (Int_builtins.popcount x); true) ;;

  QCheck.Test.check_exn @@ QCheck.Test.make ~count:1000 ~name:"popcount_untaggued" gen (fun x ->
      check_popcount x (Int_builtins.popcount_untaggued x); true) ;;

  QCheck.Test.check_exn @@ QCheck.Test.make ~count:1000 ~name:"ctz" gen (fun x ->
      check_ctz x (Int_builtins.count_trailing_zeroes x); true) ;;
  (for i = 0 to 64; do let x = 1 lsl i in check_ctz x (Int_builtins.count_trailing_zeroes x); done);;
  (let rec loop x = if(x != 0) then (check_ctz x (Int_builtins.count_trailing_zeroes x); loop (x * 2)) in loop (-1));;

  QCheck.Test.check_exn @@ QCheck.Test.make ~count:1000 ~name:"ctz_untaggued" gen (fun x ->
      check_ctz x (Int_builtins.count_trailing_zeroes_untagged x); true) ;;
  (for i = 0 to 64; do let x = 1 lsl i in check_ctz x (Int_builtins.count_trailing_zeroes_untagged x); done);;
  (let rec loop x = if(x != 0) then (check_ctz x (Int_builtins.count_trailing_zeroes_untagged x); loop (x * 2)) in loop (-1));;

  QCheck.Test.check_exn @@ QCheck.Test.make ~count:1000 ~name:"ffs" gen (fun x ->
      check_ffs x (Int_builtins.ffs x); true) ;;
  (for i = 0 to 64; do let x = 1 lsl i in check_ffs x (Int_builtins.ffs x); done);;
  (let rec loop x = if(x != 0) then (check_ffs x (Int_builtins.ffs x); loop (x * 2)) in loop (-1));;

  QCheck.Test.check_exn @@ QCheck.Test.make ~count:1000 ~name:"ffs_untaggued" gen (fun x ->
      check_ffs x (Int_builtins.ffs_untaggued x); true) ;;
  (for i = 0 to 64; do let x = 1 lsl i in check_ffs x (Int_builtins.ffs_untaggued x); done);;
  (let rec loop x = if(x != 0) then (check_ffs x (Int_builtins.ffs_untaggued x); loop (x * 2)) in loop (-1));;

  
  
  

end)
