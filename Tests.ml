open OUnit2 ;;   (* For the sake of testing. *)

open Klotski ;;

let loop_tests =
  "Loop Tests" >:::
    ["Initially True" >:: (fun cxt -> assert_equal 1 (loop (fun x -> x = 1) (fun x -> x) 1));
     "True After One Iteration" >:: (fun cxt ->
				     assert_equal 1 (loop (fun x -> x = 1) (fun x -> x - 1) 2));
     "True After Two Iterations" >:: (fun cxt ->
				      assert_equal 10 (loop (fun x -> x = 10) (fun x -> x + 2) 6))] ;;

let () =
  run_test_tt_main loop_tests ;;   (* For the sake of testing. *)

let loop_tests = 
  "Loop Tests" >::: 
    ["Initially True" >:: (fun cxt -> assert_equal 1 (loop (fun x -> x = 1) (fun x -> x) 1)); 
     "True After One Iteration" >:: (fun cxt -> 
       assert_equal 1 (loop (fun x -> x = 1) (fun x -> x - 1) 2)); 
     "True After Two Iterations" >:: (fun cxt -> 
     assert_equal 10 (loop (fun x -> x = 10) (fun x -> x + 2) 6))] ;;
  
let () =
  run_test_tt_main loop_tests ;;

  
let loop_prime_tests =
  "Loop_Prime Tests" >:::
  ["Initially True" >:: (fun cxt -> assert_equal [20]
  (loop' (fun x -> x > 10) (fun x -> x + 1) 20));
  "Not Initially True" >:: (fun cst -> assert_equal [20; 15; 10]
  (loop' (fun x -> x <= 10) (fun x -> x - 5) 20))] ;;


let () =
  run_test_tt_main loop_prime_tests ;;


let exists_tests =
  "Exists Tests" >:::
  ["Evaluates To False" >:: (fun cxt -> assert_equal false
  (exists (fun x -> x > 2.5) [0.0; 0.5; 1.0; 1.5; 2.0]));
   "Evaluates to True" >::
     (fun cxt -> assert_equal true (exists (fun x -> x > 2.5) [0.0; 0.5; 1.0; 1.5; 2.0; 2.5; 3.0]));
   "Evaluates to False" >:: (fun cxt -> assert_equal false 
   (exists (fun t -> t <> 1) []));
   "Evaluates to True" >:: (fun cxt -> assert_equal true  
   (exists (fun t -> t <> 1) [2; 1; 2]))
  ] ;;


let () =
  run_test_tt_main exists_tests ;;


