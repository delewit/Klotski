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
  run_test_tt_main loop_tests ;;

  
