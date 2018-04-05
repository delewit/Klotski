open Graphics ;;

open Klotski ;;

let startTimer = Unix.gettimeofday () ;;

let board_list = solve_klotski initial_board_simpler ;;

let stopTimer = Unix.gettimeofday () ;;

Printf.printf "It took about %.6f seconds for my program to find a solution!" (stopTimer -. startTimer) ;;

print_endline "\n" ;; 


