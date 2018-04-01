open Graphics ;;

open Klotski ;;

let startTimer = Unix.gettimeofday () ;;

let board_list = reverse begin solve_klotski initial_board_simpler2 end ;;

let stopTimer = Unix.gettimeofday () ;;

Printf.printf "It took about %.6f seconds for my program to find a solution!" (stopTimer -. startTimer) ;;

print_endline "\n" ;; 

open_graph " 600x700" ;;
                                                
List.map (List.iter (fun t -> display_board t; Unix.sleep 1))
         (repeat board_list 5) ;;


