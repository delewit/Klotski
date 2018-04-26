(* This file calls upon functions defined in the Graphics module, and also calls upon functions, 
   records, and various other data types defined in the file, Klotski.ml. *)

open Graphics ;;

open Klotski ;;

let startTimer = Unix.gettimeofday () ;;

let board_list = reverse begin solve_klotski initial_board_simpler2 end ;;

let stopTimer = Unix.gettimeofday () ;;

Printf.printf "\nIt took about %.6f seconds for my program to find a solution!" (stopTimer -. startTimer) ;;

print_endline "\n" ;;

Printf.printf "The puzzle was solved in %d moves!" (List.length board_list - 1) ;;

open_graph " 600x700" ;;
                                                
List.map (List.iter (fun t -> display_board t; Unix.sleep 1))
         (repeat board_list 1000) ;;

print_endline "\n" ;;


