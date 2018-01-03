(* The first three functions are preliminary functions for the Solver for Klotski. *)

exception NotFound ;;

let rec loop (p : ('a -> bool)) (f : ('a -> 'a)) (x : 'a) : 'a = match p x with 
                                                      |true     -> x 
                                                      |false    -> loop p f (f x) ;;


let rec exists (p : ('a -> bool)) (ls : 'a list) : bool = match ls with 
                                              |[]           -> false
                                              |head :: tail -> if p head 
                                                               then true 
                                                               else exists p tail ;;

let rec find (p : ('a -> bool)) (ls : 'a list) : 'a = match ls with 
                                            |[]            -> raise NotFound 
                                            |head :: tail  -> if p head 
                                                              then head 
                                                              else find p tail ;;


