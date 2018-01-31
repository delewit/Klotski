(* Ocaml functions written by Douglas Lewit of Oakton Community College and Northeastern Illinois University. 
   Everything in this program file is up-to-date as of September 28th, 2017. *)

exception NotFound ;;

type 'e rel = 'e -> 'e list ;;

type 'e prop = 'e -> bool ;;

type ('a, 'set) set_operations =
  {
    empty : 'set ;
    mem   : 'a -> 'set -> bool ;
    add   : 'a -> 'set -> 'set ;
  }


type ('configuration, 'move) puzzle =
    { move : 'configuration -> 'move -> 'configuration;
      possible_moves : 'configuration -> 'move list;
      final : 'configuration -> bool
    }

module IntSet = Set.Make(struct
  type t = int
  let compare = Pervasives.compare
end)

let int_set_ops : (int, IntSet.t) set_operations =
  {
    empty = IntSet.empty ;
    mem = IntSet.mem ;
    add = IntSet.add ;
  }


let rec loop (p : ('a -> bool)) (f : ('a -> 'a)) (x : 'a) : 'a =
  match p x with 
  |true     ->  x 
  |false    ->  loop p f (f x) ;;


let rec loop' (p : ('a -> bool)) (f : ('a -> 'a)) (x : 'a) : 'a list =
  match p x with 
  |true        ->  [x]
  |false       ->   x :: loop' p f (f x) ;;



let rec exists (p : ('a -> bool)) (ls : 'a list) : bool =
  match ls with 
  |[]           -> false
  |head :: tail -> if p head 
                   then true 
                   else exists p tail ;;


let rec find (p : ('a -> bool)) (ls : 'a list) : 'a =
  match ls with 
  |[]            -> raise NotFound 
  |head :: tail  -> if p head 
                    then head 
                    else find p tail ;;

(* near returns the function, near'. *)
let near : int rel =   (* The default step size here is just 1. *)
  let rec sequence ?step:(s=1) lower upper = 
    if lower > upper 
    then [] 
    else lower :: sequence ~step:s (lower + s) upper 
  in 
  let near' (n : int) : int list = 
    let lower_bound = n - 2 in 
    let upper_bound = n + 2 in 
    sequence lower_bound upper_bound 
  in near' (* Here we have an example of "partial function application". *) ;;

(* flat_map returns the function, flat_map'. *)
let flat_map (rel_function : 'e rel) : 'e list -> 'e list = 
              let rec flatten (lst : 'e list list) : 'e list = 
                      match lst with 
                     |[]           -> [] 
                     |head :: tail -> match head with 
                                     |[]         -> flatten tail 
                                     |hd :: tl   -> hd :: flatten (tl :: tail) 
              in 
              let rec flat_map' (e_list : 'e list) : 'e list list = 
                      match e_list with 
                     |[]           -> [] 
                     |head :: tail -> (rel_function head) :: flat_map' tail 
              in
              (* The variable "x" in my composition function is really a dummy variable. *)
              let composition (f : 'a -> 'b) (g : 'c -> 'a) (x : 'c) : 'b = f (g x)  
              in composition flatten flat_map' (* This is an example of "partial function application" 
                                                  and also function composition. *) ;;

(* iter_rel should be used like this: (iter_rel near 5) 2 --> the interpretation is that (iter_rel near 5) 2 should 
   yield the same result as flat_map near (flat_map near( flat_map near( flat_map near (near 2)))).  
   Try it for yourself!  By the way, this function was NOT easy to write!!! *)
let iter_rel (rel_func : 'e rel) (i : int) : 'e rel =
    let rec f x n = if n = 1
		    then x
		    else f (x |> flat_map rel_func) (n - 1)
    in let g k = f (rel_func k) i
       in g ;;


(* Here is step #7 in building a generic puzzle solver that will later be applied to the Klotski Puzzle problem. 
The only possible problem with the "solve" function is that it could iterate indefinitely if no solution is ever 
found. *)
let solve (r : 'a rel) (p : 'a prop) (x : 'a) : 'a =
    let rec solver (r' : 'a rel) (x' : 'a) (n : int) =
        let listOfPossibleSolutions = (iter_rel r' n) x in
        if exists p listOfPossibleSolutions
        then find p listOfPossibleSolutions
        else solver r' x (n + 1)
    in if p x 
       then x 
       else solver r x 1 ;;


(* The correct solve_path function needed to solve the Klotski Puzzle problem. *)
let solve_path (r : 'a rel) (p : 'a prop) (x : 'a) : 'a list =
  let rec last (lst : 'a list) : 'a =
    match lst with
    |[]            -> raise (invalid_arg "Bad argument")
    (* The base case for empty lists. *)
    |head :: []    -> head
    (* The base case for non-empty lists. *)
    |head :: tail  -> last tail (* The general recursive case. *)
  in solve (fun path -> List.map (fun y -> path @ [y]) (r (last path))) (fun path -> p (last path)) [x] ;; 
  


(* The following two functions can be used to instantiate records of type ('a, 'a list) set_operations. *)
let rec member (x : 'a) (ls : 'a list) : bool = 
    match ls with 
    |[]            -> false 
    |head :: tail  -> if x = head 
                      then true 
                      else member x tail ;;


let rec addElement (x : 'a) (ls : 'a list) : 'a list = 
    match member x ls with 
    |false       -> x :: ls 
    |true        -> ls ;;


let list_set : ('a, 'a list) set_operations =
  {
    empty = [] ;
    mem = member ;
    add = addElement ;
  }
  
(* The following two functions can be used to find the union and intersection of sets with the CAVEAT that these 
   sets are represented as lists, AND FURTHERMORE that these lists are embedded within a larger list. *)
(* let union (listOfSets : 'a list list) : 'a list =
  let rec removeDups (lst : 'a list) : 'a list =
    match lst with
    |[]           -> []
    |head :: tail -> if member head tail
                     then removeDups tail
                     else head :: removeDups tail
  in List.fold_left (fun x y -> removeDups (x @ y)) [] listOfSets ;;
 *)

(* let intersection (listOfSets : 'a list list) : 'a list =
  let rec findSharedElements (list1 : 'a list) (list2 : 'a list) (intersection : 'a list) : 'a list =
    match list1 with
    |[]              -> intersection
    |head :: tail    -> if member head list2
                        then findSharedElements tail list2 (head :: intersection)
                        else findSharedElements tail list2 intersection
  in let findSharedElems ls1 ls2 = findSharedElements ls1 ls2 []
     in
     List.fold_left (fun x y -> findSharedElems x y) (List.hd listOfSets) (List.tl listOfSets) ;;
 *)

(* Here is Step #9 of the Klotski Puzzle solution guide from the OCaml MOOC. *)
let archive_map (opset : ('a, 'set) set_operations) (rel : 'a rel) ((s, l) : ('set * 'a list)) : ('set * 'a list) =
  let rec includeInList (s : 'set) (ls : 'a list) (accumulator : 'a list) : 'a list =
    match ls with
    |[]             ->  accumulator
    |head :: tail   ->  if opset.mem head s
                        then includeInList s tail accumulator
                        else includeInList s tail (head :: accumulator)
  in 
  let l_ = flat_map rel l in
  let l' = includeInList s l_ [] in
  let s' = List.fold_left (fun set element -> opset.add element set) s l'
  in 
  (s', l') ;;



(* Here is Step #10 of the Klotski Puzzle solution guide from the OCaml MOOC. *)
let solve' (opset : ('a, 'set) set_operations) (rel : 'a rel) (predicate : 'a prop) (x : 'a) : 'a =
  let archive_map' rel' (s, l) =
    loop (fun (x, y) -> exists predicate y) (fun (x, y) -> archive_map opset rel' (x, y)) (s, l)
  in let (s, l) = archive_map' rel (opset.empty, [x])
     in find predicate l ;;


let rec last = function
  |[]           -> raise (Invalid_argument "Bad arg!")
  |head :: []   -> head
  |head :: tail -> last tail ;;


let e_rel_list1 (n : 'e rel) (x : 'e list) : 'e list list =
  let elistList = n (last x)
  in let rec appendLists (x1 : 'a list) (x2 : 'a list) (accum : 'a list list) : 'a list list =
         match x2 with
         |[]           -> List.rev accum
         |head :: tail -> appendLists x1 tail ((x1 @ [head]) :: accum)
         in appendLists x elistList [] ;;
                                                                     

let e_rel_list2 (n : 'e rel) (x : 'e list) : 'e list list =
  let elistList = n (last x)
  in List.map (fun t -> x @ [t]) elistList ;;
  
(* Here is Step #11 of the Klotski Puzzle solution guide from the OCaml MOOC. *)
let solve_path' (opset : ('a list, 'set) set_operations) (rel : 'a rel) (predicate : 'a prop) (x : 'a) : 'a list =
  solve' opset (e_rel_list1 rel) (exists predicate) [x] ;;
           

(* let rec solve_puzzle (p : ('c, 'm) puzzle) (opset : ('c list, 's) set_operations) (c : 'c) : 'c list = *)
 

(* For the sake of comic relief!!! *)
let () =
  Printf.printf "\nThis %s puzzle sure is complicated stuff!\n" "Klotski";
  Printf.printf "\n"



