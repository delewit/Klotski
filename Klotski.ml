(* OCaml functions written by Douglas Lewit of Oakton Community College and Northeastern Illinois University. 
   Everything in this program file is up-to-date as of March 14, 2018. *)

open Graphics ;;

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



let map (f : 'a -> 'b) (a_list : 'a list) : 'b list =
  let rec map' (accum : 'b list) (f : 'a -> 'b) (a_list : 'a list) : 'b list = 
    match a_list with 
    |[]          -> List.rev accum 
    |h :: t      -> map' (f h :: accum) f t in 
  map' [] f a_list ;;


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
  let flatten (ls : 'e list list) : 'e list = 
    let rec flatten_helper (accumulator : 'e list) (lst : 'e list list) : 'e list = 
      match lst with 
      |[]            ->  List.rev accumulator 
      |head :: tail  ->  match head with 
                         |[]        ->  (flatten_helper [@ocaml.tailcall]) accumulator tail 
                         |hd :: tl  ->  (flatten_helper [@ocaml.tailcall]) (hd :: accumulator) (tl :: tail) in 
    flatten_helper [] ls 
  in
  let flat_map' (e_list : 'e list) : 'e list list = 
    let rec flat_map'' (accum : 'e list list) (e_lst : 'e list) : 'e list list =
      match e_lst with
      |[]           ->  accum 
      |head :: tail ->  (flat_map'' [@ocaml.tailcall]) (rel_function head :: accum) tail
    in flat_map'' [] e_list
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
  in solve (fun path -> map (fun y -> path @ [y]) (r (last path))) (fun path -> p (last path)) [x] ;;



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
let union (listOfSets : 'a list list) : 'a list =
  let rec removeDups (lst : 'a list) : 'a list =
    match lst with
    |[]           -> []
    |head :: tail -> if member head tail
		     then removeDups tail
		     else head :: removeDups tail
  in List.sort compare begin List.fold_left (fun x y -> removeDups (x @ y)) [] listOfSets end ;;


let intersection (listOfSets : 'a list list) : 'a list =
  let rec findSharedElements (list1 : 'a list) (list2 : 'a list) (intersection : 'a list) : 'a list =
    match list1 with
    |[]              -> intersection
    |head :: tail    -> if member head list2
			then findSharedElements tail list2 (head :: intersection)
			else findSharedElements tail list2 intersection
  in let findSharedElems ls1 ls2 = findSharedElements ls1 ls2 []
     in
     List.sort compare begin List.fold_left (fun x y -> findSharedElems x y) (List.hd listOfSets) (List.tl listOfSets) end ;;


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
  in map (fun t -> x @ [t]) elistList ;;

(* Here is Step #11 of the Klotski Puzzle solution guide from the OCaml MOOC. *)
let solve_path' (opset : ('a list, 'set) set_operations) (rel : 'a rel) (predicate : 'a prop) (x : 'a) : 'a list =
  solve' opset (e_rel_list1 rel) (fun path -> let last_of_path = last path
					      in predicate last_of_path) [x] ;;


let rec solve_puzzle (p : ('c, 'm) puzzle) (opset : ('c list, 's) set_operations) (c : 'c) : 'c list =
  solve_path' opset (fun x -> map (p.move x) (p.possible_moves x)) (p.final) c ;;


(* For the sake of comic relief!!! *)
let () =
  Printf.printf "\nThis %s puzzle sure is complicated stuff!\n" "Klotski";
  Printf.printf "\n"


(* Here begins Part B of the Klotski Project. *)

type piece_kind = S|H|V|C|X ;;


type piece = piece_kind * int ;;


let x = (X, 0) and s = (S, 0) and h = (H, 0) ;;
let (c0, c1, c2, c3) = ((C, 0), (C, 1), (C, 2), (C, 3)) ;;
let (v0, v1, v2, v3) = ((V, 0), (V, 1), (V, 2), (V, 3)) ;;
let all_pieces : piece list = [ s; h; c0; c1; c2; c3; v0; v1; v2; v3 ] ;;


type board = piece array array ;;


let final (bd : board) : bool =
  let check1 = (S, 0) = bd.(3).(1) in
  let check2 = (S, 0) = bd.(3).(2) in
  let check3 = (S, 0) = bd.(4).(1) in
  let check4 = (S, 0) = bd.(4).(2) in
  let boolean_list = [check1; check2; check3; check4]
  in List.fold_right (&&) boolean_list true ;;


type move = Move of piece * direction * board
 and direction = { dcol : int; drow : int } ;;


let initial_configuration =
  [| [| (V,0) ; (S,0) ; (S,0) ; (V,1) |] ;
     [| (V,0) ; (S,0) ; (S,0) ; (V,1) |] ;
     [| (V,2) ; (H,0) ; (H,0) ; (V,3) |] ;
     [| (V,2) ; (C,0) ; (C,1) ; (V,3) |] ;
     [| (C,2) ; (X,0) ; (X,0) ; (C,3) |] |] ;;


let move (_ : board) (Move (_, _, b)) = b ;;


let query_piece (bd : board) ((x, y) : (int * int)) : piece option =
  try
    Some (bd.(x).(y))
  with Invalid_argument e -> None ;;


let get_positions (p : piece) (bd : board) : ((int * int) list) =
  let accumulator = ref [] in
  let nrows = Array.length bd in
  let ncols = Array.length bd.(0) in
  for i=0 to nrows - 1 do
    for j=0 to ncols - 1 do
      if p = bd.(i).(j)
      then accumulator := (i, j) :: !accumulator
    done;
  done;
  !accumulator ;;


let rec updateArrayValues (newValue : 'a) (arr : 'a array array) (positions : (int * int) list)
    : unit = 
  match positions with
  |[]               -> ()
  |(i, j) :: tail   ->
      arr.(i).(j) <- newValue;
      (updateArrayValues [@ocaml.tailcall]) newValue arr tail ;;


let move_piece (bd : board) (p : piece) (dir : direction) : board option =  
  let new_board = Array.map Array.copy bd in
  let positions = get_positions p bd in
  let positions' = map (fun (x, y) -> (x + dir.drow, y)) positions in
  let positions'' = map (fun (x, y) -> (x, y + dir.dcol)) positions' in
  let pieces = map (query_piece bd) positions'' in
  let rec testPieces (p : piece option) (p_list : piece option list) : bool =
    match p_list with
    |[]            -> true
    |head :: tail  -> if head = p || head = Some (X, 0) 
		      then true && testPieces p tail
		      else false

  in
  if testPieces (Some p) pieces
  then (* if dir.drow = 0
       then let new_board' = shiftArrayRows dir.dcol new_board
			     begin tupleList begin union [positions; positions''] end end in
	    Some new_board'
       else *)
    begin 
    updateArrayValues (X, 0) new_board positions;
    updateArrayValues p new_board positions'';
    Some new_board;
    end
  else None ;;


let move_piece' ((p, d, b) : piece * direction * board) : board option = move_piece b p d ;; 


(* Here's a good helper function to help with the possible_moves function that comes next! *)
let all_combinations (x : 'a list) (y : 'b list) : ('a * 'b) list =
  let flatten (ls : 'e list list) : 'e list = 
    let rec flatten_helper (accumulator : 'e list) (lst : 'e list list) : 'e list = 
      match lst with 
      |[]            ->  List.rev accumulator 
      |head :: tail  ->  match head with 
                         |[]        ->  (flatten_helper [@ocaml.tailcall]) accumulator tail 
                         |hd :: tl  ->  (flatten_helper [@ocaml.tailcall]) (hd :: accumulator) (tl :: tail) in 
    flatten_helper [] ls 
  in 
  let rec all_combinations' (a : 'a list) (b : 'b list) (accum : ('a * 'b) list list) : ('a * 'b) list list =
    let rec helper (a' : 'a) (b' : 'b list) (accum : ('a * 'b) list) : ('a * 'b) list =
      match b' with
      |[]          ->  List.rev accum 
      |h :: t      ->  (helper [@ocaml.tailcall]) a' t ((a', h) :: accum) in
    match a with
    |[]         -> List.rev accum
    |h :: t     -> (all_combinations' [@ocaml.tailcall]) t b (helper h b [] :: accum) in
  flatten begin all_combinations' x y [] end ;;


let first (x, _, _) = x ;;

let second (_, y, _) = y ;;

(* let third (_, _, z) = z ;; *)


let possible_moves' (board : board) : move list =
  let up = {drow = -1; dcol = 0} in
  let down = {drow = 1; dcol = 0} in
  let right = {drow = 0; dcol = 1} in
  let left = {drow = 0; dcol = -1} in
  let all_directions = [up; down; right; left] in 
  let all_possible_moves =
    map (fun ((i, j), k) -> (i, j, k))
	     ( all_combinations (all_combinations all_pieces all_directions) [board] ) in
  let rec create_MoveList (x : (piece * direction * board) list) (acc : move list) : move list =
    match x with
    |[]             ->  acc 
    |head :: tail   ->  match move_piece' head with
			|None      ->  create_MoveList tail acc 
			|Some y    ->
                          let p = first head in
			  let d = second head in
			  (* let b = third head in *)
                          (create_MoveList [@ocaml.tailcall]) tail ((Move (p, d, y)) :: acc) in 
  create_MoveList all_possible_moves [] ;;


let possible_moves (board : board) : move list =
  let potential_moves = possible_moves' board in
  List.filter (fun (Move (x, y, z)) -> z <> board) potential_moves ;; 


let klotski : (board, move) puzzle = { move; possible_moves; final } ;;


let display_board (board : board) : unit =
  open_graph " 600x700";
  let nRows = Array.length board in
  let nCols = Array.length board.(0) in
  set_color 0x000000; 
  set_line_width 14;
  moveto 100 100;
  lineto 100 600;
  lineto 500 600;
  lineto 500 100;
  lineto 400 100;
  moveto 200 100;
  lineto 100 100;
  for i=0 to nRows - 1 do
    for j=0 to nCols - 1 do
      let row = 100 * (j+1) in
      let col = 600 - 100*(i+1) in 
      match board.(i).(j) with
      |(V, 0)    ->  set_color 0xFFFF00;
                     fill_rect row col 100 100;
      |(S, 0)    ->  set_color 0xFF0000;
                     fill_rect row col 100 100;
      |(V, 1)    ->  set_color 0x00FF00;
                     fill_rect row col 100 100;
      |(V, 2)    ->  set_color 0x980000;
                     fill_rect row col 100 100;
      |(H, 0)    ->  set_color 0x004099;
                     fill_rect row col 100 100;
      |(V, 3)    ->  set_color 0x2F7FFF;
                     fill_rect row col 100 100;
      |(C, 0)    ->  set_color 0xFFBEBE;
                     fill_rect row col 100 100;
      |(C, 1)    ->  set_color 0xBEFFBE;
                     fill_rect row col 100 100;
      |(C, 2)    ->  set_color 0xBEBEFF;
                     fill_rect row col 100 100;
      |(C, 3)    ->  set_color 0xC8C8C8;
                     fill_rect row col 100 100;
      |_         ->  set_color 0x000000;
                     fill_rect row col 100 100;
    done;
  done ;;


(* The following two functions are necessary to compare two different values of type piece_kind. *)
let (^<) (x : piece_kind) (y : piece_kind) : bool =
  match x, y with
  |X, (V|C|H|S)     -> true
  |V, (C|H|S)       -> true
  |C, (H|S)         -> true
  |H, S             -> true
  |_, _             -> false ;;


let (^>) (x : piece_kind) (y : piece_kind) : bool =
  match x, y with
  |S, (H|C|V|X)     ->  true
  |H, (C|V|X)       ->  true
  |C, (V|X)         ->  true
  |V, X             ->  true
  |_, _             ->  false ;;



let (<!) ((pk1, int1) : piece) ((pk2, int2) : piece) : bool =
  if pk1 ^< pk2
  then true
  else if pk1 ^> pk2
       then false
       else int1 < int2 ;;
          

let (>!) (p1 : piece) (p2 : piece) : bool =
  p2 <! p1 ;;


(* The next function generates a list of all the indices of a matrix with the condition that 
   the number of columns of the matrix is one less than the number of rows of the matrix.
   The argument "k" is the number of rows of the matrix with the assumption that the first 
   row has index 0. *)
let matrix_indices k =
  let rec indices' (m : int) (n : int) (upperLimit : int) : (int * int) list =
  if m > upperLimit
  then []
  else if (n mod upperLimit) = 0
       then (m + 1, n mod upperLimit) :: indices' (m + 1) (n + 1) upperLimit 
       else (m, n mod upperLimit) :: indices' m (n + 1) upperLimit in 
  let rec exceptLast (lst : (int * int) list) : (int * int) list =
    match lst with
    |[]      -> raise (failwith "Empty list!")
    |head :: []     -> []
    |head :: tail   -> head :: exceptLast tail in
  exceptLast (indices' (-1) 0 k) ;;



(* Similar to the previous function, "matrix_indices", but this time we don't have to assume 
   that the number of columns is one less than the number of rows.  This function is more general. 
   The user provides the number of rows and columns of the matrix.  The function returns all the 
   indices in row-major order with the assumption that the first row is numbered 0, and that the 
   first column is similarly numbered 0. *) 
let general_matrix_indices rows columns =
  let matrix_indices rows columns = 
    let f x = if x > rows - 1 
              then 0 
              else x + 1 in 
    let g y = if y > columns - 1 
              then 0 
              else y + 1 in 
    let rec h x y = 
      if x >= rows && y >= columns 
      then [] 
      else if y < columns 
           then (x, g y) :: h x (g y) 
           else (f x, g y) :: h (f x) (g y) in 
    let u x y = (x, y) :: h x y in u 0 0 in  
    matrix_indices (rows - 1) (columns - 1) ;;



(* This function can only be used if certain assumptions are true. 
   First assumption: board1 and board2 must have the same dimensions.
   Second assumption: The number of columns must be one less than the number of rows, 
   which of course is the case for the boards that are used to represent the Klotski Puzzle. 
 *)

exception Compare_Result of int ;;

let boardSet_Compare (board1 : board) (board2 : board) : int =
  let nRows = Array.length board1 in
  let nCols = Array.length board1.(0) in
  try
    for i=0 to nRows - 1 do
      for j=0 to nCols - 1 do
        if board1.(i).(j) <! board2.(i).(j)
        then raise (Compare_Result (-1))
        else if board1.(i).(j) >! board2.(i).(j)
             then raise (Compare_Result 1)
        done; done; 0
  with Compare_Result i  -> i ;;
                          

module BoardSet =
  Set.Make(
      struct
      type t = board
      let compare b1 b2 = boardSet_Compare b1 b2
      end
    ) ;;



(* The function below tests to see if all the boards in some board list can be found in 
   the board set. *)
let boards_inSet (boards : board list) (set : BoardSet.t) : bool =
  let last_board = last boards in
      BoardSet.mem last_board set ;; 


(* The function below adds all the boards in some board list to a board set. *)
let boards_Add (boards : board list) (set : BoardSet.t) : BoardSet.t =
          BoardSet.add (last boards) set ;;


let solve_klotski = solve_puzzle klotski {empty=BoardSet.empty; add=boards_Add; mem=boards_inSet} ;;

  
let initial_board_trivial =
  [| [| x  ; s  ; s  ; x  |] ;
     [| x  ; s  ; s  ; x  |] ;
     [| x  ; x ;  x ; x  |] ;
     [| x  ; x ;  x ; x  |] ;
     [| x  ; x  ; x  ; x  |] |] ;;


let initial_board_simpler =
  [| [|  s ; s  ; x ; x |] ;
     [|  s ; s  ; x ; x |] ;
     [|  h ; h  ; x ; x |] ;
     [| c0 ; x  ; c1; x |] ;
     [|  x ; x  ; x ; x |] |] ;;



let startTimer = Unix.gettimeofday () ;;

let board_list = solve_klotski initial_board_simpler ;;

let stopTimer = Unix.gettimeofday () ;;

Printf.printf "It took about %.6f seconds for my program to find a solution!" (stopTimer -. startTimer) ;;

print_endline "\n" ;; 

open_graph " 600x700" ;;
    
let repeat (element : 'a) (k : int) : 'a list =
  let rec repeat' (elem : 'a) (n : int) (acc : 'a list) : 'a list = 
    if n = 0
    then acc
    else (repeat' [@ocaml.tailcall]) elem (n - 1) (elem :: acc) in
  repeat' element k [] ;;
                                                   
List.map (List.iter (fun t -> display_board t; Unix.sleep 1))
         (repeat board_list 5) ;;



