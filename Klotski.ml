(* OCaml functions written by Douglas Lewit of Oakton Community College and Northeastern Illinois University. 
   Everything in this program file is up-to-date as of March 2, 2018. *)


#require "graphics" ;;

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
  in List.map (fun t -> x @ [t]) elistList ;;

(* Here is Step #11 of the Klotski Puzzle solution guide from the OCaml MOOC. *)
let solve_path' (opset : ('a list, 'set) set_operations) (rel : 'a rel) (predicate : 'a prop) (x : 'a) : 'a list =
  solve' opset (e_rel_list1 rel) (fun path -> let last_of_path = last path
					      in predicate last_of_path) [x] ;;


let rec solve_puzzle (p : ('c, 'm) puzzle) (opset : ('c list, 's) set_operations) (c : 'c) : 'c list =
  solve_path' opset (fun x -> List.map (p.move x) (p.possible_moves x)) (p.final) c ;;


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


(* I thought this function might be helpful in case we need to start adding more elements to an array. *)
let makeNewArray (old_array : 'a array) (element : 'a) : 'a array =
  let length = Array.length old_array
  in let new_array = Array.make (length + 1) element in
     for i=0 to length - 1 do
       new_array.(i) <- old_array.(i)
     done; new_array ;;


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


(* I will need to use the shift_right and shift_left functions in the move_piece function that follows. *)
let shift_right (arr : 'a array) : 'a array =
  let length = Array.length arr in
  let last = arr.(length - 1) in
  for i=length - 1 downto 1 do
    arr.(i) <- arr.(i - 1)
  done; arr.(0) <- last; arr ;;


let shift_left (arr : 'a array) : 'a array =
  let length = Array.length arr in
  let first = arr.(0) in
  for i=0 to length - 2 do
    arr.(i) <- arr.(i + 1)
  done; arr.(length - 1) <- first; arr ;;


let shiftRight_subArray (arr : 'a array) (lower_index : int) (upper_index : int) : 'a array =
  if lower_index < 0 || upper_index > Array.length arr - 1
  then raise (Invalid_argument "Array index out of bounds exception.")
  else begin
      let section1 = Array.sub arr 0 lower_index in
      let section2 = Array.sub arr lower_index (upper_index - lower_index + 1) in
      let section3 = Array.sub arr (upper_index + 1) (Array.length arr - upper_index - 1) in
      Array.concat [Array.concat [section1; shift_right section2]; section3]
    end ;;


let shiftLeft_subArray (arr : 'a array) (lower_index : int) (upper_index : int) : 'a array =
  if lower_index < 0 || upper_index > Array.length arr - 1
  then raise (Invalid_argument "Array index out of bounds exception.")
  else begin
      let section1 = Array.sub arr 0 lower_index in
      let section2 = Array.sub arr lower_index (upper_index - lower_index + 1) in
      let section3 = Array.sub arr (upper_index + 1) (Array.length arr - upper_index - 1) in
      Array.concat [Array.concat [section1; shift_left section2]; section3]
    end ;;


(* May or may not need the transposeMatrix function for this project.  
   But there's no denying that it's a really cool function, and definitely 
   has some important practical applications. *)
let transposeMatrix (some_array : 'a array array) : 'a array array =
  let rec transposeList (some_list : 'a list list) : 'a list list =
    let rec transposeList' (some_list : 'a list list) : 'a list =
      try
	match some_list with
	|[]            ->  []
	|head :: tail  ->  (List.hd head) :: transposeList' tail
      with Failure f   ->  [] in
    try
      if some_list = []
      then []
      else (transposeList' some_list) :: (transposeList (List.map List.tl some_list))
    with Failure f   ->  [] in
  let some_list = Array.to_list (Array.map Array.to_list some_array) in
  let transposed = transposeList some_list in
  Array.of_list (List.map Array.of_list transposed) ;;


let tupleList (listOfPairs : ('a * 'a) list) : ('a * 'a list) list  =
  let rec tuple_List ?accumulator:(accum=[]) (n : 'a) (listOfPairs : ('a * 'a) list) : ('a * 'a list) =
    match listOfPairs with
    |[]             -> (n, List.sort compare accum)
    |head :: tail   -> let first = fst head in
		       let second = snd head in
		       if n = first
		       then tuple_List ~accumulator:(second :: accum) n tail
		       else tuple_List ~accumulator:(accum) n tail in
  let rec tuple_List' (n_list : 'a list) (listOfPairs : ('a * 'a) list) : ('a * 'a list) list =
    match n_list with
    |[]                 -> []
    |head :: tail       -> (tuple_List head listOfPairs) :: tuple_List' tail listOfPairs in
  let nList = List.sort_uniq compare begin List.fold_left (fun x (y, z) -> y :: x) [] listOfPairs end
  in tuple_List' nList listOfPairs ;;


(* This function was challenging because of the subtle mixture of imperative and functional programming combined 
   within a single function. *)
let rec shiftArrayRows (dir : int) (arr : 'a array array) (tuple_list : (int * int list) list) : 'a array array =
  match dir with
  |(-1)   -> let arrCopy = Array.map Array.copy arr in
	     begin
	       match tuple_list with
	       |[]              -> arrCopy
	       |head :: tail    ->
		 begin
		   let h = fst head in
		   let index1 = (List.hd (snd head)) in
		   let index2 = (last (snd head)) in
		   arrCopy.(h) <- shiftLeft_subArray arrCopy.(h) index1 index2
		 end;
		 shiftArrayRows dir arrCopy tail
	     end
  |1      -> let arrCopy = Array.map Array.copy arr in
	     begin
	       match tuple_list with
	       |[]              -> arrCopy
	       |head :: tail    ->
		 begin
		   let h = fst head in
		   let index1 = (List.hd (snd head)) in
		   let index2 = (last (snd head)) in
		   arrCopy.(h) <- shiftRight_subArray arrCopy.(h) index1 index2
		 end;
		 shiftArrayRows dir arrCopy tail
	     end
  |_      -> arr ;;



let rec updateArrayValues (newValue : 'a) (arr : 'a array array) (positions : (int * int) list)
    : 'a array array =
  let arrCopy = Array.map Array.copy arr in
  match positions with
  |[]               -> arrCopy
  |(i, j) :: tail   -> begin
		         arrCopy.(i).(j) <- newValue ;
		       end;  updateArrayValues newValue arrCopy tail ;;


let move_piece (bd : board) (p : piece) (dir : direction) : board option =  
  let new_board = Array.map Array.copy bd in
  let positions = get_positions p bd in
  let positions' = List.map (fun (x, y) -> (x + dir.drow, y)) positions in
  let positions'' = List.map (fun (x, y) -> (x, y + dir.dcol)) positions' in
  let pieces = List.map (query_piece bd) positions'' in
  let rec testPieces (p : piece option) (p_list : piece option list) : bool =
    match p_list with
    |[]            -> true
    |head :: tail  -> if head = p || head = Some (X, 0)
		      then true && testPieces p tail
		      else false

  in
  if testPieces (Some p) pieces
  then if dir.drow = 0
       then let new_board' = shiftArrayRows dir.dcol new_board
			     begin tupleList begin union [positions; positions''] end end in
	    Some new_board'
       else 
	   let new_board' = updateArrayValues (X, 0) new_board positions in
	   let new_board'' = updateArrayValues p new_board' positions'' in
	   Some new_board''
  else None ;;


let move_piece' ((p, d, b) : piece * direction * board) : board option = move_piece b p d ;; 


(* Here's a good helper function to help with the possible_moves function that comes next! *)
let all_combinations (x : 'a list) (y : 'b list) : ('a * 'b) list =
  let rec all_combinations' (a : 'a list) (b : 'b list) : ('a * 'b) list list =
    let rec helper (a' : 'a) (b' : 'b list) : ('a * 'b) list =
      match b' with
      |[]          ->  []
      |h :: t      -> (a', h) :: helper a' t in
    match a with
    |[]         -> []
    |h :: t     -> (helper h b) :: all_combinations' t b in
  List.flatten begin all_combinations' x y end ;;


let first (x, _, _) = x ;;

let second (_, y, _) = y ;;

(* let third (_, _, z) = z ;; *)


let possible_moves (board : board) : move list =
  let up = {drow = -1; dcol = 0} in
  let down = {drow = 1; dcol = 0} in
  let right = {drow = 0; dcol = 1} in
  let left = {drow = 0; dcol = -1} in
  let all_directions = [up; down; right; left] in 
  let all_possible_moves =
    List.map (fun ((i, j), k) -> (i, j, k))
	     ( all_combinations (all_combinations all_pieces all_directions) [board] ) in
  let rec create_MoveList (x : (piece * direction * board) list) : move list =
    match x with
    |[]             ->  []
    |head :: tail   ->  match move_piece' head with
			|None      ->  create_MoveList tail
			|Some y    ->  let p = first head in
				       let d = second head in
				       (* let b = third head in *)
				       (Move (p, d, y)) :: create_MoveList tail in 
  create_MoveList all_possible_moves ;;


let klotski : (board, move) puzzle = { move; possible_moves; final } ;;


let display_board (board : board) : unit =
  open_graph " 600x700";
  let nRows = Array.length board in
  let nCols = Array.length board.(0) in
  set_line_width 10;
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



