(* OCaml functions written by Douglas Lewit of Oakton Community College 
and Northeastern Illinois University. *)

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


(* Tail-recursive user-defined reverse function on lists. *)
let rec reverse ?acc:((accum:'a list)=[]) (a_list : 'a list) : 'a list =
  match a_list with
  |[]            ->  accum
  |head :: tail  ->  (reverse [@ocaml.tailcall]) ~acc:(head :: accum) tail ;;


(* Tail-recursive map function on lists.  Using this because List.map 
   is not a tail-recursive function. *)
let map (f : 'a -> 'b) (a_list : 'a list) : 'b list =
  let rec map' (accum : 'b list) (f : 'a -> 'b) (a_list : 'a list) : 'b list = 
    match a_list with 
    |[]          -> List.rev accum 
    |h :: t      -> map' (f h :: accum) f t in 
  map' [] f a_list ;;


let rec loop (p : ('a -> bool)) (f : ('a -> 'a)) (x : 'a) : 'a =
  match p x with
  |true     ->  x
  |false    ->  (loop [@ocaml.tailcall]) p f (f x) ;;
  

let rec exists (p : ('a -> bool)) (ls : 'a list) : bool =
  match ls with
  |[]           -> false
  |head :: tail ->
    if p head
    then true
    else (exists [@ocaml.tailcall]) p tail ;;


let rec find (p : ('a -> bool)) (ls : 'a list) : 'a =
  match ls with
  |[]            -> raise NotFound
  |head :: tail  ->
    if p head
    then head
    else (find [@ocaml.tailcall]) p tail ;;
  

let flat_map (rel_function : 'e rel) (e_list : 'e list) : 'e list = 
  let rec flat_map' (accumulator : 'e list) (rel_function : 'e rel)
                    (e_list : 'e list) : 'e list =
    match e_list with
    |[]            -> accumulator
    |head :: tail  ->
      (flat_map' [@ocaml.tailcall])
        (List.rev_append (rel_function head) accumulator) rel_function tail
  in flat_map' [] rel_function e_list ;;


(* Here is Step #9 of the Klotski Puzzle solution guide from the OCaml MOOC. *)
let archive_map (opset : ('a, 'set) set_operations) (rel : 'a rel) ((s, l) : ('set * 'a list)) =
  let archive (x, y) z = if opset.mem z x then (x, y) else (opset.add z x, z :: y) in
  let possible_moves = flat_map rel l in 
  List.fold_left archive (s, []) possible_moves  ;;


let solve' (opset : ('a, 'set) set_operations) (rel : 'a rel)
           (predicate : 'a prop) (x : 'a) : 'a =
  let (s, l) =
    loop (fun (x, y) -> exists predicate y)
         (fun (x, y) -> archive_map opset rel (x, y))
         (opset.empty, [x])
  in find predicate l ;;


let e_rel_list (f : 'e rel) (e_list : 'e list) : 'e list list =
  let e_list' = f (List.hd e_list) in
  List.fold_left (fun x y -> (y :: e_list) :: x) [] e_list' ;;


(* Here is Step #11 of the Klotski Puzzle solution guide from the OCaml MOOC. *)
let solve_path' (opset : ('a list, 'set) set_operations)
                (rel : 'a rel) (predicate : 'a prop) (x : 'a) : 'a list =
  solve' opset (e_rel_list rel) (fun path -> predicate (List.hd path)) [x] ;;


let solve_puzzle (p : ('c, 'm) puzzle) (opset : ('c list, 's) set_operations)
                 (c : 'c) : 'c list =
  solve_path' opset (fun x -> map (p.move x) (p.possible_moves x))
              p.final c ;;


(* Here begins Part B of the Klotski Project. *)

type piece_kind = S|H|V|C|X ;;


type piece = piece_kind * int ;;


let x = (X, 0) and s = (S, 0) and h = (H, 0) ;;
let (c0, c1, c2, c3) = ((C, 0), (C, 1), (C, 2), (C, 3)) ;;
let (v0, v1, v2, v3) = ((V, 0), (V, 1), (V, 2), (V, 3)) ;;
let all_pieces : piece list = [ s; h; c0; c1; c2; c3; v0; v1; v2; v3 ] ;;


type board = piece array array ;;


let is_S0 (s : piece) : bool =
  match s with
  |(S, 0)     -> true
  |_          -> false ;;


let final (bd : board) : bool =
  let check1 = is_S0 bd.(3).(1) in
  let check2 = is_S0 bd.(3).(2) in
  let check3 = is_S0 bd.(4).(1) in
  let check4 = is_S0 bd.(4).(2) in
  check1 && check2 && check3 && check4 ;;



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


let pieceEquals ((pk1, int1) : piece) ((pk2, int2) : piece) : bool =
  pk1 = pk2 && int1 = int2 ;;


let get_positions (p : piece) (bd : board) : ((int * int) list) =
  let accumulator = ref [] in
  let nrows = Array.length bd in
  let ncols = Array.length bd.(0) in
  for i=0 to nrows - 1 do
    for j=0 to ncols - 1 do
      if pieceEquals p bd.(i).(j)
      then accumulator := (i, j) :: !accumulator
    done;
  done;
  !accumulator ;;


let rec updateArrayValues (newValue : 'a) (arr : 'a array array)
                          (positions : (int * int) list) : unit = 
  match positions with
  |[]               -> ()
  |(i, j) :: tail   ->
      arr.(i).(j) <- newValue;
      (updateArrayValues [@ocaml.tailcall]) newValue arr tail ;;


let move_piece (bd : board) (p : piece) (dir : direction) : board option =  
  let new_board = Array.map Array.copy bd in
  let positions = get_positions p bd in
  let positions' = map (fun (x, y) -> (x + dir.drow, y + dir.dcol)) positions
  in let pieces = map (query_piece bd) positions' in  
  let rec testPieces (p : piece) (p_list : piece option list) : bool =
    match p_list with
    |[]               -> true
    |Some p' :: tail  -> if pieceEquals p p' || pieceEquals p' (X, 0) 
			 then testPieces p tail
			 else false
    |None :: tail     -> false 
  in
  if testPieces p pieces
  then 
    begin 
      updateArrayValues (X, 0) new_board positions;
      updateArrayValues p new_board positions';
      Some new_board;
    end
  else None ;;


(* Here's a good helper function to help with the possible_moves 
   function that comes next! *)
let all_combinations (x : 'a list) (y : 'b list) : ('a * 'b) list =
  let flatten (ls : 'e list list) : 'e list = 
    let rec flatten_helper (accumulator : 'e list)
                           (lst : 'e list list) : 'e list = 
      match lst with 
      |[]            ->  accumulator 
      |head :: tail  ->
        match head with 
        |[]        ->  (flatten_helper [@ocaml.tailcall]) accumulator tail 
        |hd :: tl  ->  (flatten_helper [@ocaml.tailcall]) (hd :: accumulator)
                       (tl :: tail) in flatten_helper [] ls in 
  let rec all_combinations' (a : 'a list) (b : 'b list)
                            (accum : ('a * 'b) list list)
          : ('a * 'b) list list =
    let rec helper (a' : 'a) (b' : 'b list)
                   (accum : ('a * 'b) list) : ('a * 'b) list =
      match b' with
      |[]          ->  accum 
      |h :: t      ->  (helper [@ocaml.tailcall]) a' t ((a', h) :: accum) in
    match a with
    |[]         ->
      accum
    |h :: t     ->
      (all_combinations' [@ocaml.tailcall]) t b (helper h b [] :: accum)
  in flatten begin all_combinations' x y [] end ;;


let up = {drow = -1; dcol = 0} ;;
  
let down = {drow = 1; dcol = 0} ;;
  
let right = {drow = 0; dcol = 1} ;;
  
let left = {drow = 0; dcol = -1} ;;
  
let all_directions = [up; down; right; left] ;;

let all_moves = all_combinations all_pieces all_directions ;; 
  

(* The following two functions are necessary to compare two different values of type piece_kind. *)
let (<^>) (x : piece_kind) (y : piece_kind) : int =
  match x, y with
  |X, (V|C|H|S)     -> -1
  |V, (C|H|S)       -> -1
  |C, (H|S)         -> -1
  |H, S             -> -1 
  |S, (H|C|V|X)     ->  1
  |H, (C|V|X)       ->  1
  |C, (V|X)         ->  1
  |V, X             ->  1
  |_, _             ->  0 ;;



let (<!>) ((pk1, int1) : piece) ((pk2, int2) : piece) : int =
  let comparison = pk1 <^> pk2 
  in if comparison = 0
     then compare int1 int2
     else comparison ;;

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
	let comparison = board1.(i).(j) <!> board2.(i).(j) in  
        if comparison <> 0
        then raise (Compare_Result comparison)
      done;
    done; 0
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
  let first_board = List.hd boards in
      BoardSet.mem first_board set ;; 


(* The function below adds all the boards in some board list to a board set. *)
let boards_Add (boards : board list) (set : BoardSet.t) : BoardSet.t =
          BoardSet.add (List.hd boards) set ;;


let possible_moves' (board : board) : move list =  
  let rec create_MoveList (x : (piece * direction) list) (acc : move list)
          : move list =
    match x with
    |[]               ->  acc 
    |(p, d) :: tail   ->
      match move_piece board p d with
      |None      ->  create_MoveList tail acc 
      |Some y    ->
        (create_MoveList [@ocaml.tailcall]) tail ((Move (p, d, y)) :: acc) in 
  create_MoveList all_moves [] ;;


let possible_moves (board : board) : move list =
  let potential_moves = possible_moves' board in
  List.filter (fun (Move (x, y, z)) -> boardSet_Compare z board <> 0 )
              potential_moves ;; 


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
      let color =
      match board.(i).(j) with
      |(V, 0)    ->  0xFFFF00
      |(S, 0)    ->  0xFF0000
      |(V, 1)    ->  0x00FF00
      |(V, 2)    ->  0x980000
      |(H, 0)    ->  0x004099
      |(V, 3)    ->  0x2F7FFF
      |(C, 0)    ->  0xFFBEBE
      |(C, 1)    ->  0xBEFFBE
      |(C, 2)    ->  0xBEBEFF
      |(C, 3)    ->  0xA8A8A8
      |_         ->  0x000000
      in set_color color;
         fill_rect row col 100 100;
    done;
  done ;;


let solve_klotski =
  solve_puzzle klotski
               {empty=BoardSet.empty; add=boards_Add; mem=boards_inSet} ;;

  
let initial_board_trivial =
  [| [| x  ; s  ; s  ; x  |] ;
     [| x  ; s  ; s  ; x  |] ;
     [| x  ; x ;  x ; x  |] ;
     [| x  ; x ;  x ; x  |] ;
     [| x  ; x  ; x  ; x  |] |] ;;


let initial_board_simpler =
  [| [|  s ; s  ; x ; x |] ;
     [|  s ; s  ; h ; h |] ;
     [|  v3; x  ; v1; x |] ;
     [|  v3; v2 ; v1; x |] ;
     [|  x ; v2 ;  x; x |] |] ;;


let initial_board_simpler2 =
  [| [| c2 ; s  ; s  ; c1 |] ;
     [| c0 ; s  ; s  ; c3 |] ;
     [| v1 ; v2 ; v3 ; v0 |] ;
     [| v1 ; v2 ; v3 ; v0 |] ;
     [| x  ; x  ; x  ; x  |] |] ;;


let initial_board =
  [| [| v0 ; s  ; s  ; v1 |];
     [| v0 ; s  ; s  ; v1 |];
     [| v2 ; h  ; h  ; v3 |];
     [| v2 ; c0 ; c1 ; v3 |];
     [| c2 ; x  ; x  ; c3 |] |] ;;


let initial_board_minus_square =
  [| [| v0 ; s  ; s  ; v1 |];
     [| v0 ; s  ; s  ; v1 |];
     [| v2 ; h  ; h  ; v3 |];
     [| v2 ; c0 ; c1 ; v3 |];
     [| c2 ; x  ; x  ; x  |] |] ;;  


let initial_board_simpler3 =
  [| [| c0  ; s  ; s  ; x  |] ;
     [| c1  ; s  ; s  ; x  |] ;
     [| c2  ; h ;  h ; c3  |] ;
     [| x  ; x ;  x ; x  |] ;
     [| x  ; x  ; x  ; x  |] |] ;;


let repeat (element : 'a) (k : int) : 'a list =
  let rec repeat' (elem : 'a) (n : int) (acc : 'a list) : 'a list = 
    if n = 0
    then acc
    else (repeat' [@ocaml.tailcall]) elem (n - 1) (elem :: acc) in
  repeat' element k [] ;;
                                                   
