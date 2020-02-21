open Game

type tile = string* ((int*int) list)

type t = {
  board: Game.piece option array array;
  mutable turn: bool;
  white_moves: tile list;
  black_moves: tile list;
  mutable black_enpassant: bool ref;
  mutable white_enpassant: bool ref;
  white_castle: bool*bool;
  black_castle: bool*bool
}

(** [copy_board board] takes a matrix and returns the a seperate but equal
    version of [board]*)
let copy_board board = 
  let new_arr = Array.make_matrix 8 8 None in
  for i = 0 to (Array.length board) - 1 do
    for j = 0 to (Array.length board.(0)) - 1 do
      new_arr.(i).(j) <- board.(i).(j)
    done

  done; new_arr

(** [copt_t t] returns [t] with a copy of it's board*)
let copy_t t ={
  board=copy_board (t.board); 
  turn = t.turn;
  white_moves = t.white_moves;
  black_moves = t.black_moves;
  white_enpassant = t.white_enpassant;
  black_enpassant = t.black_enpassant;
  white_castle = t.white_castle;
  black_castle = t.black_castle
}

(** [place_pieces board] takes in a empty matrix and places the initial 
    location of chess pieces*)
let place_pieces board = 
  board.(0).(0) <- Some Game.white_rook;
  board.(0).(7) <- Some Game.white_rook;
  board.(0).(2) <- Some Game.white_bishop;
  board.(0).(5) <- Some Game.white_bishop;
  board.(0).(1) <- Some Game.white_knight;
  board.(0).(6) <- Some Game.white_knight;
  board.(0).(3) <- Some Game.white_queen;
  board.(0).(4) <- Some Game.white_king;
  board.(7).(0) <- Some Game.black_rook;
  board.(7).(7) <- Some Game.black_rook;
  board.(7).(2) <- Some Game.black_bishop;
  board.(7).(5) <- Some Game.black_bishop;
  board.(7).(1) <- Some Game.black_knight;
  board.(7).(6) <- Some Game.black_knight;
  board.(7).(3) <- Some Game.black_queen;
  board.(7).(4) <- Some Game.black_king;
  for i = 0 to fst Game.board-1 do 
    board.(1).(i) <- Some Game.white_pawn;
    board.(6).(i) <- Some Game.black_pawn
  done;
  board


let get_turn state = 
  state.turn

(** [string_of_turn turn] is a helper where it takes in a bool and returns 
    "white" if [turn] is true and "black" other wise*)
let string_of_turn turn = 
  if turn then "White" else "Black" 


let get_str_turn state = 
  string_of_turn (get_turn state)

(** [current_board t] return's [t]'s board element*)
let current_board t = 
  t.board

(** [piece_at t row col] returns the option piece at the row [row] and col [col]
    in [t]'s board at*)
let piece_at t row col = 
  t.board.(row).(col)

type result = Legal of t|Illegal

exception Invalid_square of (int * int)

(** [get t coord] returns the piece at a given coordinate*)
let get t coord =
  let piece = 
    t.(fst coord).(snd coord) 
  in
  piece


(** [place t first second piece] removes the [piece] from location [first] and 
    places that piece in the location [second]*)
let place t first second piece = 
  t.(fst first).(snd first) <- None;
  t.(fst second).(snd second) <- Some piece; 
  t

(** [vector (a,b) (c,d)] returns the vector that points from the initial 
    location [a,b] to the final location [c,d]*)
let vector ((a,b): int*int) ((c,d): int*int) = 
  (c-a,d-b) 


(** [set_black_enpassant t] sets [t]'s black_enpassant bool to be true and 
    returns it*)
let set_black_enpassant t = 
  t.black_enpassant:=true;
  t.white_enpassant:=false;
  !(t.black_enpassant)

(** [set_white_enpassant t] sets [t]'s white_enpassant bool to be true and 
    returns it*)
let set_white_enpassant t = 
  t.white_enpassant:=true;
  t.black_enpassant:=false;
  !(t.white_enpassant)

(** [unset_black_enpassant t row col] sets [t]'s black_enpassant to false and 
    performs enpassant*)
let unset_black_enpassant t row col = 
  t.black_enpassant :=false;
  (t.board).(row).(col) <- None;
  true

(** [unset_black_enpassant t row col] sets [t]'s black_enpassant to false and 
    performs enpassant*)
let unset_white_enpassant t row col = 
  t.white_enpassant :=false;
  (t.board).(row).(col) <- None; 
  true

(** [get_name piece] returns the [piece]'s name*)
let get_name piece = 
  match piece with
  | None -> ""
  | Some p -> Game.piece_name p

(** [check_pawn_move t current final p] returns true if the pawn movement was 
    valid according to chess rules*)
let check_pawn_move (t:t) current final p= 
  let (a,b) = vector current final in 
  if (abs a, b) = (1,0)  
  then if (t.board).(fst final).(snd final) == None 
    then true else false else 
  if (abs a,b) = (2,0)  
  then if Game.piece_color p  
    then if (fst current)==1 && 
            (t.board).(fst final).(snd final) == None 
      then set_black_enpassant t else false else
    if (fst current)=6 && 
       (t.board).(fst final).(snd final) == None 
    then set_white_enpassant t else false else
  if (abs a,abs b) = (1,1) && 
     (t.board).(fst final).(snd final) != None 
  then true else
  if (abs a, abs b) = (1,1) &&
     !(t.black_enpassant) 
  then if fst current = 3 &&
          get_name ((t.board).((fst current)).((snd current)+b)) = "WP " 
    then unset_black_enpassant t ((fst current)) ((snd current)+b) 
    else false else
  if (abs a, abs b) = (1,1) &&
     !(t.white_enpassant)  
  then if fst current = 4 &&
          get_name ((t.board).((fst current)).((snd current)+b)) = "BP " 
    then unset_white_enpassant t ((fst current)) ((snd current)+b) 
    else false
  else false

(** [check_block_row t row col1 col2] checks to see if in [t]'s board, the path 
    from [col1] to [col2] in [row] is blocked by any piece*)
let rec check_block_row t row col1 col2 = 
  if col1 = col2 then true else
    match get t.board (row, col1) with
    |None -> check_block_row t row (col1 + 1) col2
    |Some _ -> false

(** [check_block_row t col row1 row2] checks to see if in [t]'s board, the path 
    from [row1] to [row2] in [col] is blocked by any piece*)
let rec check_block_col t col row1 row2 = 
  if row1 = row2 then true else
    match get t.board (row1, col) with
    |None -> check_block_col t col (row1 + 1) row2
    |Some _ -> false

(** [check_diag1 t row1 col1 row2 col2] check to see if the diagonal from SW to
    NE is not blocked by any piece*)
let rec check_diag1 t row1 col1 row2 col2 =
  if row1 = row2 && col1 = col2 then true else
    match get t.board (row1, col1) with
    |None -> check_diag1 t (row1+1) (col1+1) row2 col2
    |Some _ -> false

(** [check_diag1 t row1 col1 row2 col2] check to see if the diagonal from SE to
    NW is not blocked by any piece*)
let rec check_diag2 t row1 col1 row2 col2 =
  if row1 = row2 && col1 = col2 then true else
    match get t.board (row1, col1) with
    |None -> check_diag2 t (row1+1) (col1-1) row2 col2
    |Some _ -> false

(** [check_block_move t (row1, col1) (row2, col2)] process the movement 
    and makes sure that the movement is possible with nothing blocking*)
let check_block_move t (row1, col1) (row2, col2) = 
  if row1 = row2 then 
    if col1 < col2  
    then check_block_row t row1 (col1 + 1) col2 
    else check_block_row t row1 (col2+1) col1
  else if col1 = col2 
  then if row1 < row2  
    then check_block_col t col1 (row1 + 1) row2 
    else check_block_col t col1 (row2+1) row1
  else if row2-row1 = col2-col1 
  then if row1<row2 
    then check_diag1 t (row1+1) (col1+1) row2 col2
    else check_diag1 t (row2+1) (col2+1) row1 col1
  else if abs (row2-row1) = abs (col2-col1) 
  then if row1<row2 
    then check_diag2 t (row1+1) (col1-1) row2 col2 
    else check_diag2 t (row2+1) (col2-1) row1 col1 
  else true

(** [check_own_piece t (row1, col1) (row1, col1)] checks to see if the player is
    attacking an enemy piece or an ally piece*)
let check_own_piece t (row1, col1) (row2, col2) = 
  match get t.board (row1, col1) with
  |None -> failwith "Fatal Error"
  |Some p -> 
    match get t.board (row2, col2) with
    |None -> true
    |Some p2 -> piece_color p != piece_color p2

let black_king_alive st = 
  let rec black_king_pos_h t row col = 
    (match get t.board (row,col) with
     | Some p -> if piece_name p = "BK " then true
       else if col > 0 then black_king_pos_h t row (col-1)
       else if row > 0 then black_king_pos_h t (row - 1) 7
       else false
     | None -> if col > 0 then black_king_pos_h t row (col-1)
       else if row > 0 then black_king_pos_h t (row - 1) 7
       else false)
  in black_king_pos_h st 7 7

let white_king_alive st = 
  let rec white_king_pos_h t row col = 
    (match get t.board (row,col) with
     | Some p -> if piece_name p = "BK " then true
       else if col > 0 then white_king_pos_h t row (col-1)
       else if row > 0 then white_king_pos_h t (row - 1) 7
       else false
     | None -> if col > 0 then white_king_pos_h t row (col-1)
       else if row > 0 then white_king_pos_h t (row - 1) 7
       else false)
  in white_king_pos_h st 7 7

let black_king_pos t = 
  let rec black_king_pos_h t row col = 
    match get t.board (row,col) with
    | Some p -> 
      if piece_name p = "BK " 
      then (row,col) 
      else if col > 0 then black_king_pos_h t row (col-1)
      else if row > 0 then black_king_pos_h t (row - 1) 7
      else failwith "Fatal Error"
    | None -> 
      if col > 0 
      then black_king_pos_h t row (col-1)
      else if row > 0 then black_king_pos_h t (row - 1) 7
      else failwith "Fatal Error"
  in black_king_pos_h t 7 7

let white_king_pos t = 
  let rec white_king_pos_h t row col = 
    (match get t.board (row,col) with
     | Some p -> 
       if piece_name p = "WK " 
       then (row,col) 
       else if col > 0 then white_king_pos_h t row (col-1)
       else if row > 0 then white_king_pos_h t (row - 1) 7
       else failwith "Fatal Error"
     | None ->
       if col > 0 
       then white_king_pos_h t row (col-1)
       else if row > 0 then white_king_pos_h t (row - 1) 7
       else failwith "Fatal Error")
  in white_king_pos_h t 7 7

(** [keep_enpassantinfo t white black bool] makes sure that enpassant info is 
    maintained while checking various moves. Returns bool to keep logic*)
let keep_enpassantinfo t white black (bool:bool) = 
  t.white_enpassant := white;
  t.black_enpassant := black; 
  bool

(** [check_possible_move curr_b current final] checks to see if the movement is
    possible in [curr_b]'s board*)
let check_possible_move (curr_b:t) (current) (final) =
  match get curr_b.board current with 
  |None -> false
  |Some p -> 
    let black_passant = !(curr_b.black_enpassant) in
    let white_passant = !(curr_b.white_enpassant) in
    if (Game.piece_name p = "WP " || Game.piece_name p = "BP ") then 
      if check_pawn_move curr_b current final p then 
        if Game.valid_move p (vector current final) 
        && check_block_move curr_b current final
        && check_own_piece curr_b current final
        then keep_enpassantinfo curr_b white_passant black_passant true
        else keep_enpassantinfo curr_b white_passant black_passant false
      else keep_enpassantinfo curr_b white_passant black_passant false
    else 
    if Game.valid_move p (vector current final) 
    && check_block_move curr_b current final
    && check_own_piece curr_b current final
    then true
    else false

let white_check t pos=
  (**let white_k_pos = white_king_pos t in*)
  let rec white_check_h t row col = 
    ( match get t.board (row,col) with
      | Some p -> 
        if not (piece_color p) then 
          if check_possible_move t (row,col) pos then true
          else if col > 0 then white_check_h t row (col-1)
          else if row > 0 then white_check_h t (row - 1) 7
          else false
        else if col > 0 then white_check_h t row (col-1)
        else if row > 0 then white_check_h t (row - 1) 7
        else false
      | None -> 
        if col > 0 then white_check_h t row (col-1)
        else if row > 0 then white_check_h t (row - 1) 7
        else false)
  in white_check_h t 7 7

let black_check t pos =
  (*let black_k_pos = black_king_pos t in*)
  let rec black_check_h t row col = 
    ( match get t.board (row,col) with
      | Some p -> 
        if (piece_color p) then 
          if check_possible_move t (row,col) pos then true
          else if col > 0 then black_check_h t row (col-1)
          else if row > 0 then black_check_h t (row - 1) 7
          else false
        else if col > 0 then black_check_h t row (col-1)
        else if row > 0 then black_check_h t (row - 1) 7
        else false
      | None -> 
        if col > 0 then black_check_h t row (col-1)
        else if row > 0 then black_check_h t (row - 1) 7
        else false)
  in black_check_h t 7 7

(* [check_legal_move piece curr_b (c1,c2) (f1,f2)] takes in a game piece [piece], 
   a state [t], a current position of [(c1,c2]), and desired destination 
   [(f1,f2)] and checks while returning a bool depending on if the move can be 
   done according to the rules of chess.*)
let check_legal_move color piece (curr_b:t) (c1,c2) (f1,f2) : bool = 
  if (Game.piece_name piece = "WP " || Game.piece_name piece = "BP ") then 
    let black_passant = !(curr_b.black_enpassant) in
    let white_passant = !(curr_b.white_enpassant) in
    if check_pawn_move curr_b (c1,c2) (f1,f2) piece then 
      if Game.valid_move piece (vector (c1,c2) (f1,f2)) 
      && Game.piece_color piece = color
      && check_block_move curr_b (c1,c2) (f1,f2)
      && check_own_piece curr_b (c1,c2) (f1,f2)
      then keep_enpassantinfo curr_b white_passant black_passant true
      else keep_enpassantinfo curr_b white_passant black_passant false
    else keep_enpassantinfo curr_b white_passant black_passant false
  else 
  if Game.valid_move piece (vector (c1,c2) (f1,f2)) 
  && Game.piece_color piece = color
  && check_block_move curr_b (c1,c2) (f1,f2)
  && check_own_piece curr_b (c1,c2) (f1,f2)
  then true
  else false

(* [to_list list] takes a list of (int*int) that corrisponds to a location in 
   the array representing the chess board and prints the values out. Should be
   removed in the final iternation of the project.*)
let rec to_list list =
  match list with
  |[] -> ()
  |(a,b)::t ->
    print_string "(";
    print_int(a);
    print_string(",");
    print_int(b);
    print_string ") ";
    to_list t

(* [find_piece_move x curb current] takes in a game piece [x] with the current
   state [t] and the current position [currnet]. It then proceeds to go through
   the whole board to see if the piece can reach that place or not and stores
   the place it can go to in a list.*)
let find_piece_move (color:bool) (x:Game.piece) (curb:t) 
    (current:(int*int)): (int*int) list=
  let list = ref [] in
  for r = 0 to fst Game.board-1 do 
    for c = 0 to snd Game.board-1 do
      if (check_legal_move color x curb current (r,c)) 
      then list:=(r,c)::!list 
      else ();
    done; 
  done;
  list:=current::!list;
  (*to_list !list;print_endline(" ");*)
  !list

(** [possible_moves_list_white t] returns all the possible moves for white*)
let possible_moves_list_white (t:t): tile list =
  (** [runningthrough board] goes through the [board] looking for white pieces
      to add all the moves of that piece to the possible moves list*)
  let runningthrough board=
    let list = ref [] in
    for r = 0 to fst Game.board-1 do 
      for c = 0 to snd Game.board-1 do
        match board.(r).(c) with
        | Some x-> 
          if (Game.piece_color x)=true then 
            list:= ((piece_name x,(find_piece_move true x t (r,c))):: !list)
        | None-> 
          ();
      done; 
    done; 
    !list
  in
  let current_board=t.board in
  runningthrough current_board


(** [possible_moves_list_black t] returns all the possible moves for black*)
let possible_moves_list_black (t:t): tile list =
  (** [runningthrough board] goes through the [board] looking for black pieces
        to add all the moves of that piece to the possible moves list*)
  let runningthrough board=
    let list = ref [] in
    for r = 0 to fst Game.board-1 do 
      for c = 0 to snd Game.board-1 do
        match board.(r).(c) with
        | Some x-> 
          if (Game.piece_color x)=false 
          then list:= ((piece_name x,(find_piece_move false x t (r,c))):: !list)
        | None->
          ();
      done; 
    done; 
    !list
  in
  let current_board=t.board in
  runningthrough current_board

(** [to_coord_list list block] extracts the coordinates of the tile list [list]
    and will include the king if the bool [block] is false*)
let rec to_coord_list (list: tile list) block = 
  if block then
    match list with 
    |[] -> []
    |(a,b) :: t -> 
      if a <> "WK " || a <> "BK "
      then List.append b (to_coord_list t block) 
      else to_coord_list t block
  else
    match list with 
    |[] -> []
    |(a,b) :: t -> 
      List.append b (to_coord_list t block)

(** [take_king list] takes out the king tile from a list of tiles.*)
let rec take_king (list: tile list) :tile list = 
  match list with 
  |[] -> []
  |(a,b) :: t -> 
    if a = "WK " || a = "BK "
    then take_king t
    else (a,b)::(take_king t)

(** [white_king_moves list] returns the coordinates of moves in list for the 
    white king*)
let rec white_king_moves (list: tile list) = 
  match list with 
  |[] -> []
  | (a,b)::t -> 
    if a = "WK " 
    then b 
    else white_king_moves t

(** [black_king_moves list] returns the coordinates of moves in [list] for the 
    black king*)
let rec black_king_moves (list:tile list) = 
  match list with 
  |[] -> []
  | (a,b)::t -> 
    if a = "BK " 
    then b 
    else black_king_moves t

(** [can_king_move movement attacked] checks to see if the all king's moves 
    [movement] are being attacked by the list [attacked]*)
let rec can_king_move movement attacked = 
  match movement with 
  | [] -> false 
  | h :: t -> 
    if List.mem h attacked 
    then can_king_move t attacked 
    else true

(** [get_black_passant t] returns [t]'s black_enpassant*)
let get_black_passant t = 
  t.black_enpassant

(** [get_white_passant t] returns [t]'s white_enpassant*)
let get_white_passant t = 
  t.white_enpassant

let right_rook_castable_white t =
  match t.white_castle with
  | (_,b) -> b

let left_rook_castable_white t =
  match t.white_castle with
  | (b,_) -> b

let right_rook_castable_black t =
  match t.black_castle with
  | (_,b) -> b

let left_rook_castable_black t =
  match t.black_castle with
  | (b,_) -> b

let make_rook_board t current final p =
  let copyt = copy_t t in
  if t.turn then
    if current=(0,0)
    then Legal {board = place t.board current final p; 
                turn = not (t.turn);
                white_moves = possible_moves_list_white copyt; 
                black_moves = possible_moves_list_black copyt;
                black_enpassant = get_black_passant t; 
                white_enpassant = get_white_passant t;
                white_castle=(false,right_rook_castable_white t);
                black_castle=t.black_castle}
    else if current=(0,7) 
    then Legal {board = place t.board current final p; 
                turn = not (t.turn);
                white_moves = possible_moves_list_white copyt; 
                black_moves = possible_moves_list_black copyt;
                black_enpassant = get_black_passant t; 
                white_enpassant = get_white_passant t;
                white_castle=(left_rook_castable_white t,false);
                black_castle=t.black_castle}
    else Legal {board = place t.board current final p; 
                turn = not (t.turn);
                white_moves = possible_moves_list_white copyt; 
                black_moves = possible_moves_list_black copyt;
                black_enpassant = get_black_passant t; 
                white_enpassant = get_white_passant t;
                white_castle=t.white_castle;
                black_castle=t.black_castle}
  else
  if current=(7,0)
  then Legal {board = place t.board current final p; 
              turn = not (t.turn);
              white_moves = possible_moves_list_white copyt; 
              black_moves = possible_moves_list_black copyt;
              black_enpassant = get_black_passant t; 
              white_enpassant = get_white_passant t;
              white_castle=t.white_castle;
              black_castle=(false,right_rook_castable_black t)}
  else if current=(7,7) 
  then Legal {board = place t.board current final p; 
              turn = not (t.turn);
              white_moves = possible_moves_list_white copyt; 
              black_moves = possible_moves_list_black copyt;
              black_enpassant = get_black_passant t; 
              white_enpassant = get_white_passant t;
              white_castle=t.white_castle;
              black_castle=(left_rook_castable_black t,false)}
  else Legal {board = place t.board current final p; 
              turn = not (t.turn);
              white_moves = possible_moves_list_white copyt; 
              black_moves = possible_moves_list_black copyt;
              black_enpassant = get_black_passant t; 
              white_enpassant = get_white_passant t;
              white_castle=t.white_castle;
              black_castle=t.black_castle}

(* Does the castle move to the board. *)
let castle_place (t:t) rfirst rsecond kfirst ksecond =
  let get_rook t =
    match get t.board rfirst with
    |None -> failwith("Error")
    |Some p -> Some p
  in
  let get_king t =
    match get t.board kfirst with
    |None -> failwith("Error")
    |Some p -> Some p
  in
  let rook=get_rook t in
  let king=get_king t in
  t.board.(fst rfirst).(snd rfirst) <- None;
  t.board.(fst rsecond).(snd rsecond) <- rook;
  t.board.(fst kfirst).(snd kfirst) <- None;
  t.board.(fst ksecond).(snd ksecond) <- king; 
  t.board

let rec check_not_attacked_list (moves:(int*int) list) (tiles:(int*int) list)=
  match tiles with
  | [] -> true
  | a::t -> 
    if List.mem a moves 
    then false 
    else check_not_attacked_list moves t

let castle t side =
  let copyt = copy_t t in
  let turn=t.turn in
  if turn
  then begin
    match side with
    | "left" -> 
      if left_rook_castable_white t 
      then (if check_block_move t (0,0) (0,4) 
            && not (white_check t (0,4))
            && check_not_attacked_list 
                 (to_coord_list (possible_moves_list_black copyt) false) 
                 [(0,2);(0,3)] then 
              Legal {board = castle_place t (0,0) (0,3) (0,4) (0,2); 
                     turn = not (t.turn);
                     white_moves = possible_moves_list_white copyt; 
                     black_moves = possible_moves_list_black copyt;
                     black_enpassant = get_black_passant t; 
                     white_enpassant = get_white_passant t;
                     white_castle=(false,false);
                     black_castle=t.black_castle}
            else Illegal)
      else Illegal
    | "right" -> 
      if right_rook_castable_white t 
      then (if check_block_move t (0,7) (0,4)
            && not (white_check t (0,4))
            && check_not_attacked_list 
                 (to_coord_list (possible_moves_list_black copyt) false) 
                 [(0,5);(0,6)] then 
              Legal {board = castle_place t (0,7) (0,5) (0,4) (0,6); 
                     turn = not (t.turn);
                     white_moves = possible_moves_list_white copyt; 
                     black_moves = possible_moves_list_black copyt;
                     black_enpassant = get_black_passant t; 
                     white_enpassant = get_white_passant t;
                     white_castle=(false,false);
                     black_castle=t.black_castle}
            else Illegal)
      else Illegal
    | _ -> Illegal
  end
  else begin
    match side with
    | "left" -> 
      if left_rook_castable_black t 
      then (if check_block_move t (7,0) (7,4) 
            && not (black_check t (7,4))
            && check_not_attacked_list 
                 (to_coord_list (possible_moves_list_white copyt) false) 
                 [(7,2);(7,3)] then 
              Legal {board = castle_place t (7,0) (7,3) (7,4) (7,2); 
                     turn = not (t.turn);
                     white_moves = possible_moves_list_white copyt; 
                     black_moves = possible_moves_list_black copyt;
                     black_enpassant = get_black_passant t; 
                     white_enpassant = get_white_passant t;
                     white_castle=t.white_castle;
                     black_castle=(false,false)}
            else Illegal)
      else Illegal
    | "right" -> 
      if right_rook_castable_black t 
      then (if check_block_move t (7,7) (7,4)
            && not (black_check t (7,4))
            && check_not_attacked_list 
                 (to_coord_list (possible_moves_list_white copyt) false) 
                 [(7,5);(7,6)] then 
              Legal {board = castle_place t (7,7) (7,5) (7,4) (7,6); 
                     turn = not (t.turn);
                     white_moves = possible_moves_list_white copyt; 
                     black_moves = possible_moves_list_black copyt;
                     black_enpassant = get_black_passant t; 
                     white_enpassant = get_white_passant t;
                     white_castle=t.white_castle;
                     black_castle=(false,false)}
            else Illegal)
      else Illegal
    | _ -> Illegal
  end

let queen_place t first second bool = 
  if bool then
    (t.(fst first).(snd first) <- None;
     t.(fst second).(snd second) <- Some Game.white_queen; 
     t)
  else 
    (t.(fst first).(snd first) <- None;
     t.(fst second).(snd second) <- Some Game.black_queen; 
     t)

let pawn_to_queen t current final copyt p=
  if t.turn  
  then if (fst final)=7 then
      Legal {board = queen_place t.board current final t.turn; 
             turn = not (t.turn);
             white_moves = possible_moves_list_white copyt; 
             black_moves = possible_moves_list_black copyt; 
             black_enpassant = get_black_passant t; 
             white_enpassant = get_white_passant t;
             white_castle=t.white_castle;
             black_castle=t.black_castle}
    else Legal {board = place t.board current final p; 
                turn = not (t.turn);
                white_moves = possible_moves_list_white copyt; 
                black_moves = possible_moves_list_black copyt; 
                black_enpassant = get_black_passant t; 
                white_enpassant = get_white_passant t;
                white_castle=t.white_castle;
                black_castle=t.black_castle}
  else if (fst final)=0 then
    Legal {board = queen_place t.board current final t.turn; 
           turn = not (t.turn);
           white_moves = possible_moves_list_white copyt; 
           black_moves = possible_moves_list_black copyt; 
           black_enpassant = get_black_passant t; 
           white_enpassant = get_white_passant t;
           white_castle=t.white_castle;
           black_castle=t.black_castle}
  else Legal {board = place t.board current final p; 
              turn = not (t.turn);
              white_moves = possible_moves_list_white copyt; 
              black_moves = possible_moves_list_black copyt; 
              black_enpassant = get_black_passant t; 
              white_enpassant = get_white_passant t;
              white_castle=t.white_castle;
              black_castle=t.black_castle}

(* Move when we get the chance to use the and keyword. *)
let move t current final = 
  let copyt = copy_t t in
  try
    match get t.board current with 
    |None -> Illegal
    |Some p -> 
      if (Game.piece_name p = "WP " || Game.piece_name p = "BP ") then 
        if Game.valid_move p (vector current final) 
        && Game.piece_color p = t.turn
        && check_block_move t current final
        && check_own_piece t current final then 
          if check_pawn_move t current final p 
          then pawn_to_queen t current final copyt p 
          else Illegal
        else Illegal
      else if (Game.piece_name p = "WR " || Game.piece_name p = "BR ") then
        if Game.valid_move p (vector current final) 
        && Game.piece_color p = t.turn
        && check_block_move t current final
        && check_own_piece t current final
        then
          make_rook_board t current final p
        else Illegal
      else if (Game.piece_name p = "WK " || Game.piece_name p = "BK ") then
        if final=(0,2) || final= (0,6) || final=(7,2) || final=(7,6)
        then if final=(0,2) || final=(7,2) 
          then castle t "left" else castle t "right"
        else if Game.valid_move p (vector current final) 
             && Game.piece_color p = t.turn
             && check_block_move t current final
             && check_own_piece t current final
        then if t.turn= true 
          then
            Legal {board = place t.board current final p; 
                   turn = not (t.turn);
                   white_moves = possible_moves_list_white copyt; 
                   black_moves = possible_moves_list_black copyt;
                   black_enpassant = get_black_passant t;
                   white_enpassant = get_white_passant t;
                   white_castle=(false,false);
                   black_castle=t.black_castle}
          else
            Legal {board = place t.board current final p; 
                   turn = not (t.turn);
                   white_moves = possible_moves_list_white copyt; 
                   black_moves = possible_moves_list_black copyt;
                   black_enpassant = get_black_passant t; 
                   white_enpassant = get_white_passant t;
                   white_castle=t.white_castle;
                   black_castle=(false,false)}
        else Illegal
      else 
      if Game.valid_move p (vector current final) 
      && Game.piece_color p = t.turn
      && check_block_move t current final
      && check_own_piece t current final
      then Legal {board = place t.board current final p; 
                  turn = not (t.turn);
                  white_moves = possible_moves_list_white copyt; 
                  black_moves = possible_moves_list_black copyt;
                  black_enpassant = ref false; white_enpassant = ref false;
                  white_castle=t.white_castle;
                  black_castle=t.black_castle}
      else Illegal
  with Invalid_argument (e) -> Illegal

(* Sees if a piece can attack a certain thing *)
let rec possible_attacking_tile (a1,a2) = function
  | [] -> false
  | (m1,m2)::t -> 
    if a1=m1 && a2=m2 then true 
    else possible_attacking_tile (a1,a2) t

(* Gets tiles that attack a certain location. *)
let rec attacking_tile acc (attackpos:(int*int)) 
    (enemyattacks: tile list) : tile list =
  match enemyattacks with
  | [] -> acc
  | (s,l)::t -> 
    if possible_attacking_tile attackpos l 
    then attacking_tile ((s,l)::acc) attackpos t 
    else attacking_tile acc attackpos t

let to_int_int d =
  match d with
  |(a,b) ->
    print_string "(";
    print_int(a);
    print_string(",");
    print_int(b);
    print_string ") ";()

let rec copy_check t current (kingpos:int*int) (allymoves:(int*int) list) 
    (prevent:int*int) =
  let copy_t = {board= copy_board t.board; 
                turn= t.turn;
                white_moves= t.white_moves;
                black_moves= t.white_moves;
                black_enpassant = ref !(t.black_enpassant); 
                white_enpassant = ref !(t.white_enpassant) ;
                white_castle=t.white_castle;
                black_castle=t.black_castle} in
  let color = t.turn in
  match allymoves with
  | [] -> false
  | final::tail -> begin
      match (move copy_t current final) with
      | Illegal -> copy_check t current kingpos tail prevent
      | Legal st -> 
        if 
          (if color 
           then white_check st kingpos 
           else black_check st kingpos)
        then copy_check t current kingpos tail prevent
        else true
    end

(* As defined of a tile, takes the first value which is the location of the ally
   for a tile. then checks the list of places to attack. *)
let run_through_ally_list t (kingpos:int*int) (allyattacks:tile) 
    (prevent:int*int)=
  match allyattacks with
  | s,list -> begin match list with 
      | (a,b)::tail -> copy_check t (a,b) kingpos tail prevent
      | _ ->failwith ("Should not happen.")
    end

(* Of the tile list of allyattacks, take the first firs tile to check what it
   needs to prevent. Else returns false. *)
let rec take_ally_tile t (kingpos:int*int) (allyattacks:tile list) 
    (prevent:int*int) =
  match allyattacks with
  | [] -> false
  | hd::tl -> if run_through_ally_list t kingpos hd prevent then true else 
      take_ally_tile t kingpos tl prevent

(* Of the int list of a tile, goes through it. Returns true if the check can be 
   prevented. Else returns false. *)
let rec run_through_tile_list (t:t) (kingpos:int*int) (allyattacks:tile list) 
    (tilesattack:(int*int)list) =
  match tilesattack with
  | [] -> false
  | tile::tail -> if take_ally_tile t kingpos allyattacks tile
    then true else run_through_tile_list t kingpos allyattacks tail

(* Takes the string off the tiles of the enemy. Returns true if the check can be
   prevented. Else returns false. *)
let rec blockable_tile (ty:t) (kingpos:int*int) (allyattacks:tile list) 
    (tilesattack:tile list) =
  match tilesattack with
  | [] -> false
  | (s,l)::t -> if run_through_tile_list ty kingpos allyattacks l then true 
    else blockable_tile ty kingpos allyattacks t

(* Checks if a check can be blocked to prevent checkmate. *)
let blockable_check (t:t) kingpos allyattacks enemyattacks = 
  let tilesattack = attacking_tile [] kingpos enemyattacks in
  blockable_tile t kingpos allyattacks tilesattack

(* Checks if a king can capture the checking piece. *)
let rec king_check_prevent (t:t) (king_move:(int*int)list) 
    (king_pos:(int*int)) =
  let copy_t = {board= copy_board t.board; 
                turn= t.turn;
                white_moves= t.white_moves;
                black_moves= t.white_moves;
                black_enpassant = ref !(t.black_enpassant); 
                white_enpassant = ref !(t.white_enpassant) ;
                white_castle=t.white_castle;
                black_castle=t.black_castle} in
  match king_move with
  | [] -> false
  | h::tail -> 
    begin match (move copy_t king_pos h) with
      |Illegal -> king_check_prevent t tail king_pos
      |Legal st -> 
        if (not(st.turn))
        then (if white_check st h 
              then king_check_prevent t tail king_pos else true)
        else (if black_check st h 
              then king_check_prevent t tail king_pos else true)
    end

let is_checkmate (st:t) = 
  let t = {board= (copy_board st.board); 
           turn= st.turn;
           white_moves= st.white_moves;
           black_moves= st.white_moves;
           black_enpassant = ref !(st.black_enpassant); 
           white_enpassant = ref !(st.white_enpassant);
           white_castle=st.white_castle;
           black_castle=st.black_castle} in
  let white_tile_move = possible_moves_list_white t in 
  let black_tile_move = possible_moves_list_black t in
  let whites_move = to_coord_list white_tile_move false in 
  let blacks_move = to_coord_list black_tile_move false in
  let whites_blocking_move = take_king white_tile_move  in 
  let blacks_blocking_move = take_king black_tile_move  in
  let white_king_move = white_king_moves white_tile_move in 
  let black_king_move = black_king_moves black_tile_move in 
  ignore whites_move;ignore blacks_move;
  ignore white_king_move;ignore black_king_move;
  if t.turn then (
    if white_check t (white_king_pos t) 
    then not (can_king_move white_king_move blacks_move) &&
         not (blockable_check t (white_king_pos t) 
                whites_blocking_move black_tile_move) &&
         not (king_check_prevent t white_king_move (white_king_pos t))
    else false ) 
  else (
    if black_check t (black_king_pos t) 
    then
      (not (can_king_move black_king_move whites_move)) && 
      not (blockable_check t (black_king_pos t)
             blacks_blocking_move white_tile_move) &&
      not (king_check_prevent t black_king_move (black_king_pos t))
    else false ) 

let init_state =
  let board = Array.make_matrix (fst Game.board) (snd Game.board) None in
  let x = place_pieces board in 
  {board = x; turn = true; 
   white_moves = 
     possible_moves_list_white {
       board = x;
       turn = true;
       white_moves = []; 
       black_moves = [];
       black_enpassant = ref false; white_enpassant = ref false;
       white_castle=(true,true);
       black_castle=(true,true)};
   black_moves = 
     possible_moves_list_black {
       board = x; 
       turn = true; 
       white_moves = []; 
       black_moves = [];
       black_enpassant = ref false; white_enpassant = ref false;
       white_castle=(true,true);
       black_castle=(true,true)};
   black_enpassant = ref false; white_enpassant = ref false;
   white_castle=(true,true);
   black_castle=(true,true)}

let to_string (state:t) =
  let matrix = state.board in 
  let string = ref "  " in
  for j = 0 to 7 do 
    string := !string ^ "  " ^ string_of_int j ^ " "
  done;
  string := !string ^ "\n  -";
  for j = 0 to 7 do 
    string := !string ^ "----"
  done;
  string := !string ^ "\n ";
  for i = 0 to 7 do 
    string:= !string ^ (string_of_int(i)) ^ "|";
    for j = 0 to 7 do 
      match matrix.(i).(j) with 
      | None -> string := !string ^ "   |"
      | Some p -> string := !string ^ (Game.piece_name p) ^ "|"
    done;
    string := !string ^ "\n  -";
    for j = 0 to 7 do 
      string := !string ^ "----"
    done;
    string := !string ^ "\n ";
  done;
  !string
