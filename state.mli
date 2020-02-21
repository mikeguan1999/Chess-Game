(** 
   Representation of dynamic board state.

   This module represents the state of the board as it is being played. This
   includes the current position of the pieces and how those pieces behave 
   relative to the current board.
*)

(** The abstract type of values representing the game state. *)
type t 

(** The type that represents the possible places a piece can go attached to its
    name and the current tile it is on which will be the first value in the 
    list. *)
type tile = string * (( int * int ) list)

(** [init_state] is the initial state of the board where the pieces are 
    places as a standard chess board would.*)
val init_state : t   

(** [current_board t] takes in a state and returns the board of that state*)
val current_board: t -> Game.piece option array array

(** [piece_at t row col] is the piece at the [row] and [col] on the board *)
val piece_at: t -> int -> int -> Game.piece option

(** Result is the type which represents the result of an attempted movement. *)
type result = Legal of t | Illegal

(** [move t current final] is the result of inputed move where it takes the
    piece at [current] and attempts to place it on [final] if the piece 
    obeys its moveset, not out of the board, and if the move is not blocked.*)
val move : t -> (int*int) -> (int*int) -> result

(** [possible_moves_list_white t] takes in a state t [t] which a list of type  
    tiles each with the name of all the white pieces and the possible places 
    on the board it can move..*)
val possible_moves_list_white : t -> tile list

(** [possible_moves_list_white t] takes in a state t [t] which a list of type  
    tiles each with the name of all the black pieces and the possible places 
    on the board it can move..*)
val possible_moves_list_black : t -> tile list

(** [to_string state] is the string representation of [state]*)
val to_string : t -> string

(** [get_turn t] returns a bool, [t]'s turn element, where true represents white's 
    turn and false represents black's turn*)
val get_turn: t -> bool

(** [get_str_turn t] returns "white" if [t]'s turn element is true and "black"
    when it's false*)
val get_str_turn: t -> string

(** [white_check t coord] checks to see if any of the black pieces can move to
    or attack the white king's current location [coord] in [t]*)
val white_check : t -> int*int -> bool

(** [black_check t coord] checks to see if any of the white pieces can move to
    or attack the black king's current location [coord] in [t]*)
val black_check : t -> int*int -> bool

(** [white_king_pos t] returns the coordinate of the white king in [t]'s board*)
val white_king_pos : t -> int * int

(** [black_king_pos t] returns the coordinate of the black king in [t]'s board*)
val black_king_pos : t -> int * int

(** [black_king_alive t] is true if the black king is alive*)
val black_king_alive : t -> bool

(** [white_king_alive t] is true if the white king is alive*)
val white_king_alive : t -> bool

(** [is_checkmate t] runs calculations to see if the king is in check and the 
    player has no moves to prevent it.*)
val is_checkmate : t -> bool


(** [castle t side] takes in a state t [t] and a string [side] that should be
    either [right] or [left] and does the castling machanic if it passes the
    rules*)
val castle : t -> string -> result

(** [check_pawn_move t current final p] returns true if the pawn movement was 
    valid according to chess rules*)
val check_pawn_move: t-> (int*int)-> (int*int)-> Game.piece->bool

(** [copt_t t] returns [t] with a copy of it's board*)
val copy_t: t -> t

(** [check_possible_move curr_b current final] checks to see if the movement is
    possible in [curr_b]'s board*)
val check_possible_move: t -> (int*int)-> (int*int)-> bool