(** 
   Representation of static game data.

   This module represents the data of the pieces as well as the dimesions of 
   the board
*)

(** The abstract type of values representing game. *)
type t

(** The type that represents the move as Foward, Sideways, Diagonal, and 
    Custom where the tuple determines the range of the movement.*)
type move = Forward of int*int| Sideways of int*int| Diagonal of int*int| Custom of int*int

(** The type that represents pieces that has the name, color where true = white 
    and false = black, and move set*)
type piece = (string * bool * move list) 

(** the type that gives the information of the dimentions of the board *)
type board = int* int

val board : board

(** This is the piece that represents the white rook*)
val white_rook : piece

(** This is the piece that represents the white bishop*)
val white_bishop : piece

(** This is the piece that represents the white knight*)
val white_knight : piece

(** This is the piece that represents the white queen*)
val white_queen : piece

(** This is the piece that represents the white pawn*)
val white_pawn : piece

(** This is the piece that represents the white king*)
val white_king : piece

(** This is the piece that represents the black rook*)
val black_rook : piece

(** This is the piece that represents the black bishop*)
val black_bishop : piece

(** This is the piece that represents the black knight*)
val black_knight : piece

(** This is the piece that represents the black queen*)
val black_queen : piece

(** This is the piece that represents the black pawn*)
val black_pawn : piece

(** This is the piece that represents the white king*)
val black_king : piece

(** This is the list of pieces of chess*)
val chess_pieces : piece list

(** This is the game, chess*)
val game : t


val piece_name : piece -> string

val piece_color : piece -> bool

val piece_moves : piece -> move list

(** [valid_move piece x y] is true if x,y is a valid move for [piece] *)
val valid_move : piece -> int*int -> bool