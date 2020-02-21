type move = |Forward of int * int 
            |Sideways of int*int
            |Diagonal of int*int
            |Custom of int*int

type piece = string * bool * (move list)

type pieces = piece list

type board = int * int

type t = board * pieces

let board = (8,8)

let white_rook = 
  ("WR ", true, [Forward (-8,8); Sideways (-8,8)])
let white_bishop = 
  ("WB ", true, [Diagonal (-8,8)])
let white_knight = 
  ("WKn", true, [Custom (2,1); Custom (-2,1); Custom (2,-1); Custom (-2,-1); 
                 Custom (1,2); Custom (-1,2); Custom (1,-2); Custom (-1,-2)])
let white_pawn = 
  ("WP ", true, [Forward (0,2); Diagonal (-1,1)])
let white_queen = 
  ("WQ ", true, [Forward (-8,8); Sideways (-8,8);Diagonal (-8,8)])
let white_king = 
  ("WK ", true, [Forward (-1,1); Sideways (-1,1);Diagonal (-1,1)])
let black_rook = 
  ("BR ", false, [Forward (-8,8); Sideways (-8,8)])
let black_bishop =
  ("BB ", false, [Diagonal (-8,8)])
let black_knight = 
  ("BKn", false, 
   [Custom (2,1); Custom (-2,1); Custom (2,-1); Custom (-2,-1); 
    Custom (1,2); Custom (-1,2); Custom (1,-2); Custom (-1,-2) ])
let black_pawn = 
  ("BP ", false, [Forward (0,2); Diagonal (-1,1)])
let black_queen = 
  ("BQ ", false, [Forward (-8,8); Sideways (-8,8);Diagonal (-8,8)])
let black_king = 
  ("BK ", false, [Forward (-1,1); Sideways (-1,1);Diagonal (-1,1)])

let chess_pieces = [white_rook; white_bishop; white_knight; white_pawn; 
                    white_queen; white_king; black_rook; black_bishop; 
                    black_knight; black_pawn; black_queen; black_king]

let game = (board, chess_pieces)

let piece_name ((name, _, _):piece) = name

let piece_color ((_, color, _):piece) = color

let piece_moves ((_, _ , moves):piece) = moves

let good_move move (x,y) = match move with
  |Forward (a, b) -> x = 0 && y != 0 && y >= a && y <= b
  |Sideways (a,b) -> y = 0 && x != 0 && x >= a && x <= b
  |Diagonal (a,b) -> abs x = abs y && x != 0 && x >= a && x <= b
  |Custom (a,b) -> x = a && y = b


let rec check_move moves (x,y) = match moves with
  |[] -> false
  |move::t -> if good_move move (x, y) then true else check_move t (x, y)

let valid_move p (row,col) =
  if piece_color p = false then check_move (piece_moves p) (-col, -row) else
    check_move (piece_moves p) (col, row)

