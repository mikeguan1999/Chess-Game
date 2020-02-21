open OUnit2
open Game
open State
open Command

(* Our testing plan was the check the various movements of the chess pieces on
   the board and seeing if they met our expectations. The OUnit was the 
   prefered way of initially testing the movements of the pieces. We switched to
   playing the game as the code became more complicated. The test cases was
   developed using glass box concept as we already know how the code was 
   written. Then we gradually went and tested how each piece moved. This
   demonstrated the correctness of our movements in the system. We then had to
   move towards testing in the higher level as the board became hard to create
   with different situations.
*)

let make_move_test 
    (name: string)
    (piece: Game.piece)
    (move: int * int)
    (valid: bool): test =
  name >:: (fun _ -> assert_equal (valid) (Game.valid_move piece move) 
               ~printer: string_of_bool)


let game_tests = [
  make_move_test "pawn forward" Game.white_pawn (1,0) true;
  make_move_test "pawn forward" Game.black_pawn (-1,0) true;
  make_move_test "pawn invalid" Game.white_pawn (2,1) false;
  make_move_test "pawn invalid2" Game.white_pawn (1,2) false;

  make_move_test "bishop one space" Game.white_bishop (1,1) true;
  make_move_test "bishop multiple spaces" Game.white_bishop (3,3) true;
  make_move_test "bishop multiple spaces" Game.white_bishop (3,-3) true;
  make_move_test "bishop multiple spaces" Game.black_bishop (-5,5) true;
  make_move_test "bishop invalid" Game.white_bishop (0,0) false;


]

let t = State.copy_t (State.init_state)

let result_t t = match t with 
  |State.Illegal -> failwith "impossible"
  |State.Legal s -> s

let white_state = 
  let step = move (State.copy_t t) (1,0) (3,0) in 
  let step2 = move (result_t step) (6,1) (4,1) in result_t step2

let black_state = result_t (move (State.copy_t white_state) (1,6) (2,6))

let black_enpassant = 
  let step = move (State.copy_t t) (1,0) (3,0) in 
  let step2 = move (result_t step) (6,1) (4,1) in
  let step3 = (move (result_t step2) (1,6) (2,6)) in
  let step4 = move (result_t step3) (4,1) (3,1) in 
  let step5 = move (result_t step4) (1,2) (3,2) in result_t step5

let white_enpassant = 
  let step = move (State.copy_t t) (1,0) (3,0) in 
  let step2 = move (result_t step) (6,1) (4,1) in
  let step3 = (move (result_t step2) (1,6) (3,6)) in
  let step4 = move (result_t step3) (4,1) (3,1) in 
  let step5 = move (result_t step4) (1,2) (3,2) in
  let step6 = (move (result_t step5) (6,5) (5,5)) in 
  let step7 = move (result_t step6) (3,2) (4,2) in 
  let step8 = move (result_t step7) (6,3) (4,3) in result_t step8

let white_move_t = 
  let step = move (State.copy_t t) (1,0) (3,0) in 
  let step2 = move (result_t step) (6,0) (4,0) in 
  let step3 = move (result_t step2) (1,1) (3,1) in 
  let step4 = move (result_t step3) (6,1) (4,1) in 
  let step5 = move (result_t step4) (1,2) (3,2) in 
  let step6 = move (result_t step5) (6,2) (4,2) in 
  result_t step6

let white_move_t2 = 
  result_t (move (State.copy_t white_move_t) (1,3) (3,3))

let black_move_t = 
  let step = move (State.copy_t t) (1,0) (3,0) in 
  let step2 = move (result_t step) (6,0) (4,0) in 
  let step3 = move (result_t step2) (1,1) (3,1) in 
  let step4 = move (result_t step3) (6,1) (4,1) in 
  let step5 = move (result_t step4) (1,2) (3,2) in 
  let step6 = move (result_t step5) (6,2) (4,2) in 
  let step7 = move (result_t step6) (1,5) (3,5) in 
  result_t step7

let black_move_t2 =
  result_t (move (State.copy_t black_move_t) (6,3) (4,3))

let pawn_move_test 
    (name:string)
    (state:State.t)
    (piece: Game.piece)
    (current: (int*int))
    (final: (int*int))
    (expect: bool):test =
  name >:: (fun _ -> assert_equal (State.check_pawn_move state current final piece) expect)

let move_test 
    (name:string)
    (state:State.t)
    (current: (int*int))
    (final: (int*int))
    (expect: bool):test =
  name >:: (fun _ -> assert_equal (State.check_possible_move state current final) expect)

let state_tests = [
  (*move_test "black takes white pawn enpassant" black_enpassant (3,1) (2,2) true;*)
  move_test "white takes black pawn enpassant" white_enpassant (4,2) (5,3) true;
  pawn_move_test "White pawn basic foward movement" t Game.white_pawn (1,0) (2,0) true;
  pawn_move_test "White pawn foward movement 2" t Game.white_pawn (1,0) (3,0) true;
  pawn_move_test "black pawn basic foward movement" t Game.black_pawn (6,0) (5,0) true;
  pawn_move_test "black pawn foward movement 2" t Game.black_pawn (6,0) (4,0) true;
  pawn_move_test "Invalid White pawn foward movement" t Game.white_pawn (1,0) (4,0) false;
  pawn_move_test "Invalid black pawn foward movement" t Game.black_pawn (6,0) (3,0) false;
  pawn_move_test "invalid White pawn diagonal" white_state Game.white_pawn (1,1) (2,2) false;
  pawn_move_test "White pawn diagonal" white_state Game.white_pawn (3,0) (4,1) true;
  pawn_move_test "invalid black pawn diagonal" black_state Game.black_pawn (6,5) (5,4) false;
  pawn_move_test "black pawn diagonal" black_state Game.black_pawn (4,1) (3,0) true;
  move_test "white knight movement" t (0,1) (2,2) true;
  move_test "invalid white knight movement" t (0,1) (2,1) false;
  move_test "invalid white knight movement own piece" t (0,1) (1,3) false;
  move_test "black knight movement" t (7,1) (5,2) true;
  move_test "invalid black knight movement" t (7,1) (2,1) false;
  move_test "invalid black knight movement own piece" t (7,1) (6,3) false;
  move_test "white bishop movement" white_move_t (0,2) (1,1) true;
  move_test "invalid white bishop movement" white_move_t (0,2) (1,2) false;
  move_test "invalid white bishop movement own piece" white_move_t (0,2) (1,3) false;
  move_test "black bishop movement" black_move_t (7,2) (6,1) true;
  move_test "invalid black bishop movement" black_move_t (7,2) (6,2) false;
  move_test "invalid black bishop movement own piece" black_move_t (7,2) (6,3) false;
  move_test "white queen Orthogonal movement" white_move_t2 (0,3) (1,3) true;
  move_test "white queen Diagonal movement" white_move_t (0,3) (1,2) true;
  move_test "invalid white queen movement" white_move_t (0,3) (1,1) false;
  move_test "invalid white queen movement own piece" white_move_t (0,3) (1,3) false;
  move_test "black queen Orthogonal movement" black_move_t2 (7,3) (6,3) true;
  move_test "black queen Diagonal movement" white_move_t (7,3) (6,2) true;
  move_test "invalid black queen movement" black_move_t (7,3) (6,1) false;
  move_test "invalid black queen movement own piece" black_move_t (7,3) (6,3) false;
  move_test "white rook movement" white_move_t (0,0) (1,0) true;
  move_test "invalid white rook movement" white_move_t (0,0) (1,1) false;
  move_test "invalid white rook movement own piece" white_move_t (0,0) (0,1) false;
  move_test "black rook movement" black_move_t (7,0) (6,0) true;
  move_test "invalid black rook movement" black_move_t (7,0) (6,1) false;
  move_test "invalid black rook movement own piece" black_move_t (7,0) (7,1) false;
  move_test "invalid white king movement own piece" white_move_t (0,4) (1,0) false;
  move_test "invalid black king movement" black_move_t (7,4) (7,3) false;
  move_test "invalid black king movement" black_move_t (7,4) (6,2) false;
  move_test "invalid black king movement own piece" black_move_t (7,4) (6,3) false;
]

let command_tests = []



let tests = List.flatten [game_tests; state_tests; command_tests]

let suite = "search test suite" >::: tests

let _ = run_test_tt_main suite