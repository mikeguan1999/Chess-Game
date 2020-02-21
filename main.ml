open State
open Command


let rec run command = exit 0

let end_game (state:State.t) (reason:string) =
  print_endline reason;
  print_endline "\nGame has ended. Final board looks like...\n";
  print_endline (state|>State.to_string);
  ()

let rec run_game (state:State.t)=
  print_string("\n");
  print_endline (state|>State.to_string); 
  if white_check state (white_king_pos state) 
  then print_endline "White king is in check!\n" else ();
  if black_check state (black_king_pos state) 
  then print_endline "Black king is in check!\n" else ();
  print_endline "\nPlease enter your next command. ";
  print_string  "> ";
  let user_input=read_line () in
  try
    let parsed_input=Command.parse user_input in
    match parsed_input with
    | Move (a,b,c,d) -> begin
        match State.move state (a,b) (c,d) with 
        | Legal m ->  if is_checkmate state 
          then end_game m "Checkmate"
          else run_game m
        | Illegal -> print_endline "Invalid movement.";
          run_game state
      end
    | Resign -> end_game state "Resignation"
    | Quit -> end_game state "Game Quit"
    | Draw -> end_game state "Game Draw"
    | Reset -> run_game State.init_state
  with 
  |Empty -> print_endline "Empty input.";
    run_game state
  |Malformed -> print_endline "Invalid input.";
    run_game state
  |Invalid_Move -> print_endline "Invalid Move input.";
    run_game state


let play_game (file: string)=
  (*let json=Yojson.Basic.from_file file in*)
  let state=State.init_state in
  run_game state

let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the board game engine.\n");
  print_endline "Please enter the name of the game file you want to load.\n";
  print_string  "> ";
  (*match read_line () with
    | exception End_of_file -> ()
    | file_name -> *)play_game (*file_name*) ""

let _ = main()
