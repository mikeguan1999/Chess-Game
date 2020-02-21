open State
open Command
open Graphics
open Png
open Images



let display_text text start_line = 
  Graphics.set_color Graphics.white;
  Graphics.moveto 20 start_line;
  Graphics.draw_string text

let convert_to_rgb i = match i with
  | Rgba32 img -> Rgb24 (Rgb24.of_rgba32 img)
  | _ -> i

(** [apply_transparency img] is an Rgb24 representation of a Png, where the
      transparent pixels are accounted for using the Graphics library unique
      transparent color. *)
let apply_transparency = function
  | Rgba32 bitmap ->
    let w = bitmap.Rgba32.width
    and h = bitmap.Rgba32.height in
    Array.init h (fun i ->
        Array.init w (fun j ->
            let {color = {r = r; g = g; b = b}; 
                 alpha = a} = Rgba32.unsafe_get bitmap j i in
            if a = 0 then Graphics.transp else rgb r g b))
  | _ -> failwith "impossible - always PNG with RGBA"


let rec draw_board cols rows tile_size = 
  if (cols + rows) mod 2 = 0 then Graphics.set_color (Graphics.rgb 105 20 14)
  else Graphics.set_color (Graphics.rgb 213 136 54);
  if rows < 8 then
    if cols < 7 then 
      (Graphics.fill_rect (cols*tile_size) 
         (rows*tile_size) (tile_size) (tile_size); 
       draw_board (cols+1) (rows) tile_size)
    else (Graphics.fill_rect (cols*tile_size) (rows*tile_size) 
            tile_size tile_size; 
          draw_board (0) (rows+1) tile_size)
  else ();;


let draw_piece (piece: Game.piece) col row =
  let name = (Game.piece_name piece) |> String.trim |> String.lowercase_ascii in
  let file = "images/" ^ name ^ ".png" in
  let img = Png.load file [] in
  let g = Graphics.make_image (apply_transparency img) in
  Graphics.draw_image g (row*60) (col*60);;

let draw_pieces (state:State.t) = 
  for r = 0 to fst Game.board-1 do 
    for c = 0 to snd Game.board-1 do
      match State.piece_at state r c with
      | None -> ()
      | Some piece -> draw_piece piece r c
    done;
  done;;


let rec get_command state = 
  let s = Graphics.wait_next_event
      [Graphics.Button_down] in 
  let x1 = (s.Graphics.mouse_x / 60) in
  let y1 = (s.Graphics.mouse_y / 60) in
  if (x1 > 7 || x1 < 0 || y1 > 7 || y1 < 0) then get_command state else
    (if (x1 + y1) mod 2 = 0 then Graphics.set_color (Graphics.rgb 70 0 0)
     else Graphics.set_color (Graphics.rgb 180 90 30);
     if x1 < 8 && y1 < 8 
     then Graphics.fill_rect (x1 * 60) (y1 * 60) 59 59
     else ();
     (match State.piece_at state y1 x1 with
      | None -> ()
      | Some piece -> draw_piece piece y1 x1);
     let s1 = Graphics.wait_next_event
         [Graphics.Button_down] in
     let x2 = (s1.Graphics.mouse_x / 60) in
     let y2 = (s1.Graphics.mouse_y / 60) in 

     ("move " ^ string_of_int y1 ^ " " ^ string_of_int x1 ^ " " ^ 
      string_of_int y2 ^ " " ^ string_of_int x2)
    )
let rec run command = exit 0


let display_winner winner = 
  Graphics.set_color (Graphics.rgb 60 21 24);
  Graphics.fill_rect 0 480 480 100;
  Graphics.set_color Graphics.white;
  Graphics.moveto 20 500;
  Graphics.draw_string (winner ^ " wins!"); 
  ignore (wait_next_event []);
  ()


let display_checkmate winner = 
  Graphics.set_color (Graphics.rgb 60 21 24);
  Graphics.fill_rect 0 480 480 100;
  Graphics.set_color Graphics.white;
  Graphics.moveto 20 500;
  Graphics.draw_string ("Checkmate! " ^ winner ^ " wins!"); 
  ignore (wait_next_event []);
  ()


let end_game (state:State.t) (reason:string) =
  print_endline reason;
  print_endline "\nGame has ended. Final board looks like...\n";
  print_endline (state|>State.to_string);
  ()

let rec run_game (state:State.t)=
  print_string("\n");
  print_endline (state|>State.to_string); 
  draw_board 0 0 60;
  draw_pieces state;
  if not (State.black_king_alive state) then display_winner "White" else ();
  if not (State.white_king_alive state) then display_winner "Black" else ();
  (* display_text "Hello"; *)
  (* if is_checkmate state then print_endline "Checkmate!" else *)
  if white_check state (white_king_pos state) 
  then (display_text "White king is in check" 540; 
        print_endline "White king is in check!\n") 



  else if black_check state (black_king_pos state) 
  then (display_text "Black king is in check" 540; 
        print_endline "Black king is in check!\n") else ();
  print_endline "\nPlease enter your next command. A move command should 
  be in the form of 
  move [starting row] [starting column] [ending row] [ending column].\n";
  print_string  "> ";
  let user_input=get_command state in
  try
    let parsed_input=Command.parse user_input in
    match parsed_input with
    | Move (a,b,c,d) -> begin
        match State.move state (a,b) (c,d) with 
        | Legal m ->  
          if is_checkmate m 
          then (draw_board 0 0 60;
                draw_pieces m;
                print_endline "Checkmate!";
                print_endline (state|>State.to_string); 
                display_checkmate (State.get_str_turn state); 
                end_game m "checkmate")
          else
            Graphics.set_color (Graphics.rgb 60 21 24);
          Graphics.fill_rect 0 480 480 100;
          display_text ("It is now " ^ State.get_str_turn m ^ "'s turn.") 520;
          run_game m
        | Illegal -> print_endline ("Invalid movement. Please try again. 
        It is now " ^ State.get_str_turn state ^ "'s turn."); 
          display_text ("Invalid movement. Please try again.") 500;
          run_game state
      end
    | Resign -> end_game state "Resignation"
    | Quit -> end_game state "Game Quit"
    | Draw -> end_game state "Game Draw"
    | Reset -> run_game State.init_state
  with
  |Empty -> print_endline "Empty input. 
  Please enter something into the command.";
    run_game state
  |Malformed -> print_endline "Invalid input. Please enter a valid command.";
    run_game state
  |Invalid_Move -> print_endline "Invalid Move input. Please put a move command
    in the format of [move row_start column_start row_end column_end].";
    run_game state


let play_game (file: string)=
  (*let json=Yojson.Basic.from_file file in*)
  Graphics.open_graph " 480x580";
  Graphics.set_color (Graphics.rgb 60 21 24);
  Graphics.fill_rect 0 480 480 100;
  display_text ("Welcome to chess! White starts the game.") 520;
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
