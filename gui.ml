open Images
open Graphics
open Png


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
  if (cols + rows) mod 2 = 0 then Graphics.set_color (Graphics.rgb 255 178 102)
  else Graphics.set_color (Graphics.rgb 153 0 0);
  if rows < 8 then
    if cols < 7 then 
      (Graphics.fill_rect (cols*tile_size) (rows*tile_size) (tile_size) (tile_size); 
       draw_board (cols+1) (rows) tile_size)
    else (Graphics.fill_rect (cols*tile_size) (rows*tile_size) tile_size tile_size; 
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

let print_coords x y =
  print_int x;
  print_string ", ";
  print_int y;
  print_endline "\n"


let get_command = 
  let s = Graphics.wait_next_event
      [Graphics.Button_down; Graphics.Key_pressed] in 
  let s1 = Graphics.wait_next_event
      [Graphics.Button_down; Graphics.Key_pressed] in
  let x1 = (s.Graphics.mouse_x / 60) in
  let y1 = (s.Graphics.mouse_y / 60) in
  let x2 = (s1.Graphics.mouse_x / 60) in
  let y2 = (s1.Graphics.mouse_y / 60) in 

  "move " ^ string_of_int x1 ^ " " ^ string_of_int y1 ^ " " ^ string_of_int x2 
  ^ " " ^ string_of_int y2

let rec run state = 
  (* while true do *)
  draw_board 0 0 60;

  draw_pieces state;
  let s = Graphics.wait_next_event
      [Graphics.Button_down; Graphics.Key_pressed]


  in 
  (* if Graphics.button_down () then  *)

  let s1 = Graphics.wait_next_event
      [Graphics.Button_down; Graphics.Key_pressed] in
  let x1 = (s.Graphics.mouse_x / 60) in
  let y1 = (s.Graphics.mouse_y / 60) in
  let x2 = (s1.Graphics.mouse_x / 60) in
  let y2 = (s1.Graphics.mouse_y / 60) in
  print_endline "1: ";
  print_coords x1 y1;
  print_endline "2: ";
  print_coords x2 y2;

  print_endline (string_of_bool (State.get_turn state));

  match State.move state (y1,x1) (y2,x2) with 
  | Legal m -> run m
  | Illegal -> run state

let start =

  Graphics.open_graph " 500x500";;


let main () = 
  start;
  run State.init_state;;

main ()

