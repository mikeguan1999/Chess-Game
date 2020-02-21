type command=
  |Move of int*int*int*int
  |Resign
  |Quit
  |Draw
  |Reset

exception Empty
exception Malformed
exception Invalid_Move

let rec parse_command = function
  |[]->raise Empty
  |""::t->parse_command t
  |"move"::t when List.length t = 4 -> let lst = List.map int_of_string t in 
    Move (List.nth lst 0, List.nth lst 1, List.nth lst 2, 
          List.nth lst 3)
  |"move"::t -> raise Invalid_Move
  |"resign"::_-> Resign
  |"quit"::_->Quit
  |"draw"::_->Draw
  |"reset"::_->Reset
  |_->raise Malformed

let parse (str:string):command=
  let command_list = (String.split_on_char ' ' str) in
  parse_command command_list
