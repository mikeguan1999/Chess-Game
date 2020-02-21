(** The type [command] represents a player command that is parsed into a verb
    that represents some action in the game action and some more information
    partaining to what a piece should do. *)
type command =
  |Move of int*int*int*int
  |Resign
  |Quit
  |Draw
  |Reset

(** Raises when an empty command is parsed. *)
exception Empty

(** Raises when a malformed command is encountered. *)
exception Malformed

(** Raises when a malformed move command is encountered. *)
exception Invalid_Move

(** [parse str] breaks down a player's input into a [command], assuming that all
    whitespace does not exist to the machine. The first word becomes the verb
    that descrbies the type of command while and proceed words with whitespace 
    in between will be used as information to describe nature of the command.

    Requires: [str] be alphanumeric and space characters

    Raises: [Empty] if the input [str] is empty or contains only spaces

    Raises: [Malformed] if the input [str] starts with none of the lowercase 
    word formsnof the [command] verbs or if the [command] verbs do not feature
    any whitespace with any following input
*)
val parse : string -> command