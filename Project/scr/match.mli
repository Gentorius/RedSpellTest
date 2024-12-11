type game_state = {
  board : char array array;
  mutable current_turn : char;
}

val create_empty_board : unit -> char array array
val handle_turn : game_state -> file_descr -> file_descr -> unit Lwt.t