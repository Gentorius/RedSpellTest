val send_message : file_descr -> string -> unit Lwt.t
val handle_win : file_descr -> file_descr -> unit Lwt.t
val handle_tie : file_descr -> file_descr -> unit Lwt.t
val handle_ongoing_turn : file_descr -> file_descr -> char -> unit Lwt.t
val check_winner : char array array -> bool
val check_tie : char array array -> bool
val handle_game_end : game_state -> file_descr -> file_descr -> unit Lwt.t