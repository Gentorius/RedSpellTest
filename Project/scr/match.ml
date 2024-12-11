open Lwt.Infix
open Lwt_unix
open Match_end

type game_state = {
  board : char array array;
  mutable current_turn : char;
}

let create_empty_board () =
  Array.make_matrix 3 3 ' '

let handle_turn game_state client_sock opponent_sock =
  Lwt_io.read_line_opt (Lwt_io.of_fd ~mode:Lwt_io.input client_sock) >>= function
  | Some line when String.length line = 3 && String.sub line 0 4 = "turn" ->
      let x = int_of_char line.[5] - int_of_char '0' in
      let y = int_of_char line.[7] - int_of_char '0' in
      if game_state.board.(x).(y) = ' ' then
        game_state.board.(x).(y) <- game_state.current_turn;
        game_state.current_turn <- if game_state.current_turn = 'X' then 'O' else 'X';
        handle_game_end game_state client_sock opponent_sock
      else
        handle_turn game_state client_sock opponent_sock
  | Some _ -> handle_turn game_state client_sock opponent_sock
  | None -> Lwt.return_unit