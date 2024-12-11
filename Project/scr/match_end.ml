open Lwt.Infix
open Lwt_unix

let send_message sock message =
  Lwt_io.write_line (Lwt_io.of_fd ~mode:Lwt_io.output sock) message

let handle_win client_sock opponent_sock =
  send_message client_sock "You win!" >>= fun () ->
  send_message opponent_sock "You lose!"

let handle_tie client_sock opponent_sock =
  send_message client_sock "It's a tie!" >>= fun () ->
  send_message opponent_sock "It's a tie!"

let handle_ongoing_turn client_sock opponent_sock current_turn =
  if current_turn = 'X' then
    send_message client_sock "Wait for opponent's turn." >>= fun () ->
    send_message opponent_sock "Your turn."
  else
    send_message client_sock "Your turn." >>= fun () ->
    send_message opponent_sock "Wait for opponent's turn."

let check_winner board =
  let lines = [
    [board.(0).(0); board.(0).(1); board.(0).(2)];
    [board.(1).(0); board.(1).(1); board.(1).(2)];
    [board.(2).(0); board.(2).(1); board.(2).(2)];
    [board.(0).(0); board.(1).(0); board.(2).(0)];
    [board.(0).(1); board.(1).(1); board.(2).(1)];
    [board.(0).(2); board.(1).(2); board.(2).(2)];
    [board.(0).(0); board.(1).(1); board.(2).(2)];
    [board.(0).(2); board.(1).(1); board.(2).(0)];
  ] in
  List.exists (fun line -> List.for_all ((=) 'X') line || List.for_all ((=) 'O') line) lines

let check_tie board =
  Array.for_all (Array.for_all ((<>) ' ')) board

let handle_game_end game_state client_sock opponent_sock =
  if check_winner game_state.board then
    handle_win client_sock opponent_sock
  else if check_tie game_state.board then
    handle_tie client_sock opponent_sock
  else
    handle_ongoing_turn client_sock opponent_sock game_state.current_turn