open Lwt.Infix
open Lwt_unix
open Match

let waiting_clients = Lwt_mvar.create_empty ()

let handle_client client_sock =
  let rec game_loop game_state opponent_sock =
    handle_turn game_state client_sock opponent_sock >>= fun () ->
    handle_turn game_state opponent_sock client_sock
  in
  Lwt_io.read_line_opt (Lwt_io.of_fd ~mode:Lwt_io.input client_sock) >>= function
  | Some "start" ->
      Lwt_mvar.put waiting_clients client_sock >>= fun () ->
      Lwt_mvar.take waiting_clients >>= fun opponent_sock ->
      if client_sock = opponent_sock then
        Lwt_mvar.put waiting_clients client_sock >>= fun () ->
        Lwt_io.write_line (Lwt_io.of_fd ~mode:Lwt_io.output client_sock) "Waiting for opponent..."
      else
        let game_state = { board = create_empty_board (); current_turn = 'X' } in
        Lwt_io.write_line (Lwt_io.of_fd ~mode:Lwt_io.output client_sock) "Game started. Your turn." >>= fun () ->
        Lwt_io.write_line (Lwt_io.of_fd ~mode:Lwt_io.output opponent_sock) "Game started. Wait for opponent's turn." >>= fun () ->
        game_loop game_state opponent_sock
  | Some _ -> Lwt.return_unit
  | None -> Lwt.return_unit