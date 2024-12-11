open OUnit2
open Match

let test_create_empty_board _ =
  let board = create_empty_board () in
  let expected_board = [|
    [| ' '; ' '; ' ' |];
    [| ' '; ' '; ' ' |];
    [| ' '; ' '; ' ' |]
  |] in
  assert_equal expected_board board

let test_handle_turn _ =
  let game_state = { board = create_empty_board (); current_turn = 'X' } in
  let client_sock, opponent_sock = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let _ = Lwt_main.run (
    Lwt_io.write_line (Lwt_io.of_fd ~mode:Lwt_io.output client_sock) "turn 0 0" >>= fun () ->
    handle_turn game_state client_sock opponent_sock
  ) in
  assert_equal 'X' game_state.board.(0).(0);
  assert_equal 'O' game_state.current_turn

let suite =
  "Match Tests" >::: [
    "test_create_empty_board" >:: test_create_empty_board;
    "test_handle_turn" >:: test_handle_turn;
  ]

let () =
  run_test_tt_main suite