open Lwt.Infix
open Lwt_unix

let port = try int_of_string (Sys.getenv "PORT") with _ -> 12345
let client_timeout = 300.0

let create_server_socket () =
  let sock = socket PF_INET SOCK_STREAM 0 in
  let addr = ADDR_INET (Unix.inet_addr_any, port) in
  bind sock addr;
  listen sock 10;
  sock

let rec accept_connections server_sock handle_client =
  accept server_sock >>= fun (client_sock, _) ->
  Lwt_io.printl "Client connected" >>= fun () ->
  Lwt.async (fun () ->
    Lwt.catch
      (fun () ->
        Lwt_unix.with_timeout client_timeout (fun () -> handle_client client_sock)
      )
      (fun exn ->
        Lwt_io.printl ("Error handling client: " ^ Printexc.to_string exn)
      )
  );
  accept_connections server_sock handle_client

let start_server handle_client =
  let server_sock = create_server_socket () in
  Lwt_io.printl ("Server started on port " ^ string_of_int port) >>= fun () ->
  let shutdown_signal = Lwt_unix.on_signal Sys.sigint (fun _ ->
    Lwt_io.printl "Shutting down server..." >>= fun () ->
    Lwt_unix.close server_sock
  ) in
  Lwt_main.run (accept_connections server_sock handle_client);
  Lwt_unix.disable_signal_handler shutdown_signal