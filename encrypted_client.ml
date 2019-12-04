(* a client whose communication with the server are encrypted. The
   client's public and private keypair must be provided, alongside the
   server's public key. *)

module ZL = Zmq_lwt
module S = Zmq.Socket

let read_key_in_file path =
  match Bos.OS.File.read (Fpath.v path) with
  | Result.Ok contents -> Zmq.Z85.decode contents
  | Result.Error (`Msg msg) -> print_endline msg; exit 1

let main () =
  let client_public_key, client_private_key, server_public_key =
    match Sys.argv with
    | [| _; client_public_key_path; client_private_key_path; server_public_key_path |] ->
      read_key_in_file client_public_key_path,
      read_key_in_file client_private_key_path,
      read_key_in_file server_public_key_path
    | _ ->
      Printf.printf "usage: %s <client-public-key> <client-private-key> <server-public-key>\n%!"
        Sys.argv.(1);
      exit 1
  in

  let ctx = Zmq.Context.create () in
  let socket =
    let socket = S.create ctx S.dealer in
    S.set_curve_publickey socket client_public_key;
    S.set_curve_secretkey socket client_private_key;
    S.set_curve_serverkey socket server_public_key;
    let () = S.connect socket "tcp://127.0.0.1:5555" in
    ZL.Socket.of_socket socket
  in
  let rec loop () =
    let%lwt () = ZL.Socket.send socket "Hi" in
    let%lwt _reply = ZL.Socket.recv socket in
    loop ()
  in
  let%lwt () = loop () in
  let%lwt () = ZL.Socket.close socket in
  let () = Zmq.Context.terminate ctx in
  Lwt.return ()

let _ =
  Lwt_main.run ( main () )
