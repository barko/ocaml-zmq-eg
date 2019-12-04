module ZL = Zmq_lwt
module S = Zmq.Socket

let read_key_in_file path =
  match Bos.OS.File.read (Fpath.v path) with
  | Result.Ok contents -> Zmq.Z85.decode contents
  | Result.Error (`Msg msg) -> print_endline msg; exit 1

let main () =
  let server_private_key_path =
    match Sys.argv with
    | [| _; server_private_key_path |] -> server_private_key_path
    | _ ->
      Printf.printf "usage: %s <server-private-key-path>\n%!" Sys.argv.(0);
      exit 1
  in
  let server_private_key = read_key_in_file server_private_key_path in
  let ctx = Zmq.Context.create () in
  let socket =
    let socket = S.create ctx S.router in
    S.set_curve_server socket true;
    S.set_curve_secretkey socket server_private_key;
    S.bind socket "tcp://*:5555";
    ZL.Socket.of_socket socket
  in

  let rec loop () =
    let%lwt (id, _msgs) = ZL.Socket.Router.recv socket in
    let%lwt () = ZL.Socket.Router.send socket id ["Ok"] in
    loop ()
  in

  let%lwt () = loop () in
  let%lwt () = ZL.Socket.close socket in
  let () = Zmq.Context.terminate ctx in
  Lwt.return ()

let _ =
  Lwt_main.run ( main () )
