(* curve-encrypted server, whose clients' public keys must be known to
   the server in advance. The paths of these keys are provided on the
   command line, along with the server's private key. *)

module ZL = Zmq_lwt
module S = Zmq.Socket

let read_key_in_file path =
  match Bos.OS.File.read (Fpath.v path) with
  | Result.Ok contents -> Zmq.Z85.decode contents
  | Result.Error (`Msg msg) -> print_endline msg; exit 1

type mechanism = [
  | `Null
  | `Plain of string * string
  | `Curve of string
]

type auth_request = {
  version : string;
  request_id : string;
  domain : string;
  address : string;
  identity_property : string;
  mechanism : mechanism option
}

let pr = Printf.printf

let pr_auth_request r =
  pr "version=%s\n" r.version;
  pr "request_id=%s\n" r.request_id;
  pr "domain=%s\n" r.domain;
  pr "address=%s\n" r.address;
  pr "identity_property=%s\n" r.identity_property;
  pr "mechanism=";
  (match r.mechanism with
   | Some `Null -> pr "NULL";
   | Some `Plain (username,password) -> pr "(PLAIN (%s,%s))" username password
   | Some `Curve key -> pr "(CURVE %s)" (Zmq.Z85.encode key)
   | None -> pr "None"
  );
  pr "\n%!"

let main () =
  let server_private_key_path, authorized_clients_public_key_paths =
    match Array.to_list Sys.argv with
    | _ :: server_private_key_path :: authorized_clients_public_key_paths ->
      server_private_key_path, authorized_clients_public_key_paths
    | _ ->
      pr "usage: %s <server-private-key-path> \
          <client-public-key-path-1> ... <client-public-key-path-n>\n%!" Sys.argv.(0);
      exit 1
  in
  let server_private_key = read_key_in_file server_private_key_path in
  let authorized_clients_public_keys = List.map read_key_in_file
      authorized_clients_public_key_paths in

  let ctx = Zmq.Context.create () in

  let auth_socket =
    let socket = S.create ctx S.router in
    S.bind socket "inproc://zeromq.zap.01";
    ZL.Socket.of_socket socket
  in

  let socket =
    let socket = S.create ctx S.router in
    S.set_curve_server socket true;
    S.set_curve_secretkey socket server_private_key;
    S.bind socket "tcp://*:5555";
    ZL.Socket.of_socket socket
  in

  let auth_request () =
    let%lwt (id, msgs) = ZL.Socket.Router.recv auth_socket in
    let msgs = Array.of_list msgs in

    let len = Array.length msgs in
    assert ( len >= 6 && len <= 9 );
    let r = {
      version = msgs.(1);
      request_id = msgs.(2);
      domain = msgs.(3);
      address = msgs.(4);
      identity_property = msgs.(5);
      mechanism = None
    } in
    let r =
      match msgs.(6) with
      | "NULL" -> { r with mechanism = Some `Null }
      | "PLAIN" ->
        let mechanism = Some (`Plain (msgs.(7), msgs.(8))) in
        { r with mechanism }
      | "CURVE" ->
        let mechanism = Some (`Curve msgs.(7)) in
        { r with mechanism }
      | _ -> r
    in
    Lwt.return (id, r)
  in

  (* check that the client key matches one of those provided on the
     command line. *)
  let rec auth_loop () =
    let%lwt (id, r) = auth_request () in
    let is_authorized =
      match r.mechanism with
      | Some (`Curve key) -> List.mem key authorized_clients_public_keys
      | _ -> false
    in
    let response_code =
      match is_authorized with
      | true -> "200"
      | false -> "400"
    in
    let%lwt () = ZL.Socket.Router.send auth_socket id
        [""; "1.0"; r.request_id; response_code; ""; ""; ""] in
    auth_loop ()
  in

  let rec loop () =
    let%lwt (id, _msgs) = ZL.Socket.Router.recv socket in
    let%lwt () = ZL.Socket.Router.send socket id ["Ok"] in
    loop ()
  in

  let%lwt ((), ()) = Lwt.both (auth_loop ()) (loop ()) in

  let%lwt () = ZL.Socket.close socket in
  let () = Zmq.Context.terminate ctx in
  Lwt.return ()

let _ =
  Lwt_main.run ( main () )
