module ZL = Zmq_lwt
module S = Zmq.Socket

let main () =
  let ctx = Zmq.Context.create () in
  let socket =
    let socket = S.create ctx S.dealer in
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
