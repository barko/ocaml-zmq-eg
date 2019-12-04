(* subscribe to two topics "A" and "B" *)

module ZL = Zmq_lwt
module S = Zmq.Socket

let main () =
  let ctx = Zmq.Context.create () in
  let socket =
    let socket = S.create ctx Zmq.Socket.sub in
    let () = S.subscribe socket "A" in
    let () = S.subscribe socket "B" in
    let () = S.connect socket "tcp://127.0.0.1:5555" in
    ZL.Socket.of_socket socket
  in

  let rec loop () =
    let%lwt _msgs = ZL.Socket.recv_all socket in
    loop ()
  in

  let%lwt () = loop () in

  let%lwt () = ZL.Socket.close socket in
  let () = Zmq.Context.terminate ctx in
  Lwt.return ()

let _ =
  Lwt_main.run ( main () )
