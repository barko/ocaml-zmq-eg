(* server that just responds "Ok" to every request *)

module ZL = Zmq_lwt
module S = Zmq.Socket

let main () =
  let ctx = Zmq.Context.create () in
  let socket =
    let socket = S.create ctx S.router in
    let () = S.bind socket "tcp://*:5555" in
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
