(* publish an odd or even integer to topics "A" and "B", respectively *)

open Zmq_lwt

let main () =
  let ctx = Zmq.Context.create () in
  let socket =
    let socket = Zmq.Socket.create ctx Zmq.Socket.pub in
    let () = Zmq.Socket.bind socket "tcp://*:5555" in
    Socket.of_socket socket
  in

  let topics = [| "A"; "B" |] in
  let rec loop count =
    let topic = topics.( count mod 2 ) in
    let%lwt () = Socket.send_all socket [topic; string_of_int count ] in
    let%lwt () = Lwt_unix.sleep 1.0 in
    loop (count + 1)
  in

  let%lwt () = loop 0 in

  let%lwt () = Socket.close socket in
  let () = Zmq.Context.terminate ctx in
  Lwt.return ()

let _ =
  Lwt_main.run ( main () )
