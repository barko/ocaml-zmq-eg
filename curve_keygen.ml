(* create a public and private keypair, whose file prefix is provided
   as the sole command line argument, and whose suffixes are ".pub"
   and ".key", respectively. *)

let open_write ~path ~contents =
  let ch = open_out_bin path in
  output_string ch contents;
  close_out ch

let main () =
  let output_prefix =
    match Sys.argv with
    | [| _ ; output_prefix |] -> output_prefix
    | _ ->
      Printf.printf "usage: %s <output-prefix>\n%!" Sys.argv.(0);
      exit 1
  in
  let public_key, private_key = Zmq.Curve.keypair () in
  let public_key_path = output_prefix ^ ".pub" in
  let private_key_path = output_prefix ^ ".key" in
  open_write ~path:public_key_path ~contents:public_key;
  open_write ~path:private_key_path ~contents:private_key

let _ =
  main ()
