(* TCP Network connections *)
let host, port = Unix.inet_addr_loopback, 9009

let print_sockaddr = function
  | Unix.ADDR_UNIX s -> print_endline ("UNIX:" ^ s)
  | ADDR_INET (host, port) -> print_endline ("INET:" ^ Unix.string_of_inet_addr host ^ ":" ^ string_of_int port)

let listening_sock () =
  let addr = Unix.(ADDR_INET (host, port)) in
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind sock addr;
  Unix.listen sock 0;
  let sock_accept = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt sock SO_REUSEADDR true;
  print_sockaddr addr;
  (* Unix.setsockopt sock SO_REUSEPORT true; *)
  Iocp.Handle.of_unix sock, Iocp.Handle.of_unix sock_accept

type req = [ `A | `R ]

let listen () =
  let iocp = Iocp.create () in
  let sock, sock_accept = listening_sock () in
  let addr_buf = Iocp.Accept_buffer.create () in
  let _job = Iocp.accept iocp sock sock_accept addr_buf `A in
  match Iocp.get_queued_completion_status iocp with
  | None -> assert false
  | Some t ->
    assert (t.data = `A);
    let client_addr = Iocp.Accept_buffer.get_remote addr_buf sock in
    print_sockaddr @@ Iocp.Sockaddr.get client_addr;
    let buf = Cstruct.create 4096 in
    let _job : [> `R ] Iocp.job option = Iocp.recv iocp sock_accept [ buf ] `R in
    match Iocp.get_queued_completion_status iocp with
    | None -> assert false
    | Some t ->
      assert (t.data = `R);
      print_endline (Cstruct.to_string buf)

let connect () =
  let iocp = Iocp.create () in
  let addr = Unix.(ADDR_INET (host, port)) in
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind sock (ADDR_INET (Unix.inet_addr_any, 0)); 
  let sock = Iocp.Handle.of_unix sock in
  let _job = Iocp.connect iocp sock addr `A in
  match Iocp.get_queued_completion_status iocp with
  | None -> assert false
  | Some t ->
    assert (t.data = `A);
    (* Gc.full_major (); *)
    print_endline "Connected! Now sending data...";
    let buf = Cstruct.of_string "Hello socket!" in
    let _job : _ = Iocp.send iocp sock [ buf ] `W in
    (* Gc.full_major (); *)
    print_endline "Data queued up...";
    match Iocp.get_queued_completion_status iocp with
    | None -> assert false
    | Some t ->
      assert (t.data = `W);
      print_endline "All done!"
    

let () =
  try if Sys.argv.(1) = "listen" then listen () else connect () with _ -> connect ()