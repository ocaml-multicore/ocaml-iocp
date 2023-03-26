(* Safe TCP Network connections *)
let host, port = Unix.inet_addr_loopback, 9009

let print_sockaddr = function
  | Unix.ADDR_UNIX s -> print_endline ("UNIX:" ^ s)
  | ADDR_INET (host, port) -> print_endline ("INET:" ^ Unix.string_of_inet_addr host ^ ":" ^ string_of_int port)

let listening_sock iocp =
  let addr = Unix.(ADDR_INET (host, port)) in
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind sock addr;
  Unix.listen sock 0;
  let sock_accept = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt sock SO_REUSEADDR true;
  print_sockaddr addr;
  (* Unix.setsockopt sock SO_REUSEPORT true; *)
  Iocp.handle_of_fd iocp sock 1, Iocp.handle_of_fd iocp sock_accept 2

let listen () =
  let iocp = Iocp.create 5 in
  let sock, sock_accept = listening_sock iocp in
  Format.eprintf "Got an accepting socket\n%!";
  let addr_buf = Iocp.Accept_buffer.create () in
  let id = Iocp.accept iocp sock sock_accept addr_buf in
  Format.eprintf "Called accept\n%!";
  match Iocp.completion_status iocp ~timeout:1000 with
  | None -> assert false
  | Some t ->
    assert (Iocp.Id.equal t.id id);
    let client_addr = Iocp.Accept_buffer.get_remote addr_buf sock in
    print_sockaddr @@ Iocp.Sockaddr.get client_addr;
    let buf = Cstruct.create 4096 in
    let id2 = Iocp.recv iocp sock_accept [buf] in
    match Iocp.completion_status iocp ~timeout:1000 with
    | None -> assert false
    | Some t ->
      assert (Iocp.Id.equal t.id id2);
      print_endline (Cstruct.to_string buf)

let connect () =
  let iocp = Iocp.create 4 in
  let addr = Unix.(ADDR_INET (host, port)) in
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind sock (ADDR_INET (Unix.inet_addr_any, 0)); 
  let sock = Iocp.handle_of_fd iocp sock 3 in
  let id = Iocp.connect iocp sock (Iocp.Sockaddr.of_unix addr) in
  match Iocp.completion_status iocp ~timeout:1000 with
  | None -> assert false
  | Some t ->
    assert (Iocp.Id.equal t.id id);
    print_endline "Connected! Now sending data...";
    let buf = Cstruct.of_string "Hello socket!" in
    let id2 = Iocp.send iocp sock [buf] in
    print_endline "Data queued up...";
    match Iocp.completion_status iocp ~timeout:1000 with
    | None -> assert false
    | Some t ->
      assert (Iocp.Id.equal t.id id2);
      print_endline "All done!"
    

let () =
  try if Sys.argv.(1) = "listen" then listen () else connect () with _ -> connect ()
