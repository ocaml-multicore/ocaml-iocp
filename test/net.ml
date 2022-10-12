(* TCP Network connections *)
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
  Iocp.Raw.associate_fd_with_iocp iocp sock 1, Iocp.Raw.associate_fd_with_iocp iocp sock_accept 2

let listen () =
  let iocp = Iocp.Raw.create_io_completion_port 5 in
  let sock, sock_accept = listening_sock iocp in
  Format.eprintf "Got an accepting socket\n%!";
  let addr_buf = Iocp.Accept_buffer.create () in
  let ol = Iocp.Overlapped.create 1024 in
  let ol_id = Iocp.Overlapped.id ol in

  let accept_buffer = Cstruct.to_bigarray addr_buf in
  let () = Iocp.Raw.accept sock sock_accept accept_buffer ol in
  Format.eprintf "Called accept\n%!";
  match Iocp.Raw.get_queued_completion_status iocp 1000 with
  | Cs_none -> assert false
  | Cs_some t ->
    assert (t.overlapped_id = ol_id);
    assert (Iocp.Overlapped.unsafe_key t.overlapped_id = 1024);
    let client_addr = Iocp.Accept_buffer.get_remote addr_buf sock in
    print_sockaddr @@ Iocp.Sockaddr.get client_addr;
    let buf = Cstruct.create 4096 in
    let wsabuf = Iocp.Wsabuf.create [buf] in
    Iocp.Raw.recv iocp sock_accept wsabuf ol;
    match Iocp.Raw.get_queued_completion_status iocp 1000 with
    | Cs_none -> assert false
    | Cs_some t ->
      assert (t.overlapped_id = ol_id);
      print_endline (Cstruct.to_string buf)

let connect () =
  let iocp = Iocp.Raw.create_io_completion_port 4 in
  let addr = Unix.(ADDR_INET (host, port)) in
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind sock (ADDR_INET (Unix.inet_addr_any, 0)); 
  let sock = Iocp.Raw.associate_fd_with_iocp iocp sock 3 in
  let ol = Iocp.Overlapped.create 1025 in
  let ol_id = Iocp.Overlapped.id ol in

  let () = Iocp.Raw.connect iocp sock (Iocp.Sockaddr.of_unix addr) ol in
  match Iocp.Raw.get_queued_completion_status iocp 1000 with
  | Cs_none -> assert false
  | Cs_some t ->
    Format.eprintf "overlapped_id=%d\n%!" (Iocp.Overlapped.unsafe_key t.overlapped_id);
    assert (t.overlapped_id = ol_id);
    (* Gc.full_major (); *)
    print_endline "Connected! Now sending data...";
    let buf = Cstruct.of_string "Hello socket!" in
    let wsabuf = Iocp.Wsabuf.create [buf] in
    Iocp.Raw.send iocp sock wsabuf ol ;
    (* Gc.full_major (); *)
    print_endline "Data queued up...";
    match Iocp.Raw.get_queued_completion_status iocp 1000 with
    | Cs_none -> assert false
    | Cs_some t ->
      assert (t.overlapped_id = ol_id);
      print_endline "All done!"
    

let () =
  try if Sys.argv.(1) = "listen" then listen () else connect () with _ -> connect ()