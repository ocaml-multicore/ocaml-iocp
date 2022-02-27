(* TCP Network connections *)
let host, port = Unix.inet_addr_loopback, 8080

let listening_sock () =
  let addr = Unix.(ADDR_INET (host, port)) in
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind sock addr;
  Unix.listen sock 10;
  let sock_accept = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt sock SO_REUSEADDR true;
  (* Unix.setsockopt sock SO_REUSEPORT true; *)
  Iocp.Handle.of_unix sock, Iocp.Handle.of_unix sock_accept

let print_sockaddr = function
  | Unix.ADDR_UNIX s -> print_endline ("UNIX:" ^ s)
  | ADDR_INET (host, port) -> print_endline ("INET:" ^ Unix.string_of_inet_addr host ^ ":" ^ string_of_int port)

type req = [ `A | `R ]

let () =
  let iocp = Iocp.create () in
  let sock, sock_accept = listening_sock () in
  let addr_buf = Iocp.Accept_buffer.create () in
  let _job = Iocp.accept iocp sock sock_accept addr_buf `A (Iocp.Overlapped.create ()) in
  match Iocp.get_queued_completion_status iocp with
  | None -> assert false
  | Some t ->
    assert (t.data = `A);
    let sockaddr = Iocp.Accept_buffer.get_remote addr_buf sock in
    let unix = Iocp.Sockaddr.get sockaddr in
    print_sockaddr unix;
    let buf = Cstruct.create 1024 in
    let _job = Iocp.read iocp ~file_offset:0 sock_accept buf ~off:0 ~len:1024 `R in
    match Iocp.get_queued_completion_status iocp with
    | None -> assert false
    | Some t ->
      assert (t.data = `R);
      print_endline (Cstruct.to_string buf)
