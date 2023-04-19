(* AcceptEx only puts data (like the remote address) in a special buffer that needs to
   be parsed by [GetAcceptExSockaddrs]. We don't care about the first message received
   so we don't need that much space in the buffer. *)
   
type t = Cstruct.t

external get_remote : Cstruct.buffer -> Handle.t -> Sockaddr.t -> unit
  = "ocaml_iocp_get_accept_ex_sockaddr"

let get_remote t h =
  let s = Sockaddr.create () in
  get_remote (Cstruct.to_bigarray t) h s;
  s

let create () =
  (* TODO: get sizeof(sock_addr_union) to work the size out properly *)
  let buf = Cstruct.create 256 in
  buf