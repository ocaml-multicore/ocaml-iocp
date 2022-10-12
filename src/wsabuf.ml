type wsabuf

type t = wsabuf * int * Cstruct.t list

external create : Cstruct.t list -> int -> wsabuf = "ocaml_iocp_make_wsabuf"

let create bufs =
  let len = List.length bufs in
  (create bufs len, len, bufs)