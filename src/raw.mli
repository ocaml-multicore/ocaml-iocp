type t
type id = int

val create_io_completion_port : int -> t

external associate_fd_with_iocp : t -> Unix.file_descr -> id -> Handle.t
  = "ocaml_iocp_associate_fd_with_iocp"

external openfile :
  t -> id -> string -> Unix.open_flag list -> Unix.file_perm -> Handle.t
  = "ocaml_iocp_unix_open"

val pipe : t -> id -> id -> string -> Handle.t * Handle.t

type completion_status = private
  | Cs_none
  | Cs_some of {
      handle_id : id;
      bytes_transferred : int;
      overlapped_id : int;
      error : Unix.error option;
    }

external get_queued_completion_status : t -> int -> completion_status
  = "ocaml_iocp_get_queued_completion_status"

external peek : t -> completion_status = "ocaml_iocp_peek" [@@noalloc]

type unsafe_completion_status = private {
  mutable handle_id : id;
  mutable bytes_transferred : int;
  mutable overlapped_id : int;
      (* Nb, first 3 fields identical to Some clause of completion_status above*)
  mutable success : bool;
  mutable err : int;
}

external get_queued_completion_status_unsafe :
  t -> int -> unsafe_completion_status -> unit
  = "ocaml_iocp_get_queued_completion_status_unsafe" [@@noalloc]

val make_unsafe_completion_status : unit -> unsafe_completion_status

(* Operations *)
external read :
  t -> Handle.t -> Cstruct.buffer -> int -> int -> 'a Overlapped.t -> unit
  = "ocaml_iocp_read_bytes" "ocaml_iocp_read"
  [@@noalloc]

external write :
  t -> Handle.t -> Cstruct.buffer -> int -> int -> 'a Overlapped.t -> unit
  = "ocaml_iocp_write_bytes" "ocaml_iocp_write"
  [@@noalloc]

external accept :
  Handle.t -> Handle.t -> Cstruct.buffer -> 'a Overlapped.t -> unit
  = "ocaml_iocp_accept"
  [@@noalloc]

external connect :
  t -> Handle.t -> Sockaddr.t -> 'a Overlapped.t -> unit
  = "ocaml_iocp_connect"
  [@@noalloc]
external send : t -> Handle.t -> Wsabuf.t -> 'a Overlapped.t -> unit = "ocaml_iocp_send" [@@noalloc]
external recv : t -> Handle.t -> Wsabuf.t -> 'a Overlapped.t -> unit = "ocaml_iocp_recv" [@@noalloc] 
external cancel : Handle.t -> 'a Overlapped.t -> unit = "ocaml_iocp_cancel" [@@noalloc]
