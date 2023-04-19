type t
(* A handle to an I/O completion port *)

type id = int
(* An identifier associated with a Handle *)

(* Completion Port Management *)
external create_io_completion_port : int -> t
  = "ocaml_iocp_create_io_completion_port"

(* File descriptors must be associated with an IOCP before we do any IO on them *)
external associate_fd_with_iocp : t -> Unix.file_descr -> id -> Handle.t
  = "ocaml_iocp_associate_fd_with_iocp"

external openfile :
  t -> id -> string -> Unix.open_flag list -> Unix.file_perm -> Handle.t
  = "ocaml_iocp_unix_open"

external pipe' : t -> id -> id -> string -> Handle.t * Handle.t
  = "ocaml_iocp_unix_pipe"

let pipe iocp id1 id2 name =
  let path = "\\\\.\\pipe\\" ^ name in
  pipe' iocp id1 id2 path

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

external peek : t -> completion_status = "ocaml_iocp_peek"

type unsafe_completion_status = {
  mutable handle_id : id;
  mutable bytes_transferred : int;
  mutable overlapped_id : int;
  mutable success : bool;
  mutable err : int;
}

external get_queued_completion_status_unsafe :
  t -> int -> unsafe_completion_status -> unit
  = "ocaml_iocp_get_queued_completion_status_unsafe"

let make_unsafe_completion_status () =
  {
    handle_id = 0;
    bytes_transferred = 0;
    overlapped_id = 0;
    success = false;
    err = 0;
  }

(* Operations *)
external read :
  t -> Handle.t -> Cstruct.buffer -> int -> int -> 'a Overlapped.t -> unit
  = "ocaml_iocp_read_bytes" "ocaml_iocp_read"

external write :
  t -> Handle.t -> Cstruct.buffer -> int -> int -> 'a Overlapped.t -> unit
  = "ocaml_iocp_write_bytes" "ocaml_iocp_write"

external accept :
  Handle.t -> Handle.t  -> Cstruct.buffer -> 'a Overlapped.t -> unit
  = "ocaml_iocp_accept"

external connect :
  t -> Handle.t -> Sockaddr.t -> 'a Overlapped.t -> unit
  = "ocaml_iocp_connect"

external send : t -> Handle.t -> Wsabuf.t -> 'a Overlapped.t -> unit
  = "ocaml_iocp_send"

external recv : t -> Handle.t -> Wsabuf.t  -> 'a Overlapped.t -> unit
  = "ocaml_iocp_recv"

external cancel : Handle.t -> 'a Overlapped.t -> unit = "ocaml_iocp_cancel"
