
module Overlapped = struct
  type t

  external create : unit -> t = "ocaml_iocp_make_overlapped"
end

module Handle = struct
  type t = Unix.file_descr
  let to_unix t = t
  external openfile : string -> Unix.open_flag list -> Unix.file_perm -> t = "ocaml_iocp_unix_open"
end

(* Bindings to C API *)
module Iocp = struct
  type t = Handle.t
  (* A handle to an I/O completion port *)

  type id = Heap.ptr

  (* Completion Port Management *)
  external create_io_completion_port : 'a -> int -> t = "ocaml_iocp_create_io_completion_port"
  external create_io_completion_port_with_fd : Handle.t -> 'a -> int -> t = "ocaml_iocp_create_io_completion_port_with_fd"

  type completion_status = private
    | Cs_none
    | Cs_some of { heap_ptr : id; bytes_transferred : int } 

  external get_queued_completion_status : t -> completion_status = "ocaml_iocp_get_queued_completion_status"
  (* external get_queue_completion_status_timeout : t -> int -> completion_status = "ocaml_iocp_get_queued_completion_status_timeout"  *)

  (* Operations *)
  external read_file_exn : t -> Handle.t -> id -> Cstruct.buffer -> int -> Overlapped.t -> bool = "ocaml_iocp_read_file_bytes" "ocaml_iocp_read_file"
end

type 'a t = { 
  iocp : Iocp.t;
  data : 'a Heap.t;
  mutable dirty : bool;
}

let create ?(threads=0) data =
  { iocp = Iocp.create_io_completion_port data threads; data = Heap.create 128; dirty = false }

let create_with_fd ?(threads=0) fd data = 
  { iocp = Iocp.create_io_completion_port_with_fd fd data threads; data = Heap.create 128; dirty = false }

type 'a completion_status = { data : 'a; bytes_transferred : int }

let get_queued_completion_status t = 
  match Iocp.get_queued_completion_status t.iocp with
    | Cs_none -> None
    | Cs_some { heap_ptr; bytes_transferred } ->
      let data = Heap.free t.data heap_ptr in
      Some { data; bytes_transferred }

type 'a job = 'a Heap.entry

let with_id_full : type a. a t -> (Heap.ptr -> bool) -> a -> extra_data:'b -> a job option =
  fun t fn datum ~extra_data ->
    match Heap.alloc t.data datum ~extra_data with
    | exception Heap.No_space -> None
    | entry ->
      let ptr = Heap.ptr entry in
      let has_space = fn ptr in
      if has_space then (
        t.dirty <- true;
        Some entry
      ) else (
        ignore (Heap.free t.data ptr : a);
        None
      )
  
let with_id t fn a = with_id_full t fn a ~extra_data:()

let read_file_exn t fd buf num_bytes ol =
  with_id t (fun id -> Iocp.read_file_exn t.iocp fd id (Cstruct.to_bigarray buf) num_bytes ol) 10