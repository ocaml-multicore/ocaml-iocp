(*----------------------------------------------------------------------
   Copyright (c) 2022 Patrick Ferris <patrick@sirref.org>
   Distributed under the MIT license. See terms at the end of this file.
  ----------------------------------------------------------------------*)

module Overlapped = struct
  type t

  type offset = Optint.Int63.t

  external create : offset -> t = "ocaml_iocp_make_overlapped"
  let create ?(off = Optint.Int63.zero) () =
    create off
end

module Wsabuf = struct
  type wsabuf

  type t = wsabuf * int * Cstruct.t list
  
  external create : Cstruct.t list -> int -> wsabuf = "ocaml_iocp_make_wsabuf"
  
  let create bufs =
    let len = List.length bufs in
    (create bufs len, len, bufs)
end

module Sockaddr = Sockaddr

module Handle = struct
  type t = Unix.file_descr
  let to_unix t = t
  external openfile : string -> Unix.open_flag list -> Unix.file_perm -> t = "ocaml_iocp_unix_open"
  external cancel : t -> Overlapped.t -> unit = "ocaml_iocp_cancel"
  external update_accept_ctx : t -> unit = "ocaml_iocp_update_accept_ctx"
  external update_connect_ctx : t -> unit = "ocaml_iocp_update_connect_ctx"
  external shutdown : t -> Unix.shutdown_command -> unit = "caml_iocp_shutdown"
  external pipe' : string -> t * t = "ocaml_iocp_unix_pipe"

  let pipe name =
    let path = "\\\\.\\pipe\\" ^ name in
    pipe' path

  let of_unix t = t
end

(* AcceptEx only puts data (like the remote address) in a special buffer that needs to
   be parsed by [GetAcceptExSockaddrs]. We don't care about the first message received 
   so we don't need that much space in the buffer. *)
module Accept_buffer = struct
  type t = Cstruct.t
  
  external get_remote : Cstruct.buffer -> Handle.t -> Sockaddr.t -> unit = "ocaml_iocp_get_accept_ex_sockaddr"

  let get_remote t h =
    let s = Sockaddr.create () in
    get_remote (Cstruct.to_bigarray t) h s;
    s

  let create () =
    (* TODO: get sizeof(sock_addr_union) to work the size out properly *)
    let buf = Cstruct.create 256 in
    buf
end

(* Bindings to C API *)
module Iocp = struct
  type t = Handle.t
  (* A handle to an I/O completion port *)

  type id = Heap.ptr

  (* Completion Port Management *)
  external create_io_completion_port : int -> t = "ocaml_iocp_create_io_completion_port"
  external create_io_completion_port_with_fd : Handle.t -> 'a -> int -> t = "ocaml_iocp_create_io_completion_port_with_fd"

  type completion_status = private
    | Cs_none
    | Cs_some of { heap_ptr : id; bytes_transferred : int } 

  external get_queued_completion_status : t -> Overlapped.t -> completion_status = "ocaml_iocp_get_queued_completion_status"
  
  let get_queued_completion_status t =
    let o = Overlapped.create () in
    get_queued_completion_status t o

  external peek : t -> completion_status = "ocaml_iocp_peek" [@@noalloc]

  (* Operations *)
  external read : t -> Handle.t -> id -> Cstruct.buffer -> int -> int -> Overlapped.t -> bool = "ocaml_iocp_read_bytes" "ocaml_iocp_read" [@@noalloc]
  external write : t -> Handle.t -> id -> Cstruct.buffer -> int -> int -> Overlapped.t -> bool = "ocaml_iocp_write_bytes" "ocaml_iocp_write" [@@noalloc]
  external accept : t -> Handle.t -> Handle.t -> id -> Cstruct.buffer -> Overlapped.t -> bool = "ocaml_iocp_accept_bytes" "ocaml_iocp_accept" [@@noalloc]
  external connect : t -> Handle.t -> Sockaddr.t -> id -> Overlapped.t -> bool = "ocaml_iocp_connect" [@@noalloc]
  external send : t -> Handle.t -> Wsabuf.t -> id -> Overlapped.t -> bool = "ocaml_iocp_send" [@@noalloc]
  external recv : t -> Handle.t -> Wsabuf.t -> id -> Overlapped.t -> bool = "ocaml_iocp_recv" [@@noalloc]
end

type 'a t = {
  id : < >;
  iocp : Iocp.t;
  data : 'a Heap.t;
  mutable dirty : bool;
}

module Generic_ring = struct
  type ring = T : 'a t -> ring
  type t = ring
  let compare (T a) (T b) = compare a.id b.id
end

module Ring_set = Set.Make(Generic_ring)

(* Garbage collection and buffers shared with the Linux kernel.
   Many uring operations involve passing Linux the address of a buffer to which it
   should write the results. This means that both Linux and OCaml have pointers to the
   buffer, and it must not be freed until both have finished with it, but the OCaml
   garbage collector doesn't know this. To avoid OCaml's GC freeing the buffer while
   Linux is still using it:
   - We attach all such buffers to their [t.data] entry, so they don't get freed until
     the job is complete, even if the caller loses interest in the buffer.
   - We add the ring itself to the global [gc_roots] set, so that [t.data] can't be freed
     unless [exit] is called, which checks that there are no operations in progress. *)
let gc_roots = Atomic.make Ring_set.empty

let rec update_gc_roots fn =
  let old_set = Atomic.get gc_roots in
  let new_set = fn old_set in
  if not (Atomic.compare_and_set gc_roots old_set new_set) then
    update_gc_roots fn

let register_gc_root t =
  update_gc_roots (Ring_set.add (Generic_ring.T t))

let unregister_gc_root t =
  update_gc_roots (Ring_set.remove (Generic_ring.T t))

let create ?(threads=0) () =
  let id = object end in
  let t = { id; iocp = Iocp.create_io_completion_port threads; data = Heap.create 128; dirty = false } in
  register_gc_root t;
  t

let create_with_fd ?(threads=0) fd data =
  let id = object end in
  let t = { id; iocp = Iocp.create_io_completion_port_with_fd fd data threads; data = Heap.create 128; dirty = false } in
  register_gc_root t;
  t

type 'a completion_status = { data : 'a; bytes_transferred : int }

let fn_on_iocp fn t =
  match fn t.iocp with
  | Iocp.Cs_none -> None
  | Cs_some { heap_ptr; bytes_transferred } ->
    let data = Heap.free t.data heap_ptr in
    Some { data; bytes_transferred }

let get_queued_completion_status t = 
  fn_on_iocp Iocp.get_queued_completion_status t

let peek t =
  fn_on_iocp Iocp.peek t

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
  
(* let with_id t fn a = with_id_full t fn a ~extra_data:() *)

let read t ~file_offset fd buf ~off ~len data ol =
  with_id_full t (fun id -> Iocp.read t.iocp fd id (Cstruct.to_bigarray buf) len off ol) data ~extra_data:buf

let write t ~file_offset fd buf ~off ~len data ol =
  let ol = Overlapped.create ~off:file_offset () in
  with_id_full t (fun id -> Iocp.write t.iocp fd id (Cstruct.to_bigarray buf) len off ol) data ~extra_data:buf

let accept t fd acc buf data ol =
  let accept_buffer = Cstruct.to_bigarray buf in
  with_id_full t (fun id -> Iocp.accept t.iocp fd acc id accept_buffer ol) data ~extra_data:(acc, buf, ol, accept_buffer)

let connect t fd addr data ol =
  with_id_full t (fun id -> Iocp.connect t.iocp fd (Sockaddr.of_unix addr) id ol) data ~extra_data:(addr, ol)

let send t sock bufs data ol =
  let wsabuf = Wsabuf.create bufs in
  with_id_full t (fun id -> Iocp.send t.iocp sock wsabuf id ol) data ~extra_data:wsabuf

let recv t sock bufs data ol =
  let wsabuf = Wsabuf.create bufs in
  with_id_full t (fun id -> Iocp.recv t.iocp sock wsabuf id ol) data ~extra_data:wsabuf

(*---------------------------------------------------------------------------
  Copyright (c) 2022 <patrick@sirref.org>
  
  Permission to use, copy, modify, and/or distribute this software for any
  purpose with or without fee is hereby granted, provided that the above
  copyright notice and this permission notice appear in all copies.
  
  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
  DEALINGS IN THE SOFTWARE.
  ---------------------------------------------------------------------------*)