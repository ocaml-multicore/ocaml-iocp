(*----------------------------------------------------------------------
   Copyright (c) 2022 Patrick Ferris <patrick@sirref.org>
   Distributed under the MIT license. See terms at the end of this file.
  ----------------------------------------------------------------------*)

module Wsabuf = Wsabuf
module Sockaddr = Sockaddr
module Handle = Handle
module Overlapped = Overlapped
module Raw = Raw
module Managed = Managed
module Accept_buffer = Accept_buffer

type 'a t = {
  id : < >;
  iocp : Raw.t;
  data : 'a Heap.t;
}

let openfile t = Raw.openfile t.iocp
let pipe t = Raw.pipe t.iocp

module Generic_ring = struct
  type ring = T : 'a t -> ring
  type t = ring

  let compare (T a) (T b) = compare a.id b.id
end

module Ring_set = Set.Make (Generic_ring)

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

let register_gc_root t = update_gc_roots (Ring_set.add (Generic_ring.T t))

(* let unregister_gc_root t =
   update_gc_roots (Ring_set.remove (Generic_ring.T t)) *)

let create ?(threads = 0) () =
  let id = object end in
  let t =
    {
      id;
      iocp = Raw.create_io_completion_port threads;
      data = Heap.create 128;
    }
  in
  register_gc_root t;
  t

type 'a completion_status = { data : 'a; bytes_transferred : int }

let fn_on_iocp fn t =
  match fn t.iocp with
  | Raw.Cs_none -> None
  | Cs_some { handle_id; bytes_transferred; _ } ->
      let data = Heap.free t.data handle_id in
      Some { data; bytes_transferred }

let get_queued_completion_status t timeout =
  fn_on_iocp (fun t -> Raw.get_queued_completion_status t timeout) t

let peek t = fn_on_iocp Raw.peek t

type 'a job = 'a Heap.entry

(* let with_id_full :
    type a. a t -> (Heap.ptr -> bool) -> a -> extra_data:'b -> a job option =
 fun t fn datum ~extra_data ->
  print_endline "with_id_full...";
  match Heap.alloc t.data datum ~extra_data with
  | exception Heap.No_space ->
      print_endline "no space";
      None
  | entry ->
      let ptr = Heap.ptr entry in
      let has_space = fn ptr in
      if has_space then (
        print_endline "has space";
        (* t.dirty <- true; *)
        Some entry)
      else (
        print_endline "no space";
        ignore (Heap.free t.data ptr : a);
        None) *)

(* let with_id t fn a = with_id_full t fn a ~extra_data:() *)

(* let read t fd buf ~off ~len ol =
  with_id_full t
    (fun _id ->
      print_endline "about to submit2...";
      Raw.read t.iocp fd (Cstruct.to_bigarray buf) len off ol;
      true)
    ol ~extra_data:buf

let write t fd buf ~off ~len data ol =
  with_id_full t
    (fun _id ->
      Raw.write t.iocp fd (Cstruct.to_bigarray buf) len off ol;
      true)
    data ~extra_data:buf

let accept t fd acc buf data ol =
  let accept_buffer = Cstruct.to_bigarray buf in
  with_id_full t
    (fun id -> Raw.accept t.iocp fd acc id accept_buffer ol)
    data
    ~extra_data:(acc, buf, ol, accept_buffer)

let connect t fd addr data ol =
  with_id_full t
    (fun id -> Raw.connect t.iocp fd (Sockaddr.of_unix addr) id ol)
    data ~extra_data:(addr, ol) *)

(* let send t sock bufs data ol =
  let wsabuf = Wsabuf.create bufs in
  with_id_full t
    (fun id -> Raw.send t.iocp sock wsabuf id ol)
    data ~extra_data:wsabuf

let recv t sock bufs data ol =
  let wsabuf = Wsabuf.create bufs in
  with_id_full t
    (fun id -> Raw.recv t.iocp sock wsabuf id ol)
    data ~extra_data:wsabuf *)

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
