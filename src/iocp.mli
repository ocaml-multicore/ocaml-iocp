(** {2 Input/Output Completion Ports} 
    
    Bindings to input/output completion ports (IOCP) allowing for efficient,
    asynchronous IO on Windows. *)

module Wsabuf = Wsabuf
module Overlapped = Overlapped
module Handle = Handle
module Raw = Raw
module Managed = Managed
module Accept_buffer = Accept_buffer
module Sockaddr = Sockaddr

type t
type fd = Handle.t

exception Out_of_overlapped

module Id : sig
  type t
  val compare : t -> t -> int
  val hash : t -> int
  val equal : t -> t -> bool
  val to_int : t -> int
end

module H : Hashtbl.S with type key = Id.t

val create : int -> t

val handle_of_fd : t -> Unix.file_descr -> int -> fd

val openfile: t -> int -> string -> Unix.open_flag list -> int -> fd

val read : t -> fd -> Cstruct.buffer -> pos:int -> len:int -> off:Optint.Int63.t -> Id.t

val write : t -> fd -> Cstruct.buffer -> pos:int -> len:int -> off:Optint.Int63.t -> Id.t

val accept : t -> fd -> Handle.t -> Accept_buffer.t -> Id.t

val recv : t -> fd -> Cstruct.t list -> Id.t

val send : t -> fd -> Cstruct.t list -> Id.t

val connect : t -> fd -> Sockaddr.t -> Id.t

type completion_status = {
  bytes_transferred : int;
  id : Id.t;
  error : Unix.error option;
}

val completion_status : t -> timeout:int -> completion_status option