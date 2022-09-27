(** {2 Input/Ouput Completion Ports} 
    
    Bindings to input/output completion ports (IOCP) allowing for efficient,
    asynchronous IO on Windows. *)

module Overlapped = Overlapped
module Handle = Handle
module Raw = Raw
module Safest = Safest

type 'a t
(** A completion port that passes key data of type ['a]. *)

type 'a job
(** A handle to an in-flight asynchronous job, this can be used to cancel the job
    in some cases. *)

val create : ?threads:int -> unit -> 'a t
(** Like {! create_with_fd} except no handle is associated with the completion port. *)

val openfile :
  'a t ->
  Heap.ptr ->
  string ->
  Unix.open_flag list ->
  Unix.file_perm ->
  Handle.t
(** An exact copy {! Unix.openfile} except the [FILE_FLAG_OVERLAPPED] is added
    to the flags when opening the file. *)

val pipe : 'a t -> Heap.ptr -> Heap.ptr -> string -> Handle.t * Handle.t
(** [pipe name] creates a named pipe ready for asynchronous operations. *)

type 'a completion_status = { data : 'a; bytes_transferred : int }
(** A completion status  *)

val get_queued_completion_status : 'a t -> int -> 'a completion_status option
(** [get_queued_completion_status t] will wait indefinitely for a completion packet to arrive 
    at the completion port [t]. *)

val peek : 'a t -> 'a completion_status option
(** [peek t] is like {! get_queued_completion_status} but will not wait indefinitely. *)

(* val read :
  'a t ->
  Handle.t ->
  Cstruct.t ->
  off:int ->
  len:int ->
  'a ->
  'b Overlapped.t ->
  'a job option
(** [read t ~file_offset fd buf ~off ~len d] reads [len] bytes of data from [fd] at a given absolute 
    offset [file_offset] using [t] as the completion port for the read request. The data is read into [buf] at offset [off].
    [d] is the user associated data for the request. *)

val write :
  'a t ->
  Handle.t ->
  Cstruct.t ->
  off:int ->
  len:int ->
  'a ->
  'b Overlapped.t ->
  'a job option
(** [write t ~file_offset fd buf ~off ~len d] writes [len] bytes of data to [fd] at a given absolute offset [file_offset]
    using [t] as the completion port for the write request. Data is read from [buf] at offset [off]. [d] is the user associated data for 
    the request. *) *)

module Sockaddr = Sockaddr
(** {2 Networking} *)

(** An accept buffer is used to store information from a connection upon being accepted *)
module Accept_buffer : sig
  type t
  (** An accept buffer *)

  val create : unit -> t
  (** Create a fresh new buffer to be used with an {! accept} request. *)

  val get_remote : t -> Handle.t -> Sockaddr.t
  (** [get_remote t fd] returns the remote socket address of a connection. The handle 
      should be the listening socket in the connection. *)
end

(* val accept :
  'a t ->
  Handle.t ->
  Handle.t ->
  Accept_buffer.t ->
  'a ->
  'b Overlapped.t ->
  'a job option
(** [accept t listen accept buf data o] accepts connection on the socket [accept] coming into the
    address [listen] is listening too and pushes completion packets to [t]. The accept buffer [buf]
    will be updated to contain the remote address of the incoming connection. 
    
    For more information, {{: https://docs.microsoft.com/en-us/windows/win32/api/mswsock/nf-mswsock-acceptex#parameters}
    the Microsoft documentation explains the parameters}.*)

val connect :
  'a t -> Handle.t -> Unix.sockaddr -> 'a -> 'b Overlapped.t -> 'a job option *)

(* val send :
  'a t -> Handle.t -> Cstruct.t list -> 'a -> 'b Overlapped.t -> 'a job option

val recv :
  'a t -> Handle.t -> Cstruct.t list -> 'a -> 'b Overlapped.t -> 'a job option *)
