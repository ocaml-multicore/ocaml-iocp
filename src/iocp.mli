(** {2 Input/Ouput Completion Ports} 
    
    Bindings to input/output completion ports (IOCP) allowing for efficient,
    asynchronous IO on Windows. *)

module Overlapped : sig
  type t
  (** An overlapped structure used for asynchronous IO. *)

  val create : unit -> t
  (** The default structure that is completely empty. *)
end

module Handle : sig
    type t = private Unix.file_descr
    (** A handle to the device which could be a file, a console buffer,
        a socket, pipe etc. *)
    
    val openfile : string -> Unix.open_flag list -> Unix.file_perm -> t
    (** An exact copy {! Unix.openfile} except the [FILE_FLAG_OVERLAPPED] is added
        to the flags when opening the file. *)

    val to_unix : t -> Unix.file_descr
    (** Converts a handle to a unix file-descriptor. Note you can also do type coercion too,
        such as [(fd :> Unix.file_descr)]. *)

    val of_unix : Unix.file_descr -> t
    (** Potentially dangerous -- this function exists to allow other async-ready file descriptors 
        to be coerced to the right type. Converting an actual file that was not opened with [FILE_FLAG_OVERLAPPED]
        will result in errors, use {! openfile} for that instead. *)
end

type 'a t
(** A completion port that passes key data of type ['a]. *)

type 'a job
(** A handle to an in-flight asynchronous job, this can be used to cancel the job
    in some cases. *)

val create_with_fd : ?threads:int -> Handle.t -> 'a -> 'a t
(** [create ?threads fd data] makes a new IOCP for the handle [fd], associating
    the key [data] to the completion port. [threads] is the maximum number of 
    concurrent threads the operating system can allow to process completion packets. 
    
    @param fd An open handle that supports overlapped (asynchronous) IO. 
    @param data A per-handle user-defined completion key. 
    @param threads Maximum number of threads to process completion packets, if [None]
           then [0] is passed which allows as many threads as there are processors in the 
           system. *)

val create : ?threads:int -> unit -> 'a t
(** Like {! create_with_fd} except no handle is associated with the completion port. *)

type 'a completion_status = { data : 'a; bytes_transferred : int }
(** A completion status  *)

val get_queued_completion_status : 'a t  -> 'a completion_status option
(** [get_queued_completion_status t] will wait indefinitely for a completion packet to arrive 
    at the completion port [t]. *)

(** {2 File Operations} *)

val read_file : 'a t -> Handle.t -> Cstruct.t -> int -> 'a -> Overlapped.t -> 'a job option
val write_file : 'a t -> Handle.t -> Cstruct.t -> int -> 'a -> Overlapped.t -> 'a job option

(** {2 Networking} *)
module Sockaddr = Sockaddr

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

val accept : 'a t -> Handle.t -> Handle.t -> Accept_buffer.t -> 'a -> Overlapped.t -> 'a job option
(** [accept t listen accept buf data o] accepts connection on the socket [accept] coming into the
    address [listen] is listening too and pushes completion packets to [t]. The accept buffer [buf]
    will be updated to contain the remote address of the incoming connection. 
    
    For more information, {{: https://docs.microsoft.com/en-us/windows/win32/api/mswsock/nf-mswsock-acceptex#parameters}
    the Microsoft documentation explains the parameters}.*)