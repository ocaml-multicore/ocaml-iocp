(* {1 Input/Output Completion Ports}*)

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
end

(** {2 Input/Ouput Completion Ports} 
    
    Bindings to input/output completion ports (IOCP) allowing for efficient,
    asynchronous IO on Windows.*)

type 'a t

type 'a job

val create : ?threads:int -> unit -> 'a t

val create_with_fd : ?threads:int -> Handle.t -> 'a -> 'a t
(** [create ?threads fd data] makes a new IOCP for the handle [fd], associating
    the key [data] to the completion port. [threads] is the maximum number of 
    concurrent threads the operating system can allow to process completion packets. 
    
    @param fd An open handle that supports overlapped (asynchronous) IO. 
    @param data A per-handle user-defined completion key. 
    @param threads Maximum number of threads to process completion packets, if [None]
           then [0] is passed which allows as many threads as there are processors in the 
           system. *)

type 'a completion_status = { data : 'a; bytes_transferred : int }

val get_queued_completion_status : 'a t  -> 'a completion_status option

(** {2 File Operations} *)

val read_file : 'a t -> Handle.t -> Cstruct.t -> int -> 'a -> Overlapped.t -> 'a job option
val write_file : 'a t -> Handle.t -> Cstruct.t -> int -> 'a -> Overlapped.t -> 'a job option