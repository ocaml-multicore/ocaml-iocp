(** An overlapped structure used for asynchronous IO. This is the primary key used to
    associate completed IO operations with the original request. It's also used for
    cancellation. *)

type 'a t
(** An ocaml value [v] of type [t] represents a windows OVERLAPPED structure that has been
    allocated out of heap. When [v] is garbage collected the OVERLAPPED structure will
    be freed, so it is important to ensure the value is alive for the entire duration
    of an asynchronous IO operation. *)

type offset := Optint.Int63.t

val create : ?off:offset -> 'a -> 'a t
(** [create ~off key] A key is associated with the value at creation time, with the
    condition that the value is an immediate rather than a block. This key can be
    recovered via the [unsafe_key] function. *)

external set_offset : 'a t -> offset -> unit = "ocaml_iocp_set_overlapped_off"
(** Set the offset associated with an OVERLAPPED. This offset is used in some IO
    operations. *)

external set_key : 'a t -> 'a -> unit = "ocaml_iocp_set_overlapped_key"
(** Sets the key *)

val get_key : 'a t -> 'a
(** Gets the key *)

val id : 'a t -> int
(** [id v] returns the id of v. The id is an integer that uniquely specifies the
    OVERLAPPED structure and is returned in the {Iocp.Raw.completion_status}
    type. *)

val unsafe_key : int -> 'a
(** [unsafe_key i] returns the key associated with [v], where [i = id v].
    Unsafe in that applying this function to an arbitrary integer may cause a crash. *)
