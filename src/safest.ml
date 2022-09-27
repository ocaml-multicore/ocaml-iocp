(* Safest *)

module H = Hashtbl.Make(
  struct
    type t = int
    let hash = Hashtbl.hash
    let equal : t -> t -> bool = fun x y -> x = y
  end)

type t = {
    iocp : Raw.t;
    m : Mutex.t;
    in_flight : (Cstruct.buffer * int Overlapped.t) H.t
}

type completion_status = {
  bytes_transferred : int;
  id : int;
  error : Unix.error option;
}

let create n =
  { iocp = Raw.create_io_completion_port n
  ; m = Mutex.create ()
  ; in_flight = H.create 255 }

let read : t -> Handle.t -> Cstruct.buffer -> pos:int -> len:int -> off:Optint.Int63.t -> int =
  fun v fd buf ~pos ~len ~off ->
    let ol = Overlapped.create ~off 0 in
    let id = Overlapped.id ol in
    Mutex.lock v.m;
    H.replace v.in_flight id (buf, ol);
    Mutex.unlock v.m;
    Raw.read v.iocp fd buf len pos ol;
    id

let write : t -> Handle.t -> Cstruct.buffer -> pos:int -> len:int -> off:Optint.Int63.t -> int =
  fun v fd buf ~pos ~len ~off -> 
    let ol = Overlapped.create ~off 0 in
    let id = Overlapped.id ol in
    Mutex.lock v.m;
    H.replace v.in_flight id (buf, ol);
    Mutex.unlock v.m;
    Raw.write v.iocp fd buf len pos ol;
    id

let get_queued_completion_status : t -> timeout:int -> completion_status option =
  fun v ~timeout ->
    match Raw.get_queued_completion_status v.iocp timeout with
    | Raw.Cs_none -> None
    | Raw.Cs_some cs ->
      Mutex.lock v.m;
      let (_buf, _ol) = H.find v.in_flight cs.overlapped_id in
      H.remove v.in_flight cs.overlapped_id;
      Mutex.unlock v.m;
      Some { bytes_transferred = cs.bytes_transferred
          ; id = cs.overlapped_id
          ; error = cs.error }

