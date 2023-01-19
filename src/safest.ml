(* Safest *)

module Id = struct
  type t = int
  let compare : t -> t -> int = Int.compare
  let hash : t -> int = Hashtbl.hash
  let equal : t -> t -> bool = Int.equal
end

module H = Hashtbl.Make(Id)

type roots =
  | ReadWrite of Cstruct.buffer
  | RecvSend of Wsabuf.t
  | Connect of Sockaddr.t

type t = {
  iocp : Raw.t;
  m : Mutex.t;
  mutable external_key : int;
  in_flight : (Handle.t * int Overlapped.t * roots * int) H.t;
  mutable unused_overlapped : int Overlapped.t list;
  allocated_overlapped : int Overlapped.t array;
  external_keys : int array;
}

type completion_status = {
  bytes_transferred : int;
  id : Id.t;
  error : Unix.error option;
}

exception Out_of_overlapped

let create ?(overlapped = 1024) n =
  let allocated_overlapped = Array.init overlapped (fun i -> Overlapped.create i) in
  let external_keys = Array.init overlapped (fun _ -> -1) in
  let unused_overlapped = Array.to_list allocated_overlapped in
  Format.eprintf "All created\n%!";
  { iocp = Raw.create_io_completion_port n
  ; m = Mutex.create ()
  ; in_flight = H.create 255
  ; external_key = 1
  ; unused_overlapped
  ; allocated_overlapped
  ; external_keys }

let handle_of_fd v fd key =
  Raw.associate_fd_with_iocp v.iocp fd key

let get_overlapped v fd root =
  Mutex.lock v.m;
  match v.unused_overlapped with
  | [] -> Mutex.unlock v.m; raise Out_of_overlapped
  | ol :: ols ->
    Format.eprintf "Getting overlapped\n%!";
    let id = Overlapped.id ol in
    let external_key = v.external_key in
    let key = Overlapped.get_key ol in
    v.external_key <- external_key + 1;
    v.external_keys.(key) <- external_key;
    v.unused_overlapped <- ols;
    H.replace v.in_flight id (fd, ol, root, external_key);
    Mutex.unlock v.m;
    Format.eprintf "Found an overlapped: id=%d external_key=%d internal_key=%d\n%!" id external_key key;
    ol, external_key

let openfile t = Raw.openfile t.iocp

let read : t -> Handle.t -> Cstruct.buffer -> pos:int -> len:int -> off:Optint.Int63.t -> Id.t =
  fun v fd buf ~pos ~len ~off ->
    let root = ReadWrite buf in
    let ol, external_key = get_overlapped v fd root in
    Overlapped.set_offset ol off;
    Raw.read v.iocp fd buf len pos ol;
    external_key

let write : t -> Handle.t -> Cstruct.buffer -> pos:int -> len:int -> off:Optint.Int63.t -> Id.t =
  fun v fd buf ~pos ~len ~off -> 
    let root = ReadWrite buf in
    let ol, external_key = get_overlapped v fd root in
    Overlapped.set_offset ol off;
    Raw.write v.iocp fd buf len pos ol;
    external_key

let get_queued_completion_status : t -> timeout:int -> completion_status option =
  fun v ~timeout ->
    match Raw.get_queued_completion_status v.iocp timeout with
    | Raw.Cs_none -> None
    | Raw.Cs_some cs ->
      Mutex.lock v.m;
      (* let (_buf, _ol) = H.find v.in_flight cs.overlapped_id in *)
      H.remove v.in_flight cs.overlapped_id;
      let ol_key = Overlapped.unsafe_key cs.overlapped_id in
      let external_key = v.external_keys.(ol_key) in
      v.unused_overlapped <- v.allocated_overlapped.(ol_key) :: v.unused_overlapped;
      Mutex.unlock v.m;
      Some { bytes_transferred = cs.bytes_transferred
          ; id = external_key
          ; error = cs.error }

let accept : t -> Handle.t -> Handle.t -> Accept_buffer.t -> Id.t =
  fun v sock sock_accept addr_buf ->
    let buf = Cstruct.to_bigarray addr_buf in
    let root = ReadWrite buf in
    let ol, external_id = get_overlapped v sock root in
    let () = Raw.accept sock sock_accept buf ol in
    external_id

let recv : t -> Handle.t -> Cstruct.t list -> Id.t =
  fun v sock bufs ->
    let wsabuf = Wsabuf.create bufs in
    let root = RecvSend wsabuf in
    let ol, external_id = get_overlapped v sock root in
    let () = Raw.recv v.iocp sock wsabuf ol in
    external_id

let send : t -> Handle.t -> Cstruct.t list -> Id.t =
  fun v sock bufs ->
    let wsabuf = Wsabuf.create bufs in
    let root = RecvSend wsabuf in
    let ol, external_id = get_overlapped v sock root in
    let () = Raw.send v.iocp sock wsabuf ol in
    external_id
    
let connect : t -> Handle.t -> Sockaddr.t -> Id.t =
  fun v sock addr ->
    let root = Connect addr in
    let ol, external_id = get_overlapped v sock root in
    let () = Raw.connect v.iocp sock addr ol in
    external_id

let garbage = Atomic.make []

let finaliser v =
  Format.eprintf "Finaliser for Safest.t\n%!";
  H.iter (fun _ (fd, ol, _, _) ->
    Format.eprintf "Cancelling somthing\n%!";
    Raw.cancel fd ol) v.in_flight;
  let rec loop () =
    if (H.length v.in_flight = 0) then (Format.eprintf "All done\n%!") else begin
      Format.eprintf "Something to wait for\n%!";
      match get_queued_completion_status v ~timeout:100 with
      | None ->
        Format.eprintf "Error, failed to cancel some outstanding operations (%d)" (H.length v.in_flight);
        let rec add_to_garbage () =
          let cur = Atomic.get garbage in
          if Atomic.compare_and_set garbage cur (v :: cur)
          then ()
          else add_to_garbage ()
        in
        add_to_garbage ()
      | _ -> loop ()
      end
    in loop ()

let create n =
  let v = create n in
  Gc.finalise finaliser v;
  v

