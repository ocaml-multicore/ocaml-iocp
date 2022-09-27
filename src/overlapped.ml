type 'a t = { id : int } [@@boxed]

external alloc : 'a -> 'a t = "ocaml_iocp_alloc_overlapped"

external free : 'a t -> unit = "ocaml_iocp_free_overlapped"

external set_offset : 'a t -> Optint.Int63.t -> unit
  = "ocaml_iocp_set_overlapped_off"

external set_key : 'a t -> 'a -> unit = "ocaml_iocp_set_overlapped_key"

let create ?(off = Optint.Int63.zero) key =
  let v = alloc key in
  set_offset v off;
  Gc.finalise (fun v -> free v) v;
  v

let id t = t.id

external unsafe_key : int -> 'a = "ocaml_iocp_get_overlapped_key"
