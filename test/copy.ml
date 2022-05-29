type req = [ `R | `W ]

let () =
  let iocp = Iocp.create () in
  let fd = Iocp.Handle.openfile Sys.argv.(1) [O_RDONLY] 0 in
  let out = Iocp.Handle.openfile Sys.argv.(2) [O_WRONLY; O_CREAT; O_TRUNC] 0o644 in
  let buf = Cstruct.create 1024 in
  let _job = Iocp.read iocp ~file_offset:Optint.Int63.zero fd buf ~off:0 ~len:1024 `R in
  match Iocp.get_queued_completion_status iocp with
    | None -> assert false
    | Some t ->
      assert (t.data = `R);
      print_int t.bytes_transferred;
      let _job = Iocp.write iocp ~file_offset:Optint.Int63.zero out buf ~off:0 ~len:t.bytes_transferred `W in
      match Iocp.get_queued_completion_status iocp with
      | None -> assert false
      | Some t ->
        assert (t.data = `W);
        Unix.close (fd :> Unix.file_descr);
        Unix.close (out :> Unix.file_descr)
