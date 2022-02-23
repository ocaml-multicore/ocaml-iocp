type req = [ `R | `W ]

let () =
  let iocp = Iocp.create () in
  let fd = Iocp.Handle.openfile Sys.argv.(1) [O_RDONLY] 0 in
  let out = Iocp.Handle.openfile Sys.argv.(2) [O_WRONLY; O_CREAT; O_TRUNC] 0o644 in
  let buf = Cstruct.create 1024 in
  let _job = Iocp.read_file iocp fd buf 1024 `R (Iocp.Overlapped.create ()) in
  match Iocp.get_queued_completion_status iocp with
    | None -> assert false
    | Some t ->
      assert (t.data = `R);
      let _job = Iocp.write_file iocp out buf t.bytes_transferred `W (Iocp.Overlapped.create ()) in
      match Iocp.get_queued_completion_status iocp with
      | None -> assert false
      | Some t ->
        assert (t.data = `W);
        Unix.close (fd :> Unix.file_descr);
        Unix.close (out :> Unix.file_descr)