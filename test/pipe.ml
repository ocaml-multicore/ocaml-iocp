let () =
  let iocp = Iocp.create () in
  let rfd, wfd = Iocp.Handle.pipe "iocpPipe" in
  let s = "Hello" in
  let buf = Cstruct.of_string s in
  print_endline ("Sending '" ^ s ^ "' over the pipe");
  let _job = Iocp.write iocp ~file_offset:Optint.Int63.zero wfd buf ~off:0 ~len:5 `W in
  match Iocp.get_queued_completion_status iocp with
    | None -> assert false
    | Some t ->
      assert (t.data = `W);
      let buf' = Cstruct.create t.bytes_transferred in
      let _job = Iocp.read iocp ~file_offset:Optint.Int63.zero rfd buf' ~off:0 ~len:t.bytes_transferred `R in
      match Iocp.get_queued_completion_status iocp with
      | None -> assert false
      | Some t ->
        assert (t.data = `R);
        assert (Cstruct.equal buf buf');
        print_endline ("Received '" ^ Cstruct.to_string buf' ^ "' over the pipe");
        Unix.close (rfd :> Unix.file_descr);
        Unix.close (wfd :> Unix.file_descr)
