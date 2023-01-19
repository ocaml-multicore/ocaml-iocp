type req = [ `R | `W ]

let usage () =
  Printf.fprintf stderr {|
Usage:
  %s <source filename> <destination filename>
    -- copy <source filename> to <destination filename>
  |} (Filename.basename Sys.argv.(0))

let () =
  if Array.length Sys.argv <> 3 then begin
    Printf.fprintf stderr "Please supply source and destination filenames\n";
    usage();
    exit(-1);
  end;

  let iocp = Iocp.create () in
  let fd = Iocp.openfile iocp 0 Sys.argv.(1) [ O_RDONLY ] 0 in
  let out =
    Iocp.openfile iocp 1 Sys.argv.(2) [ O_WRONLY; O_CREAT; O_TRUNC ] 0o644
  in
  let buf = Cstruct.create 1024 in
  let ol = Iocp.Overlapped.create ~off:Optint.Int63.zero () in
  let (_job : req Iocp.job option) =
    Iocp.read iocp ~file_offset:Optint.Int63.zero fd buf ~off:0 ~len:1024 `R ol
  in
  print_endline "job submitted";
  match Iocp.get_queued_completion_status iocp 1000 with
  | None -> assert false
  | Some t -> (
      assert (t.data = `R);
      print_int t.bytes_transferred;
      let _job =
        Iocp.write iocp ~file_offset:Optint.Int63.zero out buf ~off:0
          ~len:t.bytes_transferred `W ol
      in
      match Iocp.get_queued_completion_status iocp 1000 with
      | None -> assert false
      | Some t ->
          print_endline "All done!";
          assert (t.data = `W);
          Unix.close (fd :> Unix.file_descr);
          Unix.close (out :> Unix.file_descr))
