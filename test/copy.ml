type req = [ `R | `W ]

let usage () =
  Printf.fprintf stderr {|
Usage:
  %s <source filename> <destination filename>
    -- copy <source filename> to <destination filename>
  |} (Filename.basename Sys.argv.(0))

let num_threads = 4

let () =
  if Array.length Sys.argv <> 3 then begin
    Printf.fprintf stderr "Please supply source and destination filenames\n";
    usage();
    exit(-1);
  end;

  let iocp = Iocp.Safest.create num_threads in
  let fd = Iocp.Safest.openfile iocp 0 Sys.argv.(1) [ O_RDONLY ] 0 in
  let out =
    Iocp.Safest.openfile iocp 1 Sys.argv.(2) [ O_WRONLY; O_CREAT; O_TRUNC ] 0o644
  in
  let buf = Cstruct.create 1024 in
  let id : Iocp.Safest.Id.t =
    Iocp.Safest.read iocp fd (Cstruct.to_bigarray buf) ~pos:0 ~off:Optint.Int63.zero ~len:1024
  in
  print_endline "job submitted";
  match Iocp.Safest.get_queued_completion_status iocp ~timeout:1000 with
  | None -> assert false
  | Some t -> (
      assert (t.id = id);
      print_int t.bytes_transferred;
      let id =
        Iocp.Safest.write iocp out (Cstruct.to_bigarray buf) ~pos:0 ~off:Optint.Int63.zero ~len:t.bytes_transferred
      in
      match Iocp.Safest.get_queued_completion_status iocp ~timeout:1000 with
      | None -> assert false
      | Some t ->
          print_endline "All done!";
          assert (t.id = id);
          Unix.close @@ Iocp.Handle.fd fd;
          Unix.close @@ Iocp.Handle.fd out
  )