
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
  let read = Latency.make 1_000_000 in
  let write = Latency.make 1_000_000 in
  Copy_lib.run_cp ~read_latency:read ~write_latency:write 65536 64 Sys.argv.(1) Sys.argv.(2) ();
  let read_fd = Unix.openfile "read-latency.dat" [Unix.O_CREAT; Unix.O_WRONLY; Unix.O_TRUNC] 0o644 in
  let read_oc = Unix.out_channel_of_descr read_fd in
  Printf.fprintf read_oc "# read latency\n";
  Latency.bins read 100 read_oc;
  flush read_oc;
  Unix.close read_fd;
  let write_fd = Unix.openfile "write-latency.dat" [Unix.O_CREAT; Unix.O_WRONLY; Unix.O_TRUNC] 0o644 in
  let write_oc = Unix.out_channel_of_descr write_fd in
  Printf.fprintf write_oc "# write latency\n";
  Latency.bins write 100 write_oc;
  flush write_oc;
  Unix.close write_fd
