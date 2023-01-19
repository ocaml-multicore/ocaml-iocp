type req = [ `R | `W ]

let usage () =
  Printf.fprintf stderr {|
Usage:
  %s <source filename> <destination filename>
    -- copy <source filename> to <destination filename>
  |} (Filename.basename Sys.argv.(0))

let num_threads = 4
let max_in_flight_requests = 16
let buffer_size = 1024

type request = {
  buf: Cstruct.buffer;
  off: Optint.Int63.t;
  req: req;
}

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
  let st_size = Optint.Int63.of_int (Unix.fstat @@ Iocp.Handle.fd fd).Unix.st_size in
  let in_progress_requests = Iocp.Safest.H.create max_in_flight_requests in

  (* Submit initial requests *)
  let rec submit_initial_requests next_read_off num_in_flight =
    if Optint.Int63.compare next_read_off st_size < 0 && num_in_flight < max_in_flight_requests then begin
      let buf = Cstruct.(to_bigarray @@ create buffer_size) in
      Printf.fprintf stderr "initial submit off=%d in_flight_requests = %d\n" (Optint.Int63.to_int next_read_off) num_in_flight;
      let id = Iocp.Safest.read iocp fd buf ~pos:0 ~off:next_read_off ~len:buffer_size in
      Iocp.Safest.H.replace in_progress_requests id {
        buf = buf;
        off = next_read_off;
        req = `R;
      };
      submit_initial_requests Optint.Int63.(add next_read_off (of_int buffer_size)) (num_in_flight + 1)
    end else next_read_off, num_in_flight in
  let next_read_off, num_in_flight = submit_initial_requests Optint.Int63.zero 0 in

  let rec handle_completion next_read_off num_in_flight =
    Printf.fprintf stderr "waiting for %d\n" num_in_flight;
    if num_in_flight > 0 then begin
      match Iocp.Safest.get_queued_completion_status iocp ~timeout:1000 with
      | None -> assert false (* TODO: should we wait forever? *)
      | Some t ->
        let request = Iocp.Safest.H.find in_progress_requests t.id in
        Iocp.Safest.H.remove in_progress_requests t.id;
        begin match request.req with
        | `R ->
          Printf.fprintf stderr "read completed at %d\n" (Optint.Int63.to_int request.off);
          let id = Iocp.Safest.write iocp out request.buf ~pos:0 ~off:request.off ~len:t.bytes_transferred in
          Iocp.Safest.H.replace in_progress_requests id { request with req = `W };
          handle_completion next_read_off num_in_flight
        | `W ->
          Printf.fprintf stderr "write completed at %d\n" (Optint.Int63.to_int request.off);
          if Optint.Int63.compare next_read_off st_size < 0 then begin
            let buf = Cstruct.(to_bigarray @@ create buffer_size) in
            let id = Iocp.Safest.read iocp fd buf ~pos:0 ~off:next_read_off ~len:buffer_size in
            Iocp.Safest.H.replace in_progress_requests id {
              buf = buf;
              off = next_read_off;
              req = `R;
            };
            handle_completion Optint.Int63.(add next_read_off (of_int buffer_size)) num_in_flight
          end else handle_completion next_read_off (num_in_flight - 1)
        end
      end in
  handle_completion next_read_off num_in_flight;

  print_endline "All done!";
  Unix.close @@ Iocp.Handle.fd fd;
  Unix.close @@ Iocp.Handle.fd out
