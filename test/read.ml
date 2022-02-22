let () =
  let iocp = Iocp.create 10 in
  let fd = Iocp.Handle.openfile Sys.argv.(1) [O_RDONLY] 0 in
  let buf = Cstruct.create 1024 in
  let _job = Iocp.read_file_exn iocp fd buf 1024 (Iocp.Overlapped.create ()) in
  match Iocp.get_queued_completion_status iocp with
    | None -> assert false
    | Some t -> Printf.printf "%s" (Cstruct.to_string ~off:0 ~len:t.bytes_transferred buf)