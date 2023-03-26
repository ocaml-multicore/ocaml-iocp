module IM = Iocp.Managed

let pipe () =
  let iocp = Iocp.Raw.create_io_completion_port 10 in
  let rfd, wfd = Iocp.Raw.pipe iocp 1 2 "iocpPipe" in
  let s = "Hello" in
  let buf = Cstruct.of_string s in
  let ol = Iocp.Overlapped.create ~off:Optint.Int63.zero () in
  let ol_id = Iocp.Overlapped.id ol in
  let () =
    Iocp.Raw.write iocp wfd (Cstruct.to_bigarray buf) (String.length s) 0 ol
  in
  match Iocp.Raw.get_queued_completion_status iocp 1000 with
  | Cs_none -> assert false
  | Cs_some t -> (
      let ol_id' = t.overlapped_id in
      assert (t.bytes_transferred = String.length s);
      assert (ol_id = ol_id');
      let buf' = Cstruct.create t.bytes_transferred in
      let () =
        Iocp.Raw.read iocp rfd (Cstruct.to_bigarray buf') t.bytes_transferred 0
          ol
      in
      match Iocp.Raw.get_queued_completion_status iocp 1000 with
      | Cs_none -> assert false
      | Cs_some t ->
          let ol_id' = t.overlapped_id in
          assert (ol_id = ol_id');
          assert (Cstruct.equal buf buf');
          Unix.close (Iocp.Handle.fd rfd);
          Unix.close (Iocp.Handle.fd wfd))

let cancel () =
  let iocp = Iocp.Raw.create_io_completion_port 10 in
  let rfd, _wfd = Iocp.Raw.pipe iocp 1 2 "iocpPipeCancel" in
  let buf' = Cstruct.create 100 in
  let ol = Iocp.Overlapped.create ~off:Optint.Int63.zero () in
  let ol_id = Iocp.Overlapped.id ol in
  let () =
    Iocp.Raw.read iocp rfd (Cstruct.to_bigarray buf') 100 0
      ol
  in
  match Iocp.Raw.get_queued_completion_status iocp 100 with
  | Cs_some _ -> assert false
  | Cs_none ->
    Iocp.Raw.cancel rfd ol;
    match Iocp.Raw.get_queued_completion_status iocp 100 with
    | Cs_none -> assert false
    | Cs_some t ->
      assert (t.overlapped_id = ol_id);
      Format.eprintf "got error: %s" (match t.error with | None -> "None" | Some e -> Unix.error_message e);
      ()
  
let write_read () =
  let iocp = Iocp.Raw.create_io_completion_port 10 in
  let filename = "test_file_1.txt" in
  let handle =
    Iocp.Raw.openfile iocp 1 filename Unix.[ O_CREAT; O_RDWR; O_TRUNC ] 0o600
  in
  let ol = Iocp.Overlapped.create ~off:Optint.Int63.zero () in
  let buf = Cstruct.of_string "Test data" in
  let () =
    Iocp.Raw.write iocp handle (Cstruct.to_bigarray buf) (Cstruct.length buf) 0
      ol
  in
  (match Iocp.Raw.get_queued_completion_status iocp 1000 with
  | Cs_none -> assert false
  | _ -> ());
  let buf2 = Cstruct.create (Cstruct.length buf) in
  let ol2 = Iocp.Overlapped.create ~off:Optint.Int63.zero () in
  let () =
    Iocp.Raw.read iocp handle (Cstruct.to_bigarray buf2) (Cstruct.length buf) 0
      ol2
  in
  (match Iocp.Raw.get_queued_completion_status iocp 1000 with
  | Cs_none -> assert false
  | _ -> ());
  assert (Cstruct.equal buf buf2)

(* Check that files that aren't opened with FILE_FLAG_OVERLAPPED cause
   [associate_fd_with_iocp] to fail. *)
let error_non_overlapped () =
  let iocp = Iocp.Raw.create_io_completion_port 10 in
  let fd = Unix.openfile "tmp" Unix.[ O_CREAT; O_RDWR ] 0o600 in
  try
    let _handle = Iocp.Raw.associate_fd_with_iocp iocp fd 0 in
    assert false
  with
  | Unix.Unix_error (Unix.EINVAL, _, _) -> ()
  | _ -> assert false

let check_key () =
  let x = Iocp.Overlapped.create 100 in
  let x_id = Iocp.Overlapped.id x in
  Format.eprintf "x_id = %d\n%!" x_id;
  let ext = Iocp.Overlapped.unsafe_key x_id in
  assert (ext = 100)

let multicore_read max () =
  let n = 4 in
  let m = 4 in
  Format.eprintf "xxxxxxxxxx multicore_read\n%!";
  let mutex = Mutex.create () in
  let concurrency = ref 0 in
  let max_concurrency = ref 0 in
  let with_mutex f =
    Mutex.lock mutex;
    try
      let res = f () in
      Mutex.unlock mutex;
      res
    with e ->
      Mutex.unlock mutex;
      raise e
  in
  let iocp = Iocp.Raw.create_io_completion_port max in
  let rfd, wfd = Iocp.Raw.pipe iocp 1 2 (Format.sprintf "iocpPipe%d" max) in
  let s = "Hello" in
  let write_buf = Cstruct.of_string s in
  let create_writes n =
    List.init n (fun _ ->
        let ol = Iocp.Overlapped.create () in
        (ol, write_buf))
  in
  let dispatch_write (ol, buf) =
    Iocp.Raw.write iocp wfd (Cstruct.to_bigarray buf) (String.length s) 0 ol
  in
  let dispatch_reads n =
    List.init n (fun _ ->
        let read_buf = Cstruct.create (String.length s) in
        let ol = Iocp.Overlapped.create () in
        let () =
          Iocp.Raw.read iocp rfd
            (Cstruct.to_bigarray read_buf)
            (String.length s) 0 ol
        in
        (ol, read_buf))
  in
  let writes, reads = (create_writes n, dispatch_reads n) in
  let rec thread_body () =
    Format.eprintf "Thread body %d\n%!" (Domain.self () :> int);
    match Iocp.Raw.get_queued_completion_status iocp 1000 with
    | Cs_none -> Format.eprintf "xx None! %d\n%!" (Domain.self () :> int)
    | Cs_some t ->
        with_mutex (fun () ->
            incr concurrency;
            Format.eprintf "concurrency=%d\n%!" !concurrency;
            if !concurrency > !max_concurrency then
              max_concurrency := !concurrency);
        if
          List.exists
            (fun (ol, _) -> Iocp.Overlapped.id ol = t.overlapped_id)
            reads
        then (
          Format.eprintf "xx Read %d\n%!" (Domain.self () :> int);
          (* Simulate some work *)
          let rec fib x =
            match x with 0 -> 1 | 1 -> 1 | n -> fib (n - 1) + fib (n - 2)
          in
          let starttime = Unix.gettimeofday () in
          ignore (fib 40);
          let endtime = Unix.gettimeofday () in
          Format.eprintf "Simulated work took %f seconds\n%!"
            (endtime -. starttime);
          with_mutex (fun () -> decr concurrency);
          thread_body ())
        else (
          (* Writes *)
          Format.eprintf "xx Write %d\n%!" (Domain.self () :> int);
          with_mutex (fun () -> decr concurrency);
          thread_body ())
    | exception e ->
        Format.eprintf "xx exn! %d %s\n%!"
          (Domain.self () :> int)
          (Printexc.to_string e)
  in

  let domains = List.init m (fun _ -> Format.eprintf "spawning domain\n%!"; Domain.spawn thread_body) in
  List.iter (fun w -> dispatch_write w) writes;
  List.iter (fun d -> Domain.join d) domains;
  Format.eprintf "max_concurrency=%d max=%d\n" !max_concurrency max
  (* assert (!max_concurrency <= max+1) *)


let multicore_read_unsafe () =
  let n = 4 in
  let m = 4 in
  Format.eprintf "xxxxxxxxxx multicore_read_unsafe\n%!";
  let iocp = Iocp.Raw.create_io_completion_port 4 in
  let rfd, wfd = Iocp.Raw.pipe iocp 1 2 "iocpPipeunsafe" in
  let s = "Hello" in
  let write_buf = Cstruct.of_string s in
  let create_writes n =
    List.init n (fun _ ->
        let ol = Iocp.Overlapped.create () in
        (ol, write_buf))
  in
  let dispatch_write (ol, buf) =
    Iocp.Raw.write iocp wfd (Cstruct.to_bigarray buf) (String.length s) 0 ol
  in
  let dispatch_reads n =
    List.init n (fun _ ->
        let read_buf = Cstruct.create (String.length s) in
        let ol = Iocp.Overlapped.create () in
        let () =
          Iocp.Raw.read iocp rfd
            (Cstruct.to_bigarray read_buf)
            (String.length s) 0 ol
        in
        (ol, read_buf))
  in
  let writes, reads = (create_writes n, dispatch_reads n) in
  let thread_body () =
    Format.eprintf "Thread body %d\n%!" (Domain.self () :> int);
    let t = Iocp.Raw.make_unsafe_completion_status () in
    let rec loop () =
      Format.eprintf "OK1...\n%!";
      let () = Iocp.Raw.get_queued_completion_status_unsafe iocp 1000 t in
      Format.eprintf "OK2...\n%!";
      match (t.success, t.err, t.overlapped_id) with
      | false, 0, _ -> Format.eprintf "ERROR_SUCCESS\n%!"
      | false, 1540, _ -> Format.eprintf "ERROR_TIMEOUT\n%!"
      | false, 258, _ -> Format.eprintf "WAIT_TIMEOUT\n%!"
      | false, i, _ ->
          Format.eprintf "Other error %d\n%!" i;
          failwith "Error"
      | true, _, 0 -> Format.eprintf "Success but no overlapped pointer\n%!"
      | true, _, v ->
          if List.exists (fun (ol, _) -> Iocp.Overlapped.id ol = v) reads then (
            Format.eprintf "xx Read %d\n%!" (Domain.self () :> int);
            (* Simulate some work *)
            let rec fib x =
              match x with 0 -> 1 | 1 -> 1 | n -> fib (n - 1) + fib (n - 2)
            in
            let starttime = Unix.gettimeofday () in
            ignore (fib 40);
            let endtime = Unix.gettimeofday () in
            Format.eprintf "Simulated work took %f seconds\n%!"
              (endtime -. starttime);
            loop ())
          else (
            (* Writes *)
            Format.eprintf "xx Write %d\n%!" (Domain.self () :> int);
            loop ())
      | exception e ->
          Format.eprintf "xx exn! %d %s\n%!"
            (Domain.self () :> int)
            (Printexc.to_string e)
    in
    loop ()
  in

  let domains = List.init m (fun _ -> Domain.spawn thread_body) in
  List.iter (fun w -> dispatch_write w) writes;
  List.iter (fun d -> Domain.join d) domains

(* Check for error from a completed async request *)
let error_out_of_bounds () =
  let filename = "test_file_2.txt" in
  let oc = Unix.openfile filename Unix.[ O_CREAT; O_RDWR; O_TRUNC ] 0o600 in
  let _n = Unix.write oc (Bytes.of_string "Test file\n") 0 10 in
  Unix.close oc;
  let iocp = Iocp.Raw.create_io_completion_port 10 in
  let handle = Iocp.Raw.openfile iocp 1 filename Unix.[ O_RDONLY ] 0 in
  let buf = Cstruct.create 1 in
  let ol = Iocp.Overlapped.create ~off:(Optint.Int63.of_int 1024) () in
  let () = Iocp.Raw.read iocp handle (Cstruct.to_bigarray buf) 1 0 ol in
  match Iocp.Raw.get_queued_completion_status iocp 1000 with
  | Cs_none -> assert false
  | Cs_some t -> ( match t.error with None -> assert false | Some _e -> ())


let safest () =
  let iocp = IM.create 10 in
  let filename = "test_file_3.txt" in
  let handle =
    Iocp.Raw.openfile iocp.iocp 1 filename Unix.[ O_CREAT; O_RDWR; O_TRUNC ] 0o600
  in
  let buf = Cstruct.of_string "Test data" in
  let write_id =
    IM.write iocp handle (Cstruct.to_bigarray buf) ~pos:0 ~len:(Cstruct.length buf) ~off:(Optint.Int63.zero)
  in
  (match IM.get_queued_completion_status iocp ~timeout:1000 with
  | None -> assert false
  | Some cs -> Format.printf "%d %d\n" cs.IM.id write_id;  Alcotest.(check int) "write_id matches" cs.IM.id write_id);

  let buf2 = Cstruct.create (Cstruct.length buf) in
  let read_id =
    IM.read iocp handle (Cstruct.to_bigarray buf2) ~pos:0 ~len:(Cstruct.length buf) ~off:(Optint.Int63.zero)
  in
  (match IM.get_queued_completion_status iocp ~timeout:1000 with
  | None -> assert false
  | Some cs ->
    Format.printf "%d %d\n" cs.IM.id read_id;
    Format.printf "bytes transferred: %d\n" cs.bytes_transferred;
    Format.printf "error = %s\n" (match cs.error with | None -> "None" | Some e -> Unix.error_message e);
    Alcotest.(check int) "read_id matches" cs.IM.id read_id);
  Format.eprintf "buf='%s' buf2='%s'\n%!" (Cstruct.to_string buf) (Cstruct.to_string buf2);
  assert (Cstruct.equal buf buf2)


  let listening_sock iocp =
    let addr = Unix.(ADDR_INET (Unix.inet_addr_loopback, 8889)) in
    let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.bind sock addr;
    Unix.listen sock 0;
    let sock_accept = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.setsockopt sock SO_REUSEADDR true;
    IM.handle_of_fd iocp sock 1, IM.handle_of_fd iocp sock_accept 2
  
  let listen () =
    let iocp = IM.create 5 in
    let sock, sock_accept = listening_sock iocp in
    Format.eprintf "Got an accepting socket\n%!";
    let addr_buf = Iocp.Accept_buffer.create () in
    let id = IM.accept iocp sock sock_accept addr_buf in
    id,iocp

let gc_test () =
  let _ = listen () in
  Gc.full_major ();
  ()

let () =
  let open Alcotest in
  run "IOCP" [
      "singlethread", [
          test_case "pipe"`Quick pipe;
          test_case "write_read" `Quick write_read;
          test_case "check_key" `Quick check_key;
          (* test_case "safest" `Quick safest; *)
          test_case "cancel" `Quick cancel;
          test_case "gc_before_more_tests" `Quick Gc.full_major;
          test_case "gc" `Quick gc_test;
        ];
      "errors", [
        test_case "non_overlapped" `Quick error_non_overlapped;
        test_case "out_of_bounds" `Quick error_out_of_bounds];
      "multicore", [
        test_case "multicore_1" `Quick (multicore_read 1);
        test_case "multicore_2" `Quick (multicore_read 2);
        test_case "multicore_3" `Quick (multicore_read 3);
        test_case "multicore_4" `Quick (multicore_read 4);
        test_case "multicore_unsafe" `Quick multicore_read_unsafe
      ]
    ]
