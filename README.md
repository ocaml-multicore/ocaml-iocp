# ocaml-iocp -- bindings to Windows IOCP

These are OCaml bindings for Windows' [input/output completion ports](https://docs.microsoft.com/en-us/windows/win32/fileio/i-o-completion-ports).

The API is very similar to that of [ocaml-uring](https://github.com/ocaml-multicore/ocaml-uring) and in fact borrows the `Heap` implementation to pass integer ids to requests instead of the actual key data. 

## Usage

To read a file, you first create a completion port, then open the file using the `Iocp.Handle` module. This ensures the file handle has `FILE_FLAG_OVERLAPPED` set. You can then perform the asynchronous read and wait on the completion port to return the result to you. By default, the completion status will wait indefinitely for a completion packet to arrive.

This program copies a file.

<!-- $MDX file=test/copy.ml -->
```ocaml
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
      let _job = Iocp.write iocp ~file_offset:Optint.Int63.zero out buf ~off:0 ~len:t.bytes_transferred `W in
      match Iocp.get_queued_completion_status iocp with
      | None -> assert false
      | Some t ->
        assert (t.data = `W);
        Unix.close (fd :> Unix.file_descr);
        Unix.close (out :> Unix.file_descr)
```
