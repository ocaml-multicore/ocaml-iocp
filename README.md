# ocaml-iocp -- bindings to Windows IOCP

These are OCaml bindings for Windows' [input/output completion ports](https://docs.microsoft.com/en-us/windows/win32/fileio/i-o-completion-ports).

The API is very similar to that of [ocaml-uring](https://github.com/ocaml-multicore/ocaml-uring) and in fact borrows the `Heap` implementation to pass integer ids to requests instead of the actual key data. 

## Usage

To read a file, you first create a completion port, then open the file using the `Iocp.Handle` module. This ensures the file handle has `FILE_FLAG_OVERLAPPED` set. You can then perform the asynchronous read and wait on the completion port to return the result to you. By default, the completion status will wait indefinitely for a completion packet to arrive.

<!-- TODO: Mdx doesn't really work on cygwin :/ -->
