## Tests and Examples

### Copy

The `copy.ml` file contains a very simple program that copies one file to another. This is run as a test to make sure that `./copy.exe test.md test.out` produces an identical file.

### Net

Net starts a TCP socket listening on `127.0.0.1:8080` for an incoming connection. Once connected, it then reads a single bit of data sent over the connection. With `./net.exe` running in one terminal you can use `nc 127.0.0.1 8080` to test it.