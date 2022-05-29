// Sockaddr implementation borrow from io_uring bindings

/*
 * Copyright (C) 2020-2021 Anil Madhavapeddy
 * Copyright (C) 2020-2021 Sadiq Jaffer
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

// OCaml APIs
#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/signals.h>
#include <caml/custom.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <caml/socketaddr.h>
#include <caml/osdeps.h>

// Windows APIs
#define WIN32_LEAN_AND_MEAN
#include <Fileapi.h>
#include <Minwinbase.h>
#include <Mswsock.h>
#include <stdio.h>

#define SIZEBUF 4096

struct sock_addr_data {
  union sock_addr_union sock_addr_addr;
  socklen_param_type sock_addr_len;
};

#define Sock_addr_val(v) (*((struct sock_addr_data **) Data_custom_val(v)))

static void finalize_sock_addr(value v) {
  caml_stat_free(Sock_addr_val(v));
  Sock_addr_val(v) = NULL;
}

static struct custom_operations sockaddr_ops = {
  "iocp.sockaddr",
  finalize_sock_addr,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

value
ocaml_iocp_make_sockaddr(value v_sockaddr) {
  CAMLparam1(v_sockaddr);
  CAMLlocal1(v);
  struct sock_addr_data *data;
  v = caml_alloc_custom_mem(&sockaddr_ops, sizeof(struct sock_addr_data *), sizeof(struct sock_addr_data));
  Sock_addr_val(v) = NULL;
  data = (struct sock_addr_data *) caml_stat_alloc(sizeof(struct sock_addr_data));
  Sock_addr_val(v) = data;
  // If this raises, the GC will free [v], which will free [data]:
  get_sockaddr(v_sockaddr, &data->sock_addr_addr, &data->sock_addr_len);
  CAMLreturn(v);
}

value
ocaml_iocp_extract_sockaddr(value v) {
  CAMLparam1(v);
  CAMLlocal1(v_sockaddr);
  struct sock_addr_data *data = Sock_addr_val(v);
  v_sockaddr = alloc_sockaddr(&data->sock_addr_addr, data->sock_addr_len, -1);
  CAMLreturn(v_sockaddr);
}

/*-----------------------------------------------------------------------
   Copyright (c) 2022 Patrick Ferris <patrick@sirref.org>
   Distributed under the MIT license. See terms further down in the file.
  -----------------------------------------------------------------------*/

// Overlapped data structure:
// Contains information for asynchronous (i.e. overlapped) input and output

#define Overlapped_val(v) (*((LPOVERLAPPED*)Data_custom_val(v)))

static void finalize_overlapped(value v) {
    caml_stat_free(Overlapped_val(v));
    Overlapped_val(v) = NULL;
}

static struct custom_operations overlapped_ops = {
    "iocp.overlapped",
    finalize_overlapped,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default
};

value ocaml_iocp_make_overlapped(value v_off) {
    CAMLparam0();
    CAMLlocal1(v);
    LPOVERLAPPED ol;

    // Allocate an OCaml value on the heap to store a pointer to the pointer to the overlapped struct.
    v = caml_alloc_custom_mem(&overlapped_ops, sizeof(LPOVERLAPPED), sizeof(OVERLAPPED));

    // Allocate (in the C heap) the overlapped struct.
    ol = (LPOVERLAPPED) caml_stat_alloc(sizeof(OVERLAPPED));
    Overlapped_val(v) = ol;
    memset(ol, 0, sizeof(OVERLAPPED));
    ol->Offset = Int_val(v_off);
    CAMLreturn(v);
}

// The key cannot be GC'd until the event is complete
value ocaml_iocp_create_io_completion_port(value v_threads) {
    CAMLparam0();
    CAMLlocal1(v);
    int num_threads = Int_val(v_threads);

    HANDLE cp = CreateIoCompletionPort(INVALID_HANDLE_VALUE, NULL, 0, num_threads);

    v = win_alloc_handle(cp);

    Handle_val(v) = cp;
    CAMLreturn(v);
}

// The key cannot be GC'd until the event is complete
value ocaml_iocp_create_io_completion_port_with_fd(value v_fd, value v_key, value v_threads) {
    CAMLparam1(v_fd);
    CAMLlocal1(v);
    int num_threads = Int_val(v_threads);

    HANDLE handle = Handle_val(v_fd);
    HANDLE cp = CreateIoCompletionPort(handle, NULL, v_key, num_threads);

    v = win_alloc_handle(cp);

    Handle_val(v) = cp;
    CAMLreturn(v);
}

static value Val_completion_status(value v_id, value v_bytes, value v_ol) {
    CAMLparam3(v_id, v_bytes, v_ol);
    CAMLlocal1(v);
    v = caml_alloc(2, 0);
    Store_field(v, 0, v_id);
    Store_field(v, 1, v_bytes);
    // Store_field(v, 2, v_ol);
    CAMLreturn(v);
}

value ocaml_iocp_get_queued_completion_status(value v_fd) {
    CAMLparam1(v_fd);
    CAMLlocal2(v, v_ol);
    BOOL b;
    HANDLE fd = Handle_val(v_fd);
    LPOVERLAPPED ol;
    DWORD transferred = 0;
    DWORD_PTR ptr = 0 ;

    ol = (LPOVERLAPPED) caml_stat_alloc(sizeof(OVERLAPPED));
    v_ol = caml_alloc_custom_mem(&overlapped_ops, sizeof(LPOVERLAPPED), sizeof(OVERLAPPED));
    caml_enter_blocking_section();
    b = GetQueuedCompletionStatus(fd, &transferred, &ptr, &ol, INFINITE);
    caml_leave_blocking_section();
    if (!b) {
        printf("ERRR %i\n", GetLastError());
    }
    Overlapped_val(v_ol) = ol;
    v = Val_completion_status(Val_int(ptr), Val_int(transferred), v_ol);
    CAMLreturn(v);
}

value ocaml_iocp_read(value v_cp, value v_fd, value v_id, value v_ba, value v_num_bytes, value v_off, value v_overlapped) {
    CAMLparam4(v_cp, v_fd, v_ba, v_overlapped);
    LPOVERLAPPED ol = Overlapped_val(v_overlapped);

    // Here we associate the file handle to the completion port handle...
    void *buf = Caml_ba_data_val(v_ba) + Long_val(v_off);
    HANDLE _cp = CreateIoCompletionPort(Handle_val(v_fd), Handle_val(v_cp), Long_val(v_id), 0);
    BOOL b = ReadFile(Handle_val(v_fd), buf, Int_val(v_num_bytes), NULL, ol);

    // The return value is non-zero (TRUE) on success. However, it is FALSE if the IO operation
    // is completing asynchronously. We change that behaviour by checking last error.
    if (GetLastError() == ERROR_IO_PENDING) {
        CAMLreturn(Val_true);
    }
    CAMLreturn(Val_bool(b));
}

value ocaml_iocp_read_bytes(value* values, int argc) {
    return ocaml_iocp_read(values[0], values[1], values[2], values[3], values[4], values[5], values[6]);
}

value ocaml_iocp_write(value v_cp, value v_fd, value v_id, value v_ba, value v_num_bytes, value v_off, value v_overlapped) {
    CAMLparam4(v_cp, v_fd, v_ba, v_overlapped);
    LPOVERLAPPED ol = Overlapped_val(v_overlapped);

    // Here we associate the file handle to the completion port handle...
    void *buf = Caml_ba_data_val(v_ba) + Long_val(v_off);
    HANDLE _cp = CreateIoCompletionPort(Handle_val(v_fd), Handle_val(v_cp), Long_val(v_id), 0);
    BOOL b = WriteFile(Handle_val(v_fd), buf, Int_val(v_num_bytes), NULL, ol);

    // The return value is non-zero (TRUE) on success. However, it is FALSE if the IO operation
    // is completing asynchronously. We change that behaviour by checking last error.
    if (GetLastError() == ERROR_IO_PENDING) {
        CAMLreturn(Val_true);
    }
    CAMLreturn(Val_bool(b));
}

value ocaml_iocp_write_bytes(value* values, int argc) {
    return ocaml_iocp_write(values[0], values[1], values[2], values[3], values[4], values[5], values[6]);
}

GUID GuidGetAddrAcceptEx = WSAID_GETACCEPTEXSOCKADDRS;

value ocaml_iocp_get_accept_ex_sockaddr(value v_accept_buffer, value v_listen, value v_sockaddr) {
    CAMLparam3(v_listen, v_accept_buffer, v_sockaddr);
    LPFN_GETACCEPTEXSOCKADDRS lpfnGetAcceptExSockaddrs = NULL;
    DWORD dwBytes;
    int result;

    result = WSAIoctl(
        Socket_val(v_listen), 
        SIO_GET_EXTENSION_FUNCTION_POINTER,
        &GuidGetAddrAcceptEx,
        sizeof(GuidGetAddrAcceptEx),
        &lpfnGetAcceptExSockaddrs,
        sizeof(lpfnGetAcceptExSockaddrs),
        &dwBytes, NULL, NULL);

    if (result == SOCKET_ERROR) {
        printf("Socket address error\n");
        CAMLreturn(Val_false);
    }
    
    int lsize = 0;
    SOCKADDR *pLocal = NULL, *pRemote = NULL;
    
    struct sock_addr_data *data = Sock_addr_val(v_sockaddr);
    // struct sockaddr_in *remote = &(data->sock_addr_addr.s_gen);

    lpfnGetAcceptExSockaddrs(
        Caml_ba_data_val(v_accept_buffer),
        0,
        sizeof(union sock_addr_union) + 16,
        sizeof(union sock_addr_union) + 16,
        &pLocal,
        &lsize,
        &pRemote,
        &(data->sock_addr_len));

    // TODO: Is there a non-copying way to get this to work?
    memcpy(&(data->sock_addr_addr), pRemote, data->sock_addr_len);

    CAMLreturn(Val_unit);
}

// We have to retrieve a pointer to AcceptEx at runtime
GUID GuidAcceptEx = WSAID_ACCEPTEX;

value ocaml_iocp_accept(value v_cp, value v_listen, value v_accept, value v_id, value v_accept_buffer, value v_overlapped) {
    CAMLparam5(v_cp, v_listen, v_accept, v_accept_buffer, v_overlapped);
    LPOVERLAPPED ol = Overlapped_val(v_overlapped);
    DWORD received;
    int result;

    LPFN_ACCEPTEX lpfnAcceptEx = NULL;
    DWORD dwBytes;

    result = WSAIoctl(
        Socket_val(v_listen), 
        SIO_GET_EXTENSION_FUNCTION_POINTER,
        &GuidAcceptEx,
        sizeof(GuidAcceptEx),
        &lpfnAcceptEx,
        sizeof(lpfnAcceptEx),
        &dwBytes, NULL, NULL);

    if (result == SOCKET_ERROR) {
        printf("Socket address error!\n");
        CAMLreturn(Val_false);
    }

    // Here we associate the socket value to the completion port
    HANDLE cp1 = CreateIoCompletionPort((HANDLE) Socket_val(v_listen), Handle_val(v_cp), Long_val(v_id), 0);
    HANDLE cp2 = CreateIoCompletionPort((HANDLE) Socket_val(v_accept), Handle_val(v_cp), Long_val(v_id), 0);

    if (cp1 == NULL || cp2 == NULL) {
        printf("Error associating completion port to accept socket\n");
        CAMLreturn(Val_false);
    }

    DWORD *buf = Caml_ba_data_val(v_accept_buffer);

    // By default, AcceptEx wants to wait for the connection along with first bit of data. The buffer it
    // accepts as an argument wants data for the first incoming message, the local address and the remote
    // address. We're only interested in the remote address.
    BOOL b = lpfnAcceptEx(
        Socket_val(v_listen),               // The listening socket
        Socket_val(v_accept),               // A socket on which to accept an incoming connection (not bound or connected) ?
        Caml_ba_data_val(v_accept_buffer),  // An output buffer that receives the first block of data (and sockaddr)
        0,                                  // Receive data length, the number of bytes in the buffer         
        sizeof(union sock_addr_union) + 16, // Number of bytes reserved for local address information
        sizeof(union sock_addr_union) + 16, // Number of bytes reserved for remote address information
        &received,                          // Pointer to DWORD for number of bytes received
        ol);                                // The OVERLAPPED structure

    // The return value is non-zero (TRUE) on success. However, it is FALSE if the IO operation
    // is completing asynchronously. We change that behaviour by checking last error.
    if (GetLastError() == ERROR_IO_PENDING) {
        CAMLreturn(Val_true);
    }

    printf("ERROR %i %i", b, GetLastError());
    CAMLreturn(Val_bool(b));
}

value ocaml_iocp_accept_bytes(value* values, int argc) {
    return ocaml_iocp_accept(values[0], values[1], values[2], values[3], values[4], values[5]);
}

/*---------------------------------------------------------------------------
   Copyright (c) 2022 <patrick@sirref.org>
   
   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.
   
   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
   THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
   DEALINGS IN THE SOFTWARE.
  ---------------------------------------------------------------------------*/

// We need an openfile that passes FILE_FLAGS_OVERLAPPED -- the quickest way
// was for me to copy the openfile from win32 in ocaml/ocaml... TODO: something
// better ?

/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*   Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt     */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#include <windows.h>
#include <fcntl.h>

static int open_access_flags[15] = {
  GENERIC_READ, GENERIC_WRITE, GENERIC_READ|GENERIC_WRITE,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
};

static int open_create_flags[15] = {
  0, 0, 0, 0, 0, O_CREAT, O_TRUNC, O_EXCL, 0, 0, 0, 0, 0, 0, 0
};

static int open_share_flags[15] = {
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, FILE_SHARE_DELETE, 0, 0
};

enum { CLOEXEC = 1, KEEPEXEC = 2 };

static int open_cloexec_flags[15] = {
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, CLOEXEC, KEEPEXEC
};

CAMLprim value ocaml_iocp_unix_pipe(value v_path)
{
  CAMLparam1(v_path);
  CAMLlocal2(readfd, writefd);
  value res;
  char *wpath = caml_strdup(String_val(v_path));

  SECURITY_ATTRIBUTES attr;
  attr.nLength = sizeof(attr);
  attr.lpSecurityDescriptor = NULL;
  attr.bInheritHandle = TRUE;

  HANDLE pipeR = CreateNamedPipe(
        TEXT(wpath), // name of the pipe
        (PIPE_ACCESS_DUPLEX | FILE_FLAG_OVERLAPPED),
        (PIPE_TYPE_BYTE | PIPE_READMODE_BYTE | PIPE_WAIT),
        1, // only allow 1 instance of this pipe
        SIZEBUF,
        SIZEBUF,
        PIPE_WAIT, // use default wait time
        &attr // use default security attributes
    );

  if (pipeR == INVALID_HANDLE_VALUE || pipeR == NULL) {
    win32_maperr(GetLastError());
    uerror("CreateNamedPipe", "pipe");
  }

  HANDLE pipeW = CreateFile(
        TEXT(wpath),
        GENERIC_READ | GENERIC_WRITE,
        FILE_SHARE_READ | FILE_SHARE_WRITE,
        NULL,
        OPEN_EXISTING,
        FILE_ATTRIBUTE_NORMAL | FILE_FLAG_OVERLAPPED,
        &attr);

  if (pipeW == INVALID_HANDLE_VALUE || pipeW == NULL) {
    printf("Write handle failed\n");
    win32_maperr(GetLastError());
    uerror("CreateNamedPipe", "pipe");
  }

  writefd = win_alloc_handle(pipeW);
  readfd = win_alloc_handle(pipeR);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = readfd;
  Field(res, 1) = writefd;
  CAMLreturn(res);
}

CAMLprim value ocaml_iocp_unix_open(value path, value flags, value perm)
{
  int fileaccess, createflags, fileattrib, filecreate, sharemode, cloexec;
  SECURITY_ATTRIBUTES attr;
  HANDLE h;
  char *wpath;

  caml_unix_check_path(path, "open");
  fileaccess = caml_convert_flag_list(flags, open_access_flags);
  sharemode = FILE_SHARE_READ | FILE_SHARE_WRITE
              | caml_convert_flag_list(flags, open_share_flags);

  createflags = caml_convert_flag_list(flags, open_create_flags);
  if ((createflags & (O_CREAT | O_EXCL)) == (O_CREAT | O_EXCL))
    filecreate = CREATE_NEW;
  else if ((createflags & (O_CREAT | O_TRUNC)) == (O_CREAT | O_TRUNC))
    filecreate = CREATE_ALWAYS;
  else if (createflags & O_TRUNC)
    filecreate = TRUNCATE_EXISTING;
  else if (createflags & O_CREAT)
    filecreate = OPEN_ALWAYS;
  else
    filecreate = OPEN_EXISTING;

  if ((createflags & O_CREAT) && (Int_val(perm) & 0200) == 0)
    fileattrib = FILE_ATTRIBUTE_READONLY;
  else
    fileattrib = FILE_ATTRIBUTE_NORMAL;

  cloexec = caml_convert_flag_list(flags, open_cloexec_flags);
  attr.nLength = sizeof(attr);
  attr.lpSecurityDescriptor = NULL;
  attr.bInheritHandle =
    cloexec & CLOEXEC ? FALSE
                      : cloexec & KEEPEXEC ? TRUE
                                           : !unix_cloexec_default;

  wpath = caml_strdup(String_val(path));
  h = CreateFile(wpath, fileaccess,
                 sharemode, &attr,
                 filecreate, fileattrib | FILE_FLAG_OVERLAPPED, NULL);
  caml_stat_free(wpath);
  if (h == INVALID_HANDLE_VALUE) {
    win32_maperr(GetLastError());
    uerror("open", path);
  }
  return win_alloc_handle(h);
}