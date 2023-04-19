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
#define CAML_NAME_SPACE
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
#include <caml/fail.h>


// Windows APIs
#define WIN32_LEAN_AND_MEAN
#include <Fileapi.h>
#include <Minwinbase.h>
#include <Mswsock.h>
#include <stdio.h>
#include <assert.h>


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

typedef struct extended_overlapped {
  OVERLAPPED o;
  value key;
} eo;

static value val_of_overlapped_ptr(eo *ptr)
{
  assert(((uintptr_t)ptr & 1)==0);
  return (value) ptr | 1;
}

static LPOVERLAPPED overlapped_ptr_of_val(value v)
{
  assert(v != 0);
  assert(v & 1 == 1);
  return (LPOVERLAPPED) (v & ~1);
}

void ocaml_iocp_free_overlapped(value v) {
    CAMLparam1(v);
    eo *lp=(eo *)overlapped_ptr_of_val(Field(v,0));
    caml_stat_free(lp);
    CAMLreturn0;
}

CAMLprim value ocaml_iocp_alloc_overlapped(value key) {
    CAMLparam1(key);

    if(Is_block(key)) {
      caml_invalid_argument("Overlapped key must be an immediate");
    }

    eo *ol = (eo *) caml_stat_alloc(sizeof(eo));
    memset(ol, 0, sizeof(eo));

    ol->key = key;

    /* Box this so the OCaml side can set a finalizer */
    CAMLreturn(caml_alloc_boxed(val_of_overlapped_ptr(ol)));
}

void ocaml_iocp_set_overlapped_off(value v, value off) {
  CAMLparam2(v, off);
  LPOVERLAPPED ol=overlapped_ptr_of_val(Field(v,0));
  memset(ol, 0, sizeof(OVERLAPPED));
  ol->Offset = Int_val(off);
  ol->OffsetHigh = 0;
  CAMLreturn0;
}

value ocaml_iocp_get_overlapped_key(value v) {
  CAMLparam1(v);
  eo *ol = (eo *)overlapped_ptr_of_val(v);
  CAMLreturn(ol->key);
}

void ocaml_iocp_set_overlapped_key(value v, value key) {
  CAMLparam2(v, key);
  eo *ol = (eo *)overlapped_ptr_of_val(v);
  if(Is_block(key)) {
      caml_invalid_argument("Overlapped key must be an immediate");
  }
  ol->key = key;
  CAMLreturn0;
}

// The key cannot be GC'd until the event is complete
value ocaml_iocp_create_io_completion_port(value v_threads) {
    CAMLparam0();
    CAMLlocal1(v);
    int num_threads = Int_val(v_threads);

    HANDLE cp = CreateIoCompletionPort(INVALID_HANDLE_VALUE, NULL, 0 /* ignored */, num_threads);

    if (cp == NULL) {
      win32_maperr(GetLastError());
      // printf("RETURNING FAILED %i\n", GetLastError());
      uerror("CreateIoCompletionPort", Nothing);
    }

    v = win_alloc_handle(cp);

    Handle_val(v) = cp;
    CAMLreturn(v);
}

value ocaml_iocp_associate_fd_with_iocp(value v_iocp, value v_fd, value v_key) {
    CAMLparam3(v_iocp, v_fd, v_key);

    HANDLE handle = Handle_val(v_fd);
    HANDLE iocp = Handle_val(v_iocp);

    HANDLE cp = CreateIoCompletionPort(handle, iocp, v_key, 0);

    if (cp == NULL) {
      win32_maperr(GetLastError());
      // printf("RETURNING FAILED %i\n", GetLastError());
      uerror("CreateIoCompletionPort", Nothing);
    }

    CAMLreturn(v_fd);

}
// The key cannot be GC'd until the event is complete
value ocaml_iocp_create_io_completion_port_with_fd(value v_fd, value v_threads, value v_key) {
    CAMLparam3(v_fd, v_threads, v_key);
    CAMLlocal1(v);
    int num_threads = Int_val(v_threads);

    HANDLE handle = Handle_val(v_fd);
    HANDLE cp = CreateIoCompletionPort(handle, NULL, v_key, num_threads);

    if (cp == NULL) {
      win32_maperr(GetLastError());
      // printf("RETURNING FAILED %i\n", GetLastError());
      uerror("CreateIoCompletionPort", Nothing);
    }

    v = win_alloc_handle(cp);

    Handle_val(v) = cp;
    CAMLreturn(v);
}

#define Val_cqe_none Val_int(0)

static value Val_completion_status(value v_id, value v_bytes, value v_overlapped) {
    CAMLparam2(v_id, v_bytes);
    CAMLlocal1(v);
    v = caml_alloc(3, 0);
    Store_field(v, 0, v_id);
    Store_field(v, 1, v_bytes);
    Store_field(v, 2, v_overlapped);
    CAMLreturn(v);
}

value ocaml_iocp_get_queued_completion_status(value v_fd, value v_timeout) {
    CAMLparam2(v_fd, v_timeout);
    CAMLlocal2(v,v_err);
    BOOL b = 0;
    HANDLE fd = Handle_val(v_fd);
    DWORD transferred = 0;
    DWORD_PTR ptr;
    DWORD err;
    v_err = Val_int(0); /* None */
    LPOVERLAPPED ol = NULL;

    caml_enter_blocking_section();
    b = GetQueuedCompletionStatus(fd, &transferred, &ptr, &ol, Int_val(v_timeout));
    caml_leave_blocking_section();

    if (b && ol == NULL) {
      printf("Unknown status: returned 'true', but ol is null");
      CAMLreturn(Val_int(0)); /* success, nothing to return (hit timeout) */
    }

    /* Indicates an error with the call to GetQueuedCompletionStatus */
    if (!b && ol == NULL) {
      /* A timeout is an OK result, don't raise an exception */
      err=GetLastError();

      /* A timeout is an OK result, don't raise an exception.
         I have also seen ERROR_SUCCESS, so let's not raise
         an error for this! */
      if(err==WAIT_TIMEOUT || err==ERROR_SUCCESS) {
        CAMLreturn(Val_int(0));
      }

      /* For all other errors, raise a Unix_error */
      win32_maperr(err);
      // printf("RETURNING FAILED %i\n", err);
      uerror("QueuedCompletionStatus", Nothing);
    }

    /* Indicates an error with the IO operation represented by ol */
    if(!b && ol != NULL) {
      /* Set the global var errno */
      win32_maperr(GetLastError ());
      v_err = caml_alloc(1, 0);
      Store_field(v_err, 0, unix_error_of_code(errno));
    }

    v = caml_alloc(4, 0);
    Store_field(v, 0, Val_int(ptr));
    Store_field(v, 1, Val_int(transferred));
    Store_field(v, 2, val_of_overlapped_ptr((eo *)ol));
    Store_field(v, 3, v_err);
    CAMLreturn(v);
}

void ocaml_iocp_get_queued_completion_status_unsafe(value v_fd, value v_timeout, value v) {
    CAMLparam3(v_fd, v_timeout, v);
    BOOL b = 0;
    HANDLE fd = Handle_val(v_fd);
    DWORD transferred = 0;
    DWORD_PTR ptr;
    DWORD err=0;
    LPOVERLAPPED ol = NULL;

    // printf("ocaml_iocp_get_queued_completion_status_unsafe: here we go!\n");

    caml_enter_blocking_section();
    b = GetQueuedCompletionStatus(fd, &transferred, &ptr, &ol, Int_val(v_timeout));
    caml_leave_blocking_section();

    /* Indicates an error with the call to GetQueuedCompletionStatus */
    if (!b) {
      /* A timeout is an OK result, don't raise an exception */
      err=GetLastError();
    }

    Store_field(v, 0, Val_int(ptr));
    Store_field(v, 1, Val_int(transferred));
    Store_field(v, 2, val_of_overlapped_ptr((eo *)ol));
    Store_field(v, 3, Val_bool(b));
    Store_field(v, 4, Val_int(err));

    // printf("OK, ol=%p res=%d err=%d\n",(void *)ol,b,err);
    CAMLreturn0;
}

value ocaml_iocp_peek(value v_fd) {
    CAMLparam1(v_fd);
    CAMLlocal1(v);
    BOOL b = 0;
    HANDLE fd = Handle_val(v_fd);
    DWORD transferred = 0;
    DWORD_PTR ptr = 0 ;
    LPOVERLAPPED ol = NULL;

    caml_enter_blocking_section();
    b = GetQueuedCompletionStatus(fd, &transferred, &ptr, &ol, 0);
    caml_leave_blocking_section();

    if (!b && GetLastError() != ERROR_HANDLE_EOF) {
      // win32_maperr(GetLastError());
      // uerror("CreateNamedPipe", Nothing);
      //CAMLreturn(Val_cqe_none);
    }
    v = caml_alloc(3, 0);
    Store_field(v, 0, Val_int(ptr));
    Store_field(v, 1, Val_int(transferred));
    Store_field(v, 2, val_of_overlapped_ptr((eo *)ol));
    CAMLreturn(v);
}

void ocaml_iocp_read(value v_cp, value v_fd, value v_ba, value v_num_bytes, value v_off, value v_overlapped) {
    CAMLparam4(v_cp, v_fd, v_ba, v_overlapped);
    LPOVERLAPPED ol = overlapped_ptr_of_val(Field(v_overlapped,0));

    // printf("READ FILE %i %i %i %i\n", Handle_val(v_fd), Int_val(v_off), Int_val(v_num_bytes), ol->Offset);

    // Here we associate the file handle to the completion port handle...
    void *buf = Caml_ba_data_val(v_ba) + Long_val(v_off);
    BOOL b = ReadFile(Handle_val(v_fd), buf, Int_val(v_num_bytes), NULL, ol);
    // The return value is non-zero (TRUE) on success. However, it is FALSE if the IO operation
    // is completing asynchronously. We change that behaviour by checking last error.
    if (!b) {
      DWORD err = GetLastError();
      if(err == ERROR_IO_PENDING) {
        // printf("Returned pending\n");
        CAMLreturn0;
      }
      win32_maperr(err);
      uerror("ReadFile", Nothing);
    }
    CAMLreturn0;
}

void ocaml_iocp_read_bytes(value* values, int argc) {
    return ocaml_iocp_read(values[0], values[1], values[2], values[3], values[4], values[5]);
}

void ocaml_iocp_write(value v_cp, value v_fd, value v_ba, value v_num_bytes, value v_off, value v_overlapped) {
    CAMLparam5(v_cp, v_fd, v_ba, v_num_bytes, v_off);
    CAMLxparam1(v_overlapped);
    LPOVERLAPPED ol = overlapped_ptr_of_val(Field(v_overlapped,0));

    // printf("iocp_write - overlapped=%p\n\n",(void*) ol);
    // Here we associate the file handle to the completion port handle...
    void *buf = Caml_ba_data_val(v_ba) + Long_val(v_off);
    BOOL b = WriteFile(Handle_val(v_fd), buf, Int_val(v_num_bytes), NULL, ol);

    // The return value is non-zero (TRUE) on success. However, it is FALSE if the IO operation
    // is completing asynchronously. We change that behaviour by checking last error.
    if (!b) {
      DWORD err = GetLastError();
      if(err == ERROR_IO_PENDING) {
        // printf("Returned pending\n");
        CAMLreturn0;
      }
      win32_maperr(err);
      uerror("WriteFile", Nothing);
    }
    CAMLreturn0;
}

void ocaml_iocp_write_bytes(value* values, int argc) {
    return ocaml_iocp_write(values[0], values[1], values[2], values[3], values[4], values[5]);
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

void ocaml_iocp_accept(value v_listen, value v_accept, value v_accept_buffer, value v_overlapped) {
    CAMLparam4(v_listen, v_accept, v_accept_buffer, v_overlapped);
    LPOVERLAPPED ol = overlapped_ptr_of_val(Field(v_overlapped,0));
    DWORD received;
    int result;

    printf("got overlapped: %p\n", ol);

    LPFN_ACCEPTEX lpfnAcceptEx = NULL;
    DWORD dwBytes;

        SOCKET sock;
            sock = socket(AF_INET, SOCK_STREAM, 0);
    printf("about to call WSAIoctl\n");
    result = WSAIoctl(
        sock, 
        SIO_GET_EXTENSION_FUNCTION_POINTER,
        &GuidAcceptEx,
        sizeof(GuidAcceptEx),
        &lpfnAcceptEx,
        sizeof(lpfnAcceptEx),
        &dwBytes, NULL, NULL);

    printf("Called WSAIoctl\n");
    fflush(stdout);

    if (result == SOCKET_ERROR) {
      DWORD err = WSAGetLastError();
      win32_maperr(err);
      uerror("WSAIoctl", Nothing);
    }
    printf("Called WSAIoctl, no error\n");

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

    printf("Called AcceptEx\n");
    fflush(stdout);
    // The return value is non-zero (TRUE) on success. However, it is FALSE if the IO operation
    // is completing asynchronously. We change that behaviour by checking last error.
    if (!b) {
      DWORD err = WSAGetLastError();
      if(err == ERROR_IO_PENDING) {
        // printf("Returned pending\n");
        CAMLreturn0;
      }
      win32_maperr(err);
      uerror("AcceptEx", Nothing);
    }
    CAMLreturn0;
}

// We have to retrieve a pointer to AcceptEx at runtime
GUID GuidConnectEx = WSAID_CONNECTEX;

void ocaml_iocp_connect(value v_cp, value v_sock, value v_addr, value v_overlapped) {
    CAMLparam4(v_cp, v_sock, v_addr, v_overlapped);
    LPOVERLAPPED ol = overlapped_ptr_of_val(Field(v_overlapped,0));
    DWORD received;
    int result;

    LPFN_CONNECTEX lpfnConnectEx = NULL;
    DWORD dwBytes;

    result = WSAIoctl(
        Socket_val(v_sock), 
        SIO_GET_EXTENSION_FUNCTION_POINTER,
        &GuidConnectEx,
        sizeof(GuidConnectEx),
        &lpfnConnectEx,
        sizeof(lpfnConnectEx),
        &dwBytes, NULL, NULL);

    if (result == SOCKET_ERROR) {
        DWORD err = WSAGetLastError();
        win32_maperr(err);
        uerror("WSAIoctl", Nothing);
    }

    struct sock_addr_data *data = Sock_addr_val(v_addr);

    BOOL b = lpfnConnectEx(
        Socket_val(v_sock),                 // The socket
        &(data->sock_addr_addr.s_gen),      // A socket address
        data->sock_addr_len,                // An output buffer that receives the first block of data (and sockaddr)
        NULL,                               // Send buffer 
        0,                                  // Number of bytes reserved for local address information
        &received,                          // Pointer to DWORD for number of bytes received
        ol);                                // The OVERLAPPED structure

    // The return value is non-zero (TRUE) on success. However, it is FALSE if the IO operation
    // is completing asynchronously. We change that behaviour by checking last error.
    if (!b) {
      DWORD err = GetLastError();
      if(err == ERROR_IO_PENDING) {
        // printf("Returned pending\n");
        CAMLreturn0;
      }
      win32_maperr(err);
      uerror("ConnectEx", Nothing);
    }
    CAMLreturn0;
}

// WSABUF

#define Wsabuf_val(v) (*((WSABUF **) Data_custom_val(v)))

static void finalize_wsabuf(value v) {
  caml_stat_free(Wsabuf_val(v));
  Wsabuf_val(v) = NULL;
}

static struct custom_operations wsabuf_ops = {
  "iocp.wsabuf_ops",
  finalize_wsabuf,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

value
ocaml_iocp_make_wsabuf(value v_cstructs, value v_len) {
  CAMLparam1(v_cstructs);
  CAMLlocal2(v, l);
  int len = Int_val(v_len);
  int i;
  WSABUF *bufs;
  // Allocate the custom block on the OCaml heap:
  v = caml_alloc_custom_mem(&wsabuf_ops, sizeof(struct wsabuf_ops *), len * sizeof(WSABUF));
  Wsabuf_val(v) = NULL;
  bufs = caml_stat_alloc(len * sizeof(WSABUF));
  Wsabuf_val(v) = bufs;
  for (i = 0, l = v_cstructs; i < len; l = Field(l, 1), i++) {
    value v_cs = Field(l, 0);
    value v_ba = Field(v_cs, 0);
    value v_off = Field(v_cs, 1);
    value v_len = Field(v_cs, 2);
    bufs[i].buf = Caml_ba_data_val(v_ba) + Long_val(v_off);
    bufs[i].len = Long_val(v_len);
    // dprintf("adding wsabuf %d: %p (%ld, %ld)\n", i, bufs[i].buf, Long_val(v_off), Long_val(v_len));
  }
  CAMLreturn(v);
}

void
ocaml_iocp_send(value v_cp, value v_sock, value v_wsabuf, value v_overlapped) {
  CAMLparam4(v_cp, v_sock, v_wsabuf, v_overlapped);
  DWORD received;
  LPOVERLAPPED ol = overlapped_ptr_of_val(Field(v_overlapped,0));

  WSABUF *wsabuf = Wsabuf_val(Field(v_wsabuf, 0));
  int len = Int_val(Field(v_wsabuf, 1));

  int i = WSASend(
    Socket_val(v_sock),
    wsabuf,
    len,
    &received,
    0,
    ol,
    NULL
  );

  if (i==SOCKET_ERROR) {
      DWORD err = WSAGetLastError();
      if(err == ERROR_IO_PENDING) {
        // printf("Returned pending\n");
        CAMLreturn0;
      }
      win32_maperr(err);
      uerror("WSASend", Nothing);
    }
    CAMLreturn0;
}

void
ocaml_iocp_recv(value v_cp, value v_sock, value v_wsabuf, value v_overlapped) {
  CAMLparam4(v_cp, v_sock, v_wsabuf, v_overlapped);
  DWORD received, flags;
  LPOVERLAPPED ol = overlapped_ptr_of_val(Field(v_overlapped,0));

  WSABUF *wsabuf = Wsabuf_val(Field(v_wsabuf, 0));
  int len = Int_val(Field(v_wsabuf, 1));


  flags = 0;

  int i = WSARecv(
    Socket_val(v_sock),
    wsabuf,
    len,
    &received,
    &flags,
    ol,
    NULL
  );

  if (i==SOCKET_ERROR) {
      DWORD err = WSAGetLastError();
      if(err == ERROR_IO_PENDING) {
        // printf("Returned pending\n");
        CAMLreturn0;
      }
      win32_maperr(err);
      uerror("WSASend", Nothing);
    }
    CAMLreturn0;
}

value ocaml_iocp_update_accept_ctx(value v_sock) {
  CAMLparam1(v_sock);
  BOOL bOptVal = TRUE;
  int bOptLen = sizeof (BOOL);
  setsockopt(Socket_val(v_sock), SOL_SOCKET, SO_UPDATE_ACCEPT_CONTEXT, (char *) &bOptVal, bOptLen);
  CAMLreturn(Val_unit);
}

value ocaml_iocp_update_connect_ctx(value v_sock) {
  CAMLparam1(v_sock);
  BOOL bOptVal = TRUE;
  int bOptLen = sizeof (BOOL);
  setsockopt(Socket_val(v_sock), SOL_SOCKET, SO_UPDATE_CONNECT_CONTEXT, (char *) &bOptVal, bOptLen);
  CAMLreturn(Val_unit);
}

value ocaml_iocp_cancel(value v_fd, value v_overlapped) {
  CAMLparam2(v_fd, v_overlapped);
  LPOVERLAPPED ol = overlapped_ptr_of_val(Field(v_overlapped,0));

  BOOL b;
  b = CancelIoEx(Handle_val(v_fd), ol);

  CAMLreturn(Val_unit);
}

static int shutdown_command_table[] = {
  0, 1, 2
};

value caml_iocp_shutdown(value v_sock, value cmd)
{
  CAMLparam1(v_sock);
  if (shutdown(Socket_val(v_sock), shutdown_command_table[Int_val(cmd)]) == -1) {
    win32_maperr(WSAGetLastError());
    uerror("shutdown", Nothing);
  }
  CAMLreturn(Val_unit);
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

value ocaml_iocp_unix_pipe(value v_iocp, value v_id1, value v_id2, value v_path)
{
  CAMLparam4(v_iocp, v_id1, v_id2, v_path);
  CAMLlocal2(readfd, writefd);
  value res;
  char *wpath = caml_stat_strdup(String_val(v_path));
  HANDLE iocp = Handle_val(v_iocp);

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

  caml_stat_free(wpath);

  if (pipeR == INVALID_HANDLE_VALUE || pipeR == NULL) {
    win32_maperr(GetLastError());
    uerror("CreateNamedPipe", Nothing);
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
    uerror("CreateNamedPipe", Nothing);
  }

  HANDLE t = CreateIoCompletionPort(pipeR, iocp, v_id1, 0);

  if (t == NULL) {
      win32_maperr(GetLastError());
      // printf("RETURNING FAILED %i\n", GetLastError());
      uerror("CreateIoCompletionPort1", Nothing);
    }

  t = CreateIoCompletionPort(pipeW, iocp, v_id2, 0);

  if (t == NULL) {
      win32_maperr(GetLastError());
      // printf("RETURNING FAILED %i\n", GetLastError());
      uerror("CreateIoCompletionPort2", Nothing);
    }

  writefd = win_alloc_handle(pipeW);
  readfd = win_alloc_handle(pipeR);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = readfd;
  Field(res, 1) = writefd;
  CAMLreturn(res);
}

CAMLprim value ocaml_iocp_unix_open(value v_iocp, value v_id, value path, value flags, value perm)
{
  CAMLparam5(v_iocp, v_id, path, flags, perm);
  int fileaccess, createflags, fileattrib, filecreate, sharemode, cloexec;
  SECURITY_ATTRIBUTES attr;
  HANDLE h;
  char * wpath;

  HANDLE iocp = Handle_val(v_iocp);

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

  cloexec = caml_convert_flag_list(flags, open_cloexec_flags);
  attr.nLength = sizeof(attr);
  attr.lpSecurityDescriptor = NULL;
  attr.bInheritHandle =
    cloexec & CLOEXEC ? FALSE
                      : cloexec & KEEPEXEC ? TRUE
                                           : !unix_cloexec_default;

  wpath = caml_stat_strdup(String_val(path));
  h = CreateFile(wpath, fileaccess,
                 sharemode, &attr,
                 filecreate, FILE_FLAG_OVERLAPPED, NULL);
  caml_stat_free(wpath);
  if (h == INVALID_HANDLE_VALUE) {
    win32_maperr(GetLastError());
    uerror("open", path);
  }

  CreateIoCompletionPort(h, iocp, v_id, 0);

  CAMLreturn(win_alloc_handle(h));
}