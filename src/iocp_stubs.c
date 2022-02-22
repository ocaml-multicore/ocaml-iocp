
/*----------------------------------------------------------------------
   Copyright (c) 2022 Patrick Ferris <patrick@sirref.org>
   Distributed under the MIT license. See terms at the end of this file.
  ----------------------------------------------------------------------*/

// OCaml APIs
#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <caml/socketaddr.h>
#include <caml/osdeps.h>

// Windows APIs
#include <Fileapi.h>
#include <Minwinbase.h>

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

value ocaml_iocp_make_overlapped(value v_unit) {
    CAMLparam0();
    CAMLlocal1(v);
    LPOVERLAPPED ol;

    // Allocate an OCaml value on the heap to store a pointer to the pointer to the overlapped struct.
    v = caml_alloc_custom_mem(&overlapped_ops, sizeof(LPOVERLAPPED), sizeof(OVERLAPPED));

    // Allocate (in the C heap) the overlapped struct.
    ol = (LPOVERLAPPED) caml_stat_alloc(sizeof(OVERLAPPED));
    Overlapped_val(v) = ol;
    memset(ol, 0, sizeof(OVERLAPPED));
    // ol->Offset = 2;
    CAMLreturn(v);
}

// The key cannot be GC'd until the event is complete
value ocaml_iocp_create_io_completion_port(value v_key, value v_threads) {
    CAMLparam0();
    CAMLlocal1(v);
    int num_threads = Int_val(v_threads);

    HANDLE cp = CreateIoCompletionPort(INVALID_HANDLE_VALUE, NULL, v_key, num_threads);

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
    HANDLE fd = Handle_val(v_fd);
    LPOVERLAPPED ol;
    DWORD transferred = 0;
    DWORD_PTR ptr = 0 ;

    ol = (LPOVERLAPPED) caml_stat_alloc(sizeof(OVERLAPPED));
    v_ol = caml_alloc_custom_mem(&overlapped_ops, sizeof(LPOVERLAPPED), sizeof(OVERLAPPED));
    BOOL b = GetQueuedCompletionStatus(fd, &transferred, &ptr, &ol, INFINITE);
    if (!b) {
        printf("ERRR %i\n", GetLastError());
    }
    Overlapped_val(v_ol) = ol;
    v = Val_completion_status(Val_int(ptr), Val_int(transferred), v_ol);
    CAMLreturn(v);
}

value ocaml_iocp_read_file(value v_cp, value v_fd, value v_id, value v_ba, value v_num_bytes, value v_overlapped) {
    CAMLparam4(v_cp, v_fd, v_ba, v_overlapped);
    LPOVERLAPPED ol = Overlapped_val(v_overlapped);

    // Here we associate the file handle to the completion port handle...
    HANDLE _cp = CreateIoCompletionPort(Handle_val(v_fd), Handle_val(v_cp), Long_val(v_id), 0);
    BOOL b = ReadFile(Handle_val(v_fd), Caml_ba_data_val(v_ba), Int_val(v_num_bytes), NULL, ol);

    // The return value is non-zero (TRUE) on success. However, it is FALSE if the IO operation
    // is completing asynchronously. We change that behaviour by checking last error.
    if (GetLastError() == ERROR_IO_PENDING) {
        CAMLreturn(Val_true);
    }
    CAMLreturn(Val_bool(b));
}

value ocaml_iocp_read_file_bytes(value* values, int argc) {
    return ocaml_iocp_read_file(values[0], values[1], values[2], values[3], values[4], values[5]);
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