type t = Unix.file_descr

let fd : t -> Unix.file_descr = fun x -> x
