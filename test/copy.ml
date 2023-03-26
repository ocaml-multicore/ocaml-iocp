
let usage () =
  Printf.fprintf stderr {|
Usage:
  %s <source filename> <destination filename>
    -- copy <source filename> to <destination filename>
  |} (Filename.basename Sys.argv.(0))

let () =
  if Array.length Sys.argv <> 3 then begin
    Printf.fprintf stderr "Please supply source and destination filenames\n";
    usage();
    exit(-1);
  end;
  Copy_lib.run_cp 1024 16 Sys.argv.(1) Sys.argv.(2) ()
