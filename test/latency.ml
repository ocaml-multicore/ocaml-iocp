
type t = {
  raw: int64 Array.t;
  mutable dropped: int;
  mutable next: int;
}

let make max = {
  raw = Array.make max 0L;
  dropped = 0;
  next = 0;
}

let add t ns =
  if t.next >= Array.length t.raw
  then t.dropped <- t.dropped + 1
  else begin
    t.raw.(t.next) <- ns;
    t.next <- t.next + 1;
  end

let bins t n out =
  if Array.length t.raw == 0 then failwith "0 values";
  let min a b = if a < b then a else b in
  let max a b = if a > b then a else b in
  let used = Array.sub t.raw 0 t.next in
  let minimum, maximum = Array.fold_left (fun (a, b) x -> min a x, max b x) (t.raw.(0), t.raw.(0)) used in
  Printf.fprintf out "# latency ranges from %Ld to %Ld\n" minimum maximum;
  let range = Int64.sub maximum minimum in
  let ideal_bin_size = Int64.(div range (of_int (n-1))) in
  (* the minimum and ideal_bin_size is probably an odd number like 12345. Round to multiple of 1 or 5 *)
  let rec one_sf x exp =
    if x < 5L then exp
    else if x < 10L then Int64.mul 5L exp
    else one_sf (Int64.div x 10L) (Int64.mul exp 10L) in
  let minimum = one_sf minimum 1L in
  let bin_size = one_sf ideal_bin_size 1L in
  Printf.fprintf out "# bin size %Ld\n" bin_size;
  let bin_of_ns ns = Int64.(to_int (div (sub ns minimum) bin_size)) in
  let midpoint_of_bin idx = Int64.(add (add minimum (mul (of_int idx) bin_size)) (div bin_size 2L)) in
  let bins = Array.make (bin_of_ns maximum + 1) 0 in
  for i = 0 to Array.length used - 1 do
    if t.raw.(i) = 0L then failwith (Printf.sprintf "idx %d" i);
    let idx = bin_of_ns t.raw.(i) in
    if idx >= Array.length bins then begin
    Printf.fprintf out "min %Ld max %Ld bin_size %Ld latency %Ld \n" minimum maximum bin_size t.raw.(i);
    end;
    bins.(idx) <- bins.(idx) + 1
  done;
  Printf.fprintf out "# latency / ns, count\n";
  for i = 0 to Array.length bins - 1 do
    let ns = midpoint_of_bin i in
    Printf.fprintf out "%Ld, %d\n" ns bins.(i)
  done