(* Min-heap keyed on Ptime.t for k-way merge *)

type t = {
  mutable data : Weft_types.log_entry array;
  mutable size : int;
}

let dummy_entry : Weft_types.log_entry = {
  timestamp = Ptime.epoch;
  raw = "";
  source = "";
  terms = [];
  metadata = [];
}

let create () =
  { data = Array.make 16 dummy_entry; size = 0 }

let parent i = (i - 1) / 2
let left i = 2 * i + 1
let right i = 2 * i + 2

let compare_entries (a : Weft_types.log_entry) (b : Weft_types.log_entry) =
  Ptime.compare a.timestamp b.timestamp

let swap h i j =
  let tmp = h.data.(i) in
  h.data.(i) <- h.data.(j);
  h.data.(j) <- tmp

let grow h =
  let new_cap = Array.length h.data * 2 in
  let new_arr = Array.make new_cap h.data.(0) in
  Array.blit h.data 0 new_arr 0 h.size;
  h.data <- new_arr

let sift_up h i =
  let pos = ref i in
  while !pos > 0 && compare_entries h.data.(!pos) h.data.(parent !pos) < 0 do
    swap h !pos (parent !pos);
    pos := parent !pos
  done

let sift_down h i =
  let pos = ref i in
  let continue = ref true in
  while !continue do
    let smallest = ref !pos in
    let l = left !pos in
    let r = right !pos in
    if l < h.size && compare_entries h.data.(l) h.data.(!smallest) < 0 then
      smallest := l;
    if r < h.size && compare_entries h.data.(r) h.data.(!smallest) < 0 then
      smallest := r;
    if !smallest <> !pos then begin
      swap h !pos !smallest;
      pos := !smallest
    end else
      continue := false
  done

let push h entry =
  if h.size >= Array.length h.data then grow h;
  h.data.(h.size) <- entry;
  h.size <- h.size + 1;
  sift_up h (h.size - 1)

let pop h =
  if h.size = 0 then None
  else begin
    let min = h.data.(0) in
    h.size <- h.size - 1;
    if h.size > 0 then begin
      h.data.(0) <- h.data.(h.size);
      sift_down h 0
    end;
    Some min
  end

let peek h =
  if h.size = 0 then None
  else Some h.data.(0)

let is_empty h = h.size = 0
let length h = h.size
