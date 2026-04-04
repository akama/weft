(* K-way merge of sorted Seq.t streams using a min-heap *)

(* Entry tagged with stream index for heap tracking *)
type tagged_entry = {
  entry : Weft_types.log_entry;
  stream_idx : int;
}

type tagged_heap = {
  mutable data : tagged_entry array;
  mutable size : int;
}

let dummy_tagged = {
  entry = Heap.dummy_entry;
  stream_idx = 0;
}

let tagged_heap_create () =
  { data = Array.make 16 dummy_tagged; size = 0 }

let compare_tagged a b =
  Ptime.compare a.entry.timestamp b.entry.timestamp

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
  while !pos > 0 && compare_tagged h.data.(!pos) h.data.((!pos - 1) / 2) < 0 do
    let parent = (!pos - 1) / 2 in
    swap h !pos parent;
    pos := parent
  done

let sift_down h i =
  let pos = ref i in
  let continue = ref true in
  while !continue do
    let smallest = ref !pos in
    let l = 2 * !pos + 1 in
    let r = 2 * !pos + 2 in
    if l < h.size && compare_tagged h.data.(l) h.data.(!smallest) < 0 then
      smallest := l;
    if r < h.size && compare_tagged h.data.(r) h.data.(!smallest) < 0 then
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

let merge (streams : (Weft_types.source_id * Weft_types.log_entry Seq.t) list) : Weft_types.log_entry Seq.t =
  let heap = tagged_heap_create () in
  let seqs = Array.of_list (List.map snd streams) in
  (* Seed the heap with the first entry from each stream *)
  Array.iteri (fun idx seq ->
    match seq () with
    | Seq.Nil -> ()
    | Seq.Cons (entry, rest) ->
      seqs.(idx) <- rest;
      push heap { entry; stream_idx = idx }
  ) seqs;
  let rec next () =
    match pop heap with
    | None -> Seq.Nil
    | Some tagged ->
      (* Advance the stream this entry came from *)
      let idx = tagged.stream_idx in
      (match seqs.(idx) () with
       | Seq.Nil -> ()
       | Seq.Cons (next_entry, rest) ->
         seqs.(idx) <- rest;
         push heap { entry = next_entry; stream_idx = idx });
      Seq.Cons (tagged.entry, next)
  in
  next

let merge_with_dedup streams =
  let dedup = Dedup.create () in
  let merged = merge streams in
  Seq.filter (fun entry ->
    not (Dedup.check_and_mark dedup entry)
  ) merged
