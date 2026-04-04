open Weft_types

(* K-way merge of sorted Seq.t streams using a min-heap *)

type source_stream = {
  mutable seq : log_entry Seq.t;
  source_id : source_id;
}

let merge (streams : (source_id * log_entry Seq.t) list) : log_entry Seq.t =
  let heap = Heap.create () in
  let sources = Array.of_list (List.map (fun (sid, seq) ->
    { seq; source_id = sid }
  ) streams) in
  (* Seed the heap with the first entry from each stream *)
  Array.iter (fun src ->
    match src.seq () with
    | Seq.Nil -> ()
    | Seq.Cons (entry, rest) ->
      src.seq <- rest;
      (* Tag with source index for replacement *)
      Heap.push heap entry
  ) sources;
  (* Build the merged sequence lazily *)
  let rec next () =
    match Heap.pop heap with
    | None -> Seq.Nil
    | Some entry ->
      (* Find the source this entry came from and push its next entry *)
      let pushed = ref false in
      Array.iter (fun src ->
        if not !pushed && src.source_id = entry.source then
          match src.seq () with
          | Seq.Nil -> ()
          | Seq.Cons (next_entry, rest) ->
            src.seq <- rest;
            Heap.push heap next_entry;
            pushed := true
      ) sources;
      Seq.Cons (entry, next)
  in
  next

let merge_with_dedup streams =
  let dedup = Dedup.create () in
  let merged = merge streams in
  Seq.filter (fun entry ->
    not (Dedup.check_and_mark dedup entry)
  ) merged
