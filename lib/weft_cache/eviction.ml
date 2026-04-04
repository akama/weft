open Weft_types

(* Evict expired sealed segments *)
let evict_expired (manifest : cache_manifest) : cache_manifest =
  let segments = List.filter (fun (seg : segment) ->
    if seg.sealed && Segment.is_expired seg then false
    else true
  ) manifest.segments in
  { manifest with segments }

(* Evict oldest sealed segments if total size exceeds limit *)
let evict_by_size (manifest : cache_manifest) ~max_bytes : cache_manifest =
  let total_size = List.fold_left (fun acc (seg : segment) ->
    Int64.add acc seg.size_bytes
  ) 0L manifest.segments in
  if total_size <= max_bytes then manifest
  else
    (* Sort sealed segments by fetched_at (oldest first) for LRU eviction *)
    let sealed, unsealed = List.partition (fun (s : segment) -> s.sealed) manifest.segments in
    let sorted = List.sort (fun (a : segment) (b : segment) ->
      Ptime.compare a.fetched_at b.fetched_at
    ) sealed in
    let current_size = ref total_size in
    let kept = List.filter (fun (seg : segment) ->
      if !current_size > max_bytes then begin
        current_size := Int64.sub !current_size seg.size_bytes;
        false
      end else true
    ) sorted in
    { manifest with segments = unsealed @ kept }

let run_eviction manifest ~max_mb_per_source =
  let max_bytes = Int64.mul (Int64.of_int max_mb_per_source) (Int64.of_int (1024 * 1024)) in
  manifest
  |> evict_expired
  |> evict_by_size ~max_bytes
