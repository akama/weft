open Weft_types

let make_entry ~source ~secs raw =
  let timestamp = match Ptime.of_float_s (float_of_int secs) with
    | Some t -> t | None -> Ptime.epoch in
  { timestamp; raw; source; terms = []; metadata = [] }

let test_heap_basic () =
  let h = Weft_merge.Heap.create () in
  Alcotest.(check bool) "empty" true (Weft_merge.Heap.is_empty h);
  Weft_merge.Heap.push h (make_entry ~source:"a" ~secs:3 "third");
  Weft_merge.Heap.push h (make_entry ~source:"a" ~secs:1 "first");
  Weft_merge.Heap.push h (make_entry ~source:"a" ~secs:2 "second");
  Alcotest.(check int) "size" 3 (Weft_merge.Heap.length h);
  let e1 = Weft_merge.Heap.pop h |> Option.get in
  Alcotest.(check string) "first" "first" e1.raw;
  let e2 = Weft_merge.Heap.pop h |> Option.get in
  Alcotest.(check string) "second" "second" e2.raw;
  let e3 = Weft_merge.Heap.pop h |> Option.get in
  Alcotest.(check string) "third" "third" e3.raw;
  Alcotest.(check bool) "now empty" true (Weft_merge.Heap.is_empty h)

let test_batch_merge () =
  let s1 = List.to_seq [
    make_entry ~source:"a" ~secs:1 "a1";
    make_entry ~source:"a" ~secs:3 "a3";
    make_entry ~source:"a" ~secs:5 "a5";
  ] in
  let s2 = List.to_seq [
    make_entry ~source:"b" ~secs:2 "b2";
    make_entry ~source:"b" ~secs:4 "b4";
  ] in
  let merged = Weft_merge.Batch_merge.merge [("a", s1); ("b", s2)] in
  let entries = List.of_seq merged in
  Alcotest.(check int) "total entries" 5 (List.length entries);
  let raws = List.map (fun (e : log_entry) -> e.raw) entries in
  Alcotest.(check (list string)) "order" ["a1"; "b2"; "a3"; "b4"; "a5"] raws

let test_batch_merge_empty () =
  let merged = Weft_merge.Batch_merge.merge [] in
  let entries = List.of_seq merged in
  Alcotest.(check int) "empty merge" 0 (List.length entries)

let test_batch_merge_single () =
  let s1 = List.to_seq [
    make_entry ~source:"a" ~secs:1 "a1";
    make_entry ~source:"a" ~secs:2 "a2";
  ] in
  let merged = Weft_merge.Batch_merge.merge [("a", s1)] in
  let entries = List.of_seq merged in
  Alcotest.(check int) "single stream" 2 (List.length entries)

let test_merge_many_sources_sorted () =
  (* 6 sources with overlapping timestamps — verify output is strictly sorted *)
  let sources = List.init 6 (fun src_i ->
    let name = Printf.sprintf "src_%d" src_i in
    let entries = List.init 50 (fun j ->
      let secs = (src_i * 3) + (j * 10) + (Random.int 5) in
      make_entry ~source:name ~secs:(secs + 1000)
        (Printf.sprintf "%s_%d" name j)
    ) in
    (* Sort within each source (merge requires sorted input streams) *)
    let sorted = List.sort (fun (a : log_entry) (b : log_entry) ->
      Ptime.compare a.timestamp b.timestamp) entries in
    (name, List.to_seq sorted)
  ) in
  let merged = Weft_merge.Batch_merge.merge sources in
  let entries = List.of_seq merged in
  Alcotest.(check int) "300 entries" 300 (List.length entries);
  (* Verify strictly sorted *)
  let rec check_sorted = function
    | [] | [_] -> true
    | (a : log_entry) :: ((b : log_entry) :: _ as rest) ->
      if Ptime.is_later a.timestamp ~than:b.timestamp then false
      else check_sorted rest
  in
  Alcotest.(check bool) "sorted output" true (check_sorted entries);
  (* Verify entries from multiple sources are interleaved *)
  let first_10_sources = List.filteri (fun i _ -> i < 10) entries
    |> List.map (fun (e : log_entry) -> e.source) in
  let unique = List.sort_uniq String.compare first_10_sources in
  Alcotest.(check bool) "interleaved (multiple sources in first 10)"
    true (List.length unique > 1)

let test_unsorted_source_input () =
  (* Simulate tail segments where entries arrive out of order
     (e.g. cron jobs that fire at delayed times) *)
  let s1 = List.to_seq [
    make_entry ~source:"a" ~secs:1 "a1";
    make_entry ~source:"a" ~secs:3 "a3";
  ] in
  (* Source b has entries out of order — this breaks merge *)
  let s2 = List.to_seq [
    make_entry ~source:"b" ~secs:4 "b4";
    make_entry ~source:"b" ~secs:2 "b2";  (* out of order! *)
  ] in
  let merged = Weft_merge.Batch_merge.merge [("a", s1); ("b", s2)] in
  let entries = List.of_seq merged in
  let raws = List.map (fun (e : log_entry) -> e.raw) entries in
  (* Merge can't fix unsorted input — demonstrates why per-source
     sorting is needed before passing to merge *)
  Alcotest.(check (list string)) "unsorted input produces wrong order"
    ["a1"; "a3"; "b4"; "b2"] raws

let () =
  Alcotest.run "weft_merge" [
    "heap", [
      Alcotest.test_case "basic operations" `Quick test_heap_basic;
    ];
    "batch_merge", [
      Alcotest.test_case "two streams" `Quick test_batch_merge;
      Alcotest.test_case "empty" `Quick test_batch_merge_empty;
      Alcotest.test_case "single stream" `Quick test_batch_merge_single;
      Alcotest.test_case "many sources sorted" `Quick test_merge_many_sources_sorted;
      Alcotest.test_case "unsorted input" `Quick test_unsorted_source_input;
    ];
  ]
