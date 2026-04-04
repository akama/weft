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

let () =
  Alcotest.run "weft_merge" [
    "heap", [
      Alcotest.test_case "basic operations" `Quick test_heap_basic;
    ];
    "batch_merge", [
      Alcotest.test_case "two streams" `Quick test_batch_merge;
      Alcotest.test_case "empty" `Quick test_batch_merge_empty;
      Alcotest.test_case "single stream" `Quick test_batch_merge_single;
    ];
  ]
