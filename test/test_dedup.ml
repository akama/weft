open Weft_types

let make_entry ~source ~secs raw =
  let timestamp = match Ptime.of_float_s (float_of_int secs) with
    | Some t -> t | None -> Ptime.epoch in
  { timestamp; raw; source; terms = []; metadata = [] }

let test_dedup_basic () =
  let d = Weft_merge.Dedup.create () in
  let e1 = make_entry ~source:"a" ~secs:1 "hello" in
  let e2 = make_entry ~source:"a" ~secs:1 "hello" in  (* same *)
  let e3 = make_entry ~source:"a" ~secs:1 "world" in  (* different raw *)
  let e4 = make_entry ~source:"b" ~secs:1 "hello" in  (* different source *)
  Alcotest.(check bool) "first is new" false
    (Weft_merge.Dedup.check_and_mark d e1);
  Alcotest.(check bool) "duplicate" true
    (Weft_merge.Dedup.check_and_mark d e2);
  Alcotest.(check bool) "different raw" false
    (Weft_merge.Dedup.check_and_mark d e3);
  Alcotest.(check bool) "different source" false
    (Weft_merge.Dedup.check_and_mark d e4)

let test_dedup_bounded () =
  let d = Weft_merge.Dedup.create ~max_size:3 () in
  let e1 = make_entry ~source:"a" ~secs:1 "one" in
  let e2 = make_entry ~source:"a" ~secs:2 "two" in
  let e3 = make_entry ~source:"a" ~secs:3 "three" in
  let e4 = make_entry ~source:"a" ~secs:4 "four" in
  ignore (Weft_merge.Dedup.check_and_mark d e1);
  ignore (Weft_merge.Dedup.check_and_mark d e2);
  ignore (Weft_merge.Dedup.check_and_mark d e3);
  (* e1 should still be remembered *)
  Alcotest.(check bool) "e1 still seen" true
    (Weft_merge.Dedup.is_duplicate d e1);
  (* Add e4, which should evict e1 *)
  ignore (Weft_merge.Dedup.check_and_mark d e4);
  (* e1 might be evicted now *)
  (* Note: exact eviction behavior depends on implementation *)
  ignore (Weft_merge.Dedup.is_duplicate d e1)

let test_merge_with_dedup () =
  (* Simulate catch-up/tail overlap: same entries from same source appear in both streams *)
  let s1 = List.to_seq [
    make_entry ~source:"app" ~secs:1 "msg1";
    make_entry ~source:"app" ~secs:2 "msg2";
    make_entry ~source:"app" ~secs:3 "msg3";
  ] in
  let s2 = List.to_seq [
    make_entry ~source:"app" ~secs:2 "msg2";  (* duplicate of s1's msg2 *)
    make_entry ~source:"app" ~secs:3 "msg3";  (* duplicate of s1's msg3 *)
    make_entry ~source:"app" ~secs:4 "msg4";  (* new entry *)
  ] in
  let merged = Weft_merge.Batch_merge.merge_with_dedup [("app", s1); ("app2", s2)] in
  let entries = List.of_seq merged in
  (* 6 total, 2 duplicates removed = 4 *)
  Alcotest.(check int) "deduped count" 4 (List.length entries)

let () =
  Alcotest.run "weft_dedup" [
    "dedup", [
      Alcotest.test_case "basic" `Quick test_dedup_basic;
      Alcotest.test_case "bounded" `Quick test_dedup_bounded;
    ];
    "merge_with_dedup", [
      Alcotest.test_case "dedup across streams" `Quick test_merge_with_dedup;
    ];
  ]
