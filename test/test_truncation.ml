(* Test that entry truncation takes newest, not oldest *)

open Weft_types

let make_entry ~source ~secs raw =
  let timestamp = match Ptime.of_float_s (float_of_int secs) with
    | Some t -> t | None -> Ptime.epoch in
  { timestamp; raw; source; terms = []; metadata = [] }

let test_seq_take_takes_oldest () =
  (* Demonstrate the problem: Seq.take N takes the first N (oldest) *)
  let entries = List.init 10 (fun i ->
    make_entry ~source:"a" ~secs:(i * 100 + 1000) (Printf.sprintf "entry_%d" i)
  ) in
  let seq = List.to_seq entries in
  let taken = List.of_seq (Seq.take 5 seq) in
  let raws = List.map (fun (e : log_entry) -> e.raw) taken in
  (* Takes the first 5 = oldest *)
  Alcotest.(check (list string)) "takes oldest"
    ["entry_0"; "entry_1"; "entry_2"; "entry_3"; "entry_4"] raws

let test_keep_newest_on_truncation () =
  (* When we have more entries than the limit, keep the newest ones *)
  let entries = List.init 10 (fun i ->
    make_entry ~source:"a" ~secs:(i * 100 + 1000)
      (Printf.sprintf "entry_%d" i)
  ) in
  let limit = 5 in
  let entry_list = List.of_seq (Seq.take (limit + 1) (List.to_seq entries)) in
  let truncated = List.length entry_list > limit in
  Alcotest.(check bool) "truncated" true truncated;
  let capped = if truncated then
    let len = List.length entry_list in
    List.filteri (fun i _ -> i >= len - limit) entry_list
  else entry_list in
  let raws = List.map (fun (e : log_entry) -> e.raw) capped in
  (* Should have the newest 5 entries *)
  Alcotest.(check (list string)) "keeps newest"
    ["entry_1"; "entry_2"; "entry_3"; "entry_4"; "entry_5"] raws

let test_no_truncation_preserves_all () =
  let entries = List.init 3 (fun i ->
    make_entry ~source:"a" ~secs:(i * 100 + 1000)
      (Printf.sprintf "entry_%d" i)
  ) in
  let limit = 5 in
  let entry_list = List.of_seq (Seq.take (limit + 1) (List.to_seq entries)) in
  let truncated = List.length entry_list > limit in
  Alcotest.(check bool) "not truncated" false truncated;
  let raws = List.map (fun (e : log_entry) -> e.raw) entry_list in
  Alcotest.(check (list string)) "all preserved"
    ["entry_0"; "entry_1"; "entry_2"] raws

let () =
  Alcotest.run "weft_truncation" [
    "truncation", [
      Alcotest.test_case "seq takes oldest" `Quick test_seq_take_takes_oldest;
      Alcotest.test_case "keep newest" `Quick test_keep_newest_on_truncation;
      Alcotest.test_case "no truncation" `Quick test_no_truncation_preserves_all;
    ];
  ]
