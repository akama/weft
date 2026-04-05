open Weft_types

let make_entry ~source ~secs raw =
  let timestamp = match Ptime.of_float_s (float_of_int secs) with
    | Some t -> t | None -> Ptime.epoch in
  { timestamp; raw; source; terms = []; metadata = [] }

(* Helper to get selected entry from timeline *)
let selected t =
  Weft_tui.Timeline.selected_entry t
  |> Option.map (fun (e : log_entry) -> e.raw)

let test_focus_preserved_exact_match () =
  let t = Weft_tui.Timeline.create () in
  (* Start with 3 entries, select the middle one *)
  Weft_tui.Timeline.set_entries t [
    make_entry ~source:"a" ~secs:1 "first";
    make_entry ~source:"a" ~secs:2 "second";
    make_entry ~source:"a" ~secs:3 "third";
  ];
  Weft_tui.Timeline.scroll_down t;
  Alcotest.(check (option string)) "selected second"
    (Some "second") (selected t);
  (* Replace with larger set containing the same entries *)
  Weft_tui.Timeline.set_entries t [
    make_entry ~source:"b" ~secs:0 "before";
    make_entry ~source:"a" ~secs:1 "first";
    make_entry ~source:"a" ~secs:2 "second";
    make_entry ~source:"a" ~secs:3 "third";
    make_entry ~source:"b" ~secs:4 "after";
  ];
  Alcotest.(check (option string)) "still second"
    (Some "second") (selected t)

let test_focus_preserved_raw_fallback () =
  let t = Weft_tui.Timeline.create () in
  (* Tail entry has arrival-time timestamp *)
  let tail_ts = match Ptime.of_float_s 99999.0 with
    | Some t -> t | None -> Ptime.epoch in
  Weft_tui.Timeline.set_entries t [
    { timestamp = tail_ts; raw = "2026-04-04T10:00:01Z ERROR something broke";
      source = "gw"; terms = ["ERROR"]; metadata = [] };
  ];
  Alcotest.(check (option string)) "selected error"
    (Some "2026-04-04T10:00:01Z ERROR something broke") (selected t);
  (* Cache version has parsed timestamp (different from tail_ts) *)
  let parsed_ts = match Ptime.of_float_s 1712224801.0 with
    | Some t -> t | None -> Ptime.epoch in
  Weft_tui.Timeline.set_entries t [
    make_entry ~source:"gw" ~secs:1 "2026-04-04T09:59:59Z INFO ok";
    { timestamp = parsed_ts; raw = "2026-04-04T10:00:01Z ERROR something broke";
      source = "gw"; terms = []; metadata = [] };
    make_entry ~source:"gw" ~secs:3 "2026-04-04T10:00:03Z INFO recovered";
  ];
  Alcotest.(check (option string)) "found via raw fallback"
    (Some "2026-04-04T10:00:01Z ERROR something broke") (selected t)

let test_focus_reset_when_not_found () =
  let t = Weft_tui.Timeline.create () in
  Weft_tui.Timeline.set_entries t [
    make_entry ~source:"a" ~secs:1 "only in old set";
  ];
  Alcotest.(check (option string)) "selected"
    (Some "only in old set") (selected t);
  (* Replace with completely different entries *)
  Weft_tui.Timeline.set_entries t [
    make_entry ~source:"b" ~secs:10 "new first";
    make_entry ~source:"b" ~secs:11 "new second";
  ];
  Alcotest.(check (option string)) "reset to top"
    (Some "new first") (selected t)

let test_focus_preserved_desc_mode () =
  let t = Weft_tui.Timeline.create () in
  Weft_tui.Timeline.toggle_order t;  (* switch to Desc *)
  Weft_tui.Timeline.set_entries t [
    make_entry ~source:"a" ~secs:1 "oldest";
    make_entry ~source:"a" ~secs:2 "middle";
    make_entry ~source:"a" ~secs:3 "newest";
  ];
  (* In Desc mode, display index 0 = newest. Scroll down to middle *)
  Weft_tui.Timeline.scroll_down t;
  Alcotest.(check (option string)) "selected middle (desc)"
    (Some "middle") (selected t);
  (* Replace with broader set *)
  Weft_tui.Timeline.set_entries t [
    make_entry ~source:"b" ~secs:0 "before";
    make_entry ~source:"a" ~secs:1 "oldest";
    make_entry ~source:"a" ~secs:2 "middle";
    make_entry ~source:"a" ~secs:3 "newest";
    make_entry ~source:"b" ~secs:4 "after";
  ];
  Alcotest.(check (option string)) "still middle (desc)"
    (Some "middle") (selected t)

let test_focus_broadening_term_disable () =
  let t = Weft_tui.Timeline.create () in
  (* Simulate: term search returned 3 ERROR entries *)
  Weft_tui.Timeline.set_entries t [
    make_entry ~source:"gw" ~secs:10 "ERROR first";
    make_entry ~source:"gw" ~secs:20 "ERROR second";
    make_entry ~source:"gw" ~secs:30 "ERROR third";
  ];
  Weft_tui.Timeline.scroll_down t;
  Alcotest.(check (option string)) "on ERROR second"
    (Some "ERROR second") (selected t);
  (* Simulate: term disabled, load_all returns everything *)
  Weft_tui.Timeline.set_entries t [
    make_entry ~source:"gw" ~secs:5 "INFO start";
    make_entry ~source:"gw" ~secs:10 "ERROR first";
    make_entry ~source:"gw" ~secs:15 "INFO middle";
    make_entry ~source:"gw" ~secs:20 "ERROR second";
    make_entry ~source:"gw" ~secs:25 "INFO late";
    make_entry ~source:"gw" ~secs:30 "ERROR third";
  ];
  Alcotest.(check (option string)) "still on ERROR second"
    (Some "ERROR second") (selected t)

let test_focus_narrowing_source_isolate () =
  let t = Weft_tui.Timeline.create () in
  (* Mixed sources *)
  Weft_tui.Timeline.set_entries t [
    make_entry ~source:"gw" ~secs:1 "gw msg";
    make_entry ~source:"worker" ~secs:2 "worker msg";
    make_entry ~source:"gw" ~secs:3 "gw msg 2";
  ];
  Weft_tui.Timeline.scroll_down t;
  Alcotest.(check (option string)) "on worker msg"
    (Some "worker msg") (selected t);
  (* Isolate gw — worker entry gone *)
  Weft_tui.Timeline.set_entries t [
    make_entry ~source:"gw" ~secs:1 "gw msg";
    make_entry ~source:"gw" ~secs:3 "gw msg 2";
  ];
  (* Worker msg not in new set, should reset to top *)
  Alcotest.(check (option string)) "reset (worker gone)"
    (Some "gw msg") (selected t)

let test_freeze_prevents_auto_follow () =
  let t = Weft_tui.Timeline.create () in
  Weft_tui.Timeline.set_entries t [
    make_entry ~source:"a" ~secs:1 "first";
    make_entry ~source:"a" ~secs:2 "second";
  ];
  Weft_tui.Timeline.scroll_down t;
  Alcotest.(check (option string)) "on second"
    (Some "second") (selected t);
  (* Simulate: search dispatched, freeze auto-follow *)
  Weft_tui.Timeline.freeze_auto_follow := true;
  (* Tail entries arrive while frozen *)
  Weft_tui.Timeline.append_entry t (make_entry ~source:"a" ~secs:3 "tail1");
  Weft_tui.Timeline.append_entry t (make_entry ~source:"a" ~secs:4 "tail2");
  Weft_tui.Timeline.append_entry t (make_entry ~source:"a" ~secs:5 "tail3");
  (* Should still be on "second", not jumped to tail entries *)
  Alcotest.(check (option string)) "still on second (frozen)"
    (Some "second") (selected t);
  (* Search completes, set_entries with broader results *)
  Weft_tui.Timeline.freeze_auto_follow := false;
  Weft_tui.Timeline.set_entries t [
    make_entry ~source:"a" ~secs:1 "first";
    make_entry ~source:"a" ~secs:2 "second";
    make_entry ~source:"a" ~secs:3 "third";
    make_entry ~source:"a" ~secs:4 "fourth";
  ];
  Alcotest.(check (option string)) "restored to second"
    (Some "second") (selected t)

let test_freeze_desc_mode () =
  let t = Weft_tui.Timeline.create () in
  Weft_tui.Timeline.toggle_order t;  (* Desc *)
  Weft_tui.Timeline.set_entries t [
    make_entry ~source:"a" ~secs:1 "oldest";
    make_entry ~source:"a" ~secs:2 "middle";
    make_entry ~source:"a" ~secs:3 "newest";
  ];
  (* In Desc, display 0=newest. Scroll to middle *)
  Weft_tui.Timeline.scroll_down t;
  Alcotest.(check (option string)) "on middle"
    (Some "middle") (selected t);
  Weft_tui.Timeline.freeze_auto_follow := true;
  Weft_tui.Timeline.append_entry t (make_entry ~source:"a" ~secs:4 "tail1");
  Weft_tui.Timeline.append_entry t (make_entry ~source:"a" ~secs:5 "tail2");
  Alcotest.(check (option string)) "still middle (frozen desc)"
    (Some "middle") (selected t);
  Weft_tui.Timeline.freeze_auto_follow := false

let () =
  Alcotest.run "weft_timeline" [
    "focus", [
      Alcotest.test_case "exact match" `Quick test_focus_preserved_exact_match;
      Alcotest.test_case "raw fallback" `Quick test_focus_preserved_raw_fallback;
      Alcotest.test_case "not found" `Quick test_focus_reset_when_not_found;
      Alcotest.test_case "desc mode" `Quick test_focus_preserved_desc_mode;
      Alcotest.test_case "term disable" `Quick test_focus_broadening_term_disable;
      Alcotest.test_case "source isolate" `Quick test_focus_narrowing_source_isolate;
      Alcotest.test_case "freeze asc" `Quick test_freeze_prevents_auto_follow;
      Alcotest.test_case "freeze desc" `Quick test_freeze_desc_mode;
    ];
  ]
