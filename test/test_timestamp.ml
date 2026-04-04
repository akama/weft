let test_iso8601 () =
  let input = "2026-04-03T14:30:01.123Z some log message" in
  match Weft_time.parse_iso8601 input with
  | None -> Alcotest.fail "Failed to parse ISO8601 timestamp"
  | Some t ->
    let (date, ((hh, mm, ss), _)) = Ptime.to_date_time t in
    Alcotest.(check int) "year" 2026 (let (y, _, _) = date in y);
    Alcotest.(check int) "month" 4 (let (_, m, _) = date in m);
    Alcotest.(check int) "day" 3 (let (_, _, d) = date in d);
    Alcotest.(check int) "hour" 14 hh;
    Alcotest.(check int) "minute" 30 mm;
    Alcotest.(check int) "second" 1 ss

let test_iso8601_with_offset () =
  let input = "2026-04-03T14:30:01+05:30 log" in
  match Weft_time.parse_iso8601 input with
  | None -> Alcotest.fail "Failed to parse ISO8601 with offset"
  | Some t ->
    (* UTC should be 14:30 - 5:30 = 09:00 *)
    let (_, ((hh, mm, _), _)) = Ptime.to_date_time t in
    Alcotest.(check int) "hour UTC" 9 hh;
    Alcotest.(check int) "minute UTC" 0 mm

let test_syslog_bsd () =
  let input = "Apr  3 14:30:01 myhost sshd[1234]: message" in
  match Weft_time.parse_syslog_bsd input with
  | None -> Alcotest.fail "Failed to parse syslog BSD timestamp"
  | Some t ->
    let (date, ((hh, mm, ss), _)) = Ptime.to_date_time t in
    Alcotest.(check int) "month" 4 (let (_, m, _) = date in m);
    Alcotest.(check int) "day" 3 (let (_, _, d) = date in d);
    Alcotest.(check int) "hour" 14 hh;
    Alcotest.(check int) "minute" 30 mm;
    Alcotest.(check int) "second" 1 ss

let test_epoch_ms () =
  (* 2026-04-03T14:30:01.000Z = 1775142601000 ms *)
  let input = "1775142601000" in
  match Weft_time.parse_epoch_ms input with
  | None -> Alcotest.fail "Failed to parse epoch_ms"
  | Some _t -> ()  (* Just check it parses *)

let test_common_log () =
  let input = "[03/Apr/2026:14:30:01 +0000] GET /index.html" in
  match Weft_time.parse_common_log input with
  | None -> Alcotest.fail "Failed to parse common log timestamp"
  | Some t ->
    let (date, ((hh, _, _), _)) = Ptime.to_date_time t in
    Alcotest.(check int) "month" 4 (let (_, m, _) = date in m);
    Alcotest.(check int) "day" 3 (let (_, _, d) = date in d);
    Alcotest.(check int) "hour" 14 hh

let test_auto_detect_iso () =
  let input = "2026-04-03T14:30:01Z some message" in
  match Weft_time.auto_detect input with
  | None -> Alcotest.fail "Auto-detect failed on ISO8601"
  | Some _ -> ()

let test_auto_detect_syslog () =
  let input = "Apr  3 14:30:01 myhost message" in
  match Weft_time.auto_detect input with
  | None -> Alcotest.fail "Auto-detect failed on syslog"
  | Some _ -> ()

let test_parser_of_strategy () =
  let parser = Weft_time.parser_of_strategy
    (Weft_types.Prefix_format "iso8601") in
  match parser "2026-01-01T00:00:00Z test" with
  | None -> Alcotest.fail "Strategy parser failed"
  | Some _ -> ()

let test_strptime_basic () =
  let parser = Weft_time.parser_of_strategy
    (Weft_types.Prefix_format "%Y-%m-%d %H:%M:%S") in
  match parser "2026-04-03 14:30:01 some log" with
  | None -> Alcotest.fail "Strptime parser failed"
  | Some t ->
    let (date, ((hh, mm, ss), _)) = Ptime.to_date_time t in
    Alcotest.(check int) "year" 2026 (let (y, _, _) = date in y);
    Alcotest.(check int) "hour" 14 hh;
    Alcotest.(check int) "minute" 30 mm;
    Alcotest.(check int) "second" 1 ss;
    ignore date

let () =
  Alcotest.run "weft_time" [
    "iso8601", [
      Alcotest.test_case "basic" `Quick test_iso8601;
      Alcotest.test_case "with offset" `Quick test_iso8601_with_offset;
    ];
    "syslog_bsd", [
      Alcotest.test_case "basic" `Quick test_syslog_bsd;
    ];
    "epoch_ms", [
      Alcotest.test_case "basic" `Quick test_epoch_ms;
    ];
    "common_log", [
      Alcotest.test_case "basic" `Quick test_common_log;
    ];
    "auto_detect", [
      Alcotest.test_case "iso8601" `Quick test_auto_detect_iso;
      Alcotest.test_case "syslog" `Quick test_auto_detect_syslog;
    ];
    "strategy", [
      Alcotest.test_case "prefix format" `Quick test_parser_of_strategy;
      Alcotest.test_case "strptime" `Quick test_strptime_basic;
    ];
  ]
