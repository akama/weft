let test_strip_ansi () =
  let input = "\027[31mERROR\027[0m: something failed" in
  let output = Weft_middleware.Strip_ansi.apply input in
  Alcotest.(check string) "strip ansi" "ERROR: something failed" output

let test_strip_ansi_no_codes () =
  let input = "plain text" in
  let output = Weft_middleware.Strip_ansi.apply input in
  Alcotest.(check string) "no-op" "plain text" output

let test_regex_extract () =
  let t = Weft_middleware.Regex_extract.create
    ~pattern:{|(\w+)\s+(\w+):\s+(.*)|}
    ~fields:(Some ["level"; "module"; "message"]) in
  let meta = Weft_middleware.Regex_extract.apply t "ERROR auth: login failed" [] in
  Alcotest.(check string) "level" "ERROR"
    (List.assoc "level" meta);
  Alcotest.(check string) "module" "auth"
    (List.assoc "module" meta);
  Alcotest.(check string) "message" "login failed"
    (List.assoc "message" meta)

let test_json_field_extract () =
  let t = Weft_middleware.Json_field_extract.create
    ~fields:["level"; "msg"; "err"]
    ~source_field:None in
  let input = {|{"level":"error","msg":"connection reset","err":"ECONNRESET","ts":1234}|} in
  let meta = Weft_middleware.Json_field_extract.apply t input [] in
  Alcotest.(check string) "level" "error"
    (List.assoc "level" meta);
  Alcotest.(check string) "msg" "connection reset"
    (List.assoc "msg" meta);
  Alcotest.(check string) "err" "ECONNRESET"
    (List.assoc "err" meta)

let test_regex_filter_exclude () =
  let t = Weft_middleware.Regex_filter.create
    ~include_:None ~exclude:(Some {|^DEBUG|healthcheck|}) in
  Alcotest.(check bool) "keep error" true
    (Weft_middleware.Regex_filter.should_keep t "ERROR: something");
  Alcotest.(check bool) "exclude debug" false
    (Weft_middleware.Regex_filter.should_keep t "DEBUG: trace info");
  Alcotest.(check bool) "exclude healthcheck" false
    (Weft_middleware.Regex_filter.should_keep t "GET /healthcheck 200")

let test_field_rename () =
  let t = Weft_middleware.Field_rename.create
    ~mapping:[("msg", "message"); ("lvl", "level")] in
  let meta = [("msg", "hello"); ("lvl", "info"); ("other", "keep")] in
  let renamed = Weft_middleware.Field_rename.apply t meta in
  Alcotest.(check string) "renamed msg" "hello"
    (List.assoc "message" renamed);
  Alcotest.(check string) "renamed lvl" "info"
    (List.assoc "level" renamed);
  Alcotest.(check string) "kept other" "keep"
    (List.assoc "other" renamed)

let test_pipeline_process () =
  let format : Weft_types.format_config = {
    name = "test";
    timestamp = Some (Weft_types.Prefix_format "iso8601");
    multiline = None;
    rotation = None;
    middleware = [
      Weft_types.Regex_extract {
        pattern = {|(\w+)\s+(\w+)\s+(.*)|};
        fields = Some ["_ts"; "level"; "message"];
      };
    ];
  } in
  let pipeline = Weft_middleware.Pipeline.create format in
  let entries = Weft_middleware.Pipeline.process_lines pipeline
    ~source:"test-src"
    ["2026-04-03T14:30:01Z ERROR something broke";
     "2026-04-03T14:30:02Z INFO all good"] in
  Alcotest.(check int) "entry count" 2 (List.length entries);
  let e1 = List.hd entries in
  Alcotest.(check string) "source" "test-src" e1.source;
  (* The regex matches from the beginning, so first group gets the timestamp portion *)
  Alcotest.(check bool) "has level field" true
    (List.mem_assoc "level" e1.metadata)

let () =
  Alcotest.run "weft_middleware" [
    "strip_ansi", [
      Alcotest.test_case "with codes" `Quick test_strip_ansi;
      Alcotest.test_case "no codes" `Quick test_strip_ansi_no_codes;
    ];
    "regex_extract", [
      Alcotest.test_case "basic" `Quick test_regex_extract;
    ];
    "json_field_extract", [
      Alcotest.test_case "basic" `Quick test_json_field_extract;
    ];
    "regex_filter", [
      Alcotest.test_case "exclude" `Quick test_regex_filter_exclude;
    ];
    "field_rename", [
      Alcotest.test_case "basic" `Quick test_field_rename;
    ];
    "pipeline", [
      Alcotest.test_case "process lines" `Quick test_pipeline_process;
    ];
  ]
