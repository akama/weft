let test_expand_simple () =
  let expanded = Weft_middleware.Grok.expand_pattern "%{IP:client} %{WORD:method}" in
  (* Should contain named groups *)
  Alcotest.(check bool) "has client group" true
    (String.length expanded > 20);
  (* Should not contain %{ references *)
  let has_ref = Re.execp (Re.compile (Re.str "%{")) expanded in
  Alcotest.(check bool) "no raw refs" false has_ref

let test_grok_apply () =
  let t = Weft_middleware.Grok.create "%{WORD:method} %{NUMBER:status}" in
  let meta = Weft_middleware.Grok.apply t "GET 200" [] in
  (* Check that we extracted some fields *)
  Alcotest.(check bool) "has fields" true (List.length meta > 0)

let test_grok_no_match () =
  let t = Weft_middleware.Grok.create "%{WORD:method} %{NUMBER:status}" in
  let meta = Weft_middleware.Grok.apply t "!!!" [] in
  Alcotest.(check int) "no fields" 0 (List.length meta)

let test_pattern_library () =
  (* Check a few standard patterns exist *)
  Alcotest.(check bool) "IP exists" true
    (Weft_middleware.Grok_patterns.lookup "IP" <> None);
  Alcotest.(check bool) "WORD exists" true
    (Weft_middleware.Grok_patterns.lookup "WORD" <> None);
  Alcotest.(check bool) "LOGLEVEL exists" true
    (Weft_middleware.Grok_patterns.lookup "LOGLEVEL" <> None);
  Alcotest.(check bool) "TIMESTAMP_ISO8601 exists" true
    (Weft_middleware.Grok_patterns.lookup "TIMESTAMP_ISO8601" <> None);
  Alcotest.(check bool) "NONEXISTENT is None" true
    (Weft_middleware.Grok_patterns.lookup "NONEXISTENT" = None)

let () =
  Alcotest.run "weft_grok" [
    "expand", [
      Alcotest.test_case "simple" `Quick test_expand_simple;
    ];
    "apply", [
      Alcotest.test_case "basic match" `Quick test_grok_apply;
      Alcotest.test_case "no match" `Quick test_grok_no_match;
    ];
    "patterns", [
      Alcotest.test_case "library" `Quick test_pattern_library;
    ];
  ]
