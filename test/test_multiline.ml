let test_join_basic () =
  let ml = Weft_middleware.Multiline.create
    ~continuation:{|^\s|^Raised_at|^Called_from|}
    ~max_lines:50 in
  let lines = [
    "2026-04-03T14:30:01Z ERROR exception raised";
    "Raised_at Stdlib.failwith in file \"stdlib.ml\"";
    "Called_from Main.run in file \"main.ml\"";
    "2026-04-03T14:30:02Z INFO recovered";
  ] in
  let blocks = Weft_middleware.Multiline.join_lines ml lines in
  Alcotest.(check int) "block count" 2 (List.length blocks);
  let first = List.hd blocks in
  Alcotest.(check bool) "first block has continuation" true
    (String.length first > String.length "2026-04-03T14:30:01Z ERROR exception raised")

let test_join_no_continuation () =
  let ml = Weft_middleware.Multiline.create
    ~continuation:{|^\s|}
    ~max_lines:50 in
  let lines = ["line1"; "line2"; "line3"] in
  let blocks = Weft_middleware.Multiline.join_lines ml lines in
  Alcotest.(check int) "no joins" 3 (List.length blocks)

let test_join_max_lines () =
  let ml = Weft_middleware.Multiline.create
    ~continuation:{|^\s|}
    ~max_lines:3 in
  let lines = [
    "start";
    " cont1";
    " cont2";
    " cont3";  (* This would be line 4 — over max *)
    " cont4";
    "new block";
  ] in
  let blocks = Weft_middleware.Multiline.join_lines ml lines in
  (* First block: start + cont1 + cont2 (3 lines)
     cont3 starts new block (over limit): cont3 + cont4 (2 lines)
     new block: 1 line *)
  Alcotest.(check int) "blocks after max" 3 (List.length blocks)

let test_streaming () =
  let ml = Weft_middleware.Multiline.create
    ~continuation:{|^\s|}
    ~max_lines:50 in
  let state = Weft_middleware.Multiline.create_state ml in
  (* Feed lines one at a time *)
  let r1 = Weft_middleware.Multiline.feed_line state "first line" in
  Alcotest.(check bool) "no block yet" true (r1 = None);
  let r2 = Weft_middleware.Multiline.feed_line state " continuation" in
  Alcotest.(check bool) "still accumulating" true (r2 = None);
  let r3 = Weft_middleware.Multiline.feed_line state "second line" in
  Alcotest.(check bool) "first block emitted" true (r3 <> None);
  let block = Option.get r3 in
  Alcotest.(check bool) "block contains continuation" true
    (String.length block > 10);
  let r4 = Weft_middleware.Multiline.flush state in
  Alcotest.(check bool) "flush emits remaining" true (r4 <> None)

let () =
  Alcotest.run "weft_multiline" [
    "join", [
      Alcotest.test_case "basic" `Quick test_join_basic;
      Alcotest.test_case "no continuation" `Quick test_join_no_continuation;
      Alcotest.test_case "max lines" `Quick test_join_max_lines;
    ];
    "streaming", [
      Alcotest.test_case "feed and flush" `Quick test_streaming;
    ];
  ]
