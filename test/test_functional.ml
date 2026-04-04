(* Functional tests: exercise the full pipeline end-to-end
   config -> source read -> middleware -> merge -> output *)

open Weft_types

let test_dir = "/tmp/weft-functional-test"

let rm_rf dir =
  let rec remove path =
    if Sys.is_directory path then begin
      let entries = Sys.readdir path in
      Array.iter (fun f -> remove (Filename.concat path f)) entries;
      Unix.rmdir path
    end else
      Sys.remove path
  in
  if Sys.file_exists dir then
    (try remove dir with Sys_error _ | Unix.Unix_error _ -> ())

let setup () =
  rm_rf test_dir;
  Unix.mkdir test_dir 0o755

let write_file path content =
  let oc = open_out path in
  output_string oc content;
  close_out oc

let teardown () =
  rm_rf test_dir

(* Helper: run the full search pipeline and collect entries *)
let run_search ~formats_toml ~sources_toml ~terms =
  write_file (Filename.concat test_dir "formats.toml") formats_toml;
  write_file (Filename.concat test_dir "sources.toml") sources_toml;
  let formats = Weft_config.parse_formats_file
    (Filename.concat test_dir "formats.toml") in
  let sources = Weft_config.parse_sources_file
    (Filename.concat test_dir "sources.toml") in
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let cache_dir = Filename.concat test_dir "cache" in
  (try Unix.mkdir cache_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let cache_config = { dir = cache_dir; max_mb_per_source = 100;
                       default_ttl_hours = 72 } in
  let cache = Weft_cache.create ~fs cache_config in
  List.iter (fun (src : source_config) ->
    ignore (Weft_cache.init_source cache ~source_name:src.name ~format:src.format)
  ) sources.sources;
  let search = Weft_search.create ~cache
    ~sources:sources.sources ~formats ~general:sources.general () in
  List.iter (fun t -> ignore (Weft_search.add_term search t)) terms;
  let entries = if terms = [] then
    Weft_search.load_all search
  else
    Weft_search.search search ~time_range:None
  in
  List.of_seq entries

(* === Test: basic ISO8601 log file with regex extraction === *)
let test_iso8601_pipeline () =
  setup ();
  write_file (Filename.concat test_dir "app.log")
    "2026-04-04T10:00:01Z INFO [Auth]: User login succeeded\n\
     2026-04-04T10:00:02Z ERROR [Db]: Connection timeout\n\
     2026-04-04T10:00:03Z DEBUG [Cache]: Cache hit for key=abc\n";
  let entries = run_search
    ~formats_toml:{|
[format.app]
[format.app.timestamp]
position = "prefix"
format = "iso8601"
[[format.app.middleware]]
type = "regex_extract"
pattern = '\S+\s+(\w+)\s+\[(\w+)\]:\s+(.*)'
fields = ["level", "module", "message"]
|}
    ~sources_toml:(Printf.sprintf {|
[general]
default_time_range = "1h"
reorder_window_ms = 500
[cache]
dir = "%s/cache"
[[source]]
name = "app"
type = "file"
path = "%s/app.log"
format = "app"
|} test_dir test_dir)
    ~terms:[] in
  Alcotest.(check int) "3 entries" 3 (List.length entries);
  let e1 = List.nth entries 0 in
  Alcotest.(check string) "source" "app" e1.source;
  Alcotest.(check bool) "has level" true (List.mem_assoc "level" e1.metadata);
  Alcotest.(check string) "level=INFO" "INFO" (List.assoc "level" e1.metadata);
  Alcotest.(check string) "module=Auth" "Auth" (List.assoc "module" e1.metadata);
  (* Check timestamps are ordered *)
  let e2 = List.nth entries 1 in
  Alcotest.(check bool) "ordered" true
    (Ptime.is_later e2.timestamp ~than:e1.timestamp ||
     Ptime.equal e2.timestamp e1.timestamp);
  teardown ()

(* === Test: search with terms filters correctly === *)
let test_term_filtering () =
  setup ();
  write_file (Filename.concat test_dir "app.log")
    "2026-04-04T10:00:01Z INFO normal message\n\
     2026-04-04T10:00:02Z ERROR something broke\n\
     2026-04-04T10:00:03Z INFO another normal\n\
     2026-04-04T10:00:04Z ERROR critical failure\n";
  let entries = run_search
    ~formats_toml:{|
[format.simple]
[format.simple.timestamp]
position = "prefix"
format = "iso8601"
|}
    ~sources_toml:(Printf.sprintf {|
[general]
default_time_range = "1h"
reorder_window_ms = 500
[cache]
dir = "%s/cache"
[[source]]
name = "app"
type = "file"
path = "%s/app.log"
format = "simple"
|} test_dir test_dir)
    ~terms:["ERROR"] in
  Alcotest.(check int) "2 ERROR entries" 2 (List.length entries);
  List.iter (fun (e : log_entry) ->
    Alcotest.(check bool) "contains ERROR" true
      (try ignore (Str.search_forward (Str.regexp_string "ERROR") e.raw 0); true
       with Not_found -> false);
    Alcotest.(check bool) "tagged with ERROR" true
      (List.mem "ERROR" e.terms)
  ) entries;
  teardown ()

(* === Test: multi-source merge === *)
let test_multi_source_merge () =
  setup ();
  write_file (Filename.concat test_dir "a.log")
    "2026-04-04T10:00:01Z msg from A first\n\
     2026-04-04T10:00:03Z msg from A second\n\
     2026-04-04T10:00:05Z msg from A third\n";
  write_file (Filename.concat test_dir "b.log")
    "2026-04-04T10:00:02Z msg from B first\n\
     2026-04-04T10:00:04Z msg from B second\n";
  let entries = run_search
    ~formats_toml:{|
[format.simple]
[format.simple.timestamp]
position = "prefix"
format = "iso8601"
|}
    ~sources_toml:(Printf.sprintf {|
[general]
default_time_range = "1h"
reorder_window_ms = 500
[cache]
dir = "%s/cache"
[[source]]
name = "src-a"
type = "file"
path = "%s/a.log"
format = "simple"
[[source]]
name = "src-b"
type = "file"
path = "%s/b.log"
format = "simple"
|} test_dir test_dir test_dir)
    ~terms:[] in
  Alcotest.(check int) "5 entries merged" 5 (List.length entries);
  (* Check chronological order *)
  let sources = List.map (fun (e : log_entry) -> e.source) entries in
  Alcotest.(check (list string)) "interleaved"
    ["src-a"; "src-b"; "src-a"; "src-b"; "src-a"] sources;
  teardown ()

(* === Test: JSON lines format with field extraction === *)
let test_json_lines () =
  setup ();
  write_file (Filename.concat test_dir "json.log")
    ({|{"ts":1712224801000,"level":"info","msg":"started"}|} ^ "\n" ^
     {|{"ts":1712224802000,"level":"error","msg":"crash"}|} ^ "\n" ^
     {|{"ts":1712224803000,"level":"info","msg":"recovered"}|} ^ "\n");
  let entries = run_search
    ~formats_toml:{|
[format.jl]
[format.jl.timestamp]
json_field = "ts"
format = "epoch_ms"
[[format.jl.middleware]]
type = "json_field_extract"
fields = ["level", "msg"]
|}
    ~sources_toml:(Printf.sprintf {|
[general]
default_time_range = "1h"
reorder_window_ms = 500
[cache]
dir = "%s/cache"
[[source]]
name = "json"
type = "file"
path = "%s/json.log"
format = "jl"
|} test_dir test_dir)
    ~terms:["error"] in
  Alcotest.(check int) "1 error entry" 1 (List.length entries);
  let e = List.hd entries in
  Alcotest.(check string) "level extracted" "error" (List.assoc "level" e.metadata);
  Alcotest.(check string) "msg extracted" "crash" (List.assoc "msg" e.metadata);
  teardown ()

(* === Test: multiline join === *)
let test_multiline_join () =
  setup ();
  write_file (Filename.concat test_dir "multi.log")
    "2026-04-04T10:00:01Z ERROR exception raised\n\
     Raised_at Stdlib.failwith in file \"stdlib.ml\"\n\
     Called_from Main.run in file \"main.ml\"\n\
     2026-04-04T10:00:02Z INFO recovered ok\n";
  let entries = run_search
    ~formats_toml:{|
[format.ml]
[format.ml.timestamp]
position = "prefix"
format = "iso8601"
[format.ml.multiline]
continuation = '^\s|^Raised_at|^Called_from'
max_lines = 50
|}
    ~sources_toml:(Printf.sprintf {|
[general]
default_time_range = "1h"
reorder_window_ms = 500
[cache]
dir = "%s/cache"
[[source]]
name = "multi"
type = "file"
path = "%s/multi.log"
format = "ml"
|} test_dir test_dir)
    ~terms:[] in
  Alcotest.(check int) "2 entries (joined)" 2 (List.length entries);
  let e1 = List.hd entries in
  Alcotest.(check bool) "first entry has stack trace" true
    (String.length e1.raw > 50);
  Alcotest.(check bool) "contains Raised_at" true
    (try ignore (Str.search_forward (Str.regexp_string "Raised_at") e1.raw 0); true
     with Not_found -> false);
  teardown ()

(* === Test: syslog timestamp parsing === *)
let test_syslog_format () =
  setup ();
  write_file (Filename.concat test_dir "syslog.log")
    "Apr  4 10:00:01 myhost sshd[1234]: Accepted publickey\n\
     Apr  4 10:00:02 myhost cron[5678]: CRON job started\n";
  let entries = run_search
    ~formats_toml:{|
[format.syslog]
[format.syslog.timestamp]
format = "syslog_bsd"
[[format.syslog.middleware]]
type = "regex_extract"
pattern = '\w+\s+\d+\s+\S+\s+\S+\s+(\w+)\[(\d+)\]:\s+(.*)'
fields = ["program", "pid", "message"]
|}
    ~sources_toml:(Printf.sprintf {|
[general]
default_time_range = "1h"
reorder_window_ms = 500
[cache]
dir = "%s/cache"
[[source]]
name = "sys"
type = "file"
path = "%s/syslog.log"
format = "syslog"
|} test_dir test_dir)
    ~terms:[] in
  Alcotest.(check int) "2 entries" 2 (List.length entries);
  let e1 = List.hd entries in
  Alcotest.(check string) "program=sshd" "sshd" (List.assoc "program" e1.metadata);
  Alcotest.(check string) "pid=1234" "1234" (List.assoc "pid" e1.metadata);
  teardown ()

(* === Test: nginx with common_log timestamp via regex capture === *)
let test_nginx_format () =
  setup ();
  write_file (Filename.concat test_dir "nginx.log")
    "10.0.0.1 - - [04/Apr/2026:10:00:01 +0000] \"GET /api HTTP/1.1\" 200 1234\n\
     10.0.0.2 - - [04/Apr/2026:10:00:02 +0000] \"POST /login HTTP/1.1\" 401 567\n";
  let entries = run_search
    ~formats_toml:{|
[format.nginx]
[format.nginx.timestamp]
regex = '\[(\d{2}/\w{3}/\d{4}:\d{2}:\d{2}:\d{2} [+-]\d{4})\]'
format = "common_log"
[[format.nginx.middleware]]
type = "regex_extract"
pattern = '(\S+)\s+-\s+-\s+\[.*?\]\s+"(\w+)\s+(\S+)\s+HTTP/\S+"\s+(\d+)\s+(\d+)'
fields = ["client_ip", "method", "path", "status", "bytes"]
|}
    ~sources_toml:(Printf.sprintf {|
[general]
default_time_range = "1h"
reorder_window_ms = 500
[cache]
dir = "%s/cache"
[[source]]
name = "nginx"
type = "file"
path = "%s/nginx.log"
format = "nginx"
|} test_dir test_dir)
    ~terms:[] in
  Alcotest.(check int) "2 entries" 2 (List.length entries);
  let e1 = List.hd entries in
  Alcotest.(check string) "client_ip" "10.0.0.1" (List.assoc "client_ip" e1.metadata);
  Alcotest.(check string) "method" "GET" (List.assoc "method" e1.metadata);
  Alcotest.(check string) "status" "200" (List.assoc "status" e1.metadata);
  teardown ()

(* === Test: multi-term search tags correctly === *)
let test_multi_term_tagging () =
  setup ();
  write_file (Filename.concat test_dir "app.log")
    "2026-04-04T10:00:01Z ERROR connection timeout\n\
     2026-04-04T10:00:02Z INFO connection established\n\
     2026-04-04T10:00:03Z ERROR disk full\n\
     2026-04-04T10:00:04Z DEBUG heartbeat\n";
  let entries = run_search
    ~formats_toml:{|
[format.s]
[format.s.timestamp]
position = "prefix"
format = "iso8601"
|}
    ~sources_toml:(Printf.sprintf {|
[general]
default_time_range = "1h"
reorder_window_ms = 500
[cache]
dir = "%s/cache"
[[source]]
name = "app"
type = "file"
path = "%s/app.log"
format = "s"
|} test_dir test_dir)
    ~terms:["ERROR"; "connection"] in
  (* Should match: ERROR connection timeout (both), connection established (connection), ERROR disk full (ERROR) *)
  Alcotest.(check int) "3 matching entries" 3 (List.length entries);
  let e1 = List.hd entries in
  Alcotest.(check bool) "first has both terms" true
    (List.mem "ERROR" e1.terms && List.mem "connection" e1.terms);
  teardown ()

(* === Test: cache warm start === *)
let test_cache_warm_start () =
  setup ();
  write_file (Filename.concat test_dir "app.log")
    "2026-04-04T10:00:01Z line one\n\
     2026-04-04T10:00:02Z line two\n";
  (* First run populates cache *)
  let entries1 = run_search
    ~formats_toml:{|
[format.s]
[format.s.timestamp]
position = "prefix"
format = "iso8601"
|}
    ~sources_toml:(Printf.sprintf {|
[general]
default_time_range = "1h"
reorder_window_ms = 500
[cache]
dir = "%s/cache"
[[source]]
name = "app"
type = "file"
path = "%s/app.log"
format = "s"
|} test_dir test_dir)
    ~terms:[] in
  Alcotest.(check int) "2 entries cold" 2 (List.length entries1);
  (* Second run reads from cache *)
  let entries2 = run_search
    ~formats_toml:{|
[format.s]
[format.s.timestamp]
position = "prefix"
format = "iso8601"
|}
    ~sources_toml:(Printf.sprintf {|
[general]
default_time_range = "1h"
reorder_window_ms = 500
[cache]
dir = "%s/cache"
[[source]]
name = "app"
type = "file"
path = "%s/app.log"
format = "s"
|} test_dir test_dir)
    ~terms:[] in
  Alcotest.(check int) "2 entries warm" 2 (List.length entries2);
  teardown ()

(* === Test: grok pattern matching === *)
let test_grok_pipeline () =
  setup ();
  write_file (Filename.concat test_dir "access.log")
    "2026-04-04T10:00:01Z 192.168.1.1 GET 200\n\
     2026-04-04T10:00:02Z 10.0.0.1 POST 404\n";
  let entries = run_search
    ~formats_toml:{|
[format.grok_test]
[format.grok_test.timestamp]
position = "prefix"
format = "iso8601"
[[format.grok_test.middleware]]
type = "grok"
pattern = '%{WORD:method} %{NUMBER:status}'
|}
    ~sources_toml:(Printf.sprintf {|
[general]
default_time_range = "1h"
reorder_window_ms = 500
[cache]
dir = "%s/cache"
[[source]]
name = "access"
type = "file"
path = "%s/access.log"
format = "grok_test"
|} test_dir test_dir)
    ~terms:[] in
  Alcotest.(check int) "2 entries" 2 (List.length entries);
  (* Grok should extract method and status *)
  let e1 = List.hd entries in
  Alcotest.(check bool) "has fields" true (List.length e1.metadata > 0);
  teardown ()

(* === Test: regex_filter excludes lines === *)
let test_regex_filter () =
  setup ();
  write_file (Filename.concat test_dir "app.log")
    "2026-04-04T10:00:01Z DEBUG trace info\n\
     2026-04-04T10:00:02Z INFO real message\n\
     2026-04-04T10:00:03Z DEBUG more trace\n\
     2026-04-04T10:00:04Z ERROR important\n";
  let entries = run_search
    ~formats_toml:{|
[format.filtered]
[format.filtered.timestamp]
position = "prefix"
format = "iso8601"
[[format.filtered.middleware]]
type = "regex_filter"
exclude = 'DEBUG'
|}
    ~sources_toml:(Printf.sprintf {|
[general]
default_time_range = "1h"
reorder_window_ms = 500
[cache]
dir = "%s/cache"
[[source]]
name = "app"
type = "file"
path = "%s/app.log"
format = "filtered"
|} test_dir test_dir)
    ~terms:[] in
  Alcotest.(check int) "2 entries (DEBUG excluded)" 2 (List.length entries);
  List.iter (fun (e : log_entry) ->
    Alcotest.(check bool) "no DEBUG" false
      (try ignore (Str.search_forward (Str.regexp_string "DEBUG") e.raw 0); true
       with Not_found -> false)
  ) entries;
  teardown ()

(* === Test: field_rename === *)
let test_field_rename () =
  setup ();
  write_file (Filename.concat test_dir "app.log")
    ({|{"ts":1712224801000,"lvl":"info","msg":"hello"}|} ^ "\n");
  let entries = run_search
    ~formats_toml:{|
[format.renamed]
[format.renamed.timestamp]
json_field = "ts"
format = "epoch_ms"
[[format.renamed.middleware]]
type = "json_field_extract"
fields = ["lvl", "msg"]
[[format.renamed.middleware]]
type = "field_rename"
mapping = { "lvl" = "level", "msg" = "message" }
|}
    ~sources_toml:(Printf.sprintf {|
[general]
default_time_range = "1h"
reorder_window_ms = 500
[cache]
dir = "%s/cache"
[[source]]
name = "app"
type = "file"
path = "%s/app.log"
format = "renamed"
|} test_dir test_dir)
    ~terms:[] in
  Alcotest.(check int) "1 entry" 1 (List.length entries);
  let e = List.hd entries in
  Alcotest.(check string) "level renamed" "info" (List.assoc "level" e.metadata);
  Alcotest.(check string) "message renamed" "hello" (List.assoc "message" e.metadata);
  teardown ()

(* === Test: term management via search API === *)
let test_term_management () =
  setup ();
  write_file (Filename.concat test_dir "app.log")
    "2026-04-04T10:00:01Z line one\n";
  write_file (Filename.concat test_dir "formats.toml") {|
[format.s]
[format.s.timestamp]
position = "prefix"
format = "iso8601"
|};
  write_file (Filename.concat test_dir "sources.toml")
    (Printf.sprintf {|
[general]
default_time_range = "1h"
reorder_window_ms = 500
[cache]
dir = "%s/cache"
[[source]]
name = "app"
type = "file"
path = "%s/app.log"
format = "s"
|} test_dir test_dir);
  let formats = Weft_config.parse_formats_file
    (Filename.concat test_dir "formats.toml") in
  let sources = Weft_config.parse_sources_file
    (Filename.concat test_dir "sources.toml") in
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let cache_dir = Filename.concat test_dir "cache" in
  (try Unix.mkdir cache_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let cache_config = { dir = cache_dir; max_mb_per_source = 100;
                       default_ttl_hours = 72 } in
  let cache = Weft_cache.create ~fs cache_config in
  List.iter (fun (src : source_config) ->
    ignore (Weft_cache.init_source cache ~source_name:src.name ~format:src.format)
  ) sources.sources;
  let search = Weft_search.create ~cache
    ~sources:sources.sources ~formats ~general:sources.general () in
  (* Add term *)
  let t1 = Weft_search.add_term search "foo" in
  Alcotest.(check bool) "add foo" true (t1 <> None);
  let t2 = Weft_search.add_term search "foo" in
  Alcotest.(check bool) "dup foo" true (t2 = None);
  ignore (Weft_search.add_term search "bar");
  Alcotest.(check int) "2 terms" 2
    (List.length (Weft_search.all_terms search));
  Weft_search.toggle_term search "foo";
  Alcotest.(check int) "1 enabled" 1
    (List.length (Weft_search.enabled_terms search));
  Weft_search.remove_term search "foo";
  Alcotest.(check int) "1 term left" 1
    (List.length (Weft_search.all_terms search));
  teardown ()

(* === Test: Loki response parsing === *)
let test_loki_response_parsing () =
  let json = {|{
    "data": {
      "result": [{
        "stream": {"job": "test", "namespace": "prod"},
        "values": [
          ["1712224801000000000", "first log line"],
          ["1712224802000000000", "second log line"]
        ]
      }]
    }
  }|} in
  let entries = Weft_source.Loki.parse_query_response ~source:"test-loki" json in
  Alcotest.(check int) "2 entries" 2 (List.length entries);
  let e1 = List.hd entries in
  Alcotest.(check string) "source" "test-loki" e1.source;
  Alcotest.(check string) "raw" "first log line" e1.raw;
  Alcotest.(check bool) "has labels" true (List.mem_assoc "labels" e1.metadata)

let () =
  Alcotest.run "weft_functional" [
    "pipeline", [
      Alcotest.test_case "iso8601 + regex" `Quick test_iso8601_pipeline;
      Alcotest.test_case "term filtering" `Quick test_term_filtering;
      Alcotest.test_case "multi-source merge" `Quick test_multi_source_merge;
      Alcotest.test_case "json lines" `Quick test_json_lines;
      Alcotest.test_case "multiline join" `Quick test_multiline_join;
      Alcotest.test_case "syslog format" `Quick test_syslog_format;
      Alcotest.test_case "nginx format" `Quick test_nginx_format;
      Alcotest.test_case "multi-term tagging" `Quick test_multi_term_tagging;
      Alcotest.test_case "grok pattern" `Quick test_grok_pipeline;
      Alcotest.test_case "regex filter" `Quick test_regex_filter;
      Alcotest.test_case "field rename" `Quick test_field_rename;
    ];
    "cache", [
      Alcotest.test_case "warm start" `Quick test_cache_warm_start;
    ];
    "components", [
      Alcotest.test_case "term management" `Quick test_term_management;
      Alcotest.test_case "loki response" `Quick test_loki_response_parsing;
    ];
  ]
