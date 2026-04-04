(* Log generator for testing weft end-to-end.

   Generates log files in multiple formats:
   - syslog BSD format
   - JSON lines (structured)
   - OCaml app with multiline stack traces
   - Nginx access log

   Usage:
     gen_logs --dir /tmp/weft-test          # generate static files + config
     gen_logs --dir /tmp/weft-test --live   # keep appending (for tail testing)
*)

let () = Random.self_init ()

(* --- Time helpers --- *)

let now_epoch () = Unix.gettimeofday ()

let epoch_to_iso8601 t =
  let tm = Unix.gmtime t in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d.%03dZ"
    (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
    (Random.int 1000)

let epoch_to_syslog t =
  let tm = Unix.gmtime t in
  let months = [|"Jan";"Feb";"Mar";"Apr";"May";"Jun";
                 "Jul";"Aug";"Sep";"Oct";"Nov";"Dec"|] in
  Printf.sprintf "%s %2d %02d:%02d:%02d"
    months.(tm.Unix.tm_mon) tm.Unix.tm_mday
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

let epoch_to_clf t =
  let tm = Unix.gmtime t in
  let months = [|"Jan";"Feb";"Mar";"Apr";"May";"Jun";
                 "Jul";"Aug";"Sep";"Oct";"Nov";"Dec"|] in
  Printf.sprintf "%02d/%s/%04d:%02d:%02d:%02d +0000"
    tm.Unix.tm_mday months.(tm.Unix.tm_mon) (tm.Unix.tm_year + 1900)
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

(* --- Random data --- *)

let pick arr = arr.(Random.int (Array.length arr))

let levels = [|"DEBUG"; "INFO"; "WARN"; "ERROR"|]
let level_weights = [|40; 35; 15; 10|]

let weighted_level () =
  let total = Array.fold_left (+) 0 level_weights in
  let r = Random.int total in
  let rec find i acc =
    let acc = acc + level_weights.(i) in
    if r < acc then levels.(i) else find (i + 1) acc
  in
  find 0 0

let modules = [|"Auth"; "Db.Pool"; "Http.Router"; "Cache"; "Worker"; "Gc"; "Metrics"|]
let programs = [|"sshd"; "cron"; "systemd"; "nginx"; "postfix"; "kernel"|]
let ips = [|"192.168.1.10"; "10.0.0.42"; "172.16.0.5"; "203.0.113.7"; "198.51.100.22"|]
let methods = [|"GET"; "POST"; "PUT"; "DELETE"; "PATCH"|]
let paths = [|"/api/v1/users"; "/api/v1/orders"; "/healthcheck"; "/login";
              "/api/v1/search"; "/static/app.js"; "/api/v1/events"|]
let statuses = [|200; 200; 200; 200; 201; 204; 301; 400; 403; 404; 500; 502; 503|]
let user_agents = [|"Mozilla/5.0"; "curl/7.88"; "Go-http-client/1.1";
                    "python-requests/2.28"; "okhttp/4.10"|]

let syslog_messages = [|
  "Accepted publickey for admin from %s port %d ssh2";
  "session opened for user admin";
  "session closed for user admin";
  "Connection from %s port %d";
  "pam_unix(sshd:auth): authentication failure; logname= uid=0";
  "CRON[%d]: (root) CMD (/usr/bin/apt-get update)";
  "Started Daily apt download activities";
  "Finished Daily apt download activities";
  "Out of memory: Killed process %d (java)";
  "TCP: request_sock_TCP: Possible SYN flooding on port %d";
|]

let ocaml_messages = [|
  "Starting request handler";
  "Database connection pool initialized: size=%d";
  "Cache miss for key=%s, fetching from origin";
  "Request completed: method=%s path=%s status=%d latency_ms=%d";
  "Worker heartbeat: queue_depth=%d active=%d";
  "GC stats: minor=%d major=%d compactions=%d";
  "Connection reset by peer: %s:%d";
  "Retrying operation: attempt %d/3";
  "Rate limit exceeded for client %s";
  "TLS handshake completed: %s";
|]

let json_messages = [|
  "request handled";
  "database query completed";
  "cache updated";
  "authentication succeeded";
  "authentication failed";
  "background job started";
  "background job completed";
  "connection established";
  "connection closed";
  "rate limit applied";
|]

let ocaml_exceptions = [|
  ("Failure", "connection_timeout", [|
    "Raised at Stdlib.failwith in file \"stdlib.ml\", line 29";
    "Called_from Db.Pool.acquire in file \"lib/db/pool.ml\", line 142";
    "Called_from Http.Handler.with_db in file \"lib/http/handler.ml\", line 87";
    "Called_from Http.Router.dispatch in file \"lib/http/router.ml\", line 203";
  |]);
  ("Not_found", "", [|
    "Raised at Stdlib__Hashtbl.find in file \"hashtbl.ml\", line 96";
    "Called_from Cache.lookup in file \"lib/cache.ml\", line 55";
    "Called_from Http.Handler.get_user in file \"lib/http/handler.ml\", line 112";
  |]);
  ("Unix.Unix_error", "ECONNRESET", [|
    "Raised at Unix.connect in file \"unix.ml\", line 401";
    "Called_from Eio_unix.Net.connect in file \"lib_eio/unix/net.ml\", line 78";
    "Called_from Http.Client.request in file \"lib/http/client.ml\", line 34";
    "Called_from Worker.fetch_upstream in file \"lib/worker.ml\", line 201";
    "Called_from Worker.run_loop in file \"lib/worker.ml\", line 189";
  |]);
  ("Out_of_memory", "", [|
    "Raised at Gc.compact in file \"gc.ml\", line 12";
    "Called_from Main.handle_signal in file \"bin/main.ml\", line 45";
  |]);
|]

(* --- Generators --- *)

let gen_syslog_line t =
  let prog = pick programs in
  let pid = 1000 + Random.int 30000 in
  let msg_template = pick syslog_messages in
  let msg =
    if String.contains msg_template '%' then
      (* Simple format string filling *)
      let ip = pick ips in
      let port = 10000 + Random.int 55000 in
      let filled = ref msg_template in
      (try
         let i = String.index !filled '%' in
         if i + 1 < String.length !filled then
           match (!filled).[i + 1] with
           | 's' ->
             filled := String.sub !filled 0 i ^ ip ^
               String.sub !filled (i + 2) (String.length !filled - i - 2)
           | 'd' ->
             filled := String.sub !filled 0 i ^ string_of_int port ^
               String.sub !filled (i + 2) (String.length !filled - i - 2)
           | _ -> ()
       with Not_found -> ());
      (* Second pass for remaining % *)
      (try
         let i = String.index !filled '%' in
         if i + 1 < String.length !filled then
           match (!filled).[i + 1] with
           | 's' ->
             filled := String.sub !filled 0 i ^ ip ^
               String.sub !filled (i + 2) (String.length !filled - i - 2)
           | 'd' ->
             filled := String.sub !filled 0 i ^ string_of_int port ^
               String.sub !filled (i + 2) (String.length !filled - i - 2)
           | _ -> ()
       with Not_found -> ());
      !filled
    else msg_template
  in
  Printf.sprintf "%s myhost %s[%d]: %s" (epoch_to_syslog t) prog pid msg

let gen_json_line t =
  let level = String.lowercase_ascii (weighted_level ()) in
  let msg = pick json_messages in
  let trace_id = Printf.sprintf "%08x%08x" (Random.bits ()) (Random.bits ()) in
  let latency = Random.int 500 in
  Printf.sprintf {|{"ts":%.3f,"level":"%s","msg":"%s","trace_id":"%s","latency_ms":%d}|}
    (t *. 1000.0) level msg trace_id latency

let gen_ocaml_line t ~include_exception =
  let level = weighted_level () in
  let mod_ = pick modules in
  let msg_template = pick ocaml_messages in
  let msg =
    if String.contains msg_template '%' then begin
      let ip = pick ips in
      let n = Random.int 1000 in
      let method_ = pick methods in
      let path = pick paths in
      let status = pick statuses in
      let filled = ref msg_template in
      let replace_next () =
        try
          let i = String.index !filled '%' in
          if i + 1 < String.length !filled then
            match (!filled).[i + 1] with
            | 's' ->
              let replacement = match Random.int 3 with
                | 0 -> ip | 1 -> method_ | _ -> path in
              filled := String.sub !filled 0 i ^ replacement ^
                String.sub !filled (i + 2) (String.length !filled - i - 2)
            | 'd' ->
              let replacement = match Random.int 3 with
                | 0 -> string_of_int n | 1 -> string_of_int status
                | _ -> string_of_int (1 + Random.int 200) in
              filled := String.sub !filled 0 i ^ replacement ^
                String.sub !filled (i + 2) (String.length !filled - i - 2)
            | _ -> ()
        with Not_found -> ()
      in
      replace_next (); replace_next (); replace_next (); replace_next ();
      !filled
    end else msg_template
  in
  let base = Printf.sprintf "%s %s [%s]: %s" (epoch_to_iso8601 t) level mod_ msg in
  if include_exception && level = "ERROR" then begin
    let (exc_name, exc_arg, trace) = pick ocaml_exceptions in
    let exc_line = if exc_arg = "" then
      Printf.sprintf "%s %s [%s]: Exception: %s" (epoch_to_iso8601 t) level mod_ exc_name
    else
      Printf.sprintf "%s %s [%s]: Exception: %s(\"%s\")" (epoch_to_iso8601 t) level mod_ exc_name exc_arg
    in
    let trace_lines = Array.to_list trace in
    exc_line :: trace_lines
  end else
    [base]

let gen_nginx_line t =
  let ip = pick ips in
  let method_ = pick methods in
  let path = pick paths in
  let status = pick statuses in
  let bytes = 100 + Random.int 50000 in
  let latency = Printf.sprintf "%.3f" (Random.float 2.0) in
  let ua = pick user_agents in
  Printf.sprintf "%s - - [%s] \"%s %s HTTP/1.1\" %d %d \"-\" \"%s\" %s"
    ip (epoch_to_clf t) method_ path status bytes ua latency

(* --- File generation --- *)

let generate_batch ~dir ~count ~start_time =
  let syslog_path = Filename.concat dir "syslog.log" in
  let json_path = Filename.concat dir "app-json.log" in
  let ocaml_path = Filename.concat dir "app-ocaml.log" in
  let nginx_path = Filename.concat dir "nginx-access.log" in

  let syslog_oc = open_out syslog_path in
  let json_oc = open_out json_path in
  let ocaml_oc = open_out ocaml_path in
  let nginx_oc = open_out nginx_path in

  let t = ref start_time in
  for _ = 1 to count do
    (* Advance time by 0.1-5 seconds *)
    t := !t +. 0.1 +. Random.float 4.9;

    (* Syslog: ~60% of lines *)
    if Random.int 100 < 60 then
      Printf.fprintf syslog_oc "%s\n" (gen_syslog_line !t);

    (* JSON: ~70% of lines *)
    if Random.int 100 < 70 then
      Printf.fprintf json_oc "%s\n" (gen_json_line !t);

    (* OCaml app: ~50%, with 10% chance of exception *)
    if Random.int 100 < 50 then begin
      let include_exception = Random.int 100 < 10 in
      let lines = gen_ocaml_line !t ~include_exception in
      List.iter (fun l -> Printf.fprintf ocaml_oc "%s\n" l) lines
    end;

    (* Nginx: ~80% *)
    if Random.int 100 < 80 then
      Printf.fprintf nginx_oc "%s\n" (gen_nginx_line !t);
  done;

  close_out syslog_oc;
  close_out json_oc;
  close_out ocaml_oc;
  close_out nginx_oc;

  Printf.printf "Generated %d events across 4 log files in %s\n" count dir;
  Printf.printf "  %s\n  %s\n  %s\n  %s\n"
    syslog_path json_path ocaml_path nginx_path

let generate_config ~dir =
  let formats_path = Filename.concat dir "formats.toml" in
  let sources_path = Filename.concat dir "sources.toml" in

  let formats_oc = open_out formats_path in
  Printf.fprintf formats_oc {|# Generated by gen_logs

[format.syslog]
[format.syslog.timestamp]
format = "syslog_bsd"

[[format.syslog.middleware]]
type = "regex_extract"
pattern = '\w+\s+\d+\s+\S+\s+\S+\s+(\w+)\[(\d+)\]: (.*)'
fields = ["program", "pid", "message"]


[format.json_lines]
[format.json_lines.timestamp]
json_field = "ts"
format = "epoch_ms"

[[format.json_lines.middleware]]
type = "json_field_extract"
fields = ["level", "msg", "trace_id", "latency_ms"]


[format.ocaml_app]
[format.ocaml_app.timestamp]
position = "prefix"
format = "iso8601"

[format.ocaml_app.multiline]
continuation = '^\s|^Raised_at|^Called_from'
max_lines = 50

[[format.ocaml_app.middleware]]
type = "regex_extract"
pattern = '\S+\s+(\w+)\s+\[(\w[\w.]*)\]:\s+(.*)'
fields = ["level", "module", "message"]


[format.nginx_access]
[format.nginx_access.timestamp]
regex = '\[(\d{2}/\w{3}/\d{4}:\d{2}:\d{2}:\d{2} [+-]\d{4})\]'
format = "common_log"

[[format.nginx_access.middleware]]
type = "regex_extract"
pattern = '(\S+)\s+-\s+-\s+\[.*?\]\s+"(\w+)\s+(\S+)\s+HTTP/\S+"\s+(\d+)\s+(\d+)'
fields = ["client_ip", "method", "path", "status", "bytes"]
|};
  close_out formats_oc;

  let sources_oc = open_out sources_path in
  Printf.fprintf sources_oc {|# Generated by gen_logs

[general]
default_time_range = "1h"
reorder_window_ms = 500

[limits]
max_ssh_connections = 4
max_loki_concurrent = 2
catch_up_timeout_sec = 30

[cache]
dir = "%s/cache"
max_mb_per_source = 200
default_ttl_hours = 72

[[source]]
name = "syslog"
type = "file"
path = "%s/syslog.log"
format = "syslog"

[[source]]
name = "app-json"
type = "file"
path = "%s/app-json.log"
format = "json_lines"

[[source]]
name = "app-ocaml"
type = "file"
path = "%s/app-ocaml.log"
format = "ocaml_app"

[[source]]
name = "nginx"
type = "file"
path = "%s/nginx-access.log"
format = "nginx_access"
|} dir dir dir dir dir;
  close_out sources_oc;

  Printf.printf "Generated config:\n  %s\n  %s\n" formats_path sources_path

let live_append ~dir ~interval_ms =
  let syslog_path = Filename.concat dir "syslog.log" in
  let json_path = Filename.concat dir "app-json.log" in
  let ocaml_path = Filename.concat dir "app-ocaml.log" in
  let nginx_path = Filename.concat dir "nginx-access.log" in

  let syslog_oc = open_out_gen [Open_append; Open_creat] 0o644 syslog_path in
  let json_oc = open_out_gen [Open_append; Open_creat] 0o644 json_path in
  let ocaml_oc = open_out_gen [Open_append; Open_creat] 0o644 ocaml_path in
  let nginx_oc = open_out_gen [Open_append; Open_creat] 0o644 nginx_path in

  Printf.printf "Live mode: appending to logs every %dms (Ctrl-C to stop)\n%!" interval_ms;

  let running = ref true in
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> running := false));

  while !running do
    let t = now_epoch () in

    if Random.int 100 < 60 then begin
      Printf.fprintf syslog_oc "%s\n%!" (gen_syslog_line t)
    end;
    if Random.int 100 < 70 then begin
      Printf.fprintf json_oc "%s\n%!" (gen_json_line t)
    end;
    if Random.int 100 < 50 then begin
      let lines = gen_ocaml_line t ~include_exception:(Random.int 100 < 10) in
      List.iter (fun l -> Printf.fprintf ocaml_oc "%s\n%!" l) lines
    end;
    if Random.int 100 < 80 then begin
      Printf.fprintf nginx_oc "%s\n%!" (gen_nginx_line t)
    end;

    Unix.sleepf (float_of_int interval_ms /. 1000.0);
  done;

  close_out syslog_oc;
  close_out json_oc;
  close_out ocaml_oc;
  close_out nginx_oc;
  Printf.printf "\nStopped.\n"

let () =
  let dir = ref "/tmp/weft-test" in
  let count = ref 1000 in
  let live = ref false in
  let live_interval = ref 200 in
  let args = Array.to_list Sys.argv |> List.tl in
  let rec parse = function
    | [] -> ()
    | "--dir" :: d :: rest -> dir := d; parse rest
    | "--count" :: n :: rest -> count := int_of_string n; parse rest
    | "--live" :: rest -> live := true; parse rest
    | "--interval" :: n :: rest -> live_interval := int_of_string n; parse rest
    | "--help" :: _ | "-h" :: _ ->
      Printf.printf "gen_logs — generate sample log files for weft testing\n\n";
      Printf.printf "Usage: gen_logs [OPTIONS]\n\n";
      Printf.printf "Options:\n";
      Printf.printf "  --dir <path>       Output directory (default: /tmp/weft-test)\n";
      Printf.printf "  --count <n>        Number of events to generate (default: 1000)\n";
      Printf.printf "  --live             Keep appending after initial generation\n";
      Printf.printf "  --interval <ms>    Live mode interval in ms (default: 200)\n";
      Printf.printf "  -h, --help         Show this help\n\n";
      Printf.printf "Output:\n";
      Printf.printf "  <dir>/syslog.log        Syslog BSD format\n";
      Printf.printf "  <dir>/app-json.log      JSON lines (structured)\n";
      Printf.printf "  <dir>/app-ocaml.log     OCaml app (multiline stack traces)\n";
      Printf.printf "  <dir>/nginx-access.log  Nginx combined access log\n";
      Printf.printf "  <dir>/formats.toml      Format definitions for weft\n";
      Printf.printf "  <dir>/sources.toml      Source config pointing to generated files\n";
      exit 0
    | x :: _ ->
      Printf.eprintf "Unknown argument: %s\n" x;
      exit 1
  in
  parse args;

  (* Create output directory *)
  (try Unix.mkdir !dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());

  let start_time = now_epoch () -. (float_of_int !count *. 2.5) in
  generate_batch ~dir:!dir ~count:!count ~start_time;
  generate_config ~dir:!dir;

  Printf.printf "\nTo test weft:\n";
  Printf.printf "  weft --formats %s/formats.toml --sources %s/sources.toml -s connection -s ERROR\n"
    !dir !dir;

  if !live then
    live_append ~dir:!dir ~interval_ms:!live_interval
