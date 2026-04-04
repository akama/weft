(* Log generator — simulates a realistic microservice architecture.

   Architecture:
     client -> nginx (reverse proxy) -> api-gateway (OCaml) -> worker-svc (JSON)
                                                             -> auth-svc (JSON)

   A request arrives at nginx, gets a trace_id, flows through the API gateway
   which calls downstream services. The same trace_id appears in all logs.
   Some requests fail, producing errors and stack traces.

   Sources:
   - nginx-access.log    Combined access log with X-Trace-Id
   - api-gateway.log     OCaml app with multiline stack traces
   - worker-svc.log      JSON lines structured logging
   - auth-svc.log        JSON lines structured logging
   - syslog.log          System-level background noise
*)

let () = Random.self_init ()

(* --- Trace ID generation --- *)
let gen_trace_id () =
  Printf.sprintf "%08x%08x%08x%08x"
    (Random.bits ()) (Random.bits ()) (Random.bits ()) (Random.bits ())

let gen_span_id () =
  Printf.sprintf "%016x" (Random.bits () lor (Random.bits () lsl 30))

(* --- Time helpers --- *)
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

let client_ips = [|"192.168.1.10"; "10.0.0.42"; "172.16.0.5";
                   "203.0.113.7"; "198.51.100.22"; "10.0.1.15";
                   "192.168.2.30"; "172.16.1.100"|]
let methods_weights = [|("GET", 50); ("POST", 25); ("PUT", 10);
                        ("DELETE", 8); ("PATCH", 7)|]
let api_paths = [|
  "/api/v1/users"; "/api/v1/users/:id"; "/api/v1/orders";
  "/api/v1/orders/:id"; "/api/v1/search"; "/api/v1/events";
  "/api/v1/auth/login"; "/api/v1/auth/refresh"; "/api/v1/payments";
  "/api/v1/notifications"; "/healthcheck"; "/api/v1/uploads";
|]
let user_agents = [|
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36";
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) rv:109.0";
  "curl/7.88.1"; "Go-http-client/2.0"; "python-requests/2.31.0";
  "okhttp/4.12.0"; "PostmanRuntime/7.36.0";
|]
let _gateway_modules = [|"Http.Router"; "Http.Handler"; "Http.Middleware";
                        "Auth.Verify"; "Auth.Session"; "Db.Pool"; "Db.Query";
                        "Cache.Redis"; "Ratelimit"|]
let user_ids = [|"usr_a1b2c3"; "usr_d4e5f6"; "usr_789abc"; "usr_def012";
                 "usr_345678"; "usr_9abcde"; "usr_f01234"|]
let order_ids = [|"ord_100"; "ord_101"; "ord_102"; "ord_103"; "ord_200"; "ord_201"|]

let weighted_method () =
  let total = Array.fold_left (fun acc (_, w) -> acc + w) 0 methods_weights in
  let r = Random.int total in
  let rec find i acc =
    let (m, w) = methods_weights.(i) in
    let acc = acc + w in
    if r < acc then m else find (i + 1) acc
  in
  find 0 0

(* --- Request simulation --- *)

type request_outcome =
  | Success of int    (* status code *)
  | ClientError of int
  | ServerError of int * string option  (* status, optional exception *)

let decide_outcome () =
  let r = Random.int 100 in
  if r < 70 then Success (pick [|200; 200; 200; 201; 204|])
  else if r < 85 then ClientError (pick [|400; 401; 403; 404; 422|])
  else ServerError (pick [|500; 502; 503; 504|],
    if Random.int 100 < 40 then
      Some (pick [|"connection_timeout"; "ECONNRESET"; "pool_exhausted";
                   "query_timeout"; "redis_unavailable"|])
    else None)

let exceptions_for_error = function
  | "connection_timeout" -> [
      "Raised at Stdlib.failwith in file \"stdlib.ml\", line 29";
      "Called_from Db.Pool.acquire in file \"lib/db/pool.ml\", line 142";
      "Called_from Db.Query.execute in file \"lib/db/query.ml\", line 87";
      "Called_from Http.Handler.with_db in file \"lib/http/handler.ml\", line 63";
    ]
  | "ECONNRESET" -> [
      "Raised at Unix.connect in file \"unix.ml\", line 401";
      "Called_from Eio_unix.Net.connect in file \"lib_eio/unix/net.ml\", line 78";
      "Called_from Http.Client.request in file \"lib/http/client.ml\", line 34";
      "Called_from Worker.call_downstream in file \"lib/worker.ml\", line 201";
    ]
  | "pool_exhausted" -> [
      "Raised at Db.Pool.acquire in file \"lib/db/pool.ml\", line 155";
      "Called_from Db.Query.execute in file \"lib/db/query.ml\", line 87";
      "Called_from Http.Handler.list_orders in file \"lib/http/handler.ml\", line 178";
    ]
  | "query_timeout" -> [
      "Raised at Db.Query.execute in file \"lib/db/query.ml\", line 112";
      "Called_from Http.Handler.search in file \"lib/http/handler.ml\", line 204";
      "Called_from Http.Router.dispatch in file \"lib/http/router.ml\", line 67";
    ]
  | "redis_unavailable" -> [
      "Raised at Cache.Redis.get in file \"lib/cache/redis.ml\", line 45";
      "Called_from Cache.Redis.with_connection in file \"lib/cache/redis.ml\", line 32";
      "Called_from Http.Middleware.cache_lookup in file \"lib/http/middleware.ml\", line 89";
    ]
  | _ -> [
      "Raised at Stdlib.failwith in file \"stdlib.ml\", line 29";
      "Called_from Http.Router.dispatch in file \"lib/http/router.ml\", line 67";
    ]

(* Simulate one full request flowing through the system *)
type log_line = { time : float; file : string; content : string }

let simulate_request ~base_time =
  let trace_id = gen_trace_id () in
  let client_ip = pick client_ips in
  let method_ = weighted_method () in
  let path = pick api_paths in
  let ua = pick user_agents in
  let outcome = decide_outcome () in

  let lines = ref [] in
  let add file time content = lines := { time; file; content } :: !lines in
  let t = ref base_time in
  let advance_ms lo hi = t := !t +. (float_of_int (lo + Random.int (hi - lo))) /. 1000.0 in

  let status = match outcome with
    | Success s -> s | ClientError s -> s | ServerError (s, _) -> s in

  (* 1. Nginx receives request *)
  let nginx_line =
    let bytes = 100 + Random.int 50000 in
    let total_latency_ms = 5 + Random.int 500 in
    let latency_s = Printf.sprintf "%.3f" (float_of_int total_latency_ms /. 1000.0) in
    Printf.sprintf "%s - - [%s] \"%s %s HTTP/1.1\" %d %d \"%s\" \"%s\" \"%s\" %s"
      client_ip (epoch_to_clf !t) method_ path status bytes
      trace_id ua trace_id latency_s
  in

  (* 2. API gateway receives request *)
  advance_ms 1 5;
  let gw_span = gen_span_id () in
  add "api-gateway"  !t
    (Printf.sprintf "%s INFO [Http.Router]: --> %s %s trace_id=%s span_id=%s client=%s"
       (epoch_to_iso8601 !t) method_ path trace_id gw_span client_ip);

  (* 2a. Auth check *)
  if path <> "/healthcheck" then begin
    advance_ms 1 10;
    let auth_span = gen_span_id () in
    let user_id = pick user_ids in
    add "auth-svc" !t
      (Printf.sprintf {|{"ts":%.3f,"level":"info","msg":"auth check","trace_id":"%s","span_id":"%s","parent_span":"%s","user_id":"%s","method":"%s","path":"%s"}|}
         (!t *. 1000.0) trace_id auth_span gw_span user_id method_ path);
    advance_ms 2 20;
    match outcome with
    | ClientError 401 | ClientError 403 ->
      add "auth-svc" !t
        (Printf.sprintf {|{"ts":%.3f,"level":"warn","msg":"auth rejected","trace_id":"%s","span_id":"%s","user_id":"%s","reason":"insufficient_permissions"}|}
           (!t *. 1000.0) trace_id auth_span user_id);
      add "api-gateway" !t
        (Printf.sprintf "%s WARN [Auth.Verify]: auth rejected for %s on %s %s trace_id=%s"
           (epoch_to_iso8601 !t) user_id method_ path trace_id)
    | _ ->
      add "auth-svc" !t
        (Printf.sprintf {|{"ts":%.3f,"level":"info","msg":"auth ok","trace_id":"%s","span_id":"%s","user_id":"%s"}|}
           (!t *. 1000.0) trace_id auth_span user_id);
  end;

  (* 2b. Business logic + downstream calls *)
  (match outcome with
   | ClientError _ -> ()  (* short-circuit *)
   | _ ->
     advance_ms 2 15;
     let worker_span = gen_span_id () in

     (* Worker service processes *)
     add "worker-svc" !t
       (Printf.sprintf {|{"ts":%.3f,"level":"info","msg":"processing request","trace_id":"%s","span_id":"%s","parent_span":"%s","method":"%s","path":"%s"}|}
          (!t *. 1000.0) trace_id worker_span gw_span method_ path);

     (* Simulate DB queries *)
     advance_ms 5 50;
     let query_target = match path with
       | "/api/v1/users" | "/api/v1/users/:id" -> "users"
       | "/api/v1/orders" | "/api/v1/orders/:id" -> "orders"
       | "/api/v1/search" -> "search_index"
       | "/api/v1/payments" -> "payments"
       | _ -> "default"
     in
     let rows = Random.int 100 in
     add "worker-svc" !t
       (Printf.sprintf {|{"ts":%.3f,"level":"debug","msg":"db query","trace_id":"%s","span_id":"%s","table":"%s","rows":%d,"duration_ms":%d}|}
          (!t *. 1000.0) trace_id worker_span query_target rows (5 + Random.int 200));

     (* Maybe cache interaction *)
     if Random.int 100 < 60 then begin
       advance_ms 1 5;
       let cache_hit = Random.int 100 < 70 in
       add "api-gateway" !t
         (Printf.sprintf "%s DEBUG [Cache.Redis]: %s key=%s:%s trace_id=%s"
            (epoch_to_iso8601 !t)
            (if cache_hit then "HIT" else "MISS")
            query_target (pick order_ids) trace_id)
     end;

     advance_ms 5 30;
     (match outcome with
      | ServerError (_, Some exc_type) ->
        (* Worker reports error *)
        add "worker-svc" !t
          (Printf.sprintf {|{"ts":%.3f,"level":"error","msg":"request failed","trace_id":"%s","span_id":"%s","error":"%s","path":"%s"}|}
             (!t *. 1000.0) trace_id worker_span exc_type path);

        (* Gateway gets the exception with stack trace *)
        advance_ms 1 3;
        add "api-gateway" !t
          (Printf.sprintf "%s ERROR [Http.Handler]: request failed trace_id=%s error=%s"
             (epoch_to_iso8601 !t) trace_id exc_type);
        let trace_lines = exceptions_for_error exc_type in
        List.iter (fun l -> add "api-gateway" !t l) trace_lines

      | ServerError (_, None) ->
        add "worker-svc" !t
          (Printf.sprintf {|{"ts":%.3f,"level":"error","msg":"internal error","trace_id":"%s","span_id":"%s","path":"%s"}|}
             (!t *. 1000.0) trace_id worker_span path);
        advance_ms 1 3;
        add "api-gateway" !t
          (Printf.sprintf "%s ERROR [Http.Handler]: downstream error trace_id=%s status=%d"
             (epoch_to_iso8601 !t) trace_id status)

      | Success _ ->
        add "worker-svc" !t
          (Printf.sprintf {|{"ts":%.3f,"level":"info","msg":"request completed","trace_id":"%s","span_id":"%s","path":"%s","status":%d}|}
             (!t *. 1000.0) trace_id worker_span path status)
      | ClientError _ -> ()
     )
  );

  (* 3. Gateway response *)
  advance_ms 1 5;
  let level = match outcome with
    | Success _ -> "INFO" | ClientError _ -> "WARN" | ServerError _ -> "ERROR" in
  add "api-gateway" !t
    (Printf.sprintf "%s %s [Http.Router]: <-- %s %s %d trace_id=%s latency_ms=%d"
       (epoch_to_iso8601 !t) level method_ path status trace_id
       (int_of_float ((!t -. base_time) *. 1000.0)));

  (* Nginx line gets the final timestamp *)
  add "nginx" base_time nginx_line;

  List.rev !lines

(* --- Syslog background noise (not correlated) --- *)
let syslog_programs = [|"sshd"; "cron"; "systemd"; "dockerd"; "kubelet"; "kernel"|]

let gen_syslog_line t =
  let prog = pick syslog_programs in
  let pid = 1000 + Random.int 30000 in
  let msgs = [|
    Printf.sprintf "Accepted publickey for deploy from %s port %d ssh2"
      (pick client_ips) (10000 + Random.int 55000);
    "session opened for user deploy";
    "session closed for user deploy";
    Printf.sprintf "Connection from %s port %d"
      (pick client_ips) (10000 + Random.int 55000);
    Printf.sprintf "CRON[%d]: (root) CMD (/usr/local/bin/health-check)" (Random.int 50000);
    Printf.sprintf "Container %s started" (pick [|"api-gw-7f8b9c"; "worker-3d4e5f";
      "auth-2a3b4c"; "redis-1x2y3z"; "postgres-9a8b7c"|]);
    Printf.sprintf "Container %s health check: healthy" (pick [|"api-gw-7f8b9c";
      "worker-3d4e5f"; "auth-2a3b4c"|]);
    Printf.sprintf "OOM kill: process %d (%s) score %d"
      (Random.int 50000) (pick [|"java"; "node"; "python"|]) (Random.int 1000);
    Printf.sprintf "TCP: request_sock_TCP: Possible SYN flooding on port %d"
      (pick [|80; 443; 8080; 5432|]);
    Printf.sprintf "kernel: [%d.%06d] eth0: link up" (Random.int 10000) (Random.int 999999);
  |] in
  Printf.sprintf "%s app-server-1 %s[%d]: %s"
    (epoch_to_syslog t) prog pid (pick msgs)

(* --- File writing --- *)

let write_request_logs ~nginx_oc ~gateway_oc ~worker_oc ~auth_oc ~syslog_oc
    ~base_time =
  let request_lines = simulate_request ~base_time in
  List.iter (fun { time = _; file; content } ->
    let oc = match file with
      | "nginx" -> nginx_oc
      | "api-gateway" -> gateway_oc
      | "worker-svc" -> worker_oc
      | "auth-svc" -> auth_oc
      | _ -> gateway_oc
    in
    Printf.fprintf oc "%s\n" content
  ) request_lines;
  (* Occasional syslog noise *)
  if Random.int 100 < 30 then
    Printf.fprintf syslog_oc "%s\n" (gen_syslog_line base_time)

let generate_batch ~dir ~count ~start_time ~rotations =
  let t_ref = ref start_time in
  let events_per_rotation = count / (rotations + 1) in

  let open_files dir suffix =
    let s = if suffix = "" then "" else "." ^ suffix in
    let oc name = open_out (Filename.concat dir (name ^ s)) in
    (oc "nginx-access.log", oc "api-gateway.log", oc "worker-svc.log",
     oc "auth-svc.log", oc "syslog.log")
  in
  let close_files (a, b, c, d, e) =
    List.iter close_out [a; b; c; d; e] in

  (* Generate rotated archives first (oldest to newest) *)
  for rot = rotations downto 1 do
    let suffix = string_of_int rot in
    let files = open_files dir suffix in
    let (nginx_oc, gateway_oc, worker_oc, auth_oc, syslog_oc) = files in
    for _ = 1 to events_per_rotation do
      t_ref := !t_ref +. 0.5 +. Random.float 3.0;
      write_request_logs ~nginx_oc ~gateway_oc ~worker_oc ~auth_oc ~syslog_oc
        ~base_time:!t_ref
    done;
    close_files files;
    if rot >= 2 then begin
      let compress name =
        let path = Filename.concat dir (name ^ "." ^ suffix) in
        ignore (Sys.command (Printf.sprintf "gzip -f '%s'" path))
      in
      List.iter compress [
        "nginx-access.log"; "api-gateway.log"; "worker-svc.log";
        "auth-svc.log"; "syslog.log"
      ]
    end
  done;

  (* Generate current (active) log files *)
  let files = open_files dir "" in
  let (nginx_oc, gateway_oc, worker_oc, auth_oc, syslog_oc) = files in
  let remaining = max (count - events_per_rotation * rotations) events_per_rotation in
  for _ = 1 to remaining do
    t_ref := !t_ref +. 0.5 +. Random.float 3.0;
    write_request_logs ~nginx_oc ~gateway_oc ~worker_oc ~auth_oc ~syslog_oc
      ~base_time:!t_ref
  done;
  close_files files;

  Printf.printf "Generated %d requests across 5 log files in %s\n" count dir;
  Printf.printf "  nginx-access.log   Reverse proxy (combined + trace_id)\n";
  Printf.printf "  api-gateway.log    OCaml API gateway (multiline traces)\n";
  Printf.printf "  worker-svc.log     Worker service (JSON lines)\n";
  Printf.printf "  auth-svc.log       Auth service (JSON lines)\n";
  Printf.printf "  syslog.log         System logs\n";
  if rotations > 0 then begin
    Printf.printf "  Rotated: %d generations" rotations;
    if rotations >= 2 then Printf.printf " (.2+ gzipped)";
    Printf.printf "\n"
  end

let generate_config ~dir =
  let formats_path = Filename.concat dir "formats.toml" in
  let sources_path = Filename.concat dir "sources.toml" in

  let formats_oc = open_out formats_path in
  Printf.fprintf formats_oc {|# Weft format definitions — generated by gen_logs

# --- Nginx reverse proxy (combined format + trace_id) ---
[format.nginx]
[format.nginx.timestamp]
regex = '\[(\d{2}/\w{3}/\d{4}:\d{2}:\d{2}:\d{2} [+-]\d{4})\]'
format = "common_log"

[[format.nginx.middleware]]
type = "regex_extract"
pattern = '(\S+)\s+-\s+-\s+\[.*?\]\s+"(\w+)\s+(\S+)\s+HTTP/\S+"\s+(\d+)\s+(\d+)\s+"([^"]*)"\s+"([^"]*)"\s+"([^"]*)"'
fields = ["client_ip", "method", "path", "status", "bytes", "trace_id", "user_agent", "x_trace"]

# --- API Gateway (OCaml structured log with multiline) ---
[format.gateway]
[format.gateway.timestamp]
position = "prefix"
format = "iso8601"

[format.gateway.multiline]
continuation = '^\s|^Raised_at|^Called_from|^Re-raised'
max_lines = 50

[[format.gateway.middleware]]
type = "regex_extract"
pattern = '\S+\s+(\w+)\s+\[([^\]]+)\]:\s+(.*)'
fields = ["level", "module", "message"]

[[format.gateway.middleware]]
type = "regex_extract"
pattern = 'trace_id=(\S+)'
fields = ["trace_id"]

[[format.gateway.middleware]]
type = "regex_extract"
pattern = 'span_id=(\S+)'
fields = ["span_id"]

# --- Worker service (JSON lines) ---
[format.worker_json]
[format.worker_json.timestamp]
json_field = "ts"
format = "epoch_ms"

[[format.worker_json.middleware]]
type = "json_field_extract"
fields = ["level", "msg", "trace_id", "span_id", "parent_span", "error", "path", "table", "rows", "duration_ms", "status"]

# --- Auth service (JSON lines) ---
[format.auth_json]
[format.auth_json.timestamp]
json_field = "ts"
format = "epoch_ms"

[[format.auth_json.middleware]]
type = "json_field_extract"
fields = ["level", "msg", "trace_id", "span_id", "parent_span", "user_id", "reason", "method", "path"]

# --- Syslog ---
[format.syslog]
[format.syslog.timestamp]
format = "syslog_bsd"

[[format.syslog.middleware]]
type = "regex_extract"
pattern = '\w+\s+\d+\s+\S+\s+\S+\s+(\w+)\[(\d+)\]:\s+(.*)'
fields = ["program", "pid", "message"]
|};
  close_out formats_oc;

  let sources_oc = open_out sources_path in
  Printf.fprintf sources_oc {|# Weft source definitions — generated by gen_logs

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
name = "nginx"
type = "file"
path = "%s/nginx-access.log"
format = "nginx"

[[source]]
name = "api-gw"
type = "file"
path = "%s/api-gateway.log"
format = "gateway"

[[source]]
name = "worker"
type = "file"
path = "%s/worker-svc.log"
format = "worker_json"

[[source]]
name = "auth"
type = "file"
path = "%s/auth-svc.log"
format = "auth_json"

[[source]]
name = "syslog"
type = "file"
path = "%s/syslog.log"
format = "syslog"
|} dir dir dir dir dir dir;
  close_out sources_oc;

  Printf.printf "Config: %s, %s\n" formats_path sources_path

let live_append ~dir ~interval_ms =
  let oc name = open_out_gen [Open_append; Open_creat] 0o644
    (Filename.concat dir name) in
  let nginx_oc = oc "nginx-access.log" in
  let gateway_oc = oc "api-gateway.log" in
  let worker_oc = oc "worker-svc.log" in
  let auth_oc = oc "auth-svc.log" in
  let syslog_oc = oc "syslog.log" in

  Printf.printf "Live mode: appending requests every %dms (Ctrl-C to stop)\n%!" interval_ms;

  let running = ref true in
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> running := false));

  while !running do
    let t = Unix.gettimeofday () in
    let request_lines = simulate_request ~base_time:t in
    List.iter (fun { time = _; file; content } ->
      let oc = match file with
        | "nginx" -> nginx_oc
        | "api-gateway" -> gateway_oc
        | "worker-svc" -> worker_oc
        | "auth-svc" -> auth_oc
        | _ -> gateway_oc
      in
      Printf.fprintf oc "%s\n%!" content
    ) request_lines;
    if Random.int 100 < 30 then
      Printf.fprintf syslog_oc "%s\n%!" (gen_syslog_line t);
    Unix.sleepf (float_of_int interval_ms /. 1000.0)
  done;

  List.iter close_out [nginx_oc; gateway_oc; worker_oc; auth_oc; syslog_oc];
  Printf.printf "\nStopped.\n"

let () =
  let dir = ref "/tmp/weft-test" in
  let count = ref 5000 in
  let rotations = ref 0 in
  let live = ref false in
  let live_interval = ref 300 in
  let args = Array.to_list Sys.argv |> List.tl in
  let rec parse = function
    | [] -> ()
    | "--dir" :: d :: rest -> dir := d; parse rest
    | "--count" :: n :: rest -> count := int_of_string n; parse rest
    | "--rotations" :: n :: rest -> rotations := int_of_string n; parse rest
    | "--live" :: rest -> live := true; parse rest
    | "--interval" :: n :: rest -> live_interval := int_of_string n; parse rest
    | "--help" :: _ | "-h" :: _ ->
      Printf.printf "gen_logs — simulate microservice log traffic for weft testing\n\n";
      Printf.printf "Architecture: client -> nginx -> api-gateway -> worker-svc/auth-svc\n";
      Printf.printf "Each request gets a trace_id that appears in all services.\n\n";
      Printf.printf "Usage: gen_logs [OPTIONS]\n\n";
      Printf.printf "Options:\n";
      Printf.printf "  --dir <path>       Output directory (default: /tmp/weft-test)\n";
      Printf.printf "  --count <n>        Number of requests to generate (default: 5000)\n";
      Printf.printf "  --rotations <n>    Rotated archive generations (default: 0)\n";
      Printf.printf "  --live             Keep generating after initial batch\n";
      Printf.printf "  --interval <ms>    Live mode interval (default: 300ms)\n";
      Printf.printf "  -h, --help         Show this help\n";
      exit 0
    | x :: _ ->
      Printf.eprintf "Unknown argument: %s\n" x;
      exit 1
  in
  parse args;

  (try Unix.mkdir !dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());

  let start_time = Unix.gettimeofday () -. (float_of_int !count *. 2.0) in
  generate_batch ~dir:!dir ~count:!count ~start_time ~rotations:!rotations;
  generate_config ~dir:!dir;

  Printf.printf "\nTo test:\n";
  Printf.printf "  weft --formats %s/formats.toml --sources %s/sources.toml\n" !dir !dir;
  Printf.printf "  weft ... -s trace_id=<id>   # follow a request across services\n";
  Printf.printf "  weft ... --dump -s ECONNRESET -s connection_timeout\n";

  if !live then
    live_append ~dir:!dir ~interval_ms:!live_interval
