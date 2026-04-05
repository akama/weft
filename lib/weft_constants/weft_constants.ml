(* Centralized constants for weft.
   Keeps magic numbers out of library code and makes them easy to find/tune. *)

(* -- TUI ---------------------------------------------------------- *)

(** Maximum entries displayed before truncation warning *)
let max_display_entries = 100_000

(* -- Cache -------------------------------------------------------- *)

(** Lines buffered per source before flushing to cache segment *)
let cache_flush_interval = 50

(** Dedup hash-set capacity for tail-vs-search overlap *)
let tail_dedup_capacity = 50_000

(* -- Loki --------------------------------------------------------- *)

(** Max entries per Loki search query_range call *)
let loki_search_limit = 5_000

(** Max entries per Loki tail poll call *)
let loki_tail_limit = 1_000

(** Seconds between Loki tail polls *)
let loki_tail_poll_sec = 2.0

(** Seconds of look-back window for each Loki tail poll in TUI *)
let loki_tail_lookback_sec = 10

(** Loki TUI poll interval in seconds *)
let loki_tui_poll_sec = 5.0

(** Maximum HTTP response body size for Loki (bytes) *)
let loki_max_body_bytes = 10 * 1024 * 1024

(* -- SSH / streaming ---------------------------------------------- *)

(** Buffer size for SSH stdout streaming (bytes) *)
let ssh_stdout_buf_size = 4096

(** Buffer size for SSH stderr streaming (bytes) *)
let ssh_stderr_buf_size = 1024

(* -- Local file tailing ------------------------------------------- *)

(** Default drain timeout on rotation (seconds) *)
let default_drain_timeout = 5.0

(** Delay after rotation before reopening file (seconds) *)
let rotation_reopen_delay = 0.1

(** Default polling interval for non-inotify systems (ms) *)
let default_poll_interval_ms = 500

(* -- Buffers ------------------------------------------------------ *)

(** Default read buffer size (bytes) *)
let default_read_buf_size = 4096

(* -- Default time range ------------------------------------------- *)

(** Fallback default time range in seconds (1 hour) *)
let default_time_range_sec = 3600
