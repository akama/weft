# Unified Log Search TUI — Design Document

**Status:** Draft v3 (Final)
**Language:** OCaml
**Interface:** Terminal UI (Minttea or Nottui/Lwd)
**Config format:** TOML

---

## 1. Overview

A terminal-based tool for searching and tailing logs across heterogeneous sources — local files, local directories, remote hosts (SSH/Teleport), and Loki — presenting results as a unified, time-ordered timeline. Users can search by keyword and time range, add search terms incrementally to discover related entries, and live-tail across all sources simultaneously.

A declarative middleware layer handles per-format transforms (timestamp extraction, field parsing, filtering, multiline joining) and is configured separately from source definitions, enabling reuse across many sources.

A persistent disk cache stores fetched log content locally, enabling fast re-queries when adding new search terms and avoiding redundant network transfers. Archives (rotated/compressed logs) are discovered automatically and fetched on demand as the user extends the search time range.

## 2. Goals

- **Unified timeline**: normalize timestamps across all sources and merge-sort into a single chronological view.
- **Incremental search**: add new terms without restarting. Each term gets a visual tag; historical entries for the new term are backfilled from local cache.
- **Dual mode**: batch search over historical logs and live tail, concurrently.
- **Minimal connection overhead**: one connection per source regardless of term count; compound filters pushed down to the transport layer.
- **Pluggable sources**: a common adapter interface; new backends added without touching the merge engine or TUI.
- **Declarative middleware**: regex, grok patterns, and jq-style field extraction configured in TOML — no scripting runtime, no plugins.
- **Persistent disk cache**: fetched log content cached locally with TTL-based expiry; survives across sessions.
- **On-demand archive access**: rotated/compressed log files discovered automatically and fetched only when the search time range requires them.

## 3. Non-Goals

- Cross-source correlation by request ID, trace ID, or other shared identifiers (may be added later).
- Indexing or persistent storage of log data beyond the TTL cache — this is a live query tool, not a log aggregator.
- Replacing Loki/Grafana for dashboarding or alerting.
- Scriptable / programmable middleware (Lua, WASM, external commands).

---

## 4. Architecture

```
┌───────────────┐
│   TUI Layer    │  Minttea / Nottui+Lwd
│  (timeline,    │  Search bar, source sidebar,
│   sidebar,     │  detail pane
│   detail)      │
└───────┬────────┘
        │ log_entry stream
┌───────┴────────┐
│  Merge Engine  │  k-way merge (heap) for batch
│                │  reorder buffer for tail
└───────┬────────┘
        │ log_entry Seq.t / Eio.Stream.t per source
┌───────┴────────┐
│   Middleware   │  Declarative pipeline per format:
│    Pipeline    │  multiline join → timestamp extract →
│                │  field parse → filter
└───────┬────────┘
        │ raw lines + metadata
┌───────┴────────┐
│  Cache Layer   │  Persistent disk cache
│                │  Segment-based, content-hashed
│                │  On-demand archive fetch
└───────┬────────┘
        │ raw bytes (fetched or cached)
┌───────┴──────────────────────────────────────────────┐
│             Source Adapters                           │
│  ┌───────┐ ┌─────┐ ┌──────────┐ ┌──────┐ ┌───────┐ │
│  │ File  │ │ Dir │ │ SSH/tsh  │ │ Loki │ │K8s opt│ │
│  └───────┘ └─────┘ └──────────┘ └──────┘ └───────┘ │
└──────────────────────────────────────────────────────┘
```

---

## 5. Configuration

Configuration is split into two files, both TOML:

- **formats.toml**: named, reusable log format definitions — timestamp extraction, multiline rules, rotation style, and the middleware pipeline.
- **sources.toml**: connection/transport details, paths, limits, cache settings, and a reference to a format by name.

### 5.1 formats.toml

```toml
# ------------------------------------------------------------------
# Nginx access log
# ------------------------------------------------------------------
[format.nginx_access]

[format.nginx_access.timestamp]
regex = '^\[(\d{2}/\w+/\d{4}:\d{2}:\d{2}:\d{2})'
format = "%d/%b/%Y:%H:%M:%S"

[format.nginx_access.rotation]
style = "rename"              # "rename" (mv + signal) or "copytruncate"
drain_timeout_sec = 5

[[format.nginx_access.middleware]]
type = "strip_ansi"

[[format.nginx_access.middleware]]
type = "grok"
pattern = '%{IP:client_ip} %{WORD:method} %{URIPATHPARAM:path} %{NUMBER:status}'


# ------------------------------------------------------------------
# OCaml application log (structured, multiline stack traces)
# ------------------------------------------------------------------
[format.ocaml_app]

[format.ocaml_app.timestamp]
position = "prefix"
format = "iso8601"

[format.ocaml_app.multiline]
continuation = '^\s|^Raised_at|^Called_from'
max_lines = 50

[format.ocaml_app.rotation]
style = "rename"
drain_timeout_sec = 5

[[format.ocaml_app.middleware]]
type = "json_field_extract"
fields = ["level", "module", "message"]


# ------------------------------------------------------------------
# Syslog (BSD format)
# ------------------------------------------------------------------
[format.syslog]

[format.syslog.timestamp]
format = "syslog_bsd"

[format.syslog.rotation]
style = "rename"
drain_timeout_sec = 5

[[format.syslog.middleware]]
type = "regex_extract"
pattern = '(\w+)\[(\d+)\]: (.*)'
fields = ["program", "pid", "message"]


# ------------------------------------------------------------------
# JSON lines (structured logging)
# ------------------------------------------------------------------
[format.json_lines]

[format.json_lines.timestamp]
json_field = "ts"
format = "epoch_ms"

[[format.json_lines.middleware]]
type = "json_field_extract"
fields = ["level", "msg", "err"]


# ------------------------------------------------------------------
# Raw (no parsing — entries use "received at" time)
# ------------------------------------------------------------------
[format.raw]
```

### 5.2 sources.toml

```toml
[general]
default_time_range = "1h"
reorder_window_ms = 500

[limits]
max_ssh_connections = 4
max_loki_concurrent = 2
catch_up_timeout_sec = 30

[cache]
dir = "~/.cache/logsearch"
max_mb_per_source = 200
default_ttl_hours = 72

# --- Remote files ---

[[source]]
name = "app-server-1"
type = "remote"
transport = "ssh app-server-1"
path = "/var/log/myapp/app.log"
format = "ocaml_app"

[[source]]
name = "bastion"
type = "remote"
transport = "tsh ssh bastion.prod"
path = "/var/log/syslog"
format = "syslog"

# --- Local directory (glob, resolved at startup) ---

[[source]]
name = "local-api-logs"
type = "directory"
glob = "/var/log/api/*.log"
format = "ocaml_app"

[[source]]
name = "nginx-logs"
type = "directory"
glob = "/var/log/nginx/access*.log"
format = "nginx_access"

# --- Local file ---

[[source]]
name = "local-dev"
type = "file"
path = "/tmp/dev-server.log"
format = "json_lines"

# --- Loki ---

[[source]]
name = "prod-loki"
type = "loki"
url = "https://loki.internal:3100"
auth = "bearer"
token_env = "LOKI_TOKEN"
default_labels = '{job="myapp"}'

# K8s via Loki — just a Loki source with k8s label selectors
[[source]]
name = "k8s-production"
type = "loki"
url = "https://loki.internal:3100"
default_labels = '{namespace="production"}'
```

---

## 6. Middleware Pipeline

The middleware layer is a declarative, ordered pipeline of transform steps applied to each log line (or joined multiline block) before it reaches the merge engine. Each format defines its own pipeline in `formats.toml`.

### 6.1 Processing Order

```
raw bytes
  → multiline join (if configured)
  → timestamp extraction
  → middleware step 1
  → middleware step 2
  → ...
  → normalized log_entry
```

### 6.2 Available Middleware Types

**`strip_ansi`** — Remove ANSI escape codes from log lines. No parameters.

**`regex_extract`** — Apply a regex with named or positional capture groups, populating metadata fields.

```toml
[[format.myapp.middleware]]
type = "regex_extract"
pattern = '(?P<level>\w+)\s+(?P<module>[\w.]+):\s+(?P<message>.*)'
```

**`grok`** — Named grok patterns (à la Logstash). Compiled to regex internally. Ships with a standard pattern library (IP, URI, NUMBER, WORD, SYSLOG*, TIMESTAMP_ISO8601, etc.).

```toml
[[format.myapp.middleware]]
type = "grok"
pattern = '%{IP:client} %{WORD:method} %{NUMBER:status}'
```

**`json_field_extract`** — Parse the line (or a previously extracted field) as JSON and extract named fields into metadata.

```toml
[[format.myapp.middleware]]
type = "json_field_extract"
fields = ["level", "msg", "err", "trace_id"]
# Optional: source_field = "message"  (extract from a previously parsed field)
```

**`regex_filter`** — Drop lines matching (or not matching) a pattern. Runs after extraction so it can reference extracted field values.

```toml
[[format.myapp.middleware]]
type = "regex_filter"
exclude = '^DEBUG|^healthcheck'
```

**`field_rename`** — Rename metadata fields for consistency across formats.

```toml
[[format.myapp.middleware]]
type = "field_rename"
mapping = { "msg" = "message", "lvl" = "level" }
```

### 6.3 Multiline Join

Configured at the format level (runs before all middleware steps):

```toml
[format.ocaml_app.multiline]
continuation = '^\s|^Raised_at|^Called_from'
max_lines = 50
```

Lines matching the continuation regex are appended to the previous entry. A safety cap (`max_lines`) prevents runaway joins. The entire joined block is treated as a single entry for search matching — if any line within the block matches a search term, the whole block is included.

### 6.4 Timestamp Extraction

Configured at the format level. Strategies:

| Strategy | Config |
|---|---|
| Prefix auto-detect | `position = "prefix"`, `format = "auto"` |
| Explicit strptime at prefix | `position = "prefix"`, `format = "%Y-%m-%d %H:%M:%S"` |
| Regex capture group | `regex = '...(group)...'`, `format = "..."` |
| JSON field | `json_field = "ts"`, `format = "epoch_ms"` |
| Named shorthand | `format = "iso8601"` / `"syslog_bsd"` / `"epoch_s"` / etc. |

Auto-detection tries a ranked list of known formats (ISO 8601, RFC 3339, syslog RFC 5424, syslog BSD, common log format, epoch seconds, epoch milliseconds) on the first N lines of a source and caches the result.

Entries where no timestamp can be extracted are attached to the previous entry's timestamp (common for multiline blocks handled outside the joiner) and flagged visually in the TUI.

---

## 7. Search Strategy: Dual-Mode by Format

The presence of a `multiline` configuration on a format changes how batch search works for that source. This is the key architectural split:

### 7.1 Simple Formats (no multiline)

Push filtering down to the source transport. Search runs remotely:

```
source → grep -E 'term1|term2' → middleware pipeline → merge engine
```

Fast: only matching lines are transferred. Adding a new term = one new grep call per source.

### 7.2 Multiline Formats

Cannot push grep to the source because matching must operate on joined blocks, not individual lines. Instead, fetch the full raw content, cache locally, and filter client-side:

```
source → fetch full file → cache to disk → multiline join → term filter → merge engine
```

First query is slower (full file transfer). But all subsequent term additions re-scan the local cache — no network round trip.

### 7.3 Loki

Loki handles multiline on ingestion, so it always uses the push-down path regardless of format. LogQL: `{labels} |~ "term1|term2"`.

---

## 8. Persistent Disk Cache

### 8.1 Purpose

The cache stores fetched log content on disk so that:

- Adding a new search term re-scans local data instead of re-fetching from remote sources.
- Subsequent sessions start warm if investigating the same systems.
- Archive files (`.gz`) are decompressed once and reused.

### 8.2 Directory Structure

```
~/.cache/logsearch/
├── app-server-1/
│   ├── manifest.json
│   ├── seg_001                    # sealed segment (pre-rotation content)
│   ├── seg_002                    # active segment (current tail)
│   └── seg_003                    # fetched archive (decompressed app.log.1.gz)
├── bastion/
│   ├── manifest.json
│   └── seg_001
├── local-api-logs/
│   ├── manifest.json
│   ├── app.log → /var/log/api/app.log   # symlink for local files
│   └── worker.log → /var/log/api/worker.log
└── prod-loki/
    ├── manifest.json
    └── seg_001                    # Loki query results cached as JSON lines
```

### 8.3 Manifest Format

Each source has a `manifest.json` tracking segments and known archives:

```json
{
  "source": "app-server-1",
  "format": "ocaml_app",
  "segments": [
    {
      "id": "seg_001",
      "origin": "app.log (pre-rotation)",
      "local_path": "app-server-1/seg_001",
      "time_range": ["2026-04-01T00:00:12Z", "2026-04-02T23:59:58Z"],
      "size_bytes": 48215040,
      "content_hash": "xxh3:a1b2c3d4e5f6",
      "fetched_at": "2026-04-03T10:00:00Z",
      "ttl_hours": 72,
      "sealed": true,
      "joined_index_built": true
    },
    {
      "id": "seg_002",
      "origin": "app.log (current)",
      "local_path": "app-server-1/seg_002",
      "time_range": ["2026-04-03T00:00:01Z", null],
      "size_bytes": 12304000,
      "content_hash": null,
      "fetched_at": "2026-04-03T14:00:00Z",
      "ttl_hours": 72,
      "sealed": false,
      "joined_index_built": true
    }
  ],
  "known_archives": [
    {
      "remote_path": "/var/log/myapp/app.log.1.gz",
      "mtime": "2026-04-02T23:59:00Z",
      "size_bytes": 5200000,
      "matches_segment": "seg_001"
    },
    {
      "remote_path": "/var/log/myapp/app.log.2.gz",
      "mtime": "2026-04-01T00:00:00Z",
      "size_bytes": 4800000,
      "matches_segment": null
    }
  ]
}
```

### 8.4 Cache Operations

**On connect (startup)**:

1. Load existing manifest (warm start) or create empty one (cold start).
2. Fetch the current active log file, store as a new segment.
3. Run archive discovery: `ls -1 {dir}/{basename}*` over SSH (or local glob). Populate `known_archives` with filenames and mtimes.
4. For known archives, peek at time boundaries (remote `zcat | head -1 && zcat | tail -1`) to estimate time ranges. Correlate with existing sealed segments by content hash to avoid redundant fetches.

**On search within cached range**: entirely local. Scan cached segments, run middleware pipeline, filter by terms.

**On time range extension**: check `known_archives` for files whose `mtime` falls in the uncovered range. Fetch, decompress, cache as a new sealed segment. TUI shows progress.

**On term addition**: re-scan all cached segments with updated term set. No network I/O.

**On TTL expiry**: sealed segments older than TTL are evicted on startup or periodically. Active (unsealed) segments are never evicted while the session is running.

**On size limit**: if a source exceeds `max_mb_per_source`, evict oldest sealed segments first (LRU).

### 8.5 Partial Cache Hits

Coverage for a source = union of all segment time ranges. When the user's query extends beyond coverage:

1. Compute the uncovered time range(s).
2. Check `known_archives` for files likely covering the gap (by `mtime`).
3. If a matching archive is found and not yet fetched: pull it, decompress, store as a new sealed segment.
4. If no matching archive exists: report to the TUI that the time range isn't available.

Segment time ranges may overlap slightly (e.g. around rotation boundaries). The dedup layer handles this.

### 8.6 Local Files

For `type = "file"` and `type = "directory"` sources, the cache can use symlinks or direct reads instead of copying. The manifest still tracks segments and time ranges for consistency, but no network fetch is needed. Rotation detection and segment sealing work the same way via inotify.

---

## 9. Rotation Handling

### 9.1 Detection

| Source type | Rotation style | Detection mechanism |
|---|---|---|
| Local file | `rename` | inotify `IN_MOVE_SELF` |
| Local file | `copytruncate` | inotify: file size < current read offset |
| Remote (SSH/tsh) | `rename` | `tail -F` stderr: `has been renamed; following new file` |
| Remote (SSH/tsh) | `copytruncate` | `tail -F` stderr: `file truncated` |
| Loki | N/A | Loki handles rotation at ingestion |

### 9.2 Rotation Lifecycle

When rotation is detected:

```
Rotation event detected
  │
  ├── 1. Keep old file descriptor open
  │      Continue reading (app may still write before receiving SIGHUP)
  │
  ├── 2. Poll old fd until EOF or drain_timeout_sec (default 5s)
  │      All drained entries go into the current segment + tail stream
  │
  ├── 3. Seal current cache segment
  │      Final time_range, content_hash computed, marked sealed = true
  │
  ├── 4. Close old fd
  │
  ├── 5. Open new file path, start new cache segment
  │      New inotify watch (local) or tail -F already following (remote)
  │
  └── 6. Refresh archive discovery (ls over SSH / local glob)
         Correlate new archive (e.g. app.log.1.gz) with sealed segment
         by content_hash to mark matches_segment
```

The drain step ensures no entries are lost during the window between logrotate moving the file and the app receiving SIGHUP to reopen. `tail -F` on remote sources handles the fd management natively; the tool just needs to parse stderr to detect the event and manage cache segments accordingly.

### 9.3 Archive Discovery

When a source config points to path `/var/log/myapp/app.log`, the adapter discovers rotated siblings by probing for common naming patterns:

**Patterns recognized**:

- Numeric suffix: `app.log.1`, `app.log.2`, ..., `app.log.N.gz`
- Date-stamped: `app-2026-04-01.log`, `app-20260401.log.gz`
- Logrotate with delayed compression: `app.log.1` (uncompressed), `app.log.2.gz`

**Discovery command** (remote): `ls -1 {dir}/{basename}*` over the SSH control socket. For local sources: glob expansion.

Archives are sorted by `mtime` (descending = most recent first). Compressed files (`.gz`, `.bz2`, `.zst`, `.xz`) are decompressed during fetch and stored decompressed in the cache.

**Fetch strategy**: on demand only. Archives are fetched when the user extends the search time range past what's currently cached. The TUI shows: `fetching app.log.3.gz from app-server-1...`

---

## 10. Core Types

```ocaml
type source_id = string

type log_entry = {
  timestamp : Ptime.t;
  raw       : string;            (* original text, or joined multiline block *)
  source    : source_id;
  terms     : string list;       (* which search terms matched this entry *)
  metadata  : (string * string) list;  (* populated by middleware pipeline *)
}

type query = {
  terms      : string list;
  time_range : time_range option;
}

type time_range = {
  start_ : Ptime.t;
  end_   : Ptime.t option;      (* None = open-ended / now *)
}

type segment = {
  id          : string;
  origin      : string;          (* human-readable provenance *)
  local_path  : string;
  time_range  : time_range;
  sealed      : bool;
  content_hash: string option;   (* xxh3, computed on seal *)
}
```

---

## 11. Source Adapter Interface

```ocaml
module type Source = sig
  type t

  val connect      : config -> (t, error) result
  val health_check : t -> (unit, error) result

  (** Fetch the full active log file for caching. *)
  val fetch : t -> path:string -> dst:string -> (unit, error) result

  (** Fetch and decompress a compressed archive. *)
  val fetch_archive : t -> path:string -> dst:string -> (unit, error) result

  (** Discover rotated/compressed siblings of the active log. *)
  val discover_archives : t -> path:string -> archive_info list

  (** Batch search (simple formats only — pushed to source). *)
  val search : t -> terms:string list -> time_range -> log_entry Seq.t

  (** Live tail with compound OR filter. Restartable with updated terms. *)
  val tail : t -> terms:string list -> log_entry Eio.Stream.t

  val close : t -> unit
end
```

For multiline formats, `search` is not used — the cache layer calls `fetch` instead and runs the middleware pipeline locally.

---

## 12. Source Backends

### 12.1 Local Flat File

| Concern | Approach |
|---|---|
| Search (simple) | `grep -E 'term1\|term2' <path>` |
| Search (multiline) | Read from cache, join + filter in middleware |
| Tail | `inotify` (Linux) / `kqueue` (macOS); read new bytes, apply regex or stream to middleware |
| Rotation | inotify `IN_MOVE_SELF` / size < offset; drain old fd (5s timeout), seal segment, reopen path |
| Cache | Symlink or direct read; no copy needed |

### 12.2 Local Directory (Glob)

| Concern | Approach |
|---|---|
| Resolution | Expand glob at startup; each matched file becomes a sub-source |
| Identity | Sub-sources named `{parent}:{filename}` (e.g. `local-api-logs:app.log`) |
| Search | Per-file grep or cache read, results merged |
| Tail | Per-file inotify/kqueue watch |
| New files | Not watched — glob resolved once at startup. Restart to pick up new files. |

### 12.3 Remote (SSH / Teleport)

| Concern | Approach |
|---|---|
| Transport | Configurable command: `ssh`, `tsh ssh`, custom |
| Search (simple) | `{transport} grep -En 'term1\|term2' {path}` |
| Search (multiline) | `{transport} cat {path}` → cache locally → middleware pipeline |
| Tail | `{transport} tail -F {path}` piped to `grep --line-buffered -E '...'` (simple) or raw stream (multiline) |
| Fetch | `{transport} cat {path}` or `scp` / `rsync` |
| Fetch archive | `{transport} zcat {path}` (or `bzcat`, `xzcat`, `zstdcat`) |
| Archive discovery | `{transport} ls -1 {dir}/{basename}*` |
| Rotation | Parse `tail -F` stderr for rename/truncate messages |
| Connection reuse | SSH ControlMaster (`ControlPath`, `ControlPersist=600`) |
| Teleport | Preflight `tsh status` check; clear error if session expired |

### 12.4 Loki

| Concern | Approach |
|---|---|
| Search | `GET /loki/api/v1/query_range`, LogQL: `{labels} \|~ "t1\|t2"` |
| Tail | `GET /loki/api/v1/tail` (WebSocket), same LogQL |
| Timestamp | Nanosecond epoch string — trivial to parse |
| Auth | Bearer token (`token_env`) or basic auth |
| Multiline | Handled at ingestion; always uses push-down search |
| Cache | Query results cached as JSON lines segments |

### 12.5 Kubernetes (Optional)

If pods ship logs to Loki (via Alloy/Promtail), K8s is just a Loki source with label selectors (`{namespace="x", pod=~"y.*"}`). No separate adapter needed.

---

## 13. Merge Engine

### 13.1 Batch Mode

K-way merge using a min-heap keyed on `Ptime.t`. Each source yields a lazy `Seq.t` of entries (from grep output or cached segment scan). The heap pops the earliest across all sources.

### 13.2 Tail Mode

Entries arrive asynchronously from multiple `Eio.Stream.t` sources. A reorder buffer holds entries for a configurable window (default `reorder_window_ms = 500`) before emitting, to handle clock skew and network jitter between sources.

```ocaml
type merge_config = {
  reorder_window : Ptime.Span.t;  (* from general.reorder_window_ms *)
}
```

Entries older than `now - reorder_window` are flushed to the TUI in timestamp order.

### 13.3 Deduplication

Entries seen during both catch-up and tail may overlap. Dedup by `(timestamp, source_id, xxh3(raw))` using a bounded hash set covering the overlap window.

---

## 14. Incremental Term Addition

### 14.1 Lifecycle (ordering is critical)

When the user adds a new search term:

1. **Register** the term with a unique color/tag.
2. **Restart tail first**: kill the current tail process/stream and immediately relaunch with the updated compound filter (`term1|term2|term3`). This ensures no gap in coverage.
3. **Catch-up from cache**: for each source with cached segments, re-scan with the new term added. This is local I/O only for multiline formats (the whole point of the cache). For simple formats, run a one-shot `grep` for just the new term over the existing time range — bounded by M (source count), not M × N.
4. **Merge + dedup**: insert catch-up results into the existing timeline. Dedup handles overlap with entries already arriving from the restarted tail.

```
old tail ──stops──┐
                  │ new tail starts immediately
new tail ─────────┤───────────────────────→
catch-up (cache)  └──scans local data──→ dedup against tail
```

### 14.2 Why Tail-First

If catch-up runs before restarting the tail, entries arriving between the old tail stopping and the new tail starting are lost. By restarting the tail first, the only overlap is between the new tail and the catch-up results — handled by dedup.

### 14.3 TUI Interaction

- `/` opens the search bar and **adds** a term (additive by default).
- Sidebar shows active terms with color badges; each can be toggled on/off or deleted.
- Timeline entries are color-coded by which term(s) matched.
- Toggling a term off hides its entries without discarding them.

---

## 15. Connection Management

### 15.1 Connection Pool

```ocaml
type source_conn = {
  config         : source_config;
  mutable tail   : Eio.Process.t option;  (* or WebSocket handle *)
  mutable filter : string list;
  mutable status : [ `Connected | `Reconnecting | `Failed of string ];
  lock           : Eio.Mutex.t;
}
```

### 15.2 SSH ControlMaster

Persistent control socket established on `connect`. All subsequent commands (grep, cat, tail, ls) reuse the socket — no repeated SSH handshake.

```
ssh -o ControlMaster=auto \
    -o ControlPath=/tmp/logsearch-%r@%h:%p \
    -o ControlPersist=600 \
    <host>
```

Critical for Teleport where `tsh` session establishment is slow.

### 15.3 Concurrency Limits

Catch-up queries and archive fetches are dispatched with per-source-type semaphores. TUI shows progress: `catching up: 7/12 sources...` or `fetching app.log.3.gz from app-server-1...`

---

## 16. TUI Layout

```
┌───────────────────────────────────────────────────────┐
│ / search: connection_reset                      [+add]│
├─────────────┬─────────────────────────────────────────┤
│ SOURCES     │ TIMELINE                                │
│             │                                         │
│ ● app-1    │ 14:30:01.123 [app-1]  conn reset from   │
│ ● bastion  │ 14:30:01.456 [loki]   OOM kill pid 882  │
│ ○ local    │ 14:30:02.001 [app-1]  retry attempt 3   │
│ ● loki     │ 14:30:02.340 [bast]   sshd: session ..  │
│ ● api:w1   │ > 14:30:02.789 [loki] panic: out of ..  │
│ ● api:w2   │                                          │
│             │                                         │
│ TERMS       │ ── fetching app.log.2.gz ────── 63% ── │
│ ■ conn_res  │                                         │
│ ■ OOM       │                                         │
│             ├─────────────────────────────────────────┤
│ CACHE       │ DETAIL                                  │
│ 148 MB      │ Source: prod-loki                       │
│ 3 segments  │ Format: ocaml_app                       │
│ Apr 1–3     │ Raw: panic: out of memory allocating .. │
│             │ Fields: level=ERROR module=Gc            │
│             │ Labels: {namespace="production",         │
│             │   pod="api-7f8b9c-x2k4"}                │
└─────────────┴─────────────────────────────────────────┘
```

**Key elements**:

- `●` / `○` = source connected / disconnected
- `■` = colored term badge
- `>` = currently selected entry (shown in detail pane)
- Directory sub-sources shown as `api:w1`, `api:w2`
- Progress bar for active archive fetches
- Cache summary in sidebar (total size, segment count, time coverage)
- Detail pane shows raw line, extracted metadata fields, and source labels

**Navigation**:

- `j/k` — scroll timeline
- `/` — add search term
- `d` — delete selected term
- `Enter` — expand/collapse detail pane
- `Tab` — cycle focus (sources → terms → timeline)
- `s` — toggle source on/off
- `t` — toggle term on/off (hide/show matching entries)
- `q` — quit

---

## 17. Concurrency Model (Eio)

```
main fiber
├── source fiber: app-server-1
│   ├── tail sub-fiber (SSH tail -F, parse stderr for rotation)
│   └── cache writer sub-fiber (append to active segment)
├── source fiber: bastion
│   ├── tail sub-fiber (tsh tail -F)
│   └── cache writer sub-fiber
├── source fiber: local-api:app.log (inotify watch + cache)
├── source fiber: local-api:worker.log (inotify watch + cache)
├── source fiber: prod-loki (WebSocket tail + cache)
├── merge fiber
│   ├── reads all source streams
│   ├── reorder buffer
│   └── emits to TUI
├── cache maintenance fiber
│   ├── TTL eviction (periodic)
│   └── size limit enforcement
└── TUI fiber (Minttea event loop)
```

Cancellation is hierarchical: closing a source cancels its fiber tree; quitting cancels everything. Eio's structured concurrency ensures no dangling SSH processes or unclosed file descriptors.

---

## 18. Security Considerations

- **Cache contains potentially sensitive log data.** The persistent cache stores raw log content on disk, potentially including secrets, PII, or access tokens that appear in logs. Users should be aware that cached data persists beyond the session (until TTL expiry or manual cleanup).
- **Teleport access controls.** Teleport enforces access policies on log access. The local cache bypasses these controls for cached content. Consider: an optional `encrypted = true` flag per source that encrypts cache segments at rest, or a `cache = false` flag to disable caching for sensitive sources entirely.
- **SSH ControlMaster sockets.** The control socket at `/tmp/logsearch-*` could be used by other processes on the same machine to piggyback SSH sessions. Use restrictive permissions (0600) and consider `ControlPath` under `~/.cache/logsearch/sockets/` instead of `/tmp`.

---

## 19. Dependencies (OCaml)

| Concern | Library |
|---|---|
| Concurrency | `eio`, `eio_main` |
| TUI | `minttea` or `nottui`+`lwd` |
| HTTP client | `cohttp-eio` |
| WebSocket | `websocket` or `ocaml-websocket` |
| Time | `ptime` |
| TOML config | `otoml` |
| Regex | `re` |
| Process mgmt | `eio.process` |
| Hashing (dedup + cache) | `digestif` (xxh3 preferred) |
| Glob expansion | `fileutils` or custom |
| JSON (manifests, Loki) | `yojson` |
| Compression | `camlzip` (gzip), `decompress` or shell out to `zstdcat`/`xzcat` |

---

## 20. Open Questions

1. **Naming**: the tool needs a name. Candidates in the alchemical/hermetic vein: *Alembic* (distillation vessel — extracting signal from mixed sources), *Athanor* (slow-burning furnace — persistent, always-on), *Crucible* (vessel for melting/combining).
2. **TUI framework spike**: Minttea (Elm-arch, simpler model) vs. Nottui/Lwd (incremental/reactive, potentially better scroll performance under high throughput). Needs a prototype to evaluate.
3. **Remote directories**: should `type = "remote"` support globs? Would require `ls` + glob expansion over SSH per connect.
4. **Saved investigations**: persist named sessions (term set + source selection + time range) for quick resumption of incident investigations.
5. **Export**: dump the current timeline view to JSON lines, plain text, or a shareable format.
6. **Cache encryption**: optional at-rest encryption for sensitive sources (see Security Considerations).
7. **Loki caching strategy**: Loki results are already aggregated and don't need multiline processing. How aggressively to cache query results vs. re-querying Loki (which is fast)?
