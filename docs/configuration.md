# Configuration Guide

Weft uses two TOML config files: **formats.toml** defines how to parse log lines, and **sources.toml** defines where logs are and how to access them.

```bash
weft --formats formats.toml --sources sources.toml
```

Config files are searched in order: the path you provide, `~/.config/weft/`, then `/etc/weft/`.

## formats.toml

Each format is a named section under `[format.*]`. Sources reference formats by name.

```toml
[format.myapp]
# subsections: timestamp, multiline, rotation, middleware
```

### Timestamp extraction

Weft needs to extract a timestamp from each log line for time ordering. If no timestamp section is provided, weft auto-detects from the line prefix.

**Auto-detect** (tries ISO 8601, syslog BSD, common log, epoch in order):
```toml
[format.myapp.timestamp]
position = "prefix"
format = "auto"
```

**Named format on line prefix:**
```toml
[format.myapp.timestamp]
position = "prefix"
format = "iso8601"
```

**Shorthand** (just a format name, implies prefix):
```toml
[format.myapp.timestamp]
format = "syslog_bsd"
```

**Regex capture** (extract timestamp from anywhere in the line):
```toml
[format.nginx.timestamp]
regex = '\[(\d{2}/\w{3}/\d{4}:\d{2}:\d{2}:\d{2} [+-]\d{4})\]'
format = "common_log"
```

The first capture group is parsed with the given format.

**JSON field** (timestamp is a field in a JSON line):
```toml
[format.worker.timestamp]
json_field = "ts"
format = "epoch_ms"
```

#### Supported timestamp formats

| Name | Example | Notes |
|------|---------|-------|
| `iso8601` | `2026-04-05T14:30:01Z`, `2026-04-05T14:30:01.123+05:00` | RFC 3339, with optional fractional seconds and timezone |
| `syslog_bsd` | `Apr  5 14:30:01` | No year — assumes current year |
| `epoch_s` | `1712338201`, `1712338201.456` | Unix seconds, optional fractional |
| `epoch_ms` | `1712338201456` | Unix milliseconds |
| `common_log` | `05/Apr/2026:14:30:01 +0000` | Apache/Nginx combined log format |
| `auto` | (any of the above) | Tries each in order, caches the first match |
| Custom strptime | `%Y-%m-%d %H:%M:%S` | Supports `%Y`, `%m`, `%d`, `%H`, `%M`, `%S`, `%b` |

### Multiline

Join continuation lines into a single log entry. Lines matching the `continuation` regex are appended to the previous line.

```toml
[format.ocaml_app.multiline]
continuation = '^\s|^Raised_at|^Called_from|^Re-raised'
max_lines = 50    # safety cap, default 50
```

Useful for stack traces, multi-line JSON, or indented log output.

### Rotation

Configure how log rotation is detected and handled.

```toml
[format.myapp.rotation]
style = "rename"           # "rename" or "copytruncate"
drain_timeout_sec = 5      # seconds to drain old file after rotation (default: 5)
```

- **rename**: logrotate moves the file (`mv app.log app.log.1`), creates a new one. Weft detects via inotify `Move_self`.
- **copytruncate**: logrotate copies then truncates in place. Weft detects via size decrease.

On rotation, weft seals the current cache segment, discovers new archives (the rotated `.1` file), fetches the authoritative copy, and opens a fresh segment for the new file.

### Middleware pipeline

An ordered array of transformation steps. Each step processes the line and metadata from the previous step.

```toml
[[format.myapp.middleware]]
type = "strip_ansi"
```

Removes ANSI escape sequences (color codes, cursor movement). No parameters.

---

```toml
[[format.myapp.middleware]]
type = "regex_extract"
pattern = '(\w+)\s+\[([^\]]+)\]:\s+(.*)'
fields = ["level", "module", "message"]
```

Applies a regex and populates metadata with captured groups. `fields` maps positional groups to names. If the regex uses named groups (`(?<level>\w+)`), field names come from the group names and `fields` is optional.

---

```toml
[[format.myapp.middleware]]
type = "grok"
pattern = '%{IP:client_ip} %{WORD:method} %{URIPATHPARAM:path} %{NUMBER:status}'
```

Logstash-compatible grok patterns. Built-in patterns include `IP`, `WORD`, `NUMBER`, `URI`, `URIPATHPARAM`, `TIMESTAMP_ISO8601`, `SYSLOGBASE`, and others.

---

```toml
[[format.myapp.middleware]]
type = "json_field_extract"
fields = ["level", "msg", "trace_id", "error"]
source_field = "message"    # optional: parse this metadata field as JSON instead of the raw line
```

Parses the line (or a previously extracted field) as JSON and extracts named fields into metadata.

---

```toml
[[format.myapp.middleware]]
type = "regex_filter"
include = 'ERROR|WARN'        # optional: keep only matching lines
exclude = 'healthcheck'       # optional: drop matching lines
```

Filter lines in or out. Both fields are optional. If both are set, `include` runs first, then `exclude`.

---

```toml
[[format.myapp.middleware]]
type = "field_rename"

[format.myapp.middleware.mapping]
msg = "message"
lvl = "level"
```

Renames metadata fields for consistency across different log formats.

### Pipeline execution order

```
raw line
  |  multiline join (if configured)
  |  timestamp extraction
  |  middleware step 1
  |  middleware step 2
  |  ...
  v
log_entry { timestamp, raw, source, metadata }
```

## sources.toml

### General settings

```toml
[general]
default_time_range = "1h"       # default query window (default: "1h")
reorder_window_ms = 500         # reorder buffer for tail streams (default: 500)
base_path = "/var/log/myapp"    # optional: prepend to relative source paths
```

`base_path` is prepended to relative `path` and `glob` values in file and directory sources. Absolute paths are unchanged. Remote and Loki sources are unaffected.

The CLI flag `--base-path` overrides the config value, letting you reuse one sources.toml across environments:

```bash
weft --base-path /tmp/worktree-1 --sources sources.toml
weft --base-path /tmp/worktree-2 --sources sources.toml
```

### Limits

```toml
[limits]
max_ssh_connections = 4         # concurrent SSH connections (default: 4)
max_loki_concurrent = 2         # concurrent Loki queries (default: 2)
catch_up_timeout_sec = 30       # search timeout in seconds (default: 30)
```

### Cache

```toml
[cache]
dir = "~/.cache/weft"           # cache directory (~ expanded, default: ~/.cache/weft)
max_mb_per_source = 200         # max cache per source in MB (default: 200)
default_ttl_hours = 72          # segment expiry in hours (default: 72)
```

The cache stores fetched log data in segments with time range metadata. Sealed segments are evicted by TTL or LRU when size exceeds the limit. Active tail segments are never evicted during a session.

### Source types

#### Local file

```toml
[[source]]
name = "api"
type = "file"
path = "logs/api.log"           # absolute or relative (resolved vs base_path)
format = "myapp"
```

- Tailed via inotify on Linux, polling on macOS
- Archives discovered automatically: `api.log.1`, `api.log.2.gz`, etc.
- Rotation detected and handled per the format's rotation config

#### Directory

```toml
[[source]]
name = "services"
type = "directory"
glob = "logs/*.log"             # absolute or relative (resolved vs base_path)
format = "myapp"
```

- Glob expanded at startup; each matched file becomes a sub-source named `services:filename.log`
- Each file tailed independently
- All files share the same format

#### Remote (SSH)

```toml
[[source]]
name = "prod-api"
type = "remote"
transport = "ssh app-server-1"  # or "tsh ssh bastion.prod" for Teleport
path = "/var/log/api/app.log"
format = "myapp"
```

- Fetches data via `ssh cat`, tails via `ssh tail -F` with streaming stdout/stderr
- SSH ControlMaster reuses connections (no repeated handshakes)
- Rotation detected by parsing `tail -F` stderr messages
- Archives discovered via `ssh ls` and fetched with `ssh zcat`

For Teleport, use `tsh ssh` as the transport. Weft checks `tsh status` on connect.

#### Remote directory

```toml
[[source]]
name = "prod-all"
type = "remote"
transport = "ssh app-server-1"
glob = "/var/log/api/*.log"     # glob expanded on remote via ssh ls
format = "myapp"
```

#### Loki

```toml
[[source]]
name = "k8s-api"
type = "loki"
url = "http://loki:3100"
default_labels = '{namespace="prod", app="api"}'
format = "myapp"
```

With authentication:
```toml
[[source]]
name = "grafana-cloud"
type = "loki"
url = "https://logs-prod.grafana.net"
default_labels = '{job="myapp"}'
auth = "basic"
user_env = "LOKI_USER"         # reads $LOKI_USER at runtime
pass_env = "LOKI_PASS"         # reads $LOKI_PASS at runtime
format = "myapp"
```

```toml
auth = "bearer"
token_env = "LOKI_TOKEN"      # reads $LOKI_TOKEN at runtime
```

- Search via HTTP `query_range` API
- Tail via periodic polling (every 5 seconds)
- Search terms are pushed down as LogQL regex filters

### Recognized archive patterns

When discovering rotated log files, weft looks for siblings of the active log:

| Pattern | Example |
|---------|---------|
| Numeric suffix | `app.log.1`, `app.log.2` |
| Compressed numeric | `app.log.1.gz`, `app.log.2.bz2`, `app.log.3.xz`, `app.log.4.zst` |
| Date-stamped | `app-2026-04-05.log`, `app-20260405.log.gz` |

Supported compression: `.gz` (zcat), `.bz2` (bzcat), `.xz` (xzcat), `.zst` (zstdcat).

Archives are fetched on-demand when the search time range extends beyond cached content.

## Full example

A microservice app with nginx, an API gateway, workers, auth service, and cron processor:

### formats.toml

```toml
# Nginx combined log with trace ID
[format.nginx]
[format.nginx.timestamp]
regex = '\[(\d{2}/\w{3}/\d{4}:\d{2}:\d{2}:\d{2} [+-]\d{4})\]'
format = "common_log"

[[format.nginx.middleware]]
type = "regex_extract"
pattern = '(\S+)\s+-\s+-\s+\[.*?\]\s+"(\w+)\s+(\S+)\s+\S+"\s+(\d+)'
fields = ["client_ip", "method", "path", "status"]

# API gateway with OCaml stack traces
[format.gateway]
[format.gateway.timestamp]
position = "prefix"
format = "iso8601"

[format.gateway.multiline]
continuation = '^\s|^Raised_at|^Called_from'

[[format.gateway.middleware]]
type = "regex_extract"
pattern = '\S+\s+(\w+)\s+\[([^\]]+)\]:\s+(.*)'
fields = ["level", "module", "message"]

[[format.gateway.middleware]]
type = "regex_extract"
pattern = 'trace_id=(\S+)'
fields = ["trace_id"]

# JSON services (worker, auth, cron)
[format.json_svc]
[format.json_svc.timestamp]
json_field = "ts"
format = "epoch_ms"

[[format.json_svc.middleware]]
type = "json_field_extract"
fields = ["level", "msg", "trace_id", "error", "duration_ms"]

# Syslog
[format.syslog]
[format.syslog.timestamp]
format = "syslog_bsd"

[[format.syslog.middleware]]
type = "regex_extract"
pattern = '\w+\s+\d+\s+\S+\s+\S+\s+(\w+)\[(\d+)\]:\s+(.*)'
fields = ["program", "pid", "message"]
```

### sources.toml

```toml
[general]
default_time_range = "1h"
base_path = "/opt/myapp"

[cache]
dir = "~/.cache/weft"
max_mb_per_source = 200

[[source]]
name = "nginx"
type = "file"
path = "logs/nginx/access.log"
format = "nginx"

[[source]]
name = "api-gw"
type = "file"
path = "logs/api-gateway.log"
format = "gateway"

[[source]]
name = "worker"
type = "file"
path = "logs/worker-svc.log"
format = "json_svc"

[[source]]
name = "auth"
type = "file"
path = "logs/auth-svc.log"
format = "json_svc"

[[source]]
name = "cron"
type = "file"
path = "logs/cron-processor.log"
format = "json_svc"

[[source]]
name = "syslog"
type = "file"
path = "/var/log/syslog"
format = "syslog"
```

Use with different environments:
```bash
weft --sources sources.toml                              # uses base_path from config
weft --base-path /tmp/test-env --sources sources.toml    # override for testing
weft --base-path /opt/myapp-v2 --sources sources.toml    # different deploy
```

Note that `/var/log/syslog` is absolute, so `base_path` doesn't affect it.
