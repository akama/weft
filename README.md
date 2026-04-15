# weft

A terminal-based tool for searching and tailing logs across heterogeneous sources — local files, remote hosts (SSH/Teleport), and Loki — presenting results as a unified, time-ordered timeline with live streaming.

![License](https://img.shields.io/badge/license-ISC-blue)

## Features

- **Unified timeline** — merge logs from local files, SSH remotes, and Loki into a single time-ordered view
- **Live tailing** — stream new entries from all sources simultaneously with per-source tail fibers
- **Interactive TUI** — search, filter, isolate sources/terms, navigate with keyboard shortcuts
- **Log rotation handling** — detects rename, truncate, and inode changes; fetches authoritative archives automatically
- **Persistent cache** — segment-based disk cache with TTL/LRU eviction and time-range-aware reads
- **Flexible parsing** — regex, grok, JSON field extraction, multiline join, ANSI stripping via declarative middleware pipelines
- **Multiple timestamp formats** — ISO 8601, syslog BSD, epoch milliseconds, common log format, custom strptime
- **CLI modes** — interactive TUI (default), one-shot dump, live tail, JSON output for piping

## Install

Download a static binary from [Releases](https://github.com/akama/weft/releases):

```bash
# Linux x86_64
curl -sL https://github.com/akama/weft/releases/latest/download/weft-linux-x86_64.tar.gz | tar xz
sudo mv weft /usr/local/bin/

# Linux aarch64
curl -sL https://github.com/akama/weft/releases/latest/download/weft-linux-aarch64.tar.gz | tar xz
sudo mv weft /usr/local/bin/

# macOS (Apple Silicon)
curl -sL https://github.com/akama/weft/releases/latest/download/weft-macos-aarch64.tar.gz | tar xz
sudo mv weft /usr/local/bin/
```

Linux binaries are fully static (musl) — no dependencies required.

### Build from source

Requires OCaml 5.2+ and opam:

```bash
opam install . --deps-only --yes
dune build
# Binary at _build/default/bin/main.exe
```

## Quick start

weft needs two TOML config files: one defining log formats, one defining sources.

```bash
weft --formats formats.toml --sources sources.toml
```

### formats.toml

Defines how to parse timestamps and extract fields from log lines:

```toml
[format.nginx]
[format.nginx.timestamp]
position = "regex"
regex = '^\S+ \S+ \S+ \[(?<ts>[^\]]+)\]'
format = "common_log"

[[format.nginx.middleware]]
type = "regex_extract"
pattern = '^(?<client_ip>\S+) \S+ \S+ \[.*?\] "(?<method>\S+) (?<path>\S+) \S+" (?<status>\d+)'

[format.app]
[format.app.timestamp]
position = "prefix"
format = "iso8601"

[[format.app.middleware]]
type = "json_field_extract"
fields = ["level", "msg", "trace_id"]
```

### sources.toml

Defines where logs are and how to access them:

```toml
[general]
default_time_range = "1h"
# base_path = "/opt/myapp"  # prepended to relative source paths

[limits]
max_ssh_connections = 4
catch_up_timeout_sec = 30

[cache]
dir = "~/.cache/weft"
max_mb_per_source = 200
default_ttl_hours = 72

[[source]]
name = "nginx"
type = "file"
path = "/var/log/nginx/access.log"
format = "nginx"

[[source]]
name = "app"
type = "remote"
transport = "ssh app-server-1"
path = "/var/log/app/app.log"
format = "app"

[[source]]
name = "logs"
type = "loki"
url = "http://loki:3100"
default_labels = '{app="myservice"}'
format = "app"
```

## Usage

```bash
# Interactive TUI (default — last hour, live tail)
weft --formats f.toml --sources s.toml

# Search for errors
weft -s ERROR --formats f.toml --sources s.toml

# One-shot dump with time range
weft --dump -s ECONNRESET --since 12:00 --until 13:00 ...

# Follow a trace across all services
weft --dump -s trace_id=abc123 ...

# CLI live tail
weft --live -s ERROR ...

# JSON output for piping
weft --json -s connection ...

# Override base path (for worktrees / multiple environments)
weft --base-path /tmp/worktree-2 --sources sources.toml ...
```

### Base path

If your sources.toml uses relative paths, `base_path` (in config or via `--base-path`) is prepended to resolve them. This lets you share one sources.toml across multiple environments:

```toml
# sources.toml — works with any --base-path
[[source]]
name = "api"
type = "file"
path = "logs/api.log"
format = "app"
```

```bash
weft --base-path /opt/myapp --sources sources.toml       # production
weft --base-path /tmp/worktree-1 --sources sources.toml  # dev worktree
weft --base-path /tmp/worktree-2 --sources sources.toml  # another worktree
```

### Time specs

`--since` and `--until` accept:
- Relative: `1h`, `30m`, `2h30m`, `90s`
- Time of day: `10:30`, `10:30:00`
- ISO 8601: `2026-04-04T10:30:00Z`

## TUI keybindings

### Navigation
| Key | Action |
|-----|--------|
| `j`/`k`, Up/Down | Scroll one line |
| PgUp/PgDn | Page up/down |
| `g` / Home | Go to top |
| `G` / End | Go to bottom |
| Tab | Cycle focus: Sources > Terms > Timeline |
| Enter | Toggle detail pane |

### Search
| Key | Action |
|-----|--------|
| `/` | Add search term |
| `d` | Delete selected term |
| `s` | Toggle source on/off |
| `x` | Isolate source |
| `X` | Restore all sources |
| `t` | Toggle term visibility |
| `i` | Isolate term |
| `I` | Restore all terms |

### Time range
| Key | Action |
|-----|--------|
| `<` / `>` | Shift window earlier/later |
| `-` / `+` | Narrow/widen window |
| `r` | Reset to default range |

### Views
| Key | Action |
|-----|--------|
| `o` | Toggle sort order (oldest/newest first) |
| `?` | Help screen |
| `H` | Time heatmap overview |
| `L` | Status log |
| `q` | Quit |

## Source types

| Type | Config | Fetch | Tail | Rotation |
|------|--------|-------|------|----------|
| `file` | `path` | Direct read | inotify (Linux) / polling (macOS) | Rename + truncate detection, archive re-discovery |
| `directory` | `glob` | Glob expand | Per-file | Per-file |
| `remote` | `transport` + `path` | SSH cat | SSH tail -F (streaming) | Stderr parsing, remote archive fetch |
| `loki` | `url` + `default_labels` | HTTP query_range | Periodic poll | N/A |
| `journald` | `unit` (+ optional `transport`) | journalctl JSON | journalctl -f | N/A (journal-managed) |

## Architecture

Built on OCaml 5.2 with Eio for structured concurrency. The runtime is a tree of cooperative fibers:

```
Eio.Switch.run
+-- search fiber        (background search, drains queued requests)
+-- per-source tail fibers
|   +-- local files: inotify / polling
|   +-- remote SSH: streaming stdout+stderr
|   +-- Loki: periodic HTTP poll
+-- TUI fiber           (input, render, dispatch)
```

All fibers cooperate via Eio's scheduler. No threads, no domains, no `Unix.select` in the hot path.

Tail entries are written to a persistent disk cache so that search results include live data. The merge engine uses a min-heap for k-way sorted merge with bounded dedup.

## Test data generator

`weft-gen-logs` simulates a microservice architecture for testing:

```
client -> nginx -> api-gateway -> worker-svc + auth-svc
                                -> cron-processor (30-120s delayed)
```

Each request gets a trace ID that threads through all services.

```bash
# Static dataset with rotated archives
weft-gen-logs --dir /tmp/test --count 5000 --rotations 2

# Live mode with rotation every 60s
weft-gen-logs --dir /tmp/test --live --interval 200 --rotate-sec 60 --keep 5
```

## License

ISC
