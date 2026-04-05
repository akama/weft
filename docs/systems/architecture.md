# Weft Architecture

## Overview

Weft is a terminal-based tool for searching and tailing logs across heterogeneous
sources — local files, remote hosts (SSH), and Loki — presenting results as a
unified, time-ordered timeline with live streaming.

## Project Structure

```
bin/
  main.ml              Entry point: Eio_main.run -> Weft_app.run
  gen_logs.ml          Microservice log simulator (correlated trace IDs,
                         live mode with rotation)
  debug_keys.ml        Notty key event debugger

lib/
  weft_types/          Core types (log_entry, query, config records)
  weft_config/         TOML parsing (formats.toml + sources.toml)
  weft_time/           Timestamp extraction (iso8601, syslog, epoch, auto-detect, strptime)
  weft_middleware/     Declarative pipeline (strip_ansi, regex, grok, json, multiline, filter)
  weft_cache/          Persistent disk cache (segments, manifests, TTL/LRU eviction,
                         time-range-aware reading, stale segment pruning, archive gap detection)
  weft_merge/          K-way merge engine (min-heap batch, reorder buffer tail, dedup)
  weft_connection/     SSH ControlMaster lifecycle, connection pool, semaphores,
                         streaming process support (stdout + stderr separation)
  weft_source/         Source adapters (local file with inotify, directory/glob,
                         remote SSH/tsh, Loki HTTP), archive discovery, rotation detection
  weft_search/         Search orchestration, term management, incremental addition,
                         Loki query integration, remote file fetching, status callback
  weft_tui/            Notty terminal UI (timeline, sidebar, detail, search bar,
                         time range controls, status bar, help screen, heatmap,
                         sort order toggle, term isolation)
  weft_app/            Application wiring: CLI args, Eio fiber engine,
                         TUI/dump/live modes, formatting

test/                  58 tests across 9 executables
  test_timeline.ml     8 tests — focus preservation, auto-follow freeze
  test_truncation.ml   3 tests — entry limit keep-newest behavior
```

## Eio Fiber Tree (Design Doc Section 17)

```
Eio.Switch.run
├── search fiber
│   Waits on Eio.Stream for search requests (term changes, time range shifts).
│   Runs search against cache, posts results back via stream.
│   Drains queued requests to only run the latest.
│
├── per-source tail fibers (one per source)
│   ├── local files: inotify via Eio_unix.await_readable
│   │   └── cache writer: buffers lines, flushes to active segment every 50 lines
│   ├── remote SSH: ssh tail -F with streaming stdout/stderr
│   │   └── cache writer: same buffered writes to active segment
│   └── Loki: periodic 5s poll for new entries
│
└── TUI fiber (main event loop)
    ├── Eio_unix.await_readable on terminal input fd (50ms timeout)
    ├── Drains search results from stream
    ├── Drains new tail entries from stream
    ├── Renders via Notty
    └── Dispatches search requests on term/range changes
```

All fibers cooperate via Eio's scheduler. No Unix.select, no Threads,
no Domains — pure cooperative Eio scheduling. The TUI stays responsive
during searches because Eio_unix.await_readable yields to the scheduler.

## Runtime Modes

### TUI (default)
Interactive terminal UI. Initial data loads in background (search fiber).
Per-source tail fibers stream new entries. Status bar shows progress.

### Dump (`--dump`)
One-shot: load sources, search, print results, exit.
Supports `--json`, `--limit`, `--since`/`--until`.

### Live (`--live` / `-f`)
Print historical entries, then tail all sources for new entries.
Entries printed to stdout as they arrive. Ctrl-C to stop.

## Source Types

| Type | Fetch | Tail | Rotation | Archives |
|------|-------|------|----------|----------|
| Local file | Direct read | inotify (Eio_unix.await_readable) | Drain + seal + re-discover archives + reopen | ls glob, decompress |
| Local dir | Glob expand -> per-file | Per-file inotify | Per-file | Per-file |
| Remote SSH | `ssh cat` | `ssh tail -F` (streaming stdout+stderr) | Stderr parse -> seal + fetch authoritative archives | `ssh ls` + `ssh zcat` |
| Loki | HTTP query_range | 5s poll | N/A | N/A |

## Cache Design

```
~/.cache/weft/
├── source-name/
│   ├── manifest.json     Segments + known archives
│   ├── seg_123456        Sealed segment (fetched or archived data)
│   ├── seg_789012        Active tail segment (appended by tail fiber)
│   └── seg_345678        Decompressed archive
```

- Segments track time ranges for range-aware reading (skip irrelevant segments)
- Sealed segments have content hashes and final time ranges
- Active (unsealed) segments receive tail data in real-time
- TTL-based expiry + LRU size limits per source
- Stale segments (missing files) pruned on startup
- Remote archives fetched on-demand based on time range gaps
- Tail entries written to cache so search finds live data

## Rotation Lifecycle

### Local Files
1. inotify detects `Move_self` (rename) or size decrease (truncate)
2. Drain remaining data from old fd (configurable `drain_timeout_sec`)
3. Flush tail buffer and seal active cache segment (content hash + end_time)
4. Re-discover archives (`ls glob`) — find the newly rotated `.1` file
5. Fetch and cache any new archives (decompress `.gz` if needed)
6. Create fresh active cache segment for post-rotation data
7. Reopen the path, re-register inotify watch
8. Continue tailing into new segment

### Remote SSH
1. `tail -F` stderr reports "has become inaccessible" / "has appeared"
2. Flush and seal active cache segment
3. Re-discover archives via `ssh ls`
4. Fetch authoritative archive data via `ssh cat`/`ssh zcat`
5. Cache the authoritative copy (replaces tail approximation)
6. Create fresh active segment
7. `tail -F` continues automatically (follows new file)

### Truncation (copytruncate)
1. Seal active segment
2. Create fresh segment
3. No archive fetch needed (same file, just truncated)

## TUI Keybindings

### Navigation
| Key | Action |
|-----|--------|
| j/k, Up/Down | Scroll one line |
| PgUp/PgDn | Page up/down |
| g / Home | Go to top |
| G / End | Go to bottom |
| Tab | Cycle focus: Sources -> Terms -> Timeline |
| Enter | Toggle detail pane |

### Search
| Key | Action |
|-----|--------|
| / | Add search term |
| d | Delete selected term |
| s | Toggle source on/off |
| x | Isolate source (context: sidebar or timeline entry) |
| X | Restore all sources |
| t | Toggle term visibility |
| i | Isolate term (context: sidebar or timeline entry) |
| I | Restore all terms |

### Time Range
| Key | Action |
|-----|--------|
| < / > | Shift window earlier/later |
| - / + | Narrow/widen window |
| r | Reset to full range |

### Views
| Key | Action |
|-----|--------|
| o | Toggle sort order (oldest/newest first) |
| ? | Help screen |
| H | Time heatmap overview |
| L | Status log (scrollable message history) |
| q | Quit |

## Key Design Decisions

- **Eio fibers** for all concurrency — no Unix.select, no Threads, no Domains
- **Eio_unix.await_readable** for terminal input — yields to scheduler
- **Pluggable wait functions** — local_file and ssh_control accept Eio-aware
  wait callbacks when in TUI, default to Unix.select for CLI mode
- **Notty** for TUI (not Minttea) — no Riot dependency, works with OCaml 5.x
- **Closure-based SSH** — stores a `run_cmd` function, avoids GADT type issues
- **Atomic bool** for tail cancellation
- **Named regex groups** use `(?<name>...)` syntax in Re.Pcre, not `(?P<name>...)`
- **Segment-level time filtering** — skip entire segments outside the query range
- **Post-filter entries** within segments for precision
- **Tail entries written to cache** — search finds live data (design section 17)
- **Snapshot search params** at request time — avoids data races with mutable model
- **Auto-follow at live edge** — Asc: bottom, Desc: top; scroll away to pin
- **Freeze auto-follow** during search-in-flight — preserves selection for focus restoration
- **Flush tail buffers** before search dispatch — ensures cache has recent data
- **Per-source sort** before merge — tail segments may not be in timestamp order (cron jobs)
- **Lazy tail segments** — created on first flush, not eagerly (avoids empty file warnings)
- **Remove old tail segment** after fetching authoritative archive on rotation
- **Keep newest on truncation** — Seq.take 100k keeps newest, not oldest, to avoid gaps
- **Default time range** from config — 'r' resets to configured range, not "all time"

## Build

```
opam switch weft          # OCaml 5.2.1
eval $(opam env --switch=weft)
dune build                # build all
dune runtest              # 58 tests
```

## Test Data Generator

```bash
# Static dataset with rotated archives
weft-gen-logs --dir /tmp/weft-test --count 5000 --rotations 2

# Live mode with rotation every 60s, keep 5 generations
weft-gen-logs --dir /tmp/weft-test --live --interval 200 --rotate-sec 60 --keep 5

# Architecture simulated:
#   client -> nginx -> api-gateway -> worker-svc + auth-svc
#                                  -> cron-processor (30-120s delayed)
# Each request has a trace_id threading through all services.
```

## Usage Examples

```bash
# TUI with live streaming
weft --formats .../formats.toml --sources .../sources.toml -s ERROR

# One-shot dump with time range
weft --dump -s ECONNRESET --since 12:00 --until 13:00 ...

# Follow a trace across all services
weft --dump -s <trace_id> ...

# CLI live tail
weft --live -s ERROR ...

# JSON output for piping
weft --json -s connection ...
```
