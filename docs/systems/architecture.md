# Weft Architecture

## Overview

Weft is a terminal-based tool for searching and tailing logs across heterogeneous
sources — local files, remote hosts (SSH), and Loki — presenting results as a
unified, time-ordered timeline.

## Project Structure

```
bin/
  main.ml              Entry point: Eio_main.run → Weft_app.run
  gen_logs.ml          Microservice log simulator for testing
  debug_keys.ml        Notty key event debugger

lib/
  weft_types/          Core types (log_entry, query, config records)
  weft_config/         TOML parsing (formats.toml + sources.toml)
  weft_time/           Timestamp extraction (iso8601, syslog, epoch, auto-detect, strptime)
  weft_middleware/     Declarative pipeline (strip_ansi, regex, grok, json, multiline, filter)
  weft_cache/          Persistent disk cache (segments, manifests, TTL/LRU eviction,
                         time-range-aware reading, archive gap detection)
  weft_merge/          K-way merge engine (min-heap batch, reorder buffer tail, dedup)
  weft_connection/     SSH ControlMaster lifecycle, connection pool, semaphores,
                         streaming process support for tail -F
  weft_source/         Source adapters (local file with inotify, directory/glob,
                         remote SSH/tsh, Loki HTTP), archive discovery, rotation detection
  weft_search/         Search orchestration, term management, incremental addition,
                         Loki query integration, remote file fetching
  weft_tui/            Notty terminal UI (timeline, sidebar, detail, search bar,
                         time range controls, progress indicators)
  weft_app/            Application wiring: CLI args, Eio fiber engine,
                         TUI/dump/live modes, formatting

test/
  test_timestamp.ml    9 tests — all timestamp formats + auto-detect
  test_middleware.ml   7 tests — all middleware types + pipeline
  test_multiline.ml    4 tests — join + streaming
  test_grok.ml         4 tests — expand, apply, pattern library
  test_merge.ml        4 tests — heap, batch merge
  test_dedup.ml        3 tests — basic, bounded, merge integration
  test_functional.ml   14 tests — full pipeline end-to-end
```

## Dependency Graph

```
weft_types (leaf — ptime)
  ↑
weft_config (otoml, re)     weft_time (ptime, re, yojson)
  ↑                           ↑
weft_middleware (re, yojson)
  ↑
weft_cache (yojson, digestif, eio, camlzip, weft_time)
weft_merge (ptime, eio, digestif)
weft_connection (eio, unix)
  ↑
weft_source (cohttp-eio, inotify, re, yojson, uri, base64)
  ↑
weft_search (re)
  ↑
weft_tui (notty, notty.unix, ptime)
  ↑
weft_app (cohttp-eio, http, uri, yojson, notty.unix)
  ↑
bin/main.ml (eio_main)
```

## Runtime Modes

### TUI (default)
Interactive terminal UI with Eio fiber tree:
- Per-source fibers load data and emit to entry_stream
- Merge fiber collects and reorders entries
- TUI fiber renders at ~20fps, polls terminal input via Unix.select on Notty fd
- Time range adjustable with `</>` (shift), `-/+` (zoom), `r` (reset)

### Dump (`--dump`)
One-shot: load sources, search, print results, exit.
Supports `--json`, `--limit`, `--since`/`--until`.

### Live (`--live` / `-f`)
Print historical entries, then tail all sources for new entries.
- Local files: inotify with rotation detection + cache segment sealing
- Remote SSH: `ssh tail -n 0 -F` with streaming line reads
- Ctrl-C to stop

## Source Types

| Type | Fetch | Tail | Rotation | Archives |
|------|-------|------|----------|----------|
| Local file | Direct read | inotify (S_Modify, S_Move_self) | Drain + seal + reopen | ls glob, decompress |
| Local dir | Glob expand → per-file | Per-file inotify | Per-file | Per-file |
| Remote SSH | `ssh cat` | `ssh tail -F` (streaming) | Stderr parse (planned) | `ssh ls` + `ssh zcat` |
| Loki | HTTP query_range | Long-poll (2s) | N/A | N/A |

## Cache Design

```
~/.cache/weft/
├── source-name/
│   ├── manifest.json     Segments + known archives
│   ├── seg_123456        Cached content (active or sealed)
│   └── seg_789012        Decompressed archive
```

- Segments track time ranges for range-aware reading (skip irrelevant segments)
- Sealed segments have content hashes; unsealed ones are active
- TTL-based expiry + LRU size limits per source
- Remote archives fetched on-demand based on time range gaps

## Rotation Lifecycle (Live Mode)

1. inotify detects `Move_self` (rename) or file size decrease (truncate)
2. Drain remaining data from old fd (configurable `drain_timeout_sec`)
3. `on_seal` callback: seal current cache segment with content hash + end time
4. `on_new` callback: create new cache segment for post-rotation data
5. Reopen the path, re-register inotify watch on new file
6. Continue tailing

## Key Design Decisions

- **Notty** for TUI (not Minttea) — no Riot dependency, works with OCaml 5.x
- **Eio** for structured concurrency — fiber-per-source, cooperative scheduling
- **Unix.select** for TUI input polling — Notty.pending doesn't check the fd
- **Closure-based SSH** — stores a `run_cmd` function, avoids GADT type issues
- **Atomic bool** for cancellation — simpler than Eio.Cancel for cross-fiber signaling
- **Stream-index tracking** in batch merge — avoids source name collision
- **Named regex groups** use `(?<name>...)` syntax in Re.Pcre, not `(?P<name>...)`
- **Segment-level time filtering** — skip entire segments outside the query range
- **Post-filter entries** within segments — segments are coarse, entries need precision

## Build

```
opam switch weft          # OCaml 5.2.1
eval $(opam env --switch=weft)
dune build                # build all
dune runtest              # 45 tests
```

## Testing Tools

```bash
# Generate 5000 correlated microservice requests
weft-gen-logs --dir /tmp/weft-test --count 5000 --rotations 2

# Follow a trace across all services
weft --dump -s <trace_id> --formats ... --sources ...

# Live tail with search filtering
weft --live -s ERROR --formats ... --sources ...

# Time-scoped search
weft --dump -s ECONNRESET --since 12:00 --until 13:00 ...
```
