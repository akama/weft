# 2026-04-04: Full Build — From Design Doc to Working Tool

## Session 1: Scaffolding and Core Libraries

Built the full weft project from DESIGN.md:
- 11 libraries + bin + test, all compiling on OCaml 5.2.1
- Notty for TUI (Minttea incompatible with 5.x)
- 24 unit tests passing

## Session 2: End-to-End Pipeline

- `--dump` mode for headless testing
- Fixed config parser (format subtables double-wrapped)
- Cache population on first read
- Archive discovery + decompression (gz/bz2/xz/zst)
- gen_logs tool for test data generation

## Session 3: Design Gap Closure

- Eliminated all `with _ ->` patterns (specific exceptions only)
- Eio fiber tree: per-source fibers, merge fiber, cache maintenance
- Loki adapter: HTTP search via cohttp-eio, long-poll tail
- TUI key handlers: s/t/d for toggles, Tab focus cycling
- inotify for local file watching (replaced polling)
- SSH socket permissions (0700)

## Session 4: Testing and Live Mode

- 14 functional tests (full pipeline for all formats)
- `--live` CLI mode: tail local/remote files
- TUI input fix: Eio_unix.await_readable (not Unix.select)
- Tab key: Notty sends `Tab variant
- Control char sanitization for multiline entries

## Session 5: Realistic Test Data + Remote Sources

- gen_logs rewrite: microservice simulator with correlated trace_ids
  - nginx -> api-gateway -> worker-svc + auth-svc + cron-processor
  - Delayed async jobs (30-120s) with same trace_id
- Remote SSH: fetch + cache + search tested against real host
- Loki integration: tested against real Grafana Loki instance
- All three source types working end-to-end

## Session 6: Time Range, Heatmap, Status Bar

- `--since`/`--until` CLI flags (relative: 1h, 30m; absolute: 12:00, ISO8601)
- TUI time controls: `</>` shift, `-/+` zoom, `r` reset
- Segment-level time filtering (skip irrelevant archived segments)
- Time heatmap (`H`): density visualization per source
- Help screen (`?`): all keybindings
- Status bar: search progress, cache operations, rotation events
- Sort order toggle (`o`): oldest first / newest first
- Term isolation (`i`/`I`): focus on one term, restore all
- Date display in timestamps when data spans multiple days

## Session 7: Eio Fiber Architecture + Live Tailing in TUI

- Replaced Unix.select/Thread with pure Eio fibers
- Search fiber: background search via Eio.Stream, snapshot params
- Per-source tail fibers: inotify (Eio_unix.await_readable), SSH streaming
- Tail entries written to cache (search finds live data)
- Auto-follow at live edge (newest first: top, oldest first: bottom)
- Rotation lifecycle: seal segment + re-discover + fetch authoritative archives
- gen_logs: `--rotate-sec` and `--keep` for live rotation testing
- Page up/down, go to top/bottom navigation

## Test Infrastructure

- 45 unit/functional tests across 7 executables
- gen_logs: microservice simulator with trace_id correlation
- Live rotation testing: `--rotate-sec 60 --keep 5`
- Remote test host: adam@10.32.140.16 (SSH + Loki)
- Mixed source testing: local + SSH + Loki in one query

## Known Limitations

- Loki tail uses 5s poll (no WebSocket implementation)
- Directory sources don't have per-file tail fibers in TUI
- No TLS support for Loki (cohttp-eio https requires tls package)
