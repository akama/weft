# 2026-04-04: Initial Build + Feature Completion

## Session 1: Scaffolding and Core Libraries

Built the full weft project from DESIGN.md:
- 11 libraries + bin + test, all compiling
- OCaml 5.2.1, Notty for TUI (Minttea incompatible with 5.x)
- 24 unit tests passing

## Session 2: End-to-End Pipeline

- Added `--dump` mode for headless testing
- Fixed config parser (format subtables were double-wrapped)
- Fixed search module to read files directly (was returning empty)
- Fixed common_log timestamp parser for regex_capture strategy
- Wired up cache population on first read
- Archive discovery + decompression (gz/bz2/xz/zst) for local files
- gen_logs tool for test data generation

## Session 3: Design Gap Closure

Addressed all high/medium gaps from the design audit:

- **Error handling**: eliminated all `with _ ->` patterns (specific exceptions only)
- **Eio fiber tree**: per-source fibers, merge fiber, cache maintenance
- **Loki adapter**: HTTP search via cohttp-eio, long-poll tail, response parsing
- **TUI key handlers**: s/t/d for source/term toggling, Tab for focus cycling
- **Rotation lifecycle**: inotify detection, drain timeout, seal/reopen callbacks
- **inotify**: replaced polling with Linux inotify for local file watching
- **Security**: SSH socket dir 0700 permissions

## Session 4: Testing and Live Mode

- **14 functional tests**: full pipeline coverage for all formats
- **`--live` mode**: tail local files via inotify, remote via ssh tail -F streaming
- **TUI input fix**: Unix.select on Notty fd (pending doesn't check the fd)
- **Tab fix**: Notty sends `Tab variant, not `ASCII '\t'`
- **Control char fix**: sanitize newlines in multiline entries before Notty rendering

## Session 5: Realistic Test Data + Remote Sources

- **gen_logs rewrite**: microservice simulator with correlated trace_ids
  - Architecture: nginx → api-gateway → worker-svc + auth-svc + cron-processor
  - Delayed async jobs (30-120s after request) with same trace_id
  - Error scenarios: ECONNRESET, pool_exhausted, query_timeout, etc.
- **Remote SSH**: fetch + cache + search working against remote host
  - Archives discovered and decompressed over SSH
  - Warm cache: zero SSH calls on subsequent runs
- **Loki integration**: tested against real Grafana Loki instance
  - HTTP query_range with LogQL label selectors
  - Results cached locally

## Session 6: Time Range Scoping

- **CLI flags**: `--since`/`--until` with relative (1h, 30m) and absolute (12:00, ISO8601)
- **TUI controls**: `</>` shift, `-/+` zoom, `r` reset
- **Segment-level optimization**: skip entire segments outside query range
- **Archive gap detection**: only fetch remote archives needed for the gap
- **Rotation → cache wiring**: on_seal/on_new callbacks connected in live mode

## Current Test Coverage

45 tests across 7 test executables:
- test_timestamp (9), test_middleware (7), test_multiline (4)
- test_grok (4), test_merge (4), test_dedup (3)
- test_functional (14): full pipeline for all format types

## Remaining Items

- Remote SSH rotation: parse tail -F stderr for rename/truncate messages
- Loki WebSocket tail (currently long-polls)
- Directory source live tailing (individual file fibers)
- TUI: more visual feedback during fetching/caching operations
