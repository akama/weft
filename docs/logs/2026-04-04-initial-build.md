# 2026-04-04 / 2026-04-05: Full Build — From Design Doc to Working Tool

## Session 1: Scaffolding and Core Libraries
- 11 libraries + bin + test on OCaml 5.2.1, Notty for TUI
- 24 unit tests passing

## Session 2: End-to-End Pipeline
- `--dump` mode, config parser fix, cache population, archive discovery
- gen_logs test data generator

## Session 3: Design Gap Closure
- Error handling (no `with _ ->`), Eio fiber tree, Loki HTTP adapter
- TUI key handlers, inotify, SSH socket permissions

## Session 4: Testing and Live Mode
- 14 functional tests, `--live` CLI mode, TUI input fix
- Tab key, control char sanitization for multiline entries

## Session 5: Realistic Test Data + Remote Sources
- gen_logs microservice simulator with correlated trace_ids
- Remote SSH and Loki integration tested against real host

## Session 6: Time Range, Heatmap, Status Bar
- `--since`/`--until`, TUI time controls, segment-level filtering
- Heatmap, help screen, status bar, sort order, term isolation

## Session 7: Eio Fiber Architecture + Live Tailing in TUI
- Pure Eio fibers (no Unix.select/Thread/Domain)
- Per-source tail fibers with inotify/SSH streaming
- Tail entries written to cache (search finds live data)
- Auto-follow at live edge, rotation lifecycle

## Session 8: Design Doc Gap Closure
- default_time_range and catch_up_timeout_sec from config
- Dedup between catch-up and tail, directory tailing in TUI
- Archive fetch progress reporting, macOS polling fallback
- Remote directory globs via SSH

## Session 9: Bug Fixes and Robustness
- Segment seal path fix (missing cache_dir/source prefix)
- Source isolation from timeline (`x`/`X` keys)
- Tail fibers respect disabled terms and sources
- Sort search results (removed — merge handles it with per-source sort)
- Quit hangs (close_process_full waits for tail -F, close channels instead)
- SSH ControlMaster socket cleanup, suppress "Exit request sent."
- Empty tail segments (lazy creation on first flush)
- Old tail segments removed after fetching authoritative archive
- load_all uses merge_with_dedup to eliminate cross-segment duplicates

## Session 10: Focus Preservation and Data Integrity
- Preserve selected entry across search refreshes (timestamp + raw fallback)
- Freeze auto-follow during search-in-flight (race condition fix)
- Flush tail buffers before search dispatch (data continuity)
- Per-source sort before merge (cron jobs out of timestamp order)
- Tail entries respect time range filter
- Seq.take truncation keeps newest entries (not oldest) to avoid gaps
- 'r' resets to default_time_range, not "all time"
- Default time range applies regardless of search terms
- Status log viewer (L key) with scrollable history

## Test Coverage
- 58 tests across 9 executables
- Unit: timestamp (9), middleware (7), multiline (4), grok (4), merge (6), dedup (3)
- Functional: pipeline (14), timeline focus (8), truncation (3)

## Known Limitations
- Loki tail uses 5s poll (no WebSocket)
- No TLS for Loki (cohttp-eio https requires tls package)
- 100k entry display limit (warning shown, use time range to see all data)
