# Weft Architecture

## Project Structure

```
bin/main.ml          — Entry point: Eio_main.run → Weft_app.run
lib/
  weft_types/        — Core types (log_entry, query, config records)
  weft_config/       — TOML parsing (formats.toml, sources.toml)
  weft_time/         — Timestamp extraction (iso8601, syslog, epoch, auto-detect, strptime)
  weft_middleware/   — Declarative pipeline (strip_ansi, regex, grok, json, multiline, filter)
  weft_cache/        — Persistent disk cache (segments, manifests, TTL/LRU eviction)
  weft_merge/        — K-way merge engine (min-heap batch, reorder buffer tail, dedup)
  weft_connection/   — SSH ControlMaster lifecycle, connection pool, semaphores
  weft_source/       — Source adapters (local file, directory, remote SSH/tsh, Loki)
  weft_search/       — Search orchestration, term management, incremental addition
  weft_tui/          — Nottui/Notty terminal UI (timeline, sidebar, detail, search bar)
  weft_app/          — Application wiring, CLI args, startup, shutdown
test/                — Alcotest test suite
```

## Dependency Graph (bottom-up)

```
weft_types (leaf)
  ↑
weft_config, weft_time
  ↑
weft_middleware
  ↑
weft_cache, weft_merge, weft_connection
  ↑
weft_source
  ↑
weft_search
  ↑
weft_tui
  ↑
weft_app → bin/main.ml
```

## Key Design Decisions

- **Nottui/Notty** for TUI (not Minttea) — no Riot dependency, works with OCaml 5.x
- **Eio** for structured concurrency — fiber-per-source model
- **Closures over GADT** for process manager — SSH control stores a `run_cmd` function
  rather than a typed process manager reference
- **Atomic bool** for cancellation — simpler than Eio.Cancel for cross-fiber signaling
- **Stream-index tracking** in batch merge — avoids source name collision when entries
  from different streams have the same source field

## Build

```
opam switch weft
eval $(opam env --switch=weft)
dune build
dune runtest
```
