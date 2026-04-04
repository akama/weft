# 2026-04-04: Initial Build

## What was done

Built the full weft project from the DESIGN.md spec:

1. Installed OCaml 5.2.1 via opam (5.3 incompatible with Minttea/Riot)
   - Switched to Nottui/Notty instead of Minttea to stay on 5.2.1
2. Created dune project with 11 libraries + bin + test
3. Implemented all libraries bottom-up:
   - Types, config, timestamp, middleware, cache, merge, connection, source, search, TUI, app
4. Fixed build issues:
   - Ptime_clock needs ptime.clock.os
   - Eio.Path.stat returns Optint.Int63.t not int64
   - Eio process manager type can't use wildcards in record declarations → use closures
   - Re.Pcre uses `(?<name>...)` not `(?P<name>...)` for named groups
   - Field name collisions between record types (timestamp, path) → type annotations
   - Notty has no `dim` style → use lightblack color
5. Wrote 24 tests, all passing

## Open items for next session

- Loki adapter needs real HTTP implementation (currently returns empty)
- WebSocket tail for Loki not implemented
- Inotify integration for local file watching (currently uses polling)
- TUI needs Eio integration for async log streaming into the render loop
- Cache segment append should use proper file handles, not load+concat
- Grok patterns with recursive IP/MAC references may need testing
- Config file loading for integration testing
