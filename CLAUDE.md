You are autonomous and should make choices to the best of your ability. You are in a sandbox so install software as needed.

Write code according to best practices and commit code frequently by logical change.

We use jj for code management in this repo.
When writing code, check what change you are in, make a new change if needed.
To finish a commit, check `jj diff` and look for any issues, fix if needed, repeat until diff is good, than write a change message using `jj describe` and finish with `jj new`.

Write good tests, run the tests before checking code in to verify it.

Write helper tools if needed to call later.

Please write system docs to docs/systems/
Please write logs of your progress in docs/logs/

Please use CLAUDE.md to store relevent facts.

## Project Facts

- **Name**: weft — unified log search TUI
- **Language**: OCaml 5.2.1 (opam switch: `weft`)
- **Build**: `dune build` / `dune runtest` (45 tests)
- **TUI**: Notty directly (not Minttea — Riot incompatible with OCaml 5.2+)
- **Concurrency**: Eio (structured concurrency, fiber-per-source)
- **Config**: TOML via otoml (formats.toml + sources.toml)
- **Tests**: Alcotest — 7 test executables, 45 tests total
- **Key libs**: eio, notty, otoml, ptime, re, yojson, cohttp-eio, digestif, camlzip, inotify, base64
- **Re.Pcre note**: Named groups use `(?<name>...)` syntax, NOT `(?P<name>...)`
- **Notty note**: Tab key is `` `Tab `` variant, not `` `ASCII '\t' ``
- **Notty note**: `Term.pending` only checks internal buffer; use `Unix.select` on `Term.fds` for input polling
- **Notty note**: `I.string` rejects control chars (newlines, tabs); sanitize before rendering
- **Eio note**: `Unix.sleepf` blocks the scheduler; use `Eio.Time.sleep` or `Eio.Fiber.yield`
- **Eio note**: Optional params — pass value directly, not wrapped in `Some`
- **Error handling**: Never use `with _ ->`. Always catch specific exceptions.

## CLI Modes

```
weft                              # TUI mode
weft --dump -s ERROR --limit 10   # One-shot search
weft --live -s ERROR              # Tail mode (Ctrl-C to stop)
weft --json -s ERROR              # JSON output (implies --dump)
weft --dump --since 1h            # Last hour
weft --dump --since 12:00 --until 13:00  # Specific window
```

## Source Types

- `type = "file"` — local file, inotify tail, local archive discovery
- `type = "directory"` — glob expansion to sub-sources
- `type = "remote"` — SSH fetch/tail via ControlMaster, remote archive discovery
- `type = "loki"` — HTTP query_range API, label selectors, cached locally

## Test Data Generator

```
weft-gen-logs --dir /tmp/weft-test --count 5000 --rotations 2
weft-gen-logs --dir /tmp/weft-test --live --interval 300
```

Simulates: nginx → api-gateway → worker-svc + auth-svc + cron-processor
Trace IDs thread through all services. Cron jobs fire 30-120s after requests.

## Remote Test Host

- Set up a test host with SSH access and Loki (standalone, no auth)
- Generate logs with `weft-gen-logs --dir /tmp/weft-logs --live`
- Create formats.toml + sources.toml pointing at the remote paths
