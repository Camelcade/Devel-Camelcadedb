# Devel::Camelcadedb

Perl-side backend of the [Camelcade](https://github.com/Camelcade/Perl5-IDEA) debugger. Loaded inside the debugged Perl interpreter (via `perl -d:Camelcadedb`); talks to the IntelliJ IDE over a TCP socket using a line-delimited JSON protocol.

## Layout

- `lib/Devel/Camelcadedb.pm` — the whole implementation (~2000 lines). Everything lives in `package DB` (Perl's debugger hook namespace); the outer `Devel::Camelcadedb` package only holds `$VERSION`.
- `lib/Devel/Camelcadedb.pod` — user docs (checked by `t/pod*.t`).
- `t/` — tests, run with `prove -lv t`.
- `testData/results/` — expected output for golden-file tests.
- `dist.ini` / `cpanfile` — Dist::Zilla build config and deps. Build with `dzil`. `cpanfile` is generated — don't hand-edit.
- `Devel-Camelcadedb-v2023.1/`, `.tar.gz` — a checked-in built release. Don't edit; it's not the source.

## How it works (the essentials)

The IDE injects this module as Perl's debugger. Perl calls into `DB::` hooks; this module implements them and forwards state to the IDE.

Perl-mandated hooks (names/behavior fixed by the Perl interpreter, see `perldebguts`):
- `DB::DB` → `step_handler` — called on each executable line when `$DB::single`/`$signal`/`$trace` is set. The core step/breakpoint gate.
- `DB::sub` → `sub_handler` — wraps every sub call; maintains `$_stack_frames`, handles step-over/step-out by juggling `$DB::single`.
- `DB::postponed` → `load_handler` — fires after each file compiles; used to set queued breakpoints on newly-loaded files.
- `template_handler` — hook for templating engines that run via `eval`; maps template lines to eval'd source lines so breakpoints work in templates.

`$_internal_process` is a reentrancy guard: it's set while the debugger runs its own code so the hooks don't debug themselves. Respect it.

Communication: JSON via `JSON::XS` (latin1 coder), newline-delimited, over `$_debug_socket`. Commands come in single-letter form (`b` set breakpoint, `g` go, `o` over, `u` step out, `e` eval, `getchildren`, `get_source`, `p` run-to-cursor, `q` quit) parsed in `_process_command`. Events go out via `_send_event` / `_send_transaction_response` (`READY`, `STOP`, `BREAKPOINT_REACHED`, `BREAKPOINT_SET/DENIED`, `LOADED_FILES_DELTA`).

Variable inspection: `_get_reference_descriptor` turns any Perl value into an IDE descriptor (type, size, blessed, tied, expandable, IO layers, custom renderers). `_compute_reference_subelements` lazily expands arrays/hashes/globs on IDE request via `$_references_cache`.

`$API_VERSION` (protocol version, currently `2019.1`) is decoupled from `$VERSION` so the debugger can be bumped without forcing an IDE update.

## Startup / env

The module connects on load. Required env vars (set by the IDE, checked at bottom of the `.pm`):
- `PERL5_DEBUG_ROLE` — `server` (Perl listens) or `client` (Perl connects to IDE).
- `PERL5_DEBUG_HOST`, `PERL5_DEBUG_PORT`.
- `PERL5_DEBUG_AUTOSTART` — default 1; set 0 to defer connecting (tests use this).
- `CAMELCADEDB_DEV_MODE` — verbose STDERR/logfile diagnostics via `_report`.

## Tests

- `prove -lv t` runs everything (matches CI).
- `t/reference_descriptor_serializer.t` — golden-file test: serializes descriptors and diffs against `testData/results/*.txt`. To regenerate expected output, set `$OVERWRITE_RESULTS = 1` in the test, run once, then set it back. It sets `PERL5_DEBUG_AUTOSTART=0` so loading the module doesn't try to connect.
- `t/pod.t`, `t/pod-coverage.t`, `t/manifest.t` — release hygiene.
- CI (`.github/workflows/main.yml`) runs the suite on Linux+macOS across Perl 5.12–5.36. Code must stay compatible across that range (`use 5.008`). Perl 5.10 is deliberately excluded.

## Conventions / gotchas

- Line numbers: Perl is 1-based, the IDE is 0-based. Descriptors and breakpoints convert (`-1` outbound, `++` inbound in `_process_breakpoints_descriptors`). Watch this when touching line handling.
- File ids: Perl stores source as `@{"::_<$filename"}`; `_get_real_path_by_...` maps between Perl file ids, `(eval N)` ids, and real filesystem paths. Maps: `%_perl_file_id_to_path_map`, `%_paths_to_perl_file_id_map`.
- The step handler saves/restores Perl's punctuation vars (`$@ $! $^E $, $/ $\ $^W`) in `@saved` around its work — don't leak debugger state into the debugged program.
- `enable`/`disable` toggle debugging by swapping the `*DB::sub` glob (can't just undef it — `$DB::sub`/`%DB::sub` must survive). See the comment block above `%_orig_db_sub`.
- `$VERSION` must keep its leading `v` (see the shouty comment on line 3) for correct versioning and JSON protocol.
- Keep everything in `package DB` — Perl's hook dispatch requires it.
