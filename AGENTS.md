# AGENTS.md: execution-algos

## Purpose

execution-algos slices a parent order into child orders with a pure, journal-replayable
scheduler (Immediate, TWAP, VWAP, POV, passive/iceberg, adaptive copy), estimates impact
from L2 depth and reports the cost of each parent. It is venue-neutral: callers map
children to their venue's orders.

## Layout

| Path | Holds |
| --- | --- |
| `lib/` | Library `execution-algos` (module `Execution_algos`): `Algo`, `Scheduler`, `Parent`, `Child`, `Fill`, `Book`, `Impact`, `Volume_profile`, `Cost_report`, `Paper` |
| `test/` | Expect traces per algo, Quickcheck invariants, allocation pins; `Harness` and `Bench_cases` shared with the bench |
| `bench/` | `main.exe`: core_bench of scheduler ticks, impact walks and whole parents |
| `.github/` | CI workflow |
| `README.md` | Algorithms, invariants, cost definitions, bench numbers |

## Build, test, docs

Run from this repository, inside the workspace:

    opam exec --switch 5.2.0+ox -- dune build --root .. @execution-algos/all
    opam exec --switch 5.2.0+ox -- dune build --root .. @execution-algos/runtest
    opam exec --switch 5.2.0+ox -- dune build --root .. @execution-algos/doc
    opam exec --switch 5.2.0+ox -- dune exec --root .. execution-algos/bench/main.exe -- -quota 2

Several agents building at once need `DUNE_BUILD_DIR=_build-execution-algos` (resolved
from the workspace root because of `--root ..`). The docs build must produce no warnings.

Standalone: `opam repo add winterchill https://github.com/winterchill-trading/opam-repository.git`,
then `opam install . --deps-only --with-test` and `dune build`.

## Conventions

- Format this repository only: `dune build --root .. @execution-algos/fmt --auto-promote`.
- Every module has an `.mli` with odoc on each value. Persisted types (`Algo`, `Parent`,
  `Child`, `Fill`, `Book`, `Scheduler.Event`, `Cost_report`) have `Stable.V1`; change them
  only by adding a version.
- The scheduler reads no clock and holds no mutable field; time and market data come in
  `Tick` events. A tick with nothing to do must return the same state without allocating
  (`test/test_alloc.ml` pins 0 words).
- Every new algo or rule gets an expect trace in `test/test_scheduler.ml` and must keep
  the properties in `test/test_invariants.ml` passing.
- Money and quantities are fixed-point (`Market_data.Price`, `Market_data.Size`, units of
  10^-8); fees and costs are float USD.
- User-visible changes get a line under `## Unreleased` in `CHANGES.md`.

## Dependencies

- Sibling repositories: `fixed-decimal`, `exchange-core`, `market-data` (which needs
  `parquet`).
- Used by: `hl-copy-engine` (reconciliation), `copy-backtest`.
- CI pin order: `fixed-decimal`, `exchange-core`, `parquet`, `market-data`.

## CI

`.github/workflows/ci.yml`: infra `ox-setup` (cached switch, pins above), build, tests,
then odoc with warnings failing the step, and the shared obj-magic job. Check with one
`gh run list --limit 1` and at most one `gh run watch <id> --exit-status`; never poll.

## Pitfalls

- Base's `List.init` calls `f` from the last index down: build cumulative sequences with
  `List.range` and `List.map`.
- A boxed tuple return allocates; the hot `Scheduler.tick` returns an unboxed pair
  (`#(t, actions)`).
- Cross-module `[@zero_alloc]` checks need the callee annotated in its `.mli` (the dev
  profile does not inline across modules).
- odoc failed to resolve `{!Algo.x}` references from other modules here; write
  `[Algo.x]` instead.
- When another repository in the workspace has a broken dune file, build from a scratch
  workspace of symlinks to this repository and its dependencies.

## Where to read more

- `README.md`; `../specs/repos/execution-algos.md`; `../specs/design/copytrading.md`.
- Skills: `../oxcaml-claude-skills/skills/oxcaml-performance/`,
  `../oxcaml-claude-skills/skills/janestreet-ocaml-style/`.
- Workspace guide: `../oxcaml-claude-skills/AGENTS.md`.

## Never do

- Install opam packages that change the compiler, `core`, `base`, `async` or `ppx_jane`.
- Publish, tag or release.
- Poll CI in a loop.
