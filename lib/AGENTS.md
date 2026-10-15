# AGENTS.md: lib/

The `execution-algos` library, module `Execution_algos`. Change the `.mli` first and keep
it documented with odoc.

- `scheduler.ml`: the state machine. `apply` dispatches events; `tick` is the hot path
  (`needs_tick` decides without allocating, `act` does the work); `settle` decides when a
  parent is done.
- `algo.ml`: the algorithm choice, its defaults, short CLI form and validation.
- `parent.ml`, `child.ml`, `fill.ml`: the order types the caller and scheduler exchange.
- `book.ml`, `impact.ml`: top-of-book snapshots and book walks (`[@zero_alloc]`).
- `volume_profile.ml`: VWAP schedules; `cost_report.ml`: per-parent costs and totals.
- `paper.ml`: the paper venue for tests and backtests (mutable, unlike the scheduler).

Rules: no clock reads and no mutable fields in the scheduler; keep the invariants in the
`Scheduler` interface true; every child within the parent's limit and on the lot grid.

See `../AGENTS.md`.
