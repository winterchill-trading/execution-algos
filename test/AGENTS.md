# AGENTS.md: test/

Inline expect and Quickcheck tests (library `execution_algos_test`).

- `harness.ml`: synthetic books and markets, and `run`, which drives a scheduler through
  the paper venue and returns the journal of events.
- `test_scheduler.ml`: one trace per algo; read the trace before accepting a change.
- `test_invariants.ml`: the properties listed in the `Scheduler` interface over 2,000
  random cases, plus replay and `bin_io` round trips, and a coverage table.
- `test_alloc.ml`: minor words per call (`bench_cases.ml` builds the states); idle ticks
  and book walks must stay at 0.

Accept new expect output with `dune promote <file>` using the same `--root` and
`DUNE_BUILD_DIR` as the test run, one file at a time.

See `../AGENTS.md`.
