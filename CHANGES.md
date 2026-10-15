# Changes

All notable changes to this library. The format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/); versions follow semantic
versioning once released.

## Unreleased

- `Algo`: Immediate, TWAP, VWAP, POV, passive (pegged, iceberg) and adaptive copy
  algorithms with stable parameters and a short command-line form.
- `Scheduler`: a pure, journal-replayable child-order state machine; idle ticks allocate
  nothing.
- `Book`, `Impact`: top-of-book snapshots, book-walk slippage estimates and depth within
  a band.
- `Volume_profile`: time-of-day volume shares from recorded trades.
- `Cost_report`: arrival slippage, implementation shortfall, fees and totals per parent.
- `Paper`: a paper venue with book-walk IOC fills, queue-aware resting fills and latency.
