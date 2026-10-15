(lang dune 3.17)

(name execution-algos)

(generate_opam_files true)

(license MIT)

(maintainers "winterchill-trading")

(authors "winterchill-trading")

(source
 (github winterchill-trading/execution-algos))

(package
 (name execution-algos)
 (synopsis
  "Venue-neutral execution algorithms: a pure, journal-replayable child-order scheduler with impact estimates and cost reports")
 (description
  "Slices a parent order (a target position change) into child orders: Immediate, TWAP, VWAP, POV, passive pegged/iceberg with timeout, and an adaptive copy algorithm. The scheduler is a pure state machine over time, book and fills, so a journal of its inputs replays to the same orders. Includes L2 impact and slippage estimates, a volume profile from market-data trades, per-parent cost reports and a paper venue for tests and backtests.")
 (tags
  (trading execution twap vwap oxcaml))
 (depends
  (ocaml
   (>= 5.2))
  dune
  core
  ppx_jane
  bin_prot
  fixed-decimal
  exchange-core
  market-data
  (core_unix :with-test)
  (core_bench :with-test)))
