# execution-algos

Venue-neutral execution algorithms for [OxCaml](https://oxcaml.org): a pure scheduler
that slices a parent order (a target position change) into child orders, L2 impact and
slippage estimates, a volume profile for VWAP, per-parent cost reports, and a paper
venue that fills children against a replayed book with queue position.

The scheduler is a state machine with no clock and no mutable state: it is fed events
(ticks carrying the time, the book and the market volume; child fills; child closes; a
cancel) and returns the child orders to place and cancel. The same events from the same
start give the same actions, so a journal of its inputs replays exactly. The
copy-trading engine (`hl-copy-engine`) uses it to reconcile followers, and
`copy-backtest` replays recorded markets through it.

## Algorithms (`Algo`)

| Algo | Short form | What it does |
| --- | --- | --- |
| `Immediate` | `immediate` | One IOC child for the whole parent at the parent's limit (the slippage bound); what does not fill is left. A plain IOC order, and the copy engine's default. |
| `Twap` | `twap[:DURATION[:SLICES]]` (60 s, 6) | `SLICES` IOC children at evenly spaced times over `DURATION`; slice `k` brings the cumulative quantity to `(k+1)/SLICES` of the parent, so a slice that found no liquidity is carried into the next. |
| `Vwap` | `vwap[:DURATION[:SLICES]]` (5 min, 10) | Like TWAP, but slice `k` brings the cumulative quantity to the share of the window's expected volume traded by the end of the slice, from a `Volume_profile` (time-of-day buckets built from recorded trades; uniform without history). |
| `Pov` | `pov[:RATE[:MAX_DURATION]]` (0.1, 5 min) | IOC children that keep the cumulative quantity at `RATE` times the market volume traded since the parent started; stops at `MAX_DURATION`. |
| `Passive` | `passive[:TIMEOUT]` (30 s) | One post-only child pegged to the best price on the parent's own side, re-pegged when the touch moves; after `TIMEOUT` whatever is left goes out as one IOC. |
| iceberg | `iceberg[:DISPLAY[:TIMEOUT]]` (0.25, 30 s) | `Passive` showing at most `DISPLAY` of the parent per child. |
| `Adaptive` | `adaptive[:TIMEOUT]` (20 s) | The copy algorithm. At arrival it compares the parent with the depth within `depth_bps` (10) of the mid: when `size <= small_ratio * depth` (0.5) and the follower is not far behind the leader (`remaining notional <= max_behind_bps`, 50 bps, of its equity), it works passively like `Passive`; otherwise it takes liquidity at once. A passive parent switches to taking when the mid moves `max_drift_bps` (10) against it, and sweeps at the timeout. |

TWAP and VWAP accept a `max_participation` cap (sexp form), applied like POV's rate.
Every algo prices children no worse than the parent's limit and sizes them on the lot
grid, never below the parent's `min_size` (the venue's minimum order, except
`Immediate`'s single child, which is the whole parent as a plain IOC would be).

The sexp form of `Algo.t` is stable (`Algo.Stable.V1`), for configuration files:

```
(Adaptive (small_ratio 0.5) (depth_bps 10) (max_behind_bps 50) (max_drift_bps 10)
 (timeout_ms 20000))
```

## Using the scheduler

```ocaml
open Execution_algos

let parent =
  Parent.create ~side:Buy ~size ~limit_px ~reference_px:mark ~lot ~min_size ~equity ()
in
let s = Scheduler.create Algo.adaptive parent in
(* every tick: the time, a top-of-book snapshot, the cumulative market volume *)
let s, actions = Scheduler.apply s (Tick { now_ns; book = Book.of_l2_book l2; traded }) in
(* send [actions] (Place child / Cancel id); report what the venue says *)
let s, _ = Scheduler.apply s (Fill { child; size; price; fee; maker }) in
let s, _ = Scheduler.apply s (Closed child) in
(* once done *)
Scheduler.report s   (* Cost_report.t option *)
```

A parent starts on its first tick (arrival time, arrival mid, market volume so far). A
tick with nothing to do returns the same state and no action without allocating
(`Scheduler.tick`, an unboxed pair). `Scheduler.step` takes the fills and a tick in one
call; `Scheduler.replay` folds a journal of events.

## Invariants

Property-tested over 2,000 random cases (algo and parameters, side, size, minimum size,
limit, equity, a random-walk market with random depth and volume, venue latency of 0 to
2.5 s) through the paper venue, in `test/test_invariants.ml`:

- never more than the target: `filled + working <= size` after every event;
- every child on the lot grid, at least `max lot min_size` (but `Immediate`), and no
  worse than the parent's limit;
- with a participation cap, `filled + working <= rate * volume since the start`;
- nothing placed at or after the horizon but the single final IOC sweep of `Passive` and
  `Adaptive`, sent once the cancelled resting children are confirmed closed; every
  parent is done once the market has outlived its horizon and two venue round trips;
- replaying the journal of events on a fresh scheduler gives the same actions and the
  same final state; events round-trip through `bin_io`.

The expect tests in `test/test_scheduler.ml` trace each algorithm on a rising and a flat
market (`passive` is filled behind the queue by sellers in the flat one and chases the
bid in the rising one).

## Impact and costs

`Impact.walk` walks the book within a limit and returns the filled size and notional
(zero allocation); `Impact.estimate` adds the average and worst price, the slippage
against the mid and the touch, the levels reached and whether the shown depth ran out;
`Impact.depth_within` is the size a taker reaches within a band around the mid. The
paper venue fills IOCs with the same walk, so an estimate and a paper fill on the same
book agree.

`Cost_report` (one per parent, `Stable.V1`) measures against the arrival price `a` (the
mid at the first tick, or the parent's reference price): arrival slippage
`s (p - a) / a` in bps, implementation shortfall `s q (p - a) + s u (e - a) + fees` in USD
and bps of the target notional (execution cost, the opportunity cost of the unfilled
part `u` at the end price `e`, and fees), maker share, children and outcome
(`filled`, `deadline`, `cancelled`, `unfilled`). `Cost_report.Totals` sums them.

## Paper venue

`Paper` fills children for one instrument: IOCs walk the book less what earlier IOCs took
from the same snapshot; post-only children rest behind the size shown at their price,
which trades at the price eat first (trades through the price fill at once), a smaller
displayed size caps, and an opposite touch reaching the price fills as the maker. Actions
land `latency` after they are sent. Fees default to 4.5 bp taker and 1.5 bp maker.

## Performance

`bench/main.exe` (core_bench, Apple M-series, `-quota 1`), in the dev profile, the
release profile and release with `OCAMLPARAM='_,O3=1'`:

| Case | Dev | Release | O3 | Minor words (dev) |
| --- | ---: | ---: | ---: | ---: |
| idle tick, TWAP between slices | 8.7 ns | 8.2 ns | 6.7 ns | 0 |
| idle tick, passive child at the peg | 16.6 ns | 16.4 ns | 10.0 ns | 0 |
| tick placing a TWAP child | 76 ns | 75 ns | 44 ns | 127 |
| `Impact.walk`, 10 levels | 65 ns | 46 ns | 29 ns | 0 |
| `Impact.estimate` | 193 ns | 140 ns | 112 ns | 29 |
| a whole TWAP parent through the paper venue, 12 steps | 1.7 us | 1.6 us | 1.3 us | 1,297 |
| a whole adaptive parent, 12 steps | 1.4 us | 1.2 us | 0.95 us | 1,011 |

core_bench reports 3 words for the zero-allocation cases (its own overhead);
`test/test_alloc.ml` pins 0 with `Gc.minor_words`. `Book` reads, `Impact.walk`,
`Impact.depth_within` and `Impact.limit_from` carry `[@zero_alloc]` in their
interfaces.

## Build

Inside the organization's dune workspace:

```sh
opam exec --switch 5.2.0+ox -- dune build --root .. @execution-algos/all @execution-algos/runtest
opam exec --switch 5.2.0+ox -- dune exec --root .. execution-algos/bench/main.exe -- -quota 2
```

Standalone, with the organization's opam repository added
(`opam repo add winterchill https://github.com/winterchill-trading/opam-repository.git`):
`opam install . --deps-only --with-test && dune build && dune test`.

Depends on `fixed-decimal`, `exchange-core` and `market-data`.
